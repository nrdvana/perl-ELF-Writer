package ELF::Writer;
use Moo 2;
use Carp;
use IO::File;
use namespace::clean;

our $VERSION; BEGIN { $VERSION= '0.001' }

# ABSTRACT: Encode elf files with pure-perl

=head1 MODULE STATUS

I wrote this module while learning the ELF format.  This is not the work of an
expert.  Yet, it could still be useful for people, so I decided to implement as
much as I could and publish it.  Bug reports are very welcome.

The API is not completely stable, but I will at least promise the numeric
accessors for the header fields will remain as-is.  (type_num, machine_num,
class_num, version, etc.)

=head1 DESCRPTION

This module lets you define the attributes, segments, and sections of an ELF
specification, and then serialize it to a file.  All data must reside in
memory before writing, so this module is really just a very elaborate call to
'pack'.  This module also assumes you know how an ELF file is structured,
and the purpose of Segments and Sections.  Patches welcome for adding
user-friendly features and sanity checks.

=head1 SYNOPSIS

  my $elf= ELF::Writer::Linux_x86_64->new(
    type => 'executable',
    segments => [{
      offset      => 0, # overlap segment with elf header
      virt_addr   => 0x10000,
      data        => $my_machine_code,
      data_start  => undef # calculated below
    }],
  );
  
  # Overlap the first segment with the elf header, for size efficiency
  # http://www.muppetlabs.com/~breadbox/software/tiny/teensy.html
  
  my $prog_offset= $elf->elf_header_len + $elf->segment_header_elem_len;
  $elf->segments->[0]->data_start( $prog_offset );
  $elf->entry_point( $elf->segments->[0]->virt_addr + $prog_offset );
  
  # Write out an elf file
  $elf->write_file($binary_name);

=cut

our (
	%ClassEnum, %ClassEnum_r,
	%DataEnum, %DataEnum_r,
	%OsabiEnum, %OsabiEnum_r,
	%TypeEnum, %TypeEnum_r,
	%MachineEnum, %MachineEnum_r,
	%SegmentTypeEnum, %SegmentTypeEnum_r,
	%SectionTypeEnum, %SectionTypeEnum_r,
);

sub _create_enum {
	my ($field_name, $enum_name, @mapping)= @_;
	no strict 'refs';
	%{__PACKAGE__.'::'.$enum_name}= @mapping;
	%{__PACKAGE__.'::'.$enum_name.'_r'}= reverse @mapping;
	eval 'sub '.$enum_name.'_encode {
		my $x= $'.$enum_name.'_r{$_[0]};
		defined $x or $x= $_[0];
		$x =~ /^[0-9]+$/ or croak "Invalid '.$field_name.': $_[0]";
		$x
	}
	sub '.$enum_name.'_decode {
		$'.$enum_name.'{$_[0]} || $_[0]
	}
	1' || die $@;
}

our $Magic; BEGIN { $Magic= "\x7fELF"; }

BEGIN {
_create_enum( 'class', 'ClassEnum',
	1 => '32bit',
	2 => '64bit'
);

_create_enum( 'data', 'DataEnum',
	1 => '2LSB',
	2 => '2MSB'
);

_create_enum( 'osabi', 'OsabiEnum',
	0 => 'System V', 1 => 'HP-UX', 2 => 'NetBSD', 3 => 'Linux', 6 => 'Solaris',
	7 => 'AIX', 8 => 'IRIX', 9 => 'FreeBSD', 0x0C => 'OpenBSD', 0x0D => 'OpenVMS'
);

_create_enum( 'type', 'TypeEnum',
	0 => 'none',
	1 => 'relocatable',
	2 => 'executable',
	3 => 'shared',
	4 => 'core'
);

_create_enum( 'machine', 'MachineEnum',
	0x02 => 'SPARC', 0x03 => 'i386', 0x04 => 'Motorola68K', 0x05 => 'Motorola88K',
	0x07 => 'i860', 0x08 => 'MIPS-RS3000', 0xA0 => 'MIPS-RS4000',
	0x14 => 'PowerPC',
	0x28 => 'ARM', 0x2A => 'SuperH', 0x32 => 'IA-64', 0x3E => 'x86-64',
	0xB7 => 'AArch64'
);

_create_enum( 'segment type', 'SegmentTypeEnum',
	0 => 'null',    # Ignored entry in program header table
	1 => 'load',    # Load segment into program address space
	2 => 'dynamic', # Dynamic linking information
	3 => 'interp',  # Specifies location of string defining path to interpreter
	4 => 'note',    # Specifies location of auxillary information
	5 => 'shlib',   # ??
	6 => 'phdr',    # Specifies location of the program header loaded into process image
);

_create_enum( 'section type', 'SectionTypeEnum',
	0 => 'null',     # Ignore this section entry
	1 => 'progbits', # Contents of section are program specific
	2 => 'symtab',   # symbol table
	3 => 'strtab',   # string table
	4 => 'rela',     # relocation table with specific addends
	5 => 'hash',     # symbol hash table
	6 => 'dynamic',  # dynamic linking information
	7 => 'note',     # various identification of file
	8 => 'nobits',   # program-specific "pointer" using offset field.  has no length.
	9 => 'rel',      # relocation table without specific addends
	10 => 'shlib',   # ??
	11 => 'dynsym',  # symbol table
	12 => 'num',     # ??
);

} # BEGIN

=head1 ATTRIBUTES

=over

=item class

8-bit integer, or one of: '32bit' or '64bit'.  Must be set before writing.

=item data

8-bit integer, or one of: '2LSB' or '2MSB'. (2's complement least/most
significant byte first)  i.e. little-endian or big-endian

Must be set before writing.

=item header_version

8-bit integer; defaults to '1' for original version of ELF.

=item osabi

8-bit integer, or one of: 'System V', 'HP-UX', 'NetBSD', 'Linux', 'Solaris',
'AIX', 'IRIX', 'FreeBSD', 'OpenBSD', 'OpenVMS'.  Must be set before writing.

=item osabi_version

Depends on osabi.  Not used for Linux.  Defaults to 0.

=item type

16-bit integer, or one of: 'relocatable', 'executable', 'shared', 'core'.
Must be set before writing.

=item machine

16-bit integer, or one of: 'Sparc', 'x86', 'MIPS', 'PowerPC', 'ARM', 'SuperH',
'IA-64', 'x86-64', 'AArch64'.

=item version

32-bit integer; defaults to '1' for original version of ELF.

=item entry_point

32-bit or 64-bit pointer to address where process starts executing.
Defaults to 0 unless type is 'executable', then you must specify it before
writing.

=item flags

32 bit flags, defined per-machine.

=cut

sub _enum_attribute {
	my ($has, $attr, $encoder, $decoder)= @_;
	my $caller= caller;
	# Define the Moo accessor for the numeric storage of the enum
	$has->($attr.'_num' => (
		is => 'rw',
		init_arg => $attr,
		coerce => $encoder
	));
	
	# Define a friendly accessor for the translated value of the enum
	eval 'sub '.$caller.'::'.$attr.' {
		if (@_ > 1) {
			my $val= $_[1];
			if (defined $val) {
				$val= $encoder->($val);
				Carp::croak("$_[1] is not a valid '.$attr.'")
					unless $val =~ /^[0-9]+$/;
			}
			$_[0]->'.$attr.'_num($val);
		}
		my $x= $_[0]->'.$attr.'_num;
		defined $x? $decoder->($x) : undef;
	}
	1' || die $@;
}

_enum_attribute(\&has, class => \&ClassEnum_encode, \&ClassEnum_decode);

_enum_attribute(\&has, data => \&DataEnum_encode, \&DataEnum_decode);

has header_version  => ( is => 'rw', default => sub { 1 } );

_enum_attribute(\&has, osabi => \&OsabiEnum_encode, \&OsabiEnum_decode);

has osabi_version   => ( is => 'rw', default => sub { 0 } );

_enum_attribute(\&has, type => \&TypeEnum_encode, \&TypeEnum_decode);

_enum_attribute(\&has, machine => \&MachineEnum_encode, \&MachineEnum_decode);

has version         => ( is => 'rw', default => sub { 1 } );

has flags           => ( is => 'rw', default => sub { 0 } );

has entry_point     => ( is => 'rw' );

=item elf_header_len

Determined from L</class>.  (52 or 64 bytes)

=item segment_header_elem_len

Determined from L</class>.  (32 or 56 bytes)

=item section_header_elem_len

Determined from L</class>.  (40 or 64 bytes)

=cut

sub elf_header_len {
	my $class= shift->class;
	return $class eq '32bit'? 52
		: $class eq '64bit'? 64
		: croak "Don't know structs for elf class $class";
}
our @Elf_Header_Pack= (
	'a4 C C C C C a7 S< S< L< L< L< L< L< S< S< S< S< S< S<', # 32-bit LE
	'a4 C C C C C a7 S> S> L> L> L> L> L> S> S> S> S> S> S>', # 32-bit BE
	'a4 C C C C C a7 S< S< L< Q< Q< Q< L< S< S< S< S< S< S<', # 64-bit LE
	'a4 C C C C C a7 S> S> L> Q> Q> Q> L> S> S> S> S> S> S>', # 64-bit BE
);
sub _elf_header_packstr {
	my ($self, $encoding)= @_;
	$encoding= $self->_encoding unless defined $encoding;
	$Elf_Header_Pack[ $encoding ];
}

sub segment_header_elem_len {
	my $class= shift->class;
	return $class eq '32bit'? 32
		: $class eq '64bit'? 56
		: croak "Don't know structs for elf class $class";
}
# Note! there is also a field swap between 32bit and 64bit
our @Segment_Header_Pack= (
	'L< L< L< L< L< L< L< L<',
	'L> L> L> L> L> L> L> L>',
	'L< L< Q< Q< Q< Q< Q< Q<',
	'L> L> Q> Q> Q> Q> Q> Q>',
);
sub _segment_header_packstr {
	my ($self, $encoding)= @_;
	$encoding= $self->_encoding unless defined $encoding;
	$Segment_Header_Pack[ $encoding ];
}

sub section_header_elem_len {
	my $class= shift->class;
	return $class eq '32bit'? 40
		: $class eq '64bit'? 64
		: croak "Don't know structs for elf class $class";
}
our @Section_Header_Pack= (
	'L< L< L< L< L< L< L< L< L< L<',
	'L> L> L> L> L> L> L> L> L> L>',
	'L< L< Q< Q< Q< Q< L< L< Q< Q<',
	'L> L> Q> Q> Q> Q> L> L> Q> Q>',
);
sub _section_header_packstr {
	my ($self, $encoding)= @_;
	$encoding= $self->_encoding unless defined $encoding;
	$Section_Header_Pack[ $encoding ];
}

# Returns a number 0..3 used by the various routines when packing binary data
sub _encoding {
	my $self= shift;
	my $endian= $self->data_num;
	my $bits=   $self->class_num;
	defined $endian && $endian > 0 && $endian < 3 or croak "Can't encode for data=$endian";
	defined $bits && $bits > 0 && $bits < 3 or croak "Can't encode for class=$bits";
	return ($bits-1)*2 + ($endian-1);
}

=item segments

Arrayref of L<ELF::Writer::Segment> objects.  You can also pass hashrefs to
the constructor which will be coerced automatically.

=item segment_count

Handy alias for $#{ $elf->segments }

=item segment_list

Handy alias for @{ $elf->segments }

=cut

has segments     => ( is => 'rw', coerce => \&_coerce_segments, default => sub { [] } );
sub segment_count { scalar @{ shift->segments } }
sub segment_list { @{ shift->segments } }

=item sections

Arrayref of L<ELF::Writer::Section> objects.  You can also pass hashrefs to
the constructor which will be coerced automatically.

=item section_count

Handy alias for $#{ $elf->sections }

=item section_list

Handy alias for @{ $elf->sections }

=item section_name_string_table_index

Insex into the section array of a string-table section where the names of
the sections are stored.

=cut

has sections     => ( is => 'rw', coerce => \&_coerce_sections, default => sub { [] } );
sub section_count { scalar @{ shift->sections } }
sub section_list { @{ shift->sections } }

has section_name_string_table_idx => ( is => 'rw' );

=back

=head1 METHODS

=head2 serialize

Return a string of the composed ELF file.  Throws exceptions if required
attributes are missing.

=cut

sub serialize {
	my $self= shift;
	
	# Faster than checking bit lengths on every field ourself
	use warnings FATAL => 'pack';
	
	# Make sure all required attributes are defined
	defined($self->can("${_}_num")->($self)) || croak "Attribute $_ is not defined"
		for qw( class data osabi type machine );
	defined($self->$_) || croak "Attribute $_ is not defined"
		for qw( header_version osabi_version version entry_point );
	
	# Clone the segments and sections so that our changes don't affect the
	# configuration the user built.
	my @segments= map { $_->clone } $self->segment_list;
	my @sections= map { $_->clone } $self->section_list;
	my $segment_table;
	my $section_table;
	
	# Now apply defaults and set numbering for diagostics of errors
	my $i= 0;
	for (@segments) {
		$_->_index($i++);
		$self->_apply_segment_defaults($_);
		
		# There can be one segment which loads the segment table itself
		# into the program's address space.  If used, we track the pointer
		# to that segment.  We also clear it's 'data' and set it's 'size'
		# to keep from confusing the code below.
		if ($_->type_num == 6) {
			croak "There can be only one segment of type 'phdr'"
				if defined $segment_table;
			$segment_table= $_;
			$segment_table->data(undef);
			$segment_table->size($self->segment_header_len * @segments);
		}
	}
	$i= 0;
	for (@sections) {
		$_->_index($i++);
		$self->_apply_section_defaults($_);
	}
	
	# Build a list of every defined range of data in the file,
	# and a list of every segment/section which needs automatically placed.
	my @defined_ranges;
	my @auto_offset;
	for (@segments, @sections) {
		# size is guaranteed to be defined by "_apply...defaults()"
		# Data might not be defined if the user just wanted to point the
		# segment at something, and offset might not be defined if the user
		# just wants it appended wherever.
		if (!defined $_->offset) {
			push @auto_offset, $_;
		}
		else {
			$_->offset >= 0 or croak $_->_name." offset cannot be negative";
			push @defined_ranges, $_
				if defined $_->data && length $_->data;
		}
	}
	
	if (@sections) {
		# First section must always be the NULL section.  If the user forgot this
		# then their indicies might be off.
		$sections[0]->type_num == 0
			or croak "Section 0 must be type NULL";
		# Sections may not overlap, regardless of whether the user attached data to them
		my $prev_end= 0;
		my $prev;
		for (sort { $a->offset <=> $b->offset } $self->section_list) {
			croak 'Section overlap between '.$_->_name.' and '.$prev->_name
				if $_->offset < $prev_end;
			$prev_end= $_->offset + $_->size;
		}
	}
	
	# Each segment and section can define data to be written to the file,
	# but segments can overlap sections.  Make sure their defined data doesn't
	# conflict, or we wouldn't know which to write.
	my $prev;
	my $prev_end= $self->elf_header_len;
	my $first_data;
	@defined_ranges= sort { $a->data_offset <=> $b->data_offset } @defined_ranges;
	for (@defined_ranges) {
		croak 'Data overlap between '.$_->_name.' and '.($prev? $prev->_name : 'ELF header')
			if $_->data_offset < $prev_end;
		$prev= $_;
		$prev_end= $_->data_offset + $_->size;
	}
	
	# For each segment or section that needs an offset assigned, append to
	# end of file.
	for (@auto_offset) {
		my $align= $_->_required_file_alignment;
		$prev_end= int(($prev_end + $align - 1) / $align) * $align;
		$_->offset($prev_end);
		push @defined_ranges, $_ if defined $_->data && length $_->data;
		$prev_end += $_->size;
	}
	
	# Now, every segment and section have an offset and a length.
	# We can now encode the tables.
	my @insert;
	if (@segments) {
		my $segment_table_data= '';
		$segment_table_data .= $self->_serialize_segment_header($_)
			for @segments;
		# The user might have defined this segment on their own.
		# Otherwise we just create a dummy to use below.
		if (!defined $segment_table) {
			$segment_table= ELF::Writer::Segment->new(
				align => 8,
				filesize => length($segment_table_data),
				data => $segment_table_data,
			);
			push @insert, $segment_table;
		} else {
			$segment_table->data($segment_table_data);
		}
	}
	if (@sections) {
		my $section_table_data= '';
		$section_table_data .= $self->_serialize_section_header($_)
			for @sections;
		
		$section_table= ELF::Writer::Segment->new(
			align => 8,
			filesize => length($section_table_data),
			data => $section_table_data,
		);
		push @insert, $section_table;
	}
	
	# Find a spot for the segment and/or section tables.
	# Due to alignment, there is probably room to squeeze the table(s) inbetween
	# other defined ranges.  Else, put them at the end.
	$prev_end= $self->elf_header_len;
	for (my $i= 0; @insert and $i <= @defined_ranges; ++$i) {
		my $align= $insert[0]->_required_file_alignment;
		$prev_end= int(($prev_end + $align-1) / $align) * $align;
		if ($i == @defined_ranges
			or $prev_end + $insert[0]->size <= $defined_ranges[$i]->data_offset
		) {
			$insert[0]->offset($prev_end);
			splice @defined_ranges, $i, 0, shift @insert;
		}
	}
	
	# Now, we can finally encode the ELF header.
	my $header= pack($self->_elf_header_packstr,
		$Magic, $self->class_num, $self->data_num, $self->header_version,
		$self->osabi_num, $self->osabi_version, '',
		$self->type_num, $self->machine_num, $self->version, $self->entry_point,
		($segment_table? $segment_table->offset : 0),
		($section_table? $section_table->offset : 0),
		$self->flags, $self->elf_header_len,
		$self->segment_header_elem_len, $self->segment_count,
		$self->section_header_elem_len, $self->section_count,
		$self->section_name_string_table_idx || 0,
	);
	# sanity check
	length($header) == $self->elf_header_len
		or croak "Elf header len mismatch";
	
	# Write out the header and each range of defined bytes, padded with NULs as needed.
	my $data= $header;
	for (@defined_ranges) {
		my $pad= $_->data_offset - length($data);
		$data .= "\0" x $pad if $pad;
		$data .= $_->data;
	}
	return $data;
}

sub _serialize_segment_header {
	my ($self, $seg)= @_;
	
	# Faster than checking bit lengths on every field ourself
	use warnings FATAL => 'pack';
	
	# Make sure all required attributes are defined
	(defined $seg->can("${_}_num")->($self)) || croak "Attribute $_ is not defined"
		for qw( type );
	defined $seg->$_ or croak "Attribute $_ is not defined"
		for qw( offset virt_addr align );
	
	my $filesize= $seg->filesize;
	$filesize= length($seg->data) + $seg->data_offset
		unless defined $filesize;
	
	my $align= $seg->align;
	my $memsize= $seg->memsize;
	$memsize= int(($filesize + $align - 1) / $align) * $align
		unless defined $memsize;
	
	# 'flags' moves depending on 32 vs 64 bit, so changing the pack string isn't enough
	return $self->_encoding < 2?
		pack($self->_segment_header_packstr,
			$seg->type_num, $seg->offset, $seg->virt_addr, $seg->phys_addr // 0,
			$filesize, $memsize, $seg->flags, $seg->align
		)
		: pack($self->_segment_header_packstr,
			$seg->type_num, $seg->flags, $seg->offset, $seg->virt_addr,
			$seg->phys_addr // 0, $filesize, $memsize, $seg->align
		);
}

sub _serialize_section_header {
	my ($self, $sec)= @_;
	
	# Make sure all required attributes are defined
	(defined $sec->can("${_}_num")->($self)) || croak "Attribute $_ is not defined"
		for qw( type );
	defined $sec->$_ or croak "Attribute $_ is not defined"
		for qw( name flags addr offset size link info addralign entsize );
	
	# Faster than checking bit lengths on every field ourself
	use warnings FATAL => 'pack';
	
	return pack($self->_section_header_packstr,
		$sec->name, $sec->type, $sec->flags, $sec->addr, $sec->offset,
		$sec->size, $sec->link, $sec->info, $sec->align, $sec->entry_size
	);
}

sub deserialize {
	my $class= shift;
	croak "Unimplemented";
}

sub _coerce_segments {
	my $spec= shift;
	return [ map { (__PACKAGE__.'::Segment')->coerce($_) } @$spec ];
}
sub _coerce_sections {
	my $spec= shift;
	return [ map { (__PACKAGE__.'::Section')->coerce($_) } @$spec ];
}

# Overridden by subclasses for machine-specific defaults
sub _apply_section_defaults {
	my ($self, $sec)= @_;
	# Undef type is "null" type 0
	my $type= $sec->type_num;
	defined $type
		or $sec->type_num($type= 0);
	my $offset= $sec->offset;
	my $size= $sec->size;
	
	if ($type == 0) { # 'null'
		# Ensure length and offset are zero
		$size= $sec->size(0) unless defined $size;
		$offset= $sec->offset(0) unless defined $offset;
		croak "null section should have offset=0 and size=0"
			if $offset || $size;
	}
	elsif ($type == 8) { # 'nobits'
		# Offset can be set but ensure size is zero
		$size= $sec->size(0) unless defined $size;
		croak "nobits section should have size=0"
			if $size;
		
	}
	else {
		# 'size' is required, but can be computed from 'data' and 'data_offset'.
		if (!defined $size) {
			defined $sec->data or croak "Section must define 'size' or 'data'"; 
			$sec->size($sec->data_start + length($sec->data));
		}
	}
}
sub _apply_segment_defaults {
	my ($self, $seg)= @_;
	# Undef type is "null" type 0
	my $type= $seg->type_num;
	defined $type
		or $seg->type_num($type= 0);
	my $offset= $seg->offset;
	my $filesize= $seg->filesize;
	
	if ($type == 0) { # 'null'
		# Ensure length and offset are zero
		$filesize= $seg->filesize(0) unless defined $filesize;
		$offset= $seg->offset(0) unless defined $offset;
		croak "null segment should have offset=0 and filesize=0"
			if $offset || $filesize;
	}
	else {
		# 'filesize' is required, but can be computed from 'data' and 'data_offset'
		if (!defined $filesize) {
			defined $seg->data or croak "Segment must define 'filesize' or 'data'";
			$filesize= $seg->filesize($seg->data_start + length($seg->data));
		}
		# Default memsize to filesize
		$seg->memsize($filesize) unless defined $seg->memsize;
	}
}

use ELF::Writer::Segment;
use ELF::Writer::Section;

1;
