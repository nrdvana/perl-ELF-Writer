package ELF::Writer;
use Moo 2;
use Carp;
use IO::File;
use namespace::clean;

# ABSTRACT: Encode elf files with pure-perl

=head1 DESCRPTION

This module lets you define the attributes, segments, and sections of an ELF
specification, and then serialize it to a file.  All data must reside in
memory before writing, so this module is really just a very elaborate call to
'pack'.  This module also assumes you know how an ELF file is structured,
and the purpose of Segments and Sections.  Patches welcome for adding
user-friendly features.

=head1 SYNOPSIS

  my $elf= ELF::Writer::Linux_x86_64->new(
    type => 'executable',
    segments => [{
      offset      => 0, # overlap segment with elf header
      virt_addr   => 0x10000,
      data        => $program,
      data_offset => undef
    }],
  );
  
  # Overlap the first segment with the elf header, for size efficiency
  # http://www.muppetlabs.com/~breadbox/software/tiny/teensy.html
  
  my $prog_offset= $elf->elf_header_len + $elf->segment_header_elem_len;
  $elf->segments->[0]->data_offset( $prog_offset );
  $elf->entry_point( $elf->segments->[0]->virt_addr + $prog_offset );
  
  # Write out an elf file
  $elf->write_file($binary_name);

=cut

our (%ClassEnum, %DataEnum, %OsabiEnum, %TypeEnum, %MachineEnum, %SegmentTypeEnum);
our (%ClassEnum_r, %DataEnum_r, %OsabiEnum_r, %TypeEnum_r, %MachineEnum_r, %SegmentTypeEnum_r);

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

our $Magic=          "\x7fELF";

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
	1 => 'relocatable',
	2 => 'executable',
	3 => 'shared',
	4 => 'core'
);

_create_enum( 'machine', 'MachineEnum',
	0x02 => 'Sparc', 0x03 => 'x86', 0x08 => 'MIPS', 0x14 => 'PowerPC',
	0x28 => 'ARM', 0x2A => 'SuperH', 0x32 => 'IA-64', 0x3E => 'x86-64',
	0xB7 => 'AArch64'
);

_create_enum( 'segment type', 'SegmentTypeEnum',
	1 => 'load',
	3 => 'interp',
	4 => 'note'
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

=item ident_version

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

has ident_version   => ( is => 'rw', default => sub { 1 } );

_enum_attribute(\&has, osabi => \&OsabiEnum_encode, \&OsabiEnum_decode);

has osabi_version   => ( is => 'rw', default => sub { 0 } );

_enum_attribute(\&has, type => \&TypeEnum_encode, \&TypeEnum_decode);

_enum_attribute(\&has, machine => \&MachineEnum_encode, \&MachineEnum_decode);

has version         => ( is => 'rw', default => sub { 1 } );

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

=item sections

Arrayref of L<ELF::Writer::Section> objects.  You can also pass hashrefs to
the constructor which will be coerced automatically.

=item section_count

Handy alias for $#{ $elf->sections }

=item section_list

Handy alias for @{ $elf->sections }

=cut

has sections     => ( is => 'rw', coerce => \&_coerce_sections, default => sub { [] } );
sub section_count { scalar @{ shift->sections } }

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
		for qw( ident_version osabi_version version entry_point );
	
	# Put segment headers right after elf header
	my $seg_header_ofs= $self->elf_header_len;
	
	my $flags= 0;

	# TODO: handle sections (ignored for now)
	my $sec_header_ofs= 0;
	my $symbol_section_idx= 0;
	if ($self->section_count > 0) {
		croak "Section encoding not implemented";
	}
	
	my $header= pack($self->_elf_header_packstr,
		$Magic, $self->class_num, $self->data_num, $self->ident_version, $self->osabi_num,
		$self->osabi_version, '',
		$self->type_num, $self->machine_num, $self->version, $self->entry_point,
		$seg_header_ofs, $sec_header_ofs, $flags,
		$self->elf_header_len,
		$self->segment_header_elem_len, $self->segment_count,
		$self->section_header_elem_len, $self->section_count,
		$symbol_section_idx
	);
	# sanity check
	length($header) == $self->elf_header_len
		or croak "Elf header len mismatch";
	
	my %segment_padding;
	
	# Auto-concat the segments with undefined offsets, and calculate the
	# padding before the payload of each segment.
	my $data_pos= length($header);
	$data_pos += $self->segment_header_elem_len * $self->segment_count;
	for my $seg (@{$self->segments}) {
		# let subclasses supply defaults
		$self->_apply_segment_defaults($seg);
		
		my $fileofs= $seg->offset;
		my $data_offset= $seg->data_offset || 0;
		# If undef, automatically pack the segment according to alignment
		# But back up by as much as data_offset.
		if (!defined $fileofs) {
			my $align= $seg->align
				or croak "Must specify segment->align if you want to auto-calculate segment file offsets";
			$fileofs= int(($data_pos - $data_offset + $align - 1)/$align) * $align;
			$fileofs= 0 if $fileofs < 0;
			$seg->offset($fileofs);
		}
		$segment_padding{$seg}= $fileofs + $data_offset - $data_pos;
		$segment_padding{$seg} >= 0
			or croak "Segment overlaps previous by more than data_offset";
		$data_pos += $segment_padding{$seg} + length($seg->data);
	}
	
	# Now build the file
	my $data= join('',
		$header,
		( map { $self->_serialize_segment_header($_) } @{$self->segments} ),
		( map { ("\0" x $segment_padding{$_}), $_->data } @{$self->segments} ),
		# TODO, add sections and section headers
	);
	# Sanity check
	length($data) == $data_pos
		or die "Bug: size calculation $data_pos != ".length($data);
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
	
	my $flags= ($seg->readable? 4 : 0)
		+ ($seg->writeable? 2 : 0)
		+ ($seg->executable? 1 : 0);
	
	my $filesize= $seg->filesize;
	$filesize= length($seg->data) + $seg->data_offset
		unless defined $filesize;
	
	my $memsize= $seg->memsize;
	$memsize= $filesize
		unless defined $memsize;
	
	# 'flags' moves depending on 32 vs 64 bit, so changing the pack string isn't enough
	return $self->_encoding < 2?
		pack($self->_segment_header_packstr,
			$seg->type_num, $seg->offset, $seg->virt_addr, $seg->phys_addr // 0,
			$filesize, $memsize, $flags, $seg->align
		)
		: pack($self->_segment_header_packstr,
			$seg->type_num, $flags, $seg->offset, $seg->virt_addr,
			$seg->phys_addr // 0, $filesize, $memsize, $seg->align
		);
}

sub _serialize_section_header {
	my ($self, $sec)= @_;
	croak "Section encoding is not implemented";
	
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

# used by subclasses for machine-specific defaults
sub _apply_section_defaults {}
sub _apply_segment_defaults {}

use ELF::Writer::Segment;


1;
