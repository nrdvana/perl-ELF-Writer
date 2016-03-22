package ELF::Writer::Segment;
use Moo 2;
use Carp;
use namespace::clean;

# ABSTRACT: Object representing the fields of one program segment in an ELF file.

=head1 ATTRIBUTES (header fields)

The following are elf program header fields:

=head2 type

Type of segment: 'null' (or undef), 'load', 'dynamic', 'interp', 'note',
'shlib', or 'phdr'.  Defaults to 'load'.

=head2 offset

Offset of this segment within the elf file

=head2 virt_addr

Address where this segment should be memory-mapped

=head2 phys_addr

Address where this segment should be loaded

=head2 filesize, size

Size of the segment within the elf file

=head2 memsize

Size of the segment after loaded into memory

=head2 flags

32-bit flags.  Use the accessors below to access the defined bits.
Defaults to readable and executable.

=head2 flag_readable

Read/write the 'readable' bit of flags

=head2 flag_writeable

Read/write the 'writeable' bit of flags

=head2 flag_executable

Read/write the 'executable' bit of flags.

=head2 align

Page size, for both the file and when loaded into memory (I think?)

=cut

ELF::Writer::_enum_attribute(\&has, 'type',
	\&ELF::Writer::SegmentTypeEnum_encode, \&ELF::Writer::SegmentTypeEnum_decode);
has '+type_num' => ( default => sub { 1 } );

has offset      => ( is => 'rw' );
has virt_addr   => ( is => 'rw' );    
has phys_addr   => ( is => 'rw' );

has filesize    => ( is => 'rw' );
*size= *filesize; # alias

has memsize     => ( is => 'rw' );

has flags       => ( is => 'rw', default => sub { 5 } );

sub flag_readable {
	my ($self, $value)= @_;
	$self->flags( $self->flags & ~1 | ($value? 1 : 0) )
		if defined $value;
	$self->flags & 1;
}

sub flag_writeable {
	my ($self, $value)= @_;
	$self->flags( $self->flags & ~2 | ($value? 2 : 0) )
		if defined $value;
	$self->flags & 2;
}
*flag_writable= *flag_writeable;

sub flag_executable {
	my ($self, $value)= @_;
	$self->flags( $self->flags & ~4 | ($value? 4 : 0) )
		if defined $value;
	$self->flags & 4;
}

has align       => ( is => 'rw' );

=head1 ATTRIBUTES (user)

=head2 data

The payload of this segment (machine code, or etc)

=head2 data_start

Used for auto-aligning segments within the elf file.  This is the number of
bytes in the file which should come between 'offset' and your data.  Typical
use of this feature is to have the first segment start at offset 0 and include
the elf header, with data starting somehwere beyond it.  If this is zero (or
just less than the size of your elf header) then nearly a whole page will be
wasted within the file as it aligns the start of the data to a page boundary.

=cut

has data        => ( is => 'rw' );
has data_start  => ( is => 'rw', default => sub { 0 } );

sub data_offset { $_[0]->offset + $_[0]->data_start }

has _index => ( is => 'rw' );
sub _name { "segment ".shift->_index }
sub _required_file_alignment { $_[0]->align || 1 }

sub BUILD {
	my ($self, $params)= @_;
	defined $params->{flag_readable}
		and $self->flag_readable($params->{flag_readable});
	defined $params->{flag_writeable}
		and $self->flag_writeable($params->{flag_writeable});
	defined $params->{flag_executable}
		and $self->flag_executable($params->{flag_executable});
}

=head1 METHODS

=head2 new

standard Moo constructor. Pass any attributes, *including* the flag aliases.

=head2 coerce

  $class->coerce($thing)

Returns C<$thing> if it is an instance of C<$class>, or passes $thing to the
constructor otherwise.

=head2 clone

Clone this instance.

=cut

sub coerce {
	my ($class, $thing)= @_;
	return (ref $thing && ref($thing)->isa(__PACKAGE__))? $thing : $class->new($thing);
}

sub clone {
	my $self= shift;
	return bless { %$self }, ref $self;
}

1;
