package ELF::Writer::Segment;
use Moo 2;
use Carp;
use namespace::clean;

=head1 ATTRIBUTES (header fields)

The following are elf program header fields:

=head2 type

Type of segment: 'load', 'interp', or 'note'.  Defaults to 'load'.

=head2 offset

Offset of this segment within the elf file

=head2 virt_addr

Address where this segment should be memory-mapped

=head2 phys_addr

Address where this segment should be loaded

=head2 filesize

Size of the segment within the elf file

=head2 memsize

Size of the segment after loaded into memory

=head2 readable, writeable, executable

Boolean access flags for the segment.  Defaults to readable and executable.

=head2 align

Page size, for both the file and when loaded into memory (I think?)

=cut

ELF::Writer::_enum_attribute(\&has, 'type', \&ELF::Writer::SegmentTypeEnum_encode, \&ELF::Writer::SegmentTypeEnum_decode);
has '+type_num' => ( default => sub { 1 } );

has offset      => ( is => 'rw' );
has virt_addr   => ( is => 'rw' );    
has phys_addr   => ( is => 'rw' );
has filesize    => ( is => 'rw' );
has memsize     => ( is => 'rw' );
has readable    => ( is => 'rw', default => sub { 1 } );
has writeable   => ( is => 'rw', default => sub { 0 } );
has executable  => ( is => 'rw', default => sub { 1 } );
has align       => ( is => 'rw' );

=head1 ATTRIBUTES (user)

=head2 data

The payload of this segment (machine code, or etc)

=head2 data_offset

Used for auto-aligning segments within the elf file.  This is the number of
bytes in the file which should come bwteen 'offset' and your data.  Typical
use of this feature is to have the first segment start at offset 0 and include
the elf header, with data starting somehwere beyond it.  If this is zero (or
just less than the size of your elf header) then nearly a whole page will be
wasted within the file as it aligns the start of the data to a page boundary.

=cut

has data        => ( is => 'rw' );
has data_offset => ( is => 'rw' );

sub coerce {
	my ($class, $thing)= @_;
	return (ref $thing && ref($thing)->isa(__PACKAGE__))? $thing : $class->new($thing);
}

1;
