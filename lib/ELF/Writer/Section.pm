package ELF::Writer::Section;
use Moo 2;
use Carp;
use namespace::clean;

# ABSTRACT: Object representing the fields of one section in an ELF file.

=head1 ATTRIBUTES (header fields)

The following are elf section header fields:

=head2 name

Pointer to name of this section within the Strings table. (.shstrtab)

TODO: auto-generate the string table if this is set to anything other than a number.

=head2 type

Type of this section.  A 32-bit number, or one of: 'null' (or undef), 'progbits',
'symtab', 'strtab', 'rela', 'hash', 'dynamic', 'note', 'nobits', 'rel', 'shlib',
'dynsym', 'num'.

=head2 flags

32-bit flags.  Use the attributes below to access known flag bits.

=head2 flag_write

Read/write accessor for write bit of flags

=head2 flag_alloc

Read/write accesor for alloc bit of flags

=head2 flag_execinstr

Read/write accessor for execinstr bit of flags

=head2 addr

The address in the process's memory where this section gets loaded, or zero if
it doesn't.

=head2 offset

Location within the ELF file where this section is located.

=head2 size

Size (in bytes) of the section within the ELF file.  If the type of the
section is 'nobits' then this field is ignored and the section does not
occupy bytes of the ELF file.

=head2 link

Reference to another section, as an index into the section table.
Meaning depends on section type.

=head2 info

Extra info, depending on section type.

=head2 addralign

Required alignment for the 'addr' field.  'Addr' must be a multiple of this
value.  Values 0 and 1 both mean no alignment is required.

=head2 entsize

If the section holds a table of fixed-size entries, this is the size of each
entry.  Set to 0 otherwise.

=cut

has name        => ( is => 'rw' );

ELF::Writer::_enum_attribute(\&has, 'type',
	\&ELF::Writer::SectionTypeEnum_encode, \&ELF::Writer::SectionTypeEnum_decode);

has flags       => ( is => 'rw' );
sub flag_write {
	my ($self, $value)= @_;
	$self->flags( $self->flags & ~1 | ($value? 1 : 0) )
		if defined $value;
	$self->flags & 1;
}
sub flag_alloc {
	my ($self, $value)= @_;
	$self->flags( $self->flags & ~2 | ($value? 2 : 0) )
		if defined $value;
	$self->flags & 2;
}
sub flag_execinstr {
	my ($self, $value)= @_;
	$self->flags( $self->flags & ~4 | ($value? 4 : 0) )
		if defined $value;
	$self->flags & 4;
}

has addr        => ( is => 'rw' );    
has offset      => ( is => 'rw' );
has size        => ( is => 'rw' );
has link        => ( is => 'rw' );
has info        => ( is => 'rw' );
has addralign   => ( is => 'rw' );
has entsize     => ( is => 'rw' );

=head1 ATTRIBUTES (user)

=head2 data

The data bytes of this section

=head2 data_start

Use this attribute to introduce padding between the start of the section and
the offset where your 'data' should be written.  This is mainly of use for
segments, but provided on sections for symmetry.

=cut

has data        => ( is => 'rw' );
has data_start  => ( is => 'rw', default => sub { 0 } );

sub data_offset { $_[0]->offset + $_[0]->data_start }

has _index => ( is => 'rw' );
sub _name { "segment ".shift->_index }
sub _required_file_alignment { $_[0]->addralign || 1 }

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

sub BUILD {
	my ($self, $params)= @_;
	defined $params->{flag_write}
		and $self->flag_write($params->{flag_write});
	defined $params->{flag_alloc}
		and $self->flag_alloc($params->{flag_alloc});
	defined $params->{flag_execinstr}
		and $self->flag_execinstr($params->{flag_execinstr});
}

sub coerce {
	my ($class, $thing)= @_;
	return (ref $thing && ref($thing)->isa(__PACKAGE__))? $thing : $class->new($thing);
}

sub clone {
	my $self= shift;
	return bless { %$self }, ref $self;
}

1;
