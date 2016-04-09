package ELF::Writer::Linux_x86_64;
use Moo 2;
use namespace::clean;
extends 'ELF::Writer';

# ABSTRACT: ELF::Writer with defaults for Linux on x86-64

=head1 DESCRPTION

This module is the same as L<ELF::Writer>, but supplies the following defaults
when constructed:

=head2 ELF attributes

=over 16

=item class

C<"64-bit">

=item data

C<"little-endian">

=item osabi

C<"Linux">

=item machine

C<"x86-64">

=back

=cut

has '+class'    => ( default => sub { 2 } );
has '+data'     => ( default => sub { 1 } );
has '+osabi'    => ( default => sub { 3 } );
has '+machine'  => ( default => sub { 0x3E } );

=head2 Segment attributes

=over 16

=item align

4096

=back

=cut

sub _apply_segment_defaults {
	my ($self, $seg)= @_;
	$seg->align(4096) unless $seg->align;
	$self->SUPER::_apply_segment_defaults($seg);
}

1;
