## $Id: builtins.pir 12709 2006-05-17 01:42:08Z pmichaud $

=head1 NAME

src/builtins/io.pir - Perl6 builtins for I/O

=head1 Functions

=over 4

=cut

.namespace

.sub 'print'
    .param pmc list            :slurpy
    .local pmc iter

    iter = new .Iterator, list
  iter_loop:
    unless iter goto iter_end
    $P0 = shift iter
    print $P0
    goto iter_loop
  iter_end:
    .return (1)
.end


.sub 'say'
    .param pmc list            :slurpy
    'print'(list :flat)
    print "\n"
    .return (1)
.end


=back

=cut


## vim: expandtab sw=4
