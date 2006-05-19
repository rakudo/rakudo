## $Id: builtins.pir 12709 2006-05-17 01:42:08Z pmichaud $

=head1 NAME

src/builtins/string.pir - Perl6 builtin string functions

=head1 Functions

=over 4

=cut

.namespace [ "" ]

.sub 'substr'
    .param string x
    .param int start
    .param int len
    $S0 = substr x, start, len
    .return ($S0)
.end


.sub 'split' :multi(String, String)
    .param string sep
    .param string target
    $P0 = split sep, target
    $P0 = 'list'($P0 :flat)
    .return ($P0)
.end


=back

=cut


## vim: expandtab sw=4
