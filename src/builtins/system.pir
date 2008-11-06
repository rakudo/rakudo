## $Id $

=head1 NAME

src/builtins/system.pir - Perl6 OS-related functions

=head1 Functions

=over 4

=cut

.namespace []
## TODO: should these be in a namespace?
## .namespace [ '???' ]

=item run

our Proc::Status multi run ( ; Str $command )
our Proc::Status multi run ( ; Str $path, *@args )
our Proc::Status multi run ( Str @path_and_args )

The versions below do not return a C<Proc::Status> object, but instead
return the status code from the C<spawnw> opcode.

=cut

.sub 'run' :multi(Perl6Str)
    .param string cmd
    .local int retval

    spawnw retval, cmd
    .return (retval)
.end

.sub 'run' :multi(Perl6Str,List)
    .param string path
    .param pmc args :slurpy
    .local int retval

    unshift args, path
    spawnw retval, args
    .return (retval)
.end

.sub 'run' :multi(List)
    .param pmc path_and_args
    .local int retval

    spawnw retval, path_and_args
    .return (retval)
.end

=back

=cut


# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4 ft=pir:
