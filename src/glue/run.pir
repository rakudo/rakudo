=head1 NAME

src/glue/run.pir - code to initiate execution of a Perl 6 program

=head2 Subs

=over 4

=item !UNIT_START(mainline, args)

Invoke the code given by mainline, using C<args> as the initial
(command-line) arguments.  

=cut

.namespace []
.sub '!UNIT_START'
    .param pmc mainline
    .param pmc args            :slurpy

    # INIT time
    '!fire_phasers'('INIT')
    $P0 = mainline()
    .return ($P0)
.end
