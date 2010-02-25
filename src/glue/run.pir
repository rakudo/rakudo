=head1 NAME

src/glue/run.pir - code to initiate execution of a Perl 6 program

=head2 Subs

=over 4

=item !UNIT_START(mainline, args)

Invoke the code given by mainline, using C<args> as the initial
(command-line) arguments.  The method C<comp_unit($/)> in
F<Perl6/Actions.pm> generates two calls to this sub, one for
executables and one for libraries, and pushes them into the AST
of the compilation unit.

=cut

.namespace []
.sub '!UNIT_START'
    .param pmc mainline
    .param pmc args            :slurpy

    # Ignore the args when executed as a library (not main program)
    unless args goto unit_start_0

    # args    is a ResizablePMCArray containing only one entry
    # args[0] is also a ResizablePMCArray, of String entries, containing 
    #         the program name or '-e' in args[0][0], followed by
    #         optional command line arguments in args[0][1] etc.
    $P0 = args[0]
    # Ignore the args when executed as a library (not main program)
    unless $P0 goto unit_start_0

    # The first args string belongs in $*PROGRAM_NAME
    $P1 = shift $P0      # the first arg is the program name
    set_hll_global '$PROGRAM_NAME', $P1

    # The remaining args strings belong in @*ARGS
    $P1 = new ['Parcel']
    splice $P1, $P0, 0, 0
    $P2 = new ['Array']
    $P2.'!STORE'($P1)
    set_hll_global '@ARGS', $P2
  unit_start_0:

    # INIT time
    '!fire_phasers'('INIT')
    $P0 = mainline()
    .return ($P0)
.end
