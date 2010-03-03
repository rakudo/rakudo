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
# .include 'interpinfo.pasm'
.include 'iglobals.pasm'

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

    # Set up @*INC from $PERL6LIB, languages/perl6/lib and ~/.perl6/lib
    .local pmc env, interp, config
    # Convert PERL6LIB first
    env = root_new ['parrot';'Env']
    $S0 = env['PERL6LIB']
    $P0 = split ':', $S0
    # Now prepend the installed Parrot languages/perl6/lib directory
    interp = getinterp
    config = interp[.IGLOBALS_CONFIG_HASH]
    $S0 = config['libdir']
    $S1 = config['versiondir']
    concat $S0, $S1
    concat $S0, '/languages/perl6/lib'
    unshift $P0, $S0
    # Now prepend ~/.perl6/lib
    $S0 = env['HOME']
    if $S0 goto have_home     # for users of unix-y systems
    # here only for those of a fenestral persuasion
    $S0 = env['HOMEDRIVE']
    $S1 = env['HOMEPATH']
    concat $S0, $S1
  have_home:
    concat $S0, '/.perl6/lib'
    unshift $P0, $S0
    push $P0, '.'
    # $P0 now has all the directories, move them to @*INC
    $P1 = new ['Parcel']
    # do not use '&circumfix:<[ ]>' because it makes a list of lists
    splice $P1, $P0, 0, 0
    $P2 = new ['Array']
    $P2.'!STORE'($P1)
    set_hll_global '@INC', $P2

    # Turn the env PMC into %*ENV (just read-only so far)
    $P2 = '&CREATE_HASH_LOW_LEVEL'(env)
    set_hll_global '%ENV', $P2

    # INIT time
    '!fire_phasers'('INIT')
    $P0 = mainline()
    .return ($P0)
.end
