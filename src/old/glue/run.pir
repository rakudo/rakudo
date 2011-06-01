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
 .include 'interpinfo.pasm'
 .include 'sysinfo.pasm'
.include 'iglobals.pasm'

.sub '!GLOBAL_VARS'
    .param pmc args

    .local string info
    .local pmc true
    true = get_hll_global 'True'

    info = interpinfo .INTERPINFO_EXECUTABLE_FULLNAME
    $P0 = new ['Str']
    $P0 = info
    set_hll_global ['PROCESS'], '$EXECUTABLE_NAME', $P0

    # The first args string belongs in $*PROGRAM_NAME
    args = clone args
    if args goto have_args
    unshift args, 'interactive'
  have_args:
    $P1 = shift args      # the first arg is the program name
    set_hll_global '$PROGRAM_NAME', $P1

    # The remaining args strings belong in @*ARGS
    $P1 = new ['Parcel']
    splice $P1, $P0, 0, 0
    $P2 = new ['Array']
    $P2.'!STORE'(args)
    set_hll_global '@ARGS', $P2
    setprop $P2, "rw", true

    ##  set up $*ARGFILES
    $P3 = get_hll_global ['IO'], 'ArgFiles'
    $P3 = $P3.'new'('args'=>$P2)
    set_hll_global '$ARGFILES', $P3

    # Turn the env PMC into %*ENV (just read-only so far)
    .local pmc env
    env = root_new ['parrot';'Env']
    $P2 = '&CREATE_HASH_FROM_LOW_LEVEL'(env)
    set_hll_global '%ENV', $P2
.end


.sub '!UNIT_OUTER'
    .param pmc unit
    $P0 = find_dynamic_lex '%*COMPILING'
    if null $P0 goto outer_setting
    $P0 = $P0['%?OPTIONS']
    if null $P0 goto outer_setting
    $P0 = $P0['outer_ctx']
    if null $P0 goto outer_setting
    unit.'set_outer_ctx'($P0)
    goto done
  outer_setting:
    $P0 = find_name '!YOU_ARE_HERE'
    if null $P0 goto done
    unit = $P0(unit)
  done:
    .return (unit)
.end


.sub '!UNIT_START'
    .param pmc unit
    .param pmc args            :optional

    # if unit already has an outer_ctx, this is an eval
    .local pmc outer_ctx
    $P0 = getinterp
    $P0 = $P0["context";1]
    outer_ctx = getattribute $P0, "outer_ctx"
    unless null outer_ctx goto eval_start
    # if no args were supplied, it's a module load via :load
    if null args goto module_start
    # if any args were supplied, it's a mainline start
    if args goto mainline_start
    # if we're in interactive mode, it's a mainline start
    $P0 = find_dynamic_lex '$*CTXSAVE'
    if null $P0 goto module_start
    $I0 = can $P0, "ctxsave"
    unless $I0 goto module_start
  mainline_start:
    '!GLOBAL_VARS'(args)
    '!fire_phasers'('INIT')
    $P0 = '!YOU_ARE_HERE'(unit)
    $P0 = $P0(1)
    .return ($P0)
  module_start:
    '!fire_phasers'('INIT')
    $P0 = '!YOU_ARE_HERE'(unit)
    $P0 = $P0(0)
    .return ($P0)
  eval_start:
    '!fire_phasers'('INIT')
    $P0 = unit(0)
    .return ($P0)
.end
