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

.sub 'IN_EVAL'
    .local pmc interp
    .local int level
    .local int result
    .local pmc eval

    result = 0
    level  = 0
    interp = getinterp
    eval = get_hll_global '&eval'
    if null eval goto done
    eval = getattribute eval, '$!do'

    # interp[sub;$to_high_level] throws an exception
    # so when we catch one, we're done walking the call chain
    push_eh done
  loop:
    inc level
    $P0 = interp['sub'; level]
    if null $P0 goto done
    eq_addr $P0, eval, has_eval
    goto loop

  has_eval:
    inc result

  done:
    $P0 = box result
    .return($P0)
.end

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
    $P1 = shift args      # the first arg is the program name
    set_hll_global '$PROGRAM_NAME', $P1

    # The remaining args strings belong in @*ARGS
    $P1 = new ['Parcel']
    splice $P1, $P0, 0, 0
    $P2 = new ['Array']
    $P2.'!STORE'($P1)
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


.sub '!UNIT_START'
    .param pmc unit
    .param pmc args            :optional

    .local int main_flag
    main_flag = 0
    # if we have args, we know it's the mainline
    unless null args goto mainline_start
    # otherwise, if the caller has an outer_ctx, it's an eval
    .local pmc outer_ctx
    $P0 = getinterp
    $P0 = $P0["context";1]
    outer_ctx = getattribute $P0, "outer_ctx"
    if null outer_ctx goto module_start
  eval_start:
    # it's an eval, set the outer_ctx of the unit, execute it, and return
    unit.'set_outer_ctx'(outer_ctx)
    '!fire_phasers'('INIT')
    $P0 = unit(0)
    .return ($P0)
  mainline_start:
    # initialize global vars
    '!GLOBAL_VARS'(args)
    main_flag = 1
  module_start:
    # run the UNIT from inside of the setting
    '!fire_phasers'('INIT')
    $P0 = '!YOU_ARE_HERE'(unit, main_flag)
    .return ($P0)
.end

