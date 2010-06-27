our sub eval(Str $code, :$lang = 'perl6') {
    my $*IN_EVAL = 1;
    Q:PIR {
        .local pmc interp, caller, code, pbc, result, exception, parrotex
        .local string lang
        interp = getinterp
        caller = interp['context';1]
        code = find_lex '$code'
        push_eh catch
        $P0 = find_lex '$lang'
        lang = $P0
        if lang != 'perl6' goto foreign
        $P0 = compreg 'perl6'
        pbc = $P0.'compile'(code, 'outer_ctx'=>caller)

        # set the outer context for the compiled code
        $P1 = pbc[0]
        $P2 = getattribute caller, 'current_sub'
        $P1.'set_outer'($P2)

        # invoke the compiled code
        result = pbc()
        goto success

      foreign:
        # Load the langauge and delegate to its eval.
        load_language lang
        $P0 = compreg lang
        result = $P0.'eval'(code, 'outer_ctx'=>caller)

      success:
        pop_eh
        # no exception occurred, so generate a dummy exception
        exception = get_hll_global 'Any'
        goto done
      catch:
        .get_results (parrotex)
        pop_eh
        exception = new ['Perl6Exception']
        setattribute exception, '$!exception', parrotex
        result = '!FAIL'(parrotex)
      done:
        $P0 = interp['lexpad';1]
        $P1 = $P0['$!']
        $P2 = new ['Perl6Scalar'], exception
        setprop $P2, 'scalar', $P2
        setprop $P2, 'rw', $P2
        copy $P1, $P2
        unless null result goto have_result
        result = '&Nil'()
      have_result:
        %r = result
    };
}
