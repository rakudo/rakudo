our sub eval(Str $code) {
    Q:PIR {
        .local pmc interp, caller, code, pbc, result, exception, parrotex
        interp = getinterp
        caller = interp['context';1]
        push_eh catch
        $P0 = compreg 'perl6'
        code = find_lex '$code'
        pbc = $P0.'compile'(code, 'outer_ctx'=>caller)

        # set the outer context for the compiled code
        $P1 = pbc[0]
        $P2 = getattribute caller, 'current_sub'
        $P1.'set_outer'($P2)

        # invoke the compiled code
        result = pbc()

        # no exception occurred, so generate a dummy exception
        exception = '!FAIL'()
        goto done
      catch:
        .get_results (parrotex)
        exception = new ['Perl6Exception']
        setattribute exception, '$!exception', parrotex
      done:
        pop_eh
        $P0 = interp['lexpad';1]
        $P0['$!'] = exception
        unless null result goto have_result
        result = '&Nil'()
      have_result:
        %r = result
    };
}
