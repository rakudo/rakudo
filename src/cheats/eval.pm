our sub eval(Str $code) {
    Q:PIR {
        .local pmc interp, caller, code, result, exception, parrotex
        interp = getinterp
        caller = interp['context';1]
        push_eh catch
        $P0 = compreg 'perl6'
        code = find_lex '$code'
        result = $P0.'eval'(code, 'outer_ctx'=>caller)
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
