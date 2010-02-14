.sub '' :anon :init :load

    # Numeric constants
    .local pmc pi, e, i
    pi = new ['Num']
    assign pi, 3.14159265358979323846264338327950288
    set_hll_global ['Numeric'], 'pi', pi
    set_hll_global 'pi', pi

    e  = new ['Num']
    assign e,  2.71828182845904523536028747135266249
    set_hll_global ['Numeric'], 'e', e
    set_hll_global 'e', e

.end

