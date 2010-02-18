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

    .local pmc Radians, Degrees, Gradians, Circles
    Radians = new ['Int']
    assign Radians, 0
    set_hll_global ['Numeric'], 'Radians', Radians
    set_hll_global 'Radians', Radians

    Degrees = new ['Int']
    assign Degrees, 1
    set_hll_global ['Numeric'], 'Degrees', Degrees
    set_hll_global 'Degrees', Degrees

    Gradians = new ['Int']
    assign Gradians, 2
    set_hll_global ['Numeric'], 'Gradians', Gradians
    set_hll_global 'Gradians', Gradians

    Circles = new ['Int']
    assign Circles, 3
    set_hll_global ['Numeric'], 'Circles', Circles
    set_hll_global 'Circles', Circles

.end

