augment class Num {
    our Str multi method Str() {
        ~self
    }
}

INIT {
    pir::set_hll_global__vSP('NaN', pir::set__NS('NaN'));
        pir::set_hll_global__vSP('NaN', Q:PIR {
        %r = new ['Num']
        $N0 = 'NaN'
        assign %r, $N0
    });
    pir::set_hll_global__vSP('Inf', Q:PIR {
        %r = new ['Num']
        $N0 = 'Inf'
        assign %r, $N0
    });
    pir::set_hll_global__vSP('+Inf', Q:PIR {
        %r = new ['Num']
        $N0 = 'Inf'
        assign %r, $N0
    });
    pir::set_hll_global__vSP('-Inf', -pir::get_hll_global__PS('Inf'));
}
