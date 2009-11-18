augment class Num {
    our Str multi method Str() {
        ~self
    }
}

INIT {
    pir::set_hll_global__vSP('NaN', pir::set__NS('NaN'));
    pir::set_hll_global__vSP('Inf', pir::set__NS('Inf'));
    pir::set_hll_global__vSP('+Inf', pir::set__NS('Inf'));
    pir::set_hll_global__vSP('-Inf', -pir::set__NS('Inf'));
}
