my class Mu {
    method new() {
        self.bless(self.CREATE());
    }
    
    proto method ACCEPTS(|$) { * }
    multi method ACCEPTS(Mu:U: \$topic) {
        pir::perl6_booleanize__PI(pir::type_check__IPP($topic, self))
    }
    
    method Bool() {
        self.defined
    }
    
    method defined() {
        pir::perl6_booleanize__PI(pir::repr_defined__IP(self))
    }
    
    method CREATE() {
        pir::repr_instance_of__PP(self.WHAT)
    }
    
    method bless(Mu \$candidate) {
        $candidate
    }
    
    proto method Str(|$) { * }
    multi method Str(Mu:U:) {
        self.HOW.name(self) ~ '()'
    }

    proto method Stringy(|$) { * }
    multi method Stringy(Mu:U:) {
        ''   # TODO: should be a warning of some sort
    }
    multi method Stringy(Mu:D:) { self.Str }
    
    method item() { self }
    
    method say() { say(self) }
}
