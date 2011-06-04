my class Mu {
    method new() {
        self.bless(self.CREATE());
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
    
    method bless(Mu $candidate) {
        $candidate
    }
    
    method item() { self }
    
    method say() { say(self) }
}
