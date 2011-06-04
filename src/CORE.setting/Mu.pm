my class Mu {
    method new() {
        self.bless(self.CREATE());
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
