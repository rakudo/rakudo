my class Iterator {
    # declared in BOOTSTRAP.pm
    #    is Iterable;          # parent class

    method iterator() { pir::perl6_decontainerize__PP(self) }
    method list() { (self,).list }
    method flat() { (self,).flat }

}
