my class Iterator is Iterable {

    method iterator() { pir::perl6_decontainerize__PP(self) }
    method list() { (self,).list }
    method flat() { (self,).flat }

}
