my class Iterable {
    # Has parent Cool declared in BOOTSTRAP

    method elems()    { self.list.elems }
    method infinite() { Mu }
    method item($self:) { $self }

    method Int()      { self.elems }
    method Num()      { self.elems.Num }
    method Numeric()  { self.elems }
    multi method Str(Iterable:D:) { self.list.Str }
}
