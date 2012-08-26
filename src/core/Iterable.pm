my class Iterable {
    method elems()    { self.list.elems }
    method infinite() { Mu }
    method item($self:) { $self }
    
    method fmt($format = '%s', $separator = ' ') {
        self.list.fmt($format, $separator)
    }

    method Int()      { self.elems }
    method Num()      { self.elems.Num }
    multi method Numeric(Iterable:D:)  { self.elems }
    multi method Str(Iterable:D:)      { self.list.Str }
    method chrs(Iterable:D:) {
        self>>.chr.join;
    }
}
