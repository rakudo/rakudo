augment class Parcel does Positional {
    method Str() { self.flat.Str }

    method elems() { self.flat.elems }

    multi method postcircumfix:<[ ]>($index) { self.flat.[$index] }
    multi method postcircumfix:<[ ]>(@index) { self.flat.[@index] }
}
