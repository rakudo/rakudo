augment class Parcel does Positional {
    method elems() { self.flat.elems }

    multi method postcircumfix:<[ ]>($index) { self.flat.[$index] }
    multi method postcircumfix:<[ ]>(@index) { self.flat.[@index] }
}
