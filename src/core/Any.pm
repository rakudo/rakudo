class MapIter { ... }

my class Any {

    # List-like methods for Any.
    method flat() { self.list.flat }
    method list() { (self,).list }
    method elems() { self.list.elems }

    method join($separator = ' ') {
        my $list = (self,).flat.eager;
        my Mu $rsa := pir::new__Ps('ResizableStringArray');
        pir::push__vPS($rsa, pir::repr_unbox_str__SP($list.shift.Stringy)) 
            while $list;
        pir::perl6_box_str__PS(
            pir::join(pir::repr_unbox_str__SP($separator.Stringy), $rsa))
    }

    method map($block) is rw {
        MapIter.new(:list(self.flat), :block($block)).list
    }
         
    proto method postcircumfix:<[ ]>(|$) { * }
    multi method postcircumfix:<[ ]>(\$pos) is rw {
        self.at_pos($pos)
    }

    # Hash-like methods for Any.

    proto method postcircumfix:<{ }>(|$) { * }
    multi method postcircumfix:<{ }>(\$key) is rw {
        self.at_key($key)
    }

}
