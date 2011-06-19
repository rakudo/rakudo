class MapIter { ... }
class Parcel { ... }

my class Any {

    ########
    # List-like methods for Any.
    ########

    method flat() { self.list.flat }
    method list() { (self,).list }
    method elems() { self.list.elems }

    method join($separator = ' ') {
        my $list = (self,).flat.eager;
        my Mu $rsa := pir::new__Ps('ResizableStringArray');
        pir::push__vPS($rsa, pir::repr_unbox_str__SP($list.shift.Stringy)) 
            while $list.gimme(0);
        pir::push__vPS($rsa, '...') if $list.infinite;
        pir::perl6_box_str__PS(
            pir::join(pir::repr_unbox_str__SP($separator.Stringy), $rsa))
    }

    method map($block) is rw {
        MapIter.new(:list(self.flat), :block($block)).list
    }
         
    proto method postcircumfix:<[ ]>(|$) { * }
    multi method postcircumfix:<[ ]>($pos) is rw {
        fail "Cannot use negative index $pos on {self.WHAT.perl}" if $pos < 0;
        self.at_pos($pos)
    }
    # XXX: Temporary hack to allow slicing; the below method(s) should be
    #      (@pos) when List, Range, Parcel, etc. "does Positional".
    multi method postcircumfix:<[ ]>(Iterable \$pos) {
        $pos.map({self[$_]}).Parcel
    }
    multi method postcircumfix:<[ ]>(Parcel \$pos) { self[$pos.flat] }

    ########
    # Hash-like methods for Any.
    ########
    proto method postcircumfix:<{ }>(|$) { * }
    multi method postcircumfix:<{ }>(\$key) is rw {
        self.at_key($key)
    }

}
