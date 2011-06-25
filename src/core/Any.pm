class MapIter { ... }
class Parcel { ... }

my class Any {

    ########
    # List-like methods for Any.
    ########

    method eager() { nqp::p6list(List, nqp::list(self), 1.Bool).eager }
    method flat() { nqp::p6list(List, nqp::list(self), 1.Bool) }
    method list() { nqp::p6list(List, nqp::list(self), Mu) }
    method elems() { self.list.elems }
    method infinite() { Mu }

    method join($separator = ' ') {
        my $list = (self,).flat.eager;
        my Mu $rsa := pir::new__Ps('ResizableStringArray');
        pir::push__vPS($rsa, nqp::unbox_s($list.shift.Stringy)) 
            while $list.gimme(0);
        pir::push__vPS($rsa, '...') if $list.infinite;
        nqp::p6box_s(pir::join(nqp::unbox_s($separator.Stringy), $rsa))
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


proto sub infix:<cmp>(|$) { * }
multi sub infix:<cmp>(\$a, \$b) { $a.Stringy cmp $b.Stringy }

# XXX: should really be '$a is rw' (no \) in the next four operators
proto prefix:<++>(|$) { * }
multi prefix:<++>(Mu:D \$a is rw) { $a = $a.succ }
multi prefix:<++>(Mu:U \$a is rw) { $a = 1 }
proto prefix:<-->(|$) { * }
multi prefix:<-->(Mu:D \$a is rw) { $a = $a.pred }
multi prefix:<-->(Mu:U \$a is rw) { $a = -1 }


proto postfix:<++>(|$) { * }
multi postfix:<++>(Mu:D \$a is rw) { my $b = $a; $a = $a.succ; $b }
multi postfix:<++>(Mu:U \$a is rw) { $a = 1; 0 }
proto postfix:<-->(|$) { * }
multi postfix:<-->(Mu:D \$a is rw) { my $b = $a; $a = $a.pred; $b }
multi postfix:<-->(Mu:U \$a is rw) { $a = -1; 0 }

