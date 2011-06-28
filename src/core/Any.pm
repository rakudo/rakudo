class MapIter { ... }
class Parcel { ... }

my class Any {

    ########
    # List-like methods for Any.
    ########

    method eager() { nqp::p6list(nqp::list(self), List, 1.Bool).eager }
    method flat() { nqp::p6list(nqp::list(self), List, 1.Bool) }
    method list() { nqp::p6list(nqp::list(self), List, Mu) }
    method elems() { self.list.elems }
    method infinite() { Mu }

    method grep(Mu $test) is rw {
        gather { self.map({ take $_ if $_ ~~ $test }) }
    }

    method join($separator = ' ') {
        my $list = (self,).flat.eager;
        my Mu $rsa := pir::new__Ps('ResizableStringArray');
        nqp::push($rsa, nqp::unbox_s($list.shift.Stringy)) 
            while $list.gimme(0);
        nqp::push($rsa, '...') if $list.infinite;
        nqp::p6box_s(nqp::join(nqp::unbox_s($separator.Stringy), $rsa))
    }

    method map($block) is rw {
        MapIter.new(:list((self,).flat), :block($block)).list
    }

    method min($by = { $^a cmp $^b }) {
        my $cmp = $by.arity == 2 ?? $by !! { $by($^a) cmp $by($^b) }
        my $min = +$Inf;
        for self { 
            $min = $_ if .defined && $_ cmp $min < 0;
        }
        $min;
    }

         
    proto method postcircumfix:<[ ]>(|$) { * }
    multi method postcircumfix:<[ ]>($pos) is rw {
        fail "Cannot use negative index $pos on {self.WHAT.perl}" if $pos < 0;
        self.at_pos($pos)
    }
    multi method postcircumfix:<[ ]>(Positional $pos) is rw {
        my $list = $pos.flat;
        $list.gimme(*);
        $list.map($list.infinite
                   ?? { last if $_ >= self.gimme($_ + 1); self[$_] }
                   !! { self[$_] }).eager.Parcel;
    }

    ########
    # Hash-like methods for Any.
    ########
    proto method postcircumfix:<{ }>(|$) { * }
    multi method postcircumfix:<{ }>(\$key) is rw {
        self.at_key($key)
    }

}


proto sub infix:<cmp>($, $) { * }
multi sub infix:<cmp>(\$a, \$b) { 
    return -1 if $a == -$Inf || $b == $Inf;
    return  1 if $a ==  $Inf || $b == -$Inf;
    $a.Stringy cmp $b.Stringy 
}

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

