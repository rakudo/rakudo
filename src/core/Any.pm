class MapIter { ... }
class Parcel { ... }

my class Any {

    ########
    # List-like methods for Any.
    ########

    method eager() { nqp::p6list(nqp::list(self), List, Bool::True).eager }
    method elems() { self.list.elems }
    method infinite() { Mu }
    method flat() { nqp::p6list(nqp::list(self), List, Bool::True) }
    method hash() { my %h = self }
    method list() { nqp::p6list(nqp::list(self), List, Mu) }
    method pick($n = 1) { self.list.pick($n) }
    method roll($n = 1) { self.list.roll($n) }
    method reverse() { self.list.reverse }
    method sort($by = &infix:<cmp>) { self.list.sort($by) }

    method Array() { Array.new(self.flat) }

    method grep(Mu $test) is rw {
        self.map({ $_ if $_ ~~ $test });
    }

    method join($separator = '') {
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
    multi method postcircumfix:<[ ]>() { self.list }
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
    multi method postcircumfix:<[ ]>(WhateverCode $block) is rw {
        self[$block(|(self.elems xx $block.count))]
    }
    multi method postcircumfix:<[ ]>(Whatever) is rw {
        self[^self.elems]
    }

    method at_pos($pos) is rw {
        if self.defined {
            fail ".[$pos] out of range for type {self.perl}" if $pos != 0;
            return self;
        }
    }

    ########
    # Hash-like methods for Any.
    ########
    proto method postcircumfix:<{ }>(|$) { * }
    multi method postcircumfix:<{ }>() { self.values }
    multi method postcircumfix:<{ }>($key) is rw {
        self.at_key($key)
    }
    multi method postcircumfix:<{ }>(Positional $key) {
        $key.map({ self{$_ } }).eager.Parcel
    }
    multi method postcircumfix:<{ }>(Whatever) {
        self{self.keys}
    }

}


proto infix:<cmp>($, $) { * }
multi infix:<cmp>(\$a, \$b) { 
    return -1 if $a == -$Inf || $b == $Inf;
    return  1 if $a ==  $Inf || $b == -$Inf;
    $a.Stringy cmp $b.Stringy 
}

proto infix:<before>(|$)       { * }
multi infix:<before>($x?)      { Bool::True }
multi infix:<before>(\$a, \$b) { ($a cmp $b) < 0 }

proto infix:<after>(|$)        { * }
multi infix:<after>($x?)       { Bool::True }
multi infix:<after>(\$a, \$b)  { ($a cmp $b) > 0 }

proto infix:<===>($a?, $b?)    { * }
multi infix:<===>($a?)         { Bool::True }
multi infix:<===>($a, $b)      { $a.WHICH === $b.WHICH }

proto sub infix:<eqv>($, $) { * }
multi sub infix:<eqv>($a, $b) {
    $a.WHAT === $b.WHAT && ($a cmp $b) == 0;
}


# XXX: should really be '$a is rw' (no \) in the next four operators
proto prefix:<++>(|$)             { * }
multi prefix:<++>(Mu:D \$a is rw) { $a = $a.succ }
multi prefix:<++>(Mu:U \$a is rw) { $a = 1 }
proto prefix:<-->(|$)             { * }
multi prefix:<-->(Mu:D \$a is rw) { $a = $a.pred }
multi prefix:<-->(Mu:U \$a is rw) { $a = -1 }

proto postfix:<++>(|$)             { * }
multi postfix:<++>(Mu:D \$a is rw) { my $b = $a; $a = $a.succ; $b }
multi postfix:<++>(Mu:U \$a is rw) { $a = 1; 0 }
proto postfix:<-->(|$)             { * }
multi postfix:<-->(Mu:D \$a is rw) { my $b = $a; $a = $a.pred; $b }
multi postfix:<-->(Mu:U \$a is rw) { $a = -1; 0 }

proto infix:<min>(|$)     { * }
multi infix:<min>(*@args) { @args.min }

proto infix:<max>(|$)     { * }
multi infix:<max>(*@args) { @args.min }

proto map(|$) {*}
multi map(&code, *@values) { @values.map(&code) }

proto grep(|$) {*}
multi grep(Mu $test, *@values) { @values.grep($test) }

proto join(|$) { * }
multi join($sep = '', *@values) { @values.join($sep) }

proto pick(|$) { * }
multi pick($n, *@values) { @values.pick($n) }

proto roll(|$) { * }
multi roll($n, *@values) { @values.roll($n) }
