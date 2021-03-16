class OperatorProperties {
    has str $.precedence;
    has str $.associative;
    has str $.thunky;
    has int $.iffy;

    method new(str :$precedence, str :$associative, str :$thunky, int :$iffy) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj,OperatorProperties,'$!precedence',
          $precedence // (nqp::defined(self) ?? $!precedence !! ""));
        nqp::bindattr_s($obj,OperatorProperties,'$!associative',
          $associative // (nqp::defined(self) ?? $!associative !! ""));
        nqp::bindattr_s($obj,OperatorProperties,'$!thunky',
          $thunky // (nqp::defined(self) ?? $!thunky !! ""));
        nqp::bindattr_i($obj,OperatorProperties,'$!iffy',
          $iffy // (nqp::defined(self) ?? $!iffy !! 0));

        $obj
    }

    # Accessors
    method precedence()  { nqp::defined(self) ?? $!precedence  !! "" }
    method associative() { nqp::defined(self) ?? $!associative !! "" }
    method thunky()      { nqp::defined(self) ?? $!thunky      !! "" }
    method iffy()        { nqp::defined(self) ?? $!iffy        !! 0  }

    # convenience methods
    method chaining() {
        nqp::defined(self) ?? False !! $!associative eq 'chain'
    }
    method short-circuit() {
        nqp::defined(self) ?? False !! $!thunky ne ""
    }

    # Return properties depending on other properties
    method equiv(str $associative) {
        nqp::defined(self)
          ?? self.new(
               precedence  => $!precedence,
               associative => $associative,
               thunky      => $!thunky,
               iffy        => $!iffy
             )
          !! self.new
    }

    method tighter(str $associative) {
        nqp::defined(self)
          ?? self.new(
               precedence  => nqp::join('@=',nqp::split('=',$!precedence)),
               associative => $associative,
               thunky      => $!thunky,
               iffy        => $!iffy
             )
          !! nqp::die("No precedence found to be tighter for")
    }

    method looser(str $associative) {
        nqp::defined(self)
          ?? self.new(
               precedence  => nqp::join(':=',nqp::split('=',$!precedence)),
               associative => $associative,
               thunky      => $!thunky,
               iffy        => $!iffy
             )
          !! nqp::die("No precedence found to be looser for")
    }

    # Return name of handler for reducing with these operator properties
    method reducer-name() {
        nqp::defined(self)
          ?? nqp::iseq_s($!precedence,'f=')
            ?? '&METAOP_REDUCE_LISTINFIX'
            !! nqp::chars($!associative)
              ?? nqp::iseq_s($!associative,'chain')
                ?? '&METAOP_REDUCE_CHAIN'
                !! nqp::iseq_s($!associative,'list')
                  ?? '&METAOP_REDUCE_LIST'
                  !! nqp::iseq_s($!associative,'right')
                    ?? '&METAOP_REDUCE_RIGHT'
                    !! '&METAOP_REDUCE_LEFT'    # assume 'left' or 'non'
              !! '&METAOP_REDUCE_LEFT'
          !! '&METAOP_REDUCE_LEFT'
    }

    # Old interface method
    method prec(str $key?) {
        if nqp::defined(self) {
            if $key {
                $key eq 'prec'
                  ?? $!precedence
                  !! $key eq 'assoc'
                    ?? $!associative
                    !! $key eq 'thunky'
                      ?? $!thunky
                      !! $key eq 'iffy'
                        ?? $!iffy
                        !! nqp::null
            }
            else {
                my $hash := nqp::hash;
                nqp::bindkey($hash,'prec',  $!precedence)  if $!precedence;
                nqp::bindkey($hash,'assoc', $!associative) if $!associative;
                nqp::bindkey($hash,'thunky',$!thunky)      if $!thunky;
                nqp::bindkey($hash,'iffy',  $!iffy)        if $!iffy;
                $hash
            }
        }

        # called on type object
        else {
            if $key {
                $key eq 'prec' || $key eq 'assoc' || $key eq 'thunky'
                  ?? ""
                  !! $key eq 'iffy'
                    ?? 0
                    !! nqp::null
            }
            else {
                nqp::hash
            }
        }
    }

    method properties-for-infix(str $op) {
        my $properties := nqp::hash();

        my $multiplicative := self.new(:precedence<u=>, :associative<left>);
        for '*', '/', 'div', 'gcd', 'lcm', '%', 'mod', '+&', '~&', '?&' {
            nqp::bindkey($properties,$_,$multiplicative);
        }

        my $additive := self.new(:precedence<t=>, :associative<left>);
        for < + - +| +^ ~| ~^ ?| ?^ > {
            nqp::bindkey($properties,$_,$additive);
        }

        my $exponentiation := self.new(:precedence<w=>, :associative<right>);
        nqp::bindkey($properties,'**',$exponentiation);

        my $iffy := self.new(:precedence<u=>, :associative<left>, :iffy);
        nqp::bindkey($properties,'%%',$iffy);

        my $replication := self.new(:precedence<s=>, :associative<left>);
        nqp::bindkey($properties,'x',$replication);

        my $replication-xx := self.new(:precedence<s=>, :associative<left>, :thunky<t.>);
        nqp::bindkey($properties,'xx',$replication-xx);

        my $concatenation := self.new(:precedence<r=>, :associative<list>);
        nqp::bindkey($properties,'~',$concatenation);

        my $junctive-and := self.new(:precedence<q=>, :associative<list>);
        for < & (&) ∩ (.) ⊍ > {
            nqp::bindkey($properties,$_,$junctive-and);
        }

        my $junctive-or := self.new(:precedence<p=>, :associative<list>);
        for < | ^ (+) ⊎ (|) ∪ (-) ∖ (^) ⊖ > {
            nqp::bindkey($properties,$_,$junctive-or);
        }

        my $structural := self.new(:precedence<n=>, :associative<non>);
        for < .. ^.. ..^» ^..^ <=> leg cmp unicmp coll but does > {
            nqp::bindkey($properties,$_,$structural);
        }

        my $chaining := self.new(:precedence<m=>, :associative<chain>, :iffy);
        # unquotewordable
        for '>', '<', '>=', '<=', '(<)', '⊂', '(>)', '⊃',
            '(<=)', '⊆', '(>=)', '⊇', '(<+)', '≼', '(>+)', '≽' {
            nqp::bindkey($properties,$_,$chaining);
        }

        for < == != eq ne le ge lt gt =:= === eqv before after ~~
              (elem) ∈ (cont) ∋ (==) ≡ ∉ ∌ ⊄ ⊅ ≢ ⊈ ⊉ > {
            nqp::bindkey($properties,$_,$chaining);
        }

        my $tight-and := self.new(:precedence<l=>, :associative<list>, :thunky<.t>);
        nqp::bindkey($properties,'&&',$tight-and);

        my $tight-or := self.new(:precedence<k=>, :associative<list>, :thunky<.t>);
        for '||', '//' {
            nqp::bindkey($properties,$_,$tight-or);
        }

        my $tight-or-xor := self.new(:precedence<k=>, :associative<list>, :thunky<..t>);
        nqp::bindkey($properties,'^^',$tight-or-xor);

        my $tight-or-minmax := self.new(:precedence<k=>, :associative<list>);
        for < min max > {
            nqp::bindkey($properties,$_,$tight-or-minmax);
        }

        my $item-assignment := self.new(:precedence<i=>, :associative<right>);
        nqp::bindkey($properties,'=>',$item-assignment);

        my $comma := self.new(:precedence<g=>, :associative<list>);
        nqp::bindkey($properties,',',$comma);

        my $list-infix := self.new(:precedence<f=>, :associative<list>);
        for < Z X ... ...^ ^... ^...^ minmax > {
            nqp::bindkey($properties,$_,$list-infix);
        }

        my $list-prefix := self.new(:precedence<e=>);
        for < = ⚛= ⚛+= ⚛-= ⚛−= > {
            nqp::bindkey($properties,$_,$list-prefix);
        }

        my $loose-and := self.new(:precedence<d=>, :associative<list>, :thunky<.t>);
        nqp::bindkey($properties,'and',$loose-and);

        my $loose-andthen := self.new(:precedence<d=>, :associative<list>, :thunky<.b>);
        for < andthen notandthen > {
            nqp::bindkey($properties,$_,$loose-andthen);
        }

        my $loose-orelse := self.new(:precedence<c=>, :associative<list>, :thunky<.b>);
        for < or xor orelse > {
            nqp::bindkey($properties,$_,$loose-orelse);
        }

        nqp::atkey($properties,$op)
    }

    method properties-for-prefix(str $op) {
        my $properties := nqp::hash();

        my $autoincrement := self.new(:precedence<x=>);
        for < ++ -- ++⚛  --⚛  > {
            nqp::bindkey($properties,$_,$autoincrement);
        }

        my $symbolic-unary := self.new(:precedence<v=>);
        for < + ~ - ? ! | +^ ~^ ?^ ^ ⚛ > {
            nqp::bindkey($properties,$_,$symbolic-unary);
        }

        my $loose-unary := self.new(:precedence<h=>);
        for < so not > {
            nqp::bindkey($properties,$_,$loose-unary);
        }

        nqp::atkey($properties,$op)
    }

    method properties-for-postfix(str $op) {
        my $properties := nqp::hash();

        my $methodcall := self.new(:precedence<y=>);
        nqp::bindkey($properties,'i',$methodcall);

        my $autoincrement := self.new(:precedence<x=>);
        for < ++ -- ⚛++ ⚛-- > {
           nqp::bindkey($properties,$_,$autoincrement);
        }

        nqp::atkey($properties,$op)
    }

    method properties-for-postcircumfix(str $op) {
        my $properties := nqp::hash();

        my $methodcall := self.new(:precedence<y=>);
        for '[ ]', '{ }' {
            nqp::bindkey($properties,$_,$methodcall);
        }

        nqp::atkey($properties,$op)
    }
}

# vim: expandtab shiftwidth=4
