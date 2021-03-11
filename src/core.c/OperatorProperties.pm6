class OperatorProperties {
    has str $.precedence  = "";
    has str $.associative = "";
    has str $.thunky      = "";
    has int $.iffy;

    # shorten signatures and invocations
    my constant OP = OperatorProperties;

    multi method WHICH(OP:D: --> ValueObjAt) {
        my $parts := nqp::list_s('OperatorProperties');

        nqp::push_s($parts,nqp::concat('precedence=',$!precedence))
          if $!precedence;
        nqp::push_s($parts,nqp::concat('associative=',$!associative))
          if $!associative;
        nqp::push_s($parts,nqp::concat('thunky=',$!thunky))
          if $!thunky;
        nqp::push_s($parts,nqp::concat('iffy=',$!iffy))
          if $!iffy;

        nqp::box_s(nqp::join('|',$parts),ValueObjAt)
    }

    method equiv(OP:D:) {
        self.new: :$!precedence,
          :associative(''), :$!thunky, :$!iffy
    }
    method looser(OP:D:) {
        self.new: precedence => $!precedence.subst('=','@='),
          :associative(''), :$!thunky, :$!iffy
    }
    method tighter(OP:D:) {
        self.new: precedence => $!precedence.subst('=',':=')
          :associative(''), :$!thunky, :$!iffy
    }

    method prec() {
        Map.new:
          (prec   => $!precedence  if $!precedence),
          (assoc  => $!associative if $!associative),
          (thunky => $!thunky      if $!thunky),
          (iffy   => $!iffy)
    }

    method methodcall(OP:U:) { OP.new:
        :precedence<y=>
    }
    method autoincrement(OP:U:) { OP.new:
        :precedence<x=>
    }
    method exponentiation(OP:U:) { OP.new:
        :precedence<w=>, :associative<left>
    }
    method symbolic-unary(OP:U:) { OP.new:
        :precedence<v=>
    }
    method multiplicative(OP:U:) { OP.new:
        :precedence<u=>, :associative<left>
    }
    method iffytive(OP:U:) { OP.new:
        :precedence<u=>, :associative<left>, :iffy
    }
    method additive(OP:U:) { OP.new:
        :precedence<t=>, :associative<left>
    }
    method replication(OP:U:) { OP.new:
        :precedence<s=>, :associative<left>
    }
    method replication-xx(OP:U:) { OP.new:
        :precedence<s=>, :associative<left>, :thunky<t.>
    }
    method concatenation(OP:U:) { OP.new:
        :precedence<r=>, :associative<list>
    }
    method junctive-and(OP:U:) { OP.new:
        :precedence<q=>, :associative<list>
    }
    method junctive-or(OP:U:) { OP.new:
        :precedence<p=>, :associative<list>
    }
    method structural(OP:U:) { OP.new:
        :precedence<n=>, :associative<non>
    }
    method chaining(OP:U:) { OP.new:
        :precedence<m=>, :associative<chain>, :iffy
    }
    method tight-and(OP:U:) { OP.new:
        :precedence<l=>, :associative<list>, :thunky<.t>
    }
    method tight-or(OP:U:) { OP.new:
        :precedence<k=>, :associative<list>, :thunky<.t>
    }
    method tight-or-xor(OP:U:) { OP.new:
        :precedence<k=>, :associative<list>, :thunky<..t>
    }
    method tight-or-minmax(OP:U:) { OP.new:
        :precedence<k=>, :associative<list>
    }
    method item-assignment(OP:U:) { OP.new:
        :precedence<i=>, :associative<right>
    }
    method loose-unary(OP:U:) { OP.new:
        :precedence<h=>
    }
    method comma(OP:U:) { OP.new:
        :precedence<g=>, :associative<list>
    }
    method list-infix(OP:U:) { OP.new:
        :precedence<f=>, :associative<list>
    }
    method list-prefix(OP:U:) { OP.new:
        :precedence<e=>
    }
    method loose-and(OP:U:) { OP.new:
        :precedence<d=>, :associative<list>, :thunky<.t>
    }
    method loose-andthen(OP:U:) { OP.new:
        :precedence<d=>, :associative<list>, :thunky<.b>
    }
    method loose-or(OP:U:) { OP.new:
        :precedence<c=>, :associative<list>, :thunky<.t>
    }
    method loose-orelse(OP:U:) { OP.new:
        :precedence<c=>, :associative<list>, :thunky<.b>
    }
}

# vim: expandtab shiftwidth=4
