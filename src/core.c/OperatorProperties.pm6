class OperatorProperties {
    # Core module installation dies with "Missing serialize REPR function
    # for REPR NativeRef (STrAttrRef)" if "str" attribute
    has Str $!precedence  is built = "";
    has Str $!associative is built = "";
    has Str $!thunky      is built = "";
    # Core module installation dies with "Missing serialize REPR function
    # for REPR NativeRef (IntAttrRef)" if "int" attribute
    has Int $!iffy        is built = 0;
}

augment class OperatorProperties {

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

    # Return properties depending on other properties
    proto method equiv() {*}
    multi method equiv(OP:U:) { self.new }
    multi method equiv(OP:D:) {
        self.new: :$!precedence,
          :associative(''), :$!thunky, :$!iffy
    }

    proto method tighter() {*}
    multi method tighter(OP:U:) {
        die "No precedence found to be tighter for";
    }
    multi method tighter(OP:D:) {
        self.new: precedence => $!precedence.subst('=',':=')
          :associative(''), :$!thunky, :$!iffy
    }

    proto method looser() {*}
    multi method looser(OP:U:) {
        die "No precedence found to be looser for";
    }
    multi method looser(OP:D:) {
        self.new: precedence => $!precedence.subst('=','@='),
          :associative(''), :$!thunky, :$!iffy
    }

    # Return handler for reducing with these operator properties
    proto method reducer() {*}
    multi method reducer(OP:U:) { &METAOP_REDUCE_LEFT }
    multi method reducer(OP:D:) {
        nqp::iseq_s($!precedence,'f=')
          ?? &METAOP_REDUCE_LISTINFIX
          !! nqp::chars($!associative)
            ?? nqp::iseq_s($!associative,'chain')
              ?? &METAOP_REDUCE_CHAIN
              !! nqp::iseq_s($!associative,'list')
                ?? &METAOP_REDUCE_LIST
                !! nqp::iseq_s($!associative,'right')
                  ?? &METAOP_REDUCE_RIGHT
                  !! &METAOP_REDUCE_LEFT    # assume 'left' or 'non'
            !! &METAOP_REDUCE_LEFT
    }

    # Old interface methods
    proto method prec(|) {*}
    multi method prec(OP:U:) { BEGIN Map.new }
    multi method prec(OP:U: "prec"   --> "") { }
    multi method prec(OP:U: "assoc"  --> "") { }
    multi method prec(OP:U: "thunky" --> "") { }
    multi method prec(OP:U: "iffy"   --> 0 ) { }

    multi method prec(OP:D:) {
        Map.new:
          (prec   => $!precedence  if $!precedence),
          (assoc  => $!associative if $!associative),
          (thunky => $!thunky      if $!thunky),
          (iffy   => $!iffy)
    }
    multi method prec(OP:D: "prec")   { $!precedence  }
    multi method prec(OP:D: "assoc")  { $!associative }
    multi method prec(OP:D: "thunky") { $!thunky      }
    multi method prec(OP:D: "iffy")   { $!iffy        }

    # New interface methods
    proto method precedence() {*}
    multi method precedence(OP:U:) { "" }
    multi method precedence(OP:D:) { $!precedence }

    proto method associative() {*}
    multi method associative(OP:U:) { "" }
    multi method associative(OP:D:) { $!associative }

    proto method thunky() {*}
    multi method thunky(OP:U:) { "" }
    multi method thunky(OP:D:) { $!thunky }

    proto method iffy() {*}
    multi method iffy(OP:U:) { 0 }
    multi method iffy(OP:D:) { $!iffy }

    method methodcall(OP:U:) { BEGIN OP.new:
        :precedence<y=>
    }
    method autoincrement(OP:U:) { BEGIN OP.new:
        :precedence<x=>
    }
    method exponentiation(OP:U:) { BEGIN OP.new:
        :precedence<w=>, :associative<left>
    }
    method symbolic-unary(OP:U:) { BEGIN OP.new:
        :precedence<v=>
    }
    method multiplicative(OP:U:) { BEGIN OP.new:
        :precedence<u=>, :associative<left>
    }
    method iffytive(OP:U:) { BEGIN OP.new:
        :precedence<u=>, :associative<left>, :iffy
    }
    method additive(OP:U:) { BEGIN OP.new:
        :precedence<t=>, :associative<left>
    }
    method replication(OP:U:) { BEGIN OP.new:
        :precedence<s=>, :associative<left>
    }
    method replication-xx(OP:U:) { BEGIN OP.new:
        :precedence<s=>, :associative<left>, :thunky<t.>
    }
    method concatenation(OP:U:) { BEGIN OP.new:
        :precedence<r=>, :associative<list>
    }
    method junctive-and(OP:U:) { BEGIN OP.new:
        :precedence<q=>, :associative<list>
    }
    method junctive-or(OP:U:) { BEGIN OP.new:
        :precedence<p=>, :associative<list>
    }
    method structural(OP:U:) { BEGIN OP.new:
        :precedence<n=>, :associative<non>
    }
    method chaining(OP:U:) { BEGIN OP.new:
        :precedence<m=>, :associative<chain>, :iffy
    }
    method tight-and(OP:U:) { BEGIN OP.new:
        :precedence<l=>, :associative<list>, :thunky<.t>
    }
    method tight-or(OP:U:) { BEGIN OP.new:
        :precedence<k=>, :associative<list>, :thunky<.t>
    }
    method tight-or-xor(OP:U:) { BEGIN OP.new:
        :precedence<k=>, :associative<list>, :thunky<..t>
    }
    method tight-or-minmax(OP:U:) { BEGIN OP.new:
        :precedence<k=>, :associative<list>
    }
    method item-assignment(OP:U:) { BEGIN OP.new:
        :precedence<i=>, :associative<right>
    }
    method loose-unary(OP:U:) { BEGIN OP.new:
        :precedence<h=>
    }
    method comma(OP:U:) { BEGIN OP.new:
        :precedence<g=>, :associative<list>
    }
    method list-infix(OP:U:) { BEGIN OP.new:
        :precedence<f=>, :associative<list>
    }
    method list-prefix(OP:U:) { BEGIN OP.new:
        :precedence<e=>
    }
    method loose-and(OP:U:) { BEGIN OP.new:
        :precedence<d=>, :associative<list>, :thunky<.t>
    }
    method loose-andthen(OP:U:) { BEGIN OP.new:
        :precedence<d=>, :associative<list>, :thunky<.b>
    }
    method loose-or(OP:U:) { BEGIN OP.new:
        :precedence<c=>, :associative<list>, :thunky<.t>
    }
    method loose-orelse(OP:U:) { BEGIN OP.new:
        :precedence<c=>, :associative<list>, :thunky<.b>
    }
}

# Attach precedence information to all operators here. This is instead of
# putting the traits directly on the op bodies, since some of the things
# that the traits are implemented using aren't defined that early.
BEGIN {
    nqp::bindattr($_,Routine,'$!op_props',OperatorProperties.methodcall) for
      &postfix:<i>,
      &postcircumfix:<[ ]>,
      &postcircumfix:<{ }>,
    ;

    nqp::bindattr($_,Routine,'$!op_props',OperatorProperties.autoincrement) for
      &prefix:<++>,
      &prefix:<-->,
      &postfix:<++>,
      &postfix:<-->,
    ;

    nqp::bindattr($_,Routine,'$!op_props',OperatorProperties.exponentiation) for
      &infix:<**>,
    ;

    nqp::bindattr($_,Routine,'$!op_props',OperatorProperties.symbolic-unary) for
      &prefix:<+>,
      &prefix:<~>,
      &prefix:<->,
      &prefix:<?>,
      &prefix:<!>,
      &prefix:<|>,
      &prefix:<+^>,
      &prefix:<~^>,
      &prefix:<?^>,
      &prefix:<^>,
    ;

    nqp::bindattr($_,Routine,'$!op_props',OperatorProperties.multiplicative) for
      &infix:<*>,
      &infix:</>,
      &infix:<div>,
      &infix:<gcd>,
      &infix:<lcm>,
      &infix:<%>,
      &infix:<mod>,
      &infix:<+&>,
      &infix:<~&>,
      &infix:<?&>,
    ;

    nqp::bindattr($_,Routine,'$!op_props',OperatorProperties.iffytive) for
      &infix:<%%>,
    ;

    nqp::bindattr($_,Routine,'$!op_props',OperatorProperties.additive) for
      &infix:<+>,
      &infix:<->,
      &infix:<+|>,
      &infix:<+^>,
      &infix:<~|>,
      &infix:<~^>,
      &infix:<?|>,
      &infix:<?^>,
    ;

    nqp::bindattr($_,Routine,'$!op_props',OperatorProperties.replication) for
      &infix:<x>,
    ;

    nqp::bindattr($_,Routine,'$!op_props',OperatorProperties.replication-xx) for
      &infix:<xx>,
    ;

    nqp::bindattr($_,Routine,'$!op_props',OperatorProperties.concatenation) for
      &infix:<~>,
    ;

    nqp::bindattr($_,Routine,'$!op_props',OperatorProperties.junctive-and) for
      &infix:<&>,
      &infix:<(&)>,
      &infix:<(.)>,
    ;

    nqp::bindattr($_,Routine,'$!op_props',OperatorProperties.junctive-or) for
      &infix:<|>,
      &infix:<^>,
      &infix:<(+)>,
      &infix:<(|)>,
      &infix:<(-)>,
      &infix:<(^)>,
    ;

    nqp::bindattr($_,Routine,'$!op_props',OperatorProperties.chaining) for
      &infix:<< > >>,
      &infix:<< < >>,
      &infix:<< >= >>,
      &infix:<< <= >>,
      &infix:<==>,
      &infix:<!=>,
      &infix:<eq>,
      &infix:<ne>,
      &infix:<le>,
      &infix:<ge>,
      &infix:<lt>,
      &infix:<gt>,
      &infix:<=:=>,
      &infix:<===>,
      &infix:<eqv>,
      &infix:<before>,
      &infix:<after>,
      &infix:<~~>,
      &infix:<(elem)>,
      &infix:<∉>,           # U+2209 NOT AN ELEMENT OF
      &infix:<(cont)>,
      &infix:<∌>,           # U+220C DOES NOT CONTAIN AS MEMBER
      &infix:<<(<)>>,
      &infix:<⊄>,           # U+2284 NOT A SUBSET OF
      &infix:<<(>)>>,
      &infix:<⊅>,           # U+2285 NOT A SUPERSET OF
      &infix:<<(==)>>,
      &infix:<≢>,           # U+2262 NOT IDENTICAL TO
      &infix:<<(<=)>>,
      &infix:<⊈>,           # U+2288 NEITHER A SUBSET OF NOR EQUAL TO
      &infix:<<(>=)>>,
      &infix:<⊉>,           # U+2289 NEITHER A SUPERSET OF NOR EQUAL TO
      &infix:<<(<+)>>,
      &infix:<<(>+)>>,
    ;

    nqp::bindattr($_,Routine,'$!op_props',OperatorProperties.structural) for
      &infix:<..>,
      &infix:<^..>,
      &infix:<..^>,
      &infix:<^..^>,
      &infix:<< <=> >>,
      &infix:<leg>,
      &infix:<cmp>,
      &infix:<unicmp>,
      &infix:<coll>,
      &infix:<but>,
      &infix:<does>,
    ;

    nqp::bindattr($_,Routine,'$!op_props',OperatorProperties.tight-and) for
      &infix:<&&>,
    ;

    nqp::bindattr($_,Routine,'$!op_props',OperatorProperties.tight-or) for
      &infix:<||>,
      &infix:<//>,
    ;

    nqp::bindattr($_,Routine,'$!op_props',OperatorProperties.tight-or-xor) for
      &infix:<^^>,
    ;

    nqp::bindattr($_,Routine,'$!op_props',OperatorProperties.tight-or-minmax) for
      &infix:<min>,
      &infix:<max>,
    ;

    nqp::bindattr($_,Routine,'$!op_props',OperatorProperties.item-assignment) for
      &infix:<< => >>,
    ;

    nqp::bindattr($_,Routine,'$!op_props',OperatorProperties.loose-unary) for
      &prefix:<so>,
      &prefix:<not>,
    ;

    nqp::bindattr($_,Routine,'$!op_props',OperatorProperties.comma) for
      &infix:<,>,
    ;

    nqp::bindattr($_,Routine,'$!op_props',OperatorProperties.list-infix) for
      &infix:<Z>,
      &infix:<X>,
      &infix:<...>,
      &infix:<...^>,
      &infix:<^...>,
      &infix:<^...^>,
      &infix:<minmax>,
    ;

    nqp::bindattr($_,Routine,'$!op_props',OperatorProperties.list-prefix) for
      &infix:<=>,
    ;

    nqp::bindattr($_,Routine,'$!op_props',OperatorProperties.loose-and) for
      &infix:<and>,
    ;

    nqp::bindattr($_,Routine,'$!op_props',OperatorProperties.loose-andthen) for
      &infix:<andthen>,
      &infix:<notandthen>,
    ;

    nqp::bindattr($_,Routine,'$!op_props',OperatorProperties.loose-orelse) for
      &infix:<or>,
      &infix:<xor>,
      &infix:<orelse>,
    ;
}

# Attach nodality information to operators that didn't get them yet.
BEGIN {
    trait_mod:<is>(&postcircumfix:<[ ]>, :nodal);
    trait_mod:<is>(&postcircumfix:<{ }>, :nodal);
}

# vim: expandtab shiftwidth=4
