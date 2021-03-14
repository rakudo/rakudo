class RakuAST::OperatorProperties {
# defined in src/Raku/ast/operator-properties
#    has str $.precedence;
#    has str $.associative;
#    has str $.thunky;
#    has int $.iffy;

    multi method WHICH(RakuAST::OperatorProperties:D: --> ValueObjAt:D) {
        my $parts := nqp::list_s('RakuAST::OperatorProperties');

        if $.precedence -> $precedence {
            nqp::push_s($parts,nqp::concat('precedence=',$precedence));
        }
        if $.associative -> $associative {
            nqp::push_s($parts,nqp::concat('associative=',$associative));
        }
        if $.thunky -> $thunky {
            nqp::push_s($parts,nqp::concat('thunky=',$thunky))
        }
        if $.iffy -> $iffy {
            nqp::push_s($parts,nqp::concat('iffy=',$iffy))
        }

        nqp::box_s(nqp::join('|',$parts),ValueObjAt)
    }

    # Return handler for reducing with these operator properties
    method reducer() { ::(self.reducer-name) }
}

# Attach operator properties to all of the built-in operators.  This is
# done here instead as traits on the op bodies, since some of the things
# that the traits are implement in, are using features that aren't defined
# that early.

BEGIN {

    # prefixes
    .set_op_props for
      &prefix:«!»,
      &prefix:«+»,
      &prefix:«++»,
      &prefix:«+^»,
      &prefix:«-»,
      &prefix:«--»,
      &prefix:«?»,
      &prefix:«?^»,
      &prefix:«^»,
      &prefix:«not»,
      &prefix:«so»,
      &prefix:«|»,
      &prefix:«~»,
      &prefix:«~^»,

    # infixes
      &infix:«!=»,
      &infix:«%»,
      &infix:«%%»,
      &infix:«&»,
      &infix:«&&»,
      &infix:«(&)»,
      &infix:«(+)»,
      &infix:«(-)»,
      &infix:«(.)»,
      &infix:«(<)»,
      &infix:«(<+)»,
      &infix:«(<=)»,
      &infix:«(==)»,
      &infix:«(>)»,
      &infix:«(>+)»,
      &infix:«(>=)»,
      &infix:«(^)»,
      &infix:«(cont)»,
      &infix:«(elem)»,
      &infix:«(|)»,
      &infix:«*»,
      &infix:«**»,
      &infix:«+»,
      &infix:«+&»,
      &infix:«+^»,
      &infix:«+|»,
      &infix:«,»,
      &infix:«-»,
      &infix:«...»,
      &infix:«...^»,
      &infix:«..»,
      &infix:«..^»,
      &infix:«/»,
      &infix:«//»,
      &infix:«<»,
      &infix:«<=»,
      &infix:«<=>»,
      &infix:«=>»,
      &infix:«=:=»,
      &infix:«==»,
      &infix:«===»,
      &infix:«=»,
      &infix:«>»,
      &infix:«>=»,
      &infix:«?&»,
      &infix:«?^»,
      &infix:«?|»,
      &infix:«X»,
      &infix:«Z»,
      &infix:«^..»,
      &infix:«^...»,
      &infix:«^..^»,
      &infix:«^...^»,
      &infix:«^»,
      &infix:«^^»,
      &infix:«after»,
      &infix:«andthen»,
      &infix:«and»,
      &infix:«before»,
      &infix:«but»,
      &infix:«cmp»,
      &infix:«coll»,
      &infix:«div»,
      &infix:«does»,
      &infix:«eqv»,
      &infix:«eq»,
      &infix:«gcd»,
      &infix:«ge»,
      &infix:«gt»,
      &infix:«lcm»,
      &infix:«leg»,
      &infix:«le»,
      &infix:«lt»,
      &infix:«max»,
      &infix:«minmax»,
      &infix:«min»,
      &infix:«mod»,
      &infix:«ne»,
      &infix:«notandthen»,
      &infix:«orelse»,
      &infix:«or»,
      &infix:«unicmp»,
      &infix:«xor»,
      &infix:«xx»,
      &infix:«x»,
      &infix:«|»,
      &infix:«||»,
      &infix:«~»,
      &infix:«~&»,
      &infix:«~^»,
      &infix:«~|»,
      &infix:«~~»,
      &infix:«∉»,  # U+2209 NOT AN ELEMENT OF
      &infix:«∌»,  # U+220C DOES NOT CONTAIN AS MEMBER
      &infix:«≢»,  # U+2262 NOT IDENTICAL TO
      &infix:«⊄»,  # U+2284 NOT A SUBSET OF
      &infix:«⊅»,  # U+2285 NOT A SUPERSET OF
      &infix:«⊈»,  # U+2288 NEITHER A SUBSET OF NOR EQUAL TO
      &infix:«⊉»,  # U+2289 NEITHER A SUPERSET OF NOR EQUAL TO

    # postfixes
      &postfix:«++»,
      &postfix:«--»,
      &postfix:«i»,

    # postcircumfixes
      &postcircumfix:<[ ]>,
      &postcircumfix:<{ }>,
    ;
}
# vim: expandtab shiftwidth=4
