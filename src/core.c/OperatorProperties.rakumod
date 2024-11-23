class OperatorProperties {
# defined in src/Raku/ast/operator-properties
#    has str $.precedence;
#    has str $.sub-precedence;
#    has str $.associative;
#    has str $.thunky;
#    has str $.dba;
#    has str $.next-term;
#    has int $.iffy;
#    has int $.diffy;
#    has int $.fiddly;
#    has int $.adverb;
#    has int $.ternary;
#    has int $.commutative;

    multi method WHICH(OperatorProperties:D: --> ValueObjAt:D) {
        my $parts := nqp::list_s('OperatorProperties');

        if $.precedence -> str $precedence {
            nqp::push_s($parts,nqp::concat('precedence=',$precedence));
        }
        if $.sub-precedence -> str $sub-precedence {
            nqp::push_s($parts,nqp::concat('sub-precedence=',$sub-precedence));
        }
        if $.associative -> str $associative {
            nqp::push_s($parts,nqp::concat('associative=',$associative));
        }
        if $.thunky -> str $thunky {
            nqp::push_s($parts,nqp::concat('thunky=',$thunky))
        }
        nqp::push_s($parts,'iffy')    if $.iffy;
        nqp::push_s($parts,'diffy')   if $.diffy;
        nqp::push_s($parts,'fiddly')  if $.fiddly;
        nqp::push_s($parts,'adverb')  if $.adverb;
        nqp::push_s($parts,'ternary') if $.ternary;
        nqp::push_s($parts,'commutative') if $.commutative;

        nqp::box_s(nqp::join('|',$parts),ValueObjAt)
    }

    # Return handler for reducing with these operator properties
    method reducer() { ::(self.reducer-name) }
}

#-------------------------------------------------------------------------------
# The REST of this file can be REMOVED **AFTER** the Raku grammar has
# become the grammar to build the setting with.  XXX

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
#?if !jvm
      &prefix:<⚛>,
#?endif

    # infixes
      &infix:«!=»,
      &infix:«!~~»,
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
      &infix:«+>»,
      &infix:«+<»,
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
      &infix:«=~=»,
      &infix:«≅»,
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
      &infix:«~>»,
      &infix:«~<»,
      &infix:«~~»,
      &infix:«=~»,
      &infix:«∉»,  # U+2209 NOT AN ELEMENT OF
      &infix:«∌»,  # U+220C DOES NOT CONTAIN AS MEMBER
      &infix:«≢»,  # U+2262 NOT IDENTICAL TO
      &infix:«⊄»,  # U+2284 NOT A SUBSET OF
      &infix:«⊅»,  # U+2285 NOT A SUPERSET OF
      &infix:«⊈»,  # U+2288 NEITHER A SUBSET OF NOR EQUAL TO
      &infix:«⊉»,  # U+2289 NEITHER A SUPERSET OF NOR EQUAL TO
      &infix:«≼»,  # U+227C PRECEDES OR EQUAL TO
      &infix:«≽»,  # U+227D SUCCEEDS OR EQUAL TO
#?if !jvm
      &infix:<⚛+=>,
      &infix:<⚛-=>,
      &infix:<⚛=>,
      &prefix:<++⚛>,
      &prefix:<--⚛>,
      &postfix:<⚛++>,
      &postfix:<⚛-->,
#?endif

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
