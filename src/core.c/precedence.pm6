# We attach precedence information to all operators here. This is instead of
# putting the traits directly on the op bodies, since some of the things that
# the traits are implemented using aren't defined that early.
BEGIN {
    my Mu $methodcall       := nqp::hash('prec', 'y=');
    my Mu $autoincrement    := nqp::hash('prec', 'x=');
    my Mu $exponentiation   := nqp::hash('prec', 'w=', 'assoc', 'right');
    my Mu $symbolic_unary   := nqp::hash('prec', 'v=');
    my Mu $multiplicative   := nqp::hash('prec', 'u=', 'assoc', 'left');
    my Mu $iffy             := nqp::hash('prec', 'u=', 'assoc', 'left', 'iffy', 1);
    my Mu $additive         := nqp::hash('prec', 't=', 'assoc', 'left');
    my Mu $replication      := nqp::hash('prec', 's=', 'assoc', 'left');
    my Mu $replication_xx   := nqp::hash('prec', 's=', 'assoc', 'left', 'thunky', 't.');
    my Mu $concatenation    := nqp::hash('prec', 'r=', 'assoc', 'list');
    my Mu $junctive_and     := nqp::hash('prec', 'q=', 'assoc', 'list');
    my Mu $junctive_or      := nqp::hash('prec', 'p=', 'assoc', 'list');
    my Mu $structural       := nqp::hash('prec', 'n=', 'assoc', 'non');
    my Mu $chaining         := nqp::hash('prec', 'm=', 'assoc', 'chain', 'iffy', 1, 'pasttype', 'chain');
    my Mu $tight_and        := nqp::hash('prec', 'l=', 'assoc', 'list', 'thunky', '.t');
    my Mu $tight_or         := nqp::hash('prec', 'k=', 'assoc', 'list', 'thunky', '.t');
    my Mu $tight_or_xor     := nqp::hash('prec', 'k=', 'assoc', 'list', 'thunky', '..t');
    my Mu $tight_or_minmax  := nqp::hash('prec', 'k=', 'assoc', 'list');
    my Mu $item_assignment  := nqp::hash('prec', 'i=', 'assoc', 'right');
    my Mu $loose_unary      := nqp::hash('prec', 'h=');
    my Mu $comma            := nqp::hash('prec', 'g=', 'assoc', 'list');
    my Mu $list_infix       := nqp::hash('prec', 'f=', 'assoc', 'list');
    my Mu $list_prefix      := nqp::hash('prec', 'e=');
    my Mu $loose_and        := nqp::hash('prec', 'd=', 'assoc', 'list', 'thunky', '.t');
    my Mu $loose_andthen    := nqp::hash('prec', 'd=', 'assoc', 'list', 'thunky', '.b');
    my Mu $loose_or         := nqp::hash('prec', 'c=', 'assoc', 'list', 'thunky', '.t');
    my Mu $loose_orelse     := nqp::hash('prec', 'c=', 'assoc', 'list', 'thunky', '.b');

    trait_mod:<is>(&postfix:<i>, :prec($methodcall));
    trait_mod:<is>(&postcircumfix:<[ ]>, :prec($methodcall));
    trait_mod:<is>(&postcircumfix:<[ ]>, :nodal);
    trait_mod:<is>(&postcircumfix:<{ }>, :prec($methodcall));
    trait_mod:<is>(&postcircumfix:<{ }>, :nodal);

    trait_mod:<is>(&prefix:<++>,  :prec($autoincrement));
    trait_mod:<is>(&prefix:<-->,  :prec($autoincrement));
    trait_mod:<is>(&postfix:<++>, :prec($autoincrement));
    trait_mod:<is>(&postfix:<-->, :prec($autoincrement));

    trait_mod:<is>(&infix:<**>, :prec($exponentiation));

    trait_mod:<is>(&prefix:<+>,  :prec($symbolic_unary));
    trait_mod:<is>(&prefix:<~>,  :prec($symbolic_unary));
    trait_mod:<is>(&prefix:<->,  :prec($symbolic_unary));
    trait_mod:<is>(&prefix:<?>,  :prec($symbolic_unary));
    trait_mod:<is>(&prefix:<!>,  :prec($symbolic_unary));
    trait_mod:<is>(&prefix:<|>,  :prec($symbolic_unary));
    trait_mod:<is>(&prefix:<+^>, :prec($symbolic_unary));
    trait_mod:<is>(&prefix:<~^>, :prec($symbolic_unary));
    trait_mod:<is>(&prefix:<?^>, :prec($symbolic_unary));
    trait_mod:<is>(&prefix:<^>,  :prec($symbolic_unary));

    trait_mod:<is>(&infix:<*>,   :prec($multiplicative));
    trait_mod:<is>(&infix:</>,   :prec($multiplicative));
    trait_mod:<is>(&infix:<div>, :prec($multiplicative));
    trait_mod:<is>(&infix:<gcd>, :prec($multiplicative));
    trait_mod:<is>(&infix:<lcm>, :prec($multiplicative));
    trait_mod:<is>(&infix:<%>,   :prec($multiplicative));
    trait_mod:<is>(&infix:<mod>, :prec($multiplicative));
    trait_mod:<is>(&infix:<+&>,  :prec($multiplicative));
    trait_mod:<is>(&infix:<~&>,  :prec($multiplicative));
    trait_mod:<is>(&infix:<?&>,  :prec($multiplicative));

    trait_mod:<is>(&infix:<%%>,  :prec($iffy));

    trait_mod:<is>(&infix:<+>,  :prec($additive));
    trait_mod:<is>(&infix:<->,  :prec($additive));
    trait_mod:<is>(&infix:<+|>, :prec($additive));
    trait_mod:<is>(&infix:<+^>, :prec($additive));
    trait_mod:<is>(&infix:<~|>, :prec($additive));
    trait_mod:<is>(&infix:<~^>, :prec($additive));
    trait_mod:<is>(&infix:<?|>, :prec($additive));
    trait_mod:<is>(&infix:<?^>, :prec($additive));

    trait_mod:<is>(&infix:<x>,  :prec($replication));
    trait_mod:<is>(&infix:<xx>, :prec($replication_xx));

    trait_mod:<is>(&infix:<~>, :prec($concatenation));

    trait_mod:<is>(&infix:<&>,   :prec($junctive_and));
    trait_mod:<is>(&infix:<(&)>, :prec($junctive_and));
    trait_mod:<is>(&infix:<(.)>, :prec($junctive_and));

    trait_mod:<is>(&infix:<|>,   :prec($junctive_or));
    trait_mod:<is>(&infix:<^>,   :prec($junctive_or));
    trait_mod:<is>(&infix:<(+)>, :prec($junctive_or));
    trait_mod:<is>(&infix:<(|)>, :prec($junctive_or));
    trait_mod:<is>(&infix:<(-)>, :prec($junctive_or));
    trait_mod:<is>(&infix:<(^)>, :prec($junctive_or));

    trait_mod:<is>(&infix:<< > >>,  :prec($chaining));
    trait_mod:<is>(&infix:<< < >>,  :prec($chaining));
    trait_mod:<is>(&infix:<< >= >>, :prec($chaining));
    trait_mod:<is>(&infix:<< <= >>, :prec($chaining));
    trait_mod:<is>(&infix:<==>,     :prec($chaining));
    trait_mod:<is>(&infix:<!=>,     :prec($chaining));
    trait_mod:<is>(&infix:<eq>,     :prec($chaining));
    trait_mod:<is>(&infix:<ne>,     :prec($chaining));
    trait_mod:<is>(&infix:<le>,     :prec($chaining));
    trait_mod:<is>(&infix:<ge>,     :prec($chaining));
    trait_mod:<is>(&infix:<lt>,     :prec($chaining));
    trait_mod:<is>(&infix:<gt>,     :prec($chaining));
    trait_mod:<is>(&infix:<=:=>,    :prec($chaining));
    trait_mod:<is>(&infix:<===>,    :prec($chaining));
    trait_mod:<is>(&infix:<eqv>,    :prec($chaining));
    trait_mod:<is>(&infix:<before>, :prec($chaining));
    trait_mod:<is>(&infix:<after>,  :prec($chaining));
    trait_mod:<is>(&infix:<~~>,     :prec($chaining));
    trait_mod:<is>(&infix:<(elem)>, :prec($chaining));
# U+2209 NOT AN ELEMENT OF
    trait_mod:<is>(&infix:<∉>,      :prec($chaining));
    trait_mod:<is>(&infix:<(cont)>, :prec($chaining));
# U+220C DOES NOT CONTAIN AS MEMBER
    trait_mod:<is>(&infix:<∌>,      :prec($chaining));
    trait_mod:<is>(&infix:<<(<)>>,  :prec($chaining));
# U+2284 NOT A SUBSET OF
    trait_mod:<is>(&infix:<⊄>,      :prec($chaining));
    trait_mod:<is>(&infix:<<(>)>>,  :prec($chaining));
# U+2285 NOT A SUPERSET OF
    trait_mod:<is>(&infix:<⊅>,      :prec($chaining));
    trait_mod:<is>(&infix:<<(==)>>, :prec($chaining));
# U+2262 NOT IDENTICAL TO
    trait_mod:<is>(&infix:<≢>,      :prec($chaining));
    trait_mod:<is>(&infix:<<(<=)>>, :prec($chaining));
# U+2288 NEITHER A SUBSET OF NOR EQUAL TO
    trait_mod:<is>(&infix:<⊈>,      :prec($chaining));
    trait_mod:<is>(&infix:<<(>=)>>, :prec($chaining));
# U+2289 NEITHER A SUPERSET OF NOR EQUAL TO
    trait_mod:<is>(&infix:<⊉>,      :prec($chaining));
    trait_mod:<is>(&infix:<<(<+)>>, :prec($chaining));
    trait_mod:<is>(&infix:<<(>+)>>, :prec($chaining));

    trait_mod:<is>(&infix:<..>,      :prec($structural));
    trait_mod:<is>(&infix:<^..>,     :prec($structural));
    trait_mod:<is>(&infix:<..^>,     :prec($structural));
    trait_mod:<is>(&infix:<^..^>,    :prec($structural));
    trait_mod:<is>(&infix:<< <=> >>, :prec($structural));
    trait_mod:<is>(&infix:<leg>,     :prec($structural));
    trait_mod:<is>(&infix:<cmp>,     :prec($structural));
    trait_mod:<is>(&infix:<unicmp>,  :prec($structural));
    trait_mod:<is>(&infix:<coll>,    :prec($structural));
    trait_mod:<is>(&infix:<but>,     :prec($structural));
    trait_mod:<is>(&infix:<does>,    :prec($structural));

    trait_mod:<is>(&infix:<&&>, :prec($tight_and));

    trait_mod:<is>(&infix:<||>,  :prec($tight_or));
    trait_mod:<is>(&infix:<^^>,  :prec($tight_or_xor));
    trait_mod:<is>(&infix:<//>,  :prec($tight_or));
    trait_mod:<is>(&infix:<min>, :prec($tight_or_minmax));
    trait_mod:<is>(&infix:<max>, :prec($tight_or_minmax));

    #trait_mod:<is>(&infix:<ff>,  :prec($conditional_ff));
    #trait_mod:<is>(&infix:<fff>, :prec($conditional_ff));

    trait_mod:<is>(&infix:<< => >>, :prec($item_assignment));

    trait_mod:<is>(&prefix:<so>,   :prec($loose_unary));
    trait_mod:<is>(&prefix:<not>,  :prec($loose_unary));

    trait_mod:<is>(&infix:<,>, :prec($comma));

    trait_mod:<is>(&infix:<Z>,      :prec($list_infix));
    trait_mod:<is>(&infix:<X>,      :prec($list_infix));
    trait_mod:<is>(&infix:<...>,    :prec($list_infix));
    trait_mod:<is>(&infix:<...^>,   :prec($list_infix));
    trait_mod:<is>(&infix:<^...>,   :prec($list_infix));
    trait_mod:<is>(&infix:<^...^>,  :prec($list_infix));
    trait_mod:<is>(&infix:<minmax>, :prec($list_infix));

    trait_mod:<is>(&infix:<=>,   :prec($list_prefix));
    #trait_mod:<is>(&infix:<:=>,  :prec($list_prefix));
    #trait_mod:<is>(&infix:<::=>, :prec($list_prefix));

    trait_mod:<is>(&infix:<and>,     :prec($loose_and));
    trait_mod:<is>(&infix:<andthen>, :prec($loose_andthen));
    trait_mod:<is>(&infix:<notandthen>, :prec($loose_andthen));

    trait_mod:<is>(&infix:<or>,     :prec($loose_orelse));
    trait_mod:<is>(&infix:<xor>,    :prec($loose_orelse));
    trait_mod:<is>(&infix:<orelse>, :prec($loose_orelse));
}

# vim: expandtab shiftwidth=4
