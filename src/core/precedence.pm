# We attach precedence information to all operators here. This is instead of
# putting the traits directly on the op bodies, since some of the things that
# the traits are implemented using aren't defined that early.
BEGIN {
    my Mu $methodcall     := nqp::hash('prec', 'y=');
    my Mu $autoincrement  := nqp::hash('prec', 'x=');
    my Mu $exponentiation := nqp::hash('prec', 'w=');
    my Mu $symbolic_unary := nqp::hash('prec', 'v=');
    my Mu $multiplicative := nqp::hash('prec', 'u=');
    my Mu $additive       := nqp::hash('prec', 't=');
    my Mu $replication    := nqp::hash('prec', 's=');
    my Mu $concatenation  := nqp::hash('prec', 'r=');
    my Mu $junctive_and   := nqp::hash('prec', 'q=');
    my Mu $junctive_or    := nqp::hash('prec', 'p=');
    my Mu $structural     := nqp::hash('prec', 'n=');
    my Mu $chaining       := nqp::hash('prec', 'm=', 'iffy', 1, 'pasttype', 'chain');
    my Mu $iffy           := nqp::hash('prec', 'u=', 'iffy', 1);

    trait_mod:<is>(&postfix:<i>, :prec($methodcall));

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
    trait_mod:<is>(&infix:<xx>, :prec($replication));

    trait_mod:<is>(&infix:<~>, :prec($concatenation));

    trait_mod:<is>(&infix:<&>,          :prec($junctive_and));
    trait_mod:<is>(&infix:<(&)>,        :prec($junctive_and));
#    trait_mod:<is>(&infix:<<"\x2229">>, :prec($junctive_and));

    trait_mod:<is>(&infix:<|>,          :prec($junctive_or));
    trait_mod:<is>(&infix:<^>,          :prec($junctive_or));
    trait_mod:<is>(&infix:<(.)>,        :prec($junctive_or));
#    trait_mod:<is>(&infix:<<"\x228D">>, :prec($junctive_or));
    trait_mod:<is>(&infix:<(+)>,        :prec($junctive_or));
#    trait_mod:<is>(&infix:<<"\x228E">>, :prec($junctive_or));
    trait_mod:<is>(&infix:<(|)>,        :prec($junctive_or));
#    trait_mod:<is>(&infix:<<"\x222A">>, :prec($junctive_or));
    trait_mod:<is>(&infix:<(-)>,        :prec($junctive_or));
#    trait_mod:<is>(&infix:<<"\x2216">>, :prec($junctive_or));
    trait_mod:<is>(&infix:<(^)>,        :prec($junctive_or));
#    trait_mod:<is>(&infix:<<"\x2296">>, :prec($junctive_or));

    trait_mod:<is>(&infix:<==>,         :prec($chaining));
    trait_mod:<is>(&infix:<!=>,         :prec($chaining));
    trait_mod:<is>(&infix:<eq>,         :prec($chaining));
    trait_mod:<is>(&infix:<ne>,         :prec($chaining));
    trait_mod:<is>(&infix:<le>,         :prec($chaining));
    trait_mod:<is>(&infix:<ge>,         :prec($chaining));
    trait_mod:<is>(&infix:<lt>,         :prec($chaining));
    trait_mod:<is>(&infix:<gt>,         :prec($chaining));
    trait_mod:<is>(&infix:<=:=>,        :prec($chaining));
    trait_mod:<is>(&infix:<===>,        :prec($chaining));
    trait_mod:<is>(&infix:<eqv>,        :prec($chaining));
    trait_mod:<is>(&infix:<before>,     :prec($chaining));
    trait_mod:<is>(&infix:<after>,      :prec($chaining));
    trait_mod:<is>(&infix:<~~>,         :prec($chaining));
    trait_mod:<is>(&infix:<(elem)>,     :prec($chaining));
#    trait_mod:<is>(&infix:<<"\x2208">>, :prec($chaining));
#    trait_mod:<is>(&infix:<<"\x2209">>, :prec($chaining));
    trait_mod:<is>(&infix:<(cont)>,     :prec($chaining));
#    trait_mod:<is>(&infix:<<"\x220B">>, :prec($chaining));
#    trait_mod:<is>(&infix:<<"\x220C">>, :prec($chaining));
#    trait_mod:<is>(&infix:<<(<)>>,      :prec($chaining));
#    trait_mod:<is>(&infix:<<"\x2282">>, :prec($chaining));
#    trait_mod:<is>(&infix:<<"\x2284">>, :prec($chaining));
#    trait_mod:<is>(&infix:<<(>)>>,      :prec($chaining));
#    trait_mod:<is>(&infix:<<"\x2283">>, :prec($chaining));
#    trait_mod:<is>(&infix:<<"\x2285">>, :prec($chaining));
#    trait_mod:<is>(&infix:<<(<=)>>,     :prec($chaining));
#    trait_mod:<is>(&infix:<<"\x2286">>, :prec($chaining));
#    trait_mod:<is>(&infix:<<"\x2288">>, :prec($chaining));
#    trait_mod:<is>(&infix:<<(>=)>>,     :prec($chaining));
#    trait_mod:<is>(&infix:<<"\x2287">>, :prec($chaining));
#    trait_mod:<is>(&infix:<<"\x2289">>, :prec($chaining));
#    trait_mod:<is>(&infix:<<(<+)>>,     :prec($chaining));
#    trait_mod:<is>(&infix:<<"\x227C">>, :prec($chaining));
#    trait_mod:<is>(&infix:<<(>+)>>,     :prec($chaining));
#    trait_mod:<is>(&infix:<<"\x227D">>, :prec($chaining));

    trait_mod:<is>(&infix:<..>,   :prec($structural));
    trait_mod:<is>(&infix:<^..>,  :prec($structural));
    trait_mod:<is>(&infix:<..^>,  :prec($structural));
    trait_mod:<is>(&infix:<^..^>, :prec($structural));
    trait_mod:<is>(&infix:<leg>,  :prec($structural));
    trait_mod:<is>(&infix:<cmp>,  :prec($structural));
    trait_mod:<is>(&infix:<but>,  :prec($structural));
    trait_mod:<is>(&infix:<does>, :prec($structural));
}
