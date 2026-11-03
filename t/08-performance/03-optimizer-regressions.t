use Test;

plan 1;

nok "definitely-not-Mu:U" ~~ Mu:U, "Smartmatch optimization does not lead to non-Mu:U object smartmatching with Mu:U";

done-testing;
