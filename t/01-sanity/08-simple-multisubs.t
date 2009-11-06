use v6;


say "1..2";

multi sub foo ($only_one_arg) {
    if $only_one_arg eq "only_one_arg" {
        say "ok 1";
    }
    else {
        say "not ok 1";
    }
}

multi sub foo ($arg1, $arg2) {
    if $arg1 eq "arg1" and $arg2 eq "arg2" {
        say "ok 2";
    }
    else {
        say "not ok 2";
    }
}

foo "only_one_arg";
foo "arg1", "arg2";
