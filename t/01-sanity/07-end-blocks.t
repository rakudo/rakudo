#!/usr/bin/pugs

use v6;

say "1..2";

our $was_in_second_end_block = 0;

END {
    if $was_in_second_end_block {
        say "ok 2";
    } else {
        say "not ok 2";
    }
}

END {
    $was_in_second_end_block = 1;
    say "ok 1";
}
