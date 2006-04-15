#!/usr/bin/pugs

use v6;

say "1..3";

sub foo () {
    say "ok 2";
    &baz.goto("param1", "param2");
}

sub bar ($param1, $param2) {
    if $param1 eq "param1" and $param2 eq "param2" {
        say "ok 1";
    } else {
        say "not ok 1";
    }
}

sub baz ($param1, $param2) {
    if $param1 eq "param1" and $param2 eq "param2" {
        say "ok 3";
    } else {
        say "not ok 3";
    }
}

bar("param1", "param2");
foo();
