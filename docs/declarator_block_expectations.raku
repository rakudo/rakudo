#! /usr/bin/env perl6

use v6;
use Test;

    #| A line

    #| of documentation
    #|
    #| For the following declarand
    sub foo {}
    #= Another line

    #= of documentation
    #=
    #= For the preceding declarand.

is &foo.WHY.Str,
   q:to/WAS_EXPECTED/;
                          A line
                          of documentation

                          For the following declarand
                          Another line
                          of documentation

                          For the preceding declarand.
                          WAS_EXPECTED

is &foo.WHY.leading,
   q:to/WAS_EXPECTED/;
                          A line
                          of documentation

                          For the following declarand
                          WAS_EXPECTED

is &foo.WHY.trailing,
   q:to/WAS_EXPECTED/;
                          Another line
                          of documentation

                          For the preceding declarand.
                          WAS_EXPECTED


done-testing();
