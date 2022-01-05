use v6;
use lib <t/packages/>;
use Test;
use Test::Helpers;

# Tests for slang interface that aren't yet spec

plan 1;

is-run ｢
    BEGIN $?LANG.refine_slang(
        'MAIN',
        role { token apostrophe { <[ - ' \\ ]> } },
        role {},
    );
    my $foo\bar = "pass"; say $foo\bar;
｣, :out("pass\n"), 'no crash when giving an Actions class to .refine_slang';

# vim: expandtab shiftwidth=4
