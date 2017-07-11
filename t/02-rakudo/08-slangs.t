use v6;
use lib <t/spec/packages/>;
use Test;
use Test::Util;

# Tests for slang interface that aren't yet spec

plan 1;

is_run ｢
        BEGIN $?LANG.refine_slang(
            'MAIN',
            role { token apostrophe { <[ - ' \\ ]> } },
            role {},
        );
        my $foo\bar = "pass"; say $foo\bar;
    ｣, {:out("pass\n"), :err(''), :0status},
    'no crash when giving an Actions class to .refine_slang';
