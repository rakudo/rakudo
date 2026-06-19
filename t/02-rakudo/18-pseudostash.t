use lib $*PROGRAM.parent(2).add('packages/Test-Helpers');
use Test;
use Test::Helpers;

plan 7;

use MONKEY-SEE-NO-EVAL;

{ # Make sure CLIENT:: works for code invoked from NQP world
    # Wether or not a code object is invoked by Raku or NQP code is pretty much implementation specific. Moreover,
    # the chosen PseudoStash path `CALLER::CLIENT::CLIENT::` also depends on how COERCE method is invoked by Rakudo.
    # Therefore any changes related to coercion protocol implementation may require tweking of this test.
    for <c d e.PREVIEW> -> $rev {
        is-run "use v6.$rev;\nmy \$foo = q<This is 6.$rev>;\n"
                ~ q:to/TEST-CODE/,
                    my class C {
                        method FALLBACK($,|) {
                            print CALLER::CLIENT::MY::<$foo>;
                        }
                    };
                    C.fubar;
                    TEST-CODE
                "CLIENT:: doesn't fail on NQP packages for 6.$rev",
                :out("This is 6.$rev"),
                :err("");
    }
}

# https://github.com/rakudo/rakudo/issues/1835
{
    my $a;
    $a = PseudoStash.new for ^9999;
    is $a.gist, 'PseudoStash.new(($_ => 9998))', 'did not hang';
}

# A lexically-bound type used as a package qualifier resolves the symbol
# through that lexical's value, rather than requiring it to have a
# compile-time value (`my \t := SomeEnum; t::{$key}`).
is EVAL(q/enum E198 <a b c>; my \t := E198; my $k = "b"; ~t::{$k}/), 'b',
    'indirect lookup through a runtime lexical type with a hash-index key';
is EVAL(q/enum E199 <a b c>; my \t := E199; ~t::<b>/), 'b',
    'indirect lookup through a runtime lexical type with a literal key';

# The same holds for a call qualified by a runtime lexical package.
is EVAL(q/class K200 { our sub gv { 42 } }; my \t := K200; t::gv()/), 42,
    'call qualified by a runtime lexical package';

# vim: expandtab shiftwidth=4
