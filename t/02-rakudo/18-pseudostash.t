use lib $*PROGRAM.parent(2).add('packages/Test-Helpers');
use Test;
use Test::Helpers;

plan 4;

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

# vim: expandtab shiftwidth=4
