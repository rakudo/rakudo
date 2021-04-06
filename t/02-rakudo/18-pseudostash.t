use v6;
use lib $?FILE.IO.parent(2).add('packages');
use Test;
use Test::Helpers;

plan 3;

{ # Make sure CLIENT:: works for code invoked from NQP world
    # Wether or not a code object is invoked by Raku or NQP code is pretty much implementation specific. Moreover,
    # the chosen PseudoStash path `CALLER::CLIENT::CLIENT::` also depends on how COERCE method is invoked by Rakudo.
    # Therefore any changes related to coercion protocol implementation may require tweking of this test.
    for <c d e.PREVIEW> -> $rev {
        is-run "use v6.$rev;\nmy \$foo = q<This is 6.$rev>;\n"
                ~ q:to/TEST-CODE/,
                    my class C {
                        method COERCE(Int $v) {
                            print CALLER::CLIENT::CLIENT::MY::<$foo>;
                            C.new;
                        }
                    };
                    C(42);
                    TEST-CODE
                "CLIENT:: doesn't fail on NQP packages for 6.$rev",
                :out("This is 6.$rev"),
                :err("");
    }
}

done-testing;
