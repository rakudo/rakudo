use Test;

plan 5;

# A QUIT phaser reports whether it handled the exception by what it returns:
# Nil after a when/default succeeds, the exception otherwise. The supply
# machinery keys on that to decide between closing the tap and rethrowing.

# A default in a whenever's QUIT intercepts the quit, so the react block
# completes instead of dying.
is do {
    my $q = "not-run";
    react {
        whenever (supply { die "boom" }) -> $v {
            QUIT { default { $q = "QUIT:" ~ .^name } }
        }
    }
    $q
}, 'QUIT:X::AdHoc', 'a default in a whenever QUIT handles the quit and react completes';

# The same interception works for a whenever inside a supply block.
is do {
    my $q = "not-run";
    my $s = supply {
        whenever (supply { die "boom" }) -> $v {
            QUIT { default { $q = "QUIT:" ~ .^name } }
        }
    }
    react { whenever $s -> $v { } }
    $q
}, 'QUIT:X::AdHoc', 'a default in a supply-block whenever QUIT handles the quit';

# A QUIT whose when clauses do not match leaves the quit unhandled, so the
# react block dies with the original exception.
{
    my $q = "not-run";
    throws-like {
        react {
            whenever (supply { die "boom" }) -> $v {
                QUIT { when X::OutOfRange { $q = "wrong" } }
            }
        }
    }, Exception, message => /boom/,
        'a QUIT with no matching when rethrows and the react block dies';
    is $q, "not-run", 'the non-matching when clause did not run';
}

# The phaser itself returns Nil when a default succeeds.
is do {
    my $b = -> $v { QUIT { default { 42 } }; 1 };
    $b.phasers("QUIT")[0](X::AdHoc.new(payload => "x")) === Nil
}, True, 'a QUIT phaser returns Nil after its default succeeds';

# vim: expandtab shiftwidth=4
