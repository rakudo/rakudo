use lib <t/packages>;
use Test;
use Test::Helpers;

plan 3;

subtest "CORE.setting Revision", {
    plan 3;
    is-run q[use v6.c; print CORE-SETTING-REV], "CORE.setting", :out<c>;
    is-run q[use v6.d; print CORE-SETTING-REV], "CORE.d.setting", :out<d>;
    is-run q[use v6.e.PREVIEW; print CORE-SETTING-REV], "CORE.e.setting", :out<e>;
};

subtest "Modifiers", {
    plan 4;
    # This test must be edited to match currently planned revision.
    my $planned_rev = 'e';
    is-run qq[use v6.$planned_rev; print CORE-SETTING-REV], "6.$planned_rev without PREVIEW dies", :exitcode(1), :err(rx:s/Raku v6'.'$planned_rev requires PREVIEW modifier/);
    is-run q[use v6.d.TEST; print CORE-SETTING-REV], "v6.d.TEST loads CORE.d.setting", :out<d>;
    is-run q[use v6.d.TESTDEPR; print CORE-SETTING-REV], "Deprecated modifier generates a warning", :out<d>, :err(rx:s/TESTDEPR modifier is deprecated for Raku'.'d/);
    is-run q[use v6.d.NOMOD; print CORE-SETTING-REV], "Deprecated modifier generates a warning", :exitcode(1), :err(rx:s/No compiler available for Raku v6'.'d'.'NOMOD/);
}

subtest "Class Version", {
    plan 3;
    is-run qq[use v6.c; print PseudoStash.^ver], "6.c class version", :exitcode(0), :out<6.c>;
    is-run qq[use v6.d; print PseudoStash.^ver], "6.c class version on 6.d compiler", :exitcode(0), :out<6.c>;
    is-run qq[use v6.e.PREVIEW; print PseudoStash.^ver], "6.e class version", :exitcode(0), :out<6.e>;
}

done-testing;

# vim: expandtab shiftwidth=4
