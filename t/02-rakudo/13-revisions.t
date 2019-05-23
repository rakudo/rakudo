use lib <t/packages>;
use Test;
use Test::Helpers;

plan 2;

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
    is-run qq[use v6.$planned_rev; print CORE-SETTING-REV], "6.$planned_rev without PREVIEW dies", :exitcode(1), :err(rx:s/Perl v6'.'$planned_rev requires modifier PREVIEW/);
    is-run q[use v6.d.TEST; print CORE-SETTING-REV], "v6.d.TEST loads CORE.d.setting", :out<d>;
    is-run q[use v6.d.TESTDEPR; print CORE-SETTING-REV], "Deprecated modifier generates a warning", :out<d>, :err(rx:s/TESTDEPR modifier is deprecated for Perl 6'.'d/);
    is-run q[use v6.d.NOMOD; print CORE-SETTING-REV], "Deprecated modifier generates a warning", :exitcode(1), :err(rx:s/No compiler available for Perl v6'.'d'.'NOMOD/);
}

done-testing;
