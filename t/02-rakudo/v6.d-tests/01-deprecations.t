use lib <t/spec/packages/>;
use Test;
use Test::Util;

plan 7;

# XXX TODO: swap v6.d.PREVIEW to v6.d, once the latter is available
constant $v6d = 'v6.d.PREVIEW';

################### HELPER ROUTINES ###################################

sub test-deprecation (Str:D $lang, Str:D $code, Bool :$is-visible) {
    is_run '
        use \qq[$lang];
        %*ENV<RAKUDO_NO_DEPRECATIONS>:delete;
        \qq[$code]
    ', { :out(''), :err($is-visible ?? /deprecated/ !! ''), :0status },
        ($is-visible ?? 'shows' !! 'no   ')
    ~ " deprecation message using $lang";
}
sub    is-deprecated (|c) { test-deprecation |c, :is-visible }
sub isn't-deprecated (|c) { test-deprecation |c              }
sub is-newly-deprecated (Str:D $code, Str:D $desc = "with `$code`") {
    subtest $desc => {
        plan 2;
        test-deprecation $v6d,   $code, :is-visible;
        test-deprecation 'v6.c', $code;
    }
}

######################################################################

is-newly-deprecated ｢$ = 4.2.Rat: 42｣;
is-newly-deprecated ｢$ = 4.2.FatRat: 42｣;
is-newly-deprecated ｢$ = FatRat.new(4,2).Rat: 42｣;
is-newly-deprecated ｢$ = FatRat.new(4,2).FatRat: 42｣;
is-newly-deprecated ｢".".IO.chdir: "."｣;

subtest 'IO::Handle.slurp-rest' => {
    plan 2;
    my $file := make-temp-file(:content<foo>).absolute.perl;
    is-newly-deprecated "$file.IO.open.slurp-rest",       '.slurp-rest';
    is-newly-deprecated "$file.IO.open.slurp-rest: :bin", '.slurp-rest: :bin';
}


# Should be removed in 6.d, but I made it just die, at least for now,
# as I'm unsure how the removal is meant to happen, if we still wish
# to have it working in 6.c tests
is_run 'use \qq[$v6d]; use Test; try is_approx 1, 1; $! and say "test passed"',
    {:out(/'test passed'/), :0status }, 'is_approx dies in v6.d';
