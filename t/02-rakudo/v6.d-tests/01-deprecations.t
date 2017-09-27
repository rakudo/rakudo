use lib <t/spec/packages/>;
use Test;
use Test::Util;

plan 10;

# XXX TODO: swap v6.d.PREVIEW to v6.d, once the latter is available
constant $v6d = 'v6.d.PREVIEW';

################### HELPER ROUTINES ###################################

sub test-deprecation (
    Str:D $lang, Str:D $code, Str:D $desc = "with `$code`",
    Bool :$is-visible
) {
    is_run '
        use \qq[$lang];
        %*ENV<RAKUDO_NO_DEPRECATIONS>:delete;
        \qq[$code]
    ', { :out(''), :err($is-visible ?? /deprecated/ !! ''), :0status },
        ($is-visible ?? 'shows' !! 'no   ')
    ~ " deprecation message $desc [using $lang]";
}
sub    is-deprecated (|c) { test-deprecation |c, :is-visible }
sub isn't-deprecated (|c) { test-deprecation |c              }
sub is-newly-deprecated (|c) {
    is-deprecated $v6d, |c;
    isn't-deprecated 'v6.c', |c;
}

######################################################################

is-newly-deprecated ｢$ = 4.2.Rat: 42｣;
is-newly-deprecated ｢$ = 4.2.FatRat: 42｣;
is-newly-deprecated ｢$ = FatRat.new(4,2).Rat: 42｣;
is-newly-deprecated ｢$ = FatRat.new(4,2).FatRat: 42｣;
is-newly-deprecated ｢".".IO.chdir: "."｣;
