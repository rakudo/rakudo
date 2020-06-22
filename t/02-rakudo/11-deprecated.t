use lib <t/packages/>;
use Test;
use Test::Helpers;

plan 2;

sub test-deprecation (Str:D $lang, Bool :$is-visible, |c) {
    my $args = c.raku;
    is-run '
        use \qq[$lang];
        %*ENV<RAKUDO_NO_DEPRECATIONS>:delete;
        Rakudo::Deprecations.DEPRECATED: "meow", |(\qq[$args]);
    ', :err($is-visible ?? /meow/ !! ''),
        ($is-visible ?? 'shows' !! 'no') ~ " deprecation message with $args";
}
sub    is-deprecated (|c) { test-deprecation |c, :is-visible }
sub isn't-deprecated (|c) { test-deprecation |c              }

isn't-deprecated 'v6.c', v6.d, v6.e, :lang-vers;
is-deprecated    'v6.d', v6.d, v6.e, :lang-vers;

# vim: expandtab shiftwidth=4
