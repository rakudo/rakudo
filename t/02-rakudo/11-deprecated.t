use lib <t/packages/>;
use Test;
use Test::Helpers;

plan 2;

sub test-deprecation (Str:D $lang, Bool :$is-visible, |c) {
    my $args = c.perl;
    is-run '
        use \qq[$lang];
        %*ENV<RAKUDO_NO_DEPRECATIONS>:delete;
        DEPRECATED "meow", |(\qq[$args]);
    ', :err($is-visible ?? /meow/ !! ''),
        ($is-visible ?? 'shows' !! 'no') ~ " deprecation message with $args";
}
sub    is-deprecated (|c) { test-deprecation |c, :is-visible }
sub isn't-deprecated (|c) { test-deprecation |c              }

isn't-deprecated 'v6.c',         v6.d, v6.e, :lang-vers;

# XXX TODO: remove `.PREVIEW` part when 6.d comes out:
is-deprecated 'v6.d.PREVIEW', v6.d, v6.e, :lang-vers;
