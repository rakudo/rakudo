use lib <t/02-rakudo/test-packages>;
use Test;

plan 11;

# `require` merges the required unit's symbols into the caller's
# %?REQUIRE-SYMBOLS, which the runtime symbolic lookup consults before
# anything visible at compile time. An indirect lookup written with a
# constant string, like ::("EXPORT"), must therefore run at run time
# rather than fold to whatever the string resolves to during
# compilation. A direct EXPORT::<...> reference still resolves at
# compile time.

class BundleShadow {
    method which() { "lexical" }
    class Inner { method which() { "lexical-inner" } }
}
sub bundle-shadow($v) { "lexical-" ~ $v }

require ::("BundleExport");
require ::("ShadowUnit");

ok ::("EXPORT").WHO<DEFAULT>:exists,
    'the required module EXPORT::DEFAULT is visible through ::("EXPORT")';

my @pairs = ::("EXPORT").WHO<DEFAULT>.WHO.pairs;
is @pairs.map(*.key).sort, ('&bundle-export',),
    'the required module exports are read from ::("EXPORT")';

is ::("EXPORT").WHO<DEFAULT>.WHO<&bundle-export>('x'), 'bundled x',
    'the re-read exported sub is callable';

is ::("BundleShadow").which, 'required',
    'a require-injected class shadows a same-named lexical class in ::("...")';

is ::("BundleShadow::Inner").which, 'required-inner',
    'a multi-part ::("...") walks the require-injected package at run time';

is (sub ($a = ::("BundleShadow")) { $a.which })(), 'required',
    'a parameter default holding ::("...") looks the symbol up per call';

is "{ ::("BundleShadow").which }", 'required',
    'a ::("...") interpolated in a string runs the symbolic lookup';

is 5.&::("bundle-shadow"), 'required-5',
    'a .&::("...") call runs the symbolic lookup that sees require-injected subs';

my $hyper-name = "bundle-shadow";
is-deeply (1, 2)>>.&::($hyper-name), ("required-1", "required-2"),
    'a hyper .&::($name) call resolves the routine at run time';

my constant IndirectInt = ::("Int");
is IndirectInt.^name, 'Int',
    'a constant initialized from an indirect lookup evaluates it at BEGIN time';

is BundleShadow.which, 'lexical',
    'a direct name still resolves to the lexical class';

# vim: expandtab shiftwidth=4
