use lib <t/packages/Test-Helpers>;
use Test;
use Test::Helpers;

# Verifies the language-version gate on the nested-package-collision
# behavior introduced by #6122 (see #5854). The pattern in question is:
#
#     module Foo::Bar { class Foo::Bar { method x { 42 } } }
#
# Pre-6.e: the class silently replaces the module in the outer stash
#          (steal_WHO); Foo::Bar resolves to the class.
# 6.e:     the class installs as Foo::Bar::Foo::Bar; Foo::Bar stays
#          the module.
#
# Both frontends must agree on the behavior selected by the language
# version. We drive the matrix via is-run, toggling RAKUDO_RAKUAST to
# exercise the RakuAST frontend.

plan 6;

my $code_6d = q:to/CODE/;
    use v6.d;
    module Foo::Bar { class Foo::Bar { method x { 42 } } }
    print Foo::Bar.HOW.^name ~ ';' ~ Foo::Bar.x;
    CODE

my $code_6e = q:to/CODE/;
    use v6.e.PREVIEW;
    module Foo::Bar { class Foo::Bar { method x { 42 } } }
    print Foo::Bar.HOW.^name ~ ';' ~ Foo::Bar::Foo::Bar.x;
    CODE

# Non-enclosing module collision (`module A::B::Mod {}; class A::B::Mod {}`
# at top level) is a different pattern: not an enclosing-package rewrite,
# just two declarations at the same address. Both frontends must reject
# it as a Redeclaration on every revision, rather than silently replacing
# the module with the class.
my $code_broader = q:to/CODE/;
    module Alpha::Beta::Mod { }
    class Alpha::Beta::Mod { method x { 7 } }
    print Alpha::Beta::Mod.x;
    CODE

{
    temp %*ENV;
    %*ENV<RAKUDO_RAKUAST>:delete;
    is-run $code_6d, :out('Perl6::Metamodel::ClassHOW;42'),
      '6.d traditional: silent-replace';
    is-run $code_6e, :out('Perl6::Metamodel::ModuleHOW;42'),
      '6.e traditional: nested Foo::Bar::Foo::Bar';
    is-run $code_broader, :err(/'Redeclaration of symbol' .* 'Alpha::Beta::Mod'/),
      :exitcode(1), :out(''),
      '6.d traditional: non-enclosing collision is a Redeclaration';
}

{
    temp %*ENV;
    %*ENV<RAKUDO_RAKUAST> = '1';
    is-run $code_6d, :out('Perl6::Metamodel::ClassHOW;42'),
      '6.d RakuAST: silent-replace (emulates traditional)';
    is-run $code_6e, :out('Perl6::Metamodel::ModuleHOW;42'),
      '6.e RakuAST: nested Foo::Bar::Foo::Bar';
    is-run $code_broader, :err(/'Redeclaration of symbol' .* 'Alpha::Beta::Mod'/),
      :exitcode(1), :out(''),
      '6.d RakuAST: non-enclosing collision is a Redeclaration';
}

# vim: expandtab shiftwidth=4
