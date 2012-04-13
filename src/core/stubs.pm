# This file contains various stubs. Note that a few are created already
# outside of the setting, such as Mu/Any/Cool, Attribute, Signature/Parameter,
# Code/Block/Routine/Sub/Method and Str/Int/Num. They are built in BOOTSTRAP.pm
# in Perl6::Metamodel for now, though should be a BEGIN block in CORE.setting
# in the end.
my class Whatever { ... }
my class Bag { ... }
my class KeyBag { ... }
my class KeySet { ... }
my class KeyHash is Iterable does Associative { }
my class Seq is List does Positional { }
my class Exception { ... }
my class X::AdHoc  { ... }
my class FatRat    { ... }
my class Enum      { ... }
my class X::OutOfRange { ... }

sub DYNAMIC(\$name) is rw { 
    my Mu $x := pir::find_dynamic_lex__Ps(nqp::unbox_s($name));
    if nqp::isnull($x) {
        my str $pkgname = pir::replace__Ssiis(nqp::unbox_s($name), 1, 1, '');
        if nqp::existskey(GLOBAL.WHO, $pkgname) { $x := nqp::atkey(GLOBAL.WHO, $pkgname) }
        elsif nqp::existskey(PROCESS.WHO, $pkgname) { $x := nqp::atkey(PROCESS.WHO, $pkgname) }
        else { fail "Dynamic variable $name not found" }
    }
    $x
}

# Set up ClassHOW's auto-gen proto (nested scope so it won't
# actually appear in the setting).
{
    my class Dummy {
        our proto method AUTOGEN(::T $: |$) { * }
    }
    Dummy.HOW.set_autogen_proto(&Dummy::AUTOGEN);
}

