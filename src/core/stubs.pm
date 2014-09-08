# This file contains various stubs. Note that a few are created already
# outside of the setting, such as Mu/Any/Cool, Attribute, Signature/Parameter,
# Code/Block/Routine/Sub/Method and Str/Int/Num. They are built in BOOTSTRAP.pm
# in Perl6::Metamodel for now, though should be a BEGIN block in CORE.setting
# in the end.
my class Seq is List does Positional { }
my class Exception { ... }
my class X::AdHoc  { ... }
my class FatRat    { ... }
my class Enum      { ... }
my class X::OutOfRange { ... }
my class X::Dynamic::NotFound { ... }

my role QuantHash { ... }
my role Setty { ... }
my class Set { ... }
my class SetHash { ... }

my role Baggy { ... }
my class Bag { ... }
my class BagHash { ... }

my role Mixy { ... }
my class Mix { ... }
my class MixHash { ... }

sub DYNAMIC(\name) is rw { 
    my Mu $x := nqp::getlexdyn(nqp::unbox_s(name));
    if nqp::isnull($x) {
        my str $pkgname = nqp::replace(nqp::unbox_s(name), 1, 1, '');
        if nqp::existskey(GLOBAL.WHO, $pkgname) {
            $x := nqp::atkey(GLOBAL.WHO, $pkgname);
        }
        elsif nqp::existskey(PROCESS.WHO, $pkgname) {
            $x := nqp::atkey(PROCESS.WHO, $pkgname);
        }
        elsif try INITIALIZE(name) -> $result {
            $x := nqp::ordat(nqp::unbox_s(name),0) == 64
              ?? @($result)
              !! $result;
        }
        else {
            fail X::Dynamic::NotFound.new(:name(name));
        }
    }
    $x
}

# prime the automagic dynamic variable initializers
proto sub INITIALIZE(|) { * }
#multi sub INITIALIZE('$*FOO') {   # example stub
#    PROCESS::<$FOO> := "foo";
#}

# Set up ClassHOW's auto-gen proto (nested scope so it won't
# actually appear in the setting).
{
    my class Dummy {
        our proto method AUTOGEN(::T $: |) { * }
    }
    Dummy.HOW.set_autogen_proto(&Dummy::AUTOGEN);
}

# vim: ft=perl6 expandtab sw=4
