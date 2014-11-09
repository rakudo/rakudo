# This file contains various stubs. Note that a few are created already
# outside of the setting, such as Mu/Any/Cool, Attribute, Signature/Parameter,
# Code/Block/Routine/Sub/Method and Str/Int/Num. They are built in BOOTSTRAP.pm
# in Perl6::Metamodel for now, though should be a BEGIN block in CORE.setting
# in the end.
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
    my Mu \x := nqp::getlexdyn(nqp::unbox_s(name));
    if nqp::isnull(x) {
        my str $pkgname = nqp::replace(nqp::unbox_s(name), 1, 1, '');
        if nqp::existskey((my \globalwho := GLOBAL.WHO), $pkgname) {
            x := nqp::atkey(globalwho, $pkgname);
        }
        elsif nqp::existskey((my \processwho := PROCESS.WHO), $pkgname) {
            x := nqp::atkey(processwho, $pkgname);
        }
        else {
#my $last = now;
#say "initializing {name}";
            x := INITIALIZE_DYNAMIC(name);
#say "    done at {now - $last}";
            fail x if nqp::istype(x, Exception);
        }
    }
    x
}

# prime the automagic dynamic variable initializers
proto sub INITIALIZE_DYNAMIC(|) { * }
multi sub INITIALIZE_DYNAMIC(\name) {
    X::Dynamic::NotFound.new(:name(name));
}
#multi sub INITIALIZE_DYNAMIC('$*FOO') {   # example stub
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
