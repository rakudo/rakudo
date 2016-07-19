# This file contains various stubs. Note that a few are created already
# outside of the setting, such as Mu/Any/Cool, Attribute, Signature/Parameter,
# Code/Block/Routine/Sub/Method and Str/Int/Num. They are built in BOOTSTRAP.pm
# in Perl6::Metamodel for now, though should be a BEGIN block in CORE.setting
# in the end.
my class Exception { ... }
my class X::AdHoc  { ... }
my class FatRat    { ... }
my class Pair      { ... }
my class Promise   { ... }
my class X::OutOfRange { ... }
my class X::Dynamic::NotFound { ... }
my class X::SecurityPolicy::Eval { ... }

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

sub DYNAMIC(\name) is raw {
    nqp::ifnull(
      nqp::getlexdyn(nqp::unbox_s(name)),
      nqp::stmts(
        nqp::unless(
          nqp::isnull(my $prom := nqp::getlexdyn('$*PROMISE')),
          (my Mu $x := nqp::getlexreldyn(
            nqp::getattr($prom,Promise,'$!dynamic_context'),nqp::unbox_s(name))
          )
        ),
        nqp::if(
          nqp::isnull($x),
          nqp::stmts(
            (my str $pkgname = nqp::replace(nqp::unbox_s(name),1,1,'')),
            ($x := nqp::ifnull(nqp::atkey(GLOBAL.WHO,$pkgname),
                   nqp::ifnull(nqp::atkey(PROCESS.WHO,$pkgname),
                   Rakudo::Internals.INITIALIZE-DYNAMIC(nqp::unbox_s(name))))
            )
          )
        ),
        nqp::if(
          nqp::istype($x,Exception),
          Failure.new($x),
          $x
        )
      )
    )
}

# Set up ClassHOW's auto-gen proto (nested scope so it won't
# actually appear in the setting).
{
    my class Dummy {
        our proto method AUTOGEN(::T $: |) { * }
    }
    Dummy.HOW.set_autogen_proto(&Dummy::AUTOGEN);
}

# vim: ft=perl6 expandtab sw=4
