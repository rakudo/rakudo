# This file contains various stubs. Note that a few are created already
# outside of the setting, such as Mu/Any/Cool, Attribute, Signature/Parameter,
# Code/Block/Routine/Sub/Method and Str/Int/Num. They are built in BOOTSTRAP.nqp
# in Perl6::Metamodel for now, though should be a BEGIN block in CORE.setting
# in the end.
my class Exception { ... }
my class X::AdHoc  { ... }
my class FatRat    { ... }
my class Pair      { ... }
my class Promise   { ... }
my class Channel   { ... }
my class X::OutOfRange { ... }
my class X::Dynamic::NotFound { ... }
my class X::SecurityPolicy::Eval { ... }
my class X::Channel::ReceiveOnClosed { ... }

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

my class Lock is repr('ReentrantMutex') { ... }
my class Lock::Async { ... }

sub DYNAMIC(\name, Mu $ctx is raw = nqp::null()) is raw {
    nqp::unless(
        nqp::defined($ctx),
        ($ctx := nqp::ctxcaller(nqp::ctx()))
    );
    nqp::ifnull(
      nqp::getlexreldyn($ctx, name),
      nqp::stmts(
        nqp::unless(
          nqp::isnull(my \promise := nqp::getlexreldyn($ctx, '$*PROMISE')),
          nqp::stmts(
            (my $promise-ctx := nqp::getattr(promise,Promise,'$!dynamic_context')),
            (my Mu \value :=
              nqp::ifnull(
                nqp::getlexreldyn($promise-ctx, name),
                (&DYNAMIC(name, $promise-ctx))
            ))
          )
        ),
        nqp::ifnull(
          value,
          nqp::stmts(
            (my str $pkgname = nqp::if(nqp::eqat(name, '*', 1), nqp::replace(name, 1, 1, ''), name )),
            nqp::ifnull(
              nqp::atkey(GLOBAL.WHO,$pkgname),
              nqp::ifnull(
                nqp::atkey(PROCESS.WHO,$pkgname),
                Rakudo::Internals.INITIALIZE-DYNAMIC(name)
              )
            )
          )
        )
      )
    )
}

# Set up ClassHOW's auto-gen proto (nested scope so it won't
# actually appear in the setting).
{
    my class Dummy {
        our proto method AUTOGEN(::T $: |) {*}
    }
    Dummy.HOW.set_autogen_proto(&Dummy::AUTOGEN);
}

# vim: ft=perl6 expandtab sw=4
