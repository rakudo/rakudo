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

sub DYNAMIC(\name) is raw {  # is implementation-detail
# Please leave this code here to be enable only for tracing calls to
# dynamic variables in the setting and during setting compilation.
#my $frame := callframe(1);
#nqp::say(name ~ ": " ~ $frame.file ~ "(" ~ $frame.line ~ ")");
    nqp::ifnull(
      nqp::getlexdyn(name),
      nqp::stmts(
        nqp::unless(
          nqp::isnull(my \promise := nqp::getlexdyn('$*PROMISE')),
          (my Mu \value := nqp::getlexreldyn(
            nqp::getattr(promise,Promise,'$!dynamic_context'),name)
          )
        ),
        nqp::ifnull(
          value,
          nqp::stmts(
            (my str $pkgname = nqp::replace(name,1,1,'')),
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
        our proto method AUTOGEN-METHOD(::T $: |) {*}
        our proto submethod AUTOGEN-SUBMETHOD(::T $: |) {*}
    }
    Dummy.HOW.set_autogen_proto(&Dummy::AUTOGEN-METHOD, &Dummy::AUTOGEN-SUBMETHOD);
}

# vim: expandtab shiftwidth=4
