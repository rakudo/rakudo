use Test;

# RakuAST's compile-time trial-bind in RakuAST::Call::Name::PERFORM-CHECK
# must skip type-capture generics (Perl6::Metamodel::GenericHOW). They
# bind to a real type at role/sub instantiation time and match the
# called sub's signature then. Without the skip, calling any sub from
# a role body with a type-capture argument errored at parse time.
use MONKEY-SEE-NO-EVAL;
lives-ok {
    EVAL 'sub frob($obj) { $obj }; role R[::T] { my $x = frob(T) }';
}, 'role body can call a sub with a type-capture argument';

done-testing;

# vim: expandtab shiftwidth=4
