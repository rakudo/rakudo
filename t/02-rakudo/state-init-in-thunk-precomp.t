use lib <t/02-rakudo/test-packages>;
use Test;
use nqp;
use StateInitInThunk;

plan 2;

unless nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    skip-rest 'state initialization in a thunk needs the RakuAST frontend';
    exit;
}

is-deeply StateInitInThunk::enter-counts(), [10, 11, 12],
    'an ENTER statement of a precompiled module initializes a state variable once';
is-deeply StateInitInThunk::check-value(), (6, 7),
    'a CHECK statement of a precompiled module initializes a state variable once';
