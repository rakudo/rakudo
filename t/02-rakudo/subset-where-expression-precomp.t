use lib <t/02-rakudo/test-packages>;
use Test;
use nqp;
use experimental :rakuast;
use SubsetWhereExpression;
use SubsetWhereExpressionThunks;

plan 6;

ok 'GET' ~~ SubsetWhereExpressionThunks::Verb,
    'a subset whose where is a call in a precompiled module accepts a value the call matches';
nok 'PUT' ~~ SubsetWhereExpressionThunks::Verb,
    'a subset whose where is a call in a precompiled module rejects a value the call does not match';
is SubsetWhereExpressionThunks::where-param(5), 5,
    'a sub with a where on a parameter in a precompiled module with such a subset accepts a value the where matches';
throws-like { SubsetWhereExpressionThunks::where-param(1.5) }, X::TypeCheck::Binding::Parameter,
    'a sub with a where on a parameter in a precompiled module with such a subset rejects a value the where does not match';
is SubsetWhereExpressionThunks::default-param(1), 2,
    'a sub with a parameter default holding code in a precompiled module with such a subset is called';

my $sc := nqp::getobjsc(SubsetWhereExpression::Verb);
my @nodes;
for ^nqp::scobjcount($sc) -> int $i {
    my $obj := nqp::scgetobj($sc, $i);
    @nodes.push($obj.^name) if nqp::istype($obj, RakuAST::Node);
}
is-deeply @nodes, [],
    'a precompiled module with a subset whose where is a call serializes no AST nodes';

# vim: expandtab shiftwidth=4
