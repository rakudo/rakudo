use Test;
use experimental :macros;

#L<S06/"Macros">

plan 2;

# https://github.com/Raku/old-issue-tracker/issues/2952
lives-ok
    { EVAL 'macro pathological { AST.new }; pathological();' },
    "macro returning AST.new doesn't blow up";

# https://github.com/Raku/old-issue-tracker/issues/2951
{
    try EVAL 'macro ma { die 1 }; ma';
    is $!, 1, "die-ing inside a macro dies normally.";
}

# vim: expandtab shiftwidth=4
