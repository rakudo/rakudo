use Test;

plan 6;

# In an LTM alternation, a branch that begins with a <before ...> lookahead
# contributes what the lookahead can match to its declarative prefix. The
# regex-argument thunk must carry its regex QAST where the NFA builder can
# find it; without that the prefix ends at the zero-width assertion and the
# branch loses to shorter alternatives it should beat.

grammar Expr {
    token TOP { <expression> }
    rule expression { \s* [<operation>|<term>] }
    token variable { <alpha> <alnum>* }
    rule term { \s* <variable> }
    token operation { <circumfix_operation> | <infix_operation_chain> }
    token infix_term { <circumfix_operation> | <term> }
    my $in_ops = "['/'|'-']";
    rule infix_operation_chain {<before .+? <$in_ops>><infix_term>[ $<op>=<$in_ops> <infix_term>]+}
    rule circumfix_operation { '(' <expression> ')' }
}

my $m = Expr.parse('(a-b)/c');
ok $m, 'a parenthesized subexpression followed by an infix parses';
ok $m && $m<expression><operation><infix_operation_chain>,
    'LTM picked the infix chain over the shorter circumfix branch';
ok Expr.parse('(y2-y1)/(x2-x1)'), 'an infix between two parenthesized subexpressions parses';

# The <?before ...> spelling wraps the same assertion and must rank the same.
grammar QExpr {
    token TOP { <expression> }
    rule expression { \s* [<operation>|<term>] }
    token variable { <alpha> <alnum>* }
    rule term { \s* <variable> }
    token operation { <circumfix_operation> | <infix_operation_chain> }
    token infix_term { <circumfix_operation> | <term> }
    my $in_ops = "['/'|'-']";
    rule infix_operation_chain {<?before .+? <$in_ops>><infix_term>[ $<op>=<$in_ops> <infix_term>]+}
    rule circumfix_operation { '(' <expression> ')' }
}

my $qm = QExpr.parse('(a-b)/c');
ok $qm, 'the <?before ...> spelling also parses the expression';
ok $qm && $qm<expression><operation><infix_operation_chain>,
    'the <?before ...> spelling also picks the infix chain';

# A negated lookahead with a regex argument must still veto the match.
nok 'xbc' ~~ /<!before x> . bc/, '<!before ...> still rejects at a position its argument matches';

# vim: expandtab shiftwidth=4
