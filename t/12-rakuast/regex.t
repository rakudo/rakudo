use MONKEY-SEE-NO-EVAL;
use Test;

plan 12;

sub rx(RakuAST::Regex $body) {
    EVAL RakuAST::QuotedRegex.new(:$body)
}

{
    is "foobarbaz" ~~ rx(RakuAST::Regex::Literal.new('foo')),
        'foo',
        'Simple literal regex matches at start of string';
    is "42foobarbaz" ~~ rx(RakuAST::Regex::Literal.new('foo')),
        'foo',
        'Simple literal regex matches in middle of string';
    nok "barbaz" ~~ rx(RakuAST::Regex::Literal.new('foo')),
        'String without literal is not matched';

    is "abcd" ~~ rx(RakuAST::Regex::SequentialAlternation.new(
            RakuAST::Regex::Literal.new('b'),
            RakuAST::Regex::Literal.new('bc'))),
        'b',
        'Sequential alternation of literals takes first match even if second is longer';
    is "abcd" ~~ rx(RakuAST::Regex::SequentialAlternation.new(
            RakuAST::Regex::Literal.new('x'),
            RakuAST::Regex::Literal.new('bc'))),
        'bc',
        'Sequential alternation of literals takes second match if first fails';
    nok "abcd" ~~ rx(RakuAST::Regex::SequentialAlternation.new(
            RakuAST::Regex::Literal.new('x'),
            RakuAST::Regex::Literal.new('y'))),
        'Sequential alternation of literals fails if no alternative matches';

    is "abcd" ~~ rx(RakuAST::Regex::Alternation.new(
            RakuAST::Regex::Literal.new('b'),
            RakuAST::Regex::Literal.new('bc'))),
        'bc',
        'LTM alternation of literals takes longest match even if it is not first';
    is "abcd" ~~ rx(RakuAST::Regex::Alternation.new(
            RakuAST::Regex::Literal.new('x'),
            RakuAST::Regex::Literal.new('bc'))),
        'bc',
        'Alternation of literals takes second match if first fails';
    nok "abcd" ~~ rx(RakuAST::Regex::Alternation.new(
            RakuAST::Regex::Literal.new('x'),
            RakuAST::Regex::Literal.new('y'))),
        'Alternation of literals fails if no alternative matches';

    is "abcd" ~~ rx(RakuAST::Regex::Conjunction.new(
            RakuAST::Regex::CharClass::Any.new,
            RakuAST::Regex::Literal.new('c'))),
        'c',
        'Conjunction matches when both items match';
    nok "abcd" ~~ rx(RakuAST::Regex::Conjunction.new(
            RakuAST::Regex::CharClass::Any.new,
            RakuAST::Regex::Literal.new('x'))),
        'Conjunction fails when one item does not match';
    nok "abcd" ~~ rx(RakuAST::Regex::Conjunction.new(
            RakuAST::Regex::CharClass::Any.new,
            RakuAST::Regex::Literal.new('cd'))),
        'Conjunction fails when items match different lengths';
}
