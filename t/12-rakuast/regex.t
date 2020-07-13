use MONKEY-SEE-NO-EVAL;
use Test;

plan 44;

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

    is "abcde" ~~ rx(RakuAST::Regex::Sequence.new(
            RakuAST::Regex::CharClass::Any.new,
            RakuAST::Regex::Literal.new('d'))),
        'cd',
        'Sequence needs one thing to match after the other (pass case)';
    nok "abcde" ~~ rx(RakuAST::Regex::Sequence.new(
            RakuAST::Regex::CharClass::Any.new,
            RakuAST::Regex::Literal.new('a'))),
        'Sequence needs one thing to match after the other (failure case)';

    is "abcde" ~~ rx(RakuAST::Regex::Sequence.new(
            RakuAST::Regex::Anchor::BeginningOfString.new,
            RakuAST::Regex::CharClass::Any.new)),
        'a',
        'Beginning of string anchor works (pass case)';
    nok "abcde" ~~ rx(RakuAST::Regex::Sequence.new(
            RakuAST::Regex::Anchor::BeginningOfString.new,
            RakuAST::Regex::Literal.new('b'))),
        'Beginning of string anchor works (failure case)';

    is "abcde" ~~ rx(RakuAST::Regex::Sequence.new(
            RakuAST::Regex::CharClass::Any.new,
            RakuAST::Regex::Anchor::EndOfString.new)),
        'e',
        'End of string anchor works (pass case)';
    nok "abcde" ~~ rx(RakuAST::Regex::Sequence.new(
            RakuAST::Regex::Literal.new('b'),
            RakuAST::Regex::Anchor::EndOfString.new)),
        'End of string anchor works (failure case)';

    is "elizabeth the second" ~~ rx(RakuAST::Regex::Sequence.new(
            RakuAST::Regex::CharClass::Any.new,
            RakuAST::Regex::Literal.new('e'),
            RakuAST::Regex::Anchor::RightWordBoundary.new)),
        'he',
        'Right word boundary works (pass case)';
    nok "elizabeth second" ~~ rx(RakuAST::Regex::Sequence.new(
            RakuAST::Regex::CharClass::Any.new,
            RakuAST::Regex::Literal.new('e'),
            RakuAST::Regex::Anchor::RightWordBoundary.new)),
        'Right word boundary works (failure case)';

    is "cat ethics committee" ~~ rx(RakuAST::Regex::Sequence.new(
            RakuAST::Regex::Anchor::LeftWordBoundary.new,
            RakuAST::Regex::CharClass::Any.new,
            RakuAST::Regex::Literal.new('t'))),
        'et',
        'Left word boundary works (pass case)';
    nok "cat committee" ~~ rx(RakuAST::Regex::Sequence.new(
            RakuAST::Regex::Anchor::LeftWordBoundary.new,
            RakuAST::Regex::CharClass::Any.new,
            RakuAST::Regex::Literal.new('t'))),
        'Left word boundary works (failure case)';

    is "99cents" ~~ rx(RakuAST::Regex::QuantifiedAtom.new(
            atom => RakuAST::Regex::CharClass::Digit.new,
            quantifier => RakuAST::Regex::Quantifier::OneOrMore.new)),
        '99',
        'Quantified built-in character class matches';
    is "99cents" ~~ rx(RakuAST::Regex::QuantifiedAtom.new(
            atom => RakuAST::Regex::CharClass::Digit.new(:negated),
            quantifier => RakuAST::Regex::Quantifier::OneOrMore.new)),
        'cents',
        'Quantified negated built-in character class matches';
    is "99cents" ~~ rx(RakuAST::Regex::QuantifiedAtom.new(
            atom => RakuAST::Regex::CharClass::Digit.new,
            quantifier => RakuAST::Regex::Quantifier::OneOrMore.new(
                backtrack => RakuAST::Regex::Backtrack::Frugal
            ))),
        '9',
        'Quantified built-in character class matches (frugal mode)';
    is "99cents" ~~ rx(RakuAST::Regex::QuantifiedAtom.new(
            atom => RakuAST::Regex::CharClass::Digit.new(:negated),
            quantifier => RakuAST::Regex::Quantifier::OneOrMore.new(
                backtrack => RakuAST::Regex::Backtrack::Frugal
            ))),
        'c',
        'Quantified negated built-in character class matches (frugal mode)';

    is "99cents" ~~ rx(RakuAST::Regex::Sequence.new(
            RakuAST::Regex::Anchor::BeginningOfString.new,
            RakuAST::Regex::QuantifiedAtom.new(
                atom => RakuAST::Regex::CharClass::Digit.new,
                quantifier => RakuAST::Regex::Quantifier::OneOrMore.new
            ),
            RakuAST::Regex::Literal.new('9'),
        )),
        '99',
        'Greedy quantifier will backtrack';
    nok "99cents" ~~ rx(RakuAST::Regex::Sequence.new(
            RakuAST::Regex::Anchor::BeginningOfString.new,
            RakuAST::Regex::QuantifiedAtom.new(
                atom => RakuAST::Regex::CharClass::Digit.new,
                quantifier => RakuAST::Regex::Quantifier::OneOrMore.new(
                    backtrack => RakuAST::Regex::Backtrack::Ratchet
                )
            ),
            RakuAST::Regex::Literal.new('9'),
        )),
        'Ratchet quantifier will not backtrack';

    is "values: 1,2,3,4,stuff" ~~ rx(RakuAST::Regex::QuantifiedAtom.new(
            atom => RakuAST::Regex::CharClass::Digit.new,
            quantifier => RakuAST::Regex::Quantifier::OneOrMore.new,
            separator => RakuAST::Regex::Literal.new(','))),
        '1,2,3,4',
        'Separator works (non-trailing case)';
    is "values: 1,2,3,4,stuff" ~~ rx(RakuAST::Regex::QuantifiedAtom.new(
            atom => RakuAST::Regex::CharClass::Digit.new,
            quantifier => RakuAST::Regex::Quantifier::OneOrMore.new,
            separator => RakuAST::Regex::Literal.new(','),
            trailing-separator => True)),
        '1,2,3,4,',
        'Separator works (trailing case)';
    is "values: 1,2,33,4,stuff" ~~ rx(RakuAST::Regex::QuantifiedAtom.new(
            atom => RakuAST::Regex::CharClass::Digit.new,
            quantifier => RakuAST::Regex::Quantifier::OneOrMore.new,
            separator => RakuAST::Regex::Literal.new(','))),
        '1,2,3',
        'Separator must be between every quantified item';

    is "values: 1,2,33,400,stuff" ~~ rx(RakuAST::Regex::QuantifiedAtom.new(
            atom => RakuAST::Regex::Group.new(
                RakuAST::Regex::QuantifiedAtom.new(
                    atom => RakuAST::Regex::CharClass::Digit.new,
                    quantifier => RakuAST::Regex::Quantifier::OneOrMore.new
                )
            ),
            quantifier => RakuAST::Regex::Quantifier::OneOrMore.new,
            separator => RakuAST::Regex::Literal.new(','))),
        '1,2,33,400',
        'Regex groups compile correctly';

    is "1a2" ~~ rx(RakuAST::Regex::Assertion::Named.new(
            name => RakuAST::Name.from-identifier('alpha'),
            capturing => True
        )),
        'a',
        'Named assertion matches correctly';
    is-deeply $/.hash.keys, ('alpha',).Seq, 'Correct match keys';
    is $<alpha>, 'a', 'Correct match captured';

    is "1a2" ~~ rx(RakuAST::Regex::Assertion::Alias.new(
            name => 'foo',
            assertion => RakuAST::Regex::Assertion::Named.new(
                name => RakuAST::Name.from-identifier('alpha'),
                capturing => True
            )
        )),
        'a',
        'Named assertion with alias matches correctly';
    is-deeply $/.hash.keys.sort, ('alpha', 'foo').Seq, 'Correct match keys';
    is $<alpha>, 'a', 'Correct match captured (original name)';
    is $<foo>, 'a', 'Correct match captured (aliased name)';

    is "1a2" ~~ rx(RakuAST::Regex::Assertion::Named.new(
            name => RakuAST::Name.from-identifier('alpha'),
            capturing => False
        )),
        'a',
        'Non-capturing named assertion matches correctly';
    is-deeply $/.hash.keys, ().Seq, 'No match keys';

    is "1a2" ~~ rx(RakuAST::Regex::Assertion::Alias.new(
            name => 'foo',
            assertion => RakuAST::Regex::Assertion::Named.new(
                name => RakuAST::Name.from-identifier('alpha'),
                capturing => False
            )
        )),
        'a',
        'Non-capturing named assertion with alias matches correctly';
    is-deeply $/.hash.keys.sort, ('foo',).Seq, 'Correct match keys';
    is $<foo>, 'a', 'Correct match captured (aliased name)';
}
