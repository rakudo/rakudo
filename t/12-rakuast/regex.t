use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

plan 41;

sub rx(RakuAST::Regex $body) { EVAL RakuAST::QuotedRegex.new(:$body) }

my $ast;
sub ast(RakuAST::Regex:D $body --> Nil) {
    $ast := RakuAST::QuotedRegex.new(:$body);
    diag $ast.DEPARSE.chomp;
}

sub match-ok($haystack, $expected) is test-assertion {
  subtest "matches" => {
    plan 2;
    is $haystack ~~ EVAL($ast), $expected, 'EVAL over RakuAST';
    is $haystack ~~ EVAL($ast.DEPARSE), $expected, 'EVAL over deparsed AST';
  }
}

sub match-nok($haystack) is test-assertion {
  subtest "doesn't match" => {
    plan 2;
    nok $haystack ~~ EVAL($ast), 'EVAL over RakuAST';
    nok $haystack ~~ EVAL($ast.DEPARSE), 'EVAL over deparsed AST';
  }
}

subtest 'Simple literal regex' => {
    # / foo /
    ast RakuAST::Regex::Literal.new('foo');
    for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, $regex {
        is "foobarbaz"   ~~ $regex, 'foo', "$type: start of string";
        is "42foobarbaz" ~~ $regex, 'foo', "$type: middle of string";
        nok "barbaz"     ~~ $regex,        "$type: not matched";
    }
}

subtest 'Sequential alternation takes first match even if second is longer' => {
    # / b || bc /
    ast RakuAST::Regex::SequentialAlternation.new(
      RakuAST::Regex::Literal.new('b'),
      RakuAST::Regex::Literal.new('bc')
    );

    match-ok "abcd", "b";
}

subtest 'Sequential alternation takes second match if first fails' => {
    # / x || bc /
    ast RakuAST::Regex::SequentialAlternation.new(
      RakuAST::Regex::Literal.new('x'),
      RakuAST::Regex::Literal.new('bc')
    );

    match-ok "abcd", "bc";
}

subtest 'Sequential alternation fails if no alternative matches' => {
    # / x || y /
    ast RakuAST::Regex::SequentialAlternation.new(
      RakuAST::Regex::Literal.new('x'),
      RakuAST::Regex::Literal.new('y')
    );

    match-nok "abcd";
}

subtest 'LTM alternation takes longest match even if it is not first' => {
    # / b | bc /
    ast RakuAST::Regex::Alternation.new(
      RakuAST::Regex::Literal.new('b'),
      RakuAST::Regex::Literal.new('bc')
    );

    match-ok "abcd", "bc";
}

subtest 'Alternation takes second match if first fails' => {
    # / x | bc /
    ast RakuAST::Regex::Alternation.new(
      RakuAST::Regex::Literal.new('x'),
      RakuAST::Regex::Literal.new('bc')
    );

    match-ok "abcd", "bc";
}

subtest 'Alternation fails if no alternative matches' => {
    # / x | y /
    ast RakuAST::Regex::Alternation.new(
      RakuAST::Regex::Literal.new('x'),
      RakuAST::Regex::Literal.new('y')
    );

    match-nok "abcd";
}

subtest 'Conjunction matches when both items match' => {
    # / . && c /
    ast RakuAST::Regex::Conjunction.new(
      RakuAST::Regex::CharClass::Any.new,
      RakuAST::Regex::Literal.new('c')
    );

    match-ok "abcd", "c";
}

subtest 'Conjunction fails when one item does not match' => {
    # / . && x /
    ast RakuAST::Regex::Conjunction.new(
      RakuAST::Regex::CharClass::Any.new,
      RakuAST::Regex::Literal.new('x')
    );

    match-nok "abcd";
}

subtest 'Conjunction fails when items match different lengths' => {
    # / . && cd /
    ast RakuAST::Regex::Conjunction.new(
      RakuAST::Regex::CharClass::Any.new,
      RakuAST::Regex::Literal.new('cd')
    );

    match-nok "abcd";
}

subtest 'Sequence needs one thing to match after the other (pass case)' => {
    # / . d /
    ast RakuAST::Regex::Sequence.new(
      RakuAST::Regex::CharClass::Any.new,
      RakuAST::Regex::Literal.new('d')
    );

    match-ok "abcd", "cd";
}

subtest 'Sequence needs one thing to match after the other (failure case)' => {
    # / . a /
    ast RakuAST::Regex::Sequence.new(
      RakuAST::Regex::CharClass::Any.new,
      RakuAST::Regex::Literal.new('a')
    );

    match-nok "abcd";
}

subtest 'Beginning of string anchor works (pass case)' => {
    # / ^ . /
    ast RakuAST::Regex::Sequence.new(
      RakuAST::Regex::Anchor::BeginningOfString.new,
      RakuAST::Regex::CharClass::Any.new
    );

    match-ok "abcd", "a";
}

subtest 'Beginning of string anchor works (failure case)' => {
    # / ^ b /
    ast RakuAST::Regex::Sequence.new(
      RakuAST::Regex::Anchor::BeginningOfString.new,
      RakuAST::Regex::Literal.new('b')
    );

    match-nok "abcd";
}

subtest 'End of string anchor works (pass case)' => {
    # / . $ /
    ast RakuAST::Regex::Sequence.new(
      RakuAST::Regex::CharClass::Any.new,
      RakuAST::Regex::Anchor::EndOfString.new
    );

    match-ok "abcde", "e";
}

subtest 'End of string anchor works (failure case)' => {
    # / b $ /
    ast RakuAST::Regex::Sequence.new(
      RakuAST::Regex::Literal.new('b'),
      RakuAST::Regex::Anchor::EndOfString.new
    );

    match-nok "abcde";
}

subtest 'Right word boundary works' => {
    # / . e >> /
    ast RakuAST::Regex::Sequence.new(
      RakuAST::Regex::CharClass::Any.new,
      RakuAST::Regex::Literal.new('e'),
      RakuAST::Regex::Anchor::RightWordBoundary.new
    );

    for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, $regex {
        is "elizabeth the second" ~~ $regex, 'he', "$type: pass case";
        nok "elizabeth second" ~~ $regex, "$type: fail case";
    }
}

subtest 'Left word boundary works' => {
    # / << . t /
    ast RakuAST::Regex::Sequence.new(
      RakuAST::Regex::Anchor::LeftWordBoundary.new,
      RakuAST::Regex::CharClass::Any.new,
      RakuAST::Regex::Literal.new('t')
    );

    for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, $regex {
        is "cat ethics committee" ~~ $regex, 'et', "$type: pass case";
        nok "cat committee" ~~ $regex, "$type: fail case";
    }
}

subtest 'Quantified built-in character class matches' => {
    # / \d+ /
    ast RakuAST::Regex::QuantifiedAtom.new(
      atom => RakuAST::Regex::CharClass::Digit.new,
      quantifier => RakuAST::Regex::Quantifier::OneOrMore.new
    );

    match-ok "99cents", "99";
}

subtest 'Quantified negated built-in character class matches' => {
    # / \D+ /
    ast RakuAST::Regex::QuantifiedAtom.new(
      atom => RakuAST::Regex::CharClass::Digit.new(:negated),
      quantifier => RakuAST::Regex::Quantifier::OneOrMore.new
    );

    match-ok "99cents", "cents";
}

subtest 'Quantified built-in character class matches (frugal mode)' => {
    # / \d+? /
    ast RakuAST::Regex::QuantifiedAtom.new(
      atom => RakuAST::Regex::CharClass::Digit.new,
      quantifier => RakuAST::Regex::Quantifier::OneOrMore.new(
        backtrack => RakuAST::Regex::Backtrack::Frugal
      )
    );

    match-ok "99cents", "9";
}

subtest 'Quantified negated built-in character class matches (frugal mode)' => {
    # / \D+? /
    ast RakuAST::Regex::QuantifiedAtom.new(
      atom => RakuAST::Regex::CharClass::Digit.new(:negated),
      quantifier => RakuAST::Regex::Quantifier::OneOrMore.new(
        backtrack => RakuAST::Regex::Backtrack::Frugal
      )
    );

    match-ok "99cents", 'c';
}

subtest 'Greedy quantifier will backtrack' => {
    # / ^ \d+ 9 /
    ast RakuAST::Regex::Sequence.new(
      RakuAST::Regex::Anchor::BeginningOfString.new,
      RakuAST::Regex::QuantifiedAtom.new(
        atom => RakuAST::Regex::CharClass::Digit.new,
        quantifier => RakuAST::Regex::Quantifier::OneOrMore.new
      ),
      RakuAST::Regex::Literal.new('9')
    );

    match-ok "99cents", '99';
}

subtest 'Ratchet quantifier will not backtrack' => {
    # / ^ \d+: 9 /
    ast RakuAST::Regex::Sequence.new(
      RakuAST::Regex::Anchor::BeginningOfString.new,
      RakuAST::Regex::QuantifiedAtom.new(
        atom => RakuAST::Regex::CharClass::Digit.new,
        quantifier => RakuAST::Regex::Quantifier::OneOrMore.new(
          backtrack => RakuAST::Regex::Backtrack::Ratchet
        )
      ),
      RakuAST::Regex::Literal.new('9')
    );

    match-nok "99cents";
}

subtest 'Separator works (non-trailing case)' => {
    # / \d+ % ',' /
    ast RakuAST::Regex::QuantifiedAtom.new(
      atom => RakuAST::Regex::CharClass::Digit.new,
      quantifier => RakuAST::Regex::Quantifier::OneOrMore.new,
      separator => RakuAST::Regex::Literal.new(',')
    );

    match-ok "values: 1,2,3,4,stuff", '1,2,3,4';
}

subtest 'Separator works (trailing case)' => {
    # / \d+ %% ',' /
    ast RakuAST::Regex::QuantifiedAtom.new(
      atom => RakuAST::Regex::CharClass::Digit.new,
      quantifier => RakuAST::Regex::Quantifier::OneOrMore.new,
      separator => RakuAST::Regex::Literal.new(','),
      trailing-separator => True
    );

    match-ok "values: 1,2,3,4,stuff", '1,2,3,4,';
}

subtest 'Separator must be between every quantified item' => {
    # / \d+ % ',' /
    ast RakuAST::Regex::QuantifiedAtom.new(
      atom => RakuAST::Regex::CharClass::Digit.new,
      quantifier => RakuAST::Regex::Quantifier::OneOrMore.new,
      separator => RakuAST::Regex::Literal.new(',')
    );

    match-ok "values: 1,2,33,4,stuff", '1,2,3';
}

subtest 'Regex groups compile correctly' => {
    # / [\d+]+ % ',' /
    ast RakuAST::Regex::QuantifiedAtom.new(
      atom => RakuAST::Regex::Group.new(
        RakuAST::Regex::QuantifiedAtom.new(
          atom => RakuAST::Regex::CharClass::Digit.new,
          quantifier => RakuAST::Regex::Quantifier::OneOrMore.new
        )
      ),
      quantifier => RakuAST::Regex::Quantifier::OneOrMore.new,
      separator => RakuAST::Regex::Literal.new(',')
    );

    for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, $regex {
        is "values: 1,2,33,400,stuff" ~~ $regex, '1,2,33,400',
          "$type: did we get correct match";
        nok $/.list.keys,
          "$type: no positional captures from non-capturing group";
        nok $/.hash.keys,
          "$type: no named captures from non-capturing group";
    }
}

subtest 'Named assertion matches correctly' => {
    ast RakuAST::Regex::Assertion::Named.new(
      name      => RakuAST::Name.from-identifier('alpha'),
      capturing => True
    );

    for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, $regex {
        is "1a2" ~~ $regex, 'a',
          "$type: did we get correct match";
        is-deeply $/.hash.keys, ('alpha',).Seq,
          "$type: correct match keys";
        is $<alpha>, 'a',
          "$type: correct match captured";
    }
}

subtest 'Non-capturing named assertion matches correctly' => {
    ast RakuAST::Regex::Assertion::Named.new(
      name      => RakuAST::Name.from-identifier('alpha'),
      capturing => False
    );

    for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, $regex {
        is "1a2" ~~ $regex, 'a',
          "$type: did we get correct match";
        is-deeply $/.hash.keys, ().Seq,
          "$type: no match keys";
    }
}

subtest 'Named assertion with alias matches correctly' => {
    # / <foo=alpha> /
    ast RakuAST::Regex::Assertion::Alias.new(
      name => 'foo',
      assertion => RakuAST::Regex::Assertion::Named.new(
        name      => RakuAST::Name.from-identifier('alpha'),
        capturing => True
      )
     );

    for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, $regex {
        is "1a2" ~~ $regex, 'a',
          "$type: did we get correct match";
        is-deeply $/.hash.keys.sort, ('alpha','foo').Seq,
          "$type: correct match keys";
        is $<alpha>, 'a',
          "$type: correct match captured (original name)";
        is $<foo>, 'a',
          "$type: correct match captured (aliased name)";
    }
}

subtest 'Non-capturing named assertion with alias matches correctly' => {
    # / <.foo=alpha> /
    ast RakuAST::Regex::Assertion::Alias.new(
      name => 'foo',
      assertion => RakuAST::Regex::Assertion::Named.new(
        name      => RakuAST::Name.from-identifier('alpha'),
        capturing => False
      )
     );

    for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, $regex {
        is "1a2" ~~ $regex, 'a',
          "$type: did we get correct match";
        is-deeply $/.hash.keys, ('foo',).Seq,
          "$type: correct match keys";
        is $<foo>, 'a',
          "$type: correct match captured (aliased name)";
    }
}

subtest 'Regex with two positional capturing groups matches correctly' => {
    # / (\w) \d (\w) /
    ast RakuAST::Regex::Sequence.new(
      RakuAST::Regex::CapturingGroup.new(
        RakuAST::Regex::CharClass::Word.new
      ),
      RakuAST::Regex::CharClass::Digit.new,
      RakuAST::Regex::CapturingGroup.new(
        RakuAST::Regex::CharClass::Word.new
      )
    );

    for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, $regex {
        is "2a1b" ~~ $regex, 'a1b',
          "$type: did we get correct match";
        is $/.list.elems, 2,
          "$type: Two positional captures";
        is $0, 'a',
          "$type: First positional capture is correct";
        is $1, 'b',
          "$type: Second positional capture is correct";
        nok $/.hash,
          "$type: No named captures";
    }
}

subtest 'Lookahead assertion with named rule works' => {
    # / <?alpha> \w /
    ast RakuAST::Regex::Sequence.new(
      RakuAST::Regex::Assertion::Lookahead.new(
        assertion => RakuAST::Regex::Assertion::Named.new(
          name => RakuAST::Name.from-identifier('alpha'),
          capturing => True
        )
      ),
      RakuAST::Regex::CharClass::Word.new
    );

    for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, $regex {
        is "!2a" ~~ $regex, 'a',
          "$type: did we get correct match";
        is $/.list.elems, 0,
          "$type: No positional captures";
        is $/.hash.elems, 0,
          "$type: No named captures";
    }
}

subtest 'Negated lookahead assertion with named rule works' => {
    # / <!alpha> \w /
    ast RakuAST::Regex::Sequence.new(
      RakuAST::Regex::Assertion::Lookahead.new(
        negated   => True,
        assertion => RakuAST::Regex::Assertion::Named.new(
          name => RakuAST::Name.from-identifier('alpha'),
          capturing => True
        )
      ),
      RakuAST::Regex::CharClass::Word.new
    );

    for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, $regex {
        is "!2a" ~~ $regex, '2',
          "$type: did we get correct match";
        is $/.list.elems, 0,
          "$type: No positional captures";
        is $/.hash.elems, 0,
          "$type: No named captures";
    }
}

subtest 'Lookahead assertion calling before with a regex arg works' => {
    # / <?before \d> \w /
    ast RakuAST::Regex::Sequence.new(
      RakuAST::Regex::Assertion::Lookahead.new(
        assertion => RakuAST::Regex::Assertion::Named::RegexArg.new(
          name => RakuAST::Name.from-identifier('before'),
          regex-arg => RakuAST::Regex::CharClass::Digit.new,
        )
      ),
      RakuAST::Regex::CharClass::Word.new
    );

    for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, $regex {
        is "!2a" ~~ $regex, '2',
          "$type: did we get correct match";
        is $/.list.elems, 0,
          "$type: No positional captures";
        is $/.hash.elems, 0,
          "$type: No named captures";
    }
}

subtest 'Negated lookahead assertion calling before with a regex arg works' => {
    # / <!before \d> \w /
    ast RakuAST::Regex::Sequence.new(
      RakuAST::Regex::Assertion::Lookahead.new(
        negated   => True,
        assertion => RakuAST::Regex::Assertion::Named::RegexArg.new(
          name => RakuAST::Name.from-identifier('before'),
          regex-arg => RakuAST::Regex::CharClass::Digit.new,
        )
      ),
      RakuAST::Regex::CharClass::Word.new
    );

    for 'AST', EVAL($ast), 'DEPARSE', EVAL($ast.DEPARSE) -> $type, $regex {
        is "!2a" ~~ $regex, 'a',
          "$type: did we get correct match";
        is $/.list.elems, 0,
          "$type: No positional captures";
        is $/.hash.elems, 0,
          "$type: No named captures";
    }
}

subtest 'Match from and match to markers works' => {
    # / b <( \d )> c /
    ast RakuAST::Regex::Sequence.new(
      RakuAST::Regex::Literal.new('b'),
      RakuAST::Regex::MatchFrom.new,
      RakuAST::Regex::CharClass::Digit.new,
      RakuAST::Regex::MatchTo.new,
      RakuAST::Regex::Literal.new('c')
    );

    match-ok "a1b2c", '2';
}

subtest 'Match involving a quoted string literal works' => {
    # / lie /
    ast RakuAST::Regex::Quote.new(
      RakuAST::QuotedString.new(
       :segments[RakuAST::StrLiteral.new('lie')]
      )
    );

    match-ok "believe", 'lie';
}

subtest 'Match involving a quoted string with interpolation works' => {
    my $end = 've';

    # / e$end /
    ast RakuAST::Regex::Quote.new(
      RakuAST::QuotedString.new(
        :segments[
          RakuAST::StrLiteral.new('e'),
          RakuAST::Var::Lexical.new('$end')
        ]
      )
    );

    is "believe" ~~ EVAL($ast), 'eve', 'EVAL over RakuAST';
    is "believe" ~~ EVAL($ast.DEPARSE), 'eve', 'EVAL over deparsed AST';
}

subtest 'Match involving quote words works' => {
    # / <{ qqw/link inky linky/ }> /
    ast RakuAST::Regex::Quote.new(
      RakuAST::QuotedString.new(
        :segments[RakuAST::StrLiteral.new('link inky linky')],
        :processors['words']
      )
    );

    match-ok "slinky sprint", 'linky';
}

# vim: expandtab shiftwidth=4
