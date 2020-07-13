# Base marker for all things that may appear as top-level regex syntax.
class RakuAST::Regex is RakuAST::Node {
    method IMPL-REGEX-TOP-LEVEL-QAST(RakuAST::IMPL::QASTContext $context, Mu $code-object, %mods) {
        # Compile the regex.
        my $regex-qast := self.IMPL-REGEX-QAST($context, %mods);

        # Store its captures and NFA.
        QRegex::P6Regex::Actions.store_regex_caps($code-object, nqp::null, nqp::hash());
        # TODO top-level NFA if applicable (e.g. if named rule)
        QRegex::P6Regex::Actions.alt_nfas($code-object, $regex-qast, $context.sc-handle);

        # Wrap in scan/pass
        QAST::Regex.new(
            :rxtype('concat'),
            QAST::Regex.new( :rxtype('scan') ),
            $regex-qast,
            QAST::Regex.new( :rxtype('pass') )
        )
    }
}

# Common role done by all branching regex constructs (alternations and conjunctions).
class RakuAST::Regex::Branching is RakuAST::Regex {
    has Mu $!branches;

    method new(*@branches) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Regex::Branching, '$!branches',
            @branches);
        $obj
    }

    method branches() {
        self.IMPL-WRAP-LIST($!branches)
    }

    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        my $qast := QAST::Regex.new(:rxtype(self.IMPL-QAST-REGEX-TYPE));
        for $!branches {
            $qast.push($_.IMPL-REGEX-QAST($context, %mods));
        }
        $qast
    }

    method visit-children(Code $visitor) {
        for $!branches {
            $visitor($_);
        }
    }
}

# Sequential alternation (||).
class RakuAST::Regex::SequentialAlternation is RakuAST::Regex::Branching {
    method IMPL-QAST-REGEX-TYPE() { 'altseq' }
}

# Sequential conjunction (&&).
class RakuAST::Regex::SequentialConjunction is RakuAST::Regex::Branching {
    method IMPL-QAST-REGEX-TYPE() { 'conjseq' }
}

# Alternation (|).
class RakuAST::Regex::Alternation is RakuAST::Regex::Branching {
    method IMPL-QAST-REGEX-TYPE() { 'alt' }
}

# Conjunction (&).
class RakuAST::Regex::Conjunction is RakuAST::Regex::Branching {
    method IMPL-QAST-REGEX-TYPE() { 'conj' }
}

# A sequence of terms to match, one after the other.
class RakuAST::Regex::Sequence is RakuAST::Regex {
    has Mu $!terms;

    method new(*@terms) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Regex::Sequence, '$!terms',
            @terms);
        $obj
    }

    method terms() {
        self.IMPL-WRAP-LIST($!terms)
    }

    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        my $concat := QAST::Regex.new(:rxtype<concat>);
        for $!terms {
            $concat.push($_.IMPL-REGEX-QAST($context, %mods));
        }
        $concat
    }

    method visit-children(Code $visitor) {
        for $!terms {
            $visitor($_);
        }
    }
}

# Marker for all regex terms.
class RakuAST::Regex::Term is RakuAST::Regex {
}

# Marker for all regex atoms.
class RakuAST::Regex::Atom is RakuAST::Regex::Term {
}

# A literal, unquoted, piece of text appearing in the regex.
class RakuAST::Regex::Literal is RakuAST::Regex::Atom {
    has str $.text;

    method new(str $text) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Regex::Literal, '$!text', $text);
        $obj
    }

    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        # TODO modifiers
        QAST::Regex.new( :rxtype<literal>, $!text )
    }
}

#| The base for all kinds of anchor.
class RakuAST::Regex::Anchor is RakuAST::Regex::Atom {
    method new() {
        nqp::create(self)
    }

    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        QAST::Regex.new( :rxtype<anchor>, :subtype(self.IMPL-QAST-SUBTYPE) )
    }
}

#| The beginning of string (^) anchor.
class RakuAST::Regex::Anchor::BeginningOfString is RakuAST::Regex::Anchor {
    method IMPL-QAST-SUBTYPE() { 'bos' }
}

#| The beginning of line (^^) anchor.
class RakuAST::Regex::Anchor::BeginningOfLine is RakuAST::Regex::Anchor {
    method IMPL-QAST-SUBTYPE() { 'bol' }
}

#| The end of string ($) anchor.
class RakuAST::Regex::Anchor::EndOfString is RakuAST::Regex::Anchor {
    method IMPL-QAST-SUBTYPE() { 'eos' }
}

#| The end of line (^^) anchor.
class RakuAST::Regex::Anchor::EndOfLine is RakuAST::Regex::Anchor {
    method IMPL-QAST-SUBTYPE() { 'eol' }
}

#| The left word boundary (<<) anchor.
class RakuAST::Regex::Anchor::LeftWordBoundary is RakuAST::Regex::Anchor {
    method IMPL-QAST-SUBTYPE() { 'lwb' }
}

#| The right word boundary (>>) anchor.
class RakuAST::Regex::Anchor::RightWordBoundary is RakuAST::Regex::Anchor {
    method IMPL-QAST-SUBTYPE() { 'rwb' }
}

# The base for all kinds of built-in character class. These include "." (match
# anything), \d (digit chars), and also things like \xCAFE because while they
# may in some senses be a literal, they are also possible to negate, in which
# case they imply a class of characters too.
class RakuAST::Regex::CharClass is RakuAST::Regex::Atom {
    method new() {
        nqp::create(self)
    }
}

# The character class matching anything (".").
class RakuAST::Regex::CharClass::Any is RakuAST::Regex::CharClass {
    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        QAST::Regex.new( :rxtype<cclass>, :name<.> )
    }
}

# The base for all negatable built-in character classes.
class RakuAST::Regex::CharClass::Negatable is RakuAST::Regex::CharClass {
    has Bool $.negated;

    method new(Bool :$negated) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Regex::CharClass::Negatable, '$!negated',
            $negated ?? True !! False);
        $obj
    }
}

# The digit character class (\d, \D).
class RakuAST::Regex::CharClass::Digit is RakuAST::Regex::CharClass::Negatable {
    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        QAST::Regex.new( :rxtype<cclass>, :name<d>, :negate(self.negated) )
    }
}

# The newline character class (\n, \N).
class RakuAST::Regex::CharClass::Newline is RakuAST::Regex::CharClass::Negatable {
    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        QAST::Regex.new( :rxtype<cclass>, :name<n>, :negate(self.negated) )
    }
}

# The space character class (\s, \S).
class RakuAST::Regex::CharClass::Space is RakuAST::Regex::CharClass::Negatable {
    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        QAST::Regex.new( :rxtype<cclass>, :name<s>, :negate(self.negated) )
    }
}

# The word character class (\w, \W).
class RakuAST::Regex::CharClass::Word is RakuAST::Regex::CharClass::Negatable {
    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        QAST::Regex.new( :rxtype<cclass>, :name<w>, :negate(self.negated) )
    }
}

# The base of all regex assertions (things of the form `<...>`, such as subrule
# calls, lookaheads, and user-defined character classes).
class RakuAST::Regex::Assertion is RakuAST::Regex::Atom {
}

# An assertion that always passes.
class RakuAST::Regex::Assertion::Pass is RakuAST::Regex::Assertion {
    method new() {
        nqp::create(self)
    }

    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        QAST::Regex.new( :rxtype<anchor>, :subtype<pass> )
    }
}

# An assertion that always fails.
class RakuAST::Regex::Assertion::Fail is RakuAST::Regex::Assertion {
    method new() {
        nqp::create(self)
    }

    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        QAST::Regex.new( :rxtype<anchor>, :subtype<fail> )
    }
}

# A named assertion, which may or may not capture. Models `<foo>` and
# `<.foo>`, and also `<foo::bar>`. Forms with arguments or taking a regex
# argument are modeled as subclasses of this.
class RakuAST::Regex::Assertion::Named is RakuAST::Regex::Assertion {
    has RakuAST::Name $.name;
    has Bool $.capturing;

    method new(RakuAST::Name :$name!, Bool :$capturing) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Regex::Assertion::Named, '$!name', $name);
        nqp::bindattr($obj, RakuAST::Regex::Assertion::Named, '$!capturing',
            $capturing ?? True !! False);
        $obj
    }

    method set-capturing(Bool $capturing) {
        nqp::bindattr(self, RakuAST::Regex::Assertion::Named, '$!capturing',
            $capturing ?? True !! False);
        Nil
    }

    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        my $longname := $!name;
        if $longname.is-identifier {
            my $name := $longname.canonicalize;
            if $name eq 'sym' {
                nqp::die('special <sym> name not yet compiled');
            }
            else {
                my $qast := QAST::Regex.new: :rxtype<subrule>,
                    QAST::NodeList.new(QAST::SVal.new( :value($name) ));
                if $!capturing {
                    $qast.subtype('capture');
                    $qast.name($name);
                }
                $qast
            }
        }
        else {
            nqp::die('non-identifier rule calls not yet compiled');
        }
    }

    method visit-children(Code $visitor) {
        $visitor($!name);
    }
}

# A named rule called with args.
class RakuAST::Regex::Assertion::Named::Args is RakuAST::Regex::Assertion::Named {
    has RakuAST::ArgList $.args;

    method new(RakuAST::Name :$name!, Bool :$capturing, Raku::ArgList :$args!) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Regex::Assertion::Named, '$!name', $name);
        nqp::bindattr($obj, RakuAST::Regex::Assertion::Named, '$!capturing',
            $capturing ?? True !! False);
        nqp::bindattr($obj, RakuAST::Regex::Assertion::Named::Args, '$!args', $args);
        $obj
    }

    method visit-children(Code $visitor) {
        $visitor(self.name);
        $visitor($!args);
    }
}

# A named rule called with a regex argument.
class RakuAST::Regex::Assertion::Named::RegexArg is RakuAST::Regex::Assertion::Named {
    has RakuAST::Regex $.regex-arg;

    method new(RakuAST::Name :$name!, Bool :$capturing, Raku::Regex :$regex-arg!) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Regex::Assertion::Named, '$!name', $name);
        nqp::bindattr($obj, RakuAST::Regex::Assertion::Named, '$!capturing',
            $capturing ?? True !! False);
        nqp::bindattr($obj, RakuAST::Regex::Assertion::Named::RegexArg,
            '$!regex-arg', $regex-arg);
        $obj
    }

    method visit-children(Code $visitor) {
        $visitor(self.name);
        $visitor($!regex-arg);
    }
}

# An alias assertion (where another assertion is given an extra name - or, in
# the case it's anonymous, perhaps just a name).
class RakuAST::Regex::Assertion::Alias is RakuAST::Regex::Assertion {
    has str $.name;
    has RakuAST::Regex::Assertion $.assertion;

    method new(str :$name!, RakuAST::Regex::Assertion :$assertion!) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Regex::Assertion::Alias, '$!name', $name);
        nqp::bindattr($obj, RakuAST::Regex::Assertion::Alias, '$!assertion', $assertion);
        $obj
    }

    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        my $qast := $!assertion.IMPL-REGEX-QAST($context, %mods);
        if $qast.rxtype eq 'subrule' {
            self.IMPL-SUBRULE-ALIAS($qast, $!name);
        }
        else {
            QAST::Regex.new( $qast, :name($!name), :rxtype<subcapture> );
        }
    }

    method IMPL-SUBRULE-ALIAS(Mu $qast, str $name) {
        if $qast.name gt '' {
            $qast.name($name ~ '=' ~ $qast.name);
        }
        else {
            $qast.name($name);
        }
        $qast.subtype('capture');
        $qast
    }

    method visit-children(Code $visitor) {
        $visitor($!assertion);
    }
}

# A lookahead assertion (where another assertion is evaluated as a
# zerowidth lookahead, either positive or negative).
class RakuAST::Regex::Assertion::Lookahead is RakuAST::Regex::Assertion {
    has Bool $.negated;
    has RakuAST::Regex::Assertion $.assertion;

    method new(Bool :$negated, RakuAST::Regex::Assertion :$assertion!) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Regex::Assertion::Lookahead, '$!negated',
            $negated ?? True !! False);
        nqp::bindattr($obj, RakuAST::Regex::Assertion::Lookahead, '$!assertion', $assertion);
        $obj
    }

    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        my $qast := $!assertion.IMPL-REGEX-QAST($context, %mods);
        $qast.subtype('zerowidth');
        if $!negated {
            $qast.negate(!$qast.negate);
        }
        $qast
    }

    method visit-children(Code $visitor) {
        $visitor($!assertion);
    }
}

# A quantified atom in a regex - that is, an atom with a quantifier and
# optional separator.
class RakuAST::Regex::QuantifiedAtom is RakuAST::Regex::Term {
    has RakuAST::Atom $.atom;
    has RakuAST::Quantifier $.quantifier;
    has RakuAST::Regex::Term $.separator;
    has Bool $.trailing-separator;

    method new(RakuAST::Atom :$atom!, RakuAST::Quantifier :$quantifier!,
               RakuAST::Separator :$separator, Bool :$trailing-separator) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Regex::QuantifiedAtom, '$!atom', $atom);
        nqp::bindattr($obj, RakuAST::Regex::QuantifiedAtom, '$!quantifier', $quantifier);
        nqp::bindattr($obj, RakuAST::Regex::QuantifiedAtom, '$!separator',
            $separator // RakuAST::Regex::Term);
        nqp::bindattr($obj, RakuAST::Regex::QuantifiedAtom, '$!trailing-separator',
            $trailing-separator ?? True !! False);
        $obj
    }

    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        my $atom := $!atom.IMPL-REGEX-QAST($context, %mods);
        my $quantified := $!quantifier.IMPL-QAST-QUANTIFY($context, $atom, %mods);
        if $!separator {
            $quantified.push($!separator.IMPL-REGEX-QAST($context, %mods));
            if $!trailing-separator {
                QAST::Regex.new(
                    :rxtype<concat>,
                    $quantified,
                    QAST::Regex.new(
                        :rxtype<quant>, :min(0), :max(1),
                        $!separator.IMPL-REGEX-QAST($context, %mods)
                    )
                )
            }
            else {
                $quantified
            }
        }
        else {
            $quantified
        }
    }

    method visit-children(Code $visitor) {
        $visitor($!atom);
        $visitor($!quantifier);
        $visitor($!separator) if $!separator;
    }
}

# The base of all regex quantifiers.
class RakuAST::Regex::Quantifier is RakuAST::Node {
    has RakuAST::Regex::Backtrack $.backtrack;

    method new(RakuAST::Regex::Backtrack :$backtrack) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Regex::Quantifier, '$!backtrack',
            nqp::istype($backtrack, RakuAST::Regex::Backtrack)
                ?? $backtrack
                !! RakuAST::Regex::Backtrack);
        $obj
    }

    method visit-children(Code $visitor) {
        $visitor($!backtrack) if $!backtrack;
    }
}

# The zero or one (?) quantifier. 
class RakuAST::Regex::Quantifier::ZeroOrOne is RakuAST::Regex::Quantifier {
    method IMPL-QAST-QUANTIFY(RakuAST::IMPL::QASTContext $context, Mu $atom-qast, %mods) {
        self.backtrack.IMPL-QAST-APPLY:
            QAST::Regex.new( :rxtype<quant>, :min(0), :max(1), $atom-qast ),
            %mods
    }
}

# The zero or more (*) quantifier. 
class RakuAST::Regex::Quantifier::ZeroOrMore is RakuAST::Regex::Quantifier {
    method IMPL-QAST-QUANTIFY(RakuAST::IMPL::QASTContext $context, Mu $atom-qast, %mods) {
        self.backtrack.IMPL-QAST-APPLY:
            QAST::Regex.new( :rxtype<quant>, :min(0), :max(-1), $atom-qast ),
            %mods
    }
}

# The one or more (+) quantifier. 
class RakuAST::Regex::Quantifier::OneOrMore is RakuAST::Regex::Quantifier {
    method IMPL-QAST-QUANTIFY(RakuAST::IMPL::QASTContext $context, Mu $atom-qast, %mods) {
        self.backtrack.IMPL-QAST-APPLY:
            QAST::Regex.new( :rxtype<quant>, :min(1), :max(-1), $atom-qast ),
            %mods
    }
}

# Backtracking modifiers.
class RakuAST::Regex::Backtrack is RakuAST::Node {
    method IMPL-QAST-APPLY(Mu $quant-qast, %mods) {
        $quant-qast.backtrack('r') if %mods<r>;
        $quant-qast
    }
}
class RakuAST::Regex::Backtrack::Greedy is RakuAST::Regex::Backtrack {
    method IMPL-QAST-APPLY(Mu $quant-qast, %mods) {
        $quant-qast.backtrack('g');
        $quant-qast
    }
}
class RakuAST::Regex::Backtrack::Frugal is RakuAST::Regex::Backtrack {
    method IMPL-QAST-APPLY(Mu $quant-qast, %mods) {
        $quant-qast.backtrack('f');
        $quant-qast
    }
}
class RakuAST::Regex::Backtrack::Ratchet is RakuAST::Regex::Backtrack {
    method IMPL-QAST-APPLY(Mu $quant-qast, %mods) {
        $quant-qast.backtrack('r');
        $quant-qast
    }
}
