# Base marker for all things that may appear as top-level regex syntax.
class RakuAST::Regex is RakuAST::Node {
    method IMPL-REGEX-TOP-LEVEL-QAST(RakuAST::IMPL::QASTContext $context, Mu $code-object, %mods) {
        # Compile the regex.
        my $regex-qast := self.IMPL-REGEX-QAST($context, %mods);

        # Store its captures and NFA.
        # TODO
        QRegex::P6Regex::Actions.store_regex_caps($code-object, nqp::null, nqp::hash());

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
            $qast.push($_.IMPL-REGEX-QAST($context, $_));
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
            $concat.push($_.IMPL-REGEX-QAST($context, $_));
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
