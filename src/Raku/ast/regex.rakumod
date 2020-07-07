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

# Conjunction (|).
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
