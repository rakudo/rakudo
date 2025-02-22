# Base marker for all things that may appear as top-level regex syntax.
class RakuAST::Regex
  is RakuAST::Node
{
    method IMPL-REGEX-TOP-LEVEL-QAST(
      RakuAST::IMPL::QASTContext  $context,
                              Mu  $code-object,
                                  %mods,
                             int :$no-scan,
                              Mu :$body-qast,
                             str :$name
    ) {
        # Compile the regex.
        my $regex-qast := $body-qast // self.IMPL-REGEX-QAST($context, %mods);

        # Store its captures and NFA.
        $code-object.SET_CAPS(QRegex::P6Regex::Actions.capnames($regex-qast, 0));
        # TODO top-level NFA if applicable (e.g. if named rule)
        QRegex::P6Regex::Actions.store_regex_caps($code-object, NQPMu, QRegex::P6Regex::Actions.capnames($regex-qast, 0));
        QRegex::P6Regex::Actions.store_regex_nfa($code-object, NQPMu, QRegex::NFA.new.addnode($regex-qast));
        QRegex::P6Regex::Actions.alt_nfas($code-object, $regex-qast, $context.sc-handle);

        # Wrap in scan/pass as appropriate.
        my $wrap-qast := QAST::Regex.new(
            :rxtype('concat'),
            $regex-qast,
            ($name
              ?? QAST::Regex.new(:rxtype('pass'), :$name)
              !! QAST::Regex.new(:rxtype('pass'))
            )
        );
        unless $no-scan {
            $wrap-qast.unshift(QAST::Regex.new( :rxtype('scan') ));
        }
        $wrap-qast
    }

    method IMPL-REGEX-BLOCK-CALL(RakuAST::IMPL::QASTContext $context, RakuAST::Block $block) {
        QAST::Stmts.new(
            QAST::Op.new(
                :op('p6store'),
                QAST::Var.new( :name('$/'), :scope<lexical> ),
                QAST::Op.new(
                    QAST::Var.new( :name('$¢'), :scope<lexical> ),
                    :name('MATCH'),
                    :op('callmethod')
                )
            ),
            $block.IMPL-TO-QAST($context, :immediate)
        )
    }

    method IMPL-APPLY-LITERAL-MODS(Mu $qast, %mods) {
        if %mods<i> && %mods<m> {
            $qast.subtype('ignorecase+ignoremark')
        }
        elsif %mods<i> {
            $qast.subtype('ignorecase')
        }
        elsif %mods<m> {
            $qast.subtype('ignoremark')
        }
        $qast
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
}

# Common role done by all branching regex constructs (alternations and conjunctions).
class RakuAST::Regex::Branching
  is RakuAST::Regex
{
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
            my $branch-qast := $_.IMPL-REGEX-QAST($context, %mods);
            $branch-qast.backtrack('r') if %mods<r> && !$branch-qast.backtrack;
            $qast.push($branch-qast);
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
class RakuAST::Regex::SequentialAlternation
  is RakuAST::Regex::Branching
{
    method IMPL-QAST-REGEX-TYPE() { 'altseq' }
}

# Sequential conjunction (&&).
class RakuAST::Regex::SequentialConjunction
  is RakuAST::Regex::Branching
{
    method IMPL-QAST-REGEX-TYPE() { 'conjseq' }
}

# Alternation (|).
class RakuAST::Regex::Alternation
  is RakuAST::Regex::Branching
{
    method IMPL-QAST-REGEX-TYPE() { 'alt' }
}

# Conjunction (&).
class RakuAST::Regex::Conjunction
  is RakuAST::Regex::Branching
{
    method IMPL-QAST-REGEX-TYPE() { 'conj' }
}

# A sequence of terms to match, one after the other.
class RakuAST::Regex::Sequence
  is RakuAST::Regex
{
    has Mu $!terms;

    method new(*@atoms) {
        my $obj := nqp::create(self);

        my @terms;
        my @literals;

        my sub handle-literals($with-whitespace) {
            my $literal := RakuAST::Regex::Literal.new(nqp::join('',@literals));
            @terms.push($with-whitespace
              ?? RakuAST::Regex::WithWhitespace.new($literal)
              !! $literal
            );
            nqp::setelems(@literals, 0);
        }

        for @atoms {
            if nqp::istype($_, RakuAST::Regex::Literal) {
                @literals.push($_.text);
            }
            elsif nqp::istype($_, RakuAST::Regex::WithWhitespace) {
                my $regex := $_.regex;
                if nqp::istype($regex, RakuAST::Regex::Literal) && @literals {
                    @literals.push($regex.text);
                    handle-literals(True);
                }
                else {
                    handle-literals(False) if @literals;
                    @terms.push($_);
                }
            }
            else {
                handle-literals(False) if @literals;
                @terms.push($_);
            }
        }
        handle-literals(False) if @literals;

        nqp::bindattr($obj, RakuAST::Regex::Sequence, '$!terms', @terms);
        $obj
    }

    method terms() {
        self.IMPL-WRAP-LIST($!terms)
    }

    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        my str $collect-literals := '';
        my @terms;
        for $!terms {
            if nqp::istype($_, RakuAST::Regex::Literal) {
                $collect-literals := $collect-literals ~ $_.text;
            }
            else {
                if $collect-literals ne '' {
                    @terms.push(self.IMPL-LIT($collect-literals, %mods));
                    $collect-literals := '';
                }
                my $qast := $_.IMPL-REGEX-QAST($context, %mods);
                if $qast {
                    $qast.backtrack('r') if %mods<r> && !$qast.backtrack;
                    @terms.push($qast);
                }
            }
        }
        if $collect-literals ne '' {
            @terms.push(self.IMPL-LIT($collect-literals, %mods));
        }

        # One more round of literal collection after we generated QAST as
        # not only RakuAST::Regex::Literal nodes can create literals
        my $concat := QAST::Regex.new(:rxtype<concat>);
        my $last-term;
        for @terms {
            if nqp::istype($_, QAST::Regex) && $_.rxtype eq 'literal' && !nqp::istype($_[0], QAST::Node)
                && $last-term && nqp::istype($last-term, QAST::Regex) && $last-term.rxtype eq 'literal' && !nqp::istype($last-term[0], QAST::Node)
                && $_.subtype eq $last-term.subtype
            {
                $last-term[0] := $last-term[0] ~ $_[0];
            }
            else {
                $last-term := $_;
                $concat.push($_);
            }
        }
        $concat
    }

    method IMPL-LIT(str $text, %mods) {
        self.IMPL-APPLY-LITERAL-MODS:
            QAST::Regex.new( :rxtype<literal>, $text ),
            %mods
    }

    method visit-children(Code $visitor) {
        for $!terms {
            $visitor($_);
        }
    }
}

# Marker for all regex terms.
class RakuAST::Regex::Term
  is RakuAST::Regex
{
    method whitespace-wrappable() { True }
}

# Marker for all regex atoms.
class RakuAST::Regex::Atom
  is RakuAST::Regex::Term
{
    method quantifiable() { True }
}

# A literal, unquoted, piece of text appearing in the regex.
class RakuAST::Regex::Literal
  is RakuAST::Regex::Atom
{
    has str $.text;

    method new(str $text) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Regex::Literal, '$!text', $text);
        $obj
    }

    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        self.IMPL-APPLY-LITERAL-MODS:
            QAST::Regex.new( :rxtype<literal>, $!text ),
            %mods
    }
}

# A quoted string appearing in the regex. Covers both standard single/double
# quotes which compile into a literal match of the evaluated string, or
# quote words, which compile into an LTM alternation of literals.
class RakuAST::Regex::Quote
  is RakuAST::Regex::Atom
{
    has RakuAST::QuotedString $.quoted;

    method new(RakuAST::QuotedString $quoted) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Regex::Quote, '$!quoted', $quoted);
        $obj
    }

    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        my $literal-value := $!quoted.literal-value;
        if nqp::isconcrete($literal-value) {
            if nqp::istype($literal-value, Str) {
                # Really simple string; just match it.
                self.IMPL-APPLY-LITERAL-MODS:
                    QAST::Regex.new( :rxtype<literal>, $literal-value ),
                    %mods
            }
            elsif nqp::istype($literal-value, List) {
                # Quote words alternation.
                my $alt := QAST::Regex.new( :rxtype<alt> );
                for self.IMPL-UNWRAP-LIST($literal-value) {
                    $alt.push: self.IMPL-APPLY-LITERAL-MODS:
                        QAST::Regex.new( :rxtype<literal>, $_ ),
                        %mods
                }
                $alt
            }
            else {
                nqp::die('Unexpected quoted string literal value type in regex quote; got ' ~
                    $literal-value.HOW.name($literal-value));
            }
        }
        else {
            if self.IMPL-UNWRAP-LIST($!quoted.processors) {
                # Somehow, overly complex quote words construct. Weird.
                nqp::die('Unsupported quoted string literal in regex quote');
            }
            else {
                # Complex string that needs interpolation.
                QAST::Regex.new(
                    :rxtype<subrule>, :subtype<method>,
                    QAST::NodeList.new(
                        QAST::SVal.new( :value('!LITERAL') ),
                        $!quoted.IMPL-TO-QAST($context),
                        QAST::IVal.new( :value(%mods<i> ?? 1 !! 0) )
                    )
                )
            }
        }
    }

    method visit-children(Code $visitor) {
        $visitor($!quoted);
    }
}

# A (non-capturing) regex group, from the [...] syntax.
class RakuAST::Regex::Group
  is RakuAST::Regex::Atom
{
    has RakuAST::Regex $.regex;

    method new(RakuAST::Regex $regex) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Regex::Group, '$!regex', $regex);
        $obj
    }

    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        $!regex.IMPL-REGEX-QAST($context, nqp::clone(%mods))
    }

    method visit-children(Code $visitor) {
        $visitor($!regex);
    }
}

class RakuAST::Regex::Nested
  is RakuAST::Regex::Atom
{
    has RakuAST::Regex $.goal;
    has RakuAST::Regex $.expr;

    method new(RakuAST::Regex $goal, RakuAST::Regex $expr) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Regex::Nested, '$!goal', $goal);
        nqp::bindattr($obj, RakuAST::Regex::Nested, '$!expr', $expr);
        $obj
    }

    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        QAST::Regex.new(
            :rxtype<goal>,
            $!goal.IMPL-REGEX-QAST($context, nqp::clone(%mods)),
            $!expr.IMPL-REGEX-QAST($context, nqp::clone(%mods)),
            QAST::Regex.new(
                :rxtype<subrule>, :subtype<method>,
                QAST::NodeList.new(
                    QAST::SVal.new( :value('FAILGOAL') ),
                    QAST::SVal.new( :value($!goal.DEPARSE) ),
                    ) ) ); #TODO: |@dba) ) );
    }

    method visit-children(Code $visitor) {
        $visitor($!goal);
        $visitor($!expr);
    }
}

# A (positional, at least by default) capturing regex group, from the (...) syntax.
class RakuAST::Regex::CapturingGroup
  is RakuAST::Regex::Atom
  is RakuAST::RegexThunk
  is RakuAST::ImplicitDeclarations
{
    has RakuAST::Regex $.regex;

    # Used as part of QAST compilation.
    has str $!unique-name;
    has Mu $!body-qast;

    method new(RakuAST::Regex $regex) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Regex::CapturingGroup, '$!regex', $regex);
        $obj
    }

    method PRODUCE-IMPLICIT-DECLARATIONS() {
        self.IMPL-WRAP-LIST([
            RakuAST::VarDeclaration::Implicit::RegexCapture.new,
        ])
    }

    method IMPL-THUNKED-REGEX-QAST(RakuAST::IMPL::QASTContext $context) {
        $!regex.IMPL-REGEX-TOP-LEVEL-QAST($context, self.meta-object, nqp::hash(),
            :body-qast($!body-qast // nqp::die('Misordered regex compilation')),
            :no-scan);
    }

    method IMPL-QAST-DECL-CODE(RakuAST::IMPL::QASTContext $context) {
        # Form the block itself and link it with the meta-object. Install it
        # in the lexpad; we'll look it up when we need it. This means we can
        # avoid closure-cloning it per time we enter it, for example if it is
        # quantified.
        my $block := self.IMPL-QAST-FORM-BLOCK($context, :blocktype('declaration_static'));
        self.IMPL-LINK-META-OBJECT($context, $block);
        QAST::Stmts.new(
            $block,
            self.get-implicit-declarations().AT-POS(0).IMPL-BIND-QAST($context,
                self.IMPL-CLOSURE-QAST($context) ),
        )
    }

    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        my $body-qast := $!regex.IMPL-REGEX-QAST($context, nqp::clone(%mods));
        nqp::bindattr(self, RakuAST::Regex::CapturingGroup, '$!body-qast', $body-qast);
        QAST::Regex.new(
            :rxtype('subrule'), :subtype('capture'),
            QAST::NodeList.new(self.get-implicit-declarations().AT-POS(0).IMPL-LOOKUP-QAST($context)),
            $body-qast
        )
    }

    method visit-children(Code $visitor) {
        $visitor($!regex);
    }
}

# A named capture, of the form $<name>=quantified-atom.
class RakuAST::Regex::NamedCapture
  is RakuAST::Regex::Atom
{
    has str $.name;
    has Bool $.array;
    has RakuAST::Term $.regex;

    method new(str :$name!, Bool :$array, RakuAST::Term :$regex!) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Regex::NamedCapture, '$!name', $name);
        nqp::bindattr($obj, RakuAST::Regex::NamedCapture, '$!array',
            $array ?? True !! False);
        nqp::bindattr($obj, RakuAST::Regex::NamedCapture, '$!regex', $regex);
        $obj
    }

    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        my $qast := $!regex.IMPL-REGEX-QAST($context, %mods);
        if ($qast.rxtype eq 'quant' || $qast.rxtype eq 'dynquant') &&
                $qast[0].rxtype eq 'subrule' {
            self.IMPL-SUBRULE-ALIAS($qast[0], $!name);
        }
        elsif $qast.rxtype eq 'subrule' {
            self.IMPL-SUBRULE-ALIAS($qast, $!name);
            $qast := QAST::Regex.new( :rxtype<quant>, :min(1), :max(1), $qast) if $!array;
        }
        else {
            $qast := QAST::Regex.new( :rxtype<subcapture>, :name($!name), $qast );
        }
        $qast
    }

    method visit-children(Code $visitor) {
        $visitor($!regex);
    }
}

#| The base for all kinds of anchor.
class RakuAST::Regex::Anchor
  is RakuAST::Regex::Atom
{
    method new() {
        nqp::create(self)
    }

    method quantifiable() { False }

    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        QAST::Regex.new( :rxtype<anchor>, :subtype(self.IMPL-QAST-SUBTYPE) )
    }
}

#| The beginning of string (^) anchor.
class RakuAST::Regex::Anchor::BeginningOfString
  is RakuAST::Regex::Anchor
{
    method IMPL-QAST-SUBTYPE() { 'bos' }
}

#| The beginning of line (^^) anchor.
class RakuAST::Regex::Anchor::BeginningOfLine
  is RakuAST::Regex::Anchor
{
    method IMPL-QAST-SUBTYPE() { 'bol' }
}

#| The end of string ($) anchor.
class RakuAST::Regex::Anchor::EndOfString
  is RakuAST::Regex::Anchor
{
    method IMPL-QAST-SUBTYPE() { 'eos' }
}

#| The end of line (^^) anchor.
class RakuAST::Regex::Anchor::EndOfLine
  is RakuAST::Regex::Anchor
{
    method IMPL-QAST-SUBTYPE() { 'eol' }
}

#| The left word boundary (<<) anchor.
class RakuAST::Regex::Anchor::LeftWordBoundary
  is RakuAST::Regex::Anchor
{
    method IMPL-QAST-SUBTYPE() { 'lwb' }
}

#| The right word boundary (>>) anchor.
class RakuAST::Regex::Anchor::RightWordBoundary
  is RakuAST::Regex::Anchor
{
    method IMPL-QAST-SUBTYPE() { 'rwb' }
}

# The start of match marker.
class RakuAST::Regex::MatchFrom
  is RakuAST::Regex::Atom
{
    method new() {
        nqp::create(self)
    }

    method quantifiable() { False }

    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        QAST::Regex.new( :rxtype<subrule>, :subtype<capture>,
            :backtrack<r>, :name('$!from'),
            QAST::NodeList.new(
                QAST::SVal.new( :value('!LITERAL') ),
                QAST::SVal.new( :value('') ) ) );
    }
}

# The end of match marker.
class RakuAST::Regex::MatchTo
  is RakuAST::Regex::Atom
{
    method new() {
        nqp::create(self)
    }

    method quantifiable() { False }

    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        QAST::Regex.new( :rxtype<subrule>, :subtype<capture>,
            :backtrack<r>, :name('$!to'),
            QAST::NodeList.new(
                QAST::SVal.new( :value('!LITERAL') ),
                QAST::SVal.new( :value('') ) ) );
    }
}

# The base for all kinds of built-in character class. These include "." (match
# anything), \d (digit chars), and also things like \xCAFE because while they
# may in some senses be a literal, they are also possible to negate, in which
# case they imply a class of characters too.
class RakuAST::Regex::CharClass
  is RakuAST::Regex::Atom
{
    method new() {
        nqp::create(self)
    }
}

# The character class matching anything (".").
class RakuAST::Regex::CharClass::Any
  is RakuAST::Regex::CharClass
{
    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        QAST::Regex.new( :rxtype<cclass>, :name<.> )
    }
}

# The base for all negatable built-in character classes.
class RakuAST::Regex::CharClass::Negatable
  is RakuAST::Regex::CharClass
{
    has Bool $.negated;

    method new(Bool :$negated) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Regex::CharClass::Negatable, '$!negated',
            $negated ?? True !! False);
        $obj
    }
}

# Done by everything that can appear inside of a user-defined character class
# enumeration (that is, `<[this]>`).
class RakuAST::Regex::CharClassEnumerationElement
  is RakuAST::Node
{
    method IMPL-CCLASS-ENUM-CHARS(%mods) { '' }

    method IMPL-CCLASS-ENUM-QAST(RakuAST::IMPL::QASTContext $context, %mods, Bool $negate) {
        nqp::die("Missing IMPL-CCLASS-ENUM-QAST implementation on " ~ self.HOW.name(self));
    }

    method IMPL-MAYBE-NEGATE(Mu $qast, Bool $negate) {
        $qast.negate(!$qast.negate) if $negate;
        $qast
    }

    # Returns a single-character string if this can potentially serve as a
    # range endpoint, or Nil if not. Does not check if it is a synthetic
    # character.
    method range-endpoint() {
        my str $chars := self.IMPL-CCLASS-ENUM-CHARS({});
        nqp::chars($chars) == 1 ?? $chars !! Nil
    }
}

# The backspace character class (\b, \B). In Raku syntax, this may only appear
# in a character class enumeration.
class RakuAST::Regex::CharClass::BackSpace
  is RakuAST::Regex::CharClass::Negatable
  is RakuAST::Regex::CharClassEnumerationElement
{
    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        QAST::Regex.new:
            :rxtype('enumcharlist'), :negate(self.negated), "\b"
    }

    method IMPL-CCLASS-ENUM-CHARS(%mods) { self.negated ?? "" !! "\b" }

    method IMPL-CCLASS-ENUM-QAST(RakuAST::IMPL::QASTContext $context, %mods, Bool $negate) {
        self.IMPL-MAYBE-NEGATE(self.IMPL-REGEX-QAST($context, %mods), $negate)
    }
}

# The digit character class (\d, \D).
class RakuAST::Regex::CharClass::Digit
  is RakuAST::Regex::CharClass::Negatable
  is RakuAST::Regex::CharClassEnumerationElement
{
    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        QAST::Regex.new( :rxtype<cclass>, :name<d>, :negate(self.negated) )
    }

    method IMPL-CCLASS-ENUM-QAST(RakuAST::IMPL::QASTContext $context, %mods, Bool $negate) {
        self.IMPL-MAYBE-NEGATE(self.IMPL-REGEX-QAST($context, %mods), $negate)
    }
}

# The escape character class (\e, \E)
class RakuAST::Regex::CharClass::Escape
  is RakuAST::Regex::CharClass::Negatable
  is RakuAST::Regex::CharClassEnumerationElement
{
    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        QAST::Regex.new:
            :rxtype('enumcharlist'), :negate(self.negated), "\c[27]"
    }

    method IMPL-CCLASS-ENUM-CHARS(%mods) { self.negated ?? "" !! "\c[27]" }

    method IMPL-CCLASS-ENUM-QAST(RakuAST::IMPL::QASTContext $context, %mods, Bool $negate) {
        self.IMPL-MAYBE-NEGATE(self.IMPL-REGEX-QAST($context, %mods), $negate)
    }
}

# The form feed character class (\f, \F)
class RakuAST::Regex::CharClass::FormFeed
  is RakuAST::Regex::CharClass::Negatable
  is RakuAST::Regex::CharClassEnumerationElement
{
    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        QAST::Regex.new:
            :rxtype('enumcharlist'), :negate(self.negated), "\c[12]"
    }

    method IMPL-CCLASS-ENUM-CHARS(%mods) { self.negated ?? "" !! "\c[12]" }

    method IMPL-CCLASS-ENUM-QAST(RakuAST::IMPL::QASTContext $context, %mods, Bool $negate) {
        self.IMPL-MAYBE-NEGATE(self.IMPL-REGEX-QAST($context, %mods), $negate)
    }
}

# The horizontal whitespace character class (\h, \H)
class RakuAST::Regex::CharClass::HorizontalSpace
  is RakuAST::Regex::CharClass::Negatable
  is RakuAST::Regex::CharClassEnumerationElement
{
    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        QAST::Regex.new:
            :rxtype('enumcharlist'), :negate(self.negated),
            "\x[09,20,a0,1680,180e,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,200a,202f,205f,3000]"
    }

    method IMPL-CCLASS-ENUM-QAST(RakuAST::IMPL::QASTContext $context, %mods, Bool $negate) {
        self.IMPL-MAYBE-NEGATE(self.IMPL-REGEX-QAST($context, %mods), $negate)
    }
}

# The newline character class (\n, \N).
class RakuAST::Regex::CharClass::Newline
  is RakuAST::Regex::CharClass::Negatable
  is RakuAST::Regex::CharClassEnumerationElement
{
    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        QAST::Regex.new( :rxtype<cclass>, :name<n>, :negate(self.negated) )
    }

    method IMPL-CCLASS-ENUM-QAST(RakuAST::IMPL::QASTContext $context, %mods, Bool $negate) {
        self.IMPL-MAYBE-NEGATE(self.IMPL-REGEX-QAST($context, %mods), $negate)
    }
}

# The carriage return character class (\r, \R)
class RakuAST::Regex::CharClass::CarriageReturn
  is RakuAST::Regex::CharClass::Negatable
  is RakuAST::Regex::CharClassEnumerationElement
{
    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        QAST::Regex.new:
            :rxtype('enumcharlist'), :negate(self.negated), "\r"
    }

    method IMPL-CCLASS-ENUM-CHARS(%mods) { self.negated ?? "" !! "\r" }

    method IMPL-CCLASS-ENUM-QAST(RakuAST::IMPL::QASTContext $context, %mods, Bool $negate) {
        self.IMPL-MAYBE-NEGATE(self.IMPL-REGEX-QAST($context, %mods), $negate)
    }
}

# The space character class (\s, \S).
class RakuAST::Regex::CharClass::Space
  is RakuAST::Regex::CharClass::Negatable
  is RakuAST::Regex::CharClassEnumerationElement
{
    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        QAST::Regex.new( :rxtype<cclass>, :name<s>, :negate(self.negated) )
    }

    method IMPL-CCLASS-ENUM-QAST(RakuAST::IMPL::QASTContext $context, %mods, Bool $negate) {
        self.IMPL-MAYBE-NEGATE(self.IMPL-REGEX-QAST($context, %mods), $negate)
    }
}

# The tab character class (\t, \T)
class RakuAST::Regex::CharClass::Tab
  is RakuAST::Regex::CharClass::Negatable
  is RakuAST::Regex::CharClassEnumerationElement
{
    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        QAST::Regex.new:
            :rxtype('enumcharlist'), :negate(self.negated), "\t"
    }

    method IMPL-CCLASS-ENUM-CHARS(%mods) { self.negated ?? "" !! "\t" }

    method IMPL-CCLASS-ENUM-QAST(RakuAST::IMPL::QASTContext $context, %mods, Bool $negate) {
        self.IMPL-MAYBE-NEGATE(self.IMPL-REGEX-QAST($context, %mods), $negate)
    }
}

# The vertical whitespace character class (\v, \V)
class RakuAST::Regex::CharClass::VerticalSpace
  is RakuAST::Regex::CharClass::Negatable
  is RakuAST::Regex::CharClassEnumerationElement
{
    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        QAST::Regex.new:
            :rxtype('enumcharlist'), :negate(self.negated),
            "\x[0a,0b,0c,0d,85,2028,2029]\r\n"
    }

    method IMPL-CCLASS-ENUM-QAST(RakuAST::IMPL::QASTContext $context, %mods, Bool $negate) {
        self.IMPL-MAYBE-NEGATE(self.IMPL-REGEX-QAST($context, %mods), $negate)
    }
}

# The word character class (\w, \W).
class RakuAST::Regex::CharClass::Word
  is RakuAST::Regex::CharClass::Negatable
  is RakuAST::Regex::CharClassEnumerationElement
{
    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        QAST::Regex.new( :rxtype<cclass>, :name<w>, :negate(self.negated) )
    }

    method IMPL-CCLASS-ENUM-QAST(RakuAST::IMPL::QASTContext $context, %mods, Bool $negate) {
        self.IMPL-MAYBE-NEGATE(self.IMPL-REGEX-QAST($context, %mods), $negate)
    }
}

# A character class containing a specified character or "long character".
# This covers \c13, \c[13,10], \x1F98B, \c[BUTTERFLY], and so forth (the
# node is always constructed with the character(s) resulting from processing
# these sequences).
class RakuAST::Regex::CharClass::Specified
  is RakuAST::Regex::CharClass::Negatable
  is RakuAST::Regex::CharClassEnumerationElement
{
    has str $.characters;

    method new(Bool :$negated, str :$characters!) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Regex::CharClass::Negatable, '$!negated',
            $negated ?? True !! False);
        nqp::bindattr_s($obj, RakuAST::Regex::CharClass::Specified, '$!characters',
            $characters);
        $obj
    }

    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        self.IMPL-QAST($context, %mods, self.negated)
    }

    method IMPL-CCLASS-ENUM-CHARS(%mods) {
        self.negated || nqp::chars($!characters) != 1 ?? "" !! $!characters
    }

    method IMPL-CCLASS-ENUM-QAST(RakuAST::IMPL::QASTContext $context, %mods, Bool $negate) {
        self.IMPL-QAST($context, %mods, self.negated ?? !$negate !! $negate)
    }

    method IMPL-QAST(RakuAST::IMPL::QASTContext $context, %mods, Bool $negated) {
        if $negated {
            # Negated, it is treated like a "long character". Quoting S05:
            # > A consequence of this is that the negated form advances by a single
            # > position (matching as . does) when the long character doesn't match
            # > as a whole
            # Rakudo got this wrong pre-RakuAST; hopefully it's rare enough of a
            # construct that we can get away with doing it right here.
            QAST::Regex.new(
                :rxtype<concat>,
                QAST::Regex.new( :rxtype<literal>, :subtype<zerowidth>, :negate, $!characters ),
                QAST::Regex.new( :rxtype<cclass>, :name<.> )
            )
        }
        else {
            # Non-negated, match the character(s)
            QAST::Regex.new( :rxtype<literal>, $!characters )
        }
    }
}

# The nul character class (\0)
class RakuAST::Regex::CharClass::Nul
  is RakuAST::Regex::CharClass
  is RakuAST::Regex::CharClassEnumerationElement
{
    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        QAST::Regex.new: :rxtype<literal>, "\0"
    }

    method IMPL-CCLASS-ENUM-CHARS(%mods) { "\0" }
}

# The base of all kinds of back-reference to a capture.
class RakuAST::Regex::BackReference
  is RakuAST::Regex::Atom
{
    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        QAST::Regex.new: :rxtype<subrule>, :subtype<method>, QAST::NodeList.new:
            QAST::SVal.new( :value('!BACKREF') ),
            QAST::SVal.new( :value(self.IMPL-CAPTURE-NAME) )
    }

    method IMPL-CAPTURE-NAME() {
        nqp::die('IMPL-CAPTURE-NAME not implemented in ' ~ self.HOW.name(self))
    }
}

# A back-reference to a positional capture.
class RakuAST::Regex::BackReference::Positional
  is RakuAST::Regex::BackReference
{
    has int $.index;

    method new(int $index) {
        my $obj := nqp::create(self);
        nqp::bindattr_i($obj, RakuAST::Regex::BackReference::Positional, '$!index', $index);
        $obj
    }

    method IMPL-CAPTURE-NAME() { ~$!index }
}

# A back-reference to a named capture.
class RakuAST::Regex::BackReference::Named
  is RakuAST::Regex::BackReference
{
    has str $.name;

    method new(str $name) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Regex::BackReference::Named, '$!name', $name);
        $obj
    }

    method IMPL-CAPTURE-NAME() { $!name }
}

# A statement embedded in a regex, typically used for making a variable
# declaration.
class RakuAST::Regex::Statement
  is RakuAST::Regex::Atom
{
    has RakuAST::Statement $.statement;

    method new(RakuAST::Statement $statement) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Regex::Statement, '$!statement', $statement);
        $obj
    }

    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        QAST::Regex.new: :rxtype<qastnode>, :subtype<declarative>,
            $!statement.IMPL-TO-QAST($context)
    }

    method visit-children(Code $visitor) {
        $visitor($!statement);
    }
}

# A block of code embedded in a regex, executed only for its side-effects.
class RakuAST::Regex::Block
  is RakuAST::Regex::Atom
  is RakuAST::CheckTime
{
    has RakuAST::Block $.block;

    method new(RakuAST::Block $block) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Regex::Block, '$!block', $block);
        $obj
    }

    method quantifiable() { False }

    method PERFORM-CHECK(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        $!block.body.statement-list.visit-children(-> $statement {
            if nqp::istype($statement, RakuAST::Statement::Expression) {
                if nqp::istype($statement.expression, RakuAST::Var::Attribute) {
                    self.add-sorry:
                      $resolver.build-exception: 'X::Attribute::Regex', :symbol($statement.expression.name);
                }
            }
        });
    }

    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        my $block-call := self.IMPL-REGEX-BLOCK-CALL($context, $!block);
        QAST::Regex.new( $block-call, :rxtype<qastnode> )
    }

    method visit-children(Code $visitor) {
        $visitor($!block);
    }
}

# An interpolation of a variable into a regex. While this is typically a
# variable, in fact it could also be a contextualizer like `$(something())`,
# thus it can be constructed with any expression.
class RakuAST::Regex::Interpolation
  is RakuAST::Regex::Atom
  is RakuAST::CheckTime
  is RakuAST::ImplicitLookups
{
    has RakuAST::Expression $.var;
    has Bool $.sequential;

    method new(RakuAST::Expression :$var, Bool :$sequential) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Regex::Interpolation, '$!var', $var);
        nqp::bindattr($obj, RakuAST::Regex::Interpolation, '$!sequential',
            $sequential ?? True !! False);
        $obj
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('PseudoStash')),
        ])
    }

    method PERFORM-CHECK(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        if nqp::istype($!var, RakuAST::Var::Attribute) {
            self.add-sorry:
              $resolver.build-exception: 'X::Attribute::Regex', :symbol($!var.name);
        }
    }

    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        # Look for fast paths.
        if nqp::istype($!var, RakuAST::Lookup) && $!var.is-resolved {
            my $resolution := $!var.resolution;
            # TODO contant case
            if !%mods<m> && nqp::istype($resolution, RakuAST::VarDeclaration::Simple) &&
                    $resolution.sigil eq '$' {
                my $type := $resolution.type;
                if $type && $type.is-known-to-be(Str) {
                    # Certainly a string.
                    return QAST::Regex.new:
                        :rxtype<subrule>, :subtype<method>,
                        QAST::NodeList.new:
                            QAST::SVal.new( :value('!LITERAL') ),
                            $!var.IMPL-TO-QAST($context),
                            QAST::IVal.new( :value(%mods<i> ?? 1 !! 0) )
                }
            }
        }

        # Fallback to slow path.
        QAST::Regex.new:
            :rxtype<subrule>, :subtype<method>,
            QAST::NodeList.new:
                QAST::SVal.new( :value('INTERPOLATE') ),
                $!var.IMPL-TO-QAST($context),
                QAST::IVal.new( :value((%mods<i> ?? 1 !! 0) + (%mods<m> ?? 2 !! 0)) ),
                QAST::IVal.new( :value(0) ),
                QAST::IVal.new( :value($!sequential ?? 1 !! 0) ),
                QAST::IVal.new( :value(0) ),
                QAST::Op.new(
                    :op<callmethod>, :name<new>,
                   self.get-implicit-lookups.AT-POS(0).IMPL-TO-QAST($context)
                )
    }

    method visit-children(Code $visitor) {
        $visitor($!var);
    }
}

# The base of all regex assertions (things of the form `<...>`, such as subrule
# calls, lookaheads, and user-defined character classes).
class RakuAST::Regex::Assertion
  is RakuAST::Regex::Atom
{
    method IMPL-INTERPOLATE-ASSERTION(RakuAST::IMPL::QASTContext $context, %mods,
            Mu $expression-qast, Bool $sequential, Mu $PseudoStash) {
        QAST::Regex.new:
            :rxtype<subrule>, :subtype<method>,
            QAST::NodeList.new:
                QAST::SVal.new( :value('INTERPOLATE_ASSERTION') ),
                $expression-qast,
                QAST::IVal.new( :value((%mods<i> ?? 1 !! 0) + (%mods<m> ?? 2 !! 0)) ),
                QAST::IVal.new( :value(0) ), # XXX 1 if MONKEY-SEE-NO-EVAL
                QAST::IVal.new( :value($sequential ?? 1 !! 0) ),
                QAST::IVal.new( :value(1) ),
                QAST::Op.new(
                    :op<callmethod>, :name<new>,
                    $PseudoStash.IMPL-TO-QAST($context)
                )
    }
}

# An assertion that always passes.
class RakuAST::Regex::Assertion::Pass
  is RakuAST::Regex::Assertion
{
    method new() {
        nqp::create(self)
    }

    method quantifiable() { False }

    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        QAST::Regex.new( :rxtype<anchor>, :subtype<pass> )
    }
}

# An assertion that always fails.
class RakuAST::Regex::Assertion::Fail
  is RakuAST::Regex::Assertion
{
    method new() {
        nqp::create(self)
    }

    method quantifiable() { False }

    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        QAST::Regex.new( :rxtype<anchor>, :subtype<fail> )
    }
}

# A named assertion, which may or may not capture. Models `<foo>` and
# `<.foo>`, and also `<foo::bar>`. Forms with arguments or taking a regex
# argument are modeled as subclasses of this.
class RakuAST::Regex::Assertion::Named
  is RakuAST::Regex::Assertion
  is RakuAST::ImplicitLookups
{
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

    method PRODUCE-IMPLICIT-LOOKUPS() {
        # A call like <foo> will look for <&foo> and only then do <.foo>
        # (but in both cases it captures).
        if $!capturing && $!name.is-identifier {
            self.IMPL-WRAP-LIST: [RakuAST::Var::Lexical.new('&' ~ $!name.canonicalize)]
        }
        else {
            if $!name.is-identifier {
                self.IMPL-WRAP-LIST: []
            }
            else {
                my @parts := $!name.IMPL-UNWRAP-LIST($!name.parts);
                my @package-parts := nqp::slice(@parts, 0, nqp::elems(@parts) - 2);
                my $package-name := RakuAST::Name.new(|@package-parts);
                self.IMPL-WRAP-LIST: [RakuAST::Type::Simple.new($package-name)]
            }
        }
    }

    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        self.IMPL-REGEX-QAST-CALL($context)
    }

    method IMPL-REGEX-QAST-CALL(RakuAST::IMPL::QASTContext $context) {
        my $longname := $!name;
        if $longname.is-identifier {
            my $name := $longname.canonicalize;
            if $name eq 'sym' {
                nqp::die('Can only use <sym> token in a proto regex');
            }
            else {
                my $lookups := self.get-implicit-lookups;
                my $qast;
                if $lookups.elems && (my $lookup := $lookups.AT-POS(0)).is-resolved
                    && nqp::istype((my $resolution := $lookup.resolution), RakuAST::CompileTimeValue)
                    && nqp::istype($resolution.compile-time-value, Regex)
                {
                    $qast := QAST::Regex.new: :rxtype<subrule>,:subtype<method>,
                        QAST::NodeList.new:
                            QAST::SVal.new( :value('CALL_SUBRULE') ),
                            $lookups.AT-POS(0).IMPL-TO-QAST($context);
                }
                else {
                    $qast := QAST::Regex.new: :rxtype<subrule>,
                        QAST::NodeList.new(QAST::SVal.new( :value($name) ));
                }
                if $!capturing {
                    $qast.subtype('capture');
                    $qast.name($name);
                }
                $qast
            }
        }
        else {
            my @parts := $!name.IMPL-UNWRAP-LIST($!name.parts);
            my @pairs := $!name.IMPL-UNWRAP-LIST($!name.colonpairs);
            my $sub := @parts[nqp::elems(@parts) - 1];
            my $sub-name := RakuAST::Name.new($sub);
            $sub-name.set-colonpairs(@pairs);

            my $lookups := self.get-implicit-lookups;
            my $package := $lookups.AT-POS(0).meta-object;
            $context.ensure-sc($package);
            my $qast := QAST::Regex.new: :rxtype<subrule>,:subtype<method>,
                QAST::NodeList.new:
                    QAST::SVal.new( :value('OTHERGRAMMAR') ),
                    QAST::WVal.new( :value($package) ),
                    QAST::SVal.new( :value($sub-name.canonicalize) ) ;
            if $!capturing {
                $qast.subtype('capture');
                $qast.name($!name.canonicalize);
            }
            $qast
        }
    }

    method visit-children(Code $visitor) {
        $visitor($!name);
    }
}

# A named rule called with args.
class RakuAST::Regex::Assertion::Named::Args
  is RakuAST::Regex::Assertion::Named
{
    has RakuAST::ArgList $.args;

    method new(RakuAST::Name :$name!, Bool :$capturing, Raku::ArgList :$args!) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Regex::Assertion::Named, '$!name', $name);
        nqp::bindattr($obj, RakuAST::Regex::Assertion::Named, '$!capturing',
            $capturing ?? True !! False);
        nqp::bindattr($obj, RakuAST::Regex::Assertion::Named::Args, '$!args', $args);
        $obj
    }

    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        my $call := self.IMPL-REGEX-QAST-CALL($context);
        $!args.IMPL-ADD-QAST-ARGS($context, $call[0]);
        $call
    }

    method visit-children(Code $visitor) {
        $visitor(self.name);
        $visitor($!args);
    }
}

# A named rule called with a regex argument.
class RakuAST::Regex::Assertion::Named::RegexArg
  is RakuAST::Regex::Assertion::Named
  is RakuAST::RegexThunk
{
    has RakuAST::Regex $.regex-arg;

    # Used during compilation
    has str $!unique-name;
    has Mu $!body-qast;

    method new(RakuAST::Name :$name!, Bool :$capturing, Raku::Regex :$regex-arg!) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Regex::Assertion::Named, '$!name', $name);
        nqp::bindattr($obj, RakuAST::Regex::Assertion::Named, '$!capturing',
            $capturing ?? True !! False);
        nqp::bindattr($obj, RakuAST::Regex::Assertion::Named::RegexArg,
            '$!regex-arg', $regex-arg);
        $obj
    }

    method IMPL-UNIQUE-NAME() {
        my str $unique-name := $!unique-name;
        unless $unique-name {
            nqp::bindattr_s(self, RakuAST::Regex::Assertion::Named::RegexArg, '$!unique-name',
                ($unique-name := QAST::Node.unique('!__REGEX_ARG_')));
        }
        $unique-name
    }

    method IMPL-THUNKED-REGEX-QAST(RakuAST::IMPL::QASTContext $context) {
        $!regex-arg.IMPL-REGEX-TOP-LEVEL-QAST($context, self.meta-object, nqp::hash(),
            :body-qast($!body-qast // nqp::die('Misordered regex compilation')),
            :no-scan);
    }

    method IMPL-QAST-DECL-CODE(RakuAST::IMPL::QASTContext $context) {
        # Form the block itself and link it with the meta-object. Install it
        # in the lexpad; we'll look it up when we need it. This means we can
        # avoid closure-cloning it per time we enter it, which may help if we
        # are scanning or it's in a quantified thing.
        my str $name := self.IMPL-UNIQUE-NAME;
        my $block := self.IMPL-QAST-FORM-BLOCK($context, :blocktype('declaration_static'));
        self.IMPL-LINK-META-OBJECT($context, $block);
        QAST::Stmts.new(
            $block,
            QAST::Op.new(
                :op('bind'),
                QAST::Var.new( :decl<var>, :scope<lexical>, :$name ),
                self.IMPL-CLOSURE-QAST($context)
            )
        )
    }

    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        nqp::bindattr(self, RakuAST::Regex::Assertion::Named::RegexArg, '$!body-qast',
            $!regex-arg.IMPL-REGEX-QAST($context, %mods));
        my $qast := self.IMPL-REGEX-QAST-CALL($context);
        my str $name := self.IMPL-UNIQUE-NAME;
        $qast[0].push(QAST::Var.new( :$name, :scope('lexical') ));
        $qast
    }

    method visit-children(Code $visitor) {
        $visitor(self.name);
        $visitor($!regex-arg);
    }
}

# An alias assertion (where another assertion is given an extra name - or, in
# the case it's anonymous, perhaps just a name).
class RakuAST::Regex::Assertion::Alias
  is RakuAST::Regex::Assertion
{
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

    method visit-children(Code $visitor) {
        $visitor($!assertion);
    }
}

# A lookahead assertion (where another assertion is evaluated as a
# zerowidth lookahead, either positive or negative).
class RakuAST::Regex::Assertion::Lookahead
  is RakuAST::Regex::Assertion
{
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

# An assertion that evaluates a block of code and then interpolates the result,
# treating it as code to be evaluated.
class RakuAST::Regex::Assertion::InterpolatedBlock
  is RakuAST::Regex::Assertion
  is RakuAST::ImplicitLookups
{
    has RakuAST::Block $.block;
    has Bool $.sequential;

    method new(RakuAST::Block :$block!, Bool :$sequential) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Regex::Assertion::InterpolatedBlock, '$!block', $block);
        nqp::bindattr($obj, RakuAST::Regex::Assertion::InterpolatedBlock, '$!sequential',
            $sequential ?? True !! False);
        $obj
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('PseudoStash')),
        ])
    }

    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        self.IMPL-INTERPOLATE-ASSERTION(
          $context,
          %mods,
          self.IMPL-REGEX-BLOCK-CALL($context, $!block),
          $!sequential,
          self.get-implicit-lookups.AT-POS(0)
        )
    }

    method visit-children(Code $visitor) {
        $visitor($!block);
    }
}

# An assertion that does a variable lookup and then interpolates the result,
# treating it as code to be evaluated.
class RakuAST::Regex::Assertion::InterpolatedVar
  is RakuAST::Regex::Assertion
  is RakuAST::CheckTime
  is RakuAST::ImplicitLookups
{
    has RakuAST::Expression $.var;
    has Bool $.sequential;

    method new(RakuAST::Expression :$var!, Bool :$sequential) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Regex::Assertion::InterpolatedVar, '$!var', $var);
        nqp::bindattr($obj, RakuAST::Regex::Assertion::InterpolatedVar, '$!sequential',
            $sequential ?? True !! False);
        $obj
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('PseudoStash')),
        ])
    }

    method PERFORM-CHECK(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        if nqp::istype($!var, RakuAST::Var::Attribute) {
            self.add-sorry:
              $resolver.build-exception: 'X::Attribute::Regex', :symbol($!var.name);
        }
    }

    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        self.IMPL-INTERPOLATE-ASSERTION(
          $context,
          %mods,
          $!var.IMPL-TO-QAST($context),
          $!sequential,
          self.get-implicit-lookups.AT-POS(0)
        )
    }

    method visit-children(Code $visitor) {
        $visitor($!var);
    }
}

# An assertion of the form <&foo> or <&foo($arg)>, which resolves the callable
# (typically a lexical rules) and then calls it.
class RakuAST::Regex::Assertion::Callable
  is RakuAST::Regex::Assertion
{
    has RakuAST::Expression $.callee;
    has RakuAST::ArgList $.args;

    method new(RakuAST::Expression :$callee!, Raku::ArgList :$args) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Regex::Assertion::Callable, '$!callee', $callee);
        nqp::bindattr($obj, RakuAST::Regex::Assertion::Callable, '$!args',
            $args // RakuAST::ArgList);
        $obj
    }

    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        my $node-list := QAST::NodeList.new:
            QAST::SVal.new( :value('CALL_SUBRULE') ),
            $!callee.IMPL-TO-QAST($context);
        $!args.IMPL-ADD-QAST-ARGS($context, $node-list) if $!args;
        QAST::Regex.new( :rxtype<subrule>, :subtype<method>, $node-list )
    }

    method visit-children(Code $visitor) {
        $visitor($!callee);
        $visitor($!args) if $!args;
    }
}

# An assertion that evaluates a block of code and then decides whether to match
# based on the boolification of the produced result.
class RakuAST::Regex::Assertion::PredicateBlock
  is RakuAST::Regex::Assertion
{
    has Bool $.negated;
    has RakuAST::Block $.block;

    method new(Bool :$negated, RakuAST::Block :$block!) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Regex::Assertion::PredicateBlock, '$!negated',
            $negated ?? True !! False);
        nqp::bindattr($obj, RakuAST::Regex::Assertion::PredicateBlock, '$!block', $block);
        $obj
    }

    method quantifiable() { False }

    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        QAST::Regex.new:
            :rxtype<qastnode>, :subtype<zerowidth>, :negate($!negated ?? 1 !! 0),
            self.IMPL-REGEX-BLOCK-CALL($context, $!block)
    }

    method visit-children(Code $visitor) {
        $visitor($!block);
    }
}

# An assertion containing one or more character class elements.
class RakuAST::Regex::Assertion::CharClass
  is RakuAST::Regex::Assertion
{
    has Mu $!elements;

    method new(*@elements) {
        my $obj := nqp::create(self);
        if nqp::elems(@elements) == 0 {
            nqp::die('RakuAST::Regex::Assertion::CharClass must have at least one element');
        }
        for @elements {
            unless nqp::istype($_, RakuAST::Regex::CharClassElement) {
                nqp::die('Can only construct a RakuAST::Regex::Assertion::CharClass with elements of type RakuAST::Regex::CharClassElement')
            }
        }
        nqp::bindattr($obj, RakuAST::Regex::Assertion::CharClass, '$!elements',
            @elements);
        $obj
    }

    method elements() {
        self.IMPL-WRAP-LIST($!elements)
    }

    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        # Compile the first element.
        my $first := $!elements[0];
        my $qast := $first.IMPL-CCLASS-QAST($context, %mods, True);

        # Add further elements, exactly how depending on the exact nature of
        # the elements
        if nqp::elems($!elements) > 1 {
            my int $i := 1;
            while $i < nqp::elems($!elements) {
                my $elem := $!elements[$i];
                my $elem-qast := $elem.IMPL-CCLASS-QAST($context, %mods, False);
                if $elem.negated {
                    $elem-qast.subtype('zerowidth');
                    $qast := QAST::Regex.new:
                        :rxtype<concat>, :subtype<zerowidth>, :negate,
                        QAST::Regex.new( :rxtype<conj>, :subtype<zerowidth>, $elem-qast ),
                        $qast;
                }
                else {
                    $qast := QAST::Regex.new( :rxtype<alt>, $qast, $elem-qast );
                }
                $i++;
            }
        }

        $qast
    }

    method visit-children(Code $visitor) {
        for $!elements {
            $visitor($_);
        }
    }
}

class RakuAST::Regex::Assertion::Recurse
  is RakuAST::Regex::Assertion
{
  has RakuAST::Regex::Term $.node;

  method new(RakuAST::Regex $node) {
    my $obj := nqp::create(self);
    nqp::bindattr($obj, RakuAST::Regex::Assertion::Recurse, '$!node', $node);
    $obj;
  }

  method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
     QAST::Regex.new:
        :rxtype<subrule>, :subtype<method>,
        QAST::NodeList.new( QAST::SVal.new( :value('RECURSE') ), :node($!node));
  }

}

# The base of all user-defined character class elements.
class RakuAST::Regex::CharClassElement
  is RakuAST::Node
{
    has Bool $.negated;
}

# A character class element that calls another rule (for example, <-alpha>).
class RakuAST::Regex::CharClassElement::Rule
  is RakuAST::Regex::CharClassElement
{
    has str $.name;

    method new(str :$name!, Bool :$negated) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Regex::CharClassElement, '$!negated',
            $negated ?? True !! False);
        nqp::bindattr_s($obj, RakuAST::Regex::CharClassElement::Rule, '$!name', $name);
        $obj
    }

    method IMPL-CCLASS-QAST(RakuAST::IMPL::QASTContext $context, %mods, Bool $first) {
        my $negate := self.negated;
        my $name := QAST::NodeList.new(QAST::SVal.new( :value($!name) ));
        if $negate && $first {
            QAST::Regex.new: :rxtype<concat>,
                QAST::Regex.new( :rxtype<subrule>, :subtype<zerowidth>, :$negate, $name ),
                QAST::Regex.new( :rxtype<cclass>, :name<.> );
        }
        else {
            QAST::Regex.new( :rxtype<subrule>, :subtype<method>, :$negate, $name )
        }
    }
}

# A character class element that tests a Unicode property.
class RakuAST::Regex::CharClassElement::Property
  is RakuAST::Regex::CharClassElement
{
    has str $.property;
    has Bool $.inverted;
    has RakuAST::Expression $.predicate;

    method new(str :$property!, Bool :$inverted, RakuAST::Expression :$predicate,
            Bool :$negated) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Regex::CharClassElement, '$!negated',
            $negated ?? True !! False);
        nqp::bindattr_s($obj, RakuAST::Regex::CharClassElement::Property, '$!property',
            $property);
        nqp::bindattr($obj, RakuAST::Regex::CharClassElement::Property, '$!inverted',
            $inverted ?? True !! False);
        nqp::bindattr($obj, RakuAST::Regex::CharClassElement::Property, '$!predicate',
            $predicate // RakuAST::Expression);
        $obj
    }

    method IMPL-CCLASS-QAST(RakuAST::IMPL::QASTContext $context, %mods, Bool $first) {
        my $negate := self.negated ?? !$!inverted !! $!inverted;
        my $qast := QAST::Regex.new( :rxtype<uniprop>, :$negate, $!property );
        $qast.push($!predicate.IMPL-TO-QAST($context)) if $!predicate;
        $qast
    }

    method visit-children(Code $visitor) {
        $visitor($!predicate) if $!predicate;
    }
}

# A character class element that is a user-defined enumeration of characters,
# including characters, ranges, and backslash sequences.
class RakuAST::Regex::CharClassElement::Enumeration
  is RakuAST::Regex::CharClassElement
  is RakuAST::CheckTime
{
    has Mu $!elements;

    method new(List :$elements!, Bool :$negated) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Regex::CharClassElement, '$!negated',
            $negated ?? True !! False);
        my @elements := self.IMPL-UNWRAP-LIST($elements);
        nqp::bindattr($obj, RakuAST::Regex::CharClassElement::Enumeration,
          '$!elements', self.IMPL-UNWRAP-LIST($elements));
        $obj
    }

    method elements() {
        self.IMPL-WRAP-LIST($!elements)
    }

    method PERFORM-CHECK(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        if nqp::elems($!elements) {
            for $!elements {
                unless nqp::istype($_,RakuAST::Regex::CharClassEnumerationElement) {
                    self.add-sorry:
                      $resolver.build-exception: 'X::AdHoc',
                        payload => "Character classes can only be built with RakuAST::Regex::CharClassEnumerationElement objects,\n not with " ~ $_.HOW.name($_) ~ " elements";
                }
            }
        }
        else {
            self.add-worry:
              $resolver.build-exception: 'X::AdHoc',
                payload => "Character classes without elements will never match"
        }
    }

    method IMPL-CCLASS-QAST(RakuAST::IMPL::QASTContext $context, %mods, Bool $first) {
        # Go through the elements, which either produce characters to go into an
        # enumeration or a QAST node.
        my str $enum := '';
        my @alts;
        for $!elements {
            my str $enum-contrib := $_.IMPL-CCLASS-ENUM-CHARS(%mods);
            if $enum-contrib ne '' {
                $enum := $enum ~ $enum-contrib;
            }
            else {
                @alts.push($_.IMPL-CCLASS-ENUM-QAST($context, %mods, self.negated));
            }
        }

        # If we collected characters, add the enumeration to the alternation
        # parts we'll compile into.
        if $enum {
            @alts.push: QAST::Regex.new:
                :rxtype<enumcharlist>, :negate(self.negated),
                :subtype(%mods<m> ?? 'ignoremark' !! ''),
                $enum
        }

        # A single alternation part can compile into just that.
        if nqp::elems(@alts) == 1 {
            @alts[0]
        }

        # An empty enumation always fails.
        elsif nqp::elems(@alts) == 0 {
            QAST::Regex.new( :rxtype<anchor>, :subtype<fail> )
        }

        else {
            self.negated ??
                QAST::Regex.new( :rxtype<concat>, :negate(1),
                    QAST::Regex.new( :rxtype<conj>, :subtype<zerowidth>, |@alts ),
                    QAST::Regex.new( :rxtype<cclass>, :name<.> ) ) !!
                QAST::Regex.new( :rxtype<alt>, |@alts );
        }
    }

    method visit-children(Code $visitor) {
        for $!elements {
            $visitor($_);
        }
    }
}

# A single character in a character class enumeration (for example, the "a" in
# `<[a]>`).
class RakuAST::Regex::CharClassEnumerationElement::Character
  is RakuAST::Regex::CharClassEnumerationElement
{
    has str $.character;

    method new(str $character) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Regex::CharClassEnumerationElement::Character,
            '$!character', $character);
        $obj
    }

    method IMPL-CCLASS-ENUM-CHARS(%mods) {
        my str $c := %mods<m>
            ?? nqp::chr(nqp::ordbaseat($!character, 0))
            !! $!character;
        %mods<i>
            ?? nqp::fc($c) ~ nqp::uc($c)
            !! $c
    }
}

# A range of characters in a character class enumeration, for example the a..f
# in `<[a..f]>`. Constructed with two integer codepoints, which means that a
# number of problems are not possible at the AST level.
class RakuAST::Regex::CharClassEnumerationElement::Range
  is RakuAST::CheckTime
  is RakuAST::Regex::CharClassEnumerationElement
{
    has int $.from;
    has int $.to;

    method new(int :$from!, int :$to!) {
        my $obj := nqp::create(self);
        nqp::bindattr_i($obj, RakuAST::Regex::CharClassEnumerationElement::Range,
            '$!from', $from);
        nqp::bindattr_i($obj, RakuAST::Regex::CharClassEnumerationElement::Range,
            '$!to', $to);
        $obj
    }

    method PERFORM-CHECK(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        if $!from > $!to {
            self.add-sorry:
              $resolver.build-exception: 'X::AdHoc',
                payload => "Illegal reversed character range";
        }
    }

    method IMPL-CCLASS-ENUM-QAST(RakuAST::IMPL::QASTContext $context, %mods, Bool $negate) {
        QAST::Regex.new:
            :rxtype<charrange>, :$negate,
            %mods<m>
                ?? (%mods<i> ?? 'ignorecase+ignoremark' !! 'ignoremark')
                !! (%mods<i> ?? 'ignorecase' !! ''),
            QAST::IVal.new( :value($!from) ),
            QAST::IVal.new( :value($!to) )
    }
}

# The base of all internal modifiers.
class RakuAST::Regex::InternalModifier
  is RakuAST::Regex::Atom
{
    has  str $.modifier;  # for proper deparsing
    has Bool $.negated;

    method new(str :$modifier, Bool :$negated) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj,RakuAST::Regex::InternalModifier,'$!modifier',
          $modifier // self.key);
        nqp::bindattr(  $obj,RakuAST::Regex::InternalModifier,'$!negated',
          $negated ?? True !! False);
        $obj
    }

    method quantifiable()         { False }
    method whitespace-wrappable() { False }

    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        %mods{self.key} := !self.negated;
        Nil
    }
}

# The ignorecase internal modifier.
class RakuAST::Regex::InternalModifier::IgnoreCase
  is RakuAST::Regex::InternalModifier
{
    method key() { 'i' }
}

# The ignoremark internal modifier.
class RakuAST::Regex::InternalModifier::IgnoreMark
  is RakuAST::Regex::InternalModifier
{
    method key() { 'm' }
}

# The ratchet internal modifier.
class RakuAST::Regex::InternalModifier::Ratchet
  is RakuAST::Regex::InternalModifier
{
    method key() { 'r' }
}

# The sigspace internal modifier.
class RakuAST::Regex::InternalModifier::Sigspace
  is RakuAST::Regex::InternalModifier
{
    method key() { 's' }
}

# A quantified atom in a regex - that is, an atom with a quantifier and
# optional separator.
class RakuAST::Regex::QuantifiedAtom
  is RakuAST::Regex::Term
  is RakuAST::CheckTime
{
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

    method replace-atom(RakuAST::Atom $atom) {
        nqp::bindattr(self, RakuAST::Regex::QuantifiedAtom, '$!atom', $atom);
        Nil
    }

    method PERFORM-CHECK(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        unless $!atom.quantifiable {
            self.add-sorry:
              $resolver.build-exception: 'X::Syntax::Regex::NonQuantifiable';
        }
    }

    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        my $atom := $!atom.IMPL-REGEX-QAST($context, %mods);
        my $quantified := $!quantifier.IMPL-QAST-QUANTIFY($context, $atom, %mods);
        if $!separator {
            my $separator-qast := $!separator.IMPL-REGEX-QAST($context, %mods);
            $quantified.push($separator-qast);
            if $!trailing-separator {
                QAST::Regex.new(
                    :rxtype<concat>,
                    $quantified,
                    QAST::Regex.new(
                        :rxtype<quant>, :min(0), :max(1),
                        #NOTE this has to be the same QAST object as pushed into quantified, so
                        # it'll get the same capture group assigned.
                        $separator-qast,
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
class RakuAST::Regex::Quantifier
  is RakuAST::Node
{
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
class RakuAST::Regex::Quantifier::ZeroOrOne
  is RakuAST::Regex::Quantifier
{
    method IMPL-QAST-QUANTIFY(RakuAST::IMPL::QASTContext $context, Mu $atom-qast, %mods) {
        self.backtrack.IMPL-QAST-APPLY:
            QAST::Regex.new( :rxtype<quant>, :subtype<item>, :min(0), :max(1), $atom-qast ),
            %mods
    }
}

# The zero or more (*) quantifier.
class RakuAST::Regex::Quantifier::ZeroOrMore
  is RakuAST::Regex::Quantifier
{
    method IMPL-QAST-QUANTIFY(RakuAST::IMPL::QASTContext $context, Mu $atom-qast, %mods) {
        self.backtrack.IMPL-QAST-APPLY:
            QAST::Regex.new( :rxtype<quant>, :min(0), :max(-1), $atom-qast ),
            %mods
    }
}

# The one or more (+) quantifier.
class RakuAST::Regex::Quantifier::OneOrMore
  is RakuAST::Regex::Quantifier
{
    method IMPL-QAST-QUANTIFY(RakuAST::IMPL::QASTContext $context, Mu $atom-qast, %mods) {
        self.backtrack.IMPL-QAST-APPLY:
            QAST::Regex.new( :rxtype<quant>, :min(1), :max(-1), $atom-qast ),
            %mods
    }
}

# The literal range (** 1..5) quantifier.
class RakuAST::Regex::Quantifier::Range
  is RakuAST::Regex::Quantifier
{
    has Int $.min;
    has Int $.max;
    has Bool $.excludes-min;
    has Bool $.excludes-max;

    method new(Int :$min, Int :$max, Bool :$excludes-max, Bool :$excludes-min,
            RakuAST::Regex::Backtrack :$backtrack) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Regex::Quantifier, '$!backtrack',
            nqp::istype($backtrack, RakuAST::Regex::Backtrack)
                ?? $backtrack
                !! RakuAST::Regex::Backtrack);
        nqp::bindattr($obj, RakuAST::Regex::Quantifier::Range, '$!min', $min // Int);
        nqp::bindattr($obj, RakuAST::Regex::Quantifier::Range, '$!max', $max // Int);
        nqp::bindattr($obj, RakuAST::Regex::Quantifier::Range, '$!excludes-min',
            $excludes-min ?? True !! False);
        nqp::bindattr($obj, RakuAST::Regex::Quantifier::Range, '$!excludes-max',
            $excludes-max ?? True !! False);
        $obj
    }

    method IMPL-QAST-QUANTIFY(RakuAST::IMPL::QASTContext $context, Mu $atom-qast, %mods) {
        my int $min := $!min // 0;
        $min++ if $!excludes-min;
        my int $max := -1;
        if $!max {
            $max := $!max;
            $max-- if $!excludes-max;
        }
        self.backtrack.IMPL-QAST-APPLY:
            QAST::Regex.new( :rxtype<quant>, :min($min), :max($max), $atom-qast ),
            %mods
    }
}

# The block range (** {$n..$m}) quantifier.
class RakuAST::Regex::Quantifier::BlockRange
  is RakuAST::Regex::Quantifier
{
    has RakuAST::Block $.block;

    method new(RakuAST::Block :$block!, RakuAST::Regex::Backtrack :$backtrack) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Regex::Quantifier, '$!backtrack',
            nqp::istype($backtrack, RakuAST::Regex::Backtrack)
                ?? $backtrack
                !! RakuAST::Regex::Backtrack);
        nqp::bindattr($obj, RakuAST::Regex::Quantifier::BlockRange, '$!block', $block);
        $obj
    }

    method IMPL-QAST-QUANTIFY(RakuAST::IMPL::QASTContext $context, Mu $atom-qast, %mods) {
        self.backtrack.IMPL-QAST-APPLY:
            QAST::Regex.new(
                :rxtype<dynquant>,
                $atom-qast,
                QAST::Op.new(
                    :op('callmethod'), :name('DYNQUANT_LIMITS'),
                    QAST::Var.new( :name('$¢'), :scope('lexical') ),
                    RakuAST::Regex.IMPL-REGEX-BLOCK-CALL($context, $!block)
                ),
            ),
            %mods
    }

    method visit-children(Code $visitor) {
        $visitor(self.backtrack);
        $visitor($!block);
    }
}

# An atom followed by a backtracking modifier.
class RakuAST::Regex::BacktrackModifiedAtom
  is RakuAST::Regex::Term
{
    has RakuAST::Atom $.atom;
    has RakuAST::Regex::Backtrack $.backtrack;

    method new(RakuAST::Atom :$atom!, RakuAST::Regex::Backtrack :$backtrack!) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Regex::BacktrackModifiedAtom, '$!atom', $atom);
        nqp::bindattr($obj, RakuAST::Regex::BacktrackModifiedAtom, '$!backtrack', $backtrack);
        $obj
    }

    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        my $atom := $!atom.IMPL-REGEX-QAST($context, %mods);
        $atom.backtrack ?? $atom !! $!backtrack.IMPL-QAST-APPLY($atom, %mods)
    }

    method visit-children(Code $visitor) {
        $visitor($!atom);
        $visitor($!backtrack);
    }
}

# Backtracking modifiers.
class RakuAST::Regex::Backtrack
  is RakuAST::Node
{
    # These classes purely exist as markers and don't need to be
    # instantiated.  However, some people might do that and then
    # find deparsing doesn't work because deparsing only checks
    # for the type objects.  Alternately, we could make calling
    # .new here a worry, but that also seems a bit over the top.
    # So just return the type object as if .new was never called.
    method new() { self }

    method IMPL-QAST-APPLY(Mu $quant-qast, %mods) {
        $quant-qast.backtrack('r') if %mods<r>;
        $quant-qast
    }
}
class RakuAST::Regex::Backtrack::Greedy
  is RakuAST::Regex::Backtrack
{
    method IMPL-QAST-APPLY(Mu $quant-qast, %mods) {
        $quant-qast.backtrack('g');
        $quant-qast
    }
}
class RakuAST::Regex::Backtrack::Frugal
  is RakuAST::Regex::Backtrack
{
    method IMPL-QAST-APPLY(Mu $quant-qast, %mods) {
        $quant-qast.backtrack('f');
        $quant-qast
    }
}
class RakuAST::Regex::Backtrack::Ratchet
  is RakuAST::Regex::Backtrack
{
    method IMPL-QAST-APPLY(Mu $quant-qast, %mods) {
        $quant-qast.backtrack('r');
        $quant-qast
    }
}

# A regex atom or term followed by some whitespace
class RakuAST::Regex::WithWhitespace
  is RakuAST::Regex::Atom
{
    has RakuAST::Regex::Term $.regex;

    method new(RakuAST::Regex::Term $regex) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Regex::WithWhitespace, '$!regex', $regex);
        $obj
    }

    method IMPL-REGEX-QAST(RakuAST::IMPL::QASTContext $context, %mods) {
        my $qast := $!regex.IMPL-REGEX-QAST($context, %mods);
        %mods<s>
          ?? QAST::Regex.new(:rxtype<concat>,
               $qast,
               QAST::Regex.new:
                 :rxtype<subrule>, :subtype<method>, :name<ws>,
                 QAST::NodeList.new(QAST::SVal.new( :value('ws') ))
             )
          !! $qast
    }

    method visit-children(Code $visitor) {
        $visitor($!regex);
    }
    method whitespace-wrappable() { False }
}
