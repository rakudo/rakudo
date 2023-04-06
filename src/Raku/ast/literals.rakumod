# Marker for all compile-time literals
class RakuAST::Literal
  is RakuAST::Term
  is RakuAST::CompileTimeValue
{
    method type() { self.value.WHAT }
    method compile-time-value() { self.value }
    method IMPL-CAN-INTERPRET() { True }
    method IMPL-INTERPRET(RakuAST::IMPL::InterpContext $ctx) { self.value }

    # default for non int/str/num literals
    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my $value := self.value;
        $context.ensure-sc($value);
        QAST::WVal.new( :$value )
    }
}

class RakuAST::IntLiteral
  is RakuAST::Literal
{
    has Int $.value;

    method new(Int $value) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::IntLiteral, '$!value', $value);
        $obj
    }

    method ast-type {
        RakuAST::Type::Simple.new(RakuAST::Name.from-identifier('Int'))
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my $value := $!value;
        $context.ensure-sc($value);
        my $wval := QAST::WVal.new( :$value );
        nqp::isbig_I($value)
          ?? $wval
          !! QAST::Want.new(
               $wval,
               'Ii',
               QAST::IVal.new(:value(nqp::unbox_i($value)))
             )
    }
}

class RakuAST::NumLiteral
  is RakuAST::Literal
{
    has Num $.value;

    method new(Num $value) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::NumLiteral, '$!value', $value);
        $obj
    }

    method ast-type {
        RakuAST::Type::Simple.new(RakuAST::Name.from-identifier('Num'))
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my $value := $!value;
        $context.ensure-sc($value);
        my $wval := QAST::WVal.new( :$value );
        QAST::Want.new($wval,'Nn', QAST::NVal.new(:value(nqp::unbox_n($value))))
    }
}

class RakuAST::RatLiteral
  is RakuAST::Literal
{
    has Rat $.value;

    method new(Rat $value) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::RatLiteral, '$!value', $value);
        $obj
    }

    method ast-type {
        RakuAST::Type::Simple.new(RakuAST::Name.from-identifier('Rat'))
    }
}

class RakuAST::ComplexLiteral
  is RakuAST::Literal
{
    has Complex $.value;

    method new(Complex $value) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::ComplexLiteral, '$!value', $value);
        $obj
    }

    method ast-type {
        RakuAST::Type::Simple.new(RakuAST::Name.from-identifier('Complex'))
    }
}

class RakuAST::VersionLiteral
  is RakuAST::Literal
{
    has Version $.value;

    method new($value) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::VersionLiteral, '$!value', $value);
        $obj
    }

    method ast-type {
        RakuAST::Type::Simple.new(RakuAST::Name.from-identifier('Version'))
    }
}

# A StrLiteral is a basic string literal without any kind of interpolation
# taking place. It may be placed in the tree directly, but a compiler will
# typically emit it in a quoted string wrapper.
class RakuAST::StrLiteral
  is RakuAST::Literal
{
    has Str $.value;

    method new(Str $value) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::StrLiteral, '$!value', $value);
        $obj
    }

    method ast-type {
        RakuAST::Type::Simple.new(RakuAST::Name.from-identifier('Str'))
    }

    method set-value(Str $value) {
        nqp::bindattr(self, RakuAST::StrLiteral, '$!value', $value);
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my $value := $!value;
        $context.ensure-sc($value);
        my $wval := QAST::WVal.new( :$value );
        QAST::Want.new($wval,'Ss', QAST::SVal.new(:value(nqp::unbox_s($value))))
    }
}

# A quoted string consists of a sequence of segments that should be evaluated
# (if needed) and concatenated. Processing may be applied to the result (these
# are "words", "quotewords", "val", and "exec", and are applied in the order
# that they are specified here).
class RakuAST::QuotedString
  is RakuAST::ColonPairish
  is RakuAST::Term
  is RakuAST::ImplicitLookups
{
    has Mu $!segments;
    has Mu $!processors;

    method new(List :$segments!, List :$processors) {
        nqp::create(self).SET-SELF($segments, $processors)
    }

    # Allow subclasses to set these attributes transparently
    method SET-SELF(List $segments, List $processors) {
        nqp::bindattr(self, RakuAST::QuotedString, '$!segments',
            self.IMPL-UNWRAP-LIST($segments));
        nqp::bindattr(self, RakuAST::QuotedString, '$!processors',
            self.IMPL-CHECK-PROCESSORS($processors));
        self
    }

    method IMPL-CHECK-PROCESSORS(Mu $processors) {
        my constant VALID := nqp::hash('words', Mu, 'quotewords', Mu, 'val', Mu, 'exec', Mu, 'heredoc', Mu);
        my @result;
        for self.IMPL-UNWRAP-LIST($processors // []) {
            if nqp::existskey(VALID, $_) {
                nqp::push(@result, nqp::box_s($_, Str));
            }
            else {
                nqp::die("Unsupported quoted string processor '$_'");
            }
        }
        @result
    }

    method segments() {
        self.IMPL-WRAP-LIST($!segments)
    }

    method processors() {
        self.IMPL-WRAP-LIST($!processors)
    }

    method canonicalize() {
        self.IMPL-QUOTE-VALUE(self.literal-value // '')
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        my @needed;
        if nqp::elems($!processors) {
            for $!processors {
                if $_ eq 'val' {
                    nqp::push(@needed, RakuAST::Var::Lexical::Setting.new('&val'));
                    last;
                }
            }
        }
        self.IMPL-WRAP-LIST(@needed)
    }

    method has-variables() {
        for $!segments {
            return True if nqp::istype($_,RakuAST::Var);
        }
        False
    }

    method type {
        if $!processors {
            # Can probably figure some of these out.
            Mu
        }
        else {
            # Always a string if no processors.
            Str
        }
    }

    method ast-type {
        if $!processors {
            # Can probably figure some of these out.
            nqp::die('NYI ast-type of non-trivial quoted strings');
        }
        else {
            # Always a string if no processors.
            RakuAST::Type::Simple.new(RakuAST::Name.from-identifier('Str'))
        }
    }

    # Tries to get a literal value for the quoted string. If that is not
    # possible, returns Nil.
    method literal-value() {
        my $base-str;
        if nqp::elems($!segments) == 1 && nqp::istype($!segments[0], RakuAST::StrLiteral) {
            $base-str := $!segments[0].value;
        }
        else {
            my str $base-from-parts := '';
            for $!segments {
                if nqp::istype($_, RakuAST::StrLiteral) {
                    $base-from-parts := $base-from-parts ~ $_.value;
                }
                else {
                    return Nil;
                }
            }
            $base-str := nqp::box_s($base-from-parts, Str);
        }
        my $result := $base-str;
        for $!processors {
            if $_ eq 'words' {
                return Nil unless nqp::istype($result, Str);
                $result := $result.WORDS_AUTODEREF();
            }
            elsif $_ eq 'quotewords' {
                return Nil unless nqp::istype($result, Str);
                #TODO actually implement special handling of « »
                $result := $result.WORDS_AUTODEREF();
            }
            elsif $_ eq 'val' {
                my $val := self.get-implicit-lookups.AT-POS(0).resolution.compile-time-value;
                $result := $val($result);
            }
            elsif $_ eq 'heredoc' {
            }
            else {
                return Nil;
            }
        }
        return $result;
    }

    # Checks if this is an empty words list, as seen in a form like %h<>.
    method is-empty-words() {
        # Is it empty?
        return False if nqp::elems($!segments) >= 2;
        if nqp::elems($!segments) == 1 {
            my $seg := $!segments[0];
            return False unless nqp::istype($seg, RakuAST::StrLiteral) && $seg.value eq '';
        }

        # Yes, but is it quote words?
        for $!processors {
            if $_ eq 'words' {
                return True;
            }
        }
        False
    }

    method dump-extras(int $indent) {
        $!processors && nqp::elems($!processors)
            ?? nqp::x(' ', $indent) ~ 'postprocessors: ' ~ nqp::join(', ', $!processors) ~ "\n"
            !! ''
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        # If we can constant fold it, just produce the constant.
        my $literal-value := self.literal-value;
        if nqp::isconcrete($literal-value) {
            $context.ensure-sc($literal-value);
            my $wval := QAST::WVal.new( :value($literal-value) );
            return nqp::istype($literal-value, Str)
                ?? QAST::Want.new(
                    $wval,
                    'Ss',
                    QAST::SVal.new( :value(nqp::unbox_s($literal-value)) )
                )
                !! $wval;
        }

        # Otherwise, needs compilation.
        my @segment-asts;
        for $!segments {
            if $_.type =:= Str || nqp::istype($_, RakuAST::QuoteWordsAtom) {
                @segment-asts.push($_.IMPL-TO-QAST($context));
            }
            else {
                my $inter-qast := $_.IMPL-TO-QAST($context);
                if nqp::istype($_, RakuAST::Block) {
                    $inter-qast := QAST::Op.new( :op('call'), $inter-qast );
                }
                @segment-asts.push(QAST::Op.new(
                    :op('callmethod'), :name('Str'),
                    $inter-qast
                ));
            }
        }
        my int $seg-count := nqp::elems(@segment-asts);
        my $qast;
        if $seg-count == 1 {
            $qast := @segment-asts[0];
        }
        elsif $seg-count == 2 {
            $qast := QAST::Op.new( :op('concat'), @segment-asts[0], @segment-asts[1] )
        }
        else {
            $qast := QAST::Op.new(
                :op('join'),
                QAST::SVal.new( :value('') ),
                QAST::Op.new( :op('list_s'), |@segment-asts )
            )
        }
        self.IMPL-QAST-PROCESSORS($context, $qast)
    }

    method IMPL-WALK($context, $node, $result) {
        if $node.ann('ww_atom') {
            $result.push($node);
        }
        elsif nqp::istype($node, QAST::Op) && $node.name eq '&infix:<~>' {
            self.IMPL-WALK($context, $node[0], $result);
            self.IMPL-WALK($context, $node[1], $result);
        }
        elsif nqp::istype($node, QAST::Op) && $node.op eq 'concat' {
            self.IMPL-WALK($context, $node[0], $result);
            self.IMPL-WALK($context, $node[1], $result);
        }
        elsif nqp::istype($node, QAST::Op) && $node.op eq 'join' {
            for $node[1].list {
                self.IMPL-WALK($context, $_, $result);
            }
        }
        # (can't just use postprocess_words here because it introduces spurious comma operations)
        elsif $node.has_compile_time_value {
            my @words := HLL::Grammar.split_words(nqp::unbox_s($node.compile_time_value));
            for @words {
                $result.push(RakuAST::StrLiteral.new($_).IMPL-EXPR-QAST($context));
            }
        }
        else {
            $result.push(
                QAST::Op.new(
                    :op('callmethod'),
                    :name('Slip'),
                    QAST::Op.new(
                        :op('callmethod'),
                        :name('WORDS_AUTODEREF'),
                        QAST::Op.new(
                            :op('callmethod'),
                            :name('Stringy'),
                            $node
                        )
                    )
                )
            );
        }
    }

    method IMPL-QAST-PROCESSORS(RakuAST::IMPL::QASTContext $context, Mu $qast) {
        # Non-optimized handling of processors.
        for $!processors {
            if $_ eq 'words' {
                $qast := QAST::Op.new(
                    :op('callmethod'), :name('WORDS_AUTODEREF'), $qast
                );
            }
            elsif $_ eq 'quotewords' {
                my $result := QAST::Op.new( :op('call'), :name('&infix:<,>') );
                self.IMPL-WALK($context, $qast, $result);

                # Strip out list op and possible Slip if only one resulting word
                $qast := nqp::elems($result) == 1
                    ?? nqp::istype($result[0], QAST::Op) && $result[0].name eq 'Slip'
                        ?? $result[0][0]
                        !! $result[0]
                    !! QAST::Stmts.new( $result );
            }
            elsif $_ eq 'val' {
                my $name :=
                  self.get-implicit-lookups.AT-POS(0).resolution.lexical-name;
                $qast := QAST::Op.new(:op('call'), :$name, $qast);
            }
            elsif $_ eq 'exec' {
                $qast := QAST::Op.new(
                    :op('call'), :name('&QX'), $qast
                );
            }
            elsif $_ eq 'heredoc' {
                $qast := QAST::Op.new(
                    :op('die_s'), QAST::SVal.new( :value("Premature heredoc consumption") )
                );
            }
            else {
                nqp::die("Compiling processor '$_' is not implemented");
            }
        }
        $qast
    }

    method visit-children(Code $visitor) {
        my @segments := $!segments;
        for @segments {
            $visitor($_);
        }
    }

    method IMPL-CAN-INTERPRET() {
        nqp::isconcrete(self.literal-value) ?? True !! False
    }

    method IMPL-INTERPRET(RakuAST::IMPL::InterpContext $ctx) {
        self.literal-value
    }
}

class RakuAST::Heredoc
  is RakuAST::QuotedString
{
    has Str $!stop;
    has int $!indent;

    method new(List :$segments!, List :$processors, Str :$stop) {
        my $obj := nqp::create(self).SET-SELF($segments, $processors);
        nqp::bindattr($obj, RakuAST::Heredoc, '$!stop', $stop // '');
        $obj
    }

    method replace-segments-from(RakuAST::QuotedString $source) {
        nqp::bindattr(
            self,
            RakuAST::QuotedString,
            '$!segments',
            nqp::getattr($source, RakuAST::QuotedString, '$!segments')
        );
        Nil
    }

    method steal-processors-from(RakuAST::QuotedString $source) {
        my $processors := nqp::getattr(self, RakuAST::QuotedString, '$!processors');
        for self.IMPL-UNWRAP-LIST($source.processors) {
            nqp::push($processors, $_);
        }
        # Also steal the implicit lookups as any processor related lookups will
        # have been done before we got to stealing those processors.
        nqp::bindattr(
            self,
            RakuAST::ImplicitLookups,
            '$!implicit-lookups-cache',
            nqp::getattr($source, RakuAST::ImplicitLookups, '$!implicit-lookups-cache')
        );
    }

    method set-stop(Str $stop) {
        nqp::bindattr(self, RakuAST::Heredoc, '$!stop', $stop);
    }
    method stop() { $!stop }

    method set-indent(int $indent) {
        nqp::bindattr_i(self, RakuAST::Heredoc, '$!indent', $indent);
    }

    method trim() {
        # Remove heredoc postprocessor to defuse the "Premature heredoc consumption" error
        my $processors := nqp::getattr(self, RakuAST::QuotedString, '$!processors');
        my $new_processors := [];
        nqp::bindattr(self, RakuAST::QuotedString, '$!processors', $new_processors);
        for $processors {
            nqp::push($new_processors, $_) if $_ ne 'heredoc';
        }

        if ($!indent) {
            my int $indent := -$!indent;

            my $in-fresh-line := 1;
            for self.IMPL-UNWRAP-LIST(self.segments) {
                if nqp::istype($_, RakuAST::StrLiteral) && !nqp::istype($_, RakuAST::Heredoc::InterpolatedWhiteSpace) {
                    if $in-fresh-line {
                        $_.set-value($_.value.indent($indent));
                    }
                    else {
                        my $strval := $_.value;
                        if $strval ~~ /\n/ { # we have more lines
                            # add temporary indentation to the front, so every line will be indented
                            my $strbox := nqp::box_s(nqp::x(" ", -$indent) ~ nqp::unbox_s($strval), Str);
                            $in-fresh-line := 1;
                            $_.set-value($strbox.indent($indent));
                        }
                    }
                }
                else {
                    $in-fresh-line := 0;
                }
            }
        }
    }
}

class RakuAST::Heredoc::InterpolatedWhiteSpace is RakuAST::StrLiteral {
}

# An atom in a quote words construct. By wrapping something in this, it is
# considered indivisible in words processing.
class RakuAST::QuoteWordsAtom
  is RakuAST::Node
{
    has RakuAST::Node $.atom;

    method new(RakuAST::Node $atom) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::QuoteWordsAtom, '$!atom', $atom);
        $obj
    }

    method type {
        $!atom.type
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        my $qast := $!atom.IMPL-TO-QAST($context);
        $qast.annotate('ww_atom', 1);
        $qast
    }

    method visit-children(Code $visitor) {
        $visitor($!atom)
    }
}
