# Marker for all compile-time literals
class RakuAST::Literal
  is RakuAST::Term
  is RakuAST::CheckTime
  is RakuAST::CompileTimeValue
{
    has Str $!typename;
    has Mu  $.value;

    method new(Mu $value) {
        nqp::die("Please use RakuAST::Literal.from-value()")
          if nqp::eqaddr(self,RakuAST::Literal);

        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Literal, '$!value',    $value);
        nqp::bindattr($obj, RakuAST::Literal, '$!typename', nqp::null);
        $obj
    }

    # Attempt to convert given value to a RakuAST::xxxLiteral object,
    # or return Mu if failed
    method from-value(Mu $value) {
        my $typename  := $value.HOW.name($value);
        my $classname := $typename ~ 'Literal';
        my $obj := nqp::create(nqp::isconcrete($value)
          && nqp::existskey(RakuAST.WHO,$classname)
          ?? RakuAST.WHO{$classname}
          !! RakuAST::Literal
        );
        nqp::bindattr($obj, RakuAST::Literal, '$!value',    $value);
        nqp::bindattr($obj, RakuAST::Literal, '$!typename', $typename);
        $obj
    }

    method set-value(Mu $value) {
        nqp::bindattr(self, RakuAST::Literal, '$!value', $value);
    }

    method expression() { self }
    method return-type() { $!value.WHAT }
    method native-type-flag() { Nil }
    method compile-time-value() { $!value }
    method IMPL-CAN-INTERPRET() { True }
    method IMPL-INTERPRET(RakuAST::IMPL::InterpContext $ctx) { $!value }
    method type-name() { 'value' }

    method PERFORM-CHECK(
               RakuAST::Resolver $resolver,
      RakuAST::IMPL::QASTContext $context
    ) {
        self.add-worry: $resolver.build-exception: 'X::AdHoc',
            payload => 'Useless use of constant ' ~ self.type-name ~ ' ' ~ $!value.gist ~ ' in sink context'
            if self.sunk;
    }

    method ast-type {
        my $type := RakuAST::Type::Simple.new(
          RakuAST::Name.from-identifier(
            nqp::ifnull(
              $!typename,
              nqp::bindattr(self,RakuAST::Literal,'$!typename',
                $!value.HOW.name($!value))
            )
          )
        );
        $type.set-resolution:
            RakuAST::Declaration::ResolvedConstant.new(
                compile-time-value => $!value.WHAT
            );
        $type
    }

    # default for non int/str/num/bool literals
    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my $value := $!value;
        $context.ensure-sc($value);
        QAST::WVal.new( :$value )
    }
}

# Base class for slangs wanting to codegen a constant value
class RakuAST::Constant
  is RakuAST::Literal
{
    method deparse() { self.value.raku }
}

class RakuAST::IntLiteral
  is RakuAST::Literal
{
    method native-type-flag() { 1 }

    method type-name() { 'integer' }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my $value := self.value;
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
    method native-type-flag() { 2 }

    method type-name() { 'floating-point number' }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my $value := self.value;
        $context.ensure-sc($value);
        my $wval := QAST::WVal.new( :$value );
        QAST::Want.new($wval,'Nn', QAST::NVal.new(:value(nqp::unbox_n($value))))
    }
}

class RakuAST::RatLiteral
  is RakuAST::Literal
{ }

class RakuAST::ComplexLiteral
  is RakuAST::Literal
{ }

class RakuAST::VersionLiteral
  is RakuAST::Literal
{ }

# A StrLiteral is a basic string literal without any kind of interpolation
# taking place. It may be placed in the tree directly, but a compiler will
# typically emit it in a quoted string wrapper.
class RakuAST::StrLiteral
  is RakuAST::Literal
{
    method native-type-flag() { 3 }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my $value := self.value;
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
  is RakuAST::CheckTime
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
        my constant VALID := nqp::hash('words', Mu, 'quotewords', Mu, 'val', Mu, 'exec', Mu, 'heredoc', Mu, 'format', Mu);
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
        self.IMPL-QUOTE-VALUE(self.literal-value(:force) // '')
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        my @needed;
        if nqp::elems($!processors) {
            for $!processors {
                if $_ eq 'val' {
                    nqp::push(@needed, RakuAST::Var::Lexical::Setting.new(
                        :sigil<&>,
                        :desigilname(RakuAST::Name.from-identifier('val'))));
                    last;
                }
                elsif $_ eq 'format' {
                    nqp::push(@needed, RakuAST::Var::Lexical::Setting.new(
                        :desigilname(RakuAST::Name.from-identifier('Format'))));
                    last;
                }
            }
        }
        self.IMPL-WRAP-LIST(@needed)
    }

    method type-name() { 'string' }

    method PERFORM-CHECK(
               RakuAST::Resolver $resolver,
      RakuAST::IMPL::QASTContext $context
    ) {
        my $value := self.literal-value;
        self.add-worry: $resolver.build-exception: 'X::AdHoc',
            payload => 'Useless use of constant ' ~ self.type-name ~ ' ' ~ $value.gist ~ ' in sink context'
            if self.sunk && $value;
    }

    method has-variables() {
        for $!segments {
            return True if nqp::istype($_,RakuAST::Var);
        }
        False
    }

    method return-type {
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
            my $type := RakuAST::Type::Simple.new(RakuAST::Name.from-identifier('Str'));
            $type.set-resolution:
                RakuAST::Declaration::ResolvedConstant.new(
                    compile-time-value => Str
                );
            $type
        }
    }

    method IMPL-WORDS-AUTODEREF(str $str) {
        my $result := nqp::list();
        my int $pos := 0;
        my int $eos := nqp::chars($str);
        my int $ws;
        my $nbsp := nqp::hash(
            "\x00A0", True,
            "\x2007", True,
            "\x202F", True,
            "\xFEFF", True,
        );
        while ($pos := nqp::findnotcclass(nqp::const::CCLASS_WHITESPACE, $str, $pos, $eos)) < $eos {
            # Search for another white space character as long as we hit non-breakable spaces.
            $ws := $pos;
            $ws++ while nqp::existskey($nbsp,
                nqp::substr($str, $ws := nqp::findcclass(nqp::const::CCLASS_WHITESPACE,
                    $str, $ws, $eos), 1));
            nqp::push($result, nqp::substr($str, $pos, $ws - $pos));
            $pos := $ws;
        }
        $result
    }

    method IMPL-PROCESS-PART($result, $part) {
        return Nil if $part =:= Nil;
        for $!processors {
            if $_ eq 'words' {
                return Nil unless nqp::istype($part, Str);
                my @parts;
                for self.IMPL-WORDS-AUTODEREF($part) {
                    nqp::push(@parts, $_);
                }
                $part := @parts;
            }
            elsif $_ eq 'quotewords' {
                return Nil unless nqp::istype($part, Str);
                #TODO actually implement special handling of « »
                my @parts;
                for self.IMPL-WORDS-AUTODEREF($part) {
                    nqp::push(@parts, $_);
                }
                $part := @parts;
            }
            elsif $_ eq 'val' {
                my $val := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[0].resolution.compile-time-value;
                $part := $val(nqp::hllizefor($part, 'Raku'));
            }
            elsif $_ eq 'heredoc' {
            }
            elsif $_ eq 'format' {
            }
            else {
                return Nil;
            }
        }
        if nqp::islist($part) {
            for $part {
                nqp::push($result, $_);
            }
        }
        elsif nqp::istype($part, List) {
            for self.IMPL-UNWRAP-LIST($part) {
                nqp::push($result, $_);
            }
        }
        else {
            nqp::push($result, $part);
        }
        True
    }

    # Tries to get a literal value for the quoted string. If that is not
    # possible, returns Nil.
    method literal-value(:$force) {
        my @parts;
        for $!segments {
            if nqp::istype($_, RakuAST::StrLiteral) {
                self.IMPL-PROCESS-PART(@parts, $_.value) || return Nil;
            }
            elsif nqp::istype($_, RakuAST::QuoteWordsAtom)
                && nqp::istype($_.atom, RakuAST::QuotedString)
                && my $nested-str := $_.atom.literal-value
            {
                return Nil if $nested-str =:= Nil;

                if nqp::istype($nested-str, Str) {
                    nqp::push(@parts, $nested-str);
                }
                elsif nqp::istype($nested-str, List) {
                    for $nested-str.FLATTENABLE_LIST {
                        nqp::push(@parts, $_);
                    }
                }
            }
            elsif nqp::istype($_, RakuAST::Var::Lexical)
                && $_.is-resolved
                && ($force || nqp::istype($_.resolution, RakuAST::VarDeclaration::Constant))
            {
                self.IMPL-PROCESS-PART(@parts, $_.resolution.compile-time-value.Str) || return Nil;
            }
            elsif $force && nqp::istype($_, RakuAST::Block) && $_.body.IMPL-CAN-INTERPRET {
                nqp::push(@parts, ~$_.body.IMPL-INTERPRET(RakuAST::IMPL::InterpContext.new));
            }
            else {
                return Nil;
            }
        }

        my int $return-list;
        for $!processors {
            if $_ eq 'words' || $_ eq 'quotewords' {
                $return-list := 1;
            }
            elsif $_ eq 'val' && nqp::elems(@parts) == 1 {
                $return-list := 0;
            }
        }
        return $return-list
            ?? nqp::hllizefor(@parts, 'Raku')
            !! nqp::elems(@parts) == 1
                ?? @parts[0] # need to preserve val() result
                !! nqp::box_s(nqp::join('', @parts), Str);
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

            # format string
            if $!processors && $!processors[0] eq 'format' {
                my $Format := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[0].resolution.compile-time-value;

# The below code "should" work, but doesn't because references to internal
# subs (such as "str-right-justified") appear to be QASTed correctly, but
# when executed, do *not* call the unit in question, but simply return the
# arguments that were given.  Not sure what is going on here.  For now,
# create the format at run-time: it will still get cached, but *will* incur
# a performance penalty at runtime, which is too bad because the whole idea
# of a quote string format, was to create all of this at compile time.
#
#                my $format := $Format.new($literal-value);
#                $context.ensure-sc($format);
#                return QAST::WVal.new(:value($format));

                # for now, until above fixed
                $context.ensure-sc($literal-value);
                return QAST::Op.new(
                  :op('callmethod'), :name('new'),
                  QAST::WVal.new( :value($Format) ),
                  QAST::WVal.new( :value($literal-value) )
                );
            }

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
            if $_.return-type =:= Str || nqp::istype($_, RakuAST::QuoteWordsAtom) {
                @segment-asts.push($_.IMPL-TO-QAST($context));
            }
            else {
                my $inter-qast := $_.IMPL-TO-QAST($context);
                if nqp::istype($_, RakuAST::Block) {
                    $inter-qast := QAST::Op.new( :op('call'), $inter-qast );
                }
                @segment-asts.push(QAST::Op.new(
                    :op('callmethod'), :name('Stringy'),
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
                  self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[0].resolution.lexical-name;
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
            elsif $_ eq 'format' {
                my $Format := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups)[0].resolution.compile-time-value;
                $qast := QAST::Op.new(
                  :op('callmethod'), :name('new'),
                  QAST::WVal.new( :value($Format)),
                  $qast
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

    method IMPL-IS-CONSTANT() {
        nqp::isconcrete(self.literal-value) ?? True !! False
    }

    method has-compile-time-value() {
        nqp::isconcrete(self.literal-value) ?? True !! False
    }

    method maybe-compile-time-value() {
        self.literal-value
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

    method return-type {
        $!atom.return-type
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
