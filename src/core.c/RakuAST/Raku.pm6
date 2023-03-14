# This augments the RakuAST::Node class so that all of its subclasses can
# generate sensible .raku output *WITHOUT* having to specify that in the
# RakuAST bootstrap.
# 
# In it, it provides a new ".raku" proto method that will catch any
# unimplemented classes in a sensible way.

augment class RakuAST::Node {
    method EVAL(RakuAST::Node:D: *%opts) {
        use MONKEY-SEE-NO-EVAL;
        EVAL self, context => CALLER::, |%opts
    }

    proto method raku(RakuAST::Node:) {
        CATCH {
            when X::Multi::NoMatch {
                die "No .raku method implemented for {self.^name} objects yet";
            }
        }
        if nqp::istype($*INDENT,Failure) {
            my $*INDENT = "";
            {*}
        }
        else {
            {*}
        }
    }

#-------------------------------------------------------------------------------
# Helper subs

    my $spaces = '  ';

    our sub indent(--> Str:D) {
        $_ = $_ ~ $spaces with $*INDENT;
    }
    
    our sub dedent(--> Str:D) {
        $_ = $_.chomp($spaces) with $*INDENT;
    }

    our sub rakufy($value) {
        if nqp::istype($value,List) && $value -> @elements {
            indent;
            my str $list = @elements.map({ $*INDENT ~ .raku }).join(",\n");
            dedent;
            "(\n$list,\n$*INDENT)"
        }
        else {
            nqp::istype($value,Bool)
              ?? ($value.defined ?? $value ?? "True" !! "False" !! 'Bool')
              !! $value.raku
        }
    }

#-------------------------------------------------------------------------------
# Private helper methods

    method !none() { self.^name ~ '.new' }

    method !literal($value) {
        self.^name ~ '.new(' ~ $value.raku ~ ')';
    }

    method !positional($value) {
        indent;
        my str $raku = $*INDENT ~ $value.raku;
        dedent;
        self.^name ~ ".new(\n$raku\n$*INDENT)"
    }

    method !positionals(@values) {
        if @values {
            indent;
            my str $parts = @values.map({
                $*INDENT ~ rakufy($_)
            }).join(",\n");
            dedent;
            self.^name ~ ".new(\n$parts\n$*INDENT)"
        }
        else {
            self.^name ~ '.new()'
        }
    }

    method !nameds(*@names) {
        sub as-class(str $name, str $raku) {
            my class Rakufy-as { has str $.raku }
            Pair.new($name, Rakufy-as.new(:$raku))
        }

        indent;
        my @pairs = @names.map: -> $method {
            if nqp::istype($method,Pair) {
                $method
            }
            elsif $method eq 'args' && self.args -> $args {
                :$args if $args.args
            }
            elsif $method eq 'scope' {
                my $scope := self.scope;
                :$scope if $scope ne self.default-scope
            }
            elsif $method eq 'how' {
                my $how := self.how;
                as-class('how', $how.^name.subst("Perl6::"))
                  unless nqp::eqaddr($how,self.default-how);
            }
            elsif $method eq 'backtrack' {
                as-class('backtrack', self.backtrack.^name)
            }
            elsif $method eq 'slurpy' {
                my $slurpy := self.slurpy;
                as-class('slurpy', $slurpy.^name)
                  unless nqp::eqaddr($slurpy,RakuAST::Parameter::Slurpy)
            }
            elsif $method eq 'labels' {
                my $labels := nqp::decont(self.labels);
                :$labels if $labels;
            }
            elsif $method eq 'traits' {
                my $traits := nqp::decont(self.traits);
                :$traits if $traits;
            }
            elsif $method eq 'signature' {
                my $signature := self.signature;
                :$signature
                  if $signature
                  && ($signature.parameters.elems || $signature.returns)
            }
            else {
                my $object := self."$method"();
                Pair.new($method, $object) if nqp::isconcrete($object)
            }
        }
        @pairs.append: %_;
        my $format  := "$*INDENT%-@pairs.map(*.key.chars).max()s => %s";
        my str $args = @pairs.map({
            sprintf($format, .key, rakufy(.value))
        }).join(",\n");
        dedent;

        self.^name ~ ($args ?? ".new(\n$args\n$*INDENT)" !! '.new')
    }

#- A ---------------------------------------------------------------------------

    multi method raku(RakuAST::ApplyInfix:D: --> Str:D) {
        self!nameds: <left infix right>
    }

    multi method raku(RakuAST::ApplyDottyInfix:D: --> Str:D) {
        self!nameds: <left infix right>
    }

    multi method raku(RakuAST::ApplyListInfix:D: --> Str:D) {
        my str @parts = "RakuAST::ApplyListInfix.new(";
        indent;
        @parts.push: $*INDENT ~ "infix    => " ~ rakufy(self.infix) ~ ",";
        if self.operands -> @operands {
            @parts.push: $*INDENT ~ "operands => " ~ rakufy(@operands);
        }
        else {
            @parts.push: $*INDENT ~ "operands => ()";
        }
        dedent;
        @parts.push: $*INDENT ~ ")";

        @parts.join("\n")
    }

    multi method raku(RakuAST::ApplyPostfix:D: --> Str:D) {
        self!nameds: <operand postfix>
    }

    multi method raku(RakuAST::ApplyPrefix:D: --> Str:D) {
        self!nameds: <prefix operand>
    }

    multi method raku(RakuAST::ArgList:D: --> Str:D) {
        self!positionals(self.args)
    }

#- B ---------------------------------------------------------------------------

    multi method raku(RakuAST::Block:D: --> Str:D) {
        self!nameds: <body implicit-topic required-topic exception>
    }

    multi method raku(RakuAST::Blockoid:D: --> Str:D) {
        (my $statements := self.statement-list)
          ?? self!positional($statements)
          !! self!none
    }

#- Call ------------------------------------------------------------------------

    multi method raku(RakuAST::Call::MaybeMethod:D: --> Str:D) {
        self!nameds: <name args>
    }

    multi method raku(RakuAST::Call::MetaMethod:D: --> Str:D) {
        self!nameds: <name args>
    }

    multi method raku(RakuAST::Call::Method:D: --> Str:D) {
        self!nameds: <name args>
    }

    multi method raku(RakuAST::Call::PrivateMethod:D: --> Str:D) {
        self!nameds: <name args>
    }

    multi method raku(RakuAST::Call::QuotedMethod:D: --> Str:D) {
        self!nameds: <name args>
    }

    multi method raku(RakuAST::Call::VarMethod:D: --> Str:D) {
        self!nameds: <name args>
    }

    multi method raku(RakuAST::Call::Name:D: --> Str:D) {
        self!nameds: <name args>
    }

    multi method raku(RakuAST::Call::Term:D: --> Str:D) {
        self!nameds: <args>
    }

#- Circumfix -------------------------------------------------------------------

    multi method raku(RakuAST::Circumfix::ArrayComposer:D: --> Str:D) {
        self!positional(self.semilist)
    }

    multi method raku(RakuAST::Circumfix::HashComposer:D: --> Str:D) {
        self!positional(self.expression)
    }

    multi method raku(RakuAST::Circumfix::Parentheses:D: --> Str:D) {
        self!positional(self.semilist)
    }

#- ColonPair -------------------------------------------------------------------

    multi method raku(RakuAST::ColonPair::False:D: --> Str:D) {
        self!nameds: <key>
    }

    multi method raku(RakuAST::ColonPair::Number:D: --> Str:D) {
        self!nameds: <key value>
    }

    multi method raku(RakuAST::ColonPair::True:D: --> Str:D) {
        self!nameds: <key>
    }

    multi method raku(RakuAST::ColonPair::Value:D: --> Str:D) {
        self!nameds: <key value>
    }

    multi method raku(RakuAST::ColonPair::Variable:D: --> Str:D) {
        self!nameds: <key value>
    }

#- Co --------------------------------------------------------------------------

    multi method raku(RakuAST::ComplexLiteral:D: --> Str:D) {
        self!literal(self.value)
    }

    multi method raku(RakuAST::CompUnit:D: :$compunit --> Str:D) {
        # XXX need to handle .finish-content and other arguments
        self!nameds: <statement-list comp-unit-name setting-name>
    }

    multi method raku(RakuAST::Contextualizer:D: --> Str:D) {
        self!positional(self.target)
    }

#- D ---------------------------------------------------------------------------

    multi method raku(RakuAST::Declaration:D: --> Str:D) {
        self!nameds: <scope>
    }

    multi method raku(RakuAST::Declaration::ResolvedConstant:D: --> Str:D) {
        self!literal(self.compile-time-value)
    }

    multi method raku(RakuAST::DottyInfixish:D: --> Str:D) {
        self!none
    }

#- E ---------------------------------------------------------------------------

    multi method raku(RakuAST::Expression:U: --> '') { }

#- F ---------------------------------------------------------------------------

    multi method raku(RakuAST::FatArrow:D: --> Str:D) {
        self!nameds: <key value>
    }

    multi method raku(RakuAST::FunctionInfix:D: --> Str:D) {
        self!positional(self.function)
    }

#- H ---------------------------------------------------------------------------

    multi method raku(RakuAST::Heredoc:D: --> Str:D) {
        self!nameds: <processors segments stop>
    }

#- I ---------------------------------------------------------------------------

    multi method raku(RakuAST::Infix:D: --> Str:D) {
        self!literal(self.operator)
    }

    multi method raku(RakuAST::Initializer::Assign:D: --> Str:D) {
        self!positional(self.expression)
    }

    multi method raku(RakuAST::Initializer::Bind:D: --> Str:D) {
        self!positional(self.expression)
    }

    multi method raku(RakuAST::IntLiteral:D: --> Str:D) {
        self!literal(self.value)
    }

#- L ---------------------------------------------------------------------------

    multi method raku(RakuAST::Label:D: --> Str:D) {
        self!nameds: <name>
    }

#- M ---------------------------------------------------------------------------

    multi method raku(RakuAST::MetaInfix::Assign:D: --> Str:D) {
        self!positional(self.infix)
    }

    multi method raku(RakuAST::MetaInfix::Negate:D: --> Str:D) {
        self!positional(self.infix)
    }

    multi method raku(RakuAST::Method:D: --> Str:D) {
        my str @nameds = <name signature traits body>;
        @nameds.unshift("multiness") if self.multiness;
        @nameds.unshift("scope") if self.scope ne self.default-scope;
        self!nameds: @nameds
    }

#- N ---------------------------------------------------------------------------

    multi method raku(RakuAST::Name:D: --> Str:D) {
        my @parts := self.parts;
        if nqp::istype(@parts.are, RakuAST::Name::Part::Simple) {
            self.^name ~ (@parts.elems == 1
              ?? ".from-identifier(@parts.head.name.raku())"
              !! ".from-identifier-parts(@parts.map(*.name.raku).join(','))"
            )
        }
        else {
            self!positionals(@parts)
        }
    }

    multi method raku(RakuAST::Nqp:D: --> Str:D) {
        my @args := self.args.args;
        @args
          ?? self!positionals( (self.op,|@args) )
          !! self!literal(self.op)
    }

    multi method raku(RakuAST::Nqp::Const:D: --> Str:D) {
        $*INDENT ~ 'RakuAST::Nqp::Const.new(' ~ self.name.raku ~ ')'
    }

    multi method raku(RakuAST::NumLiteral:D: --> Str:D) {
        self!literal(self.value)
    }

#- O ---------------------------------------------------------------------------

    multi method raku(RakuAST::OnlyStar:D: --> Str:D) {
        self!none
    }

#- P ---------------------------------------------------------------------------

    multi method raku(RakuAST::Package:D: --> Str:D) {
        my $signature := self.body.signature;
        my $self := self;
        if self.declarator eq 'role' {
            $self := nqp::clone(self);
            my $statements := self.body.body.statement-list.statements;
            nqp::bindattr($self, RakuAST::Package, '$!body',
              $statements.elems == 1
                ?? RakuAST::Block
                !! RakuAST::Block.new(
                     body => RakuAST::Blockoid.new(
                       RakuAST::StatementList.new(
                         |$statements.head(*-1) # lose fabricated return value
                       )
                     )
                   )
            );
        }

        $self!nameds:
          <scope declarator name parameterization how repr traits body>,
          (parameterization => $signature if $signature.parameters.elems)
    }

    multi method raku(RakuAST::Pragma:D: --> Str:D) {
        self!nameds: <off name argument>
    }

#- Parameter -------------------------------------------------------------------

    multi method raku(RakuAST::Parameter:D: --> Str:D) {
        my str @nameds;
        @nameds.push("type") if self.type && self.type.DEPARSE ne 'Any';
        @nameds.push("names") if self.names.elems;
        @nameds.push("type-captures") if self.type-captures.elems;
        @nameds.append: <
          target optional slurpy traits default where sub-signature value
        >;

        self!nameds: @nameds
    }

    multi method raku(RakuAST::Parameter::Slurpy:U --> Str:D) {
        self.^name
    }

    multi method raku(RakuAST::Parameter::Slurpy::Flattened:U --> Str:D) {
        self.^name
    }

    multi method raku(RakuAST::Parameter::Slurpy::SingleArgument:U --> Str:D) {
        self.^name
    }

    multi method raku(RakuAST::Parameter::Slurpy::Unflattened:U --> Str:D) {
        self.^name
    }

    multi method raku(RakuAST::Parameter::Slurpy::Capture:U --> Str:D) {
        self.^name
    }

    multi method raku(RakuAST::ParameterTarget::Var:D: --> Str:D) {
        self!literal(self.name)
    }

    multi method raku(RakuAST::ParameterTarget::Term:D: --> Str:D) {
        self!positional(self.name)
    }

    multi method raku(RakuAST::ParameterDefaultThunk:D: --> '') { }  # XXX

#- Po --------------------------------------------------------------------------

    multi method raku(RakuAST::PointyBlock:D: --> Str:D) {
        self!nameds: <signature body>
    }

    multi method raku(RakuAST::Postcircumfix::ArrayIndex:D: --> Str:D) {
        self!nameds: <index assignee>
    }

    multi method raku(RakuAST::Postcircumfix::HashIndex:D: --> Str:D) {
        self!positional(self.index)
    }

    multi method raku(RakuAST::Postcircumfix::LiteralHashIndex:D: --> Str:D) {
        self!nameds: <index assignee>
    }

    multi method raku(RakuAST::Postfix:D: --> Str:D) {
        self!literal(self.operator)
    }

    multi method raku(RakuAST::Postfix::Power:D: --> Str:D) {
        self!literal(self.power)
    }

    multi method raku(RakuAST::Prefix:D: --> Str:D) {
        self!literal(self.operator)
    }

#- Q ---------------------------------------------------------------------------

    multi method raku(RakuAST::QuotedRegex:D: --> Str:D) {
        self!nameds: <match-immediately body>
    }

    multi method raku(RakuAST::QuotedString:D: --> Str:D) {
        my str @parts = "RakuAST::QuotedString.new(";
        indent;
        if self.processors -> @processors {
            @parts.push: $*INDENT ~ "processors => <@processors[]>,";
        }
        @parts.push: $*INDENT ~ "segments   => " ~ rakufy(self.segments);
        dedent;
        @parts.push: $*INDENT ~ ")";

        @parts.join("\n")
    }

    multi method raku(RakuAST::QuoteWordsAtom:D: --> Str:D) {
        self!positional(self.atom)
    }

#- R ---------------------------------------------------------------------------

    multi method raku(RakuAST::RatLiteral:D: --> Str:D) {
        self!literal(self.value)
    }

#- Regex -----------------------------------------------------------------------

    multi method raku(RakuAST::Regex::Anchor::BeginningOfString:D: --> Str:D) {
        self!none
    }

    multi method raku(RakuAST::Regex::Anchor::EndOfString:D: --> Str:D) {
        self!none
    }

    multi method raku(RakuAST::Regex::Anchor::BeginningOfLine:D: --> Str:D) {
        self!none
    }

    multi method raku(RakuAST::Regex::Anchor::EndOfLine:D: --> Str:D) {
        self!none
    }

    multi method raku(RakuAST::Regex::Anchor::LeftWordBoundary:D: --> Str:D) {
        self!none
    }

    multi method raku(RakuAST::Regex::Anchor::RightWordBoundary:D: --> Str:D) {
        self!none
    }

    multi method raku(RakuAST::Regex::Literal:D: --> Str:D) {
        self!literal(self.text)
    }

    multi method raku(RakuAST::Regex::Alternation:D: --> Str:D) {
        self!positionals(self.branches)
    }

#- Regex::Assertion ------------------------------------------------------------

    multi method raku(RakuAST::Regex::Assertion::Alias:D: --> Str:D) {
        self!nameds: <name assertion>
    }

    multi method raku(RakuAST::Regex::Assertion::Callable:D: --> Str:D) {
        self!nameds: <callee args>
    }

    multi method raku(RakuAST::Regex::Assertion::CharClass:D: --> Str:D) {
        self!positionals(self.elements)
    }

    multi method raku(RakuAST::Regex::Assertion::Fail:D: --> Str:D) {
        self!none
    }

    multi method raku(
      RakuAST::Regex::Assertion::InterpolatedBlock:D: --> Str:D) {
        self!nameds: <block sequential>
    }

    multi method raku(RakuAST::Regex::Assertion::InterpolatedVar:D: --> Str:D) {
        self!nameds: <sequential var>
    }

    multi method raku(RakuAST::Regex::Assertion::Lookahead:D: --> Str:D) {
        self!nameds: <negated assertion>
    }

    multi method raku(RakuAST::Regex::Assertion::Named:D: --> Str:D) {
        self!nameds: <name capturing>
    }

    multi method raku(RakuAST::Regex::Assertion::Named::Args:D: --> Str:D) {
        self!nameds: <name args capturing>
    }

    multi method raku(RakuAST::Regex::Assertion::Named::RegexArg:D: --> Str:D) {
        self!nameds: <name regex-arg>
    }

    multi method raku(RakuAST::Regex::Assertion::Pass:D: --> Str:D) {
        self!none
    }

    multi method raku(RakuAST::Regex::Assertion::PredicateBlock:D: --> Str:D) {
        self!nameds: <negated block>
    }

    multi method raku(RakuAST::Regex::Assertion::Recurse:D: --> Str:D) {
        self!positional(self.node)
    }

#- Regex::B --------------------------------------------------------------------

    multi method raku(RakuAST::Regex::BackReference::Positional:D: --> Str:D) {
        self!positional(self.index)
    }

    multi method raku(RakuAST::Regex::BackReference::Named:D: --> Str:D) {
        self!positional(self.name)
    }

    multi method raku(RakuAST::Regex::Backtrack:U: --> Str:D) {
        self.^name
    }

    multi method raku(RakuAST::Regex::Backtrack::Frugal:U: --> Str:D) {
        self.^name
    }

    multi method raku(RakuAST::Regex::Backtrack::Greedy:U: --> Str:D) {
        self.^name
    }

    multi method raku(RakuAST::Regex::Backtrack::Ratchet:U: --> Str:D) {
        self.^name
    }

    multi method raku(RakuAST::Regex::BacktrackModifiedAtom:D: --> Str:D) {
        self!nameds: <atom backtrack>
    }

    multi method raku(RakuAST::Regex::Block:D: --> Str:D) {
        self!positional(self.block)
    }

#- Regex::C --------------------------------------------------------------------

    multi method raku(RakuAST::Regex::CapturingGroup:D: --> Str:D) {
        self!positional(self.regex)
    }

#- Regex::Charclass ------------------------------------------------------------

    multi method raku(RakuAST::Regex::CharClass::Any:D: --> Str:D) {
        self!none
    }

    multi method raku(RakuAST::Regex::CharClass::BackSpace:D: --> Str:D) {
        self!nameds: <negated>
    }

    multi method raku(RakuAST::Regex::CharClass::CarriageReturn:D: --> Str:D) {
        self!nameds: <negated>
    }

    multi method raku(RakuAST::Regex::CharClass::Digit:D: --> Str:D) {
        self!nameds: <negated>
    }

    multi method raku(RakuAST::Regex::CharClass::Escape:D: --> Str:D) {
        self!nameds: <negated>
    }

    multi method raku(RakuAST::Regex::CharClass::FormFeed:D: --> Str:D) {
        self!nameds: <negated>
    }

    multi method raku(RakuAST::Regex::CharClass::HorizontalSpace:D: --> Str:D) {
        self!nameds: <negated>
    }

    multi method raku(RakuAST::Regex::CharClass::Newline:D: --> Str:D) {
        self!nameds: <negated>
    }

    multi method raku(RakuAST::Regex::CharClass::Nul:D: --> Str:D) {
        self!none
    }

    multi method raku(RakuAST::Regex::CharClass::Space:D: --> Str:D) {
        self!nameds: <negated>
    }

    multi method raku(RakuAST::Regex::CharClass::Specified:D: --> Str:D) {
        self!nameds: <negated characters>
    }

    multi method raku(RakuAST::Regex::CharClass::Tab:D: --> Str:D) {
        self!nameds: <negated>
    }

    multi method raku(RakuAST::Regex::CharClass::VerticalSpace:D: --> Str:D) {
        self!nameds: <negated>
    }

    multi method raku(RakuAST::Regex::CharClass::Word:D: --> Str:D) {
        self!nameds: <negated>
    }

    multi method raku(
      RakuAST::Regex::CharClassElement::Enumeration:D: --> Str:D) {
        self!nameds: <negated elements>
    }

    multi method raku(RakuAST::Regex::CharClassElement::Property:D: --> Str:D) {
        self!nameds: <negated inverted property predicate>
    }

    multi method raku(RakuAST::Regex::CharClassElement::Rule:D: --> Str:D) {
        self!nameds: <negated name>
    }

    multi method raku(
      RakuAST::Regex::CharClassEnumerationElement::Character:D: --> Str:D) {
        self!positional(self.character)
    }

    multi method raku(
      RakuAST::Regex::CharClassEnumerationElement::Range:D: --> Str:D) {
        self!nameds: <from to>
    }

#- Regex::Co -------------------------------------------------------------------

    multi method raku(RakuAST::Regex::Conjunction:D: --> Str:D) {
        self!positionals(self.branches)
    }

#- Regex::G --------------------------------------------------------------------

    multi method raku(RakuAST::Regex::Group:D: --> Str:D) {
        self!positional(self.regex)
    }

#- Regex::I --------------------------------------------------------------------

    multi method raku(
      RakuAST::Regex::InternalModifier::IgnoreCase:D: --> Str:D) {
        self!nameds: <negated>
    }

    multi method raku(
      RakuAST::Regex::InternalModifier::IgnoreMark:D: --> Str:D) {
        self!nameds: <negated>
    }

    multi method raku(
      RakuAST::Regex::InternalModifier::Ratchet:D: --> Str:D) {
        self!nameds: <negated>
    }

    multi method raku(
      RakuAST::Regex::InternalModifier::Sigspace:D: --> Str:D) {
        self!nameds: <negated>
    }

    multi method raku(RakuAST::Regex::Interpolation:D: --> Str:D) {
        self!nameds: <sequential var>
    }

#- Regex::M --------------------------------------------------------------------

    multi method raku(RakuAST::Regex::MatchFrom:D: --> Str:D) {
        self!none
    }

    multi method raku(RakuAST::Regex::MatchTo:D: --> Str:D) {
        self!none
    }

#- Regex::N --------------------------------------------------------------------

    multi method raku(RakuAST::Regex::NamedCapture:D: --> Str:D) {
        self!nameds: <name regex>
    }

#- Regex::Q --------------------------------------------------------------------

    multi method raku(RakuAST::Regex::QuantifiedAtom:D: --> Str:D) {
        self!nameds: <atom quantifier separator trailing-separator>
    }

    multi method raku(RakuAST::Regex::Quantifier::BlockRange:D: --> Str:D) {
        self!nameds: <block backtrack>
    }

    multi method raku(RakuAST::Regex::Quantifier::OneOrMore:D: --> Str:D) {
        self!nameds: <backtrack>
    }

    multi method raku(RakuAST::Regex::Quantifier::Range:D: --> Str:D) {
        self!nameds: <min excludes-min max excludes-max backtrack>
    }

    multi method raku(RakuAST::Regex::Quantifier::ZeroOrMore:D: --> Str:D) {
        self!nameds: <backtrack>
    }

    multi method raku(RakuAST::Regex::Quantifier::ZeroOrOne:D: --> Str:D) {
        self!nameds: <backtrack>
    }

    multi method raku(RakuAST::Regex::Quote:D: --> Str:D) {
        self!positional(self.quoted)
    }

#- Regex::S --------------------------------------------------------------------

    multi method raku(RakuAST::Regex::Sequence:D: --> Str:D) {
        self!positionals(self.terms)
    }

    multi method raku(RakuAST::Regex::SequentialAlternation:D: --> Str:D) {
        self!positionals(self.branches)
    }

    multi method raku(RakuAST::Regex::SequentialConjunction:D: --> Str:D) {
        self!positionals(self.branches)
    }

    multi method raku(RakuAST::Regex::Statement:D: --> Str:D) {
        self!positional(self.statement)
    }

#- Regex::W --------------------------------------------------------------------

    multi method raku(RakuAST::Regex::WithSigspace:D: --> Str:D) {
        self!positional(self.regex)
    }

#- S ---------------------------------------------------------------------------

    multi method raku(RakuAST::SemiList:D: --> Str:D) {
        self!positionals(self.statements)
    }

    multi method raku(RakuAST::Signature:D: --> Str:D) {
        self!nameds: <parameters returns>
    }

#- Statement -------------------------------------------------------------------

    multi method raku(RakuAST::Statement::Catch:D: --> Str:D) {
        self!nameds: <labels body>
    }

    multi method raku(RakuAST::Statement::Control:D: --> Str:D) {
        self!nameds: <labels body>
    }

    multi method raku(RakuAST::Statement::Default:D: --> Str:D) {
        self!nameds: <labels body>
    }

    multi method raku(RakuAST::Statement::Empty:D: --> Str:D) {
        self!nameds: <labels>
    }

    multi method raku(RakuAST::Statement::Expression:D: --> Str:D) {
        self!nameds: <labels expression condition-modifier loop-modifier>
    }

    multi method raku(RakuAST::Statement::For:D: --> Str:D) {
        self!nameds: <labels source body mode>
    }

    multi method raku(RakuAST::Statement::Given:D: --> Str:D) {
        self!nameds: <labels source body>
    }

    multi method raku(RakuAST::Statement::If:D: --> Str:D) {
        self!nameds: <labels condition then elsifs else>
    }

    multi method raku(RakuAST::Statement::Loop:D: --> Str:D) {
        self!nameds: <labels setup condition increment body>
    }

    multi method raku(RakuAST::Statement::Loop::RepeatUntil:D: --> Str:D) {
        self!nameds: <labels body condition>
    }

    multi method raku(RakuAST::Statement::Loop::RepeatWhile:D: --> Str:D) {
        self!nameds: <labels body condition>
    }

    multi method raku(RakuAST::Statement::Loop::Until:D: --> Str:D) {
        self!nameds: <labels condition body>
    }

    multi method raku(RakuAST::Statement::Loop::While:D: --> Str:D) {
        self!nameds: <labels condition body>
    }

    multi method raku(RakuAST::Statement::Require:D: --> Str:D) {
        self!nameds: <labels module-name>
    }

    multi method raku(RakuAST::Statement::Unless:D: --> Str:D) {
        self!nameds: <labels condition body>
    }

    multi method raku(RakuAST::Statement::Use:D: --> Str:D) {
        self!nameds: <labels module-name argument>
    }

    multi method raku(RakuAST::Statement::When:D: --> Str:D) {
        self!nameds: <labels condition body>
    }

    multi method raku(RakuAST::Statement::Without:D: --> Str:D) {
        self!nameds: <labels condition body>
    }

    multi method raku(RakuAST::StatementList:D: --> Str:D) {
        self!positionals(self.statements)
    }

#- Statement::Modifier ---------------------------------------------------------

    multi method raku(RakuAST::StatementModifier::Given:D: --> Str:D) {
        self!positional(self.expression)
    }

    multi method raku(RakuAST::StatementModifier::If:D: --> Str:D) {
        self!positional(self.expression)
    }

    multi method raku( RakuAST::StatementModifier::For:D: --> Str:D) {
        self!positional(self.expression)
    }

    multi method raku(RakuAST::StatementModifier::For::Thunk:D: --> Str:D) {
        self!none
    }

    multi method raku(RakuAST::StatementModifier::Unless:D: --> Str:D) {
        self!positional(self.expression)
    }

    multi method raku(RakuAST::StatementModifier::Until:D: --> Str:D) {
        self!positional(self.expression)
    }

    multi method raku(RakuAST::StatementModifier::When:D: --> Str:D) {
        self!positional(self.expression)
    }

    multi method raku(RakuAST::StatementModifier::While:D: --> Str:D) {
        self!positional(self.expression)
    }

    multi method raku(RakuAST::StatementModifier::With:D: --> Str:D) {
        self!positional(self.expression)
    }

    multi method raku(RakuAST::StatementModifier::Without:D: --> Str:D) {
        self!positional(self.expression)
    }

#- Statement::Prefix -----------------------------------------------------------

    multi method raku(RakuAST::StatementPrefix::Do:D: --> Str:D) {
        self!positional(self.blorst)
    }

    multi method raku(RakuAST::StatementPrefix::Eager:D: --> Str:D) {
        self!positional(self.blorst)
    }

    multi method raku(RakuAST::StatementPrefix::Gather:D: --> Str:D) {
        self!positional(self.blorst)
    }

    multi method raku(RakuAST::StatementPrefix::Hyper:D: --> Str:D) {
        self!positional(self.blorst)
    }

    multi method raku(RakuAST::StatementPrefix::Lazy:D: --> Str:D) {
        self!positional(self.blorst)
    }

    multi method raku(RakuAST::StatementPrefix::Phaser::Begin:D: --> Str:D) {
        self!positional(self.blorst)
    }

    multi method raku(RakuAST::StatementPrefix::Phaser::Close:D: --> Str:D) {
        self!positional(self.blorst)
    }

    multi method raku(RakuAST::StatementPrefix::Phaser::End:D: --> Str:D) {
        self!positional(self.blorst)
    }

    multi method raku(RakuAST::StatementPrefix::Phaser::First:D: --> Str:D) {
        self!positional(self.blorst)
    }

    multi method raku(RakuAST::StatementPrefix::Phaser::Last:D: --> Str:D) {
        self!positional(self.blorst)
    }

    multi method raku(RakuAST::StatementPrefix::Phaser::Enter:D: --> Str:D) {
        self!positional(self.blorst)
    }

    multi method raku(RakuAST::StatementPrefix::Phaser::Init:D: --> Str:D) {
        self!positional(self.blorst)
    }

    multi method raku(RakuAST::StatementPrefix::Phaser::Keep:D: --> Str:D) {
        self!positional(self.blorst)
    }

    multi method raku(RakuAST::StatementPrefix::Phaser::Leave:D: --> Str:D) {
        self!positional(self.blorst)
    }

    multi method raku(RakuAST::StatementPrefix::Phaser::Next:D: --> Str:D) {
        self!positional(self.blorst)
    }

    multi method raku(RakuAST::StatementPrefix::Phaser::Post:D: --> Str:D) {
        # skip the auto-generated code
        self!positional(
          self.blorst.body
            .statement-list.statements.head.condition-modifier.expression
        )
    }

    multi method raku(RakuAST::StatementPrefix::Phaser::Pre:D: --> Str:D) {
        # skip the auto-generated code
        self!positional(self.blorst.condition-modifier.expression)
    }

    multi method raku(RakuAST::StatementPrefix::Phaser::Quit:D: --> Str:D) {
        self!positional(self.blorst)
    }

    multi method raku(RakuAST::StatementPrefix::Phaser::Undo:D: --> Str:D) {
        self!positional(self.blorst)
    }

    multi method raku(RakuAST::StatementPrefix::Quietly:D: --> Str:D) {
        self!positional(self.blorst)
    }

    multi method raku(RakuAST::StatementPrefix::Race:D: --> Str:D) {
        self!positional(self.blorst)
    }

    multi method raku(RakuAST::StatementPrefix::Start:D: --> Str:D) {
        self!positional(self.blorst)
    }

    multi method raku(RakuAST::StatementPrefix::Try:D: --> Str:D) {
        self!positional(self.blorst)
    }

#- Str -------------------------------------------------------------------------

    multi method raku(RakuAST::StrLiteral:D: --> Str:D) {
        self!literal(self.value)
    }

#- Stu -------------------------------------------------------------------------

    multi method raku(RakuAST::Stub:D: --> Str:D) {
        self!nameds: <args>
    }

#- Su --------------------------------------------------------------------------

    multi method raku(RakuAST::Sub:D: --> Str:D) {
        my str @nameds = <name signature traits body>;
        @nameds.unshift("multiness") if self.multiness;
        @nameds.unshift("scope") if self.scope ne self.default-scope;
        self!nameds: @nameds
    }

    multi method raku(RakuAST::Submethod:D: --> Str:D) {
        self!nameds: <name signature traits body>
    }

    multi method raku(RakuAST::Substitution:D: --> Str:D) {
        self!nameds: <immutable samespace adverbs infix pattern replacement>
    }

    multi method raku(RakuAST::SubstitutionReplacementThunk:D: --> Str:D) {
        self!positional(self.infix)
    }

#- Term ------------------------------------------------------------------------

    multi method raku(RakuAST::Term::Capture:D: --> Str:D) {
        self!positional(self.source)
    }

    multi method raku(RakuAST::Term::EmptySet:D: --> Str:D) {
        self!none
    }

    multi method raku(RakuAST::Term::HyperWhatever:D: --> Str:D) {
        self!none
    }

    multi method raku(RakuAST::Term::Name:D: --> Str:D) {
        self!positional(self.name)
    }

    multi method raku(RakuAST::Term::Named:D: --> Str:D) {
        self!literal(self.name)
    }

    multi method raku(RakuAST::Term::Rand:D: --> Str:D) {
        self!none
    }

    multi method raku(RakuAST::Term::RadixNumber:D: --> Str:D) {
        self!nameds: <radix multi-part value>
    }

    multi method raku(RakuAST::Term::Reduce:D: --> Str:D) {
        self!nameds: <triangle infix args>
    }

    multi method raku(RakuAST::Term::Self:D: --> Str:D) {
        self!none
    }

    multi method raku(RakuAST::Term::TopicCall:D: --> Str:D) {
        self!positional(self.call)
    }

    multi method raku(RakuAST::Term::Whatever:D: --> Str:D) {
        self!none
    }

#- Ternary ---------------------------------------------------------------------

    multi method raku(RakuAST::Ternary:D: --> Str:D) {
        self!nameds: <condition then else>
    }

#- Trait -----------------------------------------------------------------------

    multi method raku(RakuAST::Trait::Is:D: --> Str:D) {
        self!nameds: <name argument>
    }

    multi method raku(RakuAST::Trait::Hides:D: --> Str:D) {
        self!positional(self.type)
    }

    multi method raku(RakuAST::Trait::Does:D: --> Str:D) {
        self!positional(self.type)
    }

    multi method raku(RakuAST::Trait::Of:D: --> Str:D) {
        self!positional(self.type)
    }

    multi method raku(RakuAST::Trait::Returns:D: --> Str:D) {
        self!positional(self.type)
    }

#- Type ------------------------------------------------------------------------

    multi method raku(RakuAST::Type::Coercion:D: --> Str:D) {
        self!nameds: (try self.constraint.name.canonicalize eq 'Any')
          ?? <base-type>
          !! <base-type constraint>
    }

    multi method raku(RakuAST::Type::Definedness:D: --> Str:D) {
        self!nameds: <base-type definite>
    }

    multi method raku(RakuAST::Type::Enum:D: --> Str:D) {
        self!nameds: <scope name term of>
    }

    multi method raku(RakuAST::Type::Parameterized:D: --> Str:D) {
        self!nameds: <base-type args>
    }

    multi method raku(RakuAST::Type::Setting:D: --> Str:D) {
        self!positional(self.name)
    }

    multi method raku(RakuAST::Type::Simple:D: --> Str:D) {
        self!positional(self.name)
    }

    multi method raku(RakuAST::Type::Subset:D: --> Str:D) {
        self!nameds: <scope name of where traits>
    }

#- Var -------------------------------------------------------------------------

    multi method raku(RakuAST::Var::Attribute:D: --> Str:D) {
        self!positional(self.name)
    }

    multi method raku(RakuAST::Var::Compiler::File:D: --> Str:D) {
        self!positional(self.file)
    }

    multi method raku(RakuAST::Var::Compiler::Line:D: --> Str:D) {
        self!positional(self.line)
    }

    multi method raku(RakuAST::Var::Compiler::Lookup:D: --> Str:D) {
        self!positional(self.name)
    }

    multi method raku(RakuAST::Var::Dynamic:D: --> Str:D) {
        self!positional(self.name)
    }

    multi method raku(RakuAST::Var::Lexical:D: --> Str:D) {
        my $name := self.name;
        self!literal($name.starts-with('$whatevercode_arg_') ?? '*' !! $name)
    }

    multi method raku(RakuAST::Var::NamedCapture:D: --> Str:D) {
        self!positional(self.index)
    }

    multi method raku(RakuAST::Var::Package:D: --> Str:D) {
        self!nameds: <name sigil>
    }

    multi method raku(RakuAST::Var::PositionalCapture:D: --> Str:D) {
        self!positional(self.index)
    }

#- VarDeclaration --------------------------------------------------------------

    multi method raku(RakuAST::VarDeclaration::Anonymous:D: --> Str:D) {
        self!nameds: <scope sigil type initializer>
    }

    multi method raku(RakuAST::VarDeclaration::Constant:D: --> Str:D) {
        self!nameds: <scope type name traits initializer>
    }

    multi method raku(RakuAST::VarDeclaration::Implicit:D: --> Str:D) {
        self!nameds: <scope name>
    }

    multi method raku(
      RakuAST::VarDeclaration::Implicit::Constant:D: --> Str:D) {
        self!nameds: <scope name value>
    }

    multi method raku(
      RakuAST::VarDeclaration::Placeholder::Named:D: --> Str:D) {
        self!positional(self.lexical-name)
    }

    multi method raku(
      RakuAST::VarDeclaration::Placeholder::Positional:D: --> Str:D) {
        self!positional(self.lexical-name)
    }

    multi method raku(
      RakuAST::VarDeclaration::Placeholder::SlurpyArray:D: --> Str:D) {
        self!none
    }

    multi method raku(
      RakuAST::VarDeclaration::Placeholder::SlurpyHash:D: --> Str:D) {
        self!none
    }

    multi method raku(RakuAST::VarDeclaration::Simple:D: --> Str:D) {
        self!nameds: <scope type shape name initializer>
    }

    multi method raku(RakuAST::VarDeclaration::Term:D: --> Str:D) {
        self!nameds: <scope type name initializer>
    }

#- Version ---------------------------------------------------------------------

    multi method raku(RakuAST::VersionLiteral:D: --> Str:D) {
        self!positional(self.value)
    }
}

#-------------------------------------------------------------------------------
# The RakuAST::Name::Part tree is *not* descendent from RakuAST::Node and
# as such needs separate handling to prevent it from bleeding into the normal
# .raku handling

augment class RakuAST::Name::Part {
    proto method raku(RakuAST::Name::Part:) {
        CATCH {
            when X::Multi::NoMatch {
                die "No .raku method implemented for {self.^name} objects yet";
            }
        }
        if nqp::istype($*INDENT,Failure) {
            my $*INDENT = "";
            {*}
        }
        else {
            {*}
        }
    }

#- Name::Part-------------------------------------------------------------------

    multi method raku(RakuAST::Name::Part::Empty:U: --> Str:D) {
        self.^name
    }

    multi method raku(RakuAST::Name::Part::Expression:D: --> Str:D) {
        my str @parts = self.^name ~ '.new(';
        RakuAST::Node::indent();
        @parts.push: $*INDENT ~ self.expr.raku;
        RakuAST::Node::dedent();
        @parts.push: $*INDENT ~ ")";

        @parts.join("\n")
    }

    multi method raku(RakuAST::Name::Part::Simple:D: --> Str:D) {
        self.^name ~ '.new(' ~ self.name.raku ~ ')';
    }
}

#-------------------------------------------------------------------------------
# The RakuAST::Statement::Elsif tree is *not* descendent from RakuAST::Node
# and as such needs separate handling to prevent it from bleeding into the
# normal # .raku handling

augment class RakuAST::Statement::Elsif {
    multi method raku(RakuAST::Statement::Elsif:D: --> Str:D) {
        my str @parts = self.^name ~ '.new(';
        RakuAST::Node::indent();
        @parts.push: $*INDENT ~ 'condition => ' ~ self.condition.raku ~ ",";
        @parts.push: $*INDENT ~ 'then      => ' ~ self.then.raku;
        RakuAST::Node::dedent();
        @parts.push: $*INDENT ~ ")";

        @parts.join("\n")
    }
}

# vim: expandtab shiftwidth=4
