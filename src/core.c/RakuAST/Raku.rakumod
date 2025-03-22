# This augments the RakuAST::Node class so that all of its subclasses can
# generate sensible .raku output *WITHOUT* having to specify that in the
# RakuAST bootstrap.
#
# In it, it provides a new ".raku" proto method that will catch any
# unimplemented classes in a sensible way.

augment class RakuAST::Node {

    # Allow calling .EVAL on any RakuAST::Node
    method EVAL(RakuAST::Node:D: *%opts) {
        use MONKEY-SEE-NO-EVAL;
        EVAL self, context => CALLER::LEXICAL::, |%opts
    }

    proto method raku(RakuAST::Node:) {
        CATCH {
            when X::Multi::NoMatch | X::Multi::Ambiguous {
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
        $_ = $_ ~ $spaces given $*INDENT
    }

    our sub dedent(--> Str:D) {
        $_ = .chomp($spaces) given $*INDENT
    }

    our sub rakufy($value) {
        if nqp::istype($value,List) && $value -> @elements {
            if nqp::istype(@elements.are,Int) {
                "(@elements.join(',')" ~ (@elements == 1 ?? ',)' !! ')')
            }
            else {
                indent;
                my str $list = @elements.map({ $*INDENT ~ .raku }).join(",\n");
                dedent;
                "(\n$list,\n$*INDENT)"
            }
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
        self.^name ~ '.new(' ~ nqp::decont($value).raku ~ ')';
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

        my $special := BEGIN nqp::hash(
          'abbreviated', -> {
              :abbreviated if self.abbreviated && !self.directive
          },
          'adverbs', -> {
              my $adverbs := nqp::decont(self.adverbs);
              :$adverbs if $adverbs
          },
          'args', -> {
              my $args := self.args;
              :$args if $args && $args.args
          },
          'atoms', -> {
              my $atoms := nqp::decont(self.atoms);
              :$atoms if $atoms
          },
          'backtrack', -> {
              my $backtrack := self.backtrack;
              as-class('backtrack', $backtrack.^name)
                unless nqp::eqaddr($backtrack,RakuAST::Regex::Backtrack)
          },
          'capturing', -> {
              :capturing if self.capturing
          },
          'cells', -> {
              if self.cells -> @cells is copy {
                  :@cells
              }
          },
          'colonpairs', -> {
              if self.colonpairs -> @colonpairs is copy {
                  :@colonpairs
              }
          },
          'column-dividers', -> {
              if self.column-dividers -> $column-dividers {
                  :$column-dividers
              }
          },
          'column-offsets', -> {
              if self.column-offsets -> @column-offsets is copy {
                  :@column-offsets
              }
          },
          'config', -> {
              my $config := nqp::decont(self.config);
              :config($config.Hash) if $config
          },
          'directive', -> {
              :directive if self.directive
          },
          'dispatch', -> {
              my $dispatch := self.dispatch;
              :dispatch($dispatch) if $dispatch;
          },
          'dwim-left', -> {
              :dwim-left if self.dwim-left
          },
          'dwim-right', -> {
              :dwim-right if self.dwim-right
          },
          'elsifs', -> {
              my $elsifs := nqp::decont(self.elsifs);
              :$elsifs if $elsifs
          },
          'excludes-max', -> {
              :excludes-max if self.excludes-max
          },
          'excludes-min', -> {
              :excludes-min if self.excludes-min
          },
          'for', -> {
              :for if self.for
          },
          'forced-dynamic', -> {
              :forced-dynamic if self.forced-dynamic
          },
          'how', -> {
              my $how := self.how;
              as-class('how', $how.^name.subst("Perl6::"))
                unless nqp::eqaddr($how,self.default-how)
          },
          'implicit-topic', -> {
              :implicit-topic if self.implicit-topic
          },
          'inverted', -> {
              :inverted if self.inverted
          },
          'labels', -> {
              my $labels := nqp::decont(self.labels);
              :$labels if $labels
          },
          'leading', -> {
              my $leading := nqp::decont(self.leading);
              :$leading if $leading
          },
          'level', -> {
              my $level := self.level;
              :$level if $level
          },
          'margin', -> {
              my $margin := self.margin;
              :$margin if $margin
          },
          'match-immediately', -> {
              :match-immediately if self.match-immediately
          },
          'meta', -> {
              my $meta := self.meta;
              :$meta if $meta
          },
          'module-names', -> {
              my $module-names := nqp::decont(self.module-names);
              :$module-names if $module-names
          },
          'negated', -> {
              :negated if self.negated
          },
          'off', -> {
              :off if self.off
          },
          'paragraphs', -> {
              my @paragraphs := self.paragraphs;
              :@paragraphs if @paragraphs.elems
          },
          'processors', -> {
              my @processors := self.processors;
              :@processors if @processors.elems
          },
          'scope', -> {
              my $scope := self.scope;
              :$scope if $scope ne self.default-scope
          },
          'separator', -> {
              my $separator := self.separator;
              :$separator if $separator
          },
          'sigil', -> {
              my $sigil := self.sigil;
              :$sigil if $sigil
          },
          'signature', -> {
              my $signature := self.signature;
              :$signature
                if $signature
                && ($signature.parameters.elems || $signature.returns)
          },
          'slurpy', -> {
              my $slurpy := self.slurpy;
              as-class('slurpy', $slurpy.^name)
                unless nqp::eqaddr($slurpy,RakuAST::Parameter::Slurpy)
          },
          'through-pragma', -> {
              :through-pragma if self.through-pragma
          },
          'trailing', -> {
              my $trailing := nqp::decont(self.trailing);
              :$trailing if $trailing
          },
          'trailing-separator', -> {
              :trailing-separator if self.trailing-separator
          },
          'traits', -> {
              my $traits := nqp::decont(self.traits);
              :$traits if $traits
          },
          'twigil', -> {
              my $twigil := self.twigil;
              :$twigil if $twigil
          },
          'original-type', -> {
              my $type := self.original-type;
              :$type if $type
          }
        );

        indent;
        my @pairs = @names.map: -> $method {
            if nqp::istype($method,Pair) {
                $method
            }
            elsif nqp::atkey($special,$method) -> &handle {
                handle()
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

    method !add-WHY($raku) {
        (my $WHY := self.WHY)
          ?? $raku ~ $WHY.raku(:declarator-docs)
          !! $raku
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

    multi method raku(RakuAST::Assignment:D: --> Str:D) {
        self.item ?? self!literal(Pair.new("item",True)) !! self!none
    }

#- B ---------------------------------------------------------------------------

    multi method raku(RakuAST::Block:D: --> Str:D) {
        self!add-WHY: self!nameds:
          <implicit-topic required-topic exception body>
    }

    multi method raku(RakuAST::Blockoid:D: --> Str:D) {
        (my $statements := self.statement-list)
          ?? self!positional($statements)
          !! self!none
    }

#- Call ------------------------------------------------------------------------

    # Generic RakuAST::Call::xxx handler
    multi method raku(RakuAST::Call:D: --> Str:D) {
        self!nameds: <name args>
    }

    multi method raku(RakuAST::Call::BlockMethod:D: --> Str:D) {
        self!nameds: <block args dispatch>
    }

    multi method raku(RakuAST::Call::Methodish:D: --> Str:D) {
        self!nameds: <name args dispatch>
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

    multi method raku(RakuAST::ColonPairs:D: --> Str:D) {
        self!positionals(self.colonpairs)
    }

    multi method raku(RakuAST::ColonPair::False:D: --> Str:D) {
        self!literal(self.key)
    }

    multi method raku(RakuAST::ColonPair::Number:D: --> Str:D) {
        self!nameds: <key value>
    }

    multi method raku(RakuAST::ColonPair::True:D: --> Str:D) {
        self!literal(self.key)
    }

    multi method raku(RakuAST::ColonPair::Value:D: --> Str:D) {
        self!nameds: <key value>
    }

    multi method raku(RakuAST::ColonPair::Variable:D: --> Str:D) {
        self!nameds: <key value>
    }

#- Co --------------------------------------------------------------------------

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

#- Doc -------------------------------------------------------------------------

    multi method raku(RakuAST::Doc::Block:D: --> Str:D) {
        self!nameds:
          <margin type level directive for abbreviated config paragraphs>
    }

    multi method raku(RakuAST::Doc::Declarator:D: --> Str:D) {
        self!nameds: <WHEREFORE leading trailing>
    }
    multi method raku(
      RakuAST::Doc::Declarator:D: :$declarator-docs!
    --> Str:D) {
        self!nameds(<leading trailing>).subst(
          'RakuAST::Doc::Declarator.new(',
          '.declarator-docs('
        )
    }

    multi method raku(RakuAST::Doc::Markup:D: --> Str:D) {
        self!nameds: <letter opener closer atoms meta>
    }

    multi method raku(RakuAST::Doc::Paragraph:D: --> Str:D) {
        self!positionals(self.atoms)
    }

#- Dot -------------------------------------------------------------------------

    # Handles all of the RakuAST::DottyInfix::xxx classes
    multi method raku(RakuAST::DottyInfixish:D: --> Str:D) {
        self!none
    }

#- E ---------------------------------------------------------------------------

    multi method raku(RakuAST::Expression:U: --> '') { }

#- F ---------------------------------------------------------------------------

    multi method raku(RakuAST::FakeSignature:D: --> Str:D) {
        self!positional(self.signature)
    }

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

    # Also for ::FlipFlop
    multi method raku(RakuAST::Infix:D: --> Str:D) {
        self!literal(self.operator)
    }

    multi method raku(RakuAST::Initializer::Assign:D: --> Str:D) {
        self!positional(self.expression)
    }

    multi method raku(RakuAST::Initializer::Bind:D: --> Str:D) {
        self!positional(self.expression)
    }

    multi method raku(RakuAST::Initializer::CallAssign:D: --> Str:D) {
        self!positional(self.postfixish)
    }

#- L ---------------------------------------------------------------------------

    multi method raku(RakuAST::Label:D: --> Str:D) {
        self!nameds: <name>
    }

    # handles all RakuAST::xxxLiteral classes
    multi method raku(RakuAST::Literal:D: --> Str:D) {
        self!literal(self.value)
    }

#- M ---------------------------------------------------------------------------

    # Generic handling of all other RakuAST::MetaInfix::xxx classes
    multi method raku(RakuAST::MetaInfix:D: --> Str:D) {
        self!positional(self.infix)
    }

    multi method raku(RakuAST::MetaInfix::Hyper:D: --> Str:D) {
        self!nameds: <dwim-left infix dwim-right>
    }

    multi method raku(RakuAST::MetaPostfix::Hyper:D: --> Str:D) {
        self!positional(self.postfix)
    }

    multi method raku(RakuAST::Method:D: --> Str:D) {
        my str @nameds = 'name';
        @nameds.unshift("private")   if self.private;
        @nameds.unshift("meta")      if self.meta;
        @nameds.unshift("multiness") if self.multiness;
        @nameds.unshift("scope") if self.scope ne self.default-scope;
        @nameds.push("signature") if self.signature && self.signature.parameters-initialized;
        @nameds.append: <traits body>;

        self!add-WHY(self!nameds(@nameds))
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
                ?? RakuAST::RoleBody
                !! RakuAST::RoleBody.new(
                     body => RakuAST::Blockoid.new(
                       RakuAST::StatementList.new(
                         # lose fabricated values
                         |$statements.skip.head(*-1)
                       )
                     )
                   )
            );
        }

        self!add-WHY: $self!nameds:
          <scope name how repr traits body>,
          (parameterization => $signature
            if $signature && $signature.parameters.elems)
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

        self!add-WHY: self!nameds: @nameds;
    }

    # Generic handler for all RakuAST::Parameter::Slurpy::xxx classes
    multi method raku(RakuAST::Parameter::Slurpy:U: --> Str:D) {
        self.^name
    }

    multi method raku(RakuAST::ParameterTarget::Var:D: --> Str:D) {
        self!nameds: <name>
    }

    multi method raku(RakuAST::ParameterTarget::Term:D: --> Str:D) {
        self!positional(self.name)
    }

    multi method raku(RakuAST::ParameterDefaultThunk:D: --> '') { }  # XXX

#- Po --------------------------------------------------------------------------

    multi method raku(RakuAST::PointyBlock:D: --> Str:D) {
        self!add-WHY: self!nameds: self.signature && self.signature.parameters-initialized
          ?? <signature body>
          !! <body>
    }

    multi method raku(RakuAST::Postcircumfix::ArrayIndex:D: --> Str:D) {
        self!nameds: <index assignee colonpairs>
    }

    multi method raku(RakuAST::Postcircumfix::HashIndex:D: --> Str:D) {
        self!nameds: <index colonpairs>
    }

    multi method raku(RakuAST::Postcircumfix::LiteralHashIndex:D: --> Str:D) {
        self!nameds: <index assignee colonpairs>
    }

    multi method raku(RakuAST::Postfix:D: --> Str:D) {
        self!nameds: <operator colonpairs>
    }

    multi method raku(RakuAST::Postfix::Power:D: --> Str:D) {
        self!literal(self.power)
    }

    multi method raku(RakuAST::Postfix::Vulgar:D: --> Str:D) {
        self!literal(self.vulgar)
    }

    multi method raku(RakuAST::Prefix:D: --> Str:D) {
        self!literal(self.operator)
    }

#- Q ---------------------------------------------------------------------------

    multi method raku(RakuAST::QuotedRegex:D: --> Str:D) {
        self!nameds: <match-immediately body adverbs>
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

#- Regex -----------------------------------------------------------------------

    # Generic handling of all RakuAST::Regex::Anchor::xxx classes
    multi method raku(RakuAST::Regex::Anchor:D: --> Str:D) {
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

    # Generix handling of all RakuAST::Regex::Backtrack::xxx classes
    multi method raku(RakuAST::Regex::Backtrack:U: --> Str:D) {
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

    # Generic handler for most RakuAST::Regex::CharClass::xxx classes
    multi method raku(RakuAST::Regex::CharClass:D: --> Str:D) {
        self!nameds: <negated>
    }

    multi method raku(RakuAST::Regex::CharClass::Any:D: --> Str:D) {
        self!none
    }

    multi method raku(RakuAST::Regex::CharClass::Nul:D: --> Str:D) {
        self!none
    }

    multi method raku(RakuAST::Regex::CharClass::Specified:D: --> Str:D) {
        self!nameds: <negated characters>
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

#- Regex::D --------------------------------------------------------------------

    multi method raku(RakuAST::RegexDeclaration:D: --> Str:D) {
        my str @nameds = 'name';
        @nameds.unshift("scope") if self.scope ne self.default-scope;
        @nameds.push("signature") if self.signature && self.signature.parameters-initialized;
        @nameds.append: <traits body>;

        self!add-WHY: self!nameds: @nameds;
    }

#- Regex::G --------------------------------------------------------------------

    multi method raku(RakuAST::Regex::Group:D: --> Str:D) {
        self!positional(self.regex)
    }

#- Regex::I --------------------------------------------------------------------

    # Generic handler for all RakuAST::Regex::InternalModifier::xxx classes
    multi method raku( RakuAST::Regex::InternalModifier:D: --> Str:D) {
        self!nameds: <modifier negated>
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

    multi method raku(RakuAST::Regex::WithWhitespace:D: --> Str:D) {
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
        self!nameds: <labels mode source body>
    }

    multi method raku(RakuAST::Statement::Given:D: --> Str:D) {
        self!nameds: <labels source body>
    }

    # Handling both ::If and ::With
    multi method raku(RakuAST::Statement::IfWith:D: --> Str:D) {
        self!nameds: <labels condition then elsifs else>
    }

    multi method raku(RakuAST::Statement::Import:D: --> Str:D) {
        self!nameds: <labels module-name argument>
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

    multi method raku(RakuAST::Statement::Need:D: --> Str:D) {
        self!nameds: <labels module-names>
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

    multi method raku(RakuAST::Statement::Whenever:D: --> Str:D) {
        self!nameds: <labels trigger body>
    }

    multi method raku(RakuAST::Statement::Without:D: --> Str:D) {
        self!nameds: <labels condition body>
    }

    multi method raku(RakuAST::StatementList:D: --> Str:D) {
        self!positionals(self.statements)
    }

#- Statement::Modifier ---------------------------------------------------------

    # Generic handler for most RakuAST::StatementModifier::xxx classes
    multi method raku(RakuAST::StatementModifier:D: --> Str:D) {
        self!positional(self.expression)
    }

    multi method raku(RakuAST::StatementModifier::For::Thunk:D: --> Str:D) {
        self!none
    }

#- Statement::Prefix -----------------------------------------------------------

    # Generic handler for most RakuAST::StatementPrefix::xxx classes
    multi method raku(RakuAST::StatementPrefix:D: --> Str:D) {
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

     multi method raku(RakuAST::StatementPrefix::Phaser::First:D: --> Str:D) {
        # skip the auto-generated code
        self!positional(self.original-blorst)
    }

#- Stu -------------------------------------------------------------------------

    multi method raku(RakuAST::Stub:D: --> Str:D) {
        self!nameds: <args>
    }

#- Su --------------------------------------------------------------------------

    multi method raku(RakuAST::Sub:D: --> Str:D) {
        my str @nameds = 'name';
        @nameds.unshift("multiness") if self.multiness;
        @nameds.unshift("scope") if self.scope ne self.default-scope;
        @nameds.push("signature") if self.signature && self.signature.parameters-initialized;
        @nameds.append: <traits body>;

        self!add-WHY: self!nameds: @nameds;
    }

    multi method raku(RakuAST::Submethod:D: --> Str:D) {
        my str @nameds = 'name';
        @nameds.push("signature") if self.signature && self.signature.parameters-initialized;
        @nameds.append: <traits body>;

        self!add-WHY: self!nameds: @nameds;
    }

    multi method raku(RakuAST::Substitution:D: --> Str:D) {
        self!nameds: <immutable samespace adverbs infix pattern replacement>
    }

    multi method raku(RakuAST::SubstitutionReplacementThunk:D: --> Str:D) {
        self!positional(self.infix)
    }

#- Term ------------------------------------------------------------------------

    # Generic handler for some RakuAST::Term::xxx classes
    multi method raku(RakuAST::Term:D: --> Str:D) {
        self!none
    }

    multi method raku(RakuAST::Term::Capture:D: --> Str:D) {
        self!positional(self.source)
    }

    multi method raku(RakuAST::Term::Name:D: --> Str:D) {
        self!positional(self.name)
    }

    multi method raku(RakuAST::Term::Named:D: --> Str:D) {
        self!literal(self.name)
    }

    multi method raku(RakuAST::Term::RadixNumber:D: --> Str:D) {
        self!nameds: <radix multi-part value>
    }

    multi method raku(RakuAST::Term::Reduce:D: --> Str:D) {
        self!nameds: <triangle infix args>
    }

    multi method raku(RakuAST::Term::TopicCall:D: --> Str:D) {
        self!positional(self.call)
    }

#- Ternary ---------------------------------------------------------------------

    multi method raku(RakuAST::Ternary:D: --> Str:D) {
        self!nameds: <condition then else>
    }

#- Trait -----------------------------------------------------------------------

    multi method raku(RakuAST::Trait::Handles:D: --> Str:D) {
        self!positional(self.term)
    }

    multi method raku(RakuAST::Trait::Is:D: --> Str:D) {
        self!nameds: <name argument type>
    }

    # Generic handler for the RakuAST::Trait::Type classes
    multi method raku(RakuAST::Trait::Type:D: --> Str:D) {
        self!positional(self.type)
    }

    multi method raku(RakuAST::Trait::Will:D: --> Str:D) {
        self!nameds: <type expr>
    }

    multi method raku(RakuAST::Trait::WillBuild:D: --> Str:D) {
        self!positional(self.expr)
    }

#- Type ------------------------------------------------------------------------

    multi method raku(RakuAST::Type::Capture:D: --> Str:D) {
        self!positional(self.name)
    }

    multi method raku(RakuAST::Type::Coercion:D: --> Str:D) {
        self!nameds: (try self.constraint.name.canonicalize eq 'Any')
          ?? <base-type>
          !! <base-type constraint>
    }

    multi method raku(RakuAST::Type::Definedness:D: --> Str:D) {
        self!nameds: <base-type definite through-pragma>
    }

    multi method raku(RakuAST::Type::Enum:D: --> Str:D) {
        self!add-WHY: self.clean-clone!nameds: <scope name of traits term>
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
        self!add-WHY: self!nameds: <scope name of where traits>
    }

#- Var -------------------------------------------------------------------------

    multi method raku(RakuAST::Var::Attribute:D: --> Str:D) {
        self!positional(self.name)
    }

    multi method raku(RakuAST::Var::Attribute::Public:D: --> Str:D) {
        self!nameds: <name>
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

    multi method raku(RakuAST::Var::Doc:D: --> Str:D) {
        self!positional(self.name)
    }

    multi method raku(RakuAST::Var::PositionalCapture:D: --> Str:D) {
        self!literal(self.index)
    }

#- VarDeclaration --------------------------------------------------------------

    multi method raku(RakuAST::VarDeclaration::Anonymous:D: --> Str:D) {
        self!nameds: <scope sigil type initializer>
    }

    multi method raku(RakuAST::VarDeclaration::Auto:D: --> Str:D) {
        RakuAST::Var::Lexical.new(self.name).raku
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

    # handles SlurpyArray / SlurpyHash
    multi method raku(
      RakuAST::VarDeclaration::Placeholder::Slurpy:D: --> Str:D) {
        self!none
    }

    multi method raku(RakuAST::VarDeclaration::Signature:D: --> Str:D) {
        self!nameds: <signature scope type initializer>
    }

    multi method raku(RakuAST::VarDeclaration::Simple:D: --> Str:D) {
        self!add-WHY:
          self!nameds:
            <scope original-type shape sigil twigil desigilname traits initializer where>
    }

    multi method raku(RakuAST::VarDeclaration::Term:D: --> Str:D) {
        self!nameds: <scope type name initializer>
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
