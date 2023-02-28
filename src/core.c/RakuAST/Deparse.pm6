# This is the default class handling deparsing (aka, converting a given
# RakuAST::Node object into Raku source code).
#
# It is supposed to be subclassed to provide customization and further
# optimizations (although optimizations should probably live here).
#
# All methods are class methods, so do not require any type of instantiation.
#
# The "deparse" multi method expects an instance if a subclass of a
# RakuAST::Node as the first positional parameter.  All other publick methods
# are used to provide some standard functionality used by the "deparse" methods.

class RakuAST::Deparse {

#-------------------------------------------------------------------------------
# These methods are effectively constants that can be overridden by a
# subclass.

    method before-comma(--> ' ') { }
    method after-comma( --> ' ') { }

    method parens-open( --> '(') { }
    method parens-close(--> ')') { }

    method square-open( --> '[') { }
    method square-close(--> ']') { }

    method reduce-open(    --> '[')   { }
    method reduce-triangle(--> '[\\') { }
    method reduce-close(   --> '] ')  { }

    method bracket-open( --> '{') { }
    method bracket-close(--> '}') { }

    method pointy-open( --> '<') { }
    method pointy-close(--> '>') { }

    method double-pointy-open( --> '<<') { }
    method double-pointy-close(--> '>>') { }

    method block-open( --> "\{\n") { }
    method block-close(--> "\}\n") { }

    method regex-open(                  --> '/ ')   { }
    method regex-close(                 --> ' /')   { }
    method regex-alternation(           --> ' | ')  { }
    method regex-sequential-alternation(--> ' || ') { }
    method regex-conjunction(           --> ' & ')  { }
    method regex-sequential-conjunction(--> ' && ') { }

    method regex-any(                --> '.')  { }
    method regex-beginning-of-string(--> '^')  { }
    method regex-end-of-string(      --> '$')  { }
    method regex-beginning-of-line(  --> '^^') { }
    method regex-end-of-line(        --> '$$') { }
    method regex-left-word-boundary( --> '<<') { }
    method regex-right-word-boundary(--> '>>') { }

    method regex-assertion-pass(--> '<?>') { }
    method regex-assertion-fail(--> '<!>') { }

    method regex-backtrack-frugal( --> '?')  { }
    method regex-backtrack-ratchet(--> ':')  { }
    method regex-backtrack-greedy( --> ':!') { }

    method regex-match-from(--> '<(') { }
    method regex-match-to(  --> ')>') { }

    method before-infix(--> ' ')  { }
    method after-infix( --> ' ')  { }

    method list-infix-comma(     --> ', ') { }
    method list-infix-semi-colon(--> '; ') { }

    method dotty-infix-call(       --> ' .')   { }
    method dotty-infix-call-assign(--> ' .= ') { }

    method function-infix-open( --> '[') { }
    method function-infix-close(--> ']') { }

    method slurpy-flattened(      --> '*') { }
    method slurpy-single-argument(--> '+') { }
    method slurpy-unflattened(   --> '**') { }
    method slurpy-capture(       --> '|')  { }

    method term-hyperwhatever(--> '**')   { }
    method term-rand(         --> 'rand') { }
    method term-empty-set(    --> '∅')    { }
    method term-self(         --> 'self') { }
    method term-whatever(     --> '*')     { }

    method var-compiler-file(--> '$?FILE') { }
    method var-compiler-line(--> '$?LINE') { }

    method assign(--> ' = ')  { }
    method bind(  --> ' := ') { }

    method before-list-infix(--> '') { }
    method after-list-infix(--> ' ') { }

    method loop-separator(--> '; ') { }

    method pointy-sig(     --> '-> ')   { }
    method pointy-return(  --> ' --> ') { }
    method fatarrow(       --> ' => ')  { }
    method end-statement(  --> ";\n")   { }
    method last-statement( --> "\n")    { }

    method indent-with(  --> '    ') { }

    method ternary1(--> ' ?? ') { }
    method ternary2(--> ' !! ') { }

#-------------------------------------------------------------------------------
# Setting up the deparse method

    proto method deparse(|) {
        if nqp::istype($*INDENT,Failure) {
            my $*INDENT = "";
            {*}
        }
        else {
            {*}
        }
    }

    # Base class catcher
    multi method deparse(RakuAST::Node:D $ast) {
        NYI("Deparsing $ast.^name() objects").throw
    }

    # Odd value catcher, avoiding long dispatch options in error message
    multi method deparse(Mu:D $ast) {
        die "You cannot deparse a $ast.^name() instance: $ast.raku()";
    }
    multi method deparse(Mu:U $ast) {
        die "You cannot deparse a $ast.^name() type object";
    }

    # helper method for deparsing contextualizers, sadly no private multis yet
    proto method context-target(|) is implementation-detail {*}
    multi method context-target(RakuAST::StatementSequence $target --> str) {
        self!parenthesize($target)
    }
    multi method context-target($target --> str) {
        self.deparse($target)
    }

#-------------------------------------------------------------------------------
# Private helper methods

    method !indent(--> Str:D) {
        $_ = $_ ~ $.indent-with with $*INDENT;
    }

    method !dedent(--> Str:D) {
        $_ = $_.chomp($.indent-with) with $*INDENT;
    }

    method !routine(RakuAST::Routine:D $ast --> Str:D) {
        my str @parts = '';

        if $ast.name -> $name {
            @parts.push(self.deparse($name));
        }

        @parts.push(self!parenthesize($ast.signature))
          unless $ast.has-placeholder-parameters;

        if $ast.traits -> @traits {
            @parts.push(self.deparse($_)) for @traits;
        }

        @parts.push(self.deparse($ast.body));

        @parts.join(' ')
    }

    method !method(RakuAST::Method:D $ast, str $type --> Str:D) {
        my str @parts;

        if $ast.scope ne 'has' {
            @parts.push($ast.scope);
            @parts.push(' ');
        }
        @parts.push($type);
        @parts.push(self!routine($ast));

        @parts.join
    }

    method !conditional($self: $ast, str $type --> Str:D) {
        "$type $self.deparse($ast.condition) $self.deparse($ast.then)$.last-statement"
    }

    method !negated-conditional($self: $ast, str $type --> Str:D) {
        "$type $self.deparse($ast.condition) $self.deparse($ast.body)$.last-statement"
    }

    method !simple-loop($self: $ast, str $type --> Str:D) {
        "$type $self.deparse($ast.condition) $self.deparse($ast.body)$.last-statement"
    }

    method !simple-repeat($self: $ast, str $type --> Str:D) {
       "repeat $self.deparse($ast.body).chomp() $type $self.deparse($ast.condition)$.last-statement"
    }

    method !assemble-quoted-string($ast --> Str:D) {
        $ast.segments.map({
            nqp::istype($_,RakuAST::StrLiteral)
              ?? .value.raku.substr(1,*-1)
              !! nqp::istype($_,RakuAST::Block)
                ?? self.deparse($_).chomp
                !! self.deparse($_)
            }).join
    }

    my constant %processor-attribute =
      'exec',       ':x',
      'quotewords', ':ww',
      'val',        ':v',
      'words',      ':w',
      'heredoc',    ':to',
    ;

    my constant %single-processor-prefix =
      'exec',       'qx/',
      'quotewords', 'qqww/',
      'val',        'qq:v/',
      'words',      'qqw/',
    ;

    method !multiple-processors(str $string, @processors --> Str:D) {
        "qq@processors.map({
            %processor-attribute{$_} // NYI("String processors '$_'")
        }).join()/$string/"
    }

    method !branches(RakuAST::Regex::Branching:D $ast, str $joiner --> Str:D) {
        if $ast.branches -> @branches {
            @branches.map({ self.deparse($_) }).join($joiner)
        }
        else {
            ''
        }
    }

    method !quantifier(
      RakuAST::Regex::Quantifier:D $ast, str $quantifier
    --> Str:D) {
        $quantifier ~ self.deparse($ast.backtrack)
    }

    method !parenthesize($ast --> Str:D) {
        $.parens-open
          ~ ($ast.defined ?? self.deparse($ast).chomp !! '')
          ~ $.parens-close
    }

    method !bracketize($ast --> Str:D) {
        $.bracket-open
          ~ ($ast.defined ?? self.deparse($ast) !! '')
          ~ $.bracket-close
    }

    method !squarize($ast --> Str:D) {
        $.square-open
          ~ ($ast.defined ?? self.deparse($ast) !! '')
          ~ $.square-close
    }

    method !typish-trait($ast --> Str:D) {
        $ast.IMPL-TRAIT-NAME ~ ' ' ~ self.deparse($ast.type)
    }

    method !method-call($ast, str $dot, $macroish? --> Str:D) {
        my $name := (nqp::istype($_,Str) ?? $_ !! self.deparse($_))
          with $ast.name;

        $dot ~ $name ~ ($macroish ?? '' !! self!parenthesize($ast.args))
    }

    method !quote-if-needed(str $literal) {
        my int $find = nqp::findnotcclass(
          nqp::const::CCLASS_WORD,$literal,0,nqp::chars($literal)
        );
        $find == nqp::chars($literal)
          ?? $literal       # just word chars
          !! $literal.raku  # need quoting
    }

    method !labels(RakuAST::Statement:D $ast) {
        $ast.labels.map({ self.deparse($_) }).join
    }

    method !use-no(str $what, $ast) {
        my str @parts = $what, self.deparse($ast.module-name);

        if $ast.argument -> $argument {
            @parts.push(' ');
            @parts.push(self.deparse($argument));
        }

        self!labels($ast) ~ @parts.join
    }

#- A ---------------------------------------------------------------------------

    multi method deparse(RakuAST::ApplyInfix:D $ast --> Str:D) {
        self.deparse($ast.left)
          ~ $.before-infix
          ~ self.deparse($ast.infix)
          ~ $.after-infix
          ~ self.deparse($ast.right)
    }

    multi method deparse(RakuAST::ApplyDottyInfix:D $ast --> Str:D) {
        self.deparse($ast.left)
          ~ self.deparse($ast.infix)
          # lose the ".", as it is provided by the infix
          ~ self.deparse($ast.right).substr(1)
    }

    multi method deparse(RakuAST::ApplyListInfix:D $ast --> Str:D) {
        my str $operator = $ast.infix.operator;
        my str @parts    = $ast.operands.map({ self.deparse($_) });

        @parts
          ?? $operator eq ','
            ?? @parts == 1
              ?? @parts.head ~ $.list-infix-comma.chomp
              !! @parts.join($.list-infix-comma)
            !! @parts.join(
                 $.before-list-infix ~ $operator ~ $.after-list-infix
               )
          !! ''
    }

    multi method deparse(RakuAST::ApplyPostfix:D $ast --> Str:D) {
        ($ast.on-topic ?? "" !! self.deparse($ast.operand).chomp)
          ~ self.deparse($ast.postfix)
    }

    multi method deparse(RakuAST::ApplyPrefix:D $ast --> Str:D) {
        self.deparse($ast.prefix) ~ self.deparse($ast.operand)
    }

    multi method deparse(RakuAST::ArgList:D $ast --> Str:D) {
        $ast.args.map({ self.deparse($_) }).join($.list-infix-comma)
    }

#- B ---------------------------------------------------------------------------

    multi method deparse(RakuAST::Block:D $ast --> Str:D) {
        self.deparse($ast.body)
    }

    multi method deparse(RakuAST::Blockoid:D $ast --> Str:D) {
        my $statement-list := $ast.statement-list;
        my $statements := $statement-list.statements;

        if $statements.elems > 1 {
            self!indent;
            $.block-open
              ~ self.deparse($statement-list)
              ~ self!dedent
              ~ $.block-close
        }
        else {
            my str @parts = $.bracket-open;
            @parts.push(self.deparse($statements.head).chomp)
              if $statements.elems;
            @parts.push($.bracket-close);

            @parts.join(' ')
        }
    }

#- Call ------------------------------------------------------------------------

    multi method deparse(RakuAST::Call::MaybeMethod:D $ast --> Str:D) {
        self!method-call($ast, '.?')
    }

    multi method deparse(RakuAST::Call::MetaMethod:D $ast --> Str:D) {
        self!method-call($ast, '.^')
    }

    multi method deparse(RakuAST::Call::Method:D $ast --> Str:D) {
        self!method-call($ast, '.', $ast.macroish)
    }

    multi method deparse(RakuAST::Call::PrivateMethod:D $ast --> Str:D) {
        self!method-call($ast, '!')
    }

    multi method deparse(RakuAST::Call::QuotedMethod:D $ast --> Str:D) {
        self!method-call($ast, '.')
    }

    multi method deparse(RakuAST::Call::VarMethod:D $ast --> Str:D) {
        self!method-call($ast, '.&')
    }

    multi method deparse(RakuAST::Call::Name:D $ast --> Str:D) {
        self.deparse($ast.name) ~ self!parenthesize($ast.args)
    }

    multi method deparse(RakuAST::Call::Term:D $ast --> Str:D) {
        self!parenthesize($ast.args)
    }

#- Circumfix -------------------------------------------------------------------

    multi method deparse(RakuAST::Circumfix::ArrayComposer:D $ast --> Str:D) {
        self!squarize($ast.semilist)
    }

    multi method deparse(RakuAST::Circumfix::HashComposer:D $ast --> Str:D) {
        self!bracketize($ast.expression)
    }

    multi method deparse(RakuAST::Circumfix::Parentheses:D $ast --> Str:D) {
        self!parenthesize($ast.semilist)
    }

#- ColonPair -------------------------------------------------------------------

    multi method deparse(RakuAST::ColonPair:D $ast --> Str:D) {
        ':'
          ~ $ast.named-arg-name
          ~ $.parens-open
          ~ self.deparse($ast.named-arg-value)
          ~ $.parens-close
    }

    multi method deparse(RakuAST::ColonPair::False:D $ast --> Str:D) {
        ':!' ~ $ast.named-arg-name
    }

    multi method deparse(RakuAST::ColonPair::Number:D $ast --> Str:D) {
        ':' ~ self.deparse($ast.value) ~ $ast.named-arg-name
    }

    multi method deparse(RakuAST::ColonPair::True:D $ast --> Str:D) {
        ':' ~ $ast.named-arg-name
    }

    multi method deparse(RakuAST::ColonPair::Value:D $ast --> Str:D) {
        my $value := $ast.value;

        ':' ~ $ast.named-arg-name ~ (
          nqp::istype($value,RakuAST::QuotedString)
            ?? self.deparse($value)
            !! $.parens-open ~ self.deparse($value) ~ $.parens-close
        )
    }

    multi method deparse(RakuAST::ColonPair::Variable:D $ast --> Str:D) {
        ':' ~ self.deparse($ast.value)
    }

#- Co --------------------------------------------------------------------------

    multi method deparse(RakuAST::ComplexLiteral:D $ast --> Str:D) {
        $ast.value.raku
    }

    multi method deparse(RakuAST::CompUnit:D $ast --> Str:D) {
        my str $deparsed = self.deparse($ast.statement-list);
        with $ast.finish-content {
            $deparsed ~="\n=finish\n$_";
        }
        else {
            $deparsed
        }
    }

    multi method deparse(RakuAST::Contextualizer:D $ast --> Str:D) {
        $ast.sigil ~ self.context-target($ast.target)
    }

#- D ---------------------------------------------------------------------------

    multi method deparse(RakuAST::Declaration:D $ast --> Str:D) {
        $ast.scope
    }

    multi method deparse(RakuAST::DottyInfix::Call:D $ --> Str:D) {
        $.dotty-infix-call
    }

    multi method deparse(RakuAST::DottyInfix::CallAssign:D $ --> Str:D) {
        $.dotty-infix-call-assign
    }

#- F ---------------------------------------------------------------------------

    multi method deparse(RakuAST::FatArrow:D $ast --> Str:D) {
        $ast.key ~ $.fatarrow ~ self.deparse($ast.value)
    }

    multi method deparse(RakuAST::FunctionInfix:D $ast --> Str:D) {
        $.function-infix-open
          ~ self.deparse($ast.function)
          ~ $.function-infix-close
    }

#- H ---------------------------------------------------------------------------

    multi method deparse(RakuAST::Heredoc:D $ast --> Str:D) {
        my $string := self!assemble-quoted-string($ast);
        my @processors = $ast.processors;
        @processors.push('heredoc');

        my $stop   := $ast.stop;
        my $indent := $stop eq "\n"
          ?? ''
          !! " " x ($stop.chars - $stop.trim-leading.chars);

        self!multiple-processors($stop.trim, @processors)
          ~ "\n"
          ~ $string.chomp('\n').split(Q/\n/).map({
              $_ ?? "$indent$_\n" !! "\n"
            }).join
          ~ $stop
    }

#- I ---------------------------------------------------------------------------

    multi method deparse(RakuAST::Infix:D $ast --> Str:D) {
        $ast.operator
    }

    multi method deparse(RakuAST::Initializer::Assign:D $ast --> Str:D) {
        $.assign ~ self.deparse($ast.expression)
    }

    multi method deparse(RakuAST::Initializer::Bind:D $ast --> Str:D) {
        $.bind ~ self.deparse($ast.expression)
    }

    multi method deparse(RakuAST::IntLiteral:D $ast --> Str:D) {
        $ast.value.raku
    }

#- L ---------------------------------------------------------------------------

    multi method deparse(RakuAST::Label:D $ast --> Str:D) {
        $ast.name ~ ': '
    }

#- M ---------------------------------------------------------------------------

    multi method deparse(RakuAST::MetaInfix::Assign:D $ast --> Str:D) {
        self.deparse($ast.infix) ~ '='
    }

    multi method deparse(RakuAST::MetaInfix::Negate:D $ast --> Str:D) {
        self.deparse($ast.infix) ~ '!'
    }

    multi method deparse(RakuAST::Method:D $ast --> Str:D) {
        self!method($ast, 'method')
    }

#- N ---------------------------------------------------------------------------

    multi method deparse(RakuAST::Name:D $ast --> Str:D) {
        $ast.canonicalize
    }

    multi method deparse(RakuAST::Nqp:D $ast --> Str:D) {
        "nqp::" ~ $ast.op ~ self!parenthesize($ast.args)
    }

    multi method deparse(RakuAST::Nqp::Const:D $ast --> Str:D) {
        "nqp::const::" ~ $ast.name
    }

    multi method deparse(RakuAST::NumLiteral:D $ast --> Str:D) {
        $ast.value.raku
    }

#- P ---------------------------------------------------------------------------

    multi method deparse(RakuAST::Package:D $ast --> Str:D) {
        my str @parts;

        if $ast.scope -> $scope {
            @parts.push($scope) if $scope ne 'our'; # XXX
        }

        @parts.push($ast.package-declarator);
        @parts.push(self.deparse($ast.name));

        if $ast.traits -> @traits {
            for @traits -> $trait {
                @parts.push(self.deparse($trait));
            }
        }

        @parts.push(self.deparse($ast.body));

        @parts.join(' ')
    }

    multi method deparse(RakuAST::Pragma:D $ast --> Str:D) {
        my str @parts = ($ast.off ?? "no" !! "use"), $ast.name;
        @parts.push(self.deparse($_)) with $ast.argument;
        @parts.join(' ')
    }

#- Parameter -------------------------------------------------------------------

    multi method deparse(RakuAST::Parameter:D $ast --> Str:D) {
        return .raku with $ast.value;

        my $target := $ast.target;
        my str @parts;
        if $ast.type -> $type {
            my str $deparsed = self.deparse($type);
            if $deparsed ne 'Any' {
                @parts.push($deparsed);
                @parts.push(' ') if $ast.target;
            }
        }

        if $ast.target -> $target {
            my str $var = self.deparse($target, :slurpy($ast.slurpy));

            # named parameter
            if $ast.names -> @names {
                my str $varname = $var.substr(1);  # lose the sigil
                my int $parens;
                my int $seen;

                for @names -> $name {
                    if $name eq $varname {
                        $seen = 1;
                    }
                    else {
                        @parts.push(':');
                        @parts.push($name);
                        @parts.push('(');
                        ++$parens;
                    }
                }

                @parts.push(':') if $seen;
                @parts.push($var);
                @parts.push(nqp::x(')',$parens)) if $parens;
                @parts.push('?') if $ast.is-declared-optional;
                @parts.push('!') if $ast.is-declared-required;
            }

            # positional parameter
            else {
                given $ast.slurpy -> $prefix {
                    @parts.push(self.deparse($prefix));
                }
                @parts.push($var);
                if $ast.invocant {
                    @parts.push(':');
                }
                elsif $ast.is-declared-optional {
                    @parts.push('?');
                }
                elsif $ast.is-declared-required {
                    @parts.push('!');
                }
            }
        }

        @parts.join
    }

    multi method deparse(RakuAST::Parameter::Slurpy:U $ --> '') { }

    multi method deparse(RakuAST::Parameter::Slurpy::Flattened:U $ --> Str:D) {
        $.slurpy-flattened
    }

    multi method deparse(
      RakuAST::Parameter::Slurpy::SingleArgument:U $
    --> Str:D) {
        $.slurpy-single-argument
    }

    multi method deparse(
      RakuAST::Parameter::Slurpy::Unflattened:U $
    --> Str:D) {
        $.slurpy-unflattened
    }

    multi method deparse(RakuAST::Parameter::Slurpy::Capture:U $ --> Str:D) {
        $.slurpy-capture
    }

    multi method deparse(RakuAST::ParameterTarget::Var:D $ast --> Str:D) {
        $ast.name
    }

    multi method deparse(
      RakuAST::ParameterTarget::Term:D $ast, :$slurpy
    --> Str:D) {
        ($slurpy === RakuAST::Parameter::Slurpy ?? '\\' !! '')
          ~ $ast.name.canonicalize
    }

    multi method deparse(RakuAST::ParameterDefaultThunk:D $ --> '') { }

#- Po --------------------------------------------------------------------------

    multi method deparse(RakuAST::PointyBlock:D $ast --> Str:D) {
        my str @parts = '-> ';

        if self.deparse($ast.signature) -> $signature {
            @parts.push($signature);
            @parts.push(' ');
        }

        @parts.push(self.deparse($ast.body));

        @parts.join
    }

    multi method deparse(RakuAST::Postcircumfix::ArrayIndex:D $ast --> Str:D) {
        self!squarize($ast.index)
    }

    multi method deparse(RakuAST::Postcircumfix::HashIndex:D $ast --> Str:D) {
        self!bracketize($ast.index)
    }

    multi method deparse(
      RakuAST::Postcircumfix::LiteralHashIndex:D $ast
    --> Str:D) {
        self.deparse($ast.index)
    }

    multi method deparse(RakuAST::Postfix:D $ast --> Str:D) {
        $ast.operator
    }

    multi method deparse(RakuAST::Postfix::Power:D $ast --> Str:D) {
        $ast.power.trans('-0123456789' => '⁻⁰¹²³⁴⁵⁶⁷⁸⁹')
    }

    multi method deparse(RakuAST::Prefix:D $ast --> Str:D) {
        $ast.operator
    }

#- Q ---------------------------------------------------------------------------

    multi method deparse(RakuAST::QuotedRegex:D $ast --> Str:D) {
        ($ast.match-immediately ?? 'm' !! '')
          ~ $.regex-open
          ~ self.deparse($ast.body)
          ~ $.regex-close
    }

    multi method deparse(RakuAST::QuotedString:D $ast --> Str:D) {
        my str $string = self!assemble-quoted-string($ast);

        if $ast.processors -> @processors {
            if @processors == 1 && @processors.head -> $processor {
                if %single-processor-prefix{$processor} -> str $p {
                    ($p eq 'exec' && $ast.has-variables ?? 'qqx' !! $p)
                      ~ $string
                      ~ '/'
                }
                else {
                    NYI("Quoted string processor '$processor'").throw
                }
            }
            elsif @processors == 2 && !$ast.has-variables {
                my str $joined = @processors.join(' ');
                if $joined eq 'words val' {
                    $.pointy-open ~ $string ~ $.pointy-close
                }
                elsif $joined eq 'quotewords val' {
                    $.double-pointy-open ~ $string ~ $.double-pointy-close
                }
                else {
                    self!multiple-processors($string, @processors)
                }
            }
            else {
                self!multiple-processors($string, @processors)
            }
        }
        else {
            '"' ~ $string ~ '"'
        }
    }

    multi method deparse(RakuAST::QuoteWordsAtom:D $ast --> Str:D) {
        self.deparse($ast.atom)
    }

#- R ---------------------------------------------------------------------------

    multi method deparse(RakuAST::RatLiteral:D $ast --> Str:D) {
        $ast.value.raku
    }

#- Regex -----------------------------------------------------------------------

    multi method deparse(
      RakuAST::Regex::Anchor::BeginningOfString $
    --> Str:D) {
        $.regex-beginning-of-string
    }

    multi method deparse(RakuAST::Regex::Anchor::EndOfString $ --> Str:D) {
        $.regex-end-of-string
    }

    multi method deparse(
      RakuAST::Regex::Anchor::BeginningOfLine $
    --> Str:D) {
        $.regex-beginning-of-line
    }

    multi method deparse(RakuAST::Regex::Anchor::EndOfLine $ --> Str:D) {
        $.regex-end-of-line
    }

    multi method deparse(RakuAST::Regex::Anchor::LeftWordBoundary $ --> Str:D) {
        $.regex-left-word-boundary
    }

    multi method deparse(
      RakuAST::Regex::Anchor::RightWordBoundary $
    --> Str:D) {
        $.regex-right-word-boundary
    }

    multi method deparse(RakuAST::Regex::Literal:D $ast --> Str:D) {
        self!quote-if-needed($ast.text)
    }

    multi method deparse(RakuAST::Regex::Alternation:D $ast --> Str:D) {
        self!branches($ast, $.regex-alternation)
    }

#- Regex::Assertion ------------------------------------------------------------

    multi method deparse(RakuAST::Regex::Assertion::Alias:D $ast --> Str:D) {
        '<'
          ~ $ast.name
          ~ '='
          ~ self.deparse($ast.assertion).substr(1)
    }

    multi method deparse(
      RakuAST::Regex::Assertion::Callable:D $ast
    --> Str:D) {
        '<&' ~ self.deparse($ast.callee) ~ self!parenthesize($ast.args) ~ '>'
    }

    multi method deparse(
      RakuAST::Regex::Assertion::CharClass:D $ast
    --> Str:D) {
        '<' ~ $ast.elements.map({ self.deparse($_) }).join(' ') ~ '>'
    }

    multi method deparse(RakuAST::Regex::Assertion::Fail $ --> Str:D) {
        $.regex-assertion-fail
    }

    multi method deparse(
      RakuAST::Regex::Assertion::InterpolatedBlock:D $ast
    --> Str:D) {
        NYI "DEPARSE of sequential interpolated block NYI" if $ast.sequential;
        '<' ~ self.deparse($ast.block).chomp ~ '>'
    }

    multi method deparse(
      RakuAST::Regex::Assertion::InterpolatedVar:D $ast
    --> Str:D) {
        NYI "DEPARSE of sequential interpolated block NYI" if $ast.sequential;
        '<' ~ self.deparse($ast.var) ~ '>'
    }

    multi method deparse(
      RakuAST::Regex::Assertion::Lookahead:D $ast
    --> Str:D) {
        ($ast.negated ?? '<!' !! '<?')
          ~ self.deparse($ast.assertion).substr(1)
    }

    multi method deparse(RakuAST::Regex::Assertion::Named:D $ast --> Str:D) {
        ($ast.capturing ?? '<' !! '<.') ~ self.deparse($ast.name) ~ '>'
    }

    multi method deparse(
      RakuAST::Regex::Assertion::Named::Args:D $ast
    --> Str:D) {
        ($ast.capturing ?? '<' !! '<.')
          ~ self.deparse($ast.name)
          ~ self!parenthesize($ast.args)
          ~ '>'
    }

    multi method deparse(
      RakuAST::Regex::Assertion::Named::RegexArg:D $ast
    --> Str:D) {
        '<'
          ~ self.deparse($ast.name)
          ~ ' '
          ~ self.deparse($ast.regex-arg)
          ~ '>'
    }

    multi method deparse(RakuAST::Regex::Assertion::Pass $ --> Str:D) {
        $.regex-assertion-pass
    }

    multi method deparse(
      RakuAST::Regex::Assertion::PredicateBlock:D $ast
    --> Str:D) {
        '<'
          ~ ($ast.negated ?? '!' !! '?')
          ~ self.deparse($ast.block).chomp
          ~ '>'
    }

#- Regex::B --------------------------------------------------------------------

    multi method deparse(
      RakuAST::Regex::BackReference::Positional:D $ast
    --> Str:D) {
        '$' ~ $ast.index
    }

    multi method deparse(
      RakuAST::Regex::BackReference::Named:D $ast
    --> Str:D) {
        '$<' ~ $ast.name ~ '>'
    }

    # This candidate needed to represent *no* backtracking specification
    multi method deparse(RakuAST::Regex::Backtrack:U $ --> '') { }

    multi method deparse(RakuAST::Regex::Backtrack::Frugal:U $ --> Str:D) {
        $.regex-backtrack-frugal
    }

    multi method deparse(RakuAST::Regex::Backtrack::Greedy:U $ --> Str:D) {
        $.regex-backtrack-greedy
    }

    multi method deparse(RakuAST::Regex::Backtrack::Ratchet:U $ --> Str:D) {
        $.regex-backtrack-ratchet
    }

    multi method deparse(
      RakuAST::Regex::BacktrackModifiedAtom:D $ast
    --> Str:D) {
        self.deparse($ast.atom) ~ self.deparse($ast.backtrack)
    }

    multi method deparse(RakuAST::Regex::Block:D $ast --> Str:D) {
        self.deparse($ast.block).chomp
    }

#- Regex::C --------------------------------------------------------------------

    multi method deparse(RakuAST::Regex::CapturingGroup:D $ast --> Str:D) {
        '(' ~ self.deparse($ast.regex) ~ ')'
    }

#- Regex::Charclass ------------------------------------------------------------

    multi method deparse(RakuAST::Regex::CharClass::Any $ast --> Str:D) {
        $.regex-any
    }

    multi method deparse(
      RakuAST::Regex::CharClass::BackSpace:D $ast
    --> Str:D) {
        $ast.negated ?? '\\B' !! '\\b'
    }

    multi method deparse(
      RakuAST::Regex::CharClass::CarriageReturn:D $ast
    --> Str:D) {
        $ast.negated ?? '\\R' !! '\\r'
    }

    multi method deparse(RakuAST::Regex::CharClass::Digit:D $ast --> Str:D) {
        $ast.negated ?? '\\D' !! '\\d'
    }

    multi method deparse(RakuAST::Regex::CharClass::Escape:D $ast --> Str:D) {
        $ast.negated ?? '\\E' !! '\\e'
    }

    multi method deparse(RakuAST::Regex::CharClass::FormFeed:D $ast --> Str:D) {
        $ast.negated ?? '\\F' !! '\\f'
    }

    multi method deparse(
      RakuAST::Regex::CharClass::HorizontalSpace:D $ast
    --> Str:D) {
        $ast.negated ?? '\\H' !! '\\h'
    }

    multi method deparse(RakuAST::Regex::CharClass::Newline:D $ast --> Str:D) {
        $ast.negated ?? '\\N' !! '\\n'
    }

    multi method deparse(RakuAST::Regex::CharClass::Nul:D $ast --> '\0') { }

    multi method deparse(RakuAST::Regex::CharClass::Space:D $ast --> Str:D) {
        $ast.negated ?? '\\S' !! '\\s'
    }

    multi method deparse(
      RakuAST::Regex::CharClass::Specified:D $ast
    --> Str:D) {
        ($ast.negated ?? '\\C' !! '\\c')
          ~ '['
          ~ $ast.characters.ords.map(*.uniname).join(', ')
          ~ ']'
    }

    multi method deparse(RakuAST::Regex::CharClass::Tab:D $ast --> Str:D) {
        $ast.negated ?? '\\T' !! '\\t'
    }

    multi method deparse(
      RakuAST::Regex::CharClass::VerticalSpace:D $ast
    --> Str:D) {
        $ast.negated ?? '\\V' !! '\\v'
    }

    multi method deparse(RakuAST::Regex::CharClass::Word:D $ast --> Str:D) {
        $ast.negated ?? '\\W' !! '\\w'
    }

    multi method deparse(
      RakuAST::Regex::CharClassElement::Enumeration:D $ast
    --> Str:D) {
        ($ast.negated ?? '-' !! '+')
          ~ '[' ~ $ast.elements.map({ self.deparse($_) }).join(' ') ~ ']'
    }

    multi method deparse(
      RakuAST::Regex::CharClassElement::Property:D $ast
    --> Str:D) {
        my str @parts;

        @parts.push($ast.negated ?? '-' !! '+');
        @parts.push(':');
        @parts.push('!') if $ast.inverted;
        @parts.push($ast.property);
        @parts.push(self.deparse($_)) with $ast.predicate;
        @parts.join
    }

    multi method deparse(
      RakuAST::Regex::CharClassElement::Rule:D $ast
    --> Str:D) {
        ($ast.negated ?? '-' !! '+') ~ $ast.name
    }

    multi method deparse(
      RakuAST::Regex::CharClassEnumerationElement::Character:D $ast
    --> Str:D) {
        $ast.character
    }

    multi method deparse(
      RakuAST::Regex::CharClassEnumerationElement::Range:D $ast
    --> Str:D) {
        $ast.from.chr ~ '..' ~ $ast.to.chr
    }

#- Regex::Co -------------------------------------------------------------------

    multi method deparse(RakuAST::Regex::Conjunction:D $ast --> Str:D) {
        self!branches($ast, $.regex-conjunction)
    }

#- Regex::G --------------------------------------------------------------------

    multi method deparse(RakuAST::Regex::Group:D $ast --> Str:D) {
        self!squarize($ast.regex)
    }

#- Regex::I --------------------------------------------------------------------

    multi method deparse(
      RakuAST::Regex::InternalModifier::IgnoreCase:D $ast --> Str:D) {
        $ast.negated ?? ':!i' !! ':i'
    }

    multi method deparse(
      RakuAST::Regex::InternalModifier::IgnoreMark:D $ast
    --> Str:D) {
        $ast.negated ?? ':!m' !! ':m'
    }

    multi method deparse(
      RakuAST::Regex::InternalModifier::Ratchet:D $ast
    --> Str:D) {
        $ast.negated ?? ':!r' !! ':r'
    }

    multi method deparse(
      RakuAST::Regex::InternalModifier::Sigspace:D $ast
    --> Str:D) {
        $ast.negated ?? ':!s' !! ':s'
    }

    multi method deparse(RakuAST::Regex::Interpolation:D $ast --> Str:D) {
        ($ast.sequential ?? '|| ' !! '') ~ self.deparse($ast.var)
    }

#- Regex::M --------------------------------------------------------------------

    multi method deparse(RakuAST::Regex::MatchFrom:D $ --> Str:D) {
        $.regex-match-from
    }

    multi method deparse(RakuAST::Regex::MatchTo:D $ --> Str:D) {
        $.regex-match-to
    }

#- Regex::N --------------------------------------------------------------------

    multi method deparse(RakuAST::Regex::NamedCapture:D $ast --> Str:D) {
        '$<' ~ $ast.name ~ '>=' ~ self.deparse($ast.regex)
    }

#- Regex::Q --------------------------------------------------------------------

    multi method deparse(RakuAST::Regex::QuantifiedAtom:D $ast --> Str:D) {
        my str @parts = self.deparse($ast.atom), self.deparse($ast.quantifier);

        if $ast.separator -> $separator {
            @parts.push($ast.trailing-separator ?? ' %% ' !! ' % ');
            @parts.push(self.deparse($separator));
        }

        @parts.join
    }

    multi method deparse(
      RakuAST::Regex::Quantifier::BlockRange:D $ast
    --> Str:D) {
        self!quantifier($ast, ' ** ' ~ self.deparse($ast.block))
    }

    multi method deparse(
      RakuAST::Regex::Quantifier::OneOrMore:D $ast
    --> Str:D) {
        self!quantifier($ast, '+')
    }

    multi method deparse(RakuAST::Regex::Quantifier::Range:D $ast --> Str:D) {
        self!quantifier(
          $ast,
          ' ** '
            ~ ($ast.min.defined
                ??  $ast.min ~ ($ast.excludes-min ?? '^' !! '') ~ '..'
                !! '')
            ~ ($ast.excludes-max ?? '^' !! '')
            ~ ($ast.max // '*')
        )
    }

    multi method deparse(
      RakuAST::Regex::Quantifier::ZeroOrMore:D $ast
    --> Str:D) {
        self!quantifier($ast, '*')
    }

    multi method deparse(
      RakuAST::Regex::Quantifier::ZeroOrOne:D $ast
    --> Str:D) {
        self!quantifier($ast, '?')
    }

    multi method deparse(RakuAST::Regex::Quote:D $ast --> Str:D) {
        my str $quoted = self.deparse($ast.quoted);
        $quoted.chars > 2
          ?? $quoted.starts-with('"')
            ?? $quoted.substr(1,$quoted.chars - 2)
            !! ('<{ ' ~ $quoted ~ ' }>')
          !! ''
    }

#- Regex::S --------------------------------------------------------------------

    multi method deparse(RakuAST::Regex::Sequence:D $ast --> Str:D) {
        if $ast.terms -> @terms {
            @terms.map({
                nqp::istype($_,RakuAST::Regex::CharClass::BackSpace)
                  ?? ('"' ~ self.deparse($_) ~ '"')
                  !! self.deparse($_)
            }).join(' ')
        }
        else {
            ''
        }
    }

    multi method deparse(
      RakuAST::Regex::SequentialAlternation:D $ast
    --> Str:D) {
        self!branches($ast, $.regex-sequential-alternation)
    }

    multi method deparse(
      RakuAST::Regex::SequentialConjunction:D $ast
    --> Str:D) {
        self!branches($ast, $.regex-sequential-conjunction)
    }

    multi method deparse(RakuAST::Regex::Statement:D $ast --> Str:D) {
        ':' ~ self.deparse($ast.statement) ~ ';'
    }

#- Regex::W --------------------------------------------------------------------

    multi method deparse(RakuAST::Regex::WithSigspace:D $ast --> Str:D) {
        self.deparse($ast.regex)  # XXX something missing here??
    }

#- S ---------------------------------------------------------------------------

    multi method deparse(RakuAST::SemiList:D $ast --> Str:D) {
        $ast.statements.map({ self.deparse($_) }).join($.list-infix-semi-colon)
    }

    multi method deparse(RakuAST::Signature:D $ast --> Str:D) {
        my str $params  =
          $ast.parameters.map({ self.deparse($_) }).join($.list-infix-comma);
        my str $returns = self.deparse($_)
          with $ast.returns;

        $params
          ~ (" " if $params && $returns)
          ~ ($returns ?? "--> $returns" !! "")
    }

#- Statement -------------------------------------------------------------------

    multi method deparse(RakuAST::Statement::Catch:D $ast --> Str:D) {
        self!labels($ast) ~ 'CATCH ' ~ self.deparse($ast.body)
    }

    multi method deparse(RakuAST::Statement::Control:D $ast --> Str:D) {
        self!labels($ast) ~ 'CONTROL ' ~ self.deparse($ast.body)
    }

    multi method deparse(RakuAST::Statement::Default:D $ast --> Str:D) {
        self!labels($ast) ~ 'default ' ~ self.deparse($ast.body)
    }

    multi method deparse(RakuAST::Statement::Elsif:D $ast --> Str:D) {
        self!conditional($ast, 'elsif')  # cannot have labels
    }

    multi method deparse(RakuAST::Statement::Empty:D $ast --> Str:D) {
        self!labels($ast)
    }

    multi method deparse(RakuAST::Statement::Expression:D $ast --> Str:D) {
        my str @parts = self.deparse($ast.expression);

        if $ast.condition-modifier -> $condition {
            @parts.push(self.deparse($condition));
        }

        if $ast.loop-modifier -> $loop {
            @parts.push(self.deparse($loop));
        }

        self!labels($ast) ~ @parts.join(' ')
    }

    multi method deparse(RakuAST::Statement::For:D $ast --> Str:D) {
        my str @parts =
          'for',
          self.deparse($ast.source),
          self.deparse($ast.body)
        ;

        if $ast.mode -> str $mode {
            @parts.unshift($mode) if $mode ne 'serial';
        }

        self!labels($ast) ~ @parts.join(' ')
    }

    multi method deparse(RakuAST::Statement::Given:D $ast --> Str:D) {
        self!labels($ast)
          ~ 'given '
          ~ self.deparse($ast.source)
          ~ ' '
          ~ self.deparse($ast.body).chomp
          ~ $.last-statement
    }

    multi method deparse(RakuAST::Statement::If:D $ast --> Str:D) {
        my str @parts = self!conditional($ast, $ast.IMPL-QAST-TYPE);

        if $ast.elsifs -> @elsifs {
            @parts.push(self.deparse($_)) for @elsifs;
        }

        if $ast.else -> $else {
            @parts.push('else ');
            @parts.push(self.deparse($else));
            @parts.push($.last-statement);
        }

        self!labels($ast) ~ @parts.join
    }

    multi method deparse(RakuAST::Statement::Loop:D $ast --> Str:D) {
        self!labels($ast)
          ~ 'loop ('
          ~ self.deparse($ast.setup)
          ~ $.loop-separator
          ~ self.deparse($ast.condition)
          ~ $.loop-separator
          ~ self.deparse($ast.increment)
          ~ ') '
          ~ self.deparse($ast.body)
          ~ $.last-statement
    }

    multi method deparse(
      RakuAST::Statement::Loop::RepeatUntil:D $ast
    --> Str:D) {
        self!labels($ast) ~ self!simple-repeat($ast, 'until')
    }

    multi method deparse(
      RakuAST::Statement::Loop::RepeatWhile:D $ast --> Str:D) {
        self!labels($ast) ~ self!simple-repeat($ast, 'while')
    }

    multi method deparse(RakuAST::Statement::Loop::Until:D $ast --> Str:D) {
        self!labels($ast) ~ self!simple-loop($ast, 'until')
    }

    multi method deparse(RakuAST::Statement::Loop::While:D $ast --> Str:D) {
        self!labels($ast) ~ self!simple-loop($ast, 'while')
    }

    multi method deparse(RakuAST::Statement::Orwith:D $ast --> Str:D) {
        self!conditional($ast, 'orwith')  # cannot have labels
    }

    multi method deparse(RakuAST::Statement::Require:D $ast --> Str:D) {
        self!labels($ast) ~ 'require ' ~ self.deparse($ast.module-name)
    }

    multi method deparse(RakuAST::Statement::Unless:D $ast --> Str:D) {
        self!labels($ast) ~ self!negated-conditional($ast, 'unless');
    }

    multi method deparse(RakuAST::Statement::Use:D $ast --> Str:D) {
        self!use-no("use ", $ast)
    }

    multi method deparse(RakuAST::Statement::When:D $ast --> Str:D) {
        self!labels($ast)
          ~ 'when '
          ~ self.deparse($ast.condition)
          ~ ' '
          ~ self.deparse($ast.body)
    }

    multi method deparse(RakuAST::Statement::Without:D $ast --> Str:D) {
        self!labels($ast) ~ self!negated-conditional($ast, 'without');
    }

    multi method deparse(RakuAST::StatementList:D $ast --> Str:D) {

        if $ast.statements -> @statements {
            my str @parts;
            my str $spaces = $*INDENT;
            my str $last   = $.last-statement;
            my str $end    = $.end-statement;

            my str $code;
            for @statements.head(*-1) -> $statement {
                @parts.push($spaces);
                @parts.push($code = self.deparse($statement));
                @parts.push($code.ends-with($.bracket-close)
                  ?? $last !! $end
                ) unless $code.ends-with($.block-close);
            }

            @parts.push($spaces);
            @parts.push($code = self.deparse(@statements.tail));
            @parts.push($.last-statement)
              unless $code.ends-with($.block-close);

            @parts.join
        }

        else {
            ''
        }
    }

#- Statement::Modifier ---------------------------------------------------------

    multi method deparse(RakuAST::StatementModifier::Given:D $ast --> Str:D) {
        'given ' ~ self.deparse($ast.expression)
    }

    multi method deparse(RakuAST::StatementModifier::If:D $ast --> Str:D) {
        'if ' ~ self.deparse($ast.expression)
    }

    multi method deparse( RakuAST::StatementModifier::For:D $ast --> Str:D) {
        'for ' ~ self.deparse($ast.expression)
    }

    multi method deparse(RakuAST::StatementModifier::For::Thunk:D $ --> '') { }

    multi method deparse(RakuAST::StatementModifier::Unless:D $ast --> Str:D) {
        'unless ' ~ self.deparse($ast.expression)
    }

    multi method deparse(RakuAST::StatementModifier::Until:D $ast --> Str:D) {
        'until ' ~ self.deparse($ast.expression)
    }

    multi method deparse(RakuAST::StatementModifier::When:D $ast --> Str:D) {
        'when ' ~ self.deparse($ast.expression)
    }

    multi method deparse(RakuAST::StatementModifier::While:D $ast --> Str:D) {
        'while ' ~ self.deparse($ast.expression)
    }

    multi method deparse(RakuAST::StatementModifier::With:D $ast --> Str:D) {
        'with ' ~ self.deparse($ast.expression)
    }

    multi method deparse(RakuAST::StatementModifier::Without:D $ast --> Str:D) {
        'without ' ~ self.deparse($ast.expression)
    }

#- Statement::Prefix -----------------------------------------------------------

    multi method deparse(RakuAST::StatementPrefix::Do:D $ast --> Str:D) {
        'do ' ~ self.deparse($ast.blorst)
    }

    multi method deparse(RakuAST::StatementPrefix::Eager:D $ast --> Str:D) {
        'eager ' ~ self.deparse($ast.blorst)
    }

    multi method deparse(RakuAST::StatementPrefix::Gather:D $ast --> Str:D) {
        'gather ' ~ self.deparse($ast.blorst)
    }

    multi method deparse(RakuAST::StatementPrefix::Hyper:D $ast --> Str:D) {
        'hyper ' ~ self.deparse($ast.blorst)
    }

    multi method deparse(RakuAST::StatementPrefix::Lazy:D $ast --> Str:D) {
        'lazy ' ~ self.deparse($ast.blorst)
    }

    multi method deparse(
      RakuAST::StatementPrefix::Phaser::Begin:D $ast
    --> Str:D) {
        'BEGIN ' ~ self.deparse($ast.blorst)
    }

    multi method deparse(
      RakuAST::StatementPrefix::Phaser::Close:D $ast
    --> Str:D) {
        'CLOSE ' ~ self.deparse($ast.blorst)
    }

    multi method deparse(
      RakuAST::StatementPrefix::Phaser::End:D $ast
    --> Str:D) {
        'END ' ~ self.deparse($ast.blorst)
    }

    multi method deparse(
      RakuAST::StatementPrefix::Phaser::First:D $ast
    --> Str:D) {
        'FIRST ' ~ self.deparse($ast.blorst)
    }

    multi method deparse(
      RakuAST::StatementPrefix::Phaser::Last:D $ast
    --> Str:D) {
        'LAST ' ~ self.deparse($ast.blorst)
    }

    multi method deparse(
      RakuAST::StatementPrefix::Phaser::Enter:D $ast
    --> Str:D) {
        'ENTER ' ~ self.deparse($ast.blorst)
    }

    multi method deparse(
      RakuAST::StatementPrefix::Phaser::Init:D $ast
    --> Str:D) {
        'INIT ' ~ self.deparse($ast.blorst)
    }

    multi method deparse(
      RakuAST::StatementPrefix::Phaser::Keep:D $ast
    --> Str:D) {
        'KEEP ' ~ self.deparse($ast.blorst)
    }

    multi method deparse(
      RakuAST::StatementPrefix::Phaser::Leave:D $ast
    --> Str:D) {
        'LEAVE ' ~ self.deparse($ast.blorst)
    }

    multi method deparse(
      RakuAST::StatementPrefix::Phaser::Next:D $ast
    --> Str:D) {
        'NEXT ' ~ self.deparse($ast.blorst)
    }

    multi method deparse(
      RakuAST::StatementPrefix::Phaser::Post:D $ast
    --> Str:D) {
        # POST phasers get extra code inserted at RakuAST level, which
        # wraps the original blorst into a statement in which the blorst
        # becomes the condition modifier
        my $expression := $ast.blorst.body.statement-list.statements.head
          .condition-modifier.expression;
        'POST ' ~ self.deparse(
          nqp::istype($expression,RakuAST::ApplyPostfix)
            ?? $expression.operand
            !! $expression
        )
    }

    multi method deparse(
      RakuAST::StatementPrefix::Phaser::Pre:D $ast
    --> Str:D) {
        # PRE phasers get extra code inserted at RakuAST level, which
        # wraps the original blorst into a statement in which the blorst
        # becomes the condition modifier
        my $expression := $ast.blorst.condition-modifier.expression;
        'PRE ' ~ self.deparse(
          nqp::istype($expression,RakuAST::ApplyPostfix)
            ?? $expression.operand
            !! $expression
        )
    }

    multi method deparse(
      RakuAST::StatementPrefix::Phaser::Quit:D $ast
    --> Str:D) {
        'QUIT ' ~ self.deparse($ast.blorst)
    }

    multi method deparse(
      RakuAST::StatementPrefix::Phaser::Undo:D $ast
    --> Str:D) {
        'UNDO ' ~ self.deparse($ast.blorst)
    }

    multi method deparse(RakuAST::StatementPrefix::Quietly:D $ast --> Str:D) {
        'quietly ' ~ self.deparse($ast.blorst)
    }

    multi method deparse(RakuAST::StatementPrefix::Race:D $ast --> Str:D) {
        'race ' ~ self.deparse($ast.blorst)
    }

    multi method deparse(RakuAST::StatementPrefix::Start:D $ast --> Str:D) {
        'start ' ~ self.deparse($ast.blorst)
    }

    multi method deparse(RakuAST::StatementPrefix::Try:D $ast --> Str:D) {
        'try ' ~ self.deparse($ast.blorst)
    }

#- Str -------------------------------------------------------------------------

    multi method deparse(RakuAST::StrLiteral:D $ast --> Str:D) {
        $ast.value.raku
    }

#- Stu -------------------------------------------------------------------------

    multi method deparse(RakuAST::Stub:D $ast --> Str:D) {
        if $ast.args -> $real-args {
            $ast.name ~ ' ' ~ self.deparse($real-args)
        }
        else {
            $ast.name
        }
    }

#- Su --------------------------------------------------------------------------

    multi method deparse(RakuAST::Sub:D $ast --> Str:D) {
        my str @parts;

        given $ast.scope -> $scope {
            if $scope ne 'my' && ($ast.name || $scope ne 'anon') {
                @parts.push($scope);
                @parts.push(' ');
            }
        }
        @parts.push('sub');
        @parts.push(self!routine($ast));

        @parts.join
    }

    multi method deparse(RakuAST::Submethod:D $ast --> Str:D) {
        self!method($ast, 'submethod')
    }

    multi method deparse(RakuAST::Substitution:D $ast --> Str:D) {
        my str @parts = $ast.immutable ?? 'S' !! 's';
        @parts.push(':samespace') if $ast.samespace;

        if $ast.adverbs -> @adverbs {
            @parts.push(self.deparse($_)) for @adverbs;
        }

        if $ast.infix -> $infix {
            @parts.push('{');
            @parts.push(self.deparse($ast.pattern));
            @parts.push('} ');
            @parts.push(self.deparse($infix));
            @parts.push(' ');
            @parts.push(self.deparse($ast.replacement));
        }
        else {
            @parts.push('/');
            @parts.push(self.deparse($ast.pattern));
            @parts.push('/');
            @parts.push(self.deparse($ast.replacement).substr(1,*-1));
            @parts.push('/');
        }

        @parts.join
    }

    multi method deparse(
      RakuAST::SubstitutionReplacementThunk:D $ast
    --> Str:D) {
        self.deparse($ast.infix)
    }

#- Term ------------------------------------------------------------------------

    multi method deparse(RakuAST::Term::Capture:D $ast --> Str:D) {
        Q/\/ ~ self!parenthesize($ast.source)
    }

    multi method deparse(RakuAST::Term::EmptySet:D $ --> Str:D) {
        $.term-empty-set
    }

    multi method deparse(RakuAST::Term::HyperWhatever:D $ --> Str:D) {
        $.term-hyperwhatever
    }

    multi method deparse(RakuAST::Term::Name:D $ast --> Str:D) {
        self.deparse($ast.name)
    }

    multi method deparse(RakuAST::Term::Named:D $ast --> Str:D) {
        $ast.name
    }

    multi method deparse(RakuAST::Term::Rand:D $ --> Str:D) {
        $.term-rand
    }

    multi method deparse(RakuAST::Term::RadixNumber:D $ast --> Str:D) {
        # multi-part doesn't need to be checked, as it only involves
        # the legality of what is put in .value.  So deparsing .value
        # is enough
        ':' ~ $ast.radix ~ self.deparse($ast.value)
    }

    multi method deparse(RakuAST::Term::Reduce:D $ast --> Str:D) {
        my $args := $ast.args;

        ($ast.triangle ?? $.reduce-triangle !! $.reduce-open)
          ~ self.deparse($ast.infix)
          ~ $.reduce-close
          ~ ($args.defined && $args.elems == 1
              ?? self.deparse($args)
              !! self!parenthesize($args)
            )
    }

    multi method deparse(RakuAST::Term::Self:D $ --> Str:D) {
        $.term-self
    }

    multi method deparse(RakuAST::Term::TopicCall:D $ast --> Str:D) {
        self.deparse($ast.call)
    }

    multi method deparse(RakuAST::Term::Whatever:D $ --> Str:D) {
        $.term-whatever
    }

#- Ternary ---------------------------------------------------------------------

    multi method deparse(RakuAST::Ternary:D $ast --> Str:D) {
        my $heredoc := $*HEREDOC;

        # no place to store heredocs, make one, try again, add them at the end
        if nqp::istype($heredoc,Failure) {
            $heredoc := my $*HEREDOC := my str @;
            my $deparsed := self.deparse($ast);

            nqp::elems($heredoc)
              ?? $deparsed ~ $heredoc.join ~ "\n"
              !! $deparsed
        }

        # already have a place to store heredocs
        else {
            my str @parts = self.deparse($ast.condition), $.ternary1;

            # helper sup for a ternary part
            sub deparse-part($node --> Nil) {
                if nqp::istype($node,RakuAST::Heredoc) {
                    my str ($header,$rest) = self.deparse($node).split("\n",2);
                    @parts.push($header);
                    $heredoc.push("\n" ~ $rest.chomp);
                }
                else {
                    @parts.push(self.deparse($node));
                }
            }

            deparse-part($ast.then);
            @parts.push($.ternary2);
            deparse-part($ast.else);

            @parts.join
        }
    }

#- Trait -----------------------------------------------------------------------

    multi method deparse(RakuAST::Trait::Is:D $ast --> Str:D) {
        my str $base = $ast.IMPL-TRAIT-NAME ~ ' ' ~ self.deparse($ast.name);
        with $ast.argument {
            $base ~ self.deparse($_)
        }
        else {
            $base
        }
    }

    multi method deparse(RakuAST::Trait::Hides:D $ast --> Str:D) {
        self!typish-trait($ast)
    }

    multi method deparse(RakuAST::Trait::Does:D $ast --> Str:D) {
        self!typish-trait($ast)
    }

    multi method deparse(RakuAST::Trait::Of:D $ast --> Str:D) {
        self!typish-trait($ast)
    }

    multi method deparse(RakuAST::Trait::Returns:D $ast --> Str:D) {
        self!typish-trait($ast)
    }

#- Type ------------------------------------------------------------------------

    multi method deparse(RakuAST::Type::Simple:D $ast --> Str:D) {
        self.deparse($ast.name)
    }

    multi method deparse(RakuAST::Type::Definedness:D $ast --> Str:D) {
        self.deparse($ast.name)
    }

    multi method deparse(RakuAST::Type::Coercion:D $ast --> Str:D) {
        self.deparse($ast.name) ~ '(' ~ self.deparse($ast.constraint) ~ ')'
    }

    multi method deparse(RakuAST::Type::Parameterized:D $ast --> Str:D) {
        self.deparse($ast.name) ~ '[' ~ self.deparse($ast.args) ~ ']'
    }

    multi method deparse(RakuAST::Type::Subset:D $ast --> Str:D) {
        my str @parts = 'subset';
        my str $scope = $ast.scope;

        @parts.unshift($scope) if $scope && $scope ne 'our'; # XXX
        @parts.push(self.deparse($ast.name));
        @parts.push(self.deparse($_)) with $ast.of;
        @parts.push(self.deparse($_)) for $ast.traits;
        @parts.push('where ' ~ self.deparse($_)) with $ast.where;

        @parts.join(' ')
    }

#- Var -------------------------------------------------------------------------

    multi method deparse(RakuAST::Var::Attribute:D $ast --> Str:D) {
        $ast.name
    }

    multi method deparse(RakuAST::Var::Compiler::File:D $ast --> Str:D) {
        $.var-compiler-file
    }

    multi method deparse(RakuAST::Var::Compiler::Line:D $ast --> Str:D) {
        $.var-compiler-line
    }

    multi method deparse(RakuAST::Var::Compiler::Lookup:D $ast --> Str:D) {
        $ast.name
    }

    multi method deparse(RakuAST::Var::Dynamic:D $ast --> Str:D) {
        $ast.name
    }

    multi method deparse(RakuAST::Var::Lexical:D $ast --> Str:D) {
        my $name := $ast.name;
        $name.starts-with('$whatevercode_arg_') ?? '*' !! $name
    }

    multi method deparse(RakuAST::Var::NamedCapture:D $ast --> Str:D) {
        '$' ~ self.deparse($ast.index)
    }

    multi method deparse(RakuAST::Var::Package:D $ast --> Str:D) {
        $ast.sigil ~ self.deparse($ast.name)
    }

    multi method deparse(RakuAST::Var::PositionalCapture:D $ast --> Str:D) {
        '$' ~ $ast.index.Str
    }

#- VarDeclaration --------------------------------------------------------------

    multi method deparse(RakuAST::VarDeclaration::Anonymous:D $ast --> Str:D) {
        my str $sigil = $ast.sigil;
        my str $scope = $ast.scope;

        $sigil eq '$' && $scope eq 'state'
          ?? $sigil
          !! "$scope $sigil"
    }

    multi method deparse(RakuAST::VarDeclaration::Constant:D $ast --> Str:D) {
        my str @parts;

        my str $scope = $ast.scope;
        @parts.push($scope) if $scope ne $ast.default-scope;
        @parts.push(self.deparse($_)) with $ast.type;
        @parts.push('constant');
        @parts.push($ast.name);
        if $ast.traits -> @traits {
            @parts.push(self.deparse($_)) for @traits;
        }
        @parts.push('=');
        @parts.push($ast.value.raku);

        @parts.join(' ');
    }

    multi method deparse(RakuAST::VarDeclaration::Implicit:D $ast --> Str:D) {
        $ast.name
    }

    multi method deparse(
      RakuAST::VarDeclaration::Implicit::Constant:D $ast
    --> Str:D) {
        'my constant ' ~ $ast.name ~ ' = ' ~ $ast.value.raku
    }

    multi method deparse(
      RakuAST::VarDeclaration::Placeholder::Named:D $ast
    --> Str:D) {
        .substr(0, 1) ~ ':' ~ .substr(1) given $ast.lexical-name
    }

    multi method deparse(
      RakuAST::VarDeclaration::Placeholder::Positional:D $ast
    --> Str:D) {
        .substr(0, 1) ~ '^' ~ .substr(1) given $ast.lexical-name
    }

    multi method deparse(
      RakuAST::VarDeclaration::Placeholder::SlurpyArray:D $
    --> '@_') { }

    multi method deparse(
      RakuAST::VarDeclaration::Placeholder::SlurpyHash:D $
    --> '%_') { }

    multi method deparse(RakuAST::VarDeclaration::Simple:D $ast --> Str:D) {
        my str @parts;

        @parts.push($ast.scope);
        @parts.push(' ');

        if $ast.type -> $type {
            @parts.push(self.deparse($type));
            @parts.push(' ');
        }

        @parts.push(nqp::istype($ast, RakuAST::VarDeclaration::Anonymous)
          ?? $ast.sigil
          !! $ast.name
        );
        if $ast.initializer -> $initializer {
            @parts.push(self.deparse($initializer));
        }

        @parts.join
    }

    multi method deparse(RakuAST::VarDeclaration::Term:D $ast --> Str:D) {
        my str @parts;

        @parts.push($ast.scope);
        @parts.push(' ');

        if $ast.type -> $type {
            @parts.push(self.deparse($type));
            @parts.push(' ');
        }

        @parts.push(Q/\/);
        @parts.push(self.deparse($ast.name));

        @parts.push(self.deparse($ast.initializer));

        @parts.join
    }

#- Version ---------------------------------------------------------------------

    multi method deparse(RakuAST::VersionLiteral:D $ast --> Str:D) {
        $ast.value.raku
    }
}

nqp::bindhllsym('Raku', 'DEPARSE', RakuAST::Deparse);

# vim: expandtab shiftwidth=4
