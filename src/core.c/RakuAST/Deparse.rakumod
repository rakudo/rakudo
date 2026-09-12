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
# General lookup hashes

    my constant %processor-attribute =
      'exec',       'x',
      'quotewords', 'ww',
      'val',        'v',
      'words',      'w',
      'heredoc',    'to',
      'format',     'format',
    ;

    my constant %single-processor-prefix =
      'exec',       'qx/',
      'quotewords', 'qqww/',
      'val',        'qq:v/',
      'words',      'qqw/',
      'format',     'qq:format/',
    ;

    my constant %twigil2type = <
      !  var-attribute
      .  var-attribute
      ?  var-compiler
      *  var-compiler
      =  var-rakudoc
    >;

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

    method regex-open(                  --> '/')  { }
    method regex-close(                 --> '/')   { }
    method regex-alternation(           --> '| ')  { }
    method regex-sequential-alternation(--> '|| ') { }
    method regex-conjunction(           --> '& ')  { }
    method regex-sequential-conjunction(--> '&& ') { }

    method regex-any(                --> '.')   { }
    method regex-beginning-of-string(--> '^ ')  { }
    method regex-end-of-string(      --> '$ ')  { }
    method regex-beginning-of-line(  --> '^^ ') { }
    method regex-end-of-line(        --> '$$ ') { }
    method regex-left-word-boundary( --> '<< ') { }
    method regex-right-word-boundary(--> '>> ') { }

    method regex-assertion-pass(--> '<?> ')     { }
    method regex-assertion-fail(--> '<!> ')     { }
    method regex-assertion-recurse(--> '<~~> ') { }

    method regex-backtrack-frugal( --> '?') { }
    method regex-backtrack-ratchet(--> ':') { }
    method regex-backtrack-greedy( --> '!') { }

    method regex-match-from(--> '<( ') { }
    method regex-match-to(  --> ')> ') { }

    method regex-nested(--> '~ ') { }

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

    method before-list-infix(--> ' ') { }
    method after-list-infix(--> ' ') { }

    method loop-separator(--> '; ') { }

    method pointy-sig(     --> '-> ')   { }
    method pointy-return(  --> ' --> ') { }
    method fatarrow(       --> ' => ')  { }
    method end-statement(  --> ";\n")   { }
    method last-statement( --> "\n")    { }

    method indent-with(--> '    ') { }

    method ternary1(--> ' ?? ') { }
    method ternary2(--> ' !! ') { }

#-------------------------------------------------------------------------------
# Setting up the deparse method

    proto method deparse(|) {
        if nqp::isnull(nqp::getlexcaller('$*INDENT')) {
            my $*INDENT    = "";  # indentation level
            my $*DELIMITER = "";  # delimiter to add, reset if added
            my $*INTERPOLATING := False;  # in a call interpolated in a string
            my $*QUOTE-REGEX-WORD := False;  # a regex word that must keep its quotes
            my $*QUOTE-DELIMITER  := '';     # delimiter to escape in quoted text
            my $*DOTTY-INFIX = False;  # the infix supplies the dot of the call
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

#-------------------------------------------------------------------------------
# Deparsing without syntax highlighting

    # A role to inhibit syntax highlighting
    my role no-highlight {
        method hsyn($, $content) { $content }
    }

    # Deparse without highlighting
    method deparse-without-highlighting(RakuAST::Node:D $ast) {
        (nqp::eqaddr(self.WHAT,RakuAST::Deparse)
          ?? self
          !! self but no-highlight
        ).deparse($ast, |%_)
    }

#-------------------------------------------------------------------------------
# Load any deparsing slang by given string

    method slang(Str:D $slang) {
        my $basename := $slang.split(":").head;
        qq:to/CODE/.EVAL
use experimental :rakuast;
use RakuAST::Deparse::L10N::$slang;
RakuAST::Deparse::L10N::$basename
CODE
    }

#-------------------------------------------------------------------------------
# Provide translation for given syntax feature of Raku

    # The default implementation of the "xsyn" method is basically a no-op,
    # because it will ignore the prefix (which can be any of <block core
    # infix modifier multi package phaser prefix routine scope stmt-prefix
    # trait type use>).  The idea is that you can mixin a role with this
    # method (such as RakuAST::Deparse::L10N::NL) that will provide
    # translations of the Raku Programming Language syntax elements to a
    # language different from English.
    #
    # Please see lib/RakuAST/Deparse/L10N/CORE.rakumod for the default
    # mapping and an example of implementation of the "xsyn" method for
    # translations.
    method xsyn(str $prefix, str $key) { $key }

#-------------------------------------------------------------------------------
# Provide highlighting for given syntax feature of Raku

    # The default implementation of the "hsyn" method is basically a no-op,
    # because it will ignore the prefix (which can be any of <block core
    # infix modifier multi package phaser prefix routine scope stmt-prefix
    # trait type use>).  The idea is that you can mixin a role with this
    # method (such as RakuAST::Deparse::Highlight::HTML) to provide some
    # kind of syntax highlighting.
    #
    # Please see lib/RakuAST/Deparse/Highlight/HTML.rakumod for an example
    # of implementation of the "hsyn" method for highlighting.
    proto method hsyn(|) {*}
    multi method hsyn($prefix, $key) { $key }

#-------------------------------------------------------------------------------
# Helper methods

    # helper method for deparsing contextualizers
    proto method context-target(|) {*}
    multi method context-target(RakuAST::StatementSequence $target --> Str:D) {
        self.parenthesize($target)
    }
    multi method context-target($target --> Str:D) {
        self.deparse($target)
    }

    method indent($indent = $.indent-with--> Str:D) {
        $_ = $_ ~ $indent with $*INDENT;
    }

    method dedent($indent = $.indent-with--> Str:D) {
        $_ = $_.chomp($indent) with $*INDENT;
    }

    method handle-signature($ast, str $header) {
        my str @parts = $header;

        sub add-traits() {
            if $ast.traits -> @traits {
                @parts.push(self.deparse($_)) for @traits;
            }
        }

        my $signature := $ast.signature;
        my $WHY       := $ast.WHY;
        if $signature && $signature.parameters-initialized
          && $signature.parameters.first(*.WHY) {
            # the docs of the routine go around its header, the statement
            # ends with the body
            my $*DELIMITER = '';
            @parts.push("(\n");
            @parts = self.add-any-docs(@parts.join(' '), $WHY)
              ~ self.deparse($signature)
              ~ ')';
            add-traits;
        }

        else {
            # a trait right after the declarator would read as the name
            @parts.push($signature ?? self.parenthesize($signature) !! '()')
              if ($signature
                   && $signature.parameters-initialized
                   && ($signature.parameters || $signature.returns))
              || (!$ast.name && $ast.traits);
            add-traits;

            if $WHY {
                # an onlystar body has no brace for the docs to follow
                if nqp::istype($ast.body,RakuAST::OnlyStar) {
                    @parts.push(self.deparse($ast.body));
                    my $*DELIMITER = '';
                    return self.add-any-docs(@parts.join(' '), $WHY);
                }
                @parts.push('{');
                return self.block-with-docs(@parts.join(' '), $WHY, $ast.body)
            }
        }

        @parts.push(self.deparse($ast.body));
        @parts.join(' ')

    }

    method method(RakuAST::Methodish:D $ast, str $kind --> Str:D) {
        my str @parts = self.syn-routine($kind);

        if $ast.multiness -> $multiness {
            @parts.unshift(self.syn-multi($multiness));
        }

        my str $scope = $ast.scope;
        @parts.unshift(self.hsyn("scope-$scope", self.xsyn('scope', $scope)))
          if $scope ne 'has' && $scope ne $ast.default-scope;

        my constant %system-names = <
          ACCEPTS ASSIGN-KEY ASSIGN-POS AT-KEY AT-POS BIND-KEY BIND-POS
          BUILD CALL-ME DELETE-KEY DELETE-POS DESTROY EXISTS-KEY EXISTS-POS
          STORE TWEAK UPGRADE-RAT WHICH WHY
        >.map(* => 1);

        if $ast.name -> $ast-name {
            my str $name = self.deparse($ast-name);
            @parts.push(nqp::istype($ast,RakuAST::Method)
              ?? $ast.private
                ?? "!$name"
                !! $ast.meta
                  ?? "^$name"
                  !! %system-names{$name}
                    ?? self.hsyn("system-$name", self.xsyn('system', $name))
                    !! $name
              !! $name
            );
        }

        self.handle-signature($ast, @parts.join(' '))
    }

    # a declaration in a condition would add the statement delimiter
    method condition($ast --> Str:D) {
        my $*DELIMITER = '';
        self.deparse($ast)
    }

    method conditional($self: $ast, str $type --> Str:D) {
        self.syn-block($type)
         ~ " $self.condition($ast.condition) $self.deparse($ast.then)$.last-statement"
    }

    method negated-conditional($self: $ast, str $type --> Str:D) {
        self.syn-block($type)
          ~ " $self.condition($ast.condition) $self.deparse($ast.body)$.last-statement"
    }

    method simple-loop($self: $ast, str $type --> Str:D) {
        self.syn-block($type)
          ~ " $self.condition($ast.condition) $self.deparse($ast.body)"
    }

    method simple-repeat($ast, str $type --> Str:D) {
       self.syn-block('repeat')
         ~ ' '
         ~ self.deparse($ast.body).chomp
         ~ ' '
         ~ self.syn-modifier($type)
         ~ ' '
         ~ self.condition($ast.condition)
         ~ $*DELIMITER
    }

    # :raw is for the < > form, which processes no escape but the
    # backslash and its own brackets
    method assemble-quoted-string($ast, :$raw --> Str:D) {
        my int $interpolated;
        my @segments := $ast.segments;
        my str @parts;
        for @segments.kv -> $i, $segment {
            if nqp::istype($segment,RakuAST::StrLiteral) {
                my str $text = $raw
                  ?? $segment.value.subst('\\','\\\\',:g).subst('<','\\<',:g).subst('>','\\>',:g)
                  !! $segment.value.raku.substr(1,*-1);
                if $text {
                    # a bracket right after an interpolation would continue
                    # it as a call or an index, and so would a dot that
                    # leads to one.  A hyphen or apostrophe that a letter or
                    # underscore follows would continue the name of a variable
                    my $next := @segments[$i + 1];
                    $text = '\\' ~ $text
                      if $interpolated
                      && (nqp::index('([{<',$text.substr(0,1)) >= 0
                           || (nqp::istype(@segments[$i - 1],RakuAST::Var)
                                && nqp::index(q/-'/,$text.substr(0,1)) >= 0
                                && (nqp::iscclass(
                                      nqp::const::CCLASS_ALPHABETIC,$text,1
                                    ) || nqp::eqat($text,'_',1)))
                           || self.dot-continues-interpolation(
                                $text,
                                $next.defined
                                  && !nqp::istype($next,RakuAST::StrLiteral)
                                  && !nqp::istype($next,RakuAST::QuotedString)
                              ));
                    $interpolated = 0;
                }
                $text = $text.subst($*QUOTE-DELIMITER, '\\' ~ $*QUOTE-DELIMITER, :g)
                  if $*QUOTE-DELIMITER;
                @parts.push($text);
            }
            # the text between a nested pair of the delimiters is a quote of
            # its own, one without processors belongs to this one
            elsif nqp::istype($segment,RakuAST::QuotedString)
              && !$segment.processors {
                $interpolated = 0;
                @parts.push(self.assemble-quoted-string($segment, :$raw));
            }
            # one with the processors of this one is text as well, and
            # the quotewords processor starts a word at it and ends one
            # after it, which spaces do when there are no delimiters to
            # nest
            elsif nqp::istype($segment,RakuAST::QuotedString)
              && $segment.processors eqv $ast.processors {
                $interpolated = 0;
                my str $group = self.assemble-quoted-string($segment, :$raw);
                @parts.push($ast.processors.first('quotewords')
                  ?? " $group "
                  !! $group
                );
            }
            else {
                # a closure ends the interpolation
                $interpolated = nqp::istype($segment,RakuAST::Block) ?? 0 !! 1;
                # a method call only interpolates with its parentheses
                my $*INTERPOLATING := nqp::istype($segment,RakuAST::ApplyPostfix)
                  || nqp::istype($segment,RakuAST::ApplyDottyInfix);
                my $*QUOTE-DELIMITER := '';
                @parts.push(self.deparse($segment));
            }
        }
        @parts.join
    }

    # Whether text that starts with a dot would continue the
    # interpolation before it: a chain of method names, each with or
    # without a dispatch prefix, that ends in a bracket or a quote, or
    # in a dot when an interpolation follows the text
    method dot-continues-interpolation(
      str $text,
          $next-interpolates
    --> Bool:D) {
        my int $chars = nqp::chars($text);
        my int $i;
        while $i < $chars && nqp::eqat($text,'.',$i) {
            ++$i;
            return True if $i == $chars && $next-interpolates;
            ++$i if $i < $chars
              && nqp::index('?&^*+=',nqp::substr($text,$i,1)) >= 0;
            return True if $i < $chars
              && nqp::index(q/([{<'"/,nqp::substr($text,$i,1)) >= 0;

            # a method name, a hyphen or apostrophe that a letter follows
            # and a package separator are part of it
            my int $start = $i;
            loop {
                $i = nqp::findnotcclass(
                  nqp::const::CCLASS_WORD,$text,$i,$chars - $i
                );
                if nqp::eqat($text,'::',$i) {
                    $i = $i + 2;
                }
                elsif $i + 1 < $chars
                  && nqp::index(q/-'/,nqp::substr($text,$i,1)) >= 0
                  && nqp::iscclass(nqp::const::CCLASS_ALPHABETIC,$text,$i + 1) {
                    ++$i;
                }
                else {
                    last;
                }
            }
            return False if $i == $start;
            return True if $i < $chars
              && nqp::index('([{<',nqp::substr($text,$i,1)) >= 0;
        }
        False
    }

    method multiple-processors(str $string, @processors --> Str:D) {
        self.hsyn('quote-lang-qq', self.xsyn('quote-lang',"qq"))
          ~ "@processors.map({
              my str $processor = %processor-attribute{$_}
                // NYI("String processors '$_'");
              ':' ~ self.hsyn(
                      "adverb-q-$processor",
                      self.xsyn('adverb-q', $processor)
                    )
            }).join()/$string/"
    }

    method branches(RakuAST::Regex::Branching:D $ast, str $joiner --> Str:D) {
        if $ast.branches -> @branches {
            @branches.map({ self.deparse($_) }).join($joiner)
        }
        else {
            ''
        }
    }

    # Word characters never need escaping inside <[ ]>.  Everything else
    # is escaped rather than enumerating which characters are ignored or
    # meaningful there, such as whitespace, dots, hyphens, backslashes
    # and the closing bracket.  A character that does not print is given
    # by its codepoint, as a backslashed control character does not parse
    method charclass-character(str $char --> Str:D) {
        return $char if nqp::iscclass(nqp::const::CCLASS_WORD,$char,0);
        nqp::iscclass(nqp::const::CCLASS_PRINTING,$char,0)
          ?? '\\' ~ $char
          !! '\\x[' ~ $char.ord.base(16) ~ ']'
    }

    method colonpairs($ast, Str:D $xsyn = "") {
        $ast.colonpairs.map({ self.deparse($_, $xsyn) }).join
    }

    method named-arg($xsyn, str $key) {
        $xsyn
          ?? self.hsyn("named-$key", self.xsyn($xsyn,$key))
          !! $key
    }

    method quantifier(
      RakuAST::Regex::Quantifier:D $ast, str $quantifier
    --> Str:D) {
        $quantifier ~ self.deparse($ast.backtrack)
    }

    method parenthesize($ast, :$only-non-empty --> Str:D) {
        # a declaration inside the parens would add the statement delimiter
        my $*DELIMITER = '';
        my str $deparsed = $ast.defined ?? self.deparse($ast).chomp !! '';
        $deparsed || !$only-non-empty
          ?? $.parens-open ~ $deparsed ~ $.parens-close
          !! $deparsed
    }

    # A block or control statement ends in a newline the enclosing
    # statement supplies again, an expression statement may end in a
    # heredoc body that has to stay intact
    method blorst($blorst --> Str:D) {
        my $*DELIMITER = '';
        my str $deparsed = self.deparse($blorst);
        nqp::istype($blorst,RakuAST::Statement::Expression)
          ?? $deparsed
          !! $deparsed.chomp
    }

    method assignee($ast --> Str:D) {
        my $assignee := $ast.assignee;
        nqp::isconcrete($assignee)
          ?? self.syn-infix-ws($.assign) ~ self.deparse($assignee)
          !! ''
    }

    method bracketize($ast --> Str:D) {
        my $*DELIMITER = '';
        $.bracket-open
          ~ ($ast.defined ?? self.deparse($ast) !! '')
          ~ $.bracket-close
    }

    method squarize($ast --> Str:D) {
        my $*DELIMITER = '';
        $.square-open
          ~ ($ast.defined ?? self.deparse($ast) !! '')
          ~ $.square-close
    }

    # An operand that deparses to more than a single term must be
    # parenthesized under a postfix, or the postfix binds to its last
    # term only
    method is-numeric-literal($node --> Bool:D) {
        nqp::istype($node,RakuAST::IntLiteral)
          || nqp::istype($node,RakuAST::NumLiteral)
          || nqp::istype($node,RakuAST::RatLiteral)
          || nqp::istype($node,RakuAST::ComplexLiteral)
          ?? True
          !! False
    }

    method postfix-operand-needs-parens($operand, $postfix? --> Bool:D) {
        nqp::istype($operand,RakuAST::ApplyInfix)
          || nqp::istype($operand,RakuAST::ApplyListInfix)
          || nqp::istype($operand,RakuAST::ApplyDottyInfix)
          || nqp::istype($operand,RakuAST::ApplyPrefix)
          || nqp::istype($operand,RakuAST::Ternary)
          || nqp::istype($operand,RakuAST::FatArrow)
          # a declaration binds tighter than a postfix.  Only an initializer
          # or a subscript that would read as a shape needs the parentheses.
          # After an anonymous declaration a dot would read as a twigil
          || (nqp::istype($operand,RakuAST::VarDeclaration::Simple)
               && ($operand.initializer
                    || nqp::istype($postfix,RakuAST::Postcircumfix)
                    || nqp::istype($operand,RakuAST::VarDeclaration::Anonymous)))
          # a subscript would read as the value of the pair
          || (nqp::istype($operand,RakuAST::ColonPair)
               && nqp::istype($postfix,RakuAST::Postcircumfix))
          || nqp::istype($operand,RakuAST::VarDeclaration::Term)
          || nqp::istype($operand,RakuAST::VarDeclaration::Constant)
          || nqp::istype($operand,RakuAST::VarDeclaration::Signature)
          || nqp::istype($operand,RakuAST::Block)
          || nqp::istype($operand,RakuAST::Routine)
          || nqp::istype($operand,RakuAST::StatementPrefix)
          || nqp::istype($operand,RakuAST::Term::Reduce)
          || nqp::istype($operand,RakuAST::Call::Name::WithoutParentheses)
          ?? True
          !! False
    }

    # True only for a statement prefix over a block or a control
    # statement with no modifier, the one statement whose closing brace
    # may end it without a delimiter.  Any other trailing brace may close
    # a subscript, a closure or a hash composer and keeps it
    method statement-is-prefixed-block($statement --> Bool:D) {
        if nqp::istype($statement,RakuAST::Statement::Expression)
          && !$statement.condition-modifier
          && !$statement.loop-modifier {
            my $expression := $statement.expression;
            nqp::istype($expression,RakuAST::StatementPrefix)
              && !nqp::istype($expression.blorst,RakuAST::Statement::Expression)
              ?? True
              !! False
        }
        else {
            False
        }
    }

    method meta-infix-letter($ast, str $letter --> Str:D) {
        self.hsyn("meta-$letter", self.xsyn('meta',$letter))
          ~ self.deparse($ast.infix)
    }

    # a dotty infix supplies the dot of the method call it applies
    method method-dot(str $dot --> Str:D) {
        my str $shown = $dot;
        if $*DOTTY-INFIX {
            $shown = $dot.substr(1);
            $*DOTTY-INFIX = False;  # the arguments have their own dots
        }
        $shown ?? self.syn-routine($shown) !! ''
    }

    method dotty-right($ast --> Str:D) {
        if nqp::istype($ast,RakuAST::Call::Methodish) {
            my $*DOTTY-INFIX = True;
            self.deparse($ast)
        }
        # a postcircumfix has no dot to leave to the infix
        else {
            self.deparse($ast)
        }
    }

    method method-call(
      $ast, str $dot, $macroish?, :$xsyn, :$only-non-empty
    --> Str:D) {
        my str $dot-syn = self.method-dot($dot);
        my $name := (nqp::istype($_,Str) ?? $_ !! self.deparse($_))
          with $ast.name;

        $dot-syn
          ~ ($xsyn
              ?? self.hsyn("core-$name", self.xsyn('core', $name))
              !! $name
            )
          ~ ($macroish && !$*INTERPOLATING
              ?? ''
              !! self.parenthesize(
                   $ast.args,
                   :only-non-empty($only-non-empty && !$*INTERPOLATING)
                 )
            )
    }

    method quote-if-needed(str $literal) {
        my int $find = nqp::findnotcclass(
          nqp::const::CCLASS_WORD,$literal,0,nqp::chars($literal)
        );
        nqp::chars($literal) && $find == nqp::chars($literal)
          && !$*QUOTE-REGEX-WORD
          ?? $literal       # just word chars
          !! $literal.raku  # need quoting, an empty literal too
    }

    method deparse-unquoted($ast) {
        if nqp::istype($ast,Str) {
            $ast
        }
        elsif nqp::istype($ast,RakuAST::StrLiteral) {
            $ast.value
        }
        else {
            my $literal := self.deparse($ast);
            $literal.starts-with(Q/"/) && $literal.ends-with(Q/"/)
              || $literal.starts-with(Q/'/) && $literal.ends-with(Q/'/)
              ?? $literal.substr(1,*-1)
              !! $literal
        }
    }

    method labels(RakuAST::Statement:D $ast) {
        $ast.labels.map({ self.deparse($_) }).join
    }

    method use-no(str $what, $ast) {
        my str @parts =
          self.hsyn("use-$what", self.xsyn('use', $what)),
          ' ',
          self.deparse($ast.module-name);

        if $ast.argument -> $argument {
            @parts.push(' ');
            @parts.push(self.deparse($argument));
        }

        self.labels($ast) ~ @parts.join
    }

    method prefix-any-leading-doc(str $body, $WHY) {
        if $WHY && $WHY.leading -> @leading {
            # the parser stores a leading doc line without its newline
            self.hsyn('doc-leading', @leading.map({
                self.deparse-unquoted($_).lines.Slip
            }).map({
                "#| $_\n$*INDENT"
            }).join)
              ~ $body
        }
        else {
            $body
        }
    }

    method postfix-any-trailing-doc(str $body, $WHY) {
        if $WHY && $WHY.trailing -> @trailing {
            my str @lines = @trailing.map: {
                self.deparse-unquoted($_).lines.Slip
            }
            ($body ~ $*DELIMITER).chomp
              ~ (@lines > 1 ?? "\n" !! ' ')
              ~ self.hsyn(
                  'doc-trailing',
                  @lines.map({ "#= $_" }).join("$*INDENT\n")
                )
              ~ "\n"
        }
        else {
            $body ~ $*DELIMITER
        }
    }

    # a trailing doc follows the opening brace on its line, the body
    # supplies the newline after the brace
    method block-with-docs(str $prefix, $WHY, $body --> Str:D) {
        my $*DELIMITER = "";
        self.add-any-docs($prefix, $WHY).chomp
          ~ self.deparse($body, :multi).substr(1)  # lose {
    }

    method add-any-docs(str $body, $WHY) {
        self.postfix-any-trailing-doc(
          self.prefix-any-leading-doc($body, $WHY), $WHY
        )
    }

    method where-constraint($where --> Str:D) {
        ' ' ~ self.xsyn('constraint', 'where') ~ ' ' ~ self.deparse($where)
    }

    method statement-modifier(str $type, $ast) {
        my $*DELIMITER = '';
        self.syn-modifier($type) ~ ' ' ~ self.deparse($ast.expression)
    }

    method syn-block(str $type) {
        self.hsyn("block-$type", self.xsyn('block', $type))
    }

    method syn-infix-ws(Str:D $operator) {
        my str $trimmed = $operator.trim;
        $operator.leading-whitespace
          ~ self.hsyn("infix-$trimmed", self.xsyn('infix', $trimmed))
          ~ $operator.trailing-whitespace
    }

    method syn-modifier(str $type) {
        self.hsyn("modifier-$type", self.xsyn('modifier', $type))
    }

    method syn-multi(str $type) {
        self.hsyn("multi-$type", self.xsyn('multi', $type))
    }

    method syn-package(str $declarator) {
        self.hsyn("package-$declarator", self.xsyn('package', $declarator))
    }

    method syn-phaser(str $phaser) {
        self.hsyn("phaser-$phaser", self.xsyn('phaser', $phaser))
    }

    method syn-routine(str $type) {
        self.hsyn("routine-$type", self.xsyn('routine', $type))
    }

    method syn-scope(str $scope) {
        self.hsyn("scope-$scope", self.xsyn('scope', $scope))
    }

    method syn-trait(str $trait) {
        self.hsyn("traitmod-$trait", self.xsyn('traitmod', $trait))
    }

    method syn-type($ast, :$skip) {
        # a derived type, such as a coercion, deparses its base type
        # through this method, so it must not be highlighted twice
        my int $named = nqp::istype($ast,RakuAST::Type::Simple)
          || nqp::istype($ast,RakuAST::Type::Setting);
        my str $name  = self.deparse($named ?? $ast.name !! $ast);

        return "" if $skip && $skip eq $name;
        $named ?? self.hsyn("type-$name", $name) !! $name
    }

    method syn-typer($typer) {
        self.hsyn("typer-$typer", self.xsyn('typer', $typer))
    }

    # an attribute declared without a twigil keeps its bare name, .name
    # adds the twigil
    method var-declaration(RakuAST::VarDeclaration::Simple:D
      $ast, str $name = $ast.sigil ~ $ast.twigil ~ $ast.desigilname.canonicalize
    ) {
        my str @parts;

        @parts.push(self.syn-scope($ast.scope));
        @parts.push(' ');

        if $ast.original-type -> $type {
            if self.syn-type($type) -> $the-type {
                @parts.push($the-type);
                @parts.push(' ');
            }
        }

        my str $twigil = $ast.twigil;
        @parts.push(
          self.hsyn(%twigil2type{$twigil} // 'var-lexical', $name)
        );
        @parts.push($ast.sigil eq '%'
          ?? self.bracketize($_)
          !! self.squarize($_)
        ) with $ast.shape;

        if $ast.traits.grep({
            nqp::not_i(nqp::istype($_,RakuAST::Trait::WillBuild))
        }) -> @traits {
            for @traits {
                @parts.push(' ');
                @parts.push(self.deparse($_));
            }
        }

        @parts.push(self.where-constraint($_)) with $ast.where;

        if $ast.initializer -> $initializer {
            @parts.push(self.deparse($initializer));
        }

        @parts.join
    }

#- A ---------------------------------------------------------------------------

    multi method deparse(RakuAST::ApplyInfix:D $ast --> Str:D) {
        # a declaration operand would add the statement delimiter
        my $*DELIMITER = '';
        my str $deparsed = self.deparse($ast.left)
          ~ $.before-infix
          ~ self.deparse($ast.infix)
          ~ $.after-infix
          ~ self.deparse($ast.right);

        if $ast.colonpairs -> @pairs {
            "$deparsed @pairs.map({ self.deparse($_) }).join()"
        }
        else {
            $deparsed
        }
    }

    multi method deparse(RakuAST::ApplyDottyInfix:D $ast --> Str:D) {
        my $*DELIMITER = '';
        my str $infix = self.deparse($ast.infix);
        # whitespace around the infix would end an interpolation
        $infix = $infix.trim if $*INTERPOLATING;
        self.deparse($ast.left) ~ $infix ~ self.dotty-right($ast.right)
    }

    multi method deparse(RakuAST::ApplyListInfix:D $ast --> Str:D) {
        my $*DELIMITER = '';
        my $infix       := $ast.infix;
        my str $operator = self.deparse($infix);

        my str @parts = $ast.operands.map({ self.deparse($_) });
        @parts
          ?? nqp::istype($infix,RakuAST::Infix) && $infix.operator eq ','
            ?? @parts == 1
              ?? @parts.head ~ $.list-infix-comma.trim-trailing
              !! @parts.join($.list-infix-comma)
            !! @parts.join(
                 $.before-list-infix ~ $operator ~ $.after-list-infix
               )
          !! ''  # XXX ???
    }

    multi method deparse(RakuAST::ApplyPostfix:D $ast --> Str:D) {
        # a declaration operand would add the statement delimiter
        my $*DELIMITER = '';
        my     $postfix         := $ast.postfix;
        my str $deparsed-postfix = self.deparse($postfix);

        # a method call on the topic interpolates only with the topic written
        if $ast.on-topic
          && nqp::istype($postfix,RakuAST::Call::Method)
          && !$*INTERPOLATING {
            $deparsed-postfix
        }
        else {
            my $operand := $ast.operand;
            # a number followed by a dot and a colon reads as a broken decimal
            my str $deparsed-operand = self.postfix-operand-needs-parens($operand, $postfix)
              || (self.is-numeric-literal($operand)
                   && self.deparse-without-highlighting($postfix).starts-with('.::'))
              ?? self.parenthesize($operand)
              !! self.deparse($operand);

            # an imaginary postfix after a letter would become part of the
            # name, and so would one after the digit or underscore that
            # ends anything but a number
            if nqp::istype($postfix,RakuAST::Postfix)
              && $postfix.operator eq 'i' {
                my int $last = nqp::chars($deparsed-operand) - 1;
                $deparsed-operand ~= '\\'
                  if nqp::iscclass(
                       nqp::const::CCLASS_ALPHABETIC,$deparsed-operand,$last
                     )
                  || (!self.is-numeric-literal($operand)
                       && nqp::iscclass(
                            nqp::const::CCLASS_WORD,$deparsed-operand,$last
                          ));
            }

            $deparsed-operand
              # a term followed by a bare argument list is a routine call
              ~ (nqp::istype($postfix,RakuAST::Call::Term)
                  && nqp::istype($operand,RakuAST::Term::Name)
                  ?? '.'
                  !! ''
                )
              ~ $deparsed-postfix
        }
    }

    multi method deparse(RakuAST::ApplyPrefix:D $ast --> Str:D) {
        # a declaration as the operand would write the statement delimiter
        my $*DELIMITER = '';
        my str $prefix = self.deparse($ast.prefix);
        self.hsyn("prefix-$prefix", self.xsyn('prefix', $prefix))
          ~ self.deparse($ast.operand)
    }

    multi method deparse(RakuAST::ArgList:D $ast --> Str:D) {
        my $*IN-ARGLIST := True;
        my $*INTERPOLATING := False;
        # a declaration argument would add the statement delimiter
        my $*DELIMITER = '';
        $ast.args.map({
            nqp::istype($_,RakuAST::ColonPair)
              ?? self.deparse($_, "named")
              !! self.deparse($_)
        }).join($.list-infix-comma)
    }

#- B ---------------------------------------------------------------------------

    multi method deparse(RakuAST::Block:D $ast --> Str:D) {
        if $ast.WHY -> $WHY {
            self.block-with-docs('{', $WHY, $ast.body)
        }
        else {
            self.deparse($ast.body, |%_)
        }
    }

    multi method deparse(RakuAST::BracketedInfix:D $ast --> Str:D) {
        '[' ~ self.deparse($ast.infix) ~ ']'
    }

    multi method deparse(RakuAST::Blockoid:D $ast, :$multi, :$unit --> Str:D) {
        my $statement-list := $ast.statement-list;

        if $unit {
            self.deparse($statement-list)
        }
        else {
            my @statements := $statement-list.statements;
            my $in-arglist := $*IN-ARGLIST.Bool;

            if $multi || @statements {
                # Deeper deparsing assumes not in an argument list
                my $*IN-ARGLIST := False;

                if @statements == 1 && $in-arglist && !$multi {
                    my $*DELIMITER = '';
                    $.bracket-open
                      ~ ' '
                      ~ self.deparse(@statements.head).trim
                      ~ ' '
                      ~ $.bracket-close
                }
                else {
                    self.indent;
                    $.block-open
                      ~ self.deparse($statement-list)
                      ~ self.dedent
                      ~ $.bracket-close
                }
            }
            else {
                "$.bracket-open $.bracket-close"
            }
        }
    }

#- Call ------------------------------------------------------------------------

    multi method deparse(RakuAST::Call::MetaMethod:D $ast --> Str:D) {
        self.method-call($ast, '.^', :only-non-empty)
    }

    multi method deparse(RakuAST::Call::Methodish:D $ast --> Str:D) {
        self.method-call(
          $ast, ($ast.dispatch || '.'), $ast.macroish, :xsyn, :only-non-empty
        )
    }

    multi method deparse(RakuAST::Call::PrivateMethod:D $ast --> Str:D) {
        self.method-call($ast, '!', :only-non-empty)
    }

    multi method deparse(RakuAST::Call::QuotedMethod:D $ast --> Str:D) {
        self.method-call($ast, $ast.dispatch || '.')
    }

    multi method deparse(RakuAST::Call::BlockMethod:D $ast --> Str:D) {
        my $block := $ast.block;
        my str $dot-syn = self.method-dot($ast.dispatch || '.');
        # the parser wraps the block of `.&{ }` in an item contextualizer,
        # the code of `.&( )` in a statement sequence inside it
        $dot-syn
          ~ (nqp::istype($block,RakuAST::Contextualizer::Item)
              ?? '&' ~ self.context-target($block.target)
              !! self.deparse($block)
            )
          ~ self.parenthesize($ast.args, :only-non-empty(!$*INTERPOLATING))
    }

    multi method deparse(RakuAST::Call::VarMethod:D $ast --> Str:D) {
        my $dispatch := $ast.dispatch;
        self.method-call($ast, ($ast.dispatch || '.') ~ '&')
    }

    multi method deparse(RakuAST::Call::Name:D $ast --> Str:D) {
        my $name-ast := $ast.name;
        my $args     := $ast.args;
        my $name     := self.deparse($name-ast);
        # an indirect lookup without arguments is a term, not a call
        my $complete := $name.ends-with('::')
          || !($args && $args.args) && $name-ast.is-indirect-lookup;

        $name := self.hsyn("core-$name", self.xsyn('core', $name));
        $complete
          ?? $name
          !! $name ~ self.parenthesize($args)
    }

    multi method deparse(RakuAST::Call::Name::WithoutParentheses:D $ast
    --> Str:D) {
        my $name := self.deparse($ast.name);
           $name := self.hsyn("core-$name", self.xsyn('core', $name));
        my $args := $ast.args.defined ?? self.deparse($ast.args).chomp !! '';

        $args ?? "$name $args" !! $name
    }

    multi method deparse(RakuAST::Call::Term:D $ast --> Str:D) {
        self.parenthesize($ast.args)
    }

#- Circumfix -------------------------------------------------------------------

    multi method deparse(RakuAST::Circumfix::ArrayComposer:D $ast --> Str:D) {
        self.squarize($ast.semilist)
    }

    multi method deparse(RakuAST::Circumfix::HashComposer:D $ast --> Str:D) {
        self.bracketize($ast.expression)
    }

    multi method deparse(RakuAST::Circumfix::Parentheses:D $ast --> Str:D) {
        self.parenthesize($ast.semilist)
    }

#- ColonPair -------------------------------------------------------------------

    multi method deparse(RakuAST::ColonPair:D $ast, Str $xsyn = "" --> Str:D) {
        my str $key = $ast.named-arg-name;

        ':'
          ~ ($xsyn
              ?? self.hsyn("named-$key",self.xsyn($xsyn,$key))
              !! $key
            )
          ~ $.parens-open
          ~ self.deparse($ast.named-arg-value)
          ~ $.parens-close
    }

    multi method deparse(RakuAST::ColonPairs:D $ast, Str $xsyn = "" --> Str:D) {
        self.colonpairs($ast, $xsyn)
    }

    multi method deparse(
      RakuAST::ColonPair::False:D $ast, Str:D $xsyn = ""
    --> Str:D) {
        ':!' ~ self.named-arg($xsyn, $ast.key)
    }

    multi method deparse(
      RakuAST::ColonPair::Number:D $ast, Str:D $xsyn = ""
    --> Str:D) {
        ':' ~ self.deparse($ast.value) ~ self.named-arg($xsyn, $ast.key)
    }

    multi method deparse(
      RakuAST::ColonPair::True:D $ast, Str:D $xsyn = ""
    --> Str:D) {
        ':' ~ self.named-arg($xsyn, $ast.key)
    }

    multi method deparse(
      RakuAST::ColonPair::Value:D $ast, Str:D $xsyn = ""
    --> Str:D) {
        my $value        := $ast.value;
        my str $deparsed  = self.deparse($value);

        ':'
          ~ self.named-arg($xsyn, $ast.key)
          ~ (nqp::istype($value,RakuAST::Circumfix::Parentheses)
               || (nqp::istype($value,RakuAST::QuotedString)
                    && $deparsed.starts-with('<'))
              ?? $deparsed
              !! $.parens-open ~ $deparsed ~ $.parens-close
            )
    }

    multi method deparse(RakuAST::ColonPair::Variable:D $ast --> Str:D) {
        ':' ~ self.deparse($ast.value)
    }

    multi method deparse(RakuAST::Constant:D $ast --> Str:D) {
        $ast.deparse
    }

#- Co --------------------------------------------------------------------------

    multi method deparse(RakuAST::CompUnit:D $ast --> Str:D) {
        my str $deparsed = self.deparse($ast.statement-list, :no-sink);
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
        self.xsyn('scope', $ast.scope)
    }

    multi method deparse(
      RakuAST::Declaration::ResolvedConstant:D $ast
    --> Str:D) {
        $ast.compile-time-value.raku
    }

#- Doc -------------------------------------------------------------------------

    multi method deparse(RakuAST::Doc::Block:D $ast --> Str:D) {
        my str $margin = $ast.margin;
        my str $type  = $ast.type;
        my str $name  = $type ~ $ast.level;

        # highlighting shortcuts
        sub config(   str $s) { self.hsyn('rakudoc-config',    $s) }
        sub content(  str $s) { self.hsyn('rakudoc-content',   $s) }
        sub directive(str $s) { self.hsyn('rakudoc-directive', $s) }
        sub id(       str $s) { self.hsyn('rakudoc-id',        $s) }
        sub type(     str $s) { self.hsyn('rakudoc-type',      $s) }

        # handle =alias directive
        if $type eq 'alias' {
            my ($lemma, @paragraphs) = $ast.paragraphs;

            my $paragraph = do if @paragraphs.elems == 1
              && nqp::istype(@paragraphs.head,Str) {
                @paragraphs.head.chomp
            }
            else {
                @paragraphs.map({
                  nqp::istype($_,Str)
                    ?? $_
                    !! self.deparse-without-highlighting($_)
                }).join.chomp
            }

            # set up prefix for additional lines
            my str $prefix =
              "\n" ~ $margin ~ directive("=") ~ (" " x "$name $lemma ".chars);


            return $margin
              ~ directive("=$name")
              ~ " "
              ~ id($lemma)
              ~ " "
              ~ content($paragraph.subst("\n", $prefix, :global))
              ~ "\n";
        }

        # handle =defn blocks
        my $abbreviated := $ast.abbreviated;
        if $type eq 'defn' {
            my     @paras = $ast.paragraphs;
            my str $lemma = (nqp::istype($_,Str)
              ?? $_
              !! self.deparse-without-highlighting($_)
            ) with @paras.shift;

            my str $spec =
              id($lemma) ~ "\n" ~ content(@paras.map({
                  nqp::istype($_,Str)
                    ?? "$margin$_"
                    !! self.deparse-without-highlighting($_)
              }).join.chomp) ~ "\n";

            if $abbreviated {
                return $margin ~ type("=$name") ~ " " ~ $spec;
            }
            else {
                $name = " " ~ type($name);

                return $margin ~ ($ast.for
                  ?? directive("=for") ~ "$name\n$margin$spec\n"
                  !! directive("=begin") ~ "$name\n\n$margin$spec"
                       ~ $margin ~ directive("=end") ~ "$name\n\n"
                );
            }
        }

        # preprocess any config
        my %config    := $ast.config;
        my str @config = %config.sort({
            .key eq 'numbered' ?? '' !! .key  # numbered always first
        }).map: {
            my str $key = .key;
            if $key eq 'numbered' && $abbreviated {
                '#'
            }

            # =place uri :config
            elsif $key eq 'uri' && $type eq 'place' {
                Empty
            }

            # =table with header
            elsif $key eq 'header-row' && $type eq 'table' | 'numtable' {
                Empty
            }

            else {
                my $value    := .value;
                my $deparsed := self.deparse($value);
                if nqp::istype($value,RakuAST::Term::Enum) {
                    my $name := $value.name.canonicalize;
                    $name eq 'True'
                      ?? ":$key"
                      !! $name eq 'False'
                        ?? ":!$key"
                        !! ":$key$deparsed"
                }
                else {
                    ":$key$deparsed"
                }
            }
        }

        # handle =config / =place directives
        if $type eq 'config' | 'place' {
            my str $id     = $type eq 'config'
              ?? $ast.paragraphs.head
              !! %config<uri>.value;
            my str $prefix = $margin ~ directive("=$name") ~ " " ~ id($id);

            if @config {
                my str $spaces = " " x "$name $id ".chars;
                return $prefix
                  ~ " "
                  ~ config(@config.shift)
                  ~ "\n"
                  ~ @config.map({
                        $margin ~ directive("=") ~ $spaces ~ config($_) ~ "\n"
                    }).join;
            }
            else {
                return $prefix ~ "\n"
            }
        }

        my str $config = @config.join(' ');
        $config = ' ' ~ self.hsyn('rakudoc-config', $config) if $config;

        # handle =row / =column directives
        if $type eq 'row' | 'column' {
            return $margin ~ directive("=$name") ~ $config ~ "\n";
        }

        # set up paragraphs
        if $ast.visual-table {
            my str $type     = " " ~ type($name);
            my str $deparsed = $margin ~ ($abbreviated
              ?? type("=$name") ~ "$config\n"
              !! $ast.for
                ?? directive("=for") ~ "$type$config\n"
                !! directive("=begin") ~ "$type$config\n\n"
            ) ~ $ast.paragraphs.map({
                $margin ~ (nqp::istype($_,RakuAST::Doc::LegacyRow)
                  ?? self.deparse($_)
                  !! self.hsyn('rakudoc-divider',.chomp) ~ "\n"
                )
            }).join;

            return $abbreviated || $ast.for
              ?? "$deparsed\n"
              !! ("$deparsed\n" ~ $margin ~ directive("=end") ~ $type ~ "\n\n")
        }

        # standard paragraphs handling from here on
        my str $paragraphs = $ast.paragraphs.map({
            nqp::istype($_,RakuAST::Doc::Block)
              ?? self.deparse($_)
              !! (nqp::istype($_,Str) ?? $_ !! self.deparse($_))
                   .lines(:!chomp).map({
                       $_ eq "\n" ?? $_ !! "$margin$_"
                   }).join
        }).join;

        # handle implicite code blocks
        if $type eq 'implicit-code' {

            # implicit code blocks are only recognized by their indentation,
            # the paragraph text holds the blank line that ends the block
            self.hsyn('rakudoc-code', $paragraphs.chomp) ~ "\n"
        }

        # other blocks with paragraphs
        elsif $paragraphs {
            my str $style = $type eq 'code'
              ?? 'rakudoc-code'
              !! $type eq 'comment' | 'data' | 'input' | 'output'
                ?? 'rakudoc-verbatim'
                !! '';

            $paragraphs = $paragraphs.substr($margin.chars).chomp;
            $paragraphs = self.hsyn($style, $paragraphs) if $style;

            if $abbreviated {
                # a paragraph can be just the blank line that ends the block
                $margin ~ type("=$name") ~ $config
                  ~ ($paragraphs ?? " $paragraphs\n" !! "\n\n")
            }
            else {
                $name       = " " ~ type($name);
                $paragraphs = "$margin$paragraphs\n";

                $margin ~ ($ast.for
                  ?? directive("=for") ~ "$name$config\n$paragraphs"
                  !! directive("=begin") ~ "$name$config\n\n$paragraphs"
                       ~ $margin ~ directive("=end") ~ $name ~ "\n\n"
                )
            }
        }

        # other blocks *without* paragraphs
        else {

            if $abbreviated {
                $margin ~ type("=$name") ~ $config ~ "\n"
            }
            else {
                $name = " " ~ type($name);

                $margin ~ ($ast.for
                  ?? directive("=for") ~ "$name$config\n"
                  !! directive("=begin") ~ "$name$config\n\n"
                       ~ $margin ~ directive("=end") ~ $name ~ "\n\n"
                )
            }
        }
    }

    multi method deparse(RakuAST::Doc::Declarator:D $ast --> Str:D) {
        (my $wherefore := nqp::clone($ast.WHEREFORE)).set-WHY($ast);
        self.deparse($wherefore).chomp
    }

    multi method deparse(RakuAST::Doc::Markup:D $ast --> Str:D) {
        self.hsyn("markup-$ast.letter()", $ast.Str)
    }

    multi method deparse(RakuAST::Doc::Paragraph:D $ast --> Str:D) {
        $ast.atoms.map({ self.deparse-unquoted($_) }).join
    }

    multi method deparse(RakuAST::Doc::LegacyRow:D $ast --> Str:D) {
        $ast.Str: { self.hsyn('rakudoc-divider', $_) }
    }

#- Dot -------------------------------------------------------------------------

    multi method deparse(RakuAST::DottyInfix::Call:D $ --> Str:D) {
        $.dotty-infix-call
    }

    multi method deparse(RakuAST::DottyInfix::CallAssign:D $ --> Str:D) {
        $.dotty-infix-call-assign
    }

#- F ---------------------------------------------------------------------------

    multi method deparse(RakuAST::FakeSignature:D $ast --> Str:D) {
        ':' ~ self.parenthesize($ast.signature)
    }

    multi method deparse(RakuAST::FatArrow:D $ast --> Str:D) {
        $ast.key ~ $.fatarrow ~ self.deparse($ast.value)
    }

    multi method deparse(RakuAST::FunctionInfix:D $ast --> Str:D) {
        $.function-infix-open
          ~ self.deparse($ast.function)
          ~ $.function-infix-close
    }

#- H ---------------------------------------------------------------------------

    # each body goes after the line that holds its opener, which is the
    # line after the text when the opener is on its last line
    method insert-heredocs(str $text, @heredocs --> Str:D) {
        my str $result = $text;
        my int $from;
        for @heredocs {
            my str $top    = .key;
            my str $bottom = .value;
            my int $at = nqp::index($result,$top,$from);
            my int $nl = $at < 0 ?? -1 !! nqp::index($result,"\n",$at);
            if $nl < 0 {
                $result = $result
                  ~ ($result.ends-with("\n") ?? '' !! "\n")
                  ~ $bottom;
                $from   = nqp::chars($result);
            }
            else {
                $result = nqp::substr($result,0,$nl + 1)
                  ~ $bottom
                  ~ nqp::substr($result,$nl + 1);
                $from   = $nl + 1 + nqp::chars($bottom);
            }
        }
        $result
    }

    multi method deparse(RakuAST::Heredoc:D $ast --> Str:D) {
        my $string := self.assemble-quoted-string($ast);
        my @processors = $ast.processors;
        @processors.push('heredoc');

        my $stop   := $ast.stop;
        my $indent := $stop eq "\n"
          ?? ''
          !! " " x ($stop.chars - $stop.trim-leading.chars);

        my $top := self.multiple-processors($stop.trim, @processors);
        my $bottom := $string.chomp('\n').split(Q/\n/).map({
            $_ ?? "$indent$_\n" !! "\n"
        }).join ~ $stop;

        # a statement list places the bodies of the heredocs in a statement
        my $heredocs := nqp::getlexdyn('@*HEREDOCS');
        if nqp::isnull($heredocs) {
            "$top\n$bottom"
        }
        else {
            $heredocs.push($top => $bottom);
            $top
        }
    }

#- I ---------------------------------------------------------------------------

    # Also for ::FlipFlop
    multi method deparse(RakuAST::Infix:D $ast --> Str:D) {
        my str $operator = $ast.operator;
        self.hsyn("infix-$operator", self.xsyn('infix', $operator))
    }

    # a declaration as the value of an assignment or a bind would write
    # the statement delimiter
    multi method deparse(RakuAST::Initializer::Assign:D $ast --> Str:D) {
        my $*DELIMITER = '';
        self.syn-infix-ws($.assign) ~ self.deparse($ast.expression)
    }

    multi method deparse(RakuAST::Initializer::Bind:D $ast --> Str:D) {
        my $*DELIMITER = '';
        self.syn-infix-ws($.bind) ~ self.deparse($ast.expression)
    }

    multi method deparse(RakuAST::Initializer::CallAssign:D $ast --> Str:D) {
        self.syn-infix-ws($.dotty-infix-call-assign)
          ~ self.dotty-right($ast.postfixish)
    }

#- L ---------------------------------------------------------------------------

    multi method deparse(RakuAST::Label:D $ast --> Str:D) {
        self.hsyn('label', $ast.name ~ ': ')
    }

    # handles all RakuAST::xxxLiteral classes
    multi method deparse(RakuAST::Literal:D $ast --> Str:D) {
        self.hsyn('literal', $ast.value.raku)
    }

#- M ---------------------------------------------------------------------------

    multi method deparse(RakuAST::MetaInfix::Assign:D $ast --> Str:D) {
        self.deparse($ast.infix) ~ self.hsyn("meta-=", '=')
    }

    multi method deparse(RakuAST::MetaInfix::Cross:D $ast --> Str:D) {
        self.meta-infix-letter($ast, 'X')
    }

    multi method deparse(RakuAST::MetaInfix::Hyper:D $ast --> Str:D) {
        my str $infix = self.deparse($ast.infix);

        # the ASCII markers run into an operator that starts with one of
        # their characters, with the = of a fat arrow, or with a ! that one
        # of their characters follows
        my int $wide = nqp::index(
          '=<>!',
          nqp::substr(self.deparse-without-highlighting($ast.infix),0,1)
        ) >= 0;
        my str $left  = $ast.dwim-left
          ?? ($wide ?? '«' !! '<<')
          !! ($wide ?? '»' !! '>>');
        my str $right = $ast.dwim-right
          ?? ($wide ?? '»' !! '>>')
          !! ($wide ?? '«' !! '<<');

        self.hsyn("meta-hyper-left", $left)
          ~ $infix
          ~ self.hsyn("meta-hyper-right", $right)
    }

    multi method deparse(RakuAST::MetaPostfix::Hyper:D $ast --> Str:D) {
        self.hsyn("meta-hyper", '>>')
          ~ self.hsyn("postfix",
              self.xsyn("postfix", self.deparse($ast.postfix))
            )
    }

    multi method deparse(RakuAST::MetaPrefix::Hyper:D $ast --> Str:D) {
        # the space that sets a word prefix off is taken by the marker
        self.deparse($ast.prefix).trim-trailing ~ self.hsyn("meta-hyper", '<<')
    }

    multi method deparse(RakuAST::MetaInfix::Negate:D $ast --> Str:D) {
        self.hsyn("meta-!", '!') ~ self.deparse($ast.infix)
    }

    multi method deparse(RakuAST::MetaInfix::Reverse:D $ast --> Str:D) {
        self.meta-infix-letter($ast, 'R')
    }

    multi method deparse(RakuAST::MetaInfix::Sequence:D $ast --> Str:D) {
        self.meta-infix-letter($ast, 'S')
    }

    multi method deparse(RakuAST::MetaInfix::Zip:D $ast --> Str:D) {
        self.meta-infix-letter($ast, 'Z')
    }

    multi method deparse(RakuAST::Method:D $ast --> Str:D) {
        self.method($ast, 'method')
    }

#- N ---------------------------------------------------------------------------

    multi method deparse(RakuAST::Name:D $ast --> Str:D) {
        return '::' if $ast.is-anonymous;

        my @name-parts := $ast.parts;
        (nqp::istype(@name-parts.head,RakuAST::Name::Part::Expression) ?? '::' !! '')
          ~ @name-parts.map({
                if nqp::istype($_,RakuAST::Name::Part::Expression) {
                    '(' ~ self.deparse(.expr) ~ ')'
                }
                elsif nqp::istype($_,RakuAST::Name::Part::Empty) {
                    ''
                }
                else {
                    .name
                }
            }).join('::')
          ~ $ast.colonpairs.map({
                if nqp::istype($_,RakuAST::ColonPair) {
                    self.deparse($_)
                }
                # the `<+++>` of an operator name is a bare quote, not a
                # pair, and an empty `<>` colonpair is a Nil term
                elsif nqp::istype($_,RakuAST::Term::Name)
                  && .name.canonicalize eq 'Nil' {
                    ':<>'
                }
                else {
                    ':' ~ self.deparse($_)
                }
            }).join
    }

    multi method deparse(RakuAST::Nqp:D $ast --> Str:D) {
        my str $op = $ast.op;
        self.hsyn("nqp-$op", "nqp::$op") ~ self.parenthesize($ast.args)
    }

    multi method deparse(RakuAST::Nqp::Const:D $ast --> Str:D) {
        self.hsyn('nqp-const', "nqp::const::" ~ $ast.name)
    }

#- O ---------------------------------------------------------------------------

    multi method deparse(RakuAST::OnlyStar:D $ --> '{*}') { }

#- P ---------------------------------------------------------------------------

    multi method deparse(RakuAST::Package:D $ast --> Str:D) {
        my str $scope = $ast.augmented ?? 'augment' !! $ast.scope;
        my str @parts;

        if $scope {
            @parts.push(self.syn-scope($scope))
              if $scope ne $ast.default-scope;
        }

        my str $declarator = $ast.declarator;
        @parts.push(self.syn-package($declarator));

        if $ast.name -> $astname {
            my str $name = self.deparse($astname);
            if $ast.parameterization -> $signature {
                @parts.push((my $deparsed := self.deparse($signature))
                  ?? $name ~ '[' ~ $deparsed ~ ']'
                  !! $name
                );
            }
            else {
                @parts.push($name);
            }
        }

        # the parser takes `is repr` out of the traits and stores it on
        # the package
        if $ast.repr -> $repr {
            @parts.push(self.syn-trait('is')
              ~ ' repr'
              ~ self.parenthesize(RakuAST::StrLiteral.new($repr))
            );
        }

        # trusts is only written as a statement inside the body
        my str @trusts;
        if $ast.traits -> @traits {
            for @traits -> $trait {
                nqp::istype($trait,RakuAST::Trait::Trusts)
                  ?? @trusts.push(self.deparse($trait))
                  !! @parts.push(self.deparse($trait));
            }
        }

        my $body := $declarator eq 'role'
          ?? RakuAST::Block.new(
               body => RakuAST::Blockoid.new(
                 # lose fabricated return value
                 RakuAST::StatementList.new(
                   |$ast.body.body.statement-list.statements.skip.head(*-1)
                 )
               )
             )
          !! $ast.body;

        if $ast.WHY -> $WHY {
            if $scope eq 'unit' {
                self.add-any-docs(@parts.join(' ') ~ ';', $WHY)
                  ~ self.unit-with-trusts(self.deparse($body, :unit), @trusts).chomp
            }
            else {
                @parts.push('{');
                my $*DELIMITER = '';
                self.add-any-docs(@parts.join(' '), $WHY).chomp
                  ~ self.block-with-trusts(self.deparse($body, :multi), @trusts).substr(1).chomp
            }
        }
        elsif $scope eq 'unit' {
            @parts.join(' ')
              ~ $.end-statement
              ~ self.unit-with-trusts(self.deparse($body, :unit), @trusts).chomp
        }
        else {
            @parts.push($ast.is-stub
              ?? '{...}'
              !! self.block-with-trusts(
                   self.deparse($body, :multi(?@trusts)), @trusts
                 )
            );
            @parts.join(' ')
        }
    }

    # the body is on several lines, the trusts go after its first
    method block-with-trusts(str $body, @trusts --> Str:D) {
        @trusts
          ?? $body.subst("\n", "\n" ~ @trusts.map({ "$*INDENT    $_;\n" }).join)
          !! $body
    }

    method unit-with-trusts(str $body, @trusts --> Str:D) {
        @trusts.map({ "$_;\n" }).join ~ $body
    }

    multi method deparse(RakuAST::Pragma:D $ast --> Str:D) {
        my str $pragma = $ast.name;
        my str $no     = $ast.off ?? "no" !! "use";
        my str @parts  = self.xsyn('use', $no), self.xsyn('pragma', $pragma);
        @parts.push(self.deparse($_)) with $ast.argument;
        self.hsyn("pragma-$pragma", @parts.join(' ')) ~ $*DELIMITER
    }

#- Parameter -------------------------------------------------------------------

    multi method deparse(RakuAST::Parameter:D $ast --> Str:D) {
        return self.add-any-docs(self.hsyn('literal',.raku), $ast.WHY)
          with $ast.value;

        my $target   := $ast.target;
        my @captures := $ast.type-captures;
        my str @parts;
        # the implicit Any of a target or a type capture is not written,
        # a parameter that is only a type has nothing else to show
        if $ast.type -> $type {
            my str $skip = $target || @captures ?? 'Any' !! '';
            if self.deparse($type, :$skip) -> $deparsed {
                @parts.push($deparsed);
            }
        }
        if @captures {
            @parts.push(' ') if @parts;
            @parts.push(@captures.map({ self.deparse($_) }).join(' '));
        }
        @parts.push(' ') if @parts && $target;

        if $target {
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
                # the is required trait marks the parameter required
                @parts.push('!') if $ast.is-declared-required
                  && !$ast.traits.first({
                       nqp::istype($_,RakuAST::Trait::Is)
                         && .name
                         && .name.canonicalize eq 'required'
                     });
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
            }

            if $ast.traits -> @traits {
                for @traits {
                    @parts.push(' ');
                    @parts.push(self.deparse($_));
                }
            }
        }
        elsif nqp::eqaddr($ast.slurpy,RakuAST::Parameter::Slurpy::Capture) {
            @parts.push(self.deparse($ast.slurpy));
        }
        elsif $ast.invocant {  # just a type without target
            @parts.push(':');
        }

        if $ast.sub-signature -> $signature {
            @parts.push(' ') if @parts;
            # the slurpy of an unpacking parameter without a target goes
            # right before the brackets
            @parts.push(self.deparse($ast.slurpy))
              unless $target
                || nqp::eqaddr($ast.slurpy,RakuAST::Parameter::Slurpy::Capture);
            @parts.push($signature.is-array ?? '[' !! '(');
            @parts.push(self.deparse($signature));
            @parts.push($signature.is-array ?? ']' !! ')');
        }

        @parts.push(self.where-constraint($_)) with $ast.where;

        @parts = self.hsyn('param', @parts.join);
        if $ast.default -> $default {
            @parts.push(self.syn-infix-ws($.assign) ~ self.deparse($default));
        }

        self.add-any-docs(@parts.join, $ast.WHY)
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
        my str @parts = self.hsyn('arrow-one', '->');

        my $signature := $ast.signature;
        my $WHY       := $ast.WHY;
        if $signature.parameters-initialized
          && $signature.parameters.first(*.WHY) {
            @parts.push("\n");
            @parts = self.add-any-docs(@parts.join(' '), $WHY)
              ~ self.deparse($signature);
        }

        else {
            if $signature.parameters-initialized
              && self.deparse($signature) -> $deparsed {
                @parts.push($deparsed);
            }

            if $WHY {
                @parts.push('{');
                return self.block-with-docs(@parts.join(' '), $WHY, $ast.body)
            }
        }

        @parts.push(self.deparse($ast.body));
        @parts.join(' ')
    }

    multi method deparse(RakuAST::Postcircumfix::ArrayIndex:D $ast --> Str:D) {
        self.squarize($ast.index)
          ~ self.colonpairs($ast, 'adverb-pc')
          ~ self.assignee($ast)
    }

    multi method deparse(RakuAST::Postcircumfix::HashIndex:D $ast --> Str:D) {
        self.bracketize($ast.index) ~ self.colonpairs($ast, 'adverb-pc')
    }

    multi method deparse(
      RakuAST::Postcircumfix::LiteralHashIndex:D $ast
    --> Str:D) {
        self.deparse($ast.index)
          ~ self.colonpairs($ast, 'adverb-pc')
          ~ self.assignee($ast)
    }

    multi method deparse(RakuAST::Postfix:D $ast --> Str:D) {
        my str $operator = $ast.operator;
        self.hsyn("postfix-$operator", $operator)
          ~ self.colonpairs($ast, 'adverb-pc')
    }

    multi method deparse(RakuAST::Postfix::Power:D $ast --> Str:D) {
        $ast.power.trans("0123456789-+i<>" => "⁰¹²³⁴⁵⁶⁷⁸⁹⁻⁺ⁱ", :delete)
    }

    multi method deparse(RakuAST::Postfix::Vulgar:D $ast --> Str:D) {
        my $rat := $ast.vulgar;
        "$rat.numerator.Str(:superscript)/$rat.denominator.Str(:subscript)"
    }

    multi method deparse(RakuAST::Prefix:D $ast --> Str:D) {
        my str $operator = $ast.operator;
        self.hsyn("prefix-$operator", self.xsyn('prefix', $operator))
          ~ ($operator.contains(/\w/) ?? " " !! "")
    }

    # the node holds a single bar as its operator
    multi method deparse(RakuAST::Prefix::Multislice:D $ --> Str:D) {
        self.hsyn('prefix-||', self.xsyn('prefix', '||'))
    }

#- Q ---------------------------------------------------------------------------

    multi method deparse(RakuAST::QuotedRegex:D $ast --> Str:D) {
        my str $adverbs = $ast.adverbs.map({
            self.deparse($_, 'adverb-rx')  # XXX ???
        }).join;
        self.hsyn('literal',($ast.match-immediately ?? 'm' !! $adverbs ?? 'rx' !! '')
          ~ $adverbs
          ~ $.regex-open
          ~ self.hsyn('regex-body', self.deparse($ast.body))
          ~ $.regex-close
        )
    }

    multi method deparse(RakuAST::QuotedString:D $ast --> Str:D) {
        if $ast.processors -> @processors {
            if @processors == 1 && @processors.head -> $processor {
                if %single-processor-prefix{$processor} -> str $p is copy {
                    $p = 'qqx/' if $processor eq 'exec' && $ast.has-variables;
                    self.hsyn("adverb-q-$p", self.xsyn('adverb-q', $p))
                      ~ self.slash-quoted-string($ast) ~ '/'
                }
                else {
                    NYI("Quoted string processor '$processor'").throw
                }
            }
            elsif @processors == 2 {
                my str $joined = @processors.join(' ');
                # the double angles interpolate, the single ones do not.
                # The ASCII form ends early when the list starts with < or
                # ends with >, the wide form when the list holds a wide angle
                if $joined eq 'quotewords val' {
                    my str $string = self.assemble-quoted-string($ast);
                    my int $wide = $string.starts-with('<') || $string.ends-with('>');
                    $wide && ($string.contains('«') || $string.contains('»'))
                      ?? self.multiple-processors(self.slash-quoted-string($ast), @processors)
                      !! $wide
                        ?? '«' ~ $string ~ '»'
                        !! $.double-pointy-open ~ $string ~ $.double-pointy-close
                }
                elsif $joined eq 'words val' && !$ast.has-variables {
                    $.pointy-open
                      ~ self.assemble-quoted-string($ast, :raw)
                      ~ $.pointy-close
                }
                else {
                    self.multiple-processors(self.slash-quoted-string($ast), @processors)
                }
            }
            else {
                self.multiple-processors(self.slash-quoted-string($ast), @processors)
            }
        }
        else {
            self.hsyn('literal', '"' ~ self.assemble-quoted-string($ast) ~ '"')
        }
    }

    # the text of a string between slashes escapes them
    method slash-quoted-string($ast --> Str:D) {
        my $*QUOTE-DELIMITER := '/';
        self.assemble-quoted-string($ast)
    }

    multi method deparse(RakuAST::QuoteWordsAtom:D $ast --> Str:D) {
        self.deparse($ast.atom)
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
        self.hsyn('literal', self.quote-if-needed($ast.text))
    }

    multi method deparse(RakuAST::Regex::Sym:D $ast --> Str:D) {
        self.hsyn('assertion', '<sym>')
    }

    multi method deparse(RakuAST::Regex::Alternation:D $ast --> Str:D) {
        self.branches($ast, $.regex-alternation)
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
        my $args := $ast.args;
        '<'
          ~ self.deparse($ast.callee)
          ~ ($args && $args.args ?? self.parenthesize($args) !! "")
          ~ '>'
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
          ~ self.parenthesize($ast.args)
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

    multi method deparse(RakuAST::Regex::Assertion::Recurse $ --> Str:D) {
        $.regex-assertion-recurse
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

    # without a quantifier the modifier needs the colon to be one
    multi method deparse(
      RakuAST::Regex::BacktrackModifiedAtom:D $ast
    --> Str:D) {
        my $backtrack := $ast.backtrack;
        self.deparse($ast.atom)
          ~ $.regex-backtrack-ratchet
          ~ (self.deparse($backtrack)
              unless nqp::eqaddr($backtrack,RakuAST::Regex::Backtrack::Ratchet))
    }

    multi method deparse(RakuAST::Regex::Block:D $ast --> Str:D) {
        self.deparse($ast.block).chomp
    }

#- Regex::C --------------------------------------------------------------------

    multi method deparse(RakuAST::Regex::CapturingGroup:D $ast --> Str:D) {
        self.hsyn('capture-positional', self.parenthesize($ast.regex))
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
        my str $characters = $ast.characters;
        my int $chars      = nqp::chars($characters);
        my @ords           = $characters.ords;

        # a character that does not print has no name to write
        nqp::findnotcclass(
          nqp::const::CCLASS_PRINTING,$characters,0,$chars
        ) == $chars
          ?? ($ast.negated ?? '\\C' !! '\\c')
               ~ '[' ~ @ords.map(*.uniname).join(', ') ~ ']'
          !! ($ast.negated ?? '\\X' !! '\\x')
               ~ '[' ~ @ords.map(*.base(16)).join(', ') ~ ']'
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

        with $ast.predicate {
            if nqp::istype($_,RakuAST::StrLiteral) {
                @parts.push('<');
                @parts.push(self.deparse-unquoted($_));
                @parts.push('>');
            }
            else {
                @parts.push(self.deparse($_))
            }
        }

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
        self.charclass-character($ast.character)
    }

    multi method deparse(
      RakuAST::Regex::CharClassEnumerationElement::Range:D $ast
    --> Str:D) {
        self.charclass-character($ast.from.chr)
          ~ '..'
          ~ self.charclass-character($ast.to.chr)
    }

#- Regex::Co -------------------------------------------------------------------

    multi method deparse(RakuAST::Regex::Conjunction:D $ast --> Str:D) {
        self.branches($ast, $.regex-conjunction)
    }

#- Regex::G --------------------------------------------------------------------

    multi method deparse(RakuAST::Regex::Group:D $ast --> Str:D) {
        self.squarize($ast.regex)
    }

#- Regex::I --------------------------------------------------------------------

    multi method deparse(
      RakuAST::Regex::InternalModifier::Dba:D $ast --> Str:D) {
        ':' ~ self.xsyn('adverb-rx', 'dba') ~ '(' ~ $ast.name.raku ~ ') '
    }

    multi method deparse(
      RakuAST::Regex::InternalModifier:D $ast --> Str:D) {
        ':'
          ~ ($ast.negated ?? '!' !! '')
          ~ self.xsyn('adverb-rx', $ast.modifier)
          ~ ' '
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
        my str $name  = $ast.name;
        my int $chars = nqp::chars($name);
        # a numbered capture is written without the brackets
        self.hsyn('capture-named',
          $chars && nqp::findnotcclass(
            nqp::const::CCLASS_NUMERIC,$name,0,$chars
          ) == $chars
            ?? '$' ~ $name ~ '='
            !! '$<' ~ $name ~ '>='
        ) ~ self.deparse($ast.regex)
    }

    multi method deparse(RakuAST::Regex::Nested:D $ast --> Str:D) {
        my str $goal = self.deparse($ast.goal);
        $.regex-nested
          ~ $goal
          ~ ($goal.ends-with(' ') ?? '' !! ' ')
          ~ self.deparse($ast.expr)
    }

#- Regex::Q --------------------------------------------------------------------

    multi method deparse(
      RakuAST::Regex::QuantifiedAtom:D $ast, :$whitespace
    --> Str:D) {
        my str @parts = self.deparse($ast.atom), self.deparse($ast.quantifier);

        if $ast.separator -> $separator {
            # the whitespace of a quantified atom with a separator sits
            # between the quantifier and the separator
            @parts.push(' ') if $whitespace;
            @parts.push($ast.trailing-separator ?? '%% ' !! '% ');
            @parts.push(self.deparse($separator));
        }

        @parts.join
    }

    multi method deparse(
      RakuAST::Regex::Quantifier::BlockRange:D $ast
    --> Str:D) {
        my $backtrack := $ast.backtrack;

        self.hsyn: 'regex-blockrange', '**'
          ~ (self.deparse($backtrack) unless nqp::eqaddr(
              $ast.backtrack,
              RakuAST::Regex::Backtrack
            ))
          ~ ' '
          ~ self.deparse($ast.block)
    }

    multi method deparse(
      RakuAST::Regex::Quantifier::OneOrMore:D $ast
    --> Str:D) {
        self.hsyn('regex-+', self.quantifier($ast, '+'))
    }

    multi method deparse(RakuAST::Regex::Quantifier::Range:D $ast --> Str:D) {
        my str @parts = '**';

        my $backtrack := $ast.backtrack;
        @parts.push(self.deparse($backtrack))
          unless nqp::eqaddr($backtrack,RakuAST::Regex::Backtrack);
        @parts.push(' ');

        with $ast.min -> $min {
            @parts.push($min.Str);
            with $ast.max -> $max {
                if $min != $max {
                    @parts.push('^') if $ast.excludes-min;
                    @parts.push('..');
                    @parts.push('^') if $ast.excludes-max;
                    @parts.push($max.Str);
                }
            }
            else {
                @parts.push('^') if $ast.excludes-min;
                @parts.push('..*');
            }
        }
        else {
            @parts.push('^') if $ast.excludes-max;
            @parts.push($ast.max.Str);
        }

        self.hsyn('regex-range', @parts.join)
    }

    multi method deparse(
      RakuAST::Regex::Quantifier::ZeroOrMore:D $ast
    --> Str:D) {
        self.hsyn('regex-*', self.quantifier($ast, '*'))
    }

    multi method deparse(
      RakuAST::Regex::Quantifier::ZeroOrOne:D $ast
    --> Str:D) {
        self.hsyn('regex-?', self.quantifier($ast, '?'))
    }

    multi method deparse(RakuAST::Regex::Quote:D $ast --> Str:D) {
        my $quoted := $ast.quoted;

        my @processors := $quoted.processors;

        # the < a b > form, the space after < is what tells it from an assertion
        if @processors == 1
          && @processors.head eq 'words'
          && !$quoted.has-variables {
            self.hsyn('literal',
              '< ' ~ self.assemble-quoted-string($quoted, :raw).trim ~ ' >'
            )
        }

        # Complicated stuff
        elsif @processors {
            self.hsyn('regex-code', '<{ ')
              ~ self.deparse($quoted)
              ~ self.hsyn('regex-code', ' }>')
        }

        elsif self.deparse-without-highlighting($quoted) -> $deparsed {
            my str $unquoted = $deparsed.substr(1).chop;
            self.hsyn(
              'literal',
              !$unquoted || $*QUOTE-REGEX-WORD || $unquoted.contains(/\W/)
                ?? $deparsed
                !! $unquoted
            )
        }
    }

#- Regex::S --------------------------------------------------------------------

    multi method deparse(RakuAST::Regex::Sequence:D $ast --> Str:D) {
        my str @parts;
        my $previous;
        for $ast.terms {
            # a word right after a backtrack modifier would read as an
            # adverb, one right after a variable as part of its name
            my $*QUOTE-REGEX-WORD :=
              nqp::istype($previous,RakuAST::Regex::BacktrackModifiedAtom)
              || nqp::istype($previous,RakuAST::Regex::Interpolation);
            @parts.push(nqp::istype($_,RakuAST::Regex::CharClass::BackSpace)
              ?? ('"' ~ self.deparse($_) ~ '"')
              !! self.deparse($_)
            );
            $previous := $_;
        }
        @parts.join
    }

    multi method deparse(
      RakuAST::Regex::SequentialAlternation:D $ast
    --> Str:D) {
        self.branches($ast, $.regex-sequential-alternation)
    }

    multi method deparse(
      RakuAST::Regex::SequentialConjunction:D $ast
    --> Str:D) {
        self.branches($ast, $.regex-sequential-conjunction)
    }

    multi method deparse(RakuAST::Regex::Statement:D $ast --> Str:D) {
        my $deparsed := self.deparse($ast.statement).chomp;
        ':' ~ ($deparsed.ends-with(';') ?? $deparsed !! $deparsed ~ '; ')
    }

#- Regex::W --------------------------------------------------------------------

    multi method deparse(RakuAST::Regex::WithWhitespace:D $ast --> Str:D) {
        my $regex := $ast.regex;
        if nqp::istype($regex,RakuAST::Regex::QuantifiedAtom) && $regex.separator {
            self.deparse($regex, :whitespace)
        }
        else {
            # an anchor writes its own trailing space
            my str $deparsed = self.deparse($regex);
            $deparsed.ends-with(' ') ?? $deparsed !! $deparsed ~ ' '
        }
    }

#- RegexD ----------------------------------------------------------------------

    # also for ::TokenDeclaration and ::RuleDeclaration
    multi method deparse(RakuAST::RegexDeclaration:D $ast --> Str:D) {
        my str @parts = self.xsyn('routine', $ast.declarator);

        if $ast.multiness -> $multiness {
            @parts.unshift(self.syn-multi($multiness));
        }

        my str $scope = $ast.scope;
        @parts.unshift(self.xsyn('scope', $scope))
          if $scope ne 'has' && $scope ne $ast.default-scope;

        @parts.push(self.deparse($_)) with $ast.name;

        # at least one parameter with declarator doc
        my $signature := $ast.signature;
        if $signature.parameters.first(*.WHY) {
            @parts.push("(\n");
            @parts.push(self.deparse($signature));
            @parts.push(')');
        }

        # no parameters with declarator doc
        elsif $signature.parameters-initialized {
            my $sigstr := self.parenthesize($signature);
            @parts.push($sigstr) unless $sigstr eq '()';
        }

        if $ast.traits.map({self.deparse($_)}).join(' ') -> $traits {
            @parts.push($traits);
        }

        my $body := $ast.body;
        if nqp::istype($body,RakuAST::OnlyStar) {
            @parts.push(self.deparse($body));
            if $ast.WHY -> $WHY {
                my $*DELIMITER = '';
                return self.add-any-docs(@parts.join(' '), $WHY);
            }
            return @parts.join(' ');
        }

        if $ast.WHY -> $WHY {
            @parts.push('{');
            # https://github.com/rakudo/rakudo/issues/5978
            my $*DELIMITER = ' ';  # a ";" here would spoil things
            @parts = self.add-any-docs(@parts.join(' '), $WHY);
            @parts.push($*INDENT);
        }
        else {
            @parts.push('{ ');
            @parts = @parts.join(' ');
        }

        @parts.push(self.deparse($body));
        @parts.push('}');
        @parts.join
    }

#- S ---------------------------------------------------------------------------

    multi method deparse(RakuAST::SemiList:D $ast --> Str:D) {
        my $*INTERPOLATING := False;
        my @statements := $ast.statements;
        my $statement  := @statements.head;
        @statements == 1
          && nqp::istype($statement,RakuAST::Statement::Expression)
          && !($statement.condition-modifier || $statement.loop-modifier)
          ?? self.deparse($statement.expression)
          !! @statements.map({ self.deparse($_) }).join($.list-infix-semi-colon)
    }

    multi method deparse(
      RakuAST::Signature:D $ast, :$no-returns
    --> Str:D) {
        my str @parts;

        if $ast.parameters -> @parameters {

            # need special handling for declarator doc
            if @parameters.first(*.WHY) {
                my $last      := @parameters.tail;
                my $*DELIMITER;

                my str @atoms;
                self.indent('  ');
                for @parameters -> $param {
                    $*DELIMITER = $param === $last || $param.invocant
                      ?? "\n"
                      !! $.list-infix-comma.trim ~ "\n";
                    @atoms.push($*INDENT);
                    @atoms.push(self.deparse($param));
                }
                self.dedent('  ');

                @parts.push(@atoms.join);
            }

            # no special action
            else {
                my $*DELIMITER = '';
                my $last := @parameters.tail;
                @parts.push(@parameters.map({
                    # an invocant is set off by its colon, not by a comma
                    my str $separator = $.list-infix-comma;
                    $separator = ' ' if .invocant;
                    $separator = ''  if $_ === $last;
                    self.deparse($_) ~ $separator
                }).join)
            }
        }

        unless $no-returns {
            with $ast.returns {
                @parts.push(self.hsyn('arrow-two', '-->'));
                @parts.push(self.deparse($_));
            }
        }

        @parts.join(' ')
    }

#- Statement -------------------------------------------------------------------

    multi method deparse(RakuAST::Statement::Catch:D $ast --> Str:D) {
        self.labels($ast)
          ~ self.syn-phaser('CATCH') ~ ' ' ~ self.deparse($ast.body)
    }

    multi method deparse(RakuAST::Statement::Control:D $ast --> Str:D) {
        self.labels($ast)
          ~ self.syn-phaser('CONTROL') ~ ' ' ~ self.deparse($ast.body)
    }

    multi method deparse(RakuAST::Statement::Default:D $ast --> Str:D) {
        self.labels($ast)
          ~ self.syn-block('default') ~ ' ' ~ self.deparse($ast.body)
    }

    multi method deparse(RakuAST::Statement::Elsif:D $ast --> Str:D) {
        self.conditional($ast, 'elsif')  # cannot have labels
    }

    # an empty statement keeps a block that holds nothing else a block
    multi method deparse(RakuAST::Statement::Empty:D $ast --> Str:D) {
        self.labels($ast) ~ ($*DELIMITER ?? $.end-statement !! ';')
    }

    multi method deparse(RakuAST::Statement::Expression:D $ast --> Str:D) {
        # a statement deparsed on its own has no statement list to place
        # the bodies of its heredocs, so it places them itself
        my $outer := nqp::getlexdyn('@*HEREDOCS');
        my $own   := nqp::isnull($outer);
        my @*HEREDOCS := $own ?? [] !! $outer;
        my $expression := $ast.expression;
        my str $deparsed = self.deparse($expression);

        my str @parts;
        if $ast.condition-modifier -> $condition {
            @parts.push(self.deparse($condition));
        }

        if $ast.loop-modifier -> $loop {
            @parts.push(self.deparse($loop));
        }

        # condition or loop modifier
        my int $chop;
        if @parts {
            $chop = $deparsed.ends-with(self.end-statement)
              ?? self.end-statement.chars
              !! $deparsed.ends-with(self.last-statement)
                ?? self.last-statement.chars
                !! 0;
            $deparsed = $deparsed.chop($chop)
              ~ ' '
              ~ @parts.join(' ')
              ~ $deparsed.substr(* - $chop)
        }

        # a declarator target writes its own delimiter, one that ends in a
        # body, a block or a routine, writes none, so a modifier after it
        # needs the delimiter here
        my $text := self.labels($ast)
          ~ $deparsed
          ~ (nqp::istype($expression,RakuAST::Doc::DeclaratorTarget)
               && !(@parts && !$chop)
              ?? ""
              !! $*DELIMITER
            );

        $own && @*HEREDOCS
          ?? self.insert-heredocs($text, @*HEREDOCS)
          !! $text
    }

    multi method deparse(RakuAST::Statement::For:D $ast --> Str:D) {
        my str @parts =
          self.syn-block('for'),
          self.condition($ast.source),
          self.deparse($ast.body)
        ;

        if $ast.mode -> str $mode {
            @parts.unshift($mode) if $mode ne 'serial';
        }

        self.labels($ast) ~ @parts.join(' ')
    }

    multi method deparse(RakuAST::Statement::Given:D $ast --> Str:D) {
        self.labels($ast)
          ~ self.syn-block('given')
          ~ ' '
          ~ self.condition($ast.source)
          ~ ' '
          ~ self.deparse($ast.body)
    }

    # handling both ::If and ::With
    multi method deparse(RakuAST::Statement::IfWith:D $ast --> Str:D) {
        my str @parts = self.conditional($ast, $ast.IMPL-QAST-TYPE);

        my $INDENT := $*INDENT;
        if $ast.elsifs -> @elsifs {
            for @elsifs {
                @parts.push($INDENT);
                @parts.push(self.deparse($_));
            }
        }

        if $ast.else -> $else {
            @parts.push($INDENT);
            @parts.push(self.syn-block('else'));
            @parts.push(' ');
            @parts.push(self.deparse($else));
            @parts.push($.last-statement);
        }

        self.labels($ast) ~ @parts.join
    }

    multi method deparse(RakuAST::Statement::Import:D $ast --> Str:D) {
        my str @parts =
          self.xsyn('use','import'), self.deparse($ast.module-name);
        @parts.push(self.deparse($_)) with $ast.argument;
        self.labels($ast) ~ @parts.join(' ') ~ $*DELIMITER
    }

    multi method deparse(RakuAST::Statement::Loop:D $ast --> Str:D) {
        my str $condition = " ";
        if $ast.setup || $ast.condition || $ast.increment {
            # a declaration in the setup would add the statement delimiter
            my $*DELIMITER = '';
            $condition = ' ('
              ~ ($ast.setup     ?? self.deparse($ast.setup)     !! '')
              ~ $.loop-separator
              ~ ($ast.condition ?? self.deparse($ast.condition) !! '')
              ~ $.loop-separator
              ~ ($ast.increment ?? self.deparse($ast.increment) !! '')
              ~ ') ';
        }

        self.labels($ast)
          ~ self.syn-block('loop')
          ~ $condition
          ~ self.deparse($ast.body)
    }

    multi method deparse(
      RakuAST::Statement::Loop::RepeatUntil:D $ast
    --> Str:D) {
        self.labels($ast) ~ self.simple-repeat($ast, 'until')
    }

    multi method deparse(
      RakuAST::Statement::Loop::RepeatWhile:D $ast --> Str:D) {
        self.labels($ast) ~ self.simple-repeat($ast, 'while')
    }

    multi method deparse(RakuAST::Statement::Loop::Until:D $ast --> Str:D) {
        self.labels($ast) ~ self.simple-loop($ast, 'until')
    }

    multi method deparse(RakuAST::Statement::Loop::While:D $ast --> Str:D) {
        self.labels($ast) ~ self.simple-loop($ast, 'while')
    }

    multi method deparse(RakuAST::Statement::Need:D $ast --> Str:D) {
        self.labels($ast)
          ~ self.xsyn('use', 'need')
          ~ ' '
          ~ $ast.module-names.map({self.deparse($_)}).join($.list-infix-comma)
          ~ $*DELIMITER
    }

    multi method deparse(RakuAST::Statement::Orwith:D $ast --> Str:D) {
        self.conditional($ast, 'orwith')  # cannot have labels
    }

    multi method deparse(RakuAST::Statement::Require:D $ast --> Str:D) {
        my str @parts = self.xsyn('use', 'require');
        @parts.push(self.deparse($_)) with $ast.module-name;
        @parts.push(self.deparse($_)) with $ast.file;
        @parts.push(self.deparse($_)) with $ast.argument;
        self.labels($ast) ~ @parts.join(' ') ~ $*DELIMITER
    }

    multi method deparse(RakuAST::Statement::Unless:D $ast --> Str:D) {
        self.labels($ast) ~ self.negated-conditional($ast, 'unless');
    }

    multi method deparse(RakuAST::Statement::Use:D $ast --> Str:D) {
        self.use-no("use", $ast) ~ $*DELIMITER
    }

    multi method deparse(RakuAST::Statement::When:D $ast --> Str:D) {
        self.labels($ast)
          ~ self.syn-block('when')
          ~ ' '
          ~ self.deparse($ast.condition)
          ~ ' '
          ~ self.deparse($ast.body)
    }

    multi method deparse(RakuAST::Statement::Whenever:D $ast --> Str:D) {
        self.labels($ast)
          ~ self.syn-block('whenever')
          ~ ' '
          ~ self.condition($ast.trigger)
          ~ ' '
          ~ self.deparse($ast.body)
    }

    multi method deparse(RakuAST::Statement::Without:D $ast --> Str:D) {
        self.labels($ast) ~ self.negated-conditional($ast, 'without');
    }

    multi method deparse(RakuAST::StatementList:D $ast --> Str:D) {
        my $*INTERPOLATING := False;

        if $ast.statements -> @statements {
            my str @parts;
            my str $spaces = $*INDENT;
            my $last-statement := %_<no-sink>
              ?? Any
              !! @statements.first({
                     nqp::not_i(nqp::istype($_,RakuAST::Doc::Block))
                 }, :end) // @statements.tail;

            my $code;
            my $*DELIMITER;
            for @statements -> $statement {
                $*DELIMITER = $statement === $last-statement
                  ?? $.last-statement
                  !! $.end-statement;
                my @*HEREDOCS;
                my $deparsed := self.deparse($statement);
                $deparsed := $deparsed.chop(2)
                  if $deparsed.ends-with("};\n")
                  && self.statement-is-prefixed-block($statement);
                $deparsed := $deparsed ~ "\n" if $deparsed.ends-with('}');
                $deparsed := self.insert-heredocs($deparsed, @*HEREDOCS)
                  if @*HEREDOCS;

                # a doc block carries its own margin
                @parts.push($spaces)
                  unless nqp::istype($statement,RakuAST::Doc::Block);
                @parts.push($deparsed);
            }

            @parts.join
        }

        else {
            ''
        }
    }

#- Statement::Modifier ---------------------------------------------------------

    multi method deparse(RakuAST::StatementModifier::Given:D $ast --> Str:D) {
        self.statement-modifier('given', $ast)
    }

    multi method deparse(RakuAST::StatementModifier::If:D $ast --> Str:D) {
        self.statement-modifier('if', $ast)
    }

    multi method deparse( RakuAST::StatementModifier::For:D $ast --> Str:D) {
        self.statement-modifier('for', $ast)
    }

    multi method deparse(RakuAST::StatementModifier::For::Thunk:D $ --> '') { }

    multi method deparse(RakuAST::StatementModifier::Unless:D $ast --> Str:D) {
        self.statement-modifier('unless', $ast)
    }

    multi method deparse(RakuAST::StatementModifier::Until:D $ast --> Str:D) {
        self.statement-modifier('until', $ast)
    }

    multi method deparse(RakuAST::StatementModifier::When:D $ast --> Str:D) {
        self.statement-modifier('when', $ast)
    }

    multi method deparse(RakuAST::StatementModifier::While:D $ast --> Str:D) {
        self.statement-modifier('while', $ast)
    }

    multi method deparse(RakuAST::StatementModifier::With:D $ast --> Str:D) {
        self.statement-modifier('with', $ast)
    }

    multi method deparse(RakuAST::StatementModifier::Without:D $ast --> Str:D) {
        self.statement-modifier('without', $ast)
    }

#- StatementPrefix -------------------------------------------------------------

    # handles all statement prefixes
    multi method deparse(RakuAST::StatementPrefix:D $ast --> Str:D) {
        my str $prefix = $ast.type;
        self.hsyn("stmt-prefix-$prefix", self.xsyn('stmt-prefix', $prefix))
          ~ ' '
          ~ self.blorst($ast.blorst)
    }

    # handles most phasers
    multi method deparse(RakuAST::StatementPrefix::Phaser:D $ast --> Str:D) {
        self.syn-phaser($ast.type) ~ ' ' ~ self.blorst($ast.blorst)
    }

    multi method deparse(RakuAST::StatementPrefix::Phaser::First:D $ast --> Str:D) {
        self.syn-phaser($ast.type) ~ ' ' ~ self.blorst($ast.original-blorst)
    }

    multi method deparse(
      RakuAST::StatementPrefix::Phaser::Post:D $ast
    --> Str:D) {
        # POST phasers get extra code inserted at RakuAST level, which
        # wraps the original blorst into a statement in which the blorst
        # becomes the condition modifier
        my $expression := $ast.blorst.body.statement-list.statements.head
          .condition-modifier.expression;
        self.syn-phaser('POST') ~ ' ' ~ self.deparse(
          nqp::istype($expression,RakuAST::ApplyPostfix)
            ?? $expression.operand
            !! $expression
        ).chomp
    }

    multi method deparse(
      RakuAST::StatementPrefix::Phaser::Pre:D $ast
    --> Str:D) {
        # PRE phasers get extra code inserted at RakuAST level, which
        # wraps the original blorst into a statement in which the blorst
        # becomes the condition modifier
        my $expression := $ast.blorst.condition-modifier.expression;
        self.syn-phaser('PRE') ~ ' ' ~ self.deparse(
          nqp::istype($expression,RakuAST::ApplyPostfix)
            ?? $expression.operand
            !! $expression
        ).chomp
    }

#- Stu -------------------------------------------------------------------------

    multi method deparse(RakuAST::Stub:D $ast --> Str:D) {
        my str $hsyn = self.hsyn('stub', $ast.name);
        my $args := $ast.args;
        $args && $args.args
          ?? $hsyn ~ ' ' ~ self.deparse($args)
          !! $hsyn
    }

#- Su --------------------------------------------------------------------------

    multi method deparse(RakuAST::Sub:D $ast --> Str:D) {
        my str @parts = self.syn-routine('sub');

        if $ast.multiness -> $multiness {
            @parts.unshift(self.syn-multi($multiness))
        }

        my str $scope = $ast.scope;
        @parts.unshift(self.xsyn('scope', $scope))
          if $scope ne $ast.default-scope && ($ast.name || $scope ne 'anon');

        if $ast.name -> $name {
            @parts.push(self.deparse($name));
        }

        self.handle-signature($ast, @parts.join(' '))
    }

    multi method deparse(RakuAST::Submethod:D $ast --> Str:D) {
        self.method($ast, 'submethod')
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
            @parts.push(self.deparse($ast.pattern).subst('/', '\/', :g));
            @parts.push('/');
            # only the text of the replacement escapes the delimiter, the
            # code of a closure in it is closed by its braces
            @parts.push(self.slash-quoted-string($ast.replacement));
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
        Q/\/ ~ self.parenthesize($ast.source)
    }

    multi method deparse(RakuAST::Term::EmptySet:D $ --> Str:D) {
        $.term-empty-set
    }

    multi method deparse(RakuAST::Term::Enum:D $ast --> Str:D) {
        my str $name = $ast.name.canonicalize;
        self.hsyn("enum-$name", self.xsyn('enum', $name))
    }

    multi method deparse(RakuAST::Term::HyperWhatever:D $ --> Str:D) {
        $.term-hyperwhatever
    }

    multi method deparse(RakuAST::Term::Name:D $ast --> Str:D) {
        self.deparse($ast.name)
    }

    multi method deparse(RakuAST::Term::Named:D $ast --> Str:D) {
        my str $name = $ast.name;
        self.hsyn("term-$name", self.xsyn('term', $name))
    }

    multi method deparse(RakuAST::Term::Rand:D $ --> Str:D) {
        self.hsyn('term-rand', self.xsyn('term', $.term-rand))
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
          ~ ($args.defined && $args.args
              ?? $.reduce-close ~ self.deparse($args)
              !! $.reduce-close.trim-trailing
            )
    }

    multi method deparse(RakuAST::Term::Self:D $ --> Str:D) {
        self.hsyn('invocant', self.xsyn('term', $.term-self))
    }

    multi method deparse(RakuAST::Term::TopicCall:D $ast --> Str:D) {
        my $call := $ast.call;
        # A methodish call deparses with its own leading dot (.foo); a postfix
        # such as a postcircumfix (.<k>, .[0], .{...}) does not, so supply it.
        nqp::istype($call, RakuAST::Call::Methodish)
          ?? self.deparse($call)
          !! '.' ~ self.deparse($call)
    }

    multi method deparse(RakuAST::Term::Whatever:D $ --> Str:D) {
        self.hsyn('var-term', $.term-whatever)
    }

    multi method deparse(RakuAST::WhateverCode::Argument:D $ast --> Str:D) {
        self.hsyn('var-term',
          $ast.is-hyper ?? $.term-hyperwhatever !! $.term-whatever
        )
    }

#- Ternary ---------------------------------------------------------------------

    multi method deparse(RakuAST::Ternary:D $ast --> Str:D) {
        my $intern := $*TERNARY;

        # indenting for nested ternaries
        if nqp::istype($intern,Failure) {
            my $*TERNARY = "";
            return self.deparse($ast);
        }

        my $then := $ast.then;
        my $else := $ast.else;
        my $nested := $intern
          || nqp::istype($then,RakuAST::Ternary)
          || nqp::istype($else,RakuAST::Ternary);
        my str $indent = $nested
          ?? "\n" ~ ($intern ~= '  ').chop  # assume 1 space left of ?? !!
          !! '';

        my str @parts =
          self.deparse($ast.condition),
          $indent,
          self.hsyn('ternary-one', $.ternary1);

        @parts.push(self.deparse($then));
        @parts.push($indent);
        @parts.push(self.hsyn('ternary-two', $.ternary2));
        @parts.push(self.deparse($else));

        @parts.join
    }

#- Trait -----------------------------------------------------------------------

    multi method deparse(RakuAST::Trait::Handles:D $ast --> Str:D) {
        self.syn-trait("handles") ~ ' ' ~ self.deparse($ast.term)
    }

    multi method deparse(RakuAST::Trait::Trusts:D $ast --> Str:D) {
        self.syn-trait("trusts") ~ ' ' ~ self.deparse($ast.type)
    }

    multi method deparse(RakuAST::Trait::Is:D $ast --> Str:D) {
        my str $base = self.syn-trait("is") ~ ' ';

        with $ast.name -> $name {
            $base ~= self.deparse($name);
        }
        orwith $ast.type -> $type {
            $base ~= self.deparse($type)
        }

        with $ast.argument {
            my $deparsed := self.deparse($_);
            $deparsed := "($deparsed)" unless $deparsed.starts-with('(');
            $base ~ $deparsed
        }
        else {
            $base
        }
    }

    multi method deparse(RakuAST::Trait::Type:D $ast --> Str:D) {
        self.syn-trait($ast.IMPL-TRAIT-NAME) ~ ' ' ~ self.deparse($ast.type)
    }

    multi method deparse(RakuAST::Trait::Will:D $ast --> Str:D) {
        self.syn-trait("will")
          ~ ' ' ~ $ast.phase
          ~ ' ' ~ self.deparse($ast.block)
    }

    multi method deparse(RakuAST::Trait::WillBuild:D $ast --> Str:D) {
        "" # XXX for now
    }

#- Transliteration -------------------------------------------------------------

    multi method deparse(RakuAST::Transliteration:D $ast --> Str:D) {
        my str @parts = $ast.destructive ?? 'tr' !! 'TR';

        if $ast.adverbs -> @adverbs {
            @parts.push(self.deparse($_)) for @adverbs;
        }

        for $ast.left, $ast.right {
            @parts.push('/');
            @parts.push(self.deparse($_).substr(1,*-1).subst('/', '\/', :g));
        }
        @parts.push('/');

        @parts.join
    }

#- Type ------------------------------------------------------------------------

    multi method deparse(RakuAST::Type::Capture:D $ast --> Str:D) {
        '::' ~ self.deparse($ast.name)
    }

    multi method deparse(RakuAST::Type::Coercion:D $ast --> Str:D) {
        my str $constraint = self.deparse($ast.constraint);
        $constraint = "" if $constraint eq 'SETTING::<Any>';
        self.deparse($ast.base-type) ~ "($constraint)"
    }

    multi method deparse(RakuAST::Type::AnyDefinedness:D $ast --> Str:D) {
        self.type-with-smiley($ast.base-type, '_')
    }

    multi method deparse(RakuAST::Type::Definedness:D $ast --> Str:D) {
        self.type-with-smiley(
          $ast.base-type,
          $ast.through-pragma ?? '' !! $ast.definite ?? 'D' !! 'U'
        )
    }

    # the smiley of a parameterized type goes before the arguments, a
    # smiley that came from a pragma is not written
    method type-with-smiley($type, str $smiley --> Str:D) {
        my $base-type := $type;
        my str $args;
        if nqp::istype($base-type,RakuAST::Type::Parameterized) {
            my str $deparsed = self.deparse($base-type.args);
            $args = "[$deparsed]" if $deparsed;
            $base-type := $base-type.base-type;
        }

        my str $name = self.deparse(
          nqp::can($base-type,'name') ?? $base-type.name !! $base-type
        );

        self.hsyn("type-$name", $smiley
          ?? $name ~ self.hsyn("smiley-$smiley", ":$smiley")
          !! ($name eq 'Any' ?? '' !! $name)
        ) ~ $args
    }

    multi method deparse(RakuAST::Type::Enum:D $ast --> Str:D) {
        my str @parts = self.syn-typer('enum');

        # the type of the values only parses between a scope and the
        # declarator, so a type makes the scope explicit: `my Str enum`
        my $of := $ast.of;
        @parts.unshift(self.deparse($of)) if $of;

        my str $scope = $ast.scope;
        @parts.unshift(self.syn-scope($scope))
          if $scope && ($of || $scope ne $ast.default-scope);

        @parts.push(self.deparse($_)) with $ast.name;

        if $ast.clean-clone.traits -> @traits {
            @parts.push(self.deparse($_)) for @traits;
        }
        @parts.push(self.deparse($ast.term));

        self.add-any-docs(@parts.join(' '), $ast.WHY)
    }

    multi method deparse(RakuAST::Type::Parameterized:D $ast --> Str:D) {
        my str $args = self.deparse($ast.args);
        self.deparse($ast.base-type) ~ ($args ?? "[$args]" !! "")
    }

    multi method deparse(RakuAST::Type::Setting:D $ast --> Str:D) {
        my str @parts = nqp::split('::',self.deparse($ast.name));
        my str $root = @parts.shift;

        $root eq 'Any'
          ?? ''
          !! "SETTING::<$root>" ~ @parts.map({ '.WHO<' ~ $_ ~ '>' }).join
    }

    multi method deparse(RakuAST::Type::Simple:D $ast --> Str:D) {
        self.syn-type($ast, |%_)
    }

    multi method deparse(RakuAST::Type::Subset:D $ast --> Str:D) {
        my str @parts = self.syn-typer('subset');

        my str $scope = $ast.scope;
        @parts.unshift(self.syn-scope($scope))
          if $scope && $scope ne $ast.default-scope;

        @parts.push(self.hsyn("type",self.deparse($ast.name)));
        with $ast.of {
            @parts.push(self.xsyn('trait', 'of'));
            @parts.push(self.hsyn("traitmod-of", self.deparse($_)));
        }
        @parts.push(self.hsyn("type", self.deparse($_))) for $ast.traits;

        with $ast.where {
            @parts.push(
              self.hsyn('constraint-where',self.xsyn('constraint', 'where'))
            );
            @parts.push(self.deparse($_));
        }

        self.add-any-docs(@parts.join(' '), $ast.WHY)
    }

#- Var -------------------------------------------------------------------------

    multi method deparse(RakuAST::Var::Attribute:D $ast --> Str:D) {
        self.hsyn('var-attribute', $ast.name)
    }

    multi method deparse(RakuAST::Var::Attribute::Public:D $ast --> Str:D) {
        my str $deparsed = self.hsyn('var-public', $ast.name);
        if $ast.args && $ast.args.args {
            $deparsed ~ '(' ~ self.deparse($ast.args) ~ ')'
        }
        else {
            $deparsed
        }
    }

    multi method deparse(RakuAST::Var::Compiler::Block:D $ --> Str:D) {
        self.hsyn('var-lexical','&?BLOCK')
    }

    multi method deparse(RakuAST::Var::Compiler::File:D $ast --> Str:D) {
        self.hsyn('var-compile',$.var-compiler-file)
    }

    multi method deparse(RakuAST::Var::Compiler::Line:D $ast --> Str:D) {
        self.hsyn('var-compile', $.var-compiler-line)
    }

    multi method deparse(RakuAST::Var::Compiler::Lookup:D $ast --> Str:D) {
        self.hsyn('var-compile', $ast.name)
    }

    multi method deparse(RakuAST::Var::Doc:D $ast --> Str:D) {
        self.hsyn('var-rakudoc', '$=' ~ $ast.name)
    }

    multi method deparse(RakuAST::Var::Dynamic:D $ast --> Str:D) {
        self.hsyn('var-dynamic', $ast.name)
    }

    multi method deparse(RakuAST::Var::Slang:D $ast --> Str:D) {
        self.hsyn('var-slang', '$~' ~ $ast.name)
    }

    multi method deparse(RakuAST::Var::Lexical:D $ast --> Str:D) {
        my $name := $ast.name;
        self.hsyn('var-lexical', $name)
    }

    multi method deparse(RakuAST::Var::Lexical::Setting:D $ast --> Str:D) {
        self.hsyn('var-setting', 'SETTING::<' ~ $ast.name ~ '>')
    }

    multi method deparse(RakuAST::Var::NamedCapture:D $ast --> Str:D) {
        my $index := $ast.index;
        # a name held as a quoted string without processors needs its angles
        self.hsyn('capture-named', ($ast.sigil || '$') ~ (
          nqp::istype($index,RakuAST::QuotedString) && !$index.processors
            ?? $.pointy-open
                 ~ self.assemble-quoted-string($index, :raw)
                 ~ $.pointy-close
            !! self.deparse($index)
        )) ~ self.colonpairs($ast, 'adverb-pc')
    }

    multi method deparse(RakuAST::Var::Package:D $ast --> Str:D) {
        self.hsyn('var-package', $ast.sigil ~ self.deparse($ast.name))
    }

    multi method deparse(RakuAST::Var::PositionalCapture:D $ast --> Str:D) {
        self.hsyn('capture-positional', '$' ~ $ast.index.Str)
    }

#- VarDeclaration --------------------------------------------------------------

    multi method deparse(RakuAST::VarDeclaration::Anonymous:D $ast --> Str:D) {
        my str $sigil = $ast.sigil;

        # a bare $ is the anonymous state scalar with nothing else on it
        # inside an expression, a statement writes it out
        $sigil eq '$' && $ast.scope eq 'state' && !$*DELIMITER
          && !$ast.initializer && !$ast.type && !$ast.traits
          ?? $sigil
          !! self.var-declaration($ast, $sigil) ~ $*DELIMITER
    }

    multi method deparse(RakuAST::VarDeclaration::Auto:D $ast --> Str:D) {
        self.deparse(RakuAST::Var::Lexical.new($ast.name))
    }

    multi method deparse(RakuAST::VarDeclaration::Constant:D $ast --> Str:D) {
        my str @parts;

        my str $scope = $ast.scope;
        @parts.push(self.syn-scope($scope))
          if $scope ne $ast.default-scope;

        @parts.push(self.syn-type($_)) with $ast.type;
        @parts.push(self.hsyn("scope-constant", self.xsyn('scope', 'constant')));
        @parts.push($ast.name);
        if $ast.traits -> @traits {
            @parts.push(self.deparse($_)) for @traits;
        }
        @parts.push(self.deparse($ast.initializer).trim-leading);

        @parts.join(' ');
    }

    multi method deparse(RakuAST::VarDeclaration::Implicit:D $ast --> Str:D) {
        self.hsyn('var-implicit', $ast.name)
    }

    multi method deparse(
      RakuAST::VarDeclaration::Implicit::Constant:D $ast
    --> Str:D) {
        (self.hsyn('scope-my', self.xsyn('scope', 'my')),
          self.hsyn('scope-constant', self.xsyn('scope', 'constant')),
          self.hsyn('var-term', $ast.name),
          self.hsyn('infix-=', '='),
          $ast.value.raku
        ).join(' ')
    }

    multi method deparse(
      RakuAST::VarDeclaration::Placeholder::Named:D $ast
    --> Str:D) {
        self.hsyn('var-placeholder', .substr(0, 1) ~ ':' ~ .substr(1))
          given $ast.lexical-name
    }

    multi method deparse(
      RakuAST::VarDeclaration::Placeholder::Positional:D $ast
    --> Str:D) {
        self.hsyn('var-placeholder', .substr(0, 1) ~ '^' ~ .substr(1))
          given $ast.lexical-name
    }

    # handles SlurpyArray/SlurpyHash
    multi method deparse(
      RakuAST::VarDeclaration::Placeholder::Slurpy:D $ast
    --> Str:D) {
        self.hsyn('var-placeholder', $ast.lexical-name)
    }

    multi method deparse(RakuAST::VarDeclaration::Signature:D $ast --> Str:D) {
        my str @parts = self.syn-scope($ast.scope);
        @parts.push(self.syn-type($_)) with $ast.type;
        # a declared type is stored as the return type as well
        @parts.push('('
          ~ self.deparse($ast.signature, :no-returns($ast.type.defined))
          ~ ')'
        );

        @parts.join(' ')
          ~ ($ast.initializer ?? self.deparse($ast.initializer) !! '')
    }

    multi method deparse(RakuAST::VarDeclaration::Simple:D $ast --> Str:D) {
        self.add-any-docs(self.var-declaration($ast), $ast.WHY)
    }

    multi method deparse(RakuAST::VarDeclaration::Term:D $ast --> Str:D) {
        my str @parts;

        @parts.push(self.syn-scope($ast.scope));
        @parts.push(' ');

        if $ast.type -> $type {
            @parts.push(self.syn-type($type));
            @parts.push(' ');
        }

        @parts.push(Q/\/);
        @parts.push(self.hsyn('var-term', self.deparse($ast.name)));

        @parts.push(self.deparse($ast.initializer));

        @parts.join
    }

}

nqp::bindhllsym('Raku', 'DEPARSE', RakuAST::Deparse);

# vim: expandtab shiftwidth=4
