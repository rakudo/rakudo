# This is the default class handling deparsing (aka, converting a given
# RakuAST::Node object into Raku source code).  It allows for all sorts
# of customization, and can be subclassed for further optimizations.

# Apart from the .new method, which expects named parameters, each public
# method expects an instance if a subclass of a RakuAST::Node as the first
# positional parameter.

class RakuAST::Deparse {
    has str $.before-comma = ' ';
    has str $.after-comma  = ' ';

    has str $.parens-open  = '(';
    has str $.parens-close = ')';

    has str $.square-open  = '[';
    has str $.square-close = ']';

    has str $.reduce-open     = '[';
    has str $.reduce-triangle = '[\\';
    has str $.reduce-close    = ']';

    has str $.bracket-open  = '{';
    has str $.bracket-close = '}';

    has str $.pointy-open  = '<';
    has str $.pointy-close = '>';

    has str $.double-pointy-open  = '<<';
    has str $.double-pointy-close = '>>';

    has str $.block-open  = "\{\n";
    has str $.block-close = "\}\n";

    has str $.regex-open  = '/ ';
    has str $.regex-close = ' /';
    has str $.regex-alternation = ' | ';
    has str $.regex-sequential-alternation = ' || ';
    has str $.regex-conjunction = ' && ';

    has str $.before-infix = ' ';
    has str $.after-infix  = ' ';
    has str $.list-infix-comma = ', ';

    has str $.assign =  ' = ';
    has str $.bind   = ' := ';

    has str $.before-list-infix = '';
    has str $.after-list-infix  = ' ';

    has str $.loop-separator = '; ';

    has str $.pointy-sig     = '-> ';
    has str $.pointy-return  = ' --> ';
    has str $.fatarrow       = ' => ';
    has str $.end-statement  = ";\n";
    has str $.last-statement = "\n";

    has str $.indent-spaces = '';
    has str $.indent-with   = '    ';

    has str $.ternary1  = ' ?? ';
    has str $.ternary2  = ' !! ';

    proto method deparse(|) {*}

    # Base class catcher
    multi method deparse(RakuAST::Node:D $ast) {
        X::NYI.new(feature => "Deparsing $ast.^name() objects").throw
    }

    # Odd value catcher, avoiding long dispatch options in error message
    multi method deparse(Mu:D $ast) {
        die "You cannot deparse a $ast.^name() instance: $ast.raku()";
    }
    multi method deparse(Mu:U $ast) {
        die "You cannot deparse a $ast.^name type object";
    }

    # helper method for deparsing contextualizers, sadly no private multis yet
    proto method context-target(|) is implementation-detail {*}
    multi method context-target(RakuAST::StatementSequence $target --> str) {
        self!parenthesize(self.deparse($target))
    }
    multi method context-target($target --> str) {
        self.deparse($target)
    }

#--------------------------------------------------------------------------------
# Private helper methods

    method !indent(--> str) {
        $!indent-spaces = nqp::concat($!indent-spaces,$!indent-with)
    }

    method !dedent(--> str) {
        $!indent-spaces = nqp::substr(
          $!indent-spaces,
          0,
          nqp::chars($!indent-spaces) - nqp::chars($!indent-with)
        )
    }

    method !routine(RakuAST::Routine:D $ast --> str) {
        my $parts := nqp::list_s;

        nqp::push_s($parts,' ');
        if $ast.name -> $name {
            nqp::push_s($parts,self.deparse($name));
        }

        unless $ast.placeholder-signature {
            nqp::push_s($parts,self!parenthesize(self.deparse($ast.signature)));
        }

        if $ast.traits -> @traits {
            for @traits -> $trait {
                nqp::push_s($parts,' ');
                nqp::push_s($parts,self.deparse($trait));
            }
        }

        nqp::push_s($parts,' ');
        nqp::push_s($parts,self.deparse($ast.body));

        nqp::join('',$parts)
    }

    method !method(RakuAST::Method:D $ast, str $type --> str) {
        my $parts := nqp::list_s;

        if $ast.scope ne 'has' {
            nqp::push_s($parts,$ast.scope);
            nqp::push_s($parts,' ');
        }
        nqp::push_s($parts,$type);
        nqp::push_s($parts,self!routine($ast));

        nqp::join('',$parts);
    }

    method !conditional($ast, str $type) {
        nqp::join(' ',nqp::list_s(
          $type,
          self.deparse($ast.condition),
          self.deparse($ast.then)
        ))
    }

    method !negated-conditional($ast, str $type) {
        nqp::join(' ',nqp::list_s(
          $type,
          self.deparse($ast.condition),
          self.deparse($ast.body)
        ))
    }

    method !simple-loop($ast, str $type) {
        nqp::join(' ',nqp::list_s(
          $type,
          self.deparse($ast.condition),
          self.deparse($ast.body)
        ))
    }

    method !simple-repeat($ast, str $type) {
        nqp::join(' ',nqp::list_s(
          'repeat',
          self.deparse($ast.body).chomp,
          $type,
          self.deparse($ast.condition),
        ))
    }

    my $processor-attribute := nqp::hash(
      'exec',       ':x',
      'quotewords', ':ww',
      'val',        ':v',
      'words',      ':w',
    );

    my $single-processor-prefix := nqp::hash(
      'exec',       'qqx/',
      'quotewords', 'qqww/',
      'val',        'qq:v/',
      'words',      'qqw/',
    );

    method !multiple-processors(str $string, @processors --> str) {
        my $parts := nqp::list_s('qq');

        nqp::push_s(
          $parts,
          nqp::ifnull(
            nqp::atkey($processor-attribute,$_),
            X::NYI.new(feature => "String processors '$_'").throw
          )
        ) for @processors;

        nqp::push_s($parts,$string);
        nqp::push_s($parts,'/');

        nqp::join('',$parts)
    }

    method !branches(RakuAST::Regex::Branching:D $ast, str $joiner --> str) {
        my $parts := nqp::list_s;

        if $ast.branches -> @branches {
            for @branches -> $branch {
                nqp::push_s($parts,self.deparse($branch))
            }
        }

        nqp::join($joiner,$parts)
    }

    method !quantifier(RakuAST::Regex::Quantifier:D $ast, str $quantifier --> str) {
        nqp::concat($quantifier,self.deparse($ast.backtrack))
    }

    method !parenthesize(str $inside --> str) {
        nqp::concat($.parens-open,nqp::concat($inside,$.parens-close))
    }

#--------------------------------------------------------------------------------
# Deparsing methods in alphabetical order

    multi method deparse(RakuAST::ApplyInfix:D $ast --> str) {
        nqp::join('',nqp::list_s(
          self.deparse($ast.left),
          $.before-infix,
          self.deparse($ast.infix),
          $.after-infix,
          self.deparse($ast.right)
        ))
    }

    multi method deparse(RakuAST::ApplyDottyInfix:D $ast --> str) {
        nqp::concat(
          self.deparse($ast.left),
          nqp::concat(
            self.deparse($ast.infix),
            self.deparse($ast.right)
          )
        )
    }

    multi method deparse(RakuAST::ApplyListInfix:D $ast --> str) {
        my $parts := nqp::list_s;
        for $ast.operands -> \operand {
            nqp::push_s($parts,self.deparse(operand));
        }
        nqp::elems($parts)
          ?? nqp::join(
               nqp::concat(
                 $.before-list-infix,
                 nqp::concat(
                   $ast.infix.operator,
                   $.after-list-infix
                 )
               ),
               $parts
             )
          !! '()'
    }

    multi method deparse(RakuAST::ApplyPostfix:D $ast --> str) {
        my $postfix := $ast.postfix;

        nqp::concat(
          self.deparse($ast.operand),
          nqp::concat(
            nqp::istype($postfix,RakuAST::Call) ?? '.' !! '',  # XXX yuck
            self.deparse($postfix)
          )
        )
    }

    multi method deparse(RakuAST::ApplyPrefix:D $ast --> str) {
        nqp::concat(self.deparse($ast.prefix),self.deparse($ast.operand))
    }

    multi method deparse(RakuAST::ArgList:D $ast --> str) {
        if $ast.args -> @args {
            my $parts    := nqp::list_s;
            my str $comma = $.list-infix-comma;

            for @args -> $arg {
                nqp::push_s($parts,self.deparse($arg));
                nqp::push_s($parts,$comma);
            }
            nqp::pop_s($parts);  # lose last comma

            nqp::join('',$parts)
        }
        else {
            ''
        }
    }

    multi method deparse(RakuAST::Block:D $ast --> str) {
        self.deparse($ast.body)
    }

    multi method deparse(RakuAST::Blockoid:D $ast --> str) {
        my $parts := nqp::list_s($.block-open);

        self!indent;
        nqp::push_s($parts,self.deparse($ast.statement-list));

        nqp::push_s($parts,self!dedent);
        nqp::push_s($parts,$.block-close);

        nqp::join('',$parts)
    }

    multi method deparse(RakuAST::Call::Method:D $ast --> str) {
        nqp::concat(
          self.deparse($ast.name),
          self!parenthesize(self.deparse($ast.args))
        )
    }

    multi method deparse(RakuAST::Call::QuotedMethod:D $ast --> str) {
        nqp::concat(
          self.deparse($ast.name),
          self!parenthesize(self.deparse($ast.args))
        )
    }

    multi method deparse(RakuAST::Call::MetaMethod:D $ast --> str) {
        nqp::concat(
          nqp::concat('^', $ast.name),
          self!parenthesize(self.deparse($ast.args))
        )
    }

    multi method deparse(RakuAST::Call::Name:D $ast --> str) {
        nqp::concat(
          self.deparse($ast.name),
          self!parenthesize(self.deparse($ast.args))
        )
    }

    multi method deparse(RakuAST::Call::Term:D $ast --> str) {
        self!parenthesize(self.deparse($ast.args))
    }

    multi method deparse(RakuAST::Circumfix::ArrayComposer:D $ast --> str) {
        (my $semilist := $ast.semilist)
          ?? nqp::concat(
               $.square-open,
               nqp::concat(
                 self.deparse($ast.semilist),
                 $.square-close
               )
             )
          !! '[]'
    }

    multi method deparse(RakuAST::Circumfix::HashComposer:D $ast --> str) {
        (my $expression := $ast.expression)
          ?? nqp::concat(
               $.bracket-open,
               nqp::concat(
                 self.deparse($expression),
                 $.bracket-close
               )
             )
          !! '{}'
    }

    multi method deparse(RakuAST::Circumfix::Parentheses:D $ast --> str) {
        (my $semilist := $ast.semilist)
          ?? self!parenthesize(self.deparse($semilist))
          !! '()'
    }

    multi method deparse(RakuAST::ColonPair:D $ast --> str) {
        nqp::concat(':',
          nqp::concat($ast.named-arg-name,
            nqp::concat('(',
              nqp::concat(self.deparse($ast.named-arg-value),')')
            )
          )
        )
    }

    multi method deparse(RakuAST::ColonPair::False:D $ast --> str) {
        nqp::concat(':!',$ast.named-arg-name)
    }

    multi method deparse(RakuAST::ColonPair::Number:D $ast --> str) {
        nqp::concat(':',
          nqp::concat(
            self.deparse($ast.value),
            $ast.named-arg-name
          )
        )
    }

    multi method deparse(RakuAST::ColonPair::True:D $ast --> str) {
        nqp::concat(':',$ast.named-arg-name)
    }

    multi method deparse(RakuAST::ColonPair::Value:D $ast --> str) {
        nqp::concat(':',
          nqp::concat($ast.named-arg-name,
            nqp::concat('(',
              nqp::concat(self.deparse($ast.value),')')
            )
          )
        )
    }

    multi method deparse(RakuAST::ColonPair::Variable:D $ast --> str) {
        nqp::concat(':',self.deparse($ast.value))
    }

    multi method deparse(RakuAST::CompUnit:D $ast --> str) {
        my str $deparsed = self.deparse($ast.statement-list);
        with $ast.finish-content {
            $deparsed ~= "\n=finish\n$_";
        }
        $deparsed
    }

    multi method deparse(RakuAST::Contextualizer $ast --> str) {
        nqp::concat($ast.sigil,self.context-target($ast.target))
    }

    multi method deparse(RakuAST::Declaration:D $ast --> str) {
        $ast.scope
    }

    multi method deparse(RakuAST::DottyInfix::Call:D $ast --> '.') { }

    multi method deparse(RakuAST::DottyInfix::CallAssign:D $ast --> '.=') { }

    multi method deparse(RakuAST::FatArrow:D $ast --> str) {
        nqp::concat(
          $ast.key,
          nqp::concat(
            $.fatarrow,
            self.deparse($ast.value)
          )
        )
    }

    multi method deparse(RakuAST::Infix:D $ast --> str) {
        $ast.operator
    }

    multi method deparse(RakuAST::Initializer::Assign:D $ast --> str) {
        nqp::concat($.assign,self.deparse($ast.expression))
    }

    multi method deparse(RakuAST::Initializer::Bind:D $ast --> str) {
        nqp::concat($.bind,self.deparse($ast.expression))
    }

    multi method deparse(RakuAST::IntLiteral:D $ast --> str) {
        $ast.value.raku
    }

    multi method deparse(RakuAST::MetaInfix::Assign:D $ast --> str) {
        nqp::concat(self.deparse($ast.infix),'=')
    }

    multi method deparse(RakuAST::Method:D $ast --> str) {
        self!method($ast, 'method')
    }

    multi method deparse(RakuAST::Name:D $ast --> str) {
        $ast.canonicalize
    }

    multi method deparse(RakuAST::NumLiteral:D $ast --> str) {
        $ast.value.raku
    }

    multi method deparse(RakuAST::Postcircumfix::ArrayIndex:D $ast --> str) {
        nqp::concat(
          $.square-open,
          nqp::concat(
            self.deparse($ast.index),
            $.square-close,
          )
        )
    }

    multi method deparse(RakuAST::Postcircumfix::HashIndex:D $ast --> str) {
        nqp::concat(
          $.bracket-open,
          nqp::concat(
            self.deparse($ast.index),
            $.bracket-close,
          )
        )
    }

    multi method deparse(RakuAST::Postcircumfix::LiteralHashIndex:D $ast --> str) {
        self.deparse($ast.index)  
    }

    multi method deparse(RakuAST::Postfix:D $ast --> str) {
        $ast.operator
    }

    multi method deparse(RakuAST::Prefix:D $ast --> str) {
        $ast.operator
    }

    multi method deparse(RakuAST::Package:D $ast --> str) {
        my $parts := nqp::list_s;

        if $ast.scope -> $scope {
            if $scope ne 'our' {
                nqp::push_s($parts,$scope);
            }
        }

        nqp::push_s($parts,$ast.package-declarator);
        nqp::push_s($parts,self.deparse($ast.name));

        if $ast.traits -> @traits {
            for @traits -> $trait {
                nqp::push_s($parts,self.deparse($trait));
            }
        }

        nqp::push_s($parts,self.deparse($ast.body));

        nqp::join(' ',$parts)
    }

    multi method deparse(RakuAST::Parameter:D $ast --> str) {
        my $parts := nqp::list_s;

        if $ast.type -> $type {
            nqp::push_s($parts,self.deparse($type));
            nqp::push_s($parts,' ') if $ast.target;
        }

        if $ast.target -> $target {
            my str $var = self.deparse($target, :slurpy($ast.slurpy));

            # named parameter
            if $ast.names -> @names {
                my str $varname = nqp::substr($var,1);  # lose the sigil
                my int $parens;
                my int $seen;

                for @names -> $name {
                    if $name eq $varname {
                        $seen = 1;
                    }
                    else {
                        nqp::push_s($parts,':');
                        nqp::push_s($parts,$name);
                        nqp::push_s($parts,'(');
                        ++$parens;
                    }
                }

                nqp::push_s($parts,':') if $seen;
                nqp::push_s($parts,$var);
                nqp::push_s($parts,nqp::x(')',$parens)) if $parens;
                nqp::push_s($parts,'?') if $ast.is-declared-optional;
                nqp::push_s($parts,'!') if $ast.is-declared-required;
            }

            # positional parameter
            else {
                given $ast.slurpy -> $prefix {
                    nqp::push_s($parts,self.deparse($prefix));
                }
                nqp::push_s($parts,$var);
                if $ast.invocant {
                    nqp::push_s($parts,':');
                }
                elsif $ast.is-declared-optional {
                    nqp::push_s($parts,'?');
                }
                elsif $ast.is-declared-required {
                    nqp::push_s($parts,'!');
                }
            }
        }

        nqp::join('',$parts)
    }

    multi method deparse(RakuAST::Parameter::Slurpy $ast --> '') { }

    multi method deparse(RakuAST::Parameter::Slurpy::Flattened $ast --> '*') { }

    multi method deparse(RakuAST::Parameter::Slurpy::SingleArgument $ast --> '+') { }

    multi method deparse(RakuAST::Parameter::Slurpy::Unflattened $ast --> '**') { }

    multi method deparse(RakuAST::Parameter::Slurpy::Capture $ast --> '|') { }

    multi method deparse(RakuAST::ParameterTarget::Var:D $ast --> str) {
        $ast.name
    }

    multi method deparse(RakuAST::ParameterTarget::Term:D $ast, :$slurpy --> str) {
        ($slurpy === RakuAST::Parameter::Slurpy ?? '\\' !! '') ~ $ast.name.canonicalize
    }

    multi method deparse(RakuAST::PointyBlock:D $ast --> str) {
        my $parts := nqp::list_s('-> ');

        if self.deparse($ast.signature) -> $signature {
            nqp::push_s($parts,$signature);
            nqp::push_s($parts,' ');
        }

        nqp::push_s($parts,self.deparse($ast.body));

        nqp::join('',$parts);
    }

    multi method deparse(RakuAST::QuotedRegex:D $ast --> str) {
        my str $starter = $ast.match-immediately ?? 'm' !! '';
        nqp::concat(
          nqp::concat($starter, $.regex-open),
          nqp::concat(self.deparse($ast.body),$.regex-close)
        )
    }

    multi method deparse(RakuAST::QuotedString:D $ast --> str) {
        my str $string = $ast.segments.map({
            nqp::istype($_,RakuAST::StrLiteral)
              ?? .value.raku.substr(1,*-1)
              !! nqp::istype($_,RakuAST::Block)
                ?? self.deparse($_).chomp
                !! self.deparse($_)
            }).join;

        if $ast.processors -> @processors {
            if @processors == 1 && @processors.head -> $processor {
                nqp::concat(
                  nqp::ifnull(
                    nqp::atkey($single-processor-prefix,$processor),
                    X::NYI.new(
                      feature => "Quoted string processor '$processor'"
                    ).throw
                  ),
                  nqp::concat($string,'/')
                )
            }
            elsif @processors == 2 {
                my str $joined = @processors.join(' ');
                if $joined eq 'words val' {
                    nqp::concat($.pointy-open,nqp::concat($string,$.pointy-close))
                }
                elsif $joined eq 'quotewords val' {
                    nqp::concat($.double-pointy-open,
                      nqp::concat($string,$.double-pointy-close)
                    )
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
            nqp::concat('"',nqp::concat($string,'"'))
        }
    }

    multi method deparse(RakuAST::QuoteWordsAtom:D $ast --> str) {
        self.deparse($ast.atom)
    }

    multi method deparse(RakuAST::RatLiteral:D $ast --> str) {
        $ast.value.raku
    }

    multi method deparse(RakuAST::Regex::Anchor::BeginningOfString:D $ast --> '^') { }

    multi method deparse(RakuAST::Regex::Anchor::EndOfString:D $ast --> '$') { }

    multi method deparse(RakuAST::Regex::Anchor::LeftWordBoundary:D $ast --> '<<') { }

    multi method deparse(RakuAST::Regex::Anchor::RightWordBoundary:D $ast --> '>>') { }

    multi method deparse(RakuAST::Regex::Literal:D $ast --> str) {
        my str $literal = $ast.text;
        nqp::iseq_i(
          nqp::findnotcclass(
            nqp::const::CCLASS_WORD,$literal,0,nqp::chars($literal)
          ),
          nqp::chars($literal)
        ) ?? $literal                                    # just word chars
          !! nqp::concat("'",nqp::concat($literal,"'"))  # need quoting
    }

    multi method deparse(RakuAST::Regex::Alternation:D $ast --> str) {
        self!branches($ast, $.regex-alternation)
    }

    multi method deparse(RakuAST::Regex::Assertion::Alias:D $ast --> str) {
        nqp::concat('<',
          nqp::concat($ast.name,
            nqp::concat('=',nqp::substr(self.deparse($ast.assertion),1))
          )
        )
    }

    multi method deparse(RakuAST::Regex::Assertion::Lookahead:D $ast --> str) {
        nqp::concat(
          $ast.negated ?? '<!' !! '<?',
          nqp::substr(self.deparse($ast.assertion),1)
        )
    }

    multi method deparse(RakuAST::Regex::Assertion::Named:D $ast --> str) {
        nqp::concat(
          $ast.capturing ?? '<' !! '<.',
          nqp::concat(self.deparse($ast.name),'>')
        )
    }

    multi method deparse(RakuAST::Regex::Assertion::Named::RegexArg:D $ast --> str) {
        nqp::concat(
          '<',
          nqp::concat(
            self.deparse($ast.name),
            nqp::concat(' ',
              nqp::concat(self.deparse($ast.regex-arg),'>')
            )
          )
        )
    }

    multi method deparse(RakuAST::Regex::Backtrack:U $ast --> '') { }

    multi method deparse(RakuAST::Regex::Backtrack::Frugal:U $ast --> '?') { }

    multi method deparse(RakuAST::Regex::Backtrack::Ratchet:U $ast --> ':') { }

    multi method deparse(RakuAST::Regex::CapturingGroup:D $ast --> str) {
        nqp::concat('(',nqp::concat(self.deparse($ast.regex),')'))
    }

    multi method deparse(RakuAST::Regex::CharClass::Any:D $ast --> '.') { }

    multi method deparse(RakuAST::Regex::CharClass::Digit:D $ast --> str) {
        $ast.negated ?? '\\D' !! '\\d'
    }

    multi method deparse(RakuAST::Regex::CharClass::Word:D $ast --> str) {
        $ast.negated ?? '\\W' !! '\\w'
    }

    multi method deparse(RakuAST::Regex::Conjunction:D $ast --> str) {
        self!branches($ast, $.regex-conjunction)
    }

    multi method deparse(RakuAST::Regex::Group:D $ast --> str) {
        nqp::concat(
          $.square-open,
          nqp::concat(self.deparse($ast.regex),$.square-close)
        )
    }

    multi method deparse(RakuAST::Regex::MatchFrom:D $ast --> '<(') { }

    multi method deparse(RakuAST::Regex::MatchTo:D $ast --> ')>') { }

    multi method deparse(RakuAST::Regex::QuantifiedAtom:D $ast --> str) {
        my $parts := nqp::list_s(
          self.deparse($ast.atom),
          self.deparse($ast.quantifier)
        );

        if $ast.separator -> $separator {
            nqp::push_s($parts,$ast.trailing-separator ?? ' %% ' !! ' % ');
            nqp::push_s($parts,self.deparse($separator));
        }

        nqp::join('',$parts)
    }

    multi method deparse(RakuAST::Regex::Quantifier::OneOrMore:D $ast --> str) {
        self!quantifier($ast, '+')
    }

    multi method deparse(RakuAST::Regex::Quantifier::ZeroOrMore:D $ast --> str) {
        self!quantifier($ast, '*')
    }

    multi method deparse(RakuAST::Regex::Quantifier::ZeroOrOne:D $ast --> str) {
        self!quantifier($ast, '?')
    }

    multi method deparse(RakuAST::Regex::Quote:D $ast --> str) {
        my str $quoted = self.deparse($ast.quoted);
        nqp::isgt_i(nqp::chars($quoted),2)
          ?? nqp::eqat($quoted,'"',0)
            ?? nqp::substr($quoted,1,nqp::sub_i(nqp::chars($quoted),2))
            !! nqp::concat('<{ ',nqp::concat($quoted, ' }>'))
          !! ''
    }

    multi method deparse(RakuAST::Regex::Sequence:D $ast --> str) {
        my $parts := nqp::list_s;

        if $ast.terms -> @terms {
            for @terms -> $term {
                nqp::push_s($parts,self.deparse($term))
            }
        }

        nqp::join(' ',$parts)
    }

    multi method deparse(RakuAST::Regex::SequentialAlternation:D $ast --> str) {
        self!branches($ast, $.regex-sequential-alternation)
    }

    multi method deparse(RakuAST::Signature:D $ast --> str) {
        if $ast.parameters -> @parameters {
            my $parts := nqp::list_s;
            for @parameters -> $parameter {
                nqp::push_s($parts,self.deparse($parameter));
            }
            nqp::join($!list-infix-comma,$parts)
        }
        else {
            ''
        }
    }

    multi method deparse(RakuAST::Statement::Catch:D $ast --> str) {
        nqp::concat('CATCH ',self.deparse($ast.body))
    }

    multi method deparse(RakuAST::Statement::Control:D $ast --> str) {
        nqp::concat('CONTROL ',self.deparse($ast.body))
    }

    multi method deparse(RakuAST::Statement::Default:D $ast --> str) {
        nqp::concat('default ',self.deparse($ast.body))
    }

    multi method deparse(RakuAST::Statement::Elsif:D $ast --> str) {
        self!conditional($ast, 'elsif');
    }

    multi method deparse(RakuAST::Statement::Empty:D $ast --> str) {
        ''
    }

    multi method deparse(RakuAST::Statement::Expression:D $ast --> str) {
        my $parts := nqp::list_s;
        nqp::push_s($parts,self.deparse($ast.expression));

        if $ast.condition-modifier -> $modifier {
            nqp::push_s($parts,' ');
            nqp::push_s($parts,self.deparse($modifier));
        }

        if $ast.loop-modifier -> $modifier {
            nqp::push_s($parts,' ');
            nqp::push_s($parts,self.deparse($modifier));
        }

        nqp::join('',$parts)
    }

    multi method deparse(RakuAST::Statement::For:D $ast --> str) {
        my $parts := nqp::list_s(
          'for',
          self.deparse($ast.source),
          self.deparse($ast.body)
        );

        if $ast.mode -> str $mode {
            nqp::unshift($parts,$mode) if $mode ne 'serial';
        }

        nqp::join(' ',$parts)
    }

    multi method deparse(RakuAST::Statement::Given:D $ast --> str) {
        nqp::join(' ',nqp::list_s(
          'given',
          self.deparse($ast.source),
          self.deparse($ast.body)
        ))
    }

    multi method deparse(RakuAST::Statement::If:D $ast --> str) {
        my $parts := nqp::list_s(self!conditional($ast, $ast.IMPL-QAST-TYPE));

        if $ast.elsifs -> @elsifs {
            for @elsifs -> $elsif {
                nqp::push_s($parts,self.deparse($elsif));
            }
        }

        if $ast.else -> $else {
            nqp::push_s($parts,'else ');
            nqp::push_s($parts,self.deparse($else));
        }

        nqp::join('',$parts)
    }

    multi method deparse(RakuAST::Statement::Loop:D $ast --> str) {
        nqp::join('',nqp::list_s(
          'loop (',
          self.deparse($ast.setup),
          $.loop-separator,
          self.deparse($ast.condition),
          $.loop-separator,
          self.deparse($ast.increment),
          ') ',
          self.deparse($ast.body)
        ))
    }

    multi method deparse(RakuAST::Statement::Loop::RepeatUntil:D $ast --> str) {
        self!simple-repeat($ast, 'until')
    }

    multi method deparse(RakuAST::Statement::Loop::RepeatWhile:D $ast --> str) {
        self!simple-repeat($ast, 'while')
    }

    multi method deparse(RakuAST::Statement::Loop::Until:D $ast --> str) {
        self!simple-loop($ast, 'until')
    }

    multi method deparse(RakuAST::Statement::Loop::While:D $ast --> str) {
        self!simple-loop($ast, 'while')
    }

    multi method deparse(RakuAST::Statement::Orwith:D $ast --> str) {
        self!conditional($ast, 'orwith');
    }

    multi method deparse(RakuAST::Statement::Unless:D $ast --> str) {
        self!negated-conditional($ast, 'unless');
    }

    multi method deparse(RakuAST::Statement::Use:D $ast --> str) {
        my $parts := nqp::list_s(
          'use ',
          self.deparse($ast.module-name)
        );

        if $ast.argument -> $argument {
            nqp::push_s($parts,' ');
            nqp::push_s($parts,self.deparse($argument));
        }

        nqp::join('',$parts)
    }

    multi method deparse(RakuAST::Statement::When:D $ast --> str) {
        nqp::join(' ',nqp::list_s(
          'when',
          self.deparse($ast.condition),
          self.deparse($ast.body)
        ))
    }

    multi method deparse(RakuAST::Statement::Without:D $ast --> str) {
        self!negated-conditional($ast, 'without');
    }

    multi method deparse(RakuAST::StatementList:D $ast --> str) {

        if $ast.statements -> @statements {
            my $parts     := nqp::list_s;
            my str $spaces = $.indent-spaces;
            my str $end    = $.end-statement;

            my str $code;
            my $dropped;
            for @statements -> $statement {
                nqp::push_s($parts,$spaces);
                nqp::push_s($parts,$code = self.deparse($statement));
                nqp::push_s($parts,$end)
                  unless $dropped = $code.ends-with("\}\n");
            }

            nqp::pop_s($parts) unless $dropped;  # lose the last end?
            nqp::push_s($parts,$.last-statement)
              unless $code.ends-with("\}\n");

            nqp::join('',$parts)
        }

        else {
            ''
        }
    }

    multi method deparse(
      RakuAST::StatementModifier::Given:D $ast
    --> str) {
        nqp::concat('given ',self.deparse($ast.expression))
    }

    multi method deparse(
      RakuAST::StatementModifier::If:D $ast
    --> str) {
        nqp::concat('if ',self.deparse($ast.expression))
    }

    multi method deparse(
      RakuAST::StatementModifier::For:D $ast
    --> str) {
        nqp::concat('for ',self.deparse($ast.expression))
    }

    multi method deparse(
      RakuAST::StatementModifier::Unless:D $ast
    --> str) {
        nqp::concat('unless ',self.deparse($ast.expression))
    }

    multi method deparse(
      RakuAST::StatementModifier::Until:D $ast
    --> str) {
        nqp::concat('until ',self.deparse($ast.expression))
    }

    multi method deparse(
      RakuAST::StatementModifier::When:D $ast
    --> str) {
        nqp::concat('when ',self.deparse($ast.expression))
    }

    multi method deparse(
      RakuAST::StatementModifier::While:D $ast
    --> str) {
        nqp::concat('while ',self.deparse($ast.expression))
    }

    multi method deparse(
      RakuAST::StatementModifier::With:D $ast
    --> str) {
        nqp::concat('with ',self.deparse($ast.expression))
    }

    multi method deparse(
      RakuAST::StatementModifier::Without:D $ast
    --> str) {
        nqp::concat('without ',self.deparse($ast.expression))
    }

    multi method deparse(RakuAST::StatementPrefix::Do:D $ast --> str) {
        nqp::concat('do ',self.deparse($ast.blorst))
    }

    multi method deparse(RakuAST::StatementPrefix::Eager:D $ast --> str) {
        nqp::concat('eager ',self.deparse($ast.blorst))
    }

    multi method deparse(RakuAST::StatementPrefix::Gather:D $ast --> str) {
        nqp::concat('gather ',self.deparse($ast.blorst))
    }

    multi method deparse(RakuAST::StatementPrefix::Hyper:D $ast --> str) {
        nqp::concat('hyper ',self.deparse($ast.blorst))
    }

    multi method deparse(RakuAST::StatementPrefix::Lazy:D $ast --> str) {
        nqp::concat('lazy ',self.deparse($ast.blorst))
    }

    multi method deparse(RakuAST::StatementPrefix::Quietly:D $ast --> str) {
        nqp::concat('quietly ',self.deparse($ast.blorst))
    }

    multi method deparse(RakuAST::StatementPrefix::Phaser::Begin:D $ast --> str) {
        nqp::concat('BEGIN ',self.deparse($ast.blorst))
    }

    multi method deparse(RakuAST::StatementPrefix::Phaser::End:D $ast --> str) {
        nqp::concat('END ',self.deparse($ast.blorst))
    }

    multi method deparse(RakuAST::StatementPrefix::Race:D $ast --> str) {
        nqp::concat('race ',self.deparse($ast.blorst))
    }

    multi method deparse(RakuAST::StatementPrefix::Start:D $ast --> str) {
        nqp::concat('start ',self.deparse($ast.blorst))
    }

    multi method deparse(RakuAST::StatementPrefix::Try:D $ast --> str) {
        nqp::concat('try ',self.deparse($ast.blorst))
    }

    multi method deparse(RakuAST::StrLiteral:D $ast --> str) {
        $ast.value.raku
    }

    multi method deparse(RakuAST::Stub:D $ast --> str) {
        if $ast.args -> $args {
            nqp::concat($ast.IMPL-STUB-NAME,
              nqp::concat(' ',self.deparse($args))
            )
        }
        else {
            $ast.IMPL-STUB-NAME
        }
    }

    multi method deparse(RakuAST::Sub:D $ast --> str) {
        my $parts := nqp::list_s;

        given $ast.scope -> $scope {
            if $scope ne 'my' && ($ast.name || $scope ne 'anon') {
                nqp::push_s($parts,$scope);
                nqp::push_s($parts,' ');
            }
        }
        nqp::push_s($parts,'sub');
        nqp::push_s($parts,self!routine($ast));

        nqp::join('',$parts);
    }

    multi method deparse(RakuAST::Submethod:D $ast --> str) {
        self!method($ast, 'submethod')
    }

    multi method deparse(RakuAST::Term::Capture:D $ast --> str) {
        nqp::concat(
          '\\',
          self!parenthesize(self.deparse($ast.source))
        )
    }

    multi method deparse(RakuAST::Term::HyperWhatever:D $ast --> '**') { }

    multi method deparse(RakuAST::Term::Name:D $ast --> str) {
        self.deparse($ast.name)
    }

    multi method deparse(RakuAST::Term::Named:D $ast --> str) {
        $ast.name
    }

    multi method deparse(RakuAST::Term::Rand:D $ast --> 'rand') { }

    multi method deparse(RakuAST::Term::Reduce:D $ast --> str) {
        nqp::concat(($ast.triangle ?? $.reduce-triangle !! $.reduce-open),
          nqp::concat(self.deparse($ast.infix),
            nqp::concat(
              $.reduce-close,
              self!parenthesize(self.deparse($ast.args))
            )
          )
        )
    }

    multi method deparse(RakuAST::Term::EmptySet:D $ast --> '∅') { }

    multi method deparse(RakuAST::Term::Self:D $ast --> 'self') { }

    multi method deparse(RakuAST::Term::TopicCall:D $ast --> str) {
        nqp::concat('.',self.deparse($ast.call))
    }

    multi method deparse(RakuAST::Term::Whatever:D $ast --> '*') { }

    multi method deparse(RakuAST::Ternary:D $ast --> str) {
        nqp::join('',nqp::list_s(
          self.deparse($ast.condition),
          $.ternary1,
          self.deparse($ast.then),
          $.ternary2,
          self.deparse($ast.else)
        ))
    }

    multi method deparse(RakuAST::Trait::Is:D $ast --> str) {
        my str $base = $ast.IMPL-TRAIT-NAME ~ ' ' ~ self.deparse($ast.name);
        with $ast.argument { $base ~ self.deparse($_) } else { $base }
    }

    multi method deparse(RakuAST::Trait::Hides:D $ast --> str) {
        self!typish-trait($ast)
    }

    multi method deparse(RakuAST::Trait::Does:D $ast --> str) {
        self!typish-trait($ast)
    }

    multi method deparse(RakuAST::Trait::Of:D $ast --> str) {
        self!typish-trait($ast)
    }

    multi method deparse(RakuAST::Trait::Returns:D $ast --> str) {
        self!typish-trait($ast)
    }

    method !typish-trait(RakuAST::Trait:D $ast --> str) {
        nqp::concat(
          $ast.IMPL-TRAIT-NAME,
          nqp::concat(
            ' ',
            self.deparse($ast.type)
          )
        )
    }

    multi method deparse(RakuAST::Type::Simple:D $ast --> str) {
        self.deparse($ast.name)
    }

    multi method deparse(RakuAST::Type::Definedness:D $ast --> str) {
        self.deparse($ast.name) ~ ($ast.definite ?? ':D' !! ':U')
    }

    multi method deparse(RakuAST::Type::Coercion:D $ast --> str) {
        self.deparse($ast.name) ~ '(' ~ self.deparse($ast.constraint) ~ ')'
    }

    multi method deparse(RakuAST::Type::Parameterized:D $ast --> str) {
        self.deparse($ast.name) ~ '[' ~ self.deparse($ast.args) ~ ']'
    }

    multi method deparse(RakuAST::Var:D $ast --> str) {
        $ast.name
    }

    multi method deparse(RakuAST::Var::Compiler::File:D $ast --> '$?FILE') { }

    multi method deparse(RakuAST::Var::Compiler::Line:D $ast --> '$?LINE') { }

    multi method deparse(RakuAST::Var::NamedCapture:D $ast --> str) {
        nqp::concat('$',self.deparse($ast.index))
    }

    multi method deparse(RakuAST::Var::PositionalCapture:D $ast --> str) {
        nqp::concat('$',$ast.index.Str)
    }

    multi method deparse(RakuAST::VarDeclaration::Implicit:D $ast --> str) {
        $ast.name
    }

    multi method deparse(RakuAST::VarDeclaration::Placeholder::Positional:D $ast --> str) {
        .substr(0, 1) ~ '^' ~ .substr(1) given $ast.lexical-name
    }

    multi method deparse(RakuAST::VarDeclaration::Placeholder::Named:D $ast --> str) {
        .substr(0, 1) ~ ':' ~ .substr(1) given $ast.lexical-name
    }

    multi method deparse(RakuAST::VarDeclaration::Placeholder::SlurpyArray:D $ast --> str) {
        '@_'
    }

    multi method deparse(RakuAST::VarDeclaration::Placeholder::SlurpyHash:D $ast --> str) {
        '%_'
    }

    multi method deparse(RakuAST::VarDeclaration::Simple:D $ast --> str) {
        my $parts := nqp::list_s;

        nqp::push_s($parts,$ast.scope);
        nqp::push_s($parts,' ');

        if $ast.type -> $type {
            nqp::push_s($parts,self.deparse($type));
            nqp::push_s($parts,' ');
        }

        nqp::push_s($parts,nqp::istype($ast, RakuAST::VarDeclaration::Anonymous)
            ?? $ast.sigil
            !! $ast.name);
        if $ast.initializer -> $initializer {
            nqp::push_s($parts,self.deparse($initializer));
        }

        nqp::join('', $parts)
    }

    multi method deparse(RakuAST::VarDeclaration::Term:D $ast --> str) {
        my $parts := nqp::list_s;

        nqp::push_s($parts,$ast.scope);
        nqp::push_s($parts,' ');

        if $ast.type -> $type {
            nqp::push_s($parts,self.deparse($type));
            nqp::push_s($parts,' ');
        }

        nqp::push_s($parts,'\\');
        nqp::push_s($parts,self.deparse($ast.name));

        nqp::push_s($parts,self.deparse($ast.initializer));

        nqp::join('', $parts)
    }

    multi method deparse(RakuAST::VersionLiteral:D $ast --> str) {
        $ast.value.raku
    }
}

# vim: expandtab shiftwidth=4
