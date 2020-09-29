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

    has str $.bracket-open  = '{';
    has str $.bracket-close = '}';

    has str $.pointy-open  = '<';
    has str $.pointy-close = '>';

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

    method indent(--> str) {
        $!indent-spaces = nqp::concat($!indent-spaces,$!indent-with)
    }

    method dedent(--> str) {
        $!indent-spaces = nqp::substr(
          $!indent-spaces,
          0,
          nqp::chars($!indent-spaces) - nqp::chars($!indent-with)
        )
    }

#--------------------------------------------------------------------------------
# Private helper methods

    method !routine(RakuAST::Routine:D $ast --> str) {
        my $parts := nqp::list_s;

        nqp::push_s($parts,' ');
        if $ast.name -> $name {
            nqp::push_s($parts,self.deparse($name));
        }

        nqp::push_s($parts,$.parens-open);
        nqp::push_s($parts,self.deparse($ast.signature));
        nqp::push_s($parts,$.parens-close);

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
        nqp::join('',nqp::list_s(
          self.deparse($ast.left),
          self.deparse($ast.infix),
          self.deparse($ast.right)
        ))
    }

    multi method deparse(RakuAST::ApplyListInfix:D $ast --> str) {
        my $parts := nqp::list_s;
        for $ast.operands -> \operand {
            nqp::push_s($parts,self.deparse(operand));
        }
        nqp::elems($parts)
          ?? nqp::join(
               nqp::join('',nqp::list_s(
                 $.before-list-infix,
                 $ast.infix.operator,
                 $.after-list-infix
               )),
               $parts
             )
          !! '()'
    }

    multi method deparse(RakuAST::ApplyPostfix:D $ast --> str) {
        my $postfix := $ast.postfix;

        nqp::join('',nqp::list_s(
          self.deparse($ast.operand),
          nqp::istype($postfix,RakuAST::Call) ?? '.' !! '',
          self.deparse($postfix)
        ))
    }

    multi method deparse(RakuAST::ApplyPrefix:D $ast --> str) {
        nqp::concat(self.deparse($ast.prefix),self.deparse($ast.operand))
    }

    multi method deparse(RakuAST::ArgList:D $ast --> str) {
        if $ast.args -> @args {
            my $parts    := nqp::list_s;
            my str $comma = $.list-infix-comma;

            nqp::push_s($parts,$.parens-open);
            for @args -> $arg {
                nqp::push_s($parts,self.deparse($arg));
                nqp::push_s($parts,$comma);
            }
            nqp::pop_s($parts);  # lose last comma
            nqp::push_s($parts,$.parens-close);

            nqp::join('',$parts)
        }
        else {
            ''
        }

    }

    multi method deparse(RakuAST::Block:D $ast --> str) {
        self.deparse($ast.body)    # XXX probably needs more work
    }

    multi method deparse(RakuAST::Blockoid:D $ast --> str) {
        my $parts := nqp::list_s;
        nqp::push_s($parts,"\{\n");

        self.indent;
        nqp::push_s($parts,self.deparse($ast.statement-list));

        nqp::push_s($parts,self.dedent);
        nqp::push_s($parts,"}\n");

        nqp::join('',$parts)
    }

    multi method deparse(RakuAST::Call::Name:D $ast --> str) {
        nqp::concat(self.deparse($ast.name),self.deparse($ast.args))
    }

    multi method deparse(RakuAST::Call::Term:D $ast --> str) {
        self.deparse($ast.args)
    }

    multi method deparse(RakuAST::Call::Method:D $ast --> str) {
        nqp::concat(self.deparse($ast.name),self.deparse($ast.args))
    }

    multi method deparse(RakuAST::Circumfix::ArrayComposer:D $ast --> str) {
        (my $semilist := $ast.semilist)
          ?? nqp::join('',nqp::list_s(
               $.square-open,
               self.deparse($ast.semilist),
               $.square-close,
             ))
          !! '[]'
    }

    multi method deparse(RakuAST::Circumfix::HashComposer:D $ast --> str) {
        (my $expression := $ast.expression)
          ?? nqp::join('',nqp::list_s(
               $.bracket-open,
               self.deparse($expression),
               $.bracket-close,
             ))
          !! '{}'
    }

    multi method deparse(RakuAST::Circumfix::Parentheses:D $ast --> str) {
        (my $semilist := $ast.semilist)
          ?? nqp::join('',nqp::list_s(
               $.parens-open,
               self.deparse($ast.semilist),
               $.parens-close,
             ))
          !! '()'
    }

    multi method deparse(RakuAST::ColonPair:D $ast --> str) {
        nqp::join('',nqp::list_s(
          ':',
          $ast.named-arg-name,
          '(',
          self.deparse($ast.named-arg-value),
          ')'
        ))
    }

    multi method deparse(RakuAST::ColonPair::False:D $ast --> str) {
        nqp::concat(':!',$ast.named-arg-name)
    }

    multi method deparse(RakuAST::ColonPair::Number:D $ast --> str) {
        nqp::join('',nqp::list_s(
          ':',
          self.deparse($ast.value),
          $ast.named-arg-name
        ))
    }

    multi method deparse(RakuAST::ColonPair::True:D $ast --> str) {
        nqp::concat(':',$ast.named-arg-name)
    }

    multi method deparse(RakuAST::ColonPair::Value:D $ast --> str) {
        nqp::join('',nqp::list_s(
          ':',
          $ast.named-arg-name,
          '(',
          self.deparse($ast.value),
          ')'
        ))
    }

    multi method deparse(RakuAST::ColonPair::Variable:D $ast --> str) {
        nqp::join('',nqp::list_s(':',self.deparse($ast.value)))
    }

    multi method deparse(RakuAST::CompUnit:D $ast --> str) {
        self.deparse($ast.statement-list)
    }

    multi method deparse(RakuAST::Declaration:D $ast --> str) {
        $ast.scope
    }

    multi method deparse(RakuAST::DottyInfix::Call:D $ast --> str) {
        '.'
    }

    multi method deparse(RakuAST::DottyInfix::CallAssign:D $ast --> str) {
        '.='
    }

    multi method deparse(RakuAST::FatArrow:D $ast --> str) {
        nqp::join('',nqp::list_s(
          $ast.key,
          $.fatarrow,
          self.deparse($ast.value)
        ))
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
        nqp::join('',nqp::list_s(
          $.square-open,
          self.deparse($ast.index),
          $.square-close,
        ))
    }

    multi method deparse(RakuAST::Postcircumfix::LiteralHashIndex:D $ast --> str) {
        nqp::join('',nqp::list_s(
          $.pointy-open,
          self.deparse($ast.index),
          $.pointy-close,
        ))
    }

    multi method deparse(RakuAST::Postcircumfix::HashIndex:D $ast --> str) {
        nqp::join('',nqp::list_s(
          $.bracket-open,
          self.deparse($ast.index),
          $.bracket-close,
        ))
    }

    multi method deparse(RakuAST::Postfix:D $ast --> str) {
        $ast.operator
    }

    multi method deparse(RakuAST::Prefix:D $ast --> str) {
        $ast.operator
    }

    multi method deparse(RakuAST::QuotedString:D $ast --> str) {
        $ast.literal-value.join(' ')
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
                nqp::push_s($parts,'!') unless $ast.optional;
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
                elsif $ast.optional {
                    nqp::push_s($parts,'?');
                }
            }
        }

        nqp::join('',$parts)
    }

    multi method deparse(RakuAST::Parameter::Slurpy $ast --> str) {
        ''
    }

    multi method deparse(RakuAST::Parameter::Slurpy::Flattened $ast --> str) {
        '*'
    }

    multi method deparse(RakuAST::Parameter::Slurpy::SingleArgument $ast --> str) {
        '+'
    }

    multi method deparse(RakuAST::Parameter::Slurpy::Unflattened $ast --> str) {
        '**'
    }

    multi method deparse(RakuAST::Parameter::Slurpy::Capture $ast --> str) {
        '|'
    }

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

    multi method deparse(RakuAST::RatLiteral:D $ast --> str) {
        $ast.value.raku
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
        self.deparse($ast.expression)
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
        nqp::concat('\\',self.deparse($ast.source))
    }

    multi method deparse(RakuAST::Term::EmptySet:D $ast --> str) {
        '∅'
    }

    multi method deparse(RakuAST::Term::HyperWhatever:D $ast --> str) {
        '**'
    }

    multi method deparse(RakuAST::Term::Name:D $ast --> str) {
        self.deparse($ast.name)
    }

    multi method deparse(RakuAST::Term::Named:D $ast --> str) {
        $ast.name
    }

    multi method deparse(RakuAST::Term::Rand:D $ast --> str) {
        'rand'
    }

    multi method deparse(RakuAST::Term::Self:D $ast --> str) {
        'self'
    }

    multi method deparse(RakuAST::Term::TopicCall:D $ast --> str) {
        nqp::concat('.',self.deparse($ast.call))
    }

    multi method deparse(RakuAST::Term::Whatever:D $ast --> str) {
        '*'
    }

    multi method deparse(RakuAST::Ternary:D $ast --> str) {
        nqp::join('',nqp::list_s(
          self.deparse($ast.condition),
          $.ternary1,
          self.deparse($ast.then),
          $.ternary2,
          self.deparse($ast.else)
        ))
    }

    multi method deparse(RakuAST::Trait:D $ast --> str) {
        nqp::join('',nqp::list_s('',
          $ast.IMPL-TRAIT-NAME,
          ' ',
          self.deparse($ast.type)
        ))
    }

    multi method deparse(RakuAST::Type::Simple:D $ast --> str) {
        self.deparse($ast.name)
    }

    multi method deparse(RakuAST::Var:D $ast --> str) {
        $ast.name
    }

    multi method deparse(RakuAST::Var::Compiler::File:D $ast --> str) {
        '$?FILE'
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

    multi method deparse(RakuAST::Var::Compiler::Line:D $ast --> str) {
        '$?LINE'
    }

    multi method deparse(RakuAST::Var::NamedCapture:D $ast --> str) {
        nqp::join('',nqp::list_s('$<',self.deparse($ast.index),'>'))
    }

    multi method deparse(RakuAST::Var::PositionalCapture:D $ast --> str) {
        nqp::concat('$',$ast.index.Str)
    }

    multi method deparse(RakuAST::VarDeclaration::Implicit:D $ast --> str) {
        $ast.name
    }

    multi method deparse(RakuAST::VarDeclaration::Simple:D $ast --> str) {
        my $parts := nqp::list_s;

        nqp::push_s($parts,$ast.scope);
        nqp::push_s($parts,' ');

        if $ast.type -> $type {
            nqp::push_s($parts,self.deparse($type));
            nqp::push_s($parts,' ');
        }

        nqp::push_s($parts,$ast.name);
        if $ast.initializer -> $initializer {
            nqp::push_s($parts,self.deparse($initializer));
        }

        nqp::join('', $parts)
    }

    multi method deparse(RakuAST::VersionLiteral:D $ast --> str) {
        $ast.value.raku
    }
}

# vim: expandtab shiftwidth=4
