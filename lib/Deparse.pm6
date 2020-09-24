# This is a TEMPORARY location of the RakuAST::Deparse file to allow for
# easier initial development of this module (allowing for testing without
# require a full core-setting build).

# This is the default class handling deparsing (aka, converting a given
# RakuAST::Node object into Raku source code).  It allows for all sorts
# of customization, and can be subclassed for further optimizations.

# Apart from the .new method, which expects named parameters, each public
# method expects an instance if a subclass of a RakuAST::Node as the first
# positional parameter.

use nqp;

class RakuAST::Deparse {
    has str $.before-comma = ' ';
    has str $.after-comma  = ' ';

    has str $.after-parens-open   = '';
    has str $.before-parens-close = '';

    has str $.before-infix = ' ';
    has str $.after-infix  = ' ';
    has str $!list-infix-comma;

    has str $.before-list-infix = '';
    has str $.after-list-infix  = ' ';

    has str $.fatarrow = ' => ';

    has str $.ternary1  = ' ?? ';
    has str $.ternary2  = ' !! ';

    method TWEAK(--> Nil) {
        $!list-infix-comma = nqp::join('',nqp::list_s(
          $.before-infix,',',$.after-infix
        ));
    }

    proto method deparse(|) {*}

    # Base class catcher
    multi method deparse(RakuAST::Node:D $ast) {
        X::NYI.new(feature => "Deparsing $ast.^name() objects").throw
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
          $.before-infix,
          self.deparse($ast.infix),
          $.after-infix,
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
        nqp::concat(self.deparse($ast.operand),self.deparse($ast.postfix))
    }

    multi method deparse(RakuAST::ApplyPrefix:D $ast --> str) {
        nqp::concat(self.deparse($ast.prefix),self.deparse($ast.operand))
    }

    multi method deparse(RakuAST::Circumfix::ArrayComposer:D $ast --> str) {
        (my $semilist := $ast.semilist)
          ?? nqp::join('',nqp::list_s(
               '[',
               $.after-parens-open,
               self.deparse($ast.semilist),
               $.before-parens-close,
               ']'
             ))
          !! '[]'
    }

    multi method deparse(RakuAST::Circumfix::HashComposer:D $ast --> str) {
        (my $expression := $ast.expression)
          ?? nqp::join('',nqp::list_s(
               '(',
               $.after-parens-open,
               self.deparse($expression),
               $.before-parens-close,
               ')'
             ))
          !! '{}'
    }

    multi method deparse(RakuAST::Circumfix::Parentheses:D $ast --> str) {
        (my $semilist := $ast.semilist)
          ?? nqp::join('',nqp::list_s(
               '(',
               $.after-parens-open,
               self.deparse($ast.semilist),
               $.before-parens-close,
               ')'
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
          self.deparse($ast.key),
          $.fatarrow,
          self.deparse($ast.value)
        ))
    }

    multi method deparse(RakuAST::Infix:D $ast --> str) {
        $ast.operator
    }

    multi method deparse(RakuAST::IntLiteral:D $ast --> str) {
        $ast.value.raku
    }

    multi method deparse(RakuAST::MetaInfix::Assign:D $ast --> str) {
        $ast.operator ~ '='
    }

    multi method deparse(RakuAST::NumLiteral:D $ast --> str) {
        $ast.value.raku
    }

    multi method deparse(RakuAST::Postcircumfix::ArrayIndex:D $ast --> str) {
        nqp::join('',nqp::list_s(
          '[',
          $.after-parens-open,
          self.deparse($ast.index),
          $.before-parens-close,
          ']'
        ))
    }

    multi method deparse(RakuAST::Postcircumfix::LiteralHashIndex:D $ast --> str) {
        nqp::join('',nqp::list_s(
          '<',
          $.after-parens-open,
          self.deparse($ast.index),
          $.before-parens-close,
          '>'
        ))
    }

    multi method deparse(RakuAST::Postcircumfix::HashIndex:D $ast --> str) {
        nqp::join('',nqp::list_s(
          '{',
          $.after-parens-open,
          self.deparse($ast.index),
          $.before-parens-close,
          '}'
        ))
    }

    multi method deparse(RakuAST::Postfix:D $ast --> str) {
        $ast.operator
    }

    multi method deparse(RakuAST::Prefix:D $ast --> str) {
        $ast.operator
    }

    multi method deparse(RakuAST::QuotedString:D $ast --> str) {
        $ast.literal-value
    }

    multi method deparse(RakuAST::Parameter:D $ast --> str) {
        my $parts := nqp::list_s;

        if $ast.type -> $type {
            nqp::push_s($parts,self.deparse($type));
            nqp::push_s($parts,' ') if self.target;
        }
        
        if $ast.target -> $target {
            my str $var = $target.lexical-name;

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
                if $ast.slurpy -> $prefix {
                    nqp::push_s($parts,$prefix);
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

    multi method deparse(RakuAST::RatLiteral:D $ast --> str) {
        $ast.value.raku
    }

    multi method deparse(RakuAST::Signature:D $ast --> str) {
        my $parts := nqp::list_s;
        for $ast.parameters -> \parameter {
            nqp::push_s($parts,self.deparse(parameter));
        }
        nqp::elems($parts)
          ?? nqp::join($!list-infix-comma,$parts)
          !! '()'
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

    multi method deparse(RakuAST::Ternary:D $ast --> str) {
        nqp::join('',nqp::list_s(
          self.deparse($ast.condition),
          $.ternary1,
          self.deparse($ast.then),
          $.ternary2,
          self.deparse($ast.else)
        ))
    }

    multi method deparse(RakuAST::Var:D $ast --> str) {
        $ast.name
    }

    multi method deparse(RakuAST::Var::Compiler::File:D $ast --> str) {
        '$?FILE'
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

    multi method deparse(RakuAST::VersionLiteral:D $ast --> str) {
        $ast.value.raku
    }
}

#--------------------------------------------------------------------------------
INIT nqp::bindhllsym('Raku','DEPARSE',RakuAST::Deparse);
