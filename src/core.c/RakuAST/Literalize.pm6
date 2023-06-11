# This augments the RakuAST::Node class so that all of its subclasses can
# generate sensible .literalize.
#
# The .literalize method attempts to create literal HLL objects for the
# given RakuAST::Node object, and returns Nil if for some reason it is
# impossible to do so.

augment class RakuAST::Node {
    my class CannotLiteralize is Exception { }

    proto method literalize(RakuAST::Node:) {
        CATCH {
            when CannotLiteralize {
                return Nil;
            }
        }
        unless $++ {
            say self if %*ENV<RAKUDO_LITERALIZE_DEBUG>;
        }
        {*}
    }

#- helper subs -----------------------------------------------------------------

    # Return a Callable for the given infix op as string, returns
    # a Failure if the infix op could not be found
    my sub infix-op(str $op) {
        ::(Q/&infix:</ ~ $op ~ Q/>/) // ::(Q/&infix:«/ ~ $op ~ Q/»/)
    }

    # Return a Callable for the given infix op as string, returns
    # a Failure if the prefix op could not be found
    my sub prefix-op(str $op) {
        ::(Q/&prefix:</ ~ $op ~ Q/>/) // ::(Q/&prefix:«/ ~ $op ~ Q/»/)
    }

#- A ---------------------------------------------------------------------------

    multi method literalize(RakuAST::Node:D:) {
        nqp::die('literalize on ' ~ self.HOW.name(self) ~ ' NYI');
    }

    multi method literalize(RakuAST::ApplyInfix:D:) {
        if infix-op(self.infix.operator) -> &op {
            my $left  := self.left.literalize;
            my $right := self.right.literalize;
            nqp::istype($left,Nil) || nqp::istype($right,Nil)
              ?? CannotLiteralize.new.throw
              !! op($left,$right)
        }
        else {
            CannotLiteralize.new.throw
        }
    }

    multi method literalize(RakuAST::ApplyListInfix:D:) {
        my str $operator = self.infix.operator;

        if $operator eq ',' {
            self.operands.map(*.literalize).List
        }
        elsif infix-op(self.infix.operator) -> &op {
            op(self.operands.map(*.literalize))
        }
        else {
            CannotLiteralize.new.throw
        }
    }

    multi method literalize(RakuAST::ApplyPrefix:D:) {
        if prefix-op(self.prefix.operator) -> &op {
            op(self.operand.literalize)
        }
        else {
            CannotLiteralize.new.throw
        }
    }

#- B ---------------------------------------------------------------------------

    multi method literalize(RakuAST::Block:D:) {
        self.body.statement-list.literalize.Map
    }

#- Circumfix -------------------------------------------------------------------

    multi method literalize(RakuAST::Circumfix::ArrayComposer:D:) {
        self.semilist.literalize
    }

    multi method literalize(RakuAST::Circumfix::HashComposer:D:) {
        self.expression.literalize.Map
    }

    multi method literalize(RakuAST::Circumfix::Parentheses:D:) {
        self.semilist.literalize
    }

#- ColonPair -------------------------------------------------------------------

    multi method literalize(RakuAST::ColonPair::False:D:) {
        Pair.new: self.key, False
    }

    multi method literalize(RakuAST::ColonPair::Number:D:) {
        Pair.new: self.key, self.value.compile-time-value
    }

    multi method literalize(RakuAST::ColonPair::True:D:) {
        Pair.new: self.key, True
    }

    multi method literalize(RakuAST::ColonPair::Value:D:) {
        Pair.new: self.key, self.value.literalize
    }

#- D ---------------------------------------------------------------------------

    multi method literalize(RakuAST::Declaration::ResolvedConstant:D:) {
        self.compile-time-value
    }

#- F ---------------------------------------------------------------------------

    multi method literalize(RakuAST::FatArrow:D:) {
        Pair.new: self.key, self.value.literalize
    }

#- L ---------------------------------------------------------------------------

    # handles Int/Str/Num/Rat/ComplexLiteral
    multi method literalize(RakuAST::Literal:D:) {
        self.value
    }

#- Q ---------------------------------------------------------------------------

    multi method literalize(RakuAST::QuotedString:D:) {
        self.literal-value
    }

#- S ---------------------------------------------------------------------------

    multi method literalize(RakuAST::SemiList:D:) {
        my     $stmts := self.statements;
        my int $elems  = $stmts.elems;

        $elems
          ?? $elems == 1
            ?? $stmts.head.literalize
            !! $stmts.map(*.literalize).List
          !! ()
    }

#- Statement -------------------------------------------------------------------

    multi method literalize(RakuAST::Statement::Expression:D:) {
        if self.condition-modifier // self.loop-modifier {
            CannotLiteralize.new.throw
        }
        else {
            self.expression.literalize
        }
    }

    multi method literalize(RakuAST::StatementList:D:) {
        my     $stmts := self.statements;
        my int $elems  = $stmts.elems;

        $elems
          ?? $elems == 1
            ?? $stmts.head.literalize
            !! $stmts.map(*.literalize).List
          !! ()
    }

#- Term ------------------------------------------------------------------------

    multi method literalize(RakuAST::Term::Name:D:) {
        my str $name = self.name.canonicalize;

        $name eq 'True'
          ?? True
          !! $name eq 'False'
            ?? False
            !! self.resolution.compile-time-value
    }

    multi method literalize(RakuAST::Term::RadixNumber:D:) {
        (self.multi-part ?? &UNBASE_BRACKET !! &UNBASE)(
          self.radix, self.value.literalize
        )
    }

#- Var -------------------------------------------------------------------------

    multi method literalize(RakuAST::Var::Lexical:D:) {
        CannotLiteralize.new.throw
    }

    multi method literalize(RakuAST::Var::Compiler::File:D:) {
        self.file
    }

    multi method literalize(RakuAST::Var::Compiler::Line:D:) {
        self.line
    }
}

# vim: expandtab shiftwidth=4
