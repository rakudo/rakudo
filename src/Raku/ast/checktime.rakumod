# Done by every AST node that can report CHECK-time problems.
role RakuAST::CheckTime {
    # A list of sorries, lazily allocated if there are any.
    has Mu $!sorries;

    # A list of worries, lazily allocated if there are any.
    has Mu $!worries;

    # Returns True if any check-time problems (sorries or worries) have been
    # identified, and False otherwise.
    method has-check-time-problems() {
        $!sorries || $!worries ?? True !! False
    }

    # Get a list of any sorry-level check-time problems.
    method sorries() {
        self.IMPL-WRAP-LIST($!sorries // [])
    }

    # Get a list of any worry-level check-time problems.
    method worries() {
        self.IMPL-WRAP-LIST($!worries // [])
    }

    # Add a sorry check-time problem (which will produce a SORRY output in the
    # compiler).
    method add-sorry(Any $exception) {
        self.IMPL-LOCATE-EXCEPTION($exception);
        nqp::push(
          $!sorries // nqp::bindattr(self,RakuAST::CheckTime,'$!sorries',[]),
          $exception
        );
        Nil
    }

    # Add a worry check-time problem (which will produce a potential
    # difficulties output in the compiler).
    method add-worry(Any $exception) {
        self.IMPL-LOCATE-EXCEPTION($exception);
        nqp::push(
          $!worries // nqp::bindattr(self,RakuAST::CheckTime,'$!worries',[]),
          $exception
        );
        Nil
    }

    # Called when fatal is active in our lexical scope.
    method promote-worries-to-sorries() {
        if nqp::isconcrete($!worries) {
            nqp::bindattr(self, RakuAST::CheckTime, '$!sorries', []) unless nqp::isconcrete($!sorries);
            for $!worries {
                nqp::push($!sorries, $_);
            }
            nqp::bindattr(self, RakuAST::CheckTime, '$!worries', []);
        }
    }

    # Called when no worries is active in our lexical scope.
    method clear-worries() {
        nqp::bindattr(self, RakuAST::CheckTime, '$!worries', []);
    }

    # Adds a sorry when the code, parenthesized or not, is a block that is a
    # double closure. With $tested, the block's value is only tested.
    method IMPL-CHECK-FOR-DOUBLE-CLOSURE(
                              Mu $code,
               RakuAST::Resolver $resolver,
      RakuAST::IMPL::QASTContext $context,
                           Bool :$tested
    ) {
        my $block := self.IMPL-UNWRAP-PARENS($code);
        if nqp::istype($block, RakuAST::Block) {
            my $sorry := $block.IMPL-CHECK-DOUBLE-CLOSURE($resolver, $context, :$tested);
            self.add-sorry: $sorry if $sorry;
        }
    }

    # Method to be implemented by nodes that perform CHECK-time checks. Should
    # call add-sorry and add-worry with the constructed exception objects.
    method PERFORM-CHECK(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) { ... }
}
