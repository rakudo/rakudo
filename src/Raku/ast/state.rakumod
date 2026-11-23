# A state initializer runs on its first reach in each clone of the frame
# declaring the variable, as once does. A sentinel state variable guards
# it, produced as an implicit declaration for the declaring scope to hold.
role RakuAST::StateInitGuard {
    has RakuAST::VarDeclaration::Implicit::State $!state-init-guard;

    # Whether the declaration has a state initializer to guard. A binding
    # one keeps nqp::p6stateinit, as the binder writes into its own frame.
    method IMPL-GUARDS-STATE-INIT() {
        self.scope eq 'state'
          && nqp::isconcrete(self.initializer)
          && !self.initializer.is-binding
    }

    method IMPL-STATE-INIT-GUARD() {
        $!state-init-guard  # XXX is this attribute needed???
          // nqp::bindattr(self,RakuAST::StateInitGuard,'$!state-init-guard',
               RakuAST::VarDeclaration::Implicit::State.new(
                 QAST::Node.unique('!state_init_guard'),
                 :sentinel
               )
             )
    }

    # Whether the initializer is to run, marking the guard on the first reach.
    # A sub in a role body forms its code before its scope gathers the
    # implicit declarations, so the guard is produced here as well.
    method IMPL-STATE-INIT-CONDITION-QAST(RakuAST::IMPL::QASTContext $context) {

        # Assume assigning
        if self.IMPL-GUARDS-STATE-INIT {
            self.get-implicit-declarations;
            my $guard := self.IMPL-STATE-INIT-GUARD;
            QAST::Op.new(:op<if>,
              $guard.IMPL-SENTINEL-TEST-QAST($context),
              QAST::Stmts.new(
                QAST::Op.new(:op<p6store>,
                  QAST::Var.new(:name($guard.name), :scope<lexical>),
                  QAST::WVal.new(:value(True))
                ),
                QAST::IVal.new(:value(1))
              ),
              QAST::IVal.new(:value(0))
            )
        }

        # Assume binding
        else {
            QAST::Op.new(:op<p6stateinit>)
        }
    }
}
