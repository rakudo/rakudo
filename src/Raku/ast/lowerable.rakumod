role RakuAST::IMPL::Lowerable {
    has int $!lowered;
    has str $!local-name;
    has Mu  $!sentinel;

    method IMPL-SET-LOWERED-TO-LOCAL(Mu $sentinel) {
        nqp::bindattr_i(self,RakuAST::IMPL::Lowerable,'$!lowered', 1);
        nqp::bindattr(  self,RakuAST::IMPL::Lowerable,'$!sentinel',$sentinel);
    }

    # The frame-local name for a lowered term, or the empty string when
    # the term stays a by-name lexical.
    method IMPL-LOWERED-LOCAL-NAME() {
        my $local-name := $!local-name;
        if $local-name {
            $local-name
        }
        elsif nqp::not_i($!lowered) || nqp::isnull($!sentinel) {
            ''
        }
        else {
            nqp::bindattr_s(self,RakuAST::IMPL::Lowerable,'$!local-name',
              $local-name := self.IMPL-GENERATE-LOWERED-NAME
            );

            self.note(
              "lex2local: minted $local-name  for "
                ~ self.lexical-name
            ) if nqp::getenvhash<RAKUDO_LOWERING_DEBUG>;

            $local-name
        }
    }

    # Provide the QAST for variable declaration
    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        my str $name := self.lexical-name;

        # Haz been lowered
        if self.IMPL-LOWERED-LOCAL-NAME {

            # The by-name lexical stays declared for introspection
            my $value := $!sentinel;
            $context.ensure-sc($value);
            QAST::Stmts.new(
              QAST::Var.new(:$name, :decl<static>, :scope<lexical>, :$value),
              QAST::Var.new(
                :name($!local-name), :decl<var>, :scope<local>,
              )
            )
        }

        # Stays lexical
        else {
            QAST::Var.new(:$name, :decl<var>, :scope<lexical>)
        }
    }
}
