role RakuAST::IMPL::Lowerable {
    has int $!lowered;
    has str $!local-name;
    has Mu  $!sentinel;

    method IMPL-SET-LOWERED-TO-LOCAL(Mu $sentinel) {
        nqp::bindattr_i(self,RakuAST::IMPL::Lowerable,'$!lowered', 1);
        nqp::bindattr(  self,RakuAST::IMPL::Lowerable,'$!sentinel',$sentinel);
    }

    method IMPL-LOWERED-AWAY-SENTINEL() { $!sentinel }

    # Any additional reasons to not lower, intended to be shadowed if applicable
    method IMPL-NO-LOWERING() { False }

    # The frame-local name for a lowered term, or the empty string when
    # the term stays a by-name lexical.
    method IMPL-LOWERED-LOCAL-NAME() {
        my str $local-name := $!local-name;
        if $local-name {
            $local-name
        }
        elsif nqp::not_i($!lowered)
          || nqp::isnull($!sentinel)
          || self.IMPL-NO-LOWERING {
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

    # Return either the QAST for a lowered variable, or nqp::null
    method IMPL-LOWERED-QAST-DECL(
      RakuAST::IMPL::QASTContext $context,
                             str $name,    # the lexical name
                                 $returns  # constraint (nqp::null for none)
    ) {
        # Haz been lowered
        if self.IMPL-LOWERED-LOCAL-NAME {
            my $sentinel := $!sentinel;
            $context.ensure-sc($sentinel);

            my %args := nqp::hash(
              "name",  $!local-name,
              "scope", "local",
              "decl",  "var",
            );
            nqp::bindkey(%args,"returns",$returns)
              unless nqp::isnull($returns);

            # The local is declared here, in the block's declaration
            # section, so it exists before anything references it. The
            # by-name lexical stays declared for introspection, static
            # with the sentinel as its value so no per-entry setup is
            # paid for it.
            QAST::Stmts.new(
              QAST::Var.new(
                :$name, :scope<lexical>, :decl<static>, :value($sentinel)
              ),
              QAST::Var.new(|%args)
            )
        }

        # Not lowered
        else {
            nqp::null
        }
    }

    # Provide the QAST for simple lexical variable declaration
    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        my str $name := self.lexical-name;
        nqp::ifnull(
          self.IMPL-LOWERED-QAST-DECL($context, $name, nqp::null),
          QAST::Var.new(:$name, :scope<lexical>, :decl<var>)
        )
    }
}
