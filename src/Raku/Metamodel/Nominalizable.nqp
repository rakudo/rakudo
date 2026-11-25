#- Metamodel::Nominalizable ----------------------------------------------------
role Perl6::Metamodel::Nominalizable {

    method nominalizable_kind($XXX?) {
        Perl6::Metamodel::Configuration.throw_or_die(
          'X::Nominalizable::NoKind',
          self.HOW.name(self) ~ " doesn't declare method 'nominalizable_kind'",
          :nominalizable(self)
        );
    }

    method !find_wrappee($target, %kind_of) {

        if nqp::atkey(%kind_of, self.nominalizable_kind) {
            $target
        }
        else {
            my $my_wrappee := self."!wrappee"($target);

            nqp::not_i(nqp::elems(%kind_of))
              # .^wrappee without named parameter returns our immediate wrappee
              ?? $my_wrappee
              !! $my_wrappee.HOW.archetypes.nominalizable
                # If the immediate wrappee is a nominalizable then bypass the
                # request
                ?? $my_wrappee.HOW."!find_wrappee"($my_wrappee, %kind_of)
                # Otherwise the request cannot be completed
                !! nqp::null
        }
    }

    method wrappee($target, *%_) {
        nqp::ifnull(
          self."!find_wrappee"($target, %_),
          Perl6::Metamodel::Configuration.throw_or_die(
            'X::Nominalizable::NoWrappee',
            "Can't find requested wrappee on $target: reached a nominal type "
               ~ self."!wrappee"($target).HOW.name(self),
            :nominalizable($target),
            :kinds(%_),
          )
        )
    }

    method wrappee-lookup($target, *%_) {
        self."!find_wrappee"($target, %_)
    }

    method coerce($target, $value) {
        # In general, this method should be invoked via
        # $type.^wrappee(:coercion). But this would complicate QAST generated
        # by parameter binding implementation in Actions. So, let it be here.
        # Hopefully, it'd be possible to remove it either with RakuAST or by
        # implementing corresponding helper nqp:: Raku op for coercive
        # parameter binding.
        my $coercion_type := self.wrappee($target, :coercion);
        $coercion_type.HOW.coerce($coercion_type, $value)
    }
}
