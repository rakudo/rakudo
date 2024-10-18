#- Metamodel::RolePunning ------------------------------------------------------
# Contains all the logic to pun a role into a class
role Perl6::Metamodel::RolePunning {
    # Meta-object we use to make a pun.
    my $punHOW;

    # Exceptions to the punning. Hash of name to actual object to call on.
    my %exceptions;

    # The pun for the current meta-object.
    has $!pun;

    # Representation to pun to, if any.
    has str $!repr;

    method TWEAK(*%_) {
        $!pun  := nqp::null;
        $!repr := "P6opaque";
    }

    # Configures the punning.
    method configure_punning($my_punHOW, %my_exceptions) {
        $punHOW   := $my_punHOW;
        %exceptions := %my_exceptions;
    }

    method set_pun_repr($XXX, str $repr) {
        $!repr := $repr
    }
    method pun_repr($XXX?) { $!repr }

    # Produces the pun.
    method make_pun($target) {
        my $pun := $punHOW.new_type(:name(self.name($target)), :repr($!repr));
        $pun.HOW.add_role($pun, $target);
        $pun.HOW.set_pun_source($pun, $target);
        $pun.HOW.compose($pun);

        my $WHY := self.WHY;
        $pun.set_why($WHY) if $WHY;

        $pun
    }

    # Returns the pun (only creating it if it wasn't already created)
    method pun($target) {
#?if jvm
        ## Bandaid for missing nqp::null on first call.
        $!pun := nqp::null if $!pun =:= NQPMu;
#?endif
        nqp::ifnull(
          $!pun,
          self.protect({
              nqp::ifnull(  # check again in case of a race
                $!pun,
                $!pun := self.make_pun($target)
              )
          })
        )
    }

    # Produces something that can be inherited from (the pun).
    method inheritalize($target) {
        self.pun($target)
    }

    # Do a pun-based dispatch. If we pun, return a thunk that will delegate.
    method find_method($target, $name, *%c) {
        if nqp::existskey(%exceptions, $name) {
            nqp::findmethod(nqp::atkey(%exceptions, $name), $name)
        }
        else {
            my $pun := self.pun($target);
            nqp::can($pun, $name)
              ?? -> $inv, *@_, *%_ { $pun."$name"(|@_, |%_) }
              !! nqp::null
        }
    }

    method is_method_call_punned($XXX, $name) {
        nqp::not_i(nqp::existskey(%exceptions, $name))
    }
}

# vim: expandtab sw=4
