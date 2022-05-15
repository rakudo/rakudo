role Perl6::Metamodel::RolePunning {
    # Meta-object we use to make a pun.
    my $pun_meta;

    # Exceptions to the punning. Hash of name to actual object to call on.
    my %exceptions;

    # The pun for the current meta-object.
    has $!pun;

    # Did we make a pun?
    has $!made_pun;

    # Representation to pun to, if any.
    has str $!pun_repr;

    # Configures the punning.
    method configure_punning($my_pun_meta, %my_exceptions) {
        $pun_meta := $my_pun_meta;
        %exceptions := %my_exceptions;
    }

    method set_pun_repr($obj, $repr) {
        $!pun_repr := $repr
    }

    method pun_repr($obj) {
        $!pun_repr
    }

    # Produces the pun.
    method make_pun($obj) {
        my $pun := $!pun_repr
            ?? $pun_meta.new_type(:name(self.name($obj)), :repr($!pun_repr))
            !! $pun_meta.new_type(:name(self.name($obj)));
        $pun.HOW.add_role($pun, $obj);
        $pun.HOW.set_pun_source($pun, $obj);
        $pun.HOW.compose($pun);
        my $why := self.WHY;
        if $why {
            $pun.set_why(self.WHY);
        }
        $pun
    }

    # Returns the pun (only creating it if it wasn't already created)
    method pun($obj) {
        unless $!made_pun {
            $!pun := self.make_pun($obj);
            $!made_pun := 1;
        }
        $!pun
    }

    # Produces something that can be inherited from (the pun).
    method inheritalize($obj) {
        self.pun($obj)
    }

    my $proxy_type := nqp::null();
    method !make_proxy($cont) {
        if nqp::isnull($proxy_type) {
            $proxy_type := nqp::gethllsym('Raku', 'Proxy');
        }

        my $proxy   := nqp::create($proxy_type);
        my $updated := 0;
        nqp::bindattr($proxy, $proxy_type, '&!FETCH', -> $var {
            $updated ?? nqp::decont($cont) !! $!pun;
        });
        nqp::bindattr($proxy, $proxy_type, '&!STORE', -> $var, $val {
            nqp::assign($cont, $val);
            $updated := 1;
        });
        $proxy;
    }

    # Do a pun-based dispatch. If we pun, return a thunk that will delegate.
    method find_method($obj, $name, *%c) {
        if nqp::existskey(%exceptions, $name) {
            return nqp::findmethod(%exceptions{$name}, $name);
        }
        unless $!made_pun {
            $!pun := self.make_pun($obj);
            $!made_pun := 1;
        }

        my $meth := $!pun.HOW.find_method($!pun, $name, |%c);
        unless nqp::isconcrete($meth) {
            return nqp::null();
        }
        -> $inv, *@pos, *%named {
            # If called on a container, delegate to a Proxy that fetches the pun
            # until something is stored in the container
            my $proxy := nqp::iscont($inv) ?? self.'!make_proxy'($inv) !! $!pun;
            $meth($proxy, |@pos, |%named);
        }
    }

    method is_method_call_punned($obj, $name) {
        !nqp::existskey(%exceptions, $name)
    }
}

# vim: expandtab sw=4
