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
        $pun.HOW.compose($pun);
        $pun
    }
    
    # Produces something that can be inherited from (the pun).
    method inheritalize($obj) {
        unless $!made_pun {
            $!pun := self.make_pun($obj);
            $!made_pun := 1;
        }
        $!pun
    }
    
    # Do a pun-based dispatch. If we pun, return a thunk that will delegate.
    method find_method($obj, $name) {
        if nqp::existskey(%exceptions, $name) {
            return nqp::findmethod(%exceptions{$name}, $name);
        }
        unless $!made_pun {
            $!pun := self.make_pun($obj);
            $!made_pun := 1;
        }
        unless nqp::can($!pun, $name) {
            return nqp::null();
        }
        -> $inv, *@pos, *%named {
            $!pun."$name"(|@pos, |%named)
        }
    }
}
