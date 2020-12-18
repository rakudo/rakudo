# Keeps track of various special types or other things that the MOP may be
# configured with.
class Perl6::Metamodel::Configuration {
    my $stash_type := nqp::null();
    my $stash_attr_type := nqp::null();

    method set_stash_type($type, $attr_type) {
        $stash_type := $type;
        $stash_attr_type := $attr_type;
    }
    method stash_type() { $stash_type }
    method stash_attr_type() { $stash_attr_type }

    my $submethod_type := nqp::null();
    method set_submethod_type($type) {
        $submethod_type := $type;
    }
    method submethod_type() { $submethod_type }

    my $multi_sig_comparator;
    method set_multi_sig_comparator($comp) {
        $multi_sig_comparator := $comp;
    }
    method compare_multi_sigs($a, $b) {
        nqp::isconcrete($multi_sig_comparator)
            ?? $multi_sig_comparator($a, $b)
            !! 0
    }

    my $role_to_class_applier_type := nqp::null();
    method set_role_to_class_applier_type($rtca_type) {
        $role_to_class_applier_type := $rtca_type;
    }
    method role_to_class_applier_type() { $role_to_class_applier_type }

    my $role_to_role_applier_type := nqp::null();
    method set_role_to_role_applier_type($rtra_type) {
        $role_to_role_applier_type := $rtra_type;
    }
    method role_to_role_applier_type() { $role_to_role_applier_type }

    my $X_package := nqp::null();
    method set_X_package($X) {
        $X_package := $X;
    }
    method find_exception($exception) {
        my $ex_type := nqp::null();
        unless nqp::isnull($X_package) {
            my @parts := nqp::split('::', $exception);
            # If the first part of the long name is not X then pretend there is no such exception. Which is, actually
            # and most likely, is true.
            if nqp::iseq_s(nqp::shift(@parts), 'X') {
                my $who := $X_package.WHO;
                while +@parts {
                    my $name := nqp::shift(@parts);
                    if $who.EXISTS-KEY($name) {
                        if +@parts {
                            $who := $who.AT-KEY($name).WHO;
                        }
                        else {
                            $ex_type := $who.AT-KEY($name);
                        }
                    }
                    else {
                        # Signal the loop end
                        @parts := nqp::list();
                    }
                }
            }
        }
        $ex_type
    }
    method throw_or_die($exception, $die_message, *@pos, *%named) {
        my $ex_type := self.find_exception($exception);
        if nqp::isnull($ex_type) {
            nqp::die($die_message)
        }
        else {
            $ex_type.new(|@pos, |%named).throw
        }
    }
}

# vim: expandtab sw=4
