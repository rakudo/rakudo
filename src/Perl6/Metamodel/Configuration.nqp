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

    my &sym_lookup := nqp::null();
    method set_sym_lookup_routine(&slr) {
        &sym_lookup := &slr;
    }
    method throw_or_die($exception, $die_message, *@pos, *%named) {
        if nqp::isnull(&sym_lookup) {
            nqp::die($die_message)
        }
        else {
            # When &sym_lookup is registered we do have all core exception classes declared. Therefore we can use
            # use &sym_lookup safely. If it fails to find a symbol then fully legit X::NoSuchSymbol will be thrown.
            my $ex_type := &sym_lookup(nqp::hllizefor($exception, 'Raku'));
            # HLLize all named arguments for exception constructor. Note that if an exception attribute is Bool the the
            # caller of this method is responsible for using nqp::hllboolfor to produce a valid Bool instance.
            my %hll_named;
            for %named {
                %hll_named{nqp::iterkey_s($_)} := nqp::hllizefor(nqp::iterval($_), 'Raku');
            }
            $ex_type.new(|@pos, |%hll_named).throw
        }
    }

    # A class providing some HLL core services. Normally it would be Rakudo::Internals.
    my $utility_class := nqp::null();
    method set_utility_class($type) {
        $utility_class := $type;
    }

    # Produce a unique integer ID. When utility class is available then its NEXT-ID method is used. Otherwise the ID is
    # generated using local means. In order to avoid conflicts with Rakudo::Internals.NEXT-ID, the local generator
    # produces negative values.
    my int $last_id := 0;
    my $id_lock := NQPLock.new;
    method next_id() {
        if nqp::isnull($utility_class) {
            return $id_lock.protect({ --$last_id })
        }
        $utility_class.NEXT-ID
    }

    my $language-revision-type := nqp::null();
    method set_language_revision_type($type) { $language-revision-type := $type; }
    method language_revision_type()          { $language-revision-type }
    method language_revision_object(int $revision) { nqp::box_i($revision, $language-revision-type) }

    # Register HLL symbol for code which doesn't have direct access to this class. For example, moar/Perl6/Ops.nqp
    # relies on this symbol.
    nqp::bindhllsym('Raku', 'METAMODEL_CONFIGURATION', Perl6::Metamodel::Configuration);

    # C3 merge routine.
    method c3_merge(@merge_list) {
        my @result;
        my $accepted;
        my $something_accepted := 0;
        my $cand_count := 0;

        # Try to find something appropriate to add to the MRO.
        for @merge_list {
            my @cand_list := $_;
            if nqp::elems(@cand_list) {
                my $rejected := 0;
                my $cand_class := @cand_list[0];
                $cand_count := $cand_count + 1;
                for @merge_list {
                    # Skip current list.
                    unless $_ =:= @cand_list {
                        # Is current candidate in the tail? If so, reject.
                        my $cur_pos := 1;
                        while $cur_pos <= nqp::elems($_) {
                            if nqp::decont($_[$cur_pos]) =:= nqp::decont($cand_class) {
                                $rejected := 1;
                            }
                            $cur_pos := $cur_pos + 1;
                        }
                    }

                }
                # If we didn't reject it, this candidate will do.
                unless $rejected {
                    $accepted := $cand_class;
                    $something_accepted := 1;
                    last;
                }
            }
        }

        # If we never found any candidates, return an empty list.
        if $cand_count == 0 {
            return @result;
        }

        # If we didn't find anything to accept, error.
        unless $something_accepted {
            nqp::die("Could not build C3 linearization: ambiguous hierarchy");
        }

        # Otherwise, remove what was accepted from the merge lists.
        my int $i := -1;
        while ++$i < nqp::elems(@merge_list) {
            my @new_list;
            for @merge_list[$i] {
                unless nqp::decont($_) =:= nqp::decont($accepted) {
                    @new_list.push($_);
                }
            }
            @merge_list[$i] := @new_list;
        }

        # Need to merge what remains of the list, then put what was accepted on
        # the start of the list, and we're done.
        @result := self.c3_merge(@merge_list);
        @result.unshift($accepted);
        return @result;
    }
}

# vim: expandtab sw=4
