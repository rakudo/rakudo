#- Metamodel::Configuration ----------------------------------------------------
# Keeps track of various special types or other things that the MOP may be
# configured with.  Not intended to be instantiated: only provides class
# "attribute" value setters, getters and some logic appliers.
class Perl6::Metamodel::Configuration {

    # Keepers of information
    my $stash_type                 :=
    my $stash_attr_type            :=
    my $submethod_type             :=
    my $multi_sig_comparator       :=
    my $role_to_class_applier_type :=
    my $role_to_role_applier_type  :=
    my &sym_lookup                 :=
    my $utility_class              :=
    my $language-revision-type     :=
    my $type-env-type              := nqp::null;

    method set_stash_type($type, $attr_type) {
        $stash_type := $type;
        $stash_attr_type := $attr_type;
    }
    method stash_type()      { $stash_type      }
    method stash_attr_type() { $stash_attr_type }

    method set_submethod_type($type) {
        $submethod_type := $type
    }
    method submethod_type() { $submethod_type }

    method set_multi_sig_comparator($comp) {
        $multi_sig_comparator := $comp;
    }
    method compare_multi_sigs($a, $b) {
        nqp::isconcrete($multi_sig_comparator)
          && $multi_sig_comparator($a, $b)
    }

    method set_role_to_class_applier_type($type) {
        $role_to_class_applier_type := $type
    }
    method role_to_class_applier_type() { $role_to_class_applier_type }

    method set_role_to_role_applier_type($type) {
        $role_to_role_applier_type := $type
    }
    method role_to_role_applier_type() { $role_to_role_applier_type }

    method set_sym_lookup_routine(&code) {
        &sym_lookup := &code
    }
    method throw_or_die($exception, $message, *@pos, *%_) {
        nqp::die($message) if nqp::isnull(&sym_lookup);

        # When &sym_lookup is registered we do have all core exception
        # classes declared. Therefore we can use use &sym_lookup safely.
        # If it fails to find a symbol then fully legit X::NoSuchSymbol
        # will be thrown.
        my $ex_type := &sym_lookup(nqp::hllizefor($exception, 'Raku'));

        # HLLize all named arguments for exception constructor. Note that
        # if an exception attribute is Bool the caller of this method is
        # responsible for using nqp::hllboolfor to produce a valid Bool
        # instance.
        for %_ {
            nqp::bindkey(
              %_,
              nqp::iterkey_s($_),
              nqp::hllizefor(nqp::iterval($_), 'Raku')
            );
        }
        $ex_type.new(|@pos, |%_).throw
    }

    # A class providing some HLL core services. Normally it would be
    # Rakudo::Internals.
    method set_utility_class($type) {
        $utility_class := $type;
    }

    # Produce a unique integer ID. When utility class is available then its
    # NEXT-ID method is used. Otherwise the ID is generated using local means.
    # In order to avoid conflicts with Rakudo::Internals.NEXT-ID, the local
    # generator produces negative values.
    my int $last_id;
    my $id_lock := NQPLock.new;
    method next_id() {
        nqp::isnull($utility_class)
          ?? $id_lock.protect({ --$last_id })
          !! $utility_class.NEXT-ID
    }

    method set_language_revision_type($type) {
        $language-revision-type := $type
    }
    method language_revision_type() { $language-revision-type }
    method language_revision_object(int $revision) {
        nqp::box_i($revision, $language-revision-type)
    }

    # Register HLL symbol for code which doesn't have direct access to this
    # class. For example, moar/Perl6/Ops.nqp relies on this symbol.
    nqp::bindhllsym(
      'Raku',
      'METAMODEL_CONFIGURATION',
      Perl6::Metamodel::Configuration
    );

    method set_type_env_type($type) {
        $type-env-type := $type
    }
    method type_env_type() { $type-env-type }

    method type_env_from($ctx, *%_) {
        nqp::isnull($type-env-type)
          ?? nqp::null
          !! nqp::istype($ctx, $type-env-type)
            ?? $ctx
            !! $type-env-type.new-from-ctx($ctx, |%_)
    }
}

# vim: expandtab sw=4
