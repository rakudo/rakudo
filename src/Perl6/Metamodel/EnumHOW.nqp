# This is the meta-object for an enumeration (declared with enum).
# It keeps hold of the enumeration values in an Map, which is
# created at composition time. It supports having roles composed in,
# one or two of which presumably provide the core enum-ish methods.
class Perl6::Metamodel::EnumHOW
    does Perl6::Metamodel::Naming
    does Perl6::Metamodel::BUILDALL
    does Perl6::Metamodel::Documenting
    does Perl6::Metamodel::Composing
    does Perl6::Metamodel::LanguageRevision
    does Perl6::Metamodel::Stashing
    does Perl6::Metamodel::AttributeContainer
    does Perl6::Metamodel::MethodContainer
    does Perl6::Metamodel::PrivateMethodContainer
    does Perl6::Metamodel::MultiMethodContainer
    does Perl6::Metamodel::RoleContainer
    does Perl6::Metamodel::BaseType
    does Perl6::Metamodel::MROBasedMethodDispatch
    does Perl6::Metamodel::MROBasedTypeChecking
    does Perl6::Metamodel::BUILDPLAN
    does Perl6::Metamodel::BoolificationProtocol
    does Perl6::Metamodel::REPRComposeProtocol
    does Perl6::Metamodel::Mixins
#?if !moar
    does Perl6::Metamodel::InvocationProtocol
#?endif
{
    # Hash representing enumeration keys to values.
    has %!values;

    # Reverse mapping hash.
    has $!value_to_enum;

    # List of enum values (actual enum objects).
    has @!enum_value_list;

    # Roles that we do.
    has @!role_typecheck_list;

    # Role'd version of the enum.
    has $!role;
    has int $!roled;

    # Exportation callback for enum symbols, if any.
    has $!export_callback;

    my $archetypes := Perl6::Metamodel::Archetypes.new( :nominal(1), :composalizable(1),
                                                        :augmentable(1) );
    method archetypes($XXX?) { $archetypes }

    method new_type(:$name!, :$base_type?, :$repr = 'P6opaque', :$is_mixin) {
        my $meta := self.new();
        my $obj  := nqp::settypehll(nqp::newmixintype($meta, $repr), 'Raku');
        $meta.set_name($obj, $name);
        $meta.set_base_type($meta, $base_type) unless $base_type =:= NQPMu;
        $meta.setup_mixin_cache($obj);
        self.add_stash($obj);
    }

    # We only have add_parent to support mixins, which expect this method.
    method add_parent($target, $parent) {
        self.set_base_type($target, $parent);
    }

    method add_enum_value($XXX, $value) {
        %!values{nqp::unbox_s($value.key)} := $value.value;
        nqp::push(@!enum_value_list, $value);
        nqp::scwbdisable();
        $!value_to_enum := NQPMu;
        nqp::scwbenable();
    }

    method set_export_callback($XXX, $callback) {
        $!export_callback := $callback
    }

    method enum_values($XXX?) {            %!values  }
    method elems(      $XXX?) { nqp::elems(%!values) }

    method enum_from_value($XXX, $value) {
        my $value_to_enum := $!value_to_enum;
        unless $value_to_enum {
            $value_to_enum := nqp::hash;
            for @!enum_value_list {
                $value_to_enum{$_.value} := $_;
            }
            nqp::scwbdisable();
            $!value_to_enum := $value_to_enum;
            nqp::scwbenable();
        }
        nqp::existskey($value_to_enum, $value)
            ?? $value_to_enum{$value}
            !! nqp::null()
    }

    method enum_value_list($XXX?) {
        @!enum_value_list
    }

    method compose($target, :$compiler_services) {
         $target := nqp::decont($target);

        self.set_language_version($target);

        # Instantiate all of the roles we have (need to do this since
        # all roles are generic on ::?CLASS) and pass them to the
        # composer.
        my $rtca;
        unless nqp::isnull(my $r := self.pop_role_to_compose) {
            my @ins_roles;
            until nqp::isnull($r) {
                nqp::push(@!role_typecheck_list, $r);
                my $ins := $r.HOW.specialize($r, $target);
                self.check-type-compat($target, $ins, [3])
                    if nqp::istype($ins.HOW, Perl6::Metamodel::LanguageRevision);
                @ins_roles.push($ins);

                $r := self.pop_role_to_compose;
            }
            $rtca := Perl6::Metamodel::Configuration.role_to_class_applier_type.new;
            $rtca.prepare($target, @ins_roles);

            # Add them to the typecheck list, and pull in their
            # own type check lists also.
            for @ins_roles {
                nqp::push(@!role_typecheck_list, $_);
                for $_.HOW.role_typecheck_list($_) {
                    nqp::push(@!role_typecheck_list, $_);
                }
            }
        }

        # Compose own attributes first.
        for self.attributes($target, :local) {
            $_.compose($target);
        }

        if $rtca {
            $rtca.apply();
        }

        # Incorporate any new multi candidates (needs MRO built).
        self.incorporate_multi_candidates($target);

        # Compose remaining attributes.
        for self.attributes($target, :local) {
            $_.compose($target);
        }

        # Publish type and method caches.
        self.publish_type_cache($target);
        self.publish_method_cache($target);

        # Publish boolification spec.
        self.publish_boolification_spec($target);

        # Create BUILDPLAN.
        self.create_BUILDPLAN($target);

        # Compose the representation.
        unless self.is_composed {
            self.compose_repr($target);
            self.set_composed;
        }

#?if !moar
        # Compose invocation protocol.
        self.compose_invocation($target);
#?endif

        $target
    }

    # Called by the compiler when all enum values have been added, to trigger
    # any needed actions.
    method compose_values($XXX?) {
        if $!export_callback {
            $!export_callback();
            $!export_callback := Mu;
        }
    }

    my $composalizer;
    method set_composalizer($c) { $composalizer := $c }
    method composalize($target) {
        unless $!roled {
            $!role := $composalizer($target, self.name($target), @!enum_value_list);
            $!roled := 1;
        }
        $!role
    }

    method role_typecheck_list($XXX?) { @!role_typecheck_list }
}

# vim: expandtab sw=4
