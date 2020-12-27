# This is the meta-object for an enumeration (declared with enum).
# It keeps hold of the enumeration values in an Map, which is
# created at composition time. It supports having roles composed in,
# one or two of which presumably provide the core enum-ish methods.
class Perl6::Metamodel::EnumHOW
    does Perl6::Metamodel::Naming
    does Perl6::Metamodel::Documenting
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
    does Perl6::Metamodel::InvocationProtocol
    does Perl6::Metamodel::Mixins
{
    # Hash representing enumeration keys to values.
    has %!values;

    # Reverse mapping hash.
    has %!value_to_enum;

    # List of enum values (actual enum objects).
    has @!enum_value_list;

    # Roles that we do.
    has @!role_typecheck_list;

    # Role'd version of the enum.
    has $!role;
    has int $!roled;

    # Are we composed yet?
    has $!composed;

    # Exportation callback for enum symbols, if any.
    has $!export_callback;

    my $archetypes := Perl6::Metamodel::Archetypes.new( :nominal(1), :composalizable(1),
                                                        :augmentable(1) );
    method archetypes() {
        $archetypes
    }

    method new(*%named) {
        nqp::findmethod(NQPMu, 'BUILDALL')(nqp::create(self), |%named)
    }

    method new_type(:$name!, :$base_type?, :$repr = 'P6opaque', :$is_mixin) {
        my $meta := self.new();
        my $obj  := nqp::settypehll(nqp::newmixintype($meta, $repr), 'Raku');
        $meta.set_name($obj, $name);
        $meta.set_base_type($meta, $base_type) unless $base_type =:= NQPMu;
        $meta.setup_mixin_cache($obj);
        self.add_stash($obj);
    }

    # We only have add_parent to support mixins, which expect this method.
    method add_parent($obj, $parent) {
        self.set_base_type($obj, $parent);
    }

    method add_enum_value($obj, $value) {
        %!values{nqp::unbox_s($value.key)} := $value.value;
        @!enum_value_list[+@!enum_value_list] := $value;
    }

    method set_export_callback($obj, $callback) {
        $!export_callback := $callback
    }

    method enum_values($obj) {
        %!values
    }

    method elems($obj) {
        nqp::elems(%!values)
    }

    method enum_from_value($obj, $value) {
        unless %!value_to_enum {
            for @!enum_value_list {
                %!value_to_enum{$_.value} := $_;
            }
        }
        nqp::existskey(%!value_to_enum, $value)
            ?? %!value_to_enum{$value}
            !! nqp::null()
    }

    method enum_value_list($obj) {
        @!enum_value_list
    }

    method compose($the-obj, :$compiler_services) {
        my $obj := nqp::decont($the-obj);

        self.set_language_version($obj);

        # Instantiate all of the roles we have (need to do this since
        # all roles are generic on ::?CLASS) and pass them to the
        # composer.
        my @roles_to_compose := self.roles_to_compose($obj);
        my $rtca;
        if @roles_to_compose {
            my @ins_roles;
            while @roles_to_compose {
                my $r := @roles_to_compose.pop();
                @!role_typecheck_list[+@!role_typecheck_list] := $r;
                my $ins := $r.HOW.specialize($r, $obj);
                self.check-type-compat($obj, $ins, ['e'])
                    if nqp::istype($ins.HOW, Perl6::Metamodel::LanguageRevision);
                @ins_roles.push($ins);
            }
            $rtca := Perl6::Metamodel::Configuration.role_to_class_applier_type.new;
            $rtca.prepare($obj, @ins_roles);

            # Add them to the typecheck list, and pull in their
            # own type check lists also.
            for @ins_roles {
                @!role_typecheck_list[+@!role_typecheck_list] := $_;
                for $_.HOW.role_typecheck_list($_) {
                    @!role_typecheck_list[+@!role_typecheck_list] := $_;
                }
            }
        }

        # Compose own attributes first.
        for self.attributes($obj, :local) {
            $_.compose($obj);
        }

        if $rtca {
            $rtca.apply();
        }

        # Incorporate any new multi candidates (needs MRO built).
        self.incorporate_multi_candidates($obj);

        # Compose remaining attributes.
        for self.attributes($obj, :local) {
            $_.compose($obj);
        }

        # Publish type and method caches.
        self.publish_type_cache($obj);
        self.publish_method_cache($obj);

        # Publish boolification spec.
        self.publish_boolification_spec($obj);

        # Create BUILDPLAN.
        self.create_BUILDPLAN($obj);

        # Compose the representation.
        unless $!composed {
            self.compose_repr($obj);
            $!composed := 1;
        }

        # Compose invocation protocol.
        self.compose_invocation($obj);

        $obj
    }

    # Called by the compiler when all enum values have been added, to trigger
    # any needed actions.
    method compose_values($obj) {
        if $!export_callback {
            $!export_callback();
            $!export_callback := Mu;
        }
    }

    my $composalizer;
    method set_composalizer($c) { $composalizer := $c }
    method composalize($obj) {
        unless $!roled {
            $!role := $composalizer($obj, self.name($obj), @!enum_value_list);
            $!roled := 1;
        }
        $!role
    }

    method is_composed($obj) {
        $!composed
    }

    method role_typecheck_list($obj) {
        @!role_typecheck_list
    }
}

# vim: expandtab sw=4
