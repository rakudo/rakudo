#- Metamodel::EnumHOW ----------------------------------------------------------
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
    has %!seulav;

    # List of enum values (actual enum objects).
    has @!enum_value_list;

    # Roles that we do.
    has @!role_typecheck_list;

    # Role'd version of the enum, null if not initialized yet
    has $!role;

    # Exportation callback for enum symbols, if any.  Null if not
    has $!export_callback;

    # Make sure we mark as uninitialized
    method TWEAK(*%_) {
        $!role := $!export_callback := nqp::null;
    }

    my $archetypes := Perl6::Metamodel::Archetypes.new(
      :nominal, :composalizable, :augmentable
    );

    # Simple accessors
    method archetypes(         $XXX?) { $archetypes           }
    method enum_values(        $XXX?) { %!values              }
    method enum_value_list(    $XXX?) { @!enum_value_list     }
    method role_typecheck_list($XXX?) { @!role_typecheck_list }

    method new_type(
      :$name!,
      :$base_type?,
      :$repr = 'P6opaque',
      :$is_mixin
    ) {
        my $HOW  := self.new;
        my $type := nqp::newmixintype($HOW, $repr);
        nqp::settypehll($type, 'Raku');

        $HOW.set_name($type, $name);
        $HOW.set_base_type($HOW, $base_type)
          unless nqp::eqaddr($base_type, NQPMu);
        $HOW.setup_mixin_cache($type);
        self.add_stash($type);
    }

    # We only have add_parent to support mixins, which expects this method.
    method add_parent($target, $parent) {
        self.set_base_type($target, $parent);
    }

    method add_enum_value($XXX, $pair) {
        self.protect({
            my %values          := nqp::clone(%!values);
            my %seulav          := nqp::clone(%!seulav);
            my @enum_value_list := nqp::clone(@!enum_value_list);

            nqp::bindkey(%values, nqp::decont_s($pair.key), $pair.value);
            nqp::bindkey(%seulav, nqp::stringify($pair.value), $pair);
            nqp::push(@enum_value_list, $pair);

            %!values          := %values;
            %!seulav          := %seulav;
            @!enum_value_list := @enum_value_list;
        });
    }

    method set_export_callback($XXX, $callback) {
        $!export_callback := $callback
    }

    method elems($XXX?) { nqp::elems(%!values) }

    method enum_from_value($XXX, $value) {
        nqp::atkey(%!seulav, nqp::stringify($value))
    }

    method compose($target, :$compiler_services) {
         self.run_if_not_composed({
            $target := nqp::decont($target);
            self.set_language_version($target);

            # Instantiate all of the roles we have (need to do this since
            # all roles are generic on ::?CLASS) and pass them to the
            # composer.
            my $applier;
            unless nqp::isnull(my $role := self.pop_role_to_compose) {
                my @ins_roles;
                my @role_typecheck_list := nqp::clone(@!role_typecheck_list);

                # Do all roles
                until nqp::isnull($role) {
                    nqp::push(@role_typecheck_list, $role);

                    my $ins := $role.HOW.specialize($role, $target);
                    self.check-type-compat($target, $ins, nqp::list(3))
                      if nqp::istype(
                           $ins.HOW,
                           Perl6::Metamodel::LanguageRevision
                         );
                    @ins_roles.push($ins);

                    # Get next one, if any
                    $role := self.pop_role_to_compose;
                }
                $applier :=
                  Perl6::Metamodel::Configuration.role_to_class_applier_type.new;
                $applier.prepare($target, @ins_roles);

                # Add them to the typecheck list, and pull in their
                # own type check lists also.
                my int $m := nqp::elems(@ins_roles);
                my int $i;
                while $i < $m {
                    my $ins_role := nqp::atpos(@ins_roles, $i);
                    nqp::push(@role_typecheck_list, $ins_role);
                    nqp::splice(
                      @role_typecheck_list,
                      $ins_role.HOW.role_typecheck_list($ins_role),
                      nqp::elems(@role_typecheck_list),
                      0
                    );
                    ++$i;
                }

                # Atomically update
                @!role_typecheck_list := @role_typecheck_list;
            }

            # Compose own attributes first.
            my @attributes := self.attributes($target, :local);
            my int $m := nqp::elems(@attributes);
            my int $i;
            while $i < $m {
                nqp::atpos(@attributes, $i).compose($target);
                ++$i;
            }

            $applier.apply if $applier;

            # Incorporate any new multi candidates (needs MRO built).
            self.incorporate_multi_candidates($target);

            # Compose remaining attributes.
            @attributes := self.attributes($target, :local);
            $m := nqp::elems(@attributes);
            # continue where we left off
            while $i < $m {
                nqp::atpos(@attributes, $i).compose($target);
                ++$i;
            }

            # Publish type, method caches and boolification spec.
            self.publish_type_cache($target);
#?if !moar
            self.publish_method_cache($target);
#?endif
            self.publish_boolification_spec($target);

            # Create BUILDPLAN.
            self.create_BUILDPLAN($target);

            # Compose the representation.
            self.compose_repr($target);

#?if !moar
            # Compose invocation protocol.
            self.compose_invocation($target);
#?endif
        });

        $target
    }

    # Called by the compiler when all enum values have been added, to trigger
    # any needed actions.
    method compose_values($XXX?) {
        unless nqp::isnull($!export_callback) {
            $!export_callback();
            $!export_callback := nqp::null;
        }
    }

    my $composalizer;
    method set_composalizer($c) { $composalizer := $c }
    method composalize($target) {
        nqp::ifnull(
          $!role,
          $!role :=
            $composalizer($target, self.name($target), @!enum_value_list)
        )
    }
}

# vim: expandtab sw=4
