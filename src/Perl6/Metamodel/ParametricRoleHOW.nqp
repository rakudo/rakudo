#- Metamodel::ParametricRoleHOW ------------------------------------------------
# All the logic to handle a single parametric role
class Perl6::Metamodel::ParametricRoleHOW
    does Perl6::Metamodel::Naming
    does Perl6::Metamodel::BUILDALL
    does Perl6::Metamodel::Documenting
    does Perl6::Metamodel::Composing
    does Perl6::Metamodel::LanguageRevision
    does Perl6::Metamodel::MethodContainer
    does Perl6::Metamodel::PrivateMethodContainer
    does Perl6::Metamodel::MultiMethodContainer
    does Perl6::Metamodel::AttributeContainer
    does Perl6::Metamodel::RoleContainer
    does Perl6::Metamodel::MultipleInheritance
    does Perl6::Metamodel::Stashing
    does Perl6::Metamodel::TypePretense
    does Perl6::Metamodel::RolePunning
    does Perl6::Metamodel::ArrayType
#?if !moar
    does Perl6::Metamodel::InvocationProtocol
#?endif
{
    has     $!body_block;
    has int $!in_group;
    has     $!group;
    has     $!signatured;
    has     @!role_typecheck_list;

    my $archetypes := Perl6::Metamodel::Archetypes.new(
      :nominal, :composable, :inheritalizable, :parametric
    );
    method archetypes($XXX?) { $archetypes }

    # XXX consider creating a custom .new and removing does BUILDALL

    method new_type(:$repr, :$signatured, *%_) {
        my $HOW    := self.new(:$signatured);
        my $target := nqp::settypehll(nqp::newtype($HOW, 'Uninstantiable'), 'Raku');

        $HOW.set_identity($target, %_);

        $HOW.set_group($target, nqp::atkey(%_, 'group'))
          if nqp::existskey(%_, 'group');

        $HOW.add_stash($target);
    }

    method parameterize($target, *@_, *%_) {
        Perl6::Metamodel::CurriedRoleHOW.new_type($target, |@_, |%_)
    }

    method set_body_block($XXX, $block) {
        $!body_block := $block
    }

    method body_block($XXX?) { $!body_block }
    method signatured($XXX?) { $!signatured }

    method set_group($XXX, $group) {
        $!group    := $group;
        $!in_group := 1;
    }

    method group($target) {
        $!in_group ?? $!group !! $target
    }

    method compose($target, :$compiler_services) {
        $target := nqp::decont($target);

        self.set_language_version($target);

        my @rtl;
        @rtl.push($!group) if $!in_group;

        my @roles := self.roles_to_compose;
        my int $m := nqp::elems(@roles);
        my int $i;
        while $i < $m {
            my $role := nqp::atpos(@roles, $i);
            my $HOW  := $role.HOW;
            if $HOW.archetypes.composablish {
                @rtl.push($role);
                nqp::splice(
                  @rtl,
                  $HOW.role_typecheck_list($role),
                  nqp::elems(@rtl),
                  0
                );
            }
            ++$i;
        }

        @!role_typecheck_list := @rtl;
#?if !moar
        self.compose_invocation($target);
#?endif
        self.set_composed;
        $target
    }

    method roles($XXX?, :$transitive = 1, :$mro) {
        self.roles-ordered(self.roles_to_compose, :$transitive, :$mro);
    }

    method role_typecheck_list($XXX?) { @!role_typecheck_list }

    # Helper method to return 1 if the type of any of the parents matches
    # the type of the checkee, else 0
    method type_check_parents($target, $checkee) {
        self.list_istype_checkee(
          self.parents($target, :local), nqp::decont($checkee)
        )
    }

    # Helper method to return 1 if the invocant's type is matching the type
    # of the checkee
    method type_check($target, $checkee) {
        $checkee := nqp::decont($checkee);

        nqp::eqaddr($checkee, $target.WHAT)
          # checking whether the $!group is of the type $checkee allows for inline definitions of transitive roles
          || ($!in_group && nqp::istype($!group, $checkee))  # this will do an nqp::eqaddr($!group, $checkee) check
          || self.checkee_eqaddr_list($checkee, self.pretending_to_be)
          || self.checkee_istype_list($checkee, self.roles_to_compose)
          || self.type_check_parents($target, $checkee)
    }

    method specialize($target, *@_, *%_) {
        my $class := nqp::atpos(@_, 0);
        my $conc  := nqp::can($class.HOW, 'get_cached_conc')
          ?? $class.HOW.get_cached_conc($class, $target, @_, %_)
          !! nqp::null;

        if nqp::isnull($conc) {

            # We only allow one specialization of a role to take place at
            # a time, since the body block captures the methods into its
            # lexical scope, but we don't do the appropriate cloning until
            # a bit later.  These must happen before another specialize
            # happens and re-captures the things we are composing.
            self.protect({

                # Pre-create a concrete role. We'll finalize it later, in
                # specialize_with method. But for now we need it to initialize
                # $?CONCRETIZATION by role's body block.
                my $*MOP-ROLE-CONCRETIZATION :=
                                       $conc :=
                  Perl6::Metamodel::ConcreteRoleHOW.new_type(
                    :roles(nqp::list($target)),
                    :name(self.name($target))
                  );
                $conc.HOW.set_language_revision($conc, self.language_revision);
                $conc.HOW.set_hidden($conc) if self.hidden($target);

                # Run the body block to get the type environment (we know
                # the role in this case).
                my $type_env;
                my $error;
                try {
                    my $result := $!body_block(|@_, |%_);
                    if nqp::isconcrete($result) {
                        # Support for bodies returning a Raku Positional
                        my $original_result := $result;
                        $result := $result.FLATTENABLE_LIST
                          if nqp::can($result, 'FLATTENABLE_LIST');

                        $type_env :=
                          nqp::islist($result) && nqp::elems($result) == 2
                            ?? nqp::ifnull(
                                 Perl6::Metamodel::Configuration.type_env_from(
                                   nqp::atpos($result, 1)
                                 ),
                                 nqp::atpos($result, 1)
                               )
                            !! self.wrong_body_result($target, $original_result);
                    }
                    else {
                        # When there is no concrete return value from the body
                        # use empty TypeEnv then.  Assuming that no
                        # Raku-generated role body would return an undefined
                        # value, especially those that belong to the core; and
                        # assuming that the only period of time when TypeEnv
                        # is not available on the configuration class is the
                        # early stages of the CORE.c compilation, – we can
                        # safely skip the check for nullness. Can't we?
                        $type_env :=
                          Perl6::Metamodel::Configuration.type_env_type.new;
                    }
                    CATCH {
                        $error := $!;
                    }
                }

                self.could_not_instantiate($target, $error) if $error;

                # Use it to build a concrete role.
                $conc := self.specialize_with($target, $conc, $type_env, @_);

                # Cache it if possible
                $class.HOW.add_conc_to_cache($class, $target, @_, %_, $conc)
                  if nqp::can($class.HOW, 'add_conc_to_cache');
            });
        }

        $conc
    }

    method specialize_with($target, $concretization, $type_env, @_) {
        my $HOW := $concretization.HOW;

        # Go through attributes, reifying as needed and adding to
        # the concrete role.
        my @attributes := self.attributes($target, :local);
        my int $m := nqp::elems(@attributes);
        my int $i;
        while $i < $m {
            my $attribute := nqp::atpos(@attributes, $i);
            $HOW.add_attribute($concretization, $attribute.is_generic
              ?? $attribute.instantiate_generic($type_env)
              !! nqp::clone($attribute)
            );
            ++$i;
        }

        # Go through methods and instantiate them; we always do this
        # unconditionally, since we need the clone anyway.
        my @method_names := self.method_names($target);
        if $m := nqp::elems(@method_names) {
            my @methods := self.method_order($target);

            $i := 0;
            while $i < $m {
                $HOW.add_method(
                  $concretization,
                  nqp::atpos(@method_names, $i),
                  nqp::atpos(@methods, $i).instantiate_generic($type_env)
                );
                ++$i;
            }
        }

        my @private_method_names := self.private_method_names($target);
        if $m := nqp::elems(@private_method_names) {
            my %private_methods := self.private_method_table($target);

            $i := 0;
            while $i < $m {
                my str $name := nqp::atpos(@private_method_names, $i);
                $HOW.add_private_method(
                  $concretization,
                  $name,
                  nqp::atkey(
                    %private_methods, $name
                  ).instantiate_generic($type_env)
                );
                ++$i;
            }
        }

        my @multi_methods := self.multi_methods_to_incorporate($target);
        if $m := nqp::elems(@multi_methods) {
            $i := 0;
            while $i < $m {
                my $multi_method := nqp::atpos(@multi_methods, $i);
                $HOW.add_multi_method(
                  $concretization,
                  $multi_method.name,
                  $multi_method.code.instantiate_generic($type_env)
                );
                ++$i;
            }
        }

        # Roles done by this role need fully specializing also.
        my @roles := self.roles_to_compose;
        if $m := nqp::elems(@roles) {
            $i := 0;
            while $i < $m {
                my $ins := my $role := nqp::atpos(@roles, $i);

                if $role.HOW.archetypes($role).generic {
                    $ins := $role.HOW.instantiate_generic($role, $type_env);
                    $ins.HOW.archetypes.parametric
                      ?? $HOW.add_to_role_typecheck_list($concretization, $ins)
                      !! self.not_composable($target, $ins);
                }

                $ins := $ins.HOW.specialize($ins, nqp::atpos(@_, 0));
                $HOW.add_role($concretization, $ins);
                $HOW.add_concretization($concretization, $role, $ins);
                ++$i;
            }
        }

        # Pass along any parents that have been added, resolving them in
        # the case they're generic (role Foo[::T] is T { })
        my @parents := self.parents($target, :local);
        if $m := nqp::elems(@parents) {
            $i := 0;
            while $i < $m {
                my     $parent := nqp::atpos(@parents, $i);
                my int $hides  := self.hides_parent($target, $parent);
                $parent := $parent.HOW.instantiate_generic($parent, $type_env)
                  if $parent.HOW.archetypes($parent).generic;

                $HOW.add_parent($concretization, $parent, :$hides);
                ++$i;
            }
        }

        # Resolve any array type being passed along (only really used in the
        # punning case, since roles are the way we get generic types).
        if self.is_array_type {
            my $at := self.array_type;
            $at := $at.HOW.instantiate_generic($at, $type_env)
              if $at.HOW.archetypes($at).generic;
            $HOW.set_array_type($concretization, $at);
        }

        $HOW.compose($concretization);
        $concretization
    }

    # Instantiate all generics bound to special lexicals in role's body.
    # Must be invoked by role code before any of these lexicals is referenced.
    method resolve_instantiations($XXX, $ctx, @ins_list) {
        my $type_env := Perl6::Metamodel::Configuration.type_env_from(
          $ctx, :boundary-by('::?ROLE')
        );

        my int $m := nqp::elems(@ins_list);
        if nqp::isnull($type_env) {
            my int $i;
            while $i < $m {
                my str $ins := nqp::atpos(@ins_list, $i);
                nqp::bindkey(
                  $ctx,
                  $ins,
                  $type_env.cache(nqp::getlexrel($ctx, $ins))
                );
                ++$i;
            }
            $type_env
        }
        else {
            my int $i;
            while $i < $m {
                my str $ins     := nqp::atpos(@ins_list, $i);
                my     $generic := nqp::getlexrel($ctx, $ins);
                nqp::bindkey(
                  $ctx,
                  $ins,
                  $generic.HOW.instantiate_generic($generic, $type_env)
                );
                ++$i;
            }
            $ctx
        }
    }

    method mro($target, :$roles, :$concretizations, :$unhidden) {
        nqp::list($target)
    }

    # Error handling methods
    method wrong_body_result($target, $got) {
        my str $got_name := $got.HOW.name($got);

        Perl6::Metamodel::Configuration.throw_or_die(
          'X::Role::BodyReturn',
          "Role '"
            ~ self.name($target)
            ~ "' body block is expected to return a list, got '"
            ~ $got_name
            ~ "' instead",
          :role($target),
          :expected("a list of two elements"),
          :got(
            (nqp::isconcrete($got)
              ?? "an object instance"
              !! "a type object"
            ) ~ " of type $got_name"
          )
        );
    }

    method could_not_instantiate($target, $error) {
        my $exception := nqp::getpayload($error);
        Perl6::Metamodel::Configuration.throw_or_die(
          'X::Role::Instantiation',
          "Could not instantiate role '"
            ~ self.name($target)
            ~ "':\n"
            ~ ($exception || nqp::getmessage($error)),
          :role($target),
          :exception($error)
        );
    }

    method not_composable($target, $composer) {
        my str $target-name := self.name($target);
        my str $role-name   := $composer.HOW.name($composer);
        Perl6::Metamodel::Configuration.throw_or_die(
          'X::Composition::NotComposable',
          "$role-name is not composable, so $target-name cannot compose it",
          :$target-name,
          :$composer
        )
    }
}

# vim: expandtab sw=4
