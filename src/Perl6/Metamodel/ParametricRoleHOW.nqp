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
    has $!body_block;
    has $!in_group;
    has $!group;
    has $!signatured;
    has @!role_typecheck_list;

    my $archetypes := Perl6::Metamodel::Archetypes.new( :nominal(1), :composable(1), :inheritalizable(1), :parametric(1) );
    method archetypes($XXX?) { $archetypes }

    method new_type(:$repr, :$signatured, *%_) {
        my $HOW    := self.new(:$signatured);
        my $target := nqp::settypehll(nqp::newtype($HOW, 'Uninstantiable'), 'Raku');

        $HOW.set_identity($target, %_);

        $HOW.set_group($target, nqp::atkey(%_, 'group'))
          if nqp::existskey(%_, 'group');

        $HOW.add_stash($target);
    }

    method parameterize($target, *@pos_args, *%named_args) {
        Perl6::Metamodel::CurriedRoleHOW.new_type($target, |@pos_args, |%named_args)
    }

    method set_body_block($XXX, $block) {
        $!body_block := $block
    }

    method body_block($XXX?) { $!body_block }
    method signatured($XXX?) { $!signatured }

    method set_group($XXX, $group) {
        $!group := $group;
        $!in_group := 1;
    }

    method group($target) {
        $!in_group ?? $!group !! $target
    }

    method compose($target, :$compiler_services) {
        $target := nqp::decont($target);

        self.set_language_version($target);

        my @rtl;
        if $!in_group {
            @rtl.push($!group);
        }
        for self.roles_to_compose {
            my $how := $_.HOW;
            if $how.archetypes.composable || $how.archetypes.composalizable {
                @rtl.push($_);
                for $_.HOW.role_typecheck_list($_) {
                    @rtl.push($_);
                }
            }
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

    # $checkee must always be decont'ed
    method type_check_parents($target, $checkee) {
        for self.parents($target, :local) -> $parent {
            if nqp::istype($parent, $checkee) {
                return 1;
            }
        }
        0
    }

    method type_check($target, $checkee) {
        $checkee := nqp::decont($checkee);
        if $checkee =:= $target.WHAT {
            return 1;
        }
        if $!in_group && $checkee =:= $!group {
            return 1;
        }
        for self.pretending_to_be() {
            if $checkee =:= nqp::decont($_) {
                return 1;
            }
        }
        for self.roles_to_compose {
            if nqp::istype($checkee, $_) {
                return 1;
            }
        }
        self.type_check_parents($target, $checkee);
    }

    method specialize($target, *@pos_args, *%named_args) {
        # We only allow one specialization of a role to take place at a time,
        # since the body block captures the methods into its lexical scope,
        # but we don't do the appropriate cloning until a bit later. These
        # must happen before another specialize happens and re-captures the
        # things we are composing.
        self.protect({
            my $class := @pos_args[0];
            my $conc := nqp::if(nqp::can($class.HOW, 'get_cached_conc'),
                        $class.HOW.get_cached_conc($class, $target, @pos_args, %named_args),
                        nqp::null());
            if (nqp::isnull($conc)) {
                # Pre-create a concrete role. We'll finalize it later, in specialize_with method. But for now we need it
                # to initialize $?CONCRETIZATION by role's body block.
                my $*MOP-ROLE-CONCRETIZATION := $conc :=
                    Perl6::Metamodel::ConcreteRoleHOW.new_type(:roles([$target]), :name(self.name($target)));
                $conc.HOW.set_language_revision($conc, self.language_revision);
                $conc.HOW.set_hidden($conc) if self.hidden($target);

                # Run the body block to get the type environment (we know
                # the role in this case).
                my $type_env;
                my $error;
                try {
                    my $result := $!body_block(|@pos_args, |%named_args);
                    if nqp::isconcrete($result) {
                        # Support for bodies returning Raku's positional
                        my $original-result := $result;
                        if nqp::can($result, 'FLATTENABLE_LIST') {
                            $result := $result.FLATTENABLE_LIST();
                        }
                        if nqp::islist($result) && nqp::elems($result) == 2 {
                            $type_env :=
                                nqp::ifnull(Perl6::Metamodel::Configuration.type_env_from($result[1]), $result[1]);
                        }
                        else {
                            Perl6::Metamodel::Configuration.throw_or_die(
                                'X::Role::BodyReturn',
                                "Role '" ~ self.name($target) ~ "' body block is expected to return a list, got '"
                                    ~ $original-result.HOW.name($original-result) ~ "' instead",
                                :role($target),
                                :expected("a list of two elements"),
                                :got( (nqp::isconcrete($original-result)
                                        ?? "an object instance" !! "a type object")
                                      ~ " of type " ~ $original-result.HOW.name($original-result) ))
                        }
                    }
                    else {
                        # When there is no concrete return value from the body use empty TypeEnv then.
                        # Assuming that no Raku-generated role body would return an undefined value, especially those
                        # that belong to the core; and assuming that the only period of time when TypeEnv is not
                        # available on the configuration class is the early stages of the CORE.c compilation, – we can
                        # safely skip the check for nullness. Can't we?
                        $type_env := Perl6::Metamodel::Configuration.type_env_type().new;
                    }
                    CATCH {
                        $error := $!;
                    }
                }
                if $error {
                    my $exception := nqp::getpayload($error);
                    Perl6::Metamodel::Configuration.throw_or_die(
                        'X::Role::Instantiation',
                        "Could not instantiate role '" ~ self.name($target)
                            ~ "':\n" ~ ($exception || nqp::getmessage($error)),
                        :role($target),
                        :exception($error) )
                }

                # Use it to build a concrete role.
                $conc := self.specialize_with($target, $conc, $type_env, @pos_args);
                nqp::if(
                    nqp::can($class.HOW, 'add_conc_to_cache'),
                    $class.HOW.add_conc_to_cache($class, $target, @pos_args, %named_args, $conc)
                );
            }
            $conc
        })
    }

    method specialize_with($target, $conc, $type_env, @pos_args) {
        # Go through attributes, reifying as needed and adding to
        # the concrete role.
        for self.attributes($target, :local(1)) {
            $conc.HOW.add_attribute($conc,
                $_.is_generic ?? $_.instantiate_generic($type_env) !! nqp::clone($_));
        }

        # Go through methods and instantiate them; we always do this
        # unconditionally, since we need the clone anyway.
        my @methods      := nqp::hllize(self.method_order($target));
        my @method_names := nqp::hllize(self.method_names($target));
        my $method_iterator := nqp::iterator(@methods);
        for @method_names -> $name {
            $conc.HOW.add_method($conc, $name, nqp::shift($method_iterator).instantiate_generic($type_env))
        }
        my %private_methods := nqp::hllize(self.private_method_table($target));
        my @private_methods := nqp::hllize(self.private_method_names($target));
        for @private_methods -> $name {
            $conc.HOW.add_private_method($conc, $name, %private_methods{$name}.instantiate_generic($type_env));
        }
        for self.multi_methods_to_incorporate($target) {
            $conc.HOW.add_multi_method($conc, $_.name, $_.code.instantiate_generic($type_env))
        }

        # Roles done by this role need fully specializing also.
        for self.roles_to_compose {
            my $ins := my $r := $_;
            if $_.HOW.archetypes($_).generic {
                $ins := $ins.HOW.instantiate_generic($ins, $type_env);
                unless $ins.HOW.archetypes.parametric {
                    my $target-name := self.name($target);
                    my $role-name := $ins.HOW.name($ins);
                    Perl6::Metamodel::Configuration.throw_or_die(
                        'X::Composition::NotComposable',
                        $role-name ~ " is not composable, so " ~ $target-name ~ " cannot compose it",
                        :$target-name,
                        composer => $ins,
                    )
                }
                $conc.HOW.add_to_role_typecheck_list($conc, $ins);
            }
            $ins := $ins.HOW.specialize($ins, @pos_args[0]);
            $conc.HOW.add_role($conc, $ins);
            $conc.HOW.add_concretization($conc, $r, $ins);
        }

        # Pass along any parents that have been added, resolving them in
        # the case they're generic (role Foo[::T] is T { })
        for self.parents($target, :local(1)) {
            my $p := $_;
            if $p.HOW.archetypes($p).generic {
                $p := $p.HOW.instantiate_generic($p, $type_env);
            }
            $conc.HOW.add_parent($conc, $p, :hides(self.hides_parent($target, $_)));
        }

        # Resolve any array type being passed along (only really used in the
        # punning case, since roles are the way we get generic types).
        if self.is_array_type {
            my $at := self.array_type;
            if $at.HOW.archetypes($at).generic {
                $at := $at.HOW.instantiate_generic($at, $type_env);
            }
            $conc.HOW.set_array_type($conc, $at);
        }

        $conc.HOW.compose($conc);
        $conc
    }

    # Instantiate all generics bound to special lexicals in role's body. Must be invoked by role code before any
    # of these lexicals is referenced.
    method resolve_instantiations($XXX, $ctx, @ins-list) {
        my $type_env := Perl6::Metamodel::Configuration.type_env_from($ctx, :boundary-by('::?ROLE'));
        my $hll-typeenv := nqp::isnull($type_env);
        for @ins-list {
            my $generic := nqp::getlexrel($ctx, $_);
            nqp::bindkey(
                $ctx, $_,
                ($hll-typeenv
                    ?? $type_env.cache($generic)
                    !! $generic.HOW.instantiate_generic($generic, $type_env)));
        }
        $hll-typeenv ?? $type_env !! $ctx
    }

    method mro($target, :$roles, :$concretizations, :$unhidden) {
        [$target]
    }
}

# vim: expandtab sw=4
