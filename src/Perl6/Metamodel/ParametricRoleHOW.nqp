my $concrete := Perl6::Metamodel::ConcreteRoleHOW;
my $currier := Perl6::Metamodel::CurriedRoleHOW;
class Perl6::Metamodel::ParametricRoleHOW
    does Perl6::Metamodel::Naming
    does Perl6::Metamodel::Documenting
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
    does Perl6::Metamodel::InvocationProtocol
{
    has $!composed;
    has $!body_block;
    has $!in_group;
    has $!group;
    has $!signatured;
    has @!role_typecheck_list;
    has $!specialize_lock;

    my $archetypes := Perl6::Metamodel::Archetypes.new( :nominal(1), :composable(1), :inheritalizable(1), :parametric(1) );
    method archetypes() {
        $archetypes
    }

    method new(*%named) {
        nqp::findmethod(NQPMu, 'BUILDALL')(nqp::create(self), |%named)
    }

    my $anon_id := 1;
    method new_type(:$name, :$ver, :$auth, :$api, :$repr, :$signatured, *%extra) {
        my $metarole := self.new(:signatured($signatured), :specialize_lock(NQPLock.new));
        my $type := nqp::settypehll(nqp::newtype($metarole, 'Uninstantiable'), 'Raku');
        $metarole.set_name($type, $name // "<anon|{$anon_id++}>");
        $metarole.set_ver($type, $ver);
        $metarole.set_auth($type, $auth) if $auth;
        $metarole.set_api($type, $api) if $api;
        $metarole.set_pun_repr($type, $repr) if $repr;
        if nqp::existskey(%extra, 'group') {
            $metarole.set_group($type, %extra<group>);
        }
        self.add_stash($type);
    }

    method parameterize($obj, *@pos_args, *%named_args) {
        $currier.new_type($obj, |@pos_args, |%named_args)
    }

    method set_body_block($obj, $block) {
        $!body_block := $block
    }

    method body_block($obj) {
        $!body_block
    }

    method signatured($obj) {
        $!signatured
    }

    method set_group($obj, $group) {
        $!group := $group;
        $!in_group := 1;
    }

    method group($obj) {
        $!in_group ?? $!group !! $obj
    }

    method compose($the-obj, :$compiler_services) {
        my $obj := nqp::decont($the-obj);

        self.set_language_version($obj);

        my @rtl;
        if $!in_group {
            @rtl.push($!group);
        }
        for self.roles_to_compose($obj) {
            @rtl.push($_);
            for $_.HOW.role_typecheck_list($_) {
                @rtl.push($_);
            }
        }
        @!role_typecheck_list := @rtl;
        self.compose_invocation($obj);
        $!composed := 1;
        $obj
    }

    method is_composed($obj) {
        $!composed
    }

    method roles($obj, :$transitive = 1, :$mro) {
        self.roles-ordered($obj, self.roles_to_compose($obj), :$transitive, :$mro);
    }

    method role_typecheck_list($obj) {
        @!role_typecheck_list
    }

    # $checkee must always be decont'ed
    method type_check_parents($obj, $checkee) {
        for self.parents($obj, :local) -> $parent {
            if nqp::istype($checkee, $parent) {
                return 1;
            }
        }
        0
    }

    method type_check($obj, $checkee) {
        my $decont := nqp::decont($checkee);
        if $decont =:= $obj.WHAT {
            return 1;
        }
        if $!in_group && $decont =:= $!group {
            return 1;
        }
        for self.pretending_to_be() {
            if $decont =:= nqp::decont($_) {
                return 1;
            }
        }
        for self.roles_to_compose($obj) {
            if nqp::istype($decont, $_) {
                return 1;
            }
        }
        self.type_check_parents($obj, $decont);
    }

    method specialize($obj, *@pos_args, *%named_args) {
        # We only allow one specialization of a role to take place at a time,
        # since the body block captures the methods into its lexical scope,
        # but we don't do the appropriate cloning until a bit later. These
        # must happen before another specialize happens and re-captures the
        # things we are composing.
        $!specialize_lock.protect({
            my $class := @pos_args[0];
            my $conc := nqp::if(nqp::can($class.HOW, 'get_cached_conc'),
                        $class.HOW.get_cached_conc($class, $obj, @pos_args, %named_args),
                        nqp::null());
            if (nqp::isnull($conc)) {
                # Pre-create a concrete role. We'll finalize it later, in specialize_with method. But for now we need it
                # to initialize $?CONCRETIZATION by role's body block.
                my $*MOP-ROLE-CONCRETIZATION := $conc :=
                    $concrete.new_type(:roles([$obj]), :name(self.name($obj)));
                $conc.HOW.set_language_revision($conc, $obj.HOW.language-revision($obj));
                $conc.HOW.set_hidden($conc) if $obj.HOW.hidden($obj);

                # Run the body block to get the type environment (we know
                # the role in this case).
                my $type_env;
                my $error;
                try {
                    my @result := $!body_block(|@pos_args, |%named_args);
                    $type_env := @result[1];
                    CATCH {
                        $error := $!
                    }
                }
                if $error {
                    nqp::die("Could not instantiate role '" ~ self.name($obj)
                             ~ "':\n" ~ (nqp::getpayload($error) || nqp::getmessage($error)))
                }

                # Use it to build a concrete role.
                $conc := self.specialize_with($obj, $conc, $type_env, @pos_args);
                nqp::if(
                    nqp::can($class.HOW, 'add_conc_to_cache'),
                    $class.HOW.add_conc_to_cache($class, $obj, @pos_args, %named_args, $conc)
                );
            }
            $conc
        })
    }

    method specialize_with($obj, $conc, $type_env, @pos_args) {
        # Go through attributes, reifying as needed and adding to
        # the concrete role.
        for self.attributes($obj, :local(1)) {
            $conc.HOW.add_attribute($conc,
                $_.is_generic ?? $_.instantiate_generic($type_env) !! nqp::clone($_));
        }

        # Go through methods and instantiate them; we always do this
        # unconditionally, since we need the clone anyway.
        my @methods      := nqp::hllize(self.method_order($obj));
        my @method_names := nqp::hllize(self.method_names($obj));
        my $method_iterator := nqp::iterator(@methods);
        for @method_names -> $name {
            $conc.HOW.add_method($conc, $name, nqp::shift($method_iterator).instantiate_generic($type_env))
        }
        my %private_methods := nqp::hllize(self.private_method_table($obj));
        my @private_methods := nqp::hllize(self.private_method_names($obj));
        for @private_methods -> $name {
            $conc.HOW.add_private_method($conc, $name, %private_methods{$name}.instantiate_generic($type_env));
        }
        for self.multi_methods_to_incorporate($obj) {
            $conc.HOW.add_multi_method($conc, $_.name, $_.code.instantiate_generic($type_env))
        }

        # Roles done by this role need fully specializing also.
        for self.roles_to_compose($obj) {
            my $ins := my $r := $_;
            if $_.HOW.archetypes.generic {
                $ins := $ins.HOW.instantiate_generic($ins, $type_env);
                $conc.HOW.add_to_role_typecheck_list($conc, $ins);
            }
            $ins := $ins.HOW.specialize($ins, @pos_args[0]);
            $conc.HOW.add_role($conc, $ins);
            $conc.HOW.add_concretization($conc, $r, $ins);
        }

        # Pass along any parents that have been added, resolving them in
        # the case they're generic (role Foo[::T] is T { })
        for self.parents($obj, :local(1)) {
            my $p := $_;
            if $_.HOW.archetypes.generic {
                $p := $p.HOW.instantiate_generic($p, $type_env);
            }
            $conc.HOW.add_parent($conc, $p, :hides(self.hides_parent($obj, $_)));
        }

        # Resolve any array type being passed along (only really used in the
        # punning case, since roles are the way we get generic types).
        if self.is_array_type($obj) {
            my $at := self.array_type($obj);
            if $at.HOW.archetypes.generic {
                $at := $at.HOW.instantiate_generic($at, $type_env);
            }
            $conc.HOW.set_array_type($conc, $at);
        }

        $conc.HOW.compose($conc);
        return $conc;
    }

    method mro($obj, :$roles = 0, :$unhidden = 0) {
        [$obj]
    }
}

# vim: expandtab sw=4
