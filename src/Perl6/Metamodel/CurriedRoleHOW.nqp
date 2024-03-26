# Sometimes, we see references to roles that provide parameters but
# do not fully resolve them. For example, in:
#
#   class C does R[T] { }
#
# We need to represent R[T], but we cannot yet fully specialize the
# role because we don't have the first parameter to hand. We may also
# run into the issue where we have things like:
#
#   sub foo(R[T] $x) { ... }
#   if $x ~~ R[T] { ... }
#
# Where we clearly want to talk about a partial parameterization of a
# role and actually want to do so in a way distinct from a particular
# instantiation of it. This meta-object represents those "partial types"
# as both a way to curry on your way to a full specialization, but also
# as a way to do type-checking or punning.
class Perl6::Metamodel::CurriedRoleHOW
    does Perl6::Metamodel::Naming
    does Perl6::Metamodel::BUILDALL
    does Perl6::Metamodel::RolePunning
    does Perl6::Metamodel::TypePretense
    does Perl6::Metamodel::RoleContainer
    does Perl6::Metamodel::LanguageRevision
#?if !moar
    does Perl6::Metamodel::InvocationProtocol
#?endif
{
    has $!curried_role;
    has $!candidate;                # Will contain matching candidate from curried role group
    has @!pos_args;
    has %!named_args;
    has @!role_typecheck_list;
    has @!parent_typecheck_list;    # Only for parents instantiated from generics
    has $!is_complete;
    has $!archetypes;

    my $archetypes_g := Perl6::Metamodel::Archetypes.new( :composable(1), :inheritalizable(1), :parametric(1), :generic(1) );
    my $archetypes_ng := Perl6::Metamodel::Archetypes.new( :nominal(1), :composable(1), :inheritalizable(1), :parametric(1) );
    method !choose_archetype() {
        for @!pos_args {
            if $_.HOW.archetypes($_).generic {
                return $archetypes_g;
            }
        }
        for %!named_args {
            if (my $value := $_.value).HOW.archetypes($value).generic {
                return $archetypes_g;
            }
        }
        $archetypes_ng
    }
    method archetypes($XXX?) {
        if nqp::isconcrete(self) {
            $!archetypes := self.'!choose_archetype'() unless $!archetypes;
            return $!archetypes;
        }
        $archetypes_ng
    }

    method new_type($curried_role, *@pos_args, *%named_args) {
        # construct a name
        my $name := $curried_role.HOW.name($curried_role);
        if @pos_args {
            my @pieces := nqp::list_s();
            for @pos_args {
                nqp::push_s(@pieces, $_.HOW.name($_));
            }
            $name := $name ~ "[" ~ nqp::join(",", @pieces) ~ "]";
        }

        my $meta := self.new(:curried_role($curried_role), :pos_args(@pos_args),
            :named_args(%named_args), :name($name));
        my $type := nqp::settypehll(nqp::newtype($meta, 'Uninstantiable'), 'Raku');
        $meta.set_name($type, $name);
#?if !moar
        $meta.compose_invocation($type);
#?endif

        nqp::settypecheckmode($type, nqp::const::TYPE_CHECK_NEEDS_ACCEPTS);
    }

    method parameterize_roles($target) {
        my @pos_args;
        nqp::push(@pos_args, $target);
        for @!pos_args {
            nqp::push(@pos_args, $_);
        }
        if nqp::istype($!curried_role.HOW, Perl6::Metamodel::ParametricRoleGroupHOW) {
            $!candidate := $!curried_role.HOW.select_candidate($!curried_role, @pos_args, %!named_args);
            my $candidate-how := $!candidate.HOW;

            self.set_language_revision($target, $candidate-how.language_revision);

            my $type_env;
            try {
                my @result := $candidate-how.body_block($!candidate)(|@pos_args, |%!named_args);
                $type_env :=
                    self.archetypes.generic
                     ?? nqp::ifnull(Perl6::Metamodel::Configuration.type_env_from(@result[1]), @result[1])
                     !! @result[1];
            }
            for $candidate-how.roles($!candidate, :!transitive) -> $role {
                if $role.HOW.archetypes.generic && $type_env {
                    $role := $role.HOW.instantiate_generic($role, $type_env);
                }
                unless $role.HOW.archetypes.generic || $role.HOW.archetypes.parametric {
                    my $target-name := self.name($target);
                    my $role-name := $role.HOW.name($role);
                    Perl6::Metamodel::Configuration.throw_or_die(
                        'X::Composition::NotComposable',
                        $role-name ~ " is not composable, so " ~ $target-name ~ " cannot compose it",
                        :$target-name,
                        composer => $role,
                    )
                }
                self.add_role($target, $role);
            }
            # Contrary to roles, we only consider generic parents. I.e. cases like:
            # role R[::T] is T {}
            if $type_env {
                for $candidate-how.parents($!candidate, :local) -> $parent {
                    if $parent.HOW.archetypes.generic {
                        my $ins := $parent.HOW.instantiate_generic($parent, $type_env);
                        nqp::push(@!parent_typecheck_list, $ins)
                    }
                }
            }
        }
        self.update_role_typecheck_list($target);
    }

    method update_role_typecheck_list($target) {
        my @rtl;
        nqp::push(@rtl, $!curried_role);
        # XXX Not sure if it makes sense adding roles from group into the type checking.
        # for $!curried_role.HOW.role_typecheck_list($target) {
        #     nqp::push(@rtl, $_);
        # }
        for self.roles_to_compose -> $role {
            my $how := $role.HOW;
            if $how.archetypes.composable() || $how.archetypes.composalizable() {
                nqp::push(@rtl, $role);
                for $how.role_typecheck_list($role) {
                    nqp::push(@rtl, $_);
                }
            }
        }
        @!role_typecheck_list := @rtl;
    }

    method complete_parameterization($target) {
        unless $!is_complete {
            $!is_complete := 1;
            self.parameterize_roles($target);
            self.update_role_typecheck_list($target);
        }
    }

    method instantiate_generic($XXX, $type_env) {
        my @new_pos;
        my %new_named;
        for @!pos_args {
            @new_pos.push($_.HOW.archetypes($_).generic ??
                $_.HOW.instantiate_generic($_, $type_env) !!
                $_);
        }
        for %!named_args {
            %new_named{$_.key} := $_.value.HOW.archetypes($_.value).generic ??
                $_.value.HOW.instantiate_generic($_.value, $type_env) !!
                $_.value;
        }
        self.new_type($!curried_role, |@new_pos, |%new_named)
    }

    method specialize($XXX, $first_arg) {
        $!curried_role.HOW.specialize($!curried_role, $first_arg,
            |@!pos_args, |%!named_args);
    }

    method curried_role(  $XXX?) { $!curried_role }
    method role_arguments($XXX?) { @!pos_args     }

    method roles($target, :$transitive = 1, :$mro = 0) {
        self.complete_parameterization($target);
        self.roles-ordered(self.roles_to_compose, :$transitive, :$mro)
    }

    method role_typecheck_list($target) {
        self.complete_parameterization($target);
        @!role_typecheck_list
    }

    method type_check($target, $checkee) {
        $checkee := nqp::decont($checkee);
        if $checkee =:= $target.WHAT {
            return 1;
        }
        if $checkee =:= $!curried_role {
            return 1;
        }
        for self.pretending_to_be() {
            if $checkee =:= nqp::decont($_) {
                return 1;
            }
        }
        self.complete_parameterization($target) unless $!is_complete;
        if !($!candidate =:= NQPMu) && $!candidate.HOW.type_check_parents($!candidate, $checkee) {
            return 1
        }
        for @!parent_typecheck_list -> $parent {
            if nqp::istype($checkee, $parent) {
                return 1
            }
        }
        for @!role_typecheck_list {
            my $dr := nqp::decont($_);
            if $checkee =:= $dr {
                return 1;
            }
            if nqp::istype($dr, $checkee) {
                return 1;
            }
        }
        0
    }

    method accepts_type($XXX, $checkee) {
        # First, we locate candidate curryings to check against. If
        # the checkee is itself a curried role, it also goes in. Note
        # that we only want those that have the same parametric role
        # as us.
        my @cands;
        my $crdc := nqp::decont($!curried_role);
        if nqp::istype($checkee.HOW, self.WHAT) {
            if nqp::decont($checkee.HOW.curried_role($checkee)) =:= $crdc {
                @cands.push($checkee);
            }
        }
        if nqp::can($checkee.HOW, 'role_typecheck_list') {
            for $checkee.HOW.role_typecheck_list($checkee) {
                if nqp::istype($_.HOW, self.WHAT) && !$_.HOW.archetypes($_).generic {
                    if nqp::decont($_.HOW.curried_role($_)) =:= $crdc {
                        @cands.push($_);
                    }
                }
            }
        }

        # Provided we have some candidates, check the arguments.
        my int $num_args := nqp::elems(@!pos_args);
        if @cands {
            for @cands {
                my @try_args := $_.HOW.role_arguments($_);
                if nqp::elems(@try_args) == $num_args {
                    my int $i := -1;
                    my int $ok := 1;
                    while ($i := $i + 1) < $num_args {
                        unless    nqp::eqaddr(nqp::decont(@!pos_args[$i]), nqp::decont(@try_args[$i]))
                               || @!pos_args[$i].ACCEPTS(@try_args[$i])
                        {
                            $ok := 0;
                            $i := $num_args;
                        }
                    }
                    if $ok {
                        return 1;
                    }
                }
            }
        }

        0;
    }

    method shortname($curried_role) {
        $curried_role.HOW.name($curried_role);
    }

    method is-implementation-detail($target) {
        $!curried_role.is-implementation-detail($target)
    }
}

# vim: expandtab sw=4
