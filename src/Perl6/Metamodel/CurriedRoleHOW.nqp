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
    does Perl6::Metamodel::RolePunning
    does Perl6::Metamodel::TypePretense
    does Perl6::Metamodel::Naming
    does Perl6::Metamodel::RoleContainer
    does Perl6::Metamodel::LanguageRevision
    does Perl6::Metamodel::InvocationProtocol
{
    has $!curried_role;
    has $!candidate;                # Will contain matching candidate from curried role group
    has @!pos_args;
    has %!named_args;
    has @!role_typecheck_list;
    has $!is_complete;
    has $!archetypes;

    my $archetypes_g := Perl6::Metamodel::Archetypes.new( :composable(1), :inheritalizable(1), :parametric(1), :generic(1) );
    my $archetypes_ng := Perl6::Metamodel::Archetypes.new( :nominal(1), :composable(1), :inheritalizable(1), :parametric(1) );
    method !choose_archetype() {
        for @!pos_args {
            if $_.HOW.archetypes.generic {
                return $archetypes_g;
            }
        }
        for %!named_args {
            if $_.value.HOW.archetypes.generic {
                return $archetypes_g;
            }
        }
        $archetypes_ng
    }
    method archetypes() {
        if nqp::isconcrete(self) {
            $!archetypes := self.'!choose_archetype'() unless $!archetypes;
            return $!archetypes;
        }
        $archetypes_ng
    }

    method new(*%named) {
        nqp::findmethod(NQPMu, 'BUILDALL')(nqp::create(self), |%named)
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
        $meta.compose_invocation($type);

        nqp::settypecheckmode($type, 2);
    }

    method parameterize_roles($obj) {
        my @pos_args;
        nqp::push(@pos_args, $obj);
        for @!pos_args {
            nqp::push(@pos_args, $_);
        }
        if nqp::istype($!curried_role.HOW, Perl6::Metamodel::ParametricRoleGroupHOW) {
            $!candidate := $!curried_role.HOW.select_candidate($!curried_role, @pos_args, %!named_args);

            self.set_language_revision($obj, $!candidate.HOW.language-revision($!candidate));

            my $type_env;
            try {
                my @result := $!candidate.HOW.body_block($!candidate)(|@pos_args, |%!named_args);
                $type_env := @result[1];
            }
            for $!candidate.HOW.roles($!candidate, :!transitive) -> $role {
                if nqp::can($role.HOW, 'curried_role') && $role.HOW.archetypes.generic && $type_env {
                    $role := $role.HOW.instantiate_generic($role, $type_env);
                }
                self.add_role($obj, $role);
            }
        }
        self.update_role_typecheck_list($obj);
    }

    method update_role_typecheck_list($obj) {
        my @rtl;
        nqp::push(@rtl, $!curried_role);
        # XXX Not sure if it makes sense adding roles from group into the type checking.
        # for $!curried_role.HOW.role_typecheck_list($obj) {
        #     nqp::push(@rtl, $_);
        # }
        for self.roles_to_compose($obj) {
            nqp::push(@rtl, $_);
            for $_.HOW.role_typecheck_list($_) {
                nqp::push(@rtl, $_);
            }
        }
        @!role_typecheck_list := @rtl;
    }

    method complete_parameterization($obj) {
        unless $!is_complete {
            $!is_complete := 1;
            self.parameterize_roles($obj);
            self.update_role_typecheck_list($obj);
        }
    }

    method instantiate_generic($obj, $type_env) {
        my @new_pos;
        my %new_named;
        for @!pos_args {
            @new_pos.push($_.HOW.archetypes.generic ??
                $_.HOW.instantiate_generic($_, $type_env) !!
                $_);
        }
        for %!named_args {
            %new_named{$_.key} := $_.value.HOW.archetypes.generic ??
                $_.value.HOW.instantiate_generic($_.value, $type_env) !!
                $_.value;
        }
        self.new_type($!curried_role, |@new_pos, |%new_named)
    }

    method specialize($obj, $first_arg) {
        $!curried_role.HOW.specialize($!curried_role, $first_arg,
            |@!pos_args, |%!named_args);
    }

    method curried_role($obj) {
        $!curried_role
    }

    method role_arguments($obj) {
        @!pos_args
    }

    method roles($obj, :$transitive = 1, :$mro = 0) {
        self.complete_parameterization($obj);
        self.roles-ordered($obj, self.roles_to_compose($obj), :$transitive, :$mro)
    }

    method role_typecheck_list($obj) {
        self.complete_parameterization($obj);
        @!role_typecheck_list
    }

    method type_check($obj, $checkee) {
        my $decont := nqp::decont($checkee);
        if $decont =:= $obj.WHAT {
            return 1;
        }
        if $decont =:= $!curried_role {
            return 1;
        }
        for self.pretending_to_be() {
            if $decont =:= nqp::decont($_) {
                return 1;
            }
        }
        self.complete_parameterization($obj) unless $!is_complete;
        if !($!candidate =:= NQPMu) && $!candidate.HOW.type_check_parents($!candidate, $decont) {
            return 1
        }
        for @!role_typecheck_list {
            my $dr := nqp::decont($_);
            if $decont =:= $dr {
                return 1;
            }
            if nqp::istype($dr, $decont) {
                return 1;
            }
        }
        0
    }

    method accepts_type($obj, $checkee) {
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
                if nqp::istype($_.HOW, self.WHAT) && !$_.HOW.archetypes.generic {
                    if nqp::decont($_.HOW.curried_role($_)) =:= $crdc {
                        @cands.push($_);
                    }
                }
            }
        }

        # Provided we have some candidates, check the arguments.
        my int $num_args := +@!pos_args;
        if @cands {
            for @cands {
                my @try_args := $_.HOW.role_arguments($_);
                if +@try_args == $num_args {
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

    method is-implementation-detail($obj) {
        $!curried_role.is-implementation-detail($obj)
    }
}

# vim: expandtab sw=4
