#- Metamodel::CurriedRoleHOW ---------------------------------------------------
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
    does Perl6::Metamodel::Composing
    does Perl6::Metamodel::RolePunning
    does Perl6::Metamodel::TypePretense
    does Perl6::Metamodel::RoleContainer
    does Perl6::Metamodel::LanguageRevision
#?if !moar
    does Perl6::Metamodel::InvocationProtocol
#?endif
{
    has $!curried_role;
    has @!pos_args;
    has %!named_args;
    has $!archetypes;

    has $!candidate;  # Will contain matching candidate from curried role group
    has @!role_typecheck_list;
    has @!parent_typecheck_list;  # Only for parents instantiated from generics

    my $archetypes_generic := Perl6::Metamodel::Archetypes.new(
                :composable, :inheritalizable, :parametric, :generic
    );
    my $archetypes_not_generic := Perl6::Metamodel::Archetypes.new(
      :nominal, :composable, :inheritalizable, :parametric
    );
    method archetypes($XXX?) { $!archetypes }

    method TWEAK(:$curried_role, :@pos_args, :%named_args, *%_) {

        sub choose_archetype() {
            my int $m := nqp::elems(@pos_args);
            my int $i;
            while $i < $m {
                my $value := nqp::atpos(@pos_args, $i);
                $value.HOW.archetypes($value).generic
                  ?? (return $archetypes_generic)
                  !! ++$i;
            }

            for %named_args {
                my $value := $_.value;
                return $archetypes_generic
                  if $value.HOW.archetypes($value).generic;
            }

            $archetypes_not_generic
        }

        $!archetypes := choose_archetype();
        $!candidate  := nqp::null;  # no candidate yet

        my str $name := $curried_role.HOW.name($curried_role);
        my int $m    :=  nqp::elems(@pos_args);

        if $m {
            my @parts := nqp::list_s;
            my int $i;
            while $i < $m {
                my $value := nqp::atpos(@pos_args, $i);
                nqp::push_s(@parts, $value.HOW.name($value));
                ++$i;
            }
            $name := $name ~ "[" ~ nqp::join(",", @parts) ~ "]";
        }
        self.set_name($curried_role, $name);
    }

    method new_type($curried_role, *@pos_args, *%named_args) {
        my $HOW := self.new(:$curried_role, :@pos_args, :%named_args);
        my $type := nqp::newtype($HOW, 'Uninstantiable');
        nqp::settypehll($type, 'Raku');
#?if !moar
        $HOW.compose_invocation($type);
#?endif

        nqp::settypecheckmode($type, nqp::const::TYPE_CHECK_NEEDS_ACCEPTS)
    }

    # Make the curried role ready for use and return the candidate found
    # if any.  Don't bother composing again if already composed.
    method compose($target) {
        self.run_if_not_composed({

            # XXX Needs to be set here because otherwise we start to
            # infiniloop on the .type_check logic.  There should be a
            # better way to do this, but I'm not seeing one at the moment.
            self.set_composed;

            if nqp::istype(
              $!curried_role.HOW,
              Perl6::Metamodel::ParametricRoleGroupHOW
            ) {

                # Make sure positional arguments carry the invocant
                my @pos_args := nqp::clone(@!pos_args);
                nqp::unshift(@pos_args,$target);
                my %named_args := %!named_args;

                my $curried_role    := $!curried_role;
                my $curried_roleHOW := $curried_role.HOW;
                my $candidate := $curried_roleHOW.select_candidate(
                  $curried_role, @pos_args, %named_args
                );
                my $candidateHOW := $candidate.HOW;

                self.set_language_revision(
                  $target, $candidateHOW.language_revision
                );

                my $type_env;
                try {
                    $type_env := nqp::atpos(
                      $candidateHOW.body_block($candidate)(
                        |@pos_args, |%named_args
                      ),
                      1
                    );
                    if $!archetypes.generic {
                        my $env := Perl6::Metamodel::Configuration.type_env_from(
                          $type_env
                        );
                        $type_env := $env unless nqp::isnull($env);
                    }
                }

                my @roles := $candidateHOW.roles($candidate, :!transitive);
                my int $m := nqp::elems(@roles);
                my int $i;
                while $i < $m {
                    my $role := nqp::atpos(@roles, $i);
                    my $HOW  := $role.HOW;
                    $role := $HOW.instantiate_generic($role, $type_env)
                      if $type_env && $HOW.archetypes.generic;

                    $HOW.archetypes.generic || $HOW.archetypes.parametric
                      ?? self.add_role($target, $role)
                      !! self.not_composable($target, $role);
                    ++$i;
                }

                # Contrary to roles, we only consider generic parents. I.e.
                # cases like: # role R[::T] is T {}
                if $type_env {
                    my @parents := $candidateHOW.parents($candidate, :local);
                    my @parent_typecheck_list :=
                      nqp::clone(@!parent_typecheck_list);

                    my int $m := nqp::elems(@parents);
                    my int $i;
                    while $i < $m {
                        my $parent := nqp::atpos(@parents, $i);
                        nqp::push(
                          @parent_typecheck_list,
                          $parent.HOW.instantiate_generic($parent, $type_env)
                        ) if $parent.HOW.archetypes.generic;
                        ++$i;
                    }
                    @!parent_typecheck_list := @parent_typecheck_list;
                }

                $!candidate := $candidate;
            }

            my @role_typecheck_list := nqp::list($!curried_role);
            # XXX Not sure if it makes sense adding roles from group into the
            # type checking.
            # nqp::splice(
            #   @role_typecheck_list,
            #   $!curried_role.HOW.role_typecheck_list($target),
            #   1,
            #   0
            # );

            my @roles := self.roles_to_compose;

            my int $m := nqp::elems(@roles);
            my int $i;
            while $i < $m {
                my $role := nqp::atpos(@roles, $i);
                my $HOW  := $role.HOW;
                if $HOW.archetypes.composable || $HOW.archetypes.composalizable {
                    nqp::push(@role_typecheck_list, $role);
                    nqp::splice(
                      @role_typecheck_list,
                      $HOW.role_typecheck_list($role),
                      nqp::elems(@role_typecheck_list),
                      0
                   );
                }
                ++$i;
            }
            @!role_typecheck_list := @role_typecheck_list;
        });

        $!candidate
    }

    method instantiate_generic($XXX, $type_env) {
        my @new_pos;
        my int $m := nqp::elems(@!pos_args);

        if $m {
            @new_pos := nqp::clone(@!pos_args);

            my int $i;
            while $i < $m {
                my $value := nqp::atpos(@new_pos, $i);
                nqp::bindpos(@new_pos, $i,
                  $value.HOW.instantiate_generic($value, $type_env)
                ) if $value.HOW.archetypes($value).generic;
                ++$i;
            }
        }

        if nqp::elems(%!named_args) {
            my %new_named := nqp::clone(%!named_args);

            for %new_named {
                my $value := $_.value;
                nqp::bindkey(%new_named, $_.key,
                  $value.HOW.instantiate_generic($value, $type_env)
                ) if $value.HOW.archetypes($value).generic;
            }
            self.new_type($!curried_role, |@new_pos, |%new_named)
        }

        else {
            self.new_type($!curried_role, |@new_pos)
        }
    }

    method specialize($XXX, $first_arg) {
        $!curried_role.HOW.specialize($!curried_role, $first_arg,
          |@!pos_args, |%!named_args
        );
    }

    method curried_role(  $XXX?) { $!curried_role }
    method role_arguments($XXX?) { @!pos_args     }

    method roles($target, :$transitive = 1, :$mro = 0) {
        self.compose($target);
        self.roles-ordered(self.roles_to_compose, :$transitive, :$mro)
    }

    method role_typecheck_list($target) {
        self.compose($target);
        @!role_typecheck_list
    }

    method type_check($target, $checkee) {
        $checkee := nqp::decont($checkee);
        nqp::eqaddr($checkee, $target.WHAT)
          || nqp::eqaddr($checkee, $!curried_role)
          || self.checkee_eqaddr_list($checkee, self.pretending_to_be)
          || (nqp::not_i(nqp::isnull(self.compose($target)))
               && $!candidate.HOW.type_check_parents($!candidate, $checkee)
             )
          || self.checkee_istype_list($checkee, @!parent_typecheck_list)
          || self.checkee_eqaddr_list($checkee, @!role_typecheck_list)
          || self.list_istype_checkee(@!role_typecheck_list, $checkee)
    }

    method accepts_type($XXX, $checkee) {

        # No positional args, means nothing to check against, so no match
        my @pos_args := @!pos_args;
        return 0 unless my int $num_args := nqp::elems(@pos_args);

        # First, we locate candidate curryings to check against. If
        # the checkee is itself a curried role, it also goes in. Note
        # that we only want those that have the same parametric role
        # as us.
        $checkee    := nqp::decont($checkee);
        my $HOW     := $checkee.HOW;
        my $curried := nqp::decont($!curried_role);

        my @candidates;
        @candidates.push($checkee)
          if nqp::istype($HOW, self.WHAT)
          && nqp::eqaddr(nqp::decont($HOW.curried_role($checkee)), $curried);

        if nqp::can($HOW, 'role_typecheck_list') {
            my @typecheck_list := $checkee.HOW.role_typecheck_list($checkee);
            my int $m := nqp::elems(@typecheck_list);
            my int $i;
            while $i < $m {
                my $type := nqp::atpos(@typecheck_list, $i);
                @candidates.push($type)
                  if nqp::istype($type.HOW, self.WHAT)
                  && nqp::not_i($type.HOW.archetypes($type).generic)
                  && nqp::eqaddr(
                       nqp::decont($type.HOW.curried_role($type)), $curried
                     );
                ++$i;
            }
        }
        # No candidates, means nothing to check against, so no match
        return 0 unless my int $num_cands := nqp::elems(@candidates);

        my int $i;
        while $i < $num_cands {
            my $candidate := nqp::atpos(@candidates, $i);
            my @try_args  := $candidate.HOW.role_arguments($candidate);

            if nqp::elems(@try_args) == $num_args {
                my int $j;
                while $j < $num_args {
                    my $pos_arg := nqp::decont(nqp::atpos(@pos_args, $j));
                    my $try_arg := nqp::decont(nqp::atpos(@try_args, $j));

                    nqp::eqaddr($pos_arg, $try_arg)
                      || $pos_arg.ACCEPTS($try_arg)
                      ?? ++$j    # match, so continue checking
                      !! (last)  # no match, no reason to continue
                }
                return 1 if $j == $num_args;  # all matched ok
            }
            ++$i;
        }

        0
    }

    method is-implementation-detail($target) {
        $!curried_role.is-implementation-detail($target)
    }
}

# vim: expandtab sw=4
