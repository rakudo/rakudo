#- Metamodel::ConcreteRoleHOW --------------------------------------------------
class Perl6::Metamodel::ConcreteRoleHOW
    does Perl6::Metamodel::Naming
    does Perl6::Metamodel::BUILDALL
    does Perl6::Metamodel::Composing
    does Perl6::Metamodel::LanguageRevision
    does Perl6::Metamodel::PrivateMethodContainer
    does Perl6::Metamodel::MethodContainer
    does Perl6::Metamodel::MultiMethodContainer
    does Perl6::Metamodel::AttributeContainer
    does Perl6::Metamodel::RoleContainer
    does Perl6::Metamodel::MultipleInheritance
    does Perl6::Metamodel::ArrayType
    does Perl6::Metamodel::Concretization
    does Perl6::Metamodel::C3MRO
{
    # Any collisions to resolve.
    has @!collisions;

    # The (parametric) role(s) that this concrete one was directly derived
    # from.
    has @!roles;

    # Full flat list of done roles.
    has @!role_typecheck_list;

    my $archetypes := Perl6::Metamodel::Archetypes.new(
      :nominal, :composable
    );
    method archetypes($XXX?) { $archetypes }

    my class Collision {
        has str $!name;
        has     @!roles;
        has int $!private;
        has     $!multi;

        method new(str $name, @roles, $private, $multi) {
            my $obj := nqp::create(self);

            nqp::bindattr_s($obj, Collision, '$!name',  $name);
            nqp::bindattr(  $obj, Collision, '@!roles', @roles);
            nqp::bindattr_i($obj, Collision, '$!private', 1) if $private;
            nqp::bindattr(  $obj, Collision, '$!multi', $multi);

            $obj
        }

        method name()    { $!name    }
        method roles()   { @!roles   }
        method private() { $!private }
        method multi()   { $!multi   }
    }

    method new_type(:@roles, :$repr, *%_) {
        my $HOW  := self.new(:@roles);
        my $type := nqp::newtype($HOW, 'Uninstantiable');
        nqp::settypehll($type, 'Raku');
        $HOW.set_identity($type, %_);
        $type
    }

    method add_collision($XXX, $name, @role_names, :$private, :$multi) {
        self.protect({
            my @collisions := @!collisions;
            nqp::push(@!collisions, Collision.new(
              $name, @role_names, $private, $multi
            ));
            @!collisions := @collisions;
        });
    }

    method compose($target) {
        unless self.is_composed {
            $target := nqp::decont($target);
            my @roles_to_compose := self.roles_to_compose;

            Perl6::Metamodel::Configuration.role_to_role_applier_type.apply(
              $target, @roles_to_compose
            );

            my @role_typecheck_list := nqp::clone(@!role_typecheck_list);
            sub add_to_typecheck_list(@roles) {
                my int $m := nqp::elems(@roles);
                my int $i;
                while $i < $m {
                    my $role := nqp::atpos(@roles, $i);
                    nqp::push(@role_typecheck_list, $role);
                    nqp::splice(
                      @role_typecheck_list,
                      $role.HOW.role_typecheck_list($role),
                      nqp::elems(@role_typecheck_list),
                      0
                    );
                    ++$i;
                }
            }

            add_to_typecheck_list(@roles_to_compose);
            add_to_typecheck_list(@!roles);
            @!role_typecheck_list := @role_typecheck_list;

# This debugging code produces some really weird results when building the
# setting.  So leaving this in here for further investigation at a later time.
# The weirdness is really expressed by duplicating iterations, with different
# values, and skipping ierations.  Even though they all appear to be running
# in the same thread.
#nqp::say("composing role " ~ $target.HOW.name($target) ~ " typechecks " ~ nqp::elems(@role_typecheck_list));
#my int $i;
#for @role_typecheck_list {
#    nqp::say(nqp::threadid(nqp::currentthread) ~ " " ~ $i++ ~ " " ~ nqp::objectid($_));
#}

            self.publish_type_cache($target);
            self.set_composed;
        }
        $target
    }

    method collisions($XXX?) { @!collisions }

    # It makes sense for concretizations to default to MRO order of roles.
    method roles($XXX?, :$transitive = 1, :$mro = 1) {
        $transitive
          ?? self.roles-ordered(@!roles, :transitive, :$mro)
          !! @!roles
    }

    method add_to_role_typecheck_list($XXX, $type) {
        self.protect({
            my @role_typecheck_list := nqp::clone(@!role_typecheck_list);
            nqp::push(@role_typecheck_list, $type);
            @!role_typecheck_list := @role_typecheck_list;
        });
    }

    method role_typecheck_list($XXX?) { @!role_typecheck_list }

    method type_check($target, $checkee) {
        $checkee := nqp::decont($checkee);

        # Helper sub to check checkee against a list of types
        sub check_checkee_against(@types) {
            my int $m := nqp::elems(@types);
            my int $i;
            while $i < $m {
                nqp::eqaddr($checkee, nqp::decont(nqp::atpos(@types, $i)))
                  ?? (return 1)
                  !! ++$i;
            }
            0
        }

        nqp::eqaddr($checkee,$target.WHAT)
          || check_checkee_against(@!role_typecheck_list)
    }

    method publish_type_cache($target) {
        my @role_typecheck_list := @!role_typecheck_list;
        my @types := nqp::list($target.WHAT);
        my %seen;

        # While debugging another issue, it became clear that the typecheck
        # list did not contain just unique values.  This in itself is not
        # a problem for matching types, but it forces unnecessary extra checks
        # during runtime if the type does *not* match, and it uses more
        # memory.  So make sure we only have unique values in this types
        # typecheck cache.
        my int $m := nqp::elems(@role_typecheck_list);
        my int $i;
        while $i < $m {
            my $type := nqp::atpos(@role_typecheck_list, $i);
            my int $id := nqp::objectid($type);
            unless nqp::existskey(%seen, $id) {
                nqp::push(@types, $type);
                nqp::bindkey(%seen, $id, 1);
            }
            ++$i;
        }
        nqp::settypecache($target, @types)
    }

    method mro($target, :$roles, :$concretizations, :$unhidden) {
        nqp::list($target)
    }

    method find_method_qualified($target, $qtype, $name) {
        $target := nqp::decont($target);
        $qtype  := nqp::decont($qtype);

        if $qtype.HOW.archetypes.parametric {
            my $found := nqp::null;
            my @concretizations := self.concretizations($target, :transitive);

            my int $m := nqp::elems(@concretizations);
            my int $i;
            while $i < $m {
                my $candidate := nqp::atpos(@concretizations, $i);
                my $role      := nqp::atpos(
                  $candidate.HOW.roles($candidate, :!transitive, :!mro), 0
                );
                $role := $role.HOW.group($role)
                  if nqp::can($role.HOW, 'group');

                if nqp::eqaddr($qtype, $role) {
                    nqp::isnull($found)
                      ?? ($found := $candidate)
                      !! nqp::die(
                           "Ambiguous concretization lookup for "
                             ~ $qtype.HOW.name($qtype)
                         );
                }
                ++$i;
            }
            nqp::isnull($found)
              ?? nqp::null
              !! nqp::atkey($found.HOW.method_table($found), $name)
                   || nqp::atkey($found.HOW.submethod_table($found), $name)
        }

        # Non-parametric, so just locate it from the already concrete type.
        elsif nqp::istype($target, $qtype) {
            nqp::findmethod($qtype, $name)
        }

        else {
            nqp::null
        }
    }

    method is-implementation-detail($target) {
        nqp::atpos(@!roles, 0).is-implementation-detail($target)
    }
}

# vim: expandtab sw=4
