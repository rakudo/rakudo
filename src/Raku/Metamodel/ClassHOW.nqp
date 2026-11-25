class Perl6::Metamodel::ClassHOW
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
    does Perl6::Metamodel::MetaMethodContainer
    does Perl6::Metamodel::RoleContainer
    does Perl6::Metamodel::MultipleInheritance
    does Perl6::Metamodel::DefaultParent
    does Perl6::Metamodel::C3MRO
    does Perl6::Metamodel::MROBasedMethodDispatch
    does Perl6::Metamodel::MROBasedTypeChecking
    does Perl6::Metamodel::Trusting
    does Perl6::Metamodel::BUILDPLAN
    does Perl6::Metamodel::Mixins
    does Perl6::Metamodel::ArrayType
    does Perl6::Metamodel::BoolificationProtocol
    does Perl6::Metamodel::REPRComposeProtocol
    does Perl6::Metamodel::ContainerSpecProtocol
    does Perl6::Metamodel::Finalization
    does Perl6::Metamodel::Concretization
    does Perl6::Metamodel::ConcretizationCache
#?if !moar
    does Perl6::Metamodel::InvocationProtocol
#?endif
{
    has @!roles;
    has @!role_typecheck_list;
    has @!fallbacks;

    # If class is coming from a pun then this is the source role
    has $!pun_source;

    has $!archetypes;

    my $archetypes-ng := Perl6::Metamodel::Archetypes.new(
      :nominal, :inheritable, :augmentable
    );
    my $archetypes-g  := Perl6::Metamodel::Archetypes.new(
      :nominal, :inheritable, :augmentable, :generic
    );

    method TWEAK(*%_) {
        $!pun_source := nqp::null;
    }

    method archetypes($target = nqp::null()) {
#?if moar
        # The dispatcher itself is declared at the end of this file. We can't have it in the BOOTSTRAP because the
        # bootstrap process is using archetypes long before dispatchers from dispatchers.nqp gets registered.
        nqp::dispatch('raku-class-archetypes', self, $target)
#?endif
#?if !moar
        if nqp::isconcrete(my $dcobj := nqp::decont($target)) && nqp::can($dcobj, 'is-generic') {
            return $dcobj.is-generic ?? $archetypes-g !! $archetypes-ng;
        }
        $!archetypes // $archetypes-ng
#?endif
    }

    method !refresh_archetypes($target) {
        $!archetypes :=
            nqp::can($target, 'is-generic') && $target.is-generic
                ?? $archetypes-g
                !! $archetypes-ng
    }

    method set_pun_source($XXX, $role) { $!pun_source := nqp::decont($role) }
    method is_pun(    $XXX?) { nqp::not_i(nqp::isnull($!pun_source)) }
    method pun_source($XXX?) { $!pun_source }

    method new_type(:$repr = 'P6opaque', :$is_mixin, *%_) {
        my $HOW := self.new;
        my $new_type := $is_mixin
          ?? nqp::newmixintype($HOW, $repr)
          !! nqp::newtype(     $HOW, $repr);
        my $target := nqp::settypehll($new_type, 'Raku');

        $HOW.set_identity($target, %_);

        $HOW.add_stash($target);
        $HOW.setup_mixin_cache($target);
        nqp::setboolspec($target, 5, nqp::null);
        $target
    }

    my class Fallback {
        has $!calculator;
        has $!condition;

        method new($condition, $calculator) {
            my $obj := nqp::create(self);
            nqp::bindattr($obj, Fallback, '$!condition',  $condition );
            nqp::bindattr($obj, Fallback, '$!calculator', $calculator);
            $obj
        }

        method fallbacker($target, str $name) {
            $!condition($target, $name)
              ?? $!calculator($target, $name)
              !! nqp::null
        }
    }

    # Adds a new fallback for method dispatch. Expects the specified
    # condition to have been met (passes it the object and method name),
    # and if it is calls $calculator with the object and method name to
    # calculate an invokable object.
    method add_fallback($target, $condition, $calculator) {
        self.protect({
#?if !moar
            # Adding a fallback means any method cache is no longer
            # authoritative.
            nqp::setmethcacheauth($target, 0);
#?endif

            # Add it in a threadsafe manner
            my @fallbacks := nqp::clone(@!fallbacks);
            nqp::push(@fallbacks, Fallback.new($condition, $calculator));
            @!fallbacks := @fallbacks;
        });
    }

    # Does the type declare a method by the given name
    sub has_method($target, str $name) {
        my @mro := $target.HOW.mro($target);

        my int $m := nqp::elems(@mro);
        my int $i;
        while $i < $m {
            my $type := nqp::atpos(@mro, $i);
            $type.HOW.declares_method($type, $name)
              ?? (return 1)
              !! ++$i;
        }
        0
    }

    method compose($target, :$compiler_services) {
        $target := nqp::decont($target);

        self.set_language_version($target);

        # Instantiate all of the roles we have (need to do this since
        # all roles are generic on ::?CLASS) and pass them to the
        # composer.
        my $applier;
        unless nqp::isnull(my $role := self.pop_role_to_compose) {
            my @roles               := nqp::clone(@!roles);
            my @role_typecheck_list := nqp::clone(@!role_typecheck_list);
            my @ins_roles;

            until nqp::isnull($role) {
                nqp::push(@roles, $role);
                nqp::push(@role_typecheck_list, $role);

                my $ins := $role.HOW.specialize($role, $target);

                # If class is a result of pun then transfer hidden flag
                # and language revision from the source role
#?if !moar
                if self.is_pun && nqp::eqaddr($!pun_source, $role) {
#?endif
#?if moar
                if nqp::eqaddr($!pun_source, $role) {
#?endif
                    self.set_hidden($target) if $ins.HOW.hidden($ins);
                    self.set_language_revision(
                      $target, $ins.HOW.language_revision, :force
                    );
                }

                nqp::push(@ins_roles, $ins);
                self.add_concretization($target, $role, $ins);

                # fetch next tole to handle
                $role := self.pop_role_to_compose;
            }

            # Because the role applier needs to have it
            self.compute_mro($target);
            $applier :=
              Perl6::Metamodel::Configuration.role_to_class_applier_type.new;
            $applier.prepare($target, @ins_roles);

            self.wipe_conc_cache;

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

            # Update atomically
            @!roles               := @roles;
            @!role_typecheck_list := @role_typecheck_list;
        }

        # Compose class attributes first. We prioritize them and their
        # accessors over anything coming from roles.
        self.compose_attributes($target, :$compiler_services);

        # Apply any roles to the class, obtain remaining stubs
        my @stubs := $applier.apply if $applier;

        # Some things we only do if we weren't already composed once, like
        # building the MRO.
        my $was_composed := self.run_if_not_composed({
            self.add_parent($target, self.get_default_parent_type)
              if nqp::elems(self.parents($target, :local)) == 0
              && self.has_default_parent_type
              && self.name($target) ne 'Mu';

            self.compute_mro($target);
        });

        # Incorporate any new multi candidates (needs MRO built).
        self.incorporate_multi_candidates($target);

        # Compose remaining attributes from roles.
        self.compose_attributes($target, :$compiler_services);

        # Set up finalization as needed.
        self.setup_finalization($target);

        # Test the remaining stubs
        my int $m := nqp::elems(@stubs);
        my int $i;
        while $i < $m {
            my %data := nqp::atpos(@stubs, $i);
            has_method($target, nqp::atkey(%data, 'name'))
              ?? ++$i
              !! self.method_not_implemented($target, %data)
        }

        # See if we have a Bool method other than the one in the top type.
        # If not, all it does is check if we have the type object.
        unless self.get_boolification_mode($target) != 0 {
            my @mro := self.mro($target);

            my int $m := nqp::elems(@mro);
            my int $i;
            while $i < $m {
                my $ptype := nqp::atpos(@mro, $i);
                $ptype.HOW.declares_method($ptype, 'Bool')
                  ?? (last)
                  !! ++$i;
            }
            self.set_boolification_mode($target, 5) if $i + 1 == $m;
        }

        # If there's a FALLBACK method, register something to forward
        # calls to it.
        my $FALLBACK := self.find_method($target, 'FALLBACK', :no_fallback);
        if nqp::isconcrete($FALLBACK) {
            self.add_fallback($target,
              sub ($target, str $name) {
                  $name ne 'sink' && $name ne 'CALL-ME'
              },
              sub ($target, str $name) {
                  -> $self, *@_, *%_ { $FALLBACK($self, $name, |@_, |%_) }
              }
            );
        }

        # This isn't an augment.
        unless $was_composed {

            # Create BUILDPLAN.
            self.create_BUILDPLAN($target);

            # Attempt to auto-generate a POPULATE method. We can
            # only auto-generate a POPULATE method if we have compiler
            # services. If we don't, then POPULATE will fall back to the
            # one in Mu, which will iterate over the BUILDALLPLAN.
            if nqp::isconcrete($compiler_services) && nqp::can($compiler_services, 'generate_buildplan_executor') {

                # Class does not appear to have a POPULATE yet
                unless self.declares_method($target, 'POPULATE') {
                    my $method := nqp::findmethod(
                      $compiler_services, 'generate_buildplan_executor'
                    )($compiler_services, $target, self.BUILDALLPLAN($target));

                    # We have a generated POPULATE submethod, so install!
                    if nqp::isconcrete($method) {
                        $method.set_name('POPULATE');
                        self.add_method($target, 'POPULATE', $method);
                    }
                }
            }

            # Compose the representation
            self.compose_repr($target);
        }

        # Publish type and method caches.
        self.publish_type_cache($target);
        self.publish_method_cache($target);
        self.publish_boolification_spec($target);
        self.publish_container_spec($target);

        # Compose the meta-methods.
        self.compose_meta_methods($target);

#?if !moar
        # Compose invocation protocol.
        self.compose_invocation($target);
#?endif

        self.'!refresh_archetypes'($target);

        $target
    }

    method roles($target, :$local, :$transitive = 1, :$mro) {
        my @roles := self.roles-ordered(@!roles, :$transitive, :$mro);
        unless $local {
            my @mro := self.mro($target);

            my int $m := nqp::elems(@mro);
            my int $i := 1;  # intentionally skip first
            while $i < $m {
                my $type := nqp::atpos(@mro, $i);
                nqp::splice(
                  @roles,
                  $type.HOW.roles($type, :$transitive, :$mro, :local),
                  nqp::elems(@roles),
                  0
                );
                ++$i;
            }
        }
        @roles
    }

    method role_typecheck_list($target) {
        self.is_composed ?? @!role_typecheck_list !! self.roles_to_compose
    }

    # Stuff for junctiony dispatch fallback.
    my $junction_type;
    my $junction_autothreader;
    method setup_junction_fallback($type, $autothreader) {
#?if !moar
        nqp::setmethcacheauth($type, 0);
#?endif
        $junction_type         := $type;
        $junction_autothreader := $autothreader;
    }

    # Handles the various dispatch fallback cases we have.
    method find_method_fallback($target, $name, :$local) {

        # If the object is a junction, need to do a junction dispatch.
        if nqp::isconcrete($junction_autothreader)
          && nqp::istype($target, $junction_type) {

            $name := nqp::hllizefor($name, 'Raku');
            -> *@_, *%_ {
                # Fallback on an undefined junction means no method found.
                nqp::isconcrete(nqp::atpos(@_, 0))
                  ?? $junction_autothreader($name, |@_, |%_)
                  !! nqp::null
            }
        }

        else {
            my @fallbacks := @!fallbacks;

            # Consider other fallbacks, if we have any.
            my int $m := nqp::elems(@fallbacks);
            my int $i;
            while $i < $m {
                nqp::isnull(
                  my $fallbacker := nqp::atpos(
                    @fallbacks, $i
                  ).fallbacker($target, $name)
                ) ?? ++$i
                  !! (return $fallbacker)
            }

            unless $local {
                my @mro := self.mro($target);

                my int $m := nqp::elems(@mro);
                my int $i := 1;  # intentionally skip first
                while $i < $m {
                    my $HOW := nqp::atpos(@mro, $i).HOW;
                    nqp::can($HOW, 'find_method_fallback')
                      && nqp::isconcrete(
                           my $fallback := $HOW.find_method_fallback(
                             $target, $name, :local
                           )
                         )
                      ?? (return $fallback)
                      !! ++$i;
                }
            }

            # Otherwise, didn't find anything.
            nqp::null
        }
    }

    # Does the type have any fallbacks?
    method has_fallbacks($target, :$local = 0) {
        return 1
          if nqp::istype($target, $junction_type)
          || nqp::elems(@!fallbacks);

        unless $local {
            my @mro := self.mro($target);

            my int $m := nqp::elems(@mro);
            my int $i := 1;  # skip first intentionally
            while $i < $m {
                my $HOW := nqp::atpos(@mro, $i).HOW;
                nqp::can($HOW, 'has_fallbacks')
                  && $HOW.has_fallbacks($target, :local)
                  ?? (return 1)
                  !! ++$i;
            }
        }
        0
    }

    method instantiate_generic($target, $type_environment) {
        my $type-env :=
          Perl6::Metamodel::Configuration.type_env_from($type_environment);

        nqp::isnull($type-env)
          ?? $target
          !! $type-env.cache(
               $target,
               { $target.INSTANTIATE-GENERIC($type-env) }
             )
    }

    method method_not_implemented($target, %data) {
        nqp::die("Method '"
          ~ nqp::atkey(%data, 'name')
          ~ "' must be implemented by "
          ~ $target.HOW.name($target)
          ~ " because it is required by roles: "
          ~ nqp::join(", ", nqp::atkey(%data, 'needed'))
          ~ "."
        );
    }

#?if moar
    # Returns archetypes of a class or a class instance.
    # Dispatcher arguments: ClassHOW object, invocant object
    nqp::register('raku-class-archetypes', -> $capture {

        my $how := nqp::captureposarg($capture, 0);
        my $Thow := nqp::track('arg', $capture, 0);
        nqp::guard('concreteness', $Thow);

        nqp::delegate('boot-code-constant', $archetypes-ng)
          unless nqp::isconcrete($how);

        my $obj  := nqp::captureposarg($capture, 1);
        my $Tobj := nqp::track('arg', $capture, 1);
        nqp::guard('concreteness', $Tobj);
        nqp::guard('type', $Tobj);

        if nqp::isconcrete_nd($obj) && nqp::iscont($obj) {
            my $Scalar := nqp::gethllsym('Raku', 'Scalar');
            my $Tvalue := nqp::track('attr', $Tobj, $Scalar, '$!value');
            nqp::guard('concreteness', $Tvalue);
            nqp::guard('type', $Tvalue);
            $obj := nqp::getattr($obj, $Scalar, '$!value');
        }

        my $can-is-generic :=
          !nqp::isnull($obj) && nqp::can($obj, 'is-generic');
        if nqp::isconcrete($obj) && $can-is-generic {
            # If invocant of .HOW.archetypes is a concrete object
            # implementing 'is-generic' method then method outcome
            # is the ultimate result. But we won't cache it in
            # type's HOW $!archetypes.
            nqp::delegate('boot-code-constant',
              nqp::syscall('dispatcher-insert-arg-literal-obj',
                nqp::syscall('dispatcher-drop-n-args',
                  $capture, 0, 2
                ),
                0, { $obj.is-generic ?? $archetypes-g !! $archetypes-ng }
              )
            );
        }
        else {
            nqp::guard('literal', nqp::track('attr',
              $Thow, Perl6::Metamodel::ClassHOW, '$!archetypes'
            ));

            nqp::delegate('boot-constant',
              nqp::syscall('dispatcher-insert-arg-literal-obj',
                $capture, 0,
                nqp::getattr($how, Perl6::Metamodel::ClassHOW, '$!archetypes')
                  // $archetypes-ng
              )
            );
        }
    }
);
#?endif
}

# vim: expandtab sw=4
