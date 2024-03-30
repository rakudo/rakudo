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
    has $!is_pun;
    has $!pun_source; # If class is coming from a pun then this is the source role
    has $!archetypes;

    my $archetypes-ng := Perl6::Metamodel::Archetypes.new( :nominal, :inheritable, :augmentable );
    my $archetypes-g  := Perl6::Metamodel::Archetypes.new( :nominal, :inheritable, :augmentable, :generic );

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

    # Adds a new fallback for method dispatch. Expects the specified
    # condition to have been met (passes it the object and method name),
    # and if it is calls $calculator with the object and method name to
    # calculate an invokable object.
    method add_fallback($target, $condition, $calculator) {
#?if !moar
        # Adding a fallback means any method cache is no longer authoritative.
        nqp::setmethcacheauth($target, 0);
#?endif

        # Add it.
        my %desc;
        %desc<cond> := $condition;
        %desc<calc> := $calculator;
        nqp::push(@!fallbacks, %desc);
    }

    sub has_method($target, $name) {
        for $target.HOW.mro($target) {
            my %mt := nqp::hllize($_.HOW.method_table($_));
            if nqp::existskey(%mt, $name) {
                return 1;
            }
            %mt := nqp::hllize($_.HOW.submethod_table($_));
            if nqp::existskey(%mt, $name) {
                return 1;
            }
        }
        0
    }

    method compose($target, :$compiler_services) {
        $target := nqp::decont($target);

        self.set_language_version($target);

        # Instantiate all of the roles we have (need to do this since
        # all roles are generic on ::?CLASS) and pass them to the
        # composer.
        my @stubs;
        my $rtca;
        unless nqp::isnull(my $r := self.pop_role_to_compose) {
            my @ins_roles;
            until nqp::isnull($r) {
                nqp::push(@!roles, $r);
                nqp::push(@!role_typecheck_list, $r);
                my $ins := $r.HOW.specialize($r, $target);
                # If class is a result of pun then transfer hidden flag from the source role
                if $!pun_source =:= $r {
                    self.set_hidden($target) if $ins.HOW.hidden($ins);
                    self.set_language_revision($target, $ins.HOW.language_revision, :force);
                }
                @ins_roles.push($ins);
                self.add_concretization($target, $r, $ins);

                $r := self.pop_role_to_compose;
            }
            self.compute_mro($target); # to the best of our knowledge, because the role applier wants it.
            $rtca := Perl6::Metamodel::Configuration.role_to_class_applier_type.new;
            $rtca.prepare($target, @ins_roles);

            self.wipe_conc_cache;

            # Add them to the typecheck list, and pull in their
            # own type check lists also.
            for @ins_roles {
                nqp::push(@!role_typecheck_list, $_);
                for $_.HOW.role_typecheck_list($_) {
                    nqp::push(@!role_typecheck_list, $_);
                }
            }
        }

        # Compose class attributes first. We prioritize them and their accessors over anything coming from roles.
        self.compose_attributes($target, :$compiler_services);

        if $rtca {
            @stubs := $rtca.apply();
        }

        # Some things we only do if we weren't already composed once, like
        # building the MRO.
        my $was_composed := self.run_if_not_composed({
            if self.parents($target, :local) == 0
              && self.has_default_parent_type
              && self.name($target) ne 'Mu' {
                self.add_parent($target, self.get_default_parent_type);
            }
            self.compute_mro($target);
        });

        # Incorporate any new multi candidates (needs MRO built).
        self.incorporate_multi_candidates($target);

        # Compose remaining attributes from roles.
        self.compose_attributes($target, :$compiler_services);

        # Set up finalization as needed.
        self.setup_finalization($target);

        # Test the remaining stubs
        for @stubs -> %data {
            if !has_method($target, %data<name>) {
                nqp::die("Method '" ~ %data<name> ~ "' must be implemented by " ~
                         $target.HOW.name($target) ~
                         " because it is required by roles: " ~
                         nqp::join(", ", %data<needed>) ~ ".");
            }
        }

        # See if we have a Bool method other than the one in the top type.
        # If not, all it does is check if we have the type object.
        unless self.get_boolification_mode($target) != 0 {
            my @mro := self.mro($target);

            my int $m := nqp::elems(@mro);
            my int $i;
            while $i < $m {
                my $ptype := @mro[$i];
                last if nqp::existskey(nqp::hllize($ptype.HOW.method_table($ptype)), 'Bool');
                last if nqp::can($ptype.HOW, 'submethod_table') &&
                    nqp::existskey(nqp::hllize($ptype.HOW.submethod_table($ptype)), 'Bool');
                ++$i;
            }
            if $i + 1 == $m {
                self.set_boolification_mode($target, 5)
            }
        }

        # If there's a FALLBACK method, register something to forward calls to it.
        my $FALLBACK := self.find_method($target, 'FALLBACK', :no_fallback);
        if !nqp::isnull($FALLBACK) && nqp::defined($FALLBACK) {
            self.add_fallback($target,
                sub ($target, str $name) {
                    $name ne 'sink' && $name ne 'CALL-ME'
                },
                sub ($target, str $name) {
                    -> $inv, *@pos, *%named { $FALLBACK($inv, $name, |@pos, |%named) }
                });
        }

        # This isn't an augment.
        unless $was_composed {

            # Create BUILDPLAN.
            self.create_BUILDPLAN($target);

            # Attempt to auto-generate a BUILDALL method. We can
            # only auto-generate a BUILDALL method if we have compiler
            # services. If we don't, then BUILDALL will fall back to the
            # one in Mu, which will iterate over the BUILDALLPLAN.
            if nqp::isconcrete($compiler_services) {

                # Class does not appear to have a BUILDALL yet
                unless nqp::existskey(nqp::hllize(self.submethod_table($target)),'BUILDALL')
                  || nqp::existskey(nqp::hllize(self.method_table($target)),'BUILDALL') {
                    my $builder := nqp::findmethod(
                      $compiler_services,'generate_buildplan_executor');
                    my $method :=
                      $builder($compiler_services,$target,self.BUILDALLPLAN($target));

                    # We have a generated BUILDALL submethod, so install!
                    unless $method =:= NQPMu {
                        $method.set_name('BUILDALL');
                        self.add_method($target,'BUILDALL',$method);
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

    method roles($target, :$local, :$transitive = 1, :$mro = 0) {
        my @result := self.roles-ordered(@!roles, :$transitive, :$mro);
        unless $local {
            my $first := 1;
            for self.mro($target) {
                if $first {
                    $first := 0;
                    next;
                }
                for $_.HOW.roles($_, :$transitive, :$mro, :local(1)) {
                    @result.push($_);
                }
            }
        }
        @result
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
        $junction_type := $type;
        $junction_autothreader := $autothreader;
    }

    # Handles the various dispatch fallback cases we have.
    method find_method_fallback($target, $name, :$local = 0) {
        # If the object is a junction, need to do a junction dispatch.
        if nqp::istype($target.WHAT, $junction_type) && $junction_autothreader {
            my $p6name := nqp::hllizefor($name, 'Raku');
            return -> *@pos_args, *%named_args {
                # Fallback on an undefined junction means no method found.
                nqp::isconcrete(@pos_args[0])
                    ?? $junction_autothreader($p6name, |@pos_args, |%named_args)
                    !! nqp::null()
            };
        }

        # Consider other fallbacks, if we have any.
        for @!fallbacks {
            if ($_<cond>)($target, $name) {
                return ($_<calc>)(targetobj, $name);
            }
        }

        unless $local {
            my @mro := self.mro($target);

            my int $m := nqp::elems(@mro);
            my int $i := 1;  # intentionally skip first
            while $i < $m {
                my $HOW := nqp::atpos(@mro, $i).HOW;
                nqp::can($HOW, 'find_method_fallback')
                  && nqp::not_i(nqp::isnull(
                       my $fallback := $HOW.find_method_fallback(
                         $target, $name, :local
                       )
                     ))
                  ?? (return $fallback)
                  !! ++$i;
            }
        }

        # Otherwise, didn't find anything.
        nqp::null()
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

    method set_pun_source($XXX, $role) {
        $!pun_source := nqp::decont($role);
        $!is_pun := 1;
    }

    method is_pun(    $XXX?) { $!is_pun     }
    method pun_source($XXX?) { $!pun_source }

    method instantiate_generic($target, $type_environment) {
        my $type-env := Perl6::Metamodel::Configuration.type_env_from($type_environment);
        return $target if nqp::isnull($type-env);
        $type-env.cache($target, { $target.INSTANTIATE-GENERIC($type-env) });
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
