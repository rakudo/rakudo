class Perl6::Metamodel::ClassHOW
    does Perl6::Metamodel::Naming
    does Perl6::Metamodel::Documenting
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
    does Perl6::Metamodel::InvocationProtocol
    does Perl6::Metamodel::ContainerSpecProtocol
    does Perl6::Metamodel::Finalization
    does Perl6::Metamodel::Concretization
{
    has @!roles;
    has @!role_typecheck_list;
    has @!fallbacks;
    has $!composed;

    my $archetypes := Perl6::Metamodel::Archetypes.new(
        :nominal(1), :inheritable(1), :augmentable(1) );
    method archetypes() {
        $archetypes
    }

    method new(*%named) {
        nqp::findmethod(NQPMu, 'BUILDALL')(nqp::create(self), |%named)
    }

    my $anon_id := 1;
    method new_type(:$name, :$repr = 'P6opaque', :$ver, :$auth, :$api, :$is_mixin) {
        my $metaclass := self.new();
        my $new_type;
        if $is_mixin {
            $new_type := nqp::newmixintype($metaclass, $repr);
        }
        else {
            $new_type := nqp::newtype($metaclass, $repr);
        }
        my $obj := nqp::settypehll($new_type, 'perl6');
        $metaclass.set_name($obj, $name // "<anon|{$anon_id++}>");
        self.add_stash($obj);
        $metaclass.set_ver($obj, $ver) if $ver;
        $metaclass.set_auth($obj, $auth) if $auth;
        $metaclass.set_api($obj, $api) if $api;
        $metaclass.setup_mixin_cache($obj);
        nqp::setboolspec($obj, 5, nqp::null());
        $obj
    }

    # Adds a new fallback for method dispatch. Expects the specified
    # condition to have been met (passes it the object and method name),
    # and if it is calls $calculator with the object and method name to
    # calculate an invokable object.
    method add_fallback($obj, $condition, $calculator) {
        # Adding a fallback means any method cache is no longer authoritative.
        nqp::setmethcacheauth($obj, 0);

        # Add it.
        my %desc;
        %desc<cond> := $condition;
        %desc<calc> := $calculator;
        @!fallbacks[+@!fallbacks] := %desc;
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
        return 0;
    }

    method compose($the-obj, :$compiler_services) {
        my $obj := nqp::decont($the-obj);

        # Set class language version if class belongs to the CORE
        if $*COMPILING_CORE_SETTING {
            self.set_language_version($the-obj);
        }

        # Instantiate all of the roles we have (need to do this since
        # all roles are generic on ::?CLASS) and pass them to the
        # composer.
        my @roles_to_compose := self.roles_to_compose($obj);
        my @stubs;
        if @roles_to_compose {
            my @ins_roles;
            while @roles_to_compose {
                my $r := @roles_to_compose.pop();
                @!roles[+@!roles] := $r;
                @!role_typecheck_list[+@!role_typecheck_list] := $r;
                my $ins := $r.HOW.specialize($r, $obj);
                @ins_roles.push($ins);
                self.add_concretization($obj, $r, $ins);
            }
            self.compute_mro($obj); # to the best of our knowledge, because the role applier wants it.
            @stubs := RoleToClassApplier.apply($obj, @ins_roles);

            # Add them to the typecheck list, and pull in their
            # own type check lists also.
            for @ins_roles {
                @!role_typecheck_list[+@!role_typecheck_list] := $_;
                for $_.HOW.role_typecheck_list($_) {
                    @!role_typecheck_list[+@!role_typecheck_list] := $_;
                }
            }
        }

        # Some things we only do if we weren't already composed once, like
        # building the MRO.
        my $was_composed := $!composed;
        unless $!composed {
            if self.parents($obj, :local(1)) == 0 && self.has_default_parent_type && self.name($obj) ne 'Mu' {
                self.add_parent($obj, self.get_default_parent_type);
            }
            self.compute_mro($obj);
            $!composed := 1;
        }

        # Incorporate any new multi candidates (needs MRO built).
        self.incorporate_multi_candidates($obj);

        # Set up finalization as needed.
        self.setup_finalization($obj);

        # Compose attributes.
        self.compose_attributes($obj, :$compiler_services);

        # Test the remaining stubs
        for @stubs -> %data {
            if !has_method(%data<target>, %data<name>) {
                nqp::die("Method '" ~ %data<name> ~ "' must be implemented by " ~
                         %data<target>.HOW.name(%data<target>) ~
                         " because it is required by roles: " ~
                         nqp::join(", ", %data<needed>) ~ ".");
            }
        }

        # See if we have a Bool method other than the one in the top type.
        # If not, all it does is check if we have the type object.
        unless self.get_boolification_mode($obj) != 0 {
            my $i := 0;
            my @mro := self.mro($obj);
            while $i < +@mro {
                my $ptype := @mro[$i];
                last if nqp::existskey(nqp::hllize($ptype.HOW.method_table($ptype)), 'Bool');
                last if nqp::can($ptype.HOW, 'submethod_table') &&
                    nqp::existskey(nqp::hllize($ptype.HOW.submethod_table($ptype)), 'Bool');
                $i := $i + 1;
            }
            if $i + 1 == +@mro {
                self.set_boolification_mode($obj, 5)
            }
        }

        # If there's a FALLBACK method, register something to forward calls to it.
        my $FALLBACK := self.find_method($obj, 'FALLBACK', :no_fallback);
        if !nqp::isnull($FALLBACK) && nqp::defined($FALLBACK) {
            self.add_fallback($obj,
                sub ($obj, str $name) {
                    $name ne 'sink' && $name ne 'CALL-ME'
                },
                sub ($obj, str $name) {
                    -> $inv, *@pos, *%named { $FALLBACK($inv, $name, |@pos, |%named) }
                });
        }

        # This isn't an augment.
        unless $was_composed {

            # Create BUILDPLAN.
            self.create_BUILDPLAN($obj);

            # Attempt to auto-generate a BUILDALL method. We can
            # only auto-generate a BUILDALL method if we have compiler
            # services. If we don't, then BUILDALL will fall back to the
            # one in Mu, which will iterate over the BUILDALLPLAN.
            if nqp::isconcrete($compiler_services) {

                # Class does not appear to have a BUILDALL yet
                unless nqp::existskey(nqp::hllize($obj.HOW.submethod_table($obj)),'BUILDALL')
                  || nqp::existskey(nqp::hllize($obj.HOW.method_table($obj)),'BUILDALL') {
                    my $builder := nqp::findmethod(
                      $compiler_services,'generate_buildplan_executor');
                    my $method :=
                      $builder($compiler_services,$obj,self.BUILDALLPLAN($obj));

                    # We have a generated BUILDALL submethod, so install!
                    unless $method =:= NQPMu {
                        $method.set_name('BUILDALL');
                        self.add_method($obj,'BUILDALL',$method);
                    }
                }
            }

            # Compose the representation
            self.compose_repr($obj);
        }

        # Publish type and method caches.
        self.publish_type_cache($obj);
        self.publish_method_cache($obj);
        self.publish_boolification_spec($obj);
        self.publish_container_spec($obj);

        # Compose the meta-methods.
        self.compose_meta_methods($obj);

        # Compose invocation protocol.
        self.compose_invocation($obj);

        $obj
    }

    method roles($obj, :$local, :$transitive = 1) {
        my @result;
        for @!roles {
            @result.push($_);
            if $transitive {
                for $_.HOW.roles($_, :transitive(1)) {
                    @result.push($_);
                }
            }
        }
        unless $local {
            my $first := 1;
            for self.mro($obj) {
                if $first {
                    $first := 0;
                    next;
                }
                for $_.HOW.roles($_, :$transitive, :local(1)) {
                    @result.push($_);
                }
            }
        }
        @result
    }

    method role_typecheck_list($obj) {
        $!composed ?? @!role_typecheck_list !! self.roles_to_compose($obj)
    }

    method is_composed($obj) {
        $!composed
    }

    # Stuff for junctiony dispatch fallback.
    my $junction_type;
    my $junction_autothreader;
    method setup_junction_fallback($type, $autothreader) {
        nqp::setmethcacheauth($type, 0);
        $junction_type := $type;
        $junction_autothreader := $autothreader;
    }

    # Handles the various dispatch fallback cases we have.
    method find_method_fallback($obj, $name) {
        # If the object is a junction, need to do a junction dispatch.
        if $obj.WHAT =:= $junction_type && $junction_autothreader {
            my $p6name := nqp::hllizefor($name, 'perl6');
            return -> *@pos_args, *%named_args {
                $junction_autothreader($p6name, |@pos_args, |%named_args)
            };
        }

        # Consider other fallbacks, if we have any.
        for @!fallbacks {
            if ($_<cond>)($obj, $name) {
                return ($_<calc>)($obj, $name);
            }
        }

        # Otherwise, didn't find anything.
        nqp::null()
    }

    # Does the type have any fallbacks?
    method has_fallbacks($obj) {
        return nqp::istype($obj, $junction_type) || +@!fallbacks;
    }
}
