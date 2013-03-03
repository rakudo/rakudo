use Perl6::Metamodel;
use QRegex;

# Here we start to piece together the top of the object model hierarchy.
# We can't just declare these bits in CORE.setting with normal Perl 6
# syntax due to circularity issues. Note that we don't compose any of
# these - which is equivalent to a { ... } body.
#
# One particular circularity we break here is that you can't have
# inheritance in Perl 6 without traits, but that needs multiple
# dispatch, which can't function without some a type hierarchy in
# place. It also needs us to be able to declare a signature with
# parameters and a code objects with dispatchees, which in turn need
# attributes. So, we set up quite a few bits in here, though the aim
# is to keep it "lagom". :-)

# Bootstrapping Attribute class that we eventually replace with the read
# one.
my class BOOTSTRAPATTR {
    has $!name;
    has $!type;
    has $!box_target;
    has $!package;
    method name() { $!name }
    method type() { $!type }
    method box_target() { $!box_target }
    method package() { $!package }
    method has_accessor() { 0 }
    method has-accessor() { 0 }
    method build() { }
    method is_generic() { $!type.HOW.archetypes.generic }
    method instantiate_generic($type_environment) {
        my $ins := $!type.HOW.instantiate_generic($!type, $type_environment);
        self.new(:name($!name), :box_target($!box_target), :type($ins))
    }
    method compose($obj) { }
}

# Stub all types.
my stub Mu metaclass Perl6::Metamodel::ClassHOW { ... };
my stub Any metaclass Perl6::Metamodel::ClassHOW { ... };
my stub Cool metaclass Perl6::Metamodel::ClassHOW { ... };
my stub Attribute metaclass Perl6::Metamodel::ClassHOW { ... };
my stub Scalar metaclass Perl6::Metamodel::ClassHOW { ... };
my stub Proxy metaclass Perl6::Metamodel::ClassHOW { ... };
my stub Signature metaclass Perl6::Metamodel::ClassHOW { ... };
my stub Parameter metaclass Perl6::Metamodel::ClassHOW { ... };
my stub Code metaclass Perl6::Metamodel::ClassHOW { ... };
my stub Block metaclass Perl6::Metamodel::ClassHOW { ... };
my stub Routine metaclass Perl6::Metamodel::ClassHOW { ... };
my stub Sub metaclass Perl6::Metamodel::ClassHOW { ... };
my stub Method metaclass Perl6::Metamodel::ClassHOW { ... };
my stub Submethod metaclass Perl6::Metamodel::ClassHOW { ... };
my stub Regex metaclass Perl6::Metamodel::ClassHOW { ... };
my stub Str metaclass Perl6::Metamodel::ClassHOW { ... };
my knowhow bigint is repr('P6bigint') { }
my stub Int metaclass Perl6::Metamodel::ClassHOW { ... };
my stub Num metaclass Perl6::Metamodel::ClassHOW { ... };
my stub Parcel metaclass Perl6::Metamodel::ClassHOW { ... };
my stub Iterable metaclass Perl6::Metamodel::ClassHOW { ... };
my stub Iterator metaclass Perl6::Metamodel::ClassHOW { ... };
my stub ListIter metaclass Perl6::Metamodel::ClassHOW { ... };
my stub List metaclass Perl6::Metamodel::ClassHOW { ... };
my stub Array metaclass Perl6::Metamodel::ClassHOW { ... };
my stub LoL metaclass Perl6::Metamodel::ClassHOW { ... };
my stub EnumMap metaclass Perl6::Metamodel::ClassHOW { ... };
my stub Hash metaclass Perl6::Metamodel::ClassHOW { ... };
my stub Capture metaclass Perl6::Metamodel::ClassHOW { ... };
my stub Bool metaclass Perl6::Metamodel::ClassHOW { ... };
my stub ObjAt metaclass Perl6::Metamodel::ClassHOW { ... };
my stub Stash metaclass Perl6::Metamodel::ClassHOW { ... };
my stub PROCESS metaclass Perl6::Metamodel::ModuleHOW { ... };
my stub Grammar metaclass Perl6::Metamodel::ClassHOW { ... };
my stub Metamodel metaclass Perl6::Metamodel::PackageHOW { ... };

# We stick all the declarative bits inside of a BEGIN, so they get
# serialized.
BEGIN {
    # Maps code objects to their static self, avoiding them being closures.
    sub static($code) {
        $code.get_lexinfo().get_static_code()
    }

    # class Mu { ... }
    Mu.HOW.add_parrot_vtable_mapping(Mu, 'get_integer',
        static(sub ($self) {
            nqp::unbox_i($self.Int())
        }));
    Mu.HOW.add_parrot_vtable_mapping(Mu, 'get_number',
        static(sub ($self) {
            nqp::unbox_n($self.Num())
        }));
    Mu.HOW.add_parrot_vtable_mapping(Mu, 'get_string',
        static(sub ($self) {
            nqp::unbox_s($self.Str())
        }));
    Mu.HOW.add_parrot_vtable_mapping(Mu, 'defined',
        static(sub ($self) { nqp::istrue($self.defined()) }));
    Mu.HOW.compose_repr(Mu);

    # class Any is Mu { ... }
    Any.HOW.add_parent(Any, Mu);
    Any.HOW.compose_repr(Any);

    # class Cool is Any { ... }
    Cool.HOW.add_parent(Cool, Any);
    Cool.HOW.compose_repr(Cool);

    # class Attribute {
    #     has str $!name;
    #     has int $!rw;
    #     has int $!has_accessor;
    #     has $!type;
    #     has $!container_descriptor;
    #     has $!auto_viv_container;
    #     has $!build_closure;
    #     has $!package;
    #     ... # Uncomposed
    # }
    Attribute.HOW.add_parent(Attribute, Any);
    Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(:name<$!name>, :type(str), :package(Attribute)));
    Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(:name<$!rw>, :type(int), :package(Attribute)));
    Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(:name<$!ro>, :type(int), :package(Attribute)));
    Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(:name<$!has_accessor>, :type(int), :package(Attribute)));
    Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(:name<$!type>, :type(Mu), :package(Attribute)));
    Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(:name<$!container_descriptor>, :type(Mu), :package(Attribute)));
    Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(:name<$!auto_viv_container>, :type(Mu), :package(Attribute)));
    Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(:name<$!build_closure>, :type(Mu), :package(Attribute)));
    Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(:name<$!package>, :type(Mu), :package(Attribute)));
    Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(:name<$!box_target>, :type(int), :package(Attribute)));

    # Need new and accessor methods for Attribute in here for now.
    Attribute.HOW.add_method(Attribute, 'new',
        static(sub ($self, :$name!, :$type!, :$package!, :$has_accessor, *%other) {
            my $attr := nqp::create($self);
            nqp::bindattr_s($attr, Attribute, '$!name', $name);
            nqp::bindattr($attr, Attribute, '$!type', $type);
            nqp::bindattr_i($attr, Attribute, '$!has_accessor', $has_accessor);
            nqp::bindattr($attr, Attribute, '$!package', $package);
            if nqp::existskey(%other, 'container_descriptor') {
                nqp::bindattr($attr, Attribute, '$!container_descriptor', %other<container_descriptor>);
                if nqp::existskey(%other, 'auto_viv_container') {
                    nqp::bindattr($attr, Attribute, '$!auto_viv_container',
                        %other<auto_viv_container>);
                }
            }
            else {
                my $cd := Perl6::Metamodel::ContainerDescriptor.new(
                    :of($type), :rw(1), :name($name));
                my $scalar := nqp::create(Scalar);
                nqp::bindattr($scalar, Scalar, '$!descriptor', $cd);
                nqp::bindattr($scalar, Scalar, '$!value', $type);
                nqp::bindattr($attr, Attribute, '$!container_descriptor', $cd);
                nqp::bindattr($attr, Attribute, '$!auto_viv_container', $scalar);
            }
            $attr
        }));
    Attribute.HOW.add_method(Attribute, 'name', static(sub ($self) {
            nqp::getattr_s(pir::perl6_decontainerize__PP($self),
                Attribute, '$!name');
        }));
    Attribute.HOW.add_method(Attribute, 'type', static(sub ($self) {
            nqp::getattr(pir::perl6_decontainerize__PP($self),
                Attribute, '$!type');
        }));
    Attribute.HOW.add_method(Attribute, 'container_descriptor', static(sub ($self) {
            nqp::getattr(pir::perl6_decontainerize__PP($self),
                Attribute, '$!container_descriptor');
        }));
    Attribute.HOW.add_method(Attribute, 'auto_viv_container', static(sub ($self) {
            nqp::getattr(pir::perl6_decontainerize__PP($self),
                Attribute, '$!auto_viv_container');
        }));
    Attribute.HOW.add_method(Attribute, 'has_accessor', static(sub ($self) {
            pir::perl6_booleanize__PI(
                nqp::getattr_i(pir::perl6_decontainerize__PP($self),
                    Attribute, '$!has_accessor'));
        }));
    Attribute.HOW.add_method(Attribute, 'rw', static(sub ($self) {
            pir::perl6_booleanize__PI(
                nqp::getattr_i(pir::perl6_decontainerize__PP($self),
                    Attribute, '$!rw'));
        }));
    Attribute.HOW.add_method(Attribute, 'set_rw', static(sub ($self) {
            nqp::bindattr_i(pir::perl6_decontainerize__PP($self),
                Attribute, '$!rw', 1);
            pir::perl6_booleanize__PI(1)
        }));
    Attribute.HOW.add_method(Attribute, 'set_readonly', static(sub ($self) {
            nqp::bindattr_i(pir::perl6_decontainerize__PP($self),
                Attribute, '$!ro', 1);
            pir::perl6_booleanize__PI(1)
        }));
    Attribute.HOW.add_method(Attribute, 'default_to_rw', static(sub ($self) {
            my $dcself := pir::perl6_decontainerize__PP($self);
            unless nqp::getattr_i($dcself, Attribute, '$!ro') {
                nqp::bindattr_i($dcself, Attribute, '$!rw', 1);
            }
            pir::perl6_booleanize__PI(1)
        }));
    Attribute.HOW.add_method(Attribute, 'set_build', static(sub ($self, $closure) {
            pir::setattribute__0PPsP(pir::perl6_decontainerize__PP($self),
                Attribute, '$!build_closure', $closure);
        }));
    Attribute.HOW.add_method(Attribute, 'build', static(sub ($self) {
            nqp::getattr(pir::perl6_decontainerize__PP($self),
                Attribute, '$!build_closure');
        }));
    Attribute.HOW.add_method(Attribute, 'set_box_target', static(sub ($self) {
            nqp::bindattr_i(pir::perl6_decontainerize__PP($self),
                Attribute, '$!box_target', 1);
            pir::perl6_booleanize__PI(1)
        }));
    Attribute.HOW.add_method(Attribute, 'box_target', static(sub ($self) {
            nqp::getattr_i(pir::perl6_decontainerize__PP($self),
                Attribute, '$!box_target')
        }));
    Attribute.HOW.add_method(Attribute, 'is_generic', static(sub ($self) {
            my $dcself   := pir::perl6_decontainerize__PP($self);
            my $type := nqp::getattr(pir::perl6_decontainerize__PP($dcself),
                Attribute, '$!type');
            my $package := nqp::getattr(pir::perl6_decontainerize__PP($dcself),
                Attribute, '$!package');
            my $build := nqp::getattr(pir::perl6_decontainerize__PP($dcself),
                Attribute, '$!build_closure');
            pir::perl6_booleanize__PI($type.HOW.archetypes.generic || $package.HOW.archetypes.generic || nqp::defined($build));
        }));
    Attribute.HOW.add_method(Attribute, 'instantiate_generic', static(sub ($self, $type_environment) {
            my $dcself   := pir::perl6_decontainerize__PP($self);
            my $type     := nqp::getattr($dcself, Attribute, '$!type');
            my $cd       := nqp::getattr($dcself, Attribute, '$!container_descriptor');
            my $pkg      := nqp::getattr($dcself, Attribute, '$!package');
            my $avc      := nqp::getattr($dcself, Attribute, '$!auto_viv_container');
            my $bc       := nqp::getattr($dcself, Attribute, '$!build_closure');
            my $ins      := nqp::clone($dcself);
            if $type.HOW.archetypes.generic {
                nqp::bindattr($ins, Attribute, '$!type',
                    $type.HOW.instantiate_generic($type, $type_environment));
                my $cd_ins := $cd.instantiate_generic($type_environment);
                nqp::bindattr($ins, Attribute, '$!container_descriptor', $cd_ins);
                my $avc_var  := pir::perl6_var__PP($avc);
                my $avc_copy := nqp::clone($avc_var);
                my @avc_mro  := $avc_var.HOW.mro($avc_var);
                my $i := 0;
                $i := $i + 1 while @avc_mro[$i].HOW.is_mixin(@avc_mro[$i]);
                nqp::bindattr($ins, Attribute, '$!auto_viv_container',
                    pir::setattribute__0PPsP($avc_copy, @avc_mro[$i], '$!descriptor', $cd_ins));
            }
            if $pkg.HOW.archetypes.generic {
                nqp::bindattr($ins, Attribute, '$!package',
                    $pkg.HOW.instantiate_generic($pkg, $type_environment));
            }
            if nqp::defined($bc) {
                nqp::bindattr($ins, Attribute, '$!build_closure', $bc.clone());
            }
            $ins
        }));
    Attribute.HOW.compose_repr(Attribute);
    
    # class Scalar is Any {
    #     has $!descriptor;
    #     has $!value;
    #     ...
    # }
    Scalar.HOW.add_parent(Scalar, Any);
    Scalar.HOW.add_attribute(Scalar, BOOTSTRAPATTR.new(:name<$!descriptor>, :type(Mu), :package(Scalar)));
    Scalar.HOW.add_attribute(Scalar, BOOTSTRAPATTR.new(:name<$!value>, :type(Mu), :package(Scalar)));
    Scalar.HOW.add_attribute(Scalar, BOOTSTRAPATTR.new(:name<$!whence>, :type(Mu), :package(Scalar)));
    Scalar.HOW.add_method(Scalar, 'is_generic', static(sub ($self) {
        my $dcself := pir::perl6_decontainerize__PP($self);
        nqp::getattr($dcself, Scalar, '$!descriptor').is_generic()
    }));
    Scalar.HOW.add_method(Scalar, 'instantiate_generic', static(sub ($self, $type_environment) {
        my $dcself := pir::perl6_decontainerize__PP($self);
        nqp::bindattr($dcself, Scalar, '$!descriptor',
            nqp::getattr($dcself, Scalar, '$!descriptor').instantiate_generic(
                $type_environment));
        my $val := nqp::getattr($dcself, Scalar, '$!value');
        if $val.HOW.archetypes.generic {
            nqp::bindattr($dcself, Scalar, '$!value',
                $val.HOW.instantiate_generic($val, $type_environment));
        }
        $self
    }));
    Scalar.HOW.compose_repr(Scalar);

    # Scalar needs to be registered as a container type.
    nqp::setcontspec(Scalar, Scalar, '$!value', nqp::null());

    # class Proxy is Any {
    #    has &!FETCH;
    #    has &!STORE;
    #    method FETCH() { ... }
    #    method STORE(\$v) { ... }
    # }
    my $PROXY_FETCH;
    Proxy.HOW.add_parent(Proxy, Any);
    Proxy.HOW.add_attribute(Proxy, BOOTSTRAPATTR.new(:name<&!FETCH>, :type(Mu), :package(Proxy)));
    Proxy.HOW.add_attribute(Proxy, BOOTSTRAPATTR.new(:name<&!STORE>, :type(Mu), :package(Proxy)));
    Proxy.HOW.add_method(Proxy, 'FETCH', ($PROXY_FETCH := static(sub ($cont) {
        pir::perl6_decontainerize__PP(
            nqp::getattr($cont, Proxy, '&!FETCH')(pir::perl6_var__PP($cont)))
    })));
    Proxy.HOW.add_method(Proxy, 'STORE', static(sub ($cont, $val) {
        nqp::getattr($cont, Proxy, '&!STORE')(pir::perl6_var__PP($cont), $val)
    }));
    Proxy.HOW.add_method(Proxy, 'new', static(sub ($type, :$FETCH, :$STORE) {
        my $cont := nqp::create(Proxy);
        nqp::bindattr($cont, Proxy, '&!FETCH', $FETCH);
        nqp::bindattr($cont, Proxy, '&!STORE', $STORE);
        $cont
    }));
    Proxy.HOW.compose(Proxy);
    nqp::setcontspec(Proxy, nqp::null(), '', $PROXY_FETCH);
    Proxy.HOW.compose_repr(Proxy);

    # Helper for creating a scalar attribute. Sets it up as a real Perl 6
    # Attribute instance, complete with container desciptor and auto-viv
    # container.
    sub scalar_attr($name, $type, $package) {
        my $cd := Perl6::Metamodel::ContainerDescriptor.new(
            :of($type), :rw(1), :name($name));
        my $scalar := nqp::create(Scalar);
        nqp::bindattr($scalar, Scalar, '$!descriptor', $cd);
        nqp::bindattr($scalar, Scalar, '$!value', $type);
        return Attribute.new( :name($name), :type($type), :package($package),
            :container_descriptor($cd), :auto_viv_container($scalar));
    }
        
    # class Signature {
    #    has $!params;
    #    has $!returns;
    #    has $!arity;
    #    has $!count;
    #    has $!code;
    #     ... # Uncomposed
    # }
    Signature.HOW.add_parent(Signature, Any);
    Signature.HOW.add_attribute(Signature, BOOTSTRAPATTR.new(:name<$!params>, :type(Mu), :package(Signature)));
    Signature.HOW.add_attribute(Signature, BOOTSTRAPATTR.new(:name<$!returns>, :type(Mu), :package(Signature)));
    Signature.HOW.add_attribute(Signature, BOOTSTRAPATTR.new(:name<$!arity>, :type(Mu), :package(Signature)));
    Signature.HOW.add_attribute(Signature, BOOTSTRAPATTR.new(:name<$!count>, :type(Mu), :package(Signature)));
    Signature.HOW.add_attribute(Signature, BOOTSTRAPATTR.new(:name<$!code>, :type(Mu), :package(Signature)));
    Signature.HOW.add_method(Signature, 'is_generic', static(sub ($self) {
            # If any parameter is generic, so are we.
            my @params := nqp::getattr($self, Signature, '$!params');
            for @params {
                my $is_generic := $_.is_generic();
                if $is_generic { return $is_generic }
            }
            return pir::perl6_booleanize__PI(0);
        }));
    Signature.HOW.add_method(Signature, 'instantiate_generic', static(sub ($self, $type_environment) {
            # Go through parameters, builidng new list. If any
            # are generic, instantiate them. Otherwise leave them
            # as they are.
            my $ins    := nqp::clone($self);
            my @params := nqp::getattr($self, Signature, '$!params');
            my @ins_params;
            for @params {
                if $_.is_generic() {
                    @ins_params.push($_.instantiate_generic($type_environment))
                }
                else {
                    @ins_params.push($_);
                }
            }
            nqp::bindattr($ins, Signature, '$!params', @ins_params);
            $ins
        }));
    Signature.HOW.add_method(Signature, 'set_returns', static(sub ($self, $type) {
            nqp::bindattr(pir::perl6_decontainerize__PP($self),
                Signature, '$!returns', pir::perl6_decontainerize__PP($type));
        }));
    Signature.HOW.add_method(Signature, 'has_returns', static(sub ($self) {
            pir::perl6_booleanize__PI(
                nqp::not_i(
                    nqp::isnull(
                        nqp::getattr(pir::perl6_decontainerize__PP($self),
                            Signature, '$!returns')
                    )
                )
            );
        }));
    Signature.HOW.compose_repr(Signature);
        
    # class Parameter {
    #     has str $!variable_name
    #     has $!named_names
    #     has $!type_captures
    #     has int $!flags
    #     has $!nominal_type
    #     has $!post_constraints
    #     has $!coerce_type
    #     has str $!coerce_method
    #     has $!sub_signature
    #     has $!default_value
    #     has $!container_descriptor;
    #     has $!attr_package;
    #     ... # Uncomposed
    # }
    Parameter.HOW.add_parent(Parameter, Any);
    Parameter.HOW.add_attribute(Parameter, BOOTSTRAPATTR.new(:name<$!variable_name>, :type(str), :package(Parameter)));
    Parameter.HOW.add_attribute(Parameter, BOOTSTRAPATTR.new(:name<$!named_names>, :type(Mu), :package(Parameter)));
    Parameter.HOW.add_attribute(Parameter, BOOTSTRAPATTR.new(:name<$!type_captures>, :type(Mu), :package(Parameter)));
    Parameter.HOW.add_attribute(Parameter, BOOTSTRAPATTR.new(:name<$!flags>, :type(int), :package(Parameter)));
    Parameter.HOW.add_attribute(Parameter, BOOTSTRAPATTR.new(:name<$!nominal_type>, :type(Mu), :package(Parameter)));
    Parameter.HOW.add_attribute(Parameter, BOOTSTRAPATTR.new(:name<$!post_constraints>, :type(Mu), :package(Parameter)));
    Parameter.HOW.add_attribute(Parameter, BOOTSTRAPATTR.new(:name<$!coerce_type>, :type(Mu), :package(Parameter)));
    Parameter.HOW.add_attribute(Parameter, BOOTSTRAPATTR.new(:name<$!coerce_method>, :type(str), :package(Parameter)));
    Parameter.HOW.add_attribute(Parameter, BOOTSTRAPATTR.new(:name<$!sub_signature>, :type(Mu), :package(Parameter)));
    Parameter.HOW.add_attribute(Parameter, BOOTSTRAPATTR.new(:name<$!default_value>, :type(Mu), :package(Parameter)));
    Parameter.HOW.add_attribute(Parameter, BOOTSTRAPATTR.new(:name<$!container_descriptor>, :type(Mu), :package(Parameter)));
    Parameter.HOW.add_attribute(Parameter, BOOTSTRAPATTR.new(:name<$!attr_package>, :type(Mu), :package(Parameter)));
    Parameter.HOW.add_method(Parameter, 'is_generic', static(sub ($self) {
            # If nonimnal type is generic, so are we.
            my $type := nqp::getattr($self, Parameter, '$!nominal_type');
            pir::perl6_booleanize__PI($type.HOW.archetypes.generic)
        }));
    Parameter.HOW.add_method(Parameter, 'instantiate_generic', static(sub ($self, $type_environment) {
            # Clone with the type instantiated.
            my $SIG_ELEM_NOMINAL_GENERIC := 524288;
            my $ins      := nqp::clone($self);
            my $type     := nqp::getattr($self, Parameter, '$!nominal_type');
            my $cd       := nqp::getattr($self, Parameter, '$!container_descriptor');
            my $ins_type := $type.HOW.instantiate_generic($type, $type_environment);
            my $ins_cd   := nqp::isnull($cd) ?? $cd !! $cd.instantiate_generic($type_environment);
            unless $ins_type.HOW.archetypes.generic {
                my $flags := nqp::getattr_i($ins, Parameter, '$!flags');
                if $flags +& $SIG_ELEM_NOMINAL_GENERIC {
                    nqp::bindattr_i($ins, Parameter, '$!flags',
                        $flags - $SIG_ELEM_NOMINAL_GENERIC)
                }
            }
            nqp::bindattr($ins, Parameter, '$!nominal_type', $ins_type);
            nqp::bindattr($ins, Parameter, '$!container_descriptor', $ins_cd);
            $ins
        }));
    Parameter.HOW.add_method(Parameter, 'set_rw', static(sub ($self) {
            my $SIG_ELEM_IS_RW       := 256;
            my $SIG_ELEM_IS_OPTIONAL := 2048;
            my $dcself := pir::perl6_decontainerize__PP($self);
            my $flags  := nqp::getattr_i($dcself, Parameter, '$!flags');
            if $flags +& $SIG_ELEM_IS_OPTIONAL {
                nqp::die("Cannot use 'is rw' on an optional parameter");
            }
            my $cd := nqp::getattr($dcself, Parameter, '$!container_descriptor');
            if $cd { $cd.set_rw(1) }
            nqp::bindattr_i($dcself, Parameter, '$!flags', $flags + $SIG_ELEM_IS_RW);
            $dcself
        }));
    Parameter.HOW.add_method(Parameter, 'set_copy', static(sub ($self) {
            my $SIG_ELEM_IS_COPY := 512;
            my $dcself := pir::perl6_decontainerize__PP($self);
            my $cd     := nqp::getattr($dcself, Parameter, '$!container_descriptor');
            if $cd { $cd.set_rw(1) }
            nqp::bindattr_i($dcself, Parameter, '$!flags',
                nqp::getattr_i($dcself, Parameter, '$!flags') + $SIG_ELEM_IS_COPY);
            $dcself
        }));
    Parameter.HOW.add_method(Parameter, 'set_required', static(sub ($self) {
            my $SIG_ELEM_IS_OPTIONAL := 2048;
            my $dcself := pir::perl6_decontainerize__PP($self);
            my $flags := nqp::getattr_i($dcself, Parameter, '$!flags');
            if $flags +& $SIG_ELEM_IS_OPTIONAL {
                nqp::bindattr_i($dcself, Parameter, '$!flags',
                    $flags - $SIG_ELEM_IS_OPTIONAL);
            }
            $dcself
        }));
    Parameter.HOW.add_method(Parameter, 'set_parcel', static(sub ($self) {
            my $SIG_ELEM_IS_PARCEL := 1024;
            my $dcself := pir::perl6_decontainerize__PP($self);
            my $flags := nqp::getattr_i($dcself, Parameter, '$!flags');
            unless $flags +& $SIG_ELEM_IS_PARCEL {
                nqp::bindattr_i($dcself, Parameter, '$!flags',
                    $flags + $SIG_ELEM_IS_PARCEL);
            }
            $dcself
        }));
    Parameter.HOW.add_method(Parameter, 'set_coercion', static(sub ($self, $type) {
            my $dcself := pir::perl6_decontainerize__PP($self);
            nqp::bindattr_s($dcself, Parameter, '$!coerce_method', $type.HOW.name($type));
            nqp::bindattr($dcself, Parameter, '$!coerce_type', $type);
            $dcself
        }));
    Parameter.HOW.compose_repr(Parameter);
    
    # class Code {
    #     has $!do;                # Low level code object
    #     has $!signature;         # Signature object
    #     ... # Uncomposed
    # }
    Code.HOW.add_parent(Code, Any);
    Code.HOW.add_attribute(Code, BOOTSTRAPATTR.new(:name<$!do>, :type(Mu), :package(Code)));
    Code.HOW.add_attribute(Code, BOOTSTRAPATTR.new(:name<$!signature>, :type(Mu), :package(Code)));

    # Need clone in here, plus generics instantiation.
    Code.HOW.add_method(Code, 'clone', static(sub ($self) {
            my $dcself := pir::perl6_decontainerize__PP($self);
            my $cloned := nqp::clone($dcself);
            nqp::bindattr($cloned, Code, '$!do',
                pir::perl6_associate_sub_code_object__0PP(
                    nqp::clone(nqp::getattr($dcself, Code, '$!do')),
                    $cloned));
            Q:PIR {
                $P0 = find_lex '$dcself'
                $P1 = find_lex 'Code'
                $P0 = getattribute $P0, $P1, '$!do'
                $P1 = getprop 'CLONE_CALLBACK', $P0
                if null $P1 goto no_callback
                $P2 = find_lex '$cloned'
                $P1($P0, $P2)
              no_callback:
            };
            $cloned
        }));
    Code.HOW.add_method(Code, 'is_generic', static(sub ($self) {
            # Delegate to signature, since it contains all the type info.
            my $dc_self := pir::perl6_decontainerize__PP($self);
            nqp::getattr($dc_self, Code, '$!signature').is_generic()
        }));
    Code.HOW.add_method(Code, 'instantiate_generic', static(sub ($self, $type_environment) {
            # Clone the code object, then instantiate the generic signature. Also
            # need to clone dispatchees list.
            my $dcself := pir::perl6_decontainerize__PP($self);
            my $ins := $self.clone();
            if nqp::defined(nqp::getattr($dcself, Routine, '$!dispatchees')) {
                nqp::bindattr($ins, Routine, '$!dispatchees',
                    nqp::clone(nqp::getattr($dcself, Routine, '$!dispatchees')));
            }
            my $sig := nqp::getattr($dcself, Code, '$!signature');
            nqp::bindattr($ins, Code, '$!signature',
                $sig.instantiate_generic($type_environment));
            $ins
        }));
    Code.HOW.add_method(Code, 'name', static(sub ($self) {
            nqp::getcodename(nqp::getattr(pir::perl6_decontainerize__PP($self),
                Code, '$!do'))
        }));
    Code.HOW.add_method(Code, 'set_name', static(sub ($self, $name) {
            nqp::setcodename(
                nqp::getattr(pir::perl6_decontainerize__PP($self), Code, '$!do'),
                $name)
        }));
    Code.HOW.add_method(Code, 'id', static(sub ($self) {
            nqp::where(nqp::getattr(pir::perl6_decontainerize__PP($self),
                Code, '$!do'))
        }));
    Code.HOW.compose_repr(Code);
        
    # Need to actually run the code block. Also need this available before we finish
    # up the stub.
    Code.HOW.add_parrot_vtable_mapping(Code, 'invoke', nqp::null());
    Code.HOW.add_parrot_vtable_handler_mapping(Code, 'invoke', '$!do');

    # class Block is Code { ... }
    Block.HOW.add_parent(Block, Code);
    Block.HOW.add_attribute(Block, BOOTSTRAPATTR.new(:name<$!state_vars>, :type(Mu), :package(Block)));
    Block.HOW.add_attribute(Block, BOOTSTRAPATTR.new(:name<$!phasers>, :type(Mu), :package(Block)));
    Block.HOW.add_method(Block, 'clone', static(sub ($self) {
            my $dcself := pir::perl6_decontainerize__PP($self);
            my $cloned := nqp::clone($dcself);
            nqp::bindattr($cloned, Code, '$!do',
                pir::perl6_associate_sub_code_object__0PP(
                    nqp::clone(nqp::getattr($dcself, Code, '$!do')),
                    $cloned));
            Q:PIR {
                $P0 = find_lex '$dcself'
                $P1 = find_lex 'Code'
                $P0 = getattribute $P0, $P1, '$!do'
                $P1 = getprop 'CLONE_CALLBACK', $P0
                if null $P1 goto no_callback
                $P2 = find_lex '$cloned'
                $P1($P0, $P2)
              no_callback:
            };
            nqp::bindattr($cloned, Block, '$!state_vars', nqp::null());
            $cloned
        }));
    Block.HOW.compose_repr(Block);

    # class Routine is Block { ... }
    Routine.HOW.add_parent(Routine, Block);
    Routine.HOW.add_attribute(Routine, BOOTSTRAPATTR.new(:name<$!dispatchees>, :type(Mu), :package(Routine)));
    Routine.HOW.add_attribute(Routine, BOOTSTRAPATTR.new(:name<$!dispatcher_cache>, :type(Mu), :package(Routine)));
    Routine.HOW.add_attribute(Routine, BOOTSTRAPATTR.new(:name<$!dispatcher>, :type(Mu), :package(Routine)));
    Routine.HOW.add_attribute(Routine, BOOTSTRAPATTR.new(:name<$!md_thunk>, :type(Mu), :package(Routine)));
    Routine.HOW.add_attribute(Routine, BOOTSTRAPATTR.new(:name<$!rw>, :type(int), :package(Routine)));
    Routine.HOW.add_attribute(Routine, BOOTSTRAPATTR.new(:name<$!inline_info>, :type(Mu), :package(Routine)));
    Routine.HOW.add_attribute(Routine, BOOTSTRAPATTR.new(:name<$!yada>, :type(int), :package(Routine)));
    Routine.HOW.add_attribute(Routine, BOOTSTRAPATTR.new(:name<$!package>, :type(Mu), :package(Routine)));
    Routine.HOW.add_attribute(Routine, BOOTSTRAPATTR.new(:name<$!onlystar>, :type(int), :package(Routine)));

    Routine.HOW.add_method(Routine, 'is_dispatcher', static(sub ($self) {
            my $dc_self   := pir::perl6_decontainerize__PP($self);
            my $disp_list := nqp::getattr($dc_self, Routine, '$!dispatchees');
            pir::perl6_booleanize__PI(nqp::defined($disp_list));
        }));
    Routine.HOW.add_method(Routine, 'add_dispatchee', static(sub ($self, $dispatchee) {
            my $dc_self   := pir::perl6_decontainerize__PP($self);
            my $disp_list := nqp::getattr($dc_self, Routine, '$!dispatchees');
            if nqp::defined($disp_list) {
                $disp_list.push($dispatchee);
                nqp::bindattr(pir::perl6_decontainerize__PP($dispatchee),
                    Routine, '$!dispatcher', $dc_self);
                nqp::bindattr($dc_self, Routine, '$!dispatcher_cache', nqp::null());
                $dc_self
            }
            else {
                nqp::die("Cannot add a dispatchee to a non-dispatcher code object");
            }
        }));
    Routine.HOW.add_method(Routine, 'derive_dispatcher', static(sub ($self) {
            my $clone := $self.clone();
            nqp::bindattr($clone, Routine, '$!dispatchees',
                nqp::clone(nqp::getattr($self, Routine, '$!dispatchees')));
            nqp::bindattr($clone, Routine, '$!md_thunk', nqp::null());
            $clone
        }));
    Routine.HOW.add_method(Routine, 'dispatcher', static(sub ($self) {
            nqp::getattr(pir::perl6_decontainerize__PP($self),
                Routine, '$!dispatcher')
        }));
    Routine.HOW.add_method(Routine, 'dispatchees', static(sub ($self) {
            nqp::getattr(pir::perl6_decontainerize__PP($self),
                Routine, '$!dispatchees')
        }));
    Routine.HOW.add_method(Routine, 'set_rw', static(sub ($self) {
            my $dcself := pir::perl6_decontainerize__PP($self);
            nqp::bindattr_i($dcself, Routine, '$!rw', 1);
            $dcself
        }));
    Routine.HOW.add_method(Routine, 'set_inline_info', static(sub ($self, $info) {
            my $dcself := pir::perl6_decontainerize__PP($self);
            nqp::bindattr($dcself, Routine, '$!inline_info', $info);
            $dcself
        }));
    Routine.HOW.add_method(Routine, 'inline_info', static(sub ($self) {
            my $dcself := pir::perl6_decontainerize__PP($self);
            nqp::getattr($dcself, Routine, '$!inline_info')
        }));
    Routine.HOW.add_method(Routine, 'set_onlystar', static(sub ($self) {
            my $dcself := pir::perl6_decontainerize__PP($self);
            nqp::bindattr_i($dcself, Routine, '$!onlystar', 1);
            $dcself
        }));
    Routine.HOW.compose_repr(Routine);

    # class Sub is Routine { ... }
    Sub.HOW.add_parent(Sub, Routine);
    Sub.HOW.compose_repr(Sub);

    # class Method is Routine { ... }
    Method.HOW.add_parent(Method, Routine);
    Method.HOW.compose_repr(Method);

    # class Submethod is Routine { ... }
    Submethod.HOW.add_parent(Submethod, Routine);
    Submethod.HOW.compose_repr(Submethod);

    # class Regex is Method { ... }
    Regex.HOW.add_parent(Regex, Method);
    Regex.HOW.add_attribute(Regex, scalar_attr('$!caps', Mu, Regex));
    Regex.HOW.add_attribute(Regex, scalar_attr('$!nfa', Mu, Regex));
    Regex.HOW.add_attribute(Regex, scalar_attr('$!alt_nfas', Mu, Regex));
    Regex.HOW.add_method(Regex, 'SET_CAPS', static(sub ($self, $caps) {
            nqp::bindattr(pir::perl6_decontainerize__PP($self), Regex, '$!caps', $caps)
        }));
    Regex.HOW.add_method(Regex, 'SET_NFA', static(sub ($self, $nfa) {
            nqp::bindattr(pir::perl6_decontainerize__PP($self), Regex, '$!nfa', $nfa)
        }));
    Regex.HOW.add_method(Regex, 'SET_ALT_NFA', static(sub ($self, str $name, $nfa) {
            my %alts := nqp::getattr(pir::perl6_decontainerize__PP($self), Regex, '$!alt_nfas');
            unless %alts {
                %alts := nqp::hash();
                nqp::bindattr(pir::perl6_decontainerize__PP($self), Regex, '$!alt_nfas', %alts);
            }
            nqp::bindkey(%alts, $name, $nfa);
        }));
    Regex.HOW.add_method(Regex, 'CAPS', static(sub ($self) {
            nqp::getattr(pir::perl6_decontainerize__PP($self), Regex, '$!caps')
        }));
    Regex.HOW.add_method(Regex, 'NFA', static(sub ($self) {
            nqp::getattr(pir::perl6_decontainerize__PP($self), Regex, '$!nfa')
        }));
    Regex.HOW.add_method(Regex, 'ALT_NFA', static(sub ($self, str $name) {
            nqp::atkey(
                nqp::getattr(pir::perl6_decontainerize__PP($self), Regex, '$!alt_nfas'),
                $name)
        }));
    Regex.HOW.compose_repr(Regex);

    # class Str is Cool {
    #     has str $!value is box_target;
    #     ...
    # }
    Str.HOW.add_parent(Str, Cool);
    Str.HOW.add_attribute(Str, BOOTSTRAPATTR.new(:name<$!value>, :type(str), :box_target(1), :package(Str)));
    Str.HOW.set_boolification_mode(Str, 4);
    Str.HOW.publish_boolification_spec(Str);
    Str.HOW.add_parrot_vtable_mapping(Str, 'get_string',
        static(sub ($self) {
            nqp::unbox_s($self)
        }));
    Str.HOW.compose_repr(Str);

    # class Int is Cool {
    #     has int $!value is box_target;
    #     ...
    # }
    Int.HOW.add_parent(Int, Cool);
    Int.HOW.add_attribute(Int, BOOTSTRAPATTR.new(:name<$!value>, :type(bigint), :box_target(1), :package(Int)));
    Int.HOW.set_boolification_mode(Int, 6);
    Int.HOW.publish_boolification_spec(Int);
    Int.HOW.compose_repr(Int);

    # class Num is Cool {
    #     has num $!value is box_target;
    #     ...
    # }
    Num.HOW.add_parent(Num, Cool);
    Num.HOW.add_attribute(Num, BOOTSTRAPATTR.new(:name<$!value>, :type(num), :box_target(1), :package(Num)));
    Num.HOW.set_boolification_mode(Num, 2);
    Num.HOW.publish_boolification_spec(Num);
    Num.HOW.compose_repr(Num);

    # class Parcel is Cool {
    #     ...
    # }
    Parcel.HOW.add_parent(Parcel, Cool);
    Parcel.HOW.add_attribute(Parcel, scalar_attr('$!storage', Mu, Parcel));
    Parcel.HOW.compose_repr(Parcel);

    # class Iterable is Any {
    #     ...
    # }
    Iterable.HOW.add_parent(Iterable, Any);
    Iterable.HOW.compose_repr(Iterable);

    # class Iterator is Iterable {
    #     ...
    # }
    Iterator.HOW.add_parent(Iterator, Iterable);
    Iterator.HOW.compose_repr(Iterator);
    
    # class ListIter is Iterator {
    #     has $!reified;
    #     has $!nextiter;
    #     has $!rest;
    #     has $!list;
    #    ...
    # }
    ListIter.HOW.add_parent(ListIter, Iterator);
    ListIter.HOW.add_attribute(ListIter, scalar_attr('$!reified', Mu, ListIter));
    ListIter.HOW.add_attribute(ListIter, scalar_attr('$!nextiter', Mu, ListIter));
    ListIter.HOW.add_attribute(ListIter, scalar_attr('$!rest', Mu, ListIter));
    ListIter.HOW.add_attribute(ListIter, scalar_attr('$!list', Mu, ListIter));
    ListIter.HOW.compose_repr(ListIter);
    
    # class List is Iterable is Cool {
    #     has $!items;
    #     has $!flattens;
    #     has $!nextiter;
    #     ...
    # }
    List.HOW.add_parent(List, Iterable);
    List.HOW.add_parent(List, Cool);
    List.HOW.add_attribute(List, scalar_attr('$!items', Mu, List));
    List.HOW.add_attribute(List, scalar_attr('$!flattens', Mu, List));
    List.HOW.add_attribute(List, scalar_attr('$!nextiter', Mu, List));
    List.HOW.compose_repr(List);

    # class Array is List {
    #     has $!descriptor;
    #     ...
    # }
    Array.HOW.add_parent(Array, List);
    Array.HOW.add_attribute(Array, BOOTSTRAPATTR.new(:name<$!descriptor>, :type(Mu), :package(Array)));
    Array.HOW.compose_repr(Array);

    # class LoL is List {
    #     has $!descriptor;
    #     ...
    # }
    LoL.HOW.add_parent(LoL, List);
    LoL.HOW.add_attribute(LoL, BOOTSTRAPATTR.new(:name<$!descriptor>, :type(Mu), :package(LoL)));
    LoL.HOW.compose_repr(LoL);

    # my class EnumMap is Iterable is Cool {
    #     has $!storage;
    #     ...
    # }
    EnumMap.HOW.add_parent(EnumMap, Iterable);
    EnumMap.HOW.add_parent(EnumMap, Cool);
    EnumMap.HOW.add_attribute(EnumMap, scalar_attr('$!storage', Mu, EnumMap));
    EnumMap.HOW.compose_repr(EnumMap);

    # my class Hash is EnumMap {
    #     has $!descriptor;
    #     ...
    # }
    Hash.HOW.add_parent(Hash, EnumMap);
    Hash.HOW.add_attribute(Hash, BOOTSTRAPATTR.new(:name<$!descriptor>, :type(Mu), :package(Hash)));
    Hash.HOW.compose_repr(Hash);

    # class Capture {
    #     ...
    # }
    Capture.HOW.add_parent(Capture, Any);
    Capture.HOW.add_attribute(Capture, BOOTSTRAPATTR.new(:name<$!list>, :type(Mu), :package(Capture)));
    Capture.HOW.add_attribute(Capture, BOOTSTRAPATTR.new(:name<$!hash>, :type(Mu), :package(Capture)));
    Capture.HOW.compose_repr(Capture);
    
    # class Bool is Cool {
    #     has int $!value;
    #     ...
    # }
    Bool.HOW.add_parent(Bool, Cool);
    Bool.HOW.add_attribute(Bool, BOOTSTRAPATTR.new(:name<$!value>, :type(int), :box_target(1), :package(Bool)));
    Bool.HOW.set_boolification_mode(Bool, 1);
    Bool.HOW.publish_boolification_spec(Bool);
    Bool.HOW.compose_repr(Bool);

    ObjAt.HOW.add_attribute(ObjAt, BOOTSTRAPATTR.new(:name<$!value>, :type(str), :box_target(1), :package(ObjAt)));
    ObjAt.HOW.compose_repr(ObjAt);

    # Set up Stash type, using a Parrot hash under the hood for storage.
    Stash.HOW.add_parent(Stash, Hash);
    Stash.HOW.add_parrot_vtable_handler_mapping(EnumMap, 'get_pmc_keyed', '$!storage');
    Stash.HOW.add_parrot_vtable_handler_mapping(EnumMap, 'get_pmc_keyed_str', '$!storage');
    Stash.HOW.add_parrot_vtable_handler_mapping(EnumMap, 'set_pmc_keyed', '$!storage');
    Stash.HOW.add_parrot_vtable_handler_mapping(EnumMap, 'set_pmc_keyed_str', '$!storage');
    Stash.HOW.add_parrot_vtable_handler_mapping(EnumMap, 'exists_keyed', '$!storage');
    Stash.HOW.add_parrot_vtable_handler_mapping(EnumMap, 'exists_keyed_str', '$!storage');
    Stash.HOW.add_parrot_vtable_handler_mapping(EnumMap, 'get_iter', '$!storage');
    Stash.HOW.publish_parrot_vtable_handler_mapping(Stash);
    Stash.HOW.compose_repr(Stash);

    # Set this Stash type for the various types of package.
    Perl6::Metamodel::PackageHOW.set_stash_type(Stash, EnumMap);
    Perl6::Metamodel::ModuleHOW.set_stash_type(Stash, EnumMap);
    Perl6::Metamodel::NativeHOW.set_stash_type(Stash, EnumMap);
    Perl6::Metamodel::ClassHOW.set_stash_type(Stash, EnumMap);
    Perl6::Metamodel::GrammarHOW.set_stash_type(Stash, EnumMap);
    Perl6::Metamodel::ParametricRoleHOW.set_stash_type(Stash, EnumMap);
    Perl6::Metamodel::ParametricRoleGroupHOW.set_stash_type(Stash, EnumMap);

    # Give everything we've set up so far a Stash.
    Perl6::Metamodel::ClassHOW.add_stash(Mu);
    Perl6::Metamodel::ClassHOW.add_stash(Any);
    Perl6::Metamodel::ClassHOW.add_stash(Cool);
    Perl6::Metamodel::ClassHOW.add_stash(Attribute);
    Perl6::Metamodel::ClassHOW.add_stash(Signature);
    Perl6::Metamodel::ClassHOW.add_stash(Parameter);
    Perl6::Metamodel::ClassHOW.add_stash(Code);
    Perl6::Metamodel::ClassHOW.add_stash(Block);
    Perl6::Metamodel::ClassHOW.add_stash(Routine);
    Perl6::Metamodel::ClassHOW.add_stash(Sub);
    Perl6::Metamodel::ClassHOW.add_stash(Method);
    Perl6::Metamodel::ClassHOW.add_stash(Str);
    Perl6::Metamodel::ClassHOW.add_stash(Int);
    Perl6::Metamodel::ClassHOW.add_stash(Num);
    Perl6::Metamodel::ClassHOW.add_stash(Scalar);
    Perl6::Metamodel::ClassHOW.add_stash(Bool);
    Perl6::Metamodel::ClassHOW.add_stash(Stash);
    Perl6::Metamodel::ClassHOW.add_stash(List);
    Perl6::Metamodel::ClassHOW.add_stash(Array);
    Perl6::Metamodel::ClassHOW.add_stash(Hash);
    Perl6::Metamodel::ClassHOW.add_stash(ObjAt);

    # Make Parrot invoke v-table construct a capture and delegate off
    # to postcircumfix:<( )>.
    my $invoke_forwarder :=
        static(sub ($self, *@pos, *%named) {
            if !nqp::isconcrete($self) && !nqp::can($self, 'postcircumfix:<( )>') {
                my $coercer_name := $self.HOW.name($self);
                if +@pos == 1 {
                    @pos[0]."$coercer_name"()
                }
                else {
                    my $parcel := nqp::create(Parcel);
                    nqp::bindattr($parcel, Parcel, '$!storage', @pos);
                    $parcel."$coercer_name"()
                }
            }
            else {
                my $c := nqp::create(Capture);
                nqp::bindattr($c, Capture, '$!list', @pos);
                nqp::bindattr($c, Capture, '$!hash', %named);
                $self.postcircumfix:<( )>($c);
            }
        });
    Mu.HOW.add_parrot_vtable_mapping(Mu, 'invoke', $invoke_forwarder);

    # If we don't already have a PROCESS, set it up.
    my $PROCESS;
    my $hll_ns := pir::get_root_global__PS('perl6');
    if !nqp::isnull($hll_ns) && nqp::existskey($hll_ns, 'PROCESS') {
        $PROCESS := $hll_ns<PROCESS>;
    }
    else {
        PROCESS.HOW.compose(PROCESS);
        Perl6::Metamodel::ModuleHOW.add_stash(PROCESS);
        $PROCESS := PROCESS;
        pir::set_root_global__2PsP(['perl6'], 'PROCESS', $PROCESS);
    }

    # Bool::False and Bool::True.
    my $false := nqp::create(Bool);
    nqp::bindattr_i($false, Bool, '$!value', 0);
    (Bool.WHO)<False> := $false;
    my $true := nqp::create(Bool);
    nqp::bindattr_i($true, Bool, '$!value', 1);
    (Bool.WHO)<True> := $true;

    # Setup some regexy/grammary bits.
    Perl6::Metamodel::ClassHOW.add_stash(Grammar);
    Grammar.HOW.compose_repr(Grammar);

    # Export the metamodel bits to a Metamodel namespace so it's available
    # from user land.
    Perl6::Metamodel::PackageHOW.add_stash(Metamodel);
    for Perl6::Metamodel.WHO {
        (Metamodel.WHO){$_.key} := $_.value;
    }

    # Fill out EXPORT namespace.
    EXPORT::DEFAULT.WHO<Mu>        := Mu;
    EXPORT::DEFAULT.WHO<Any>       := Any;
    EXPORT::DEFAULT.WHO<Cool>      := Cool;
    EXPORT::DEFAULT.WHO<Attribute> := Attribute;
    EXPORT::DEFAULT.WHO<Signature> := Signature;
    EXPORT::DEFAULT.WHO<Parameter> := Parameter;
    EXPORT::DEFAULT.WHO<Code>      := Code;
    EXPORT::DEFAULT.WHO<Block>     := Block;
    EXPORT::DEFAULT.WHO<Routine>   := Routine;
    EXPORT::DEFAULT.WHO<Sub>       := Sub;
    EXPORT::DEFAULT.WHO<Method>    := Method;
    EXPORT::DEFAULT.WHO<Submethod> := Submethod;
    EXPORT::DEFAULT.WHO<Regex>     := Regex;
    EXPORT::DEFAULT.WHO<Str>       := Str;
    EXPORT::DEFAULT.WHO<Int>       := Int;
    EXPORT::DEFAULT.WHO<Num>       := Num;
    EXPORT::DEFAULT.WHO<Parcel>    := Parcel;  
    EXPORT::DEFAULT.WHO<Iterable>  := Iterable;
    EXPORT::DEFAULT.WHO<Iterator>  := Iterator;
    EXPORT::DEFAULT.WHO<ListIter>  := ListIter;
    EXPORT::DEFAULT.WHO<List>      := List;
    EXPORT::DEFAULT.WHO<Array>     := Array;
    EXPORT::DEFAULT.WHO<LoL>       := LoL;
    EXPORT::DEFAULT.WHO<EnumMap>   := EnumMap;
    EXPORT::DEFAULT.WHO<Hash>      := Hash;
    EXPORT::DEFAULT.WHO<Capture>   := Capture;
    EXPORT::DEFAULT.WHO<ObjAt>     := ObjAt;
    EXPORT::DEFAULT.WHO<Stash>     := Stash;
    EXPORT::DEFAULT.WHO<Scalar>    := Scalar;
    EXPORT::DEFAULT.WHO<Proxy>     := Proxy;
    EXPORT::DEFAULT.WHO<Grammar>   := Grammar;
    EXPORT::DEFAULT.WHO<PROCESS>   := $PROCESS;
    EXPORT::DEFAULT.WHO<Bool>      := Bool;
    EXPORT::DEFAULT.WHO<False>     := $false;
    EXPORT::DEFAULT.WHO<True>      := $true;
    EXPORT::DEFAULT.WHO<ContainerDescriptor> := Perl6::Metamodel::ContainerDescriptor;
    EXPORT::DEFAULT.WHO<MethodDispatcher>    := Perl6::Metamodel::MethodDispatcher;
    EXPORT::DEFAULT.WHO<MultiDispatcher>     := Perl6::Metamodel::MultiDispatcher;
    EXPORT::DEFAULT.WHO<WrapDispatcher>      := Perl6::Metamodel::WrapDispatcher;
    EXPORT::DEFAULT.WHO<StaticLexPad>        := Perl6::Metamodel::StaticLexPad;
    EXPORT::DEFAULT.WHO<Metamodel>           := Metamodel;
}
EXPORT::DEFAULT.WHO<NQPCursorRole> := NQPCursorRole;

# Publish Parrot v-table handler mappings.
Mu.HOW.publish_parrot_vtable_mapping(Mu);
Attribute.HOW.publish_parrot_vtable_mapping(Attribute);
Code.HOW.publish_parrot_vtable_handler_mapping(Code);
Code.HOW.publish_parrot_vtable_mapping(Code);
Block.HOW.publish_parrot_vtable_handler_mapping(Block);
Block.HOW.publish_parrot_vtable_mapping(Block);
Routine.HOW.publish_parrot_vtable_handler_mapping(Routine);
Routine.HOW.publish_parrot_vtable_mapping(Routine);
Sub.HOW.publish_parrot_vtable_handler_mapping(Sub);
Sub.HOW.publish_parrot_vtable_mapping(Sub);
Method.HOW.publish_parrot_vtable_handler_mapping(Method);
Method.HOW.publish_parrot_vtable_mapping(Method);
Submethod.HOW.publish_parrot_vtable_handler_mapping(Submethod);
Submethod.HOW.publish_parrot_vtable_mapping(Submethod);
Regex.HOW.publish_parrot_vtable_handler_mapping(Regex);
Regex.HOW.publish_parrot_vtable_mapping(Regex);
Stash.HOW.publish_parrot_vtable_handler_mapping(Stash);
Str.HOW.publish_parrot_vtable_handler_mapping(Str);

# Set up various type mappings.
pir::perl6_set_type_packagehow__vP(Perl6::Metamodel::PackageHOW);
pir::perl6_set_types_mu_any__vPP(Mu, Any);
pir::perl6_set_type_code__vP(Code);
pir::perl6_set_type_routine__vP(Routine);
pir::perl6_set_types_ins__vPPP(Int, Num, Str);
pir::perl6_set_types_list_array_lol__vPPPPP(List, ListIter, Array, LoL, Parcel);
pir::perl6_set_types_enummap_hash__vPP(EnumMap, Hash);
pir::perl6_set_type_capture__vP(Capture);
pir::perl6_set_bools__vPP(Bool.WHO<False>, Bool.WHO<True>);
pir::set_scalar_container_type__vP(Scalar);

# We'll build container descriptors for $_, $! and $/ that we can
# share with all of the magically/lazily created scalars.
my $topic_cd := Perl6::Metamodel::ContainerDescriptor.new(
    :of(Mu), :rw(1), :name('$_'));
my $error_cd := Perl6::Metamodel::ContainerDescriptor.new(
    :of(Mu), :rw(1), :name('$!'));
my $match_cd := Perl6::Metamodel::ContainerDescriptor.new(
    :of(Mu), :rw(1), :name('$/'));
pir::new__PsP('Perl6LexPad', hash()).configure_magicals(
    $topic_cd, $error_cd, $match_cd, Scalar, Any, EnumMap, Hash, Block);

# Tell parametric role groups how to create a dispatcher.
Perl6::Metamodel::ParametricRoleGroupHOW.set_selector_creator({
    my $sel := nqp::create(Sub);
    my $onlystar := sub (*@pos, *%named) {
        pir::perl6_enter_multi_dispatch_from_onlystar_block__P();
    };
    pir::perl6_associate_sub_code_object__vPP($onlystar, $sel);
    nqp::bindattr($sel, Code, '$!do', $onlystar);
    nqp::bindattr($sel, Routine, '$!dispatchees', []);
    $sel
});

# Roles pretend to be narrower than certain types for the purpose
# of type checking. Also, they pun to classes.
Perl6::Metamodel::ParametricRoleGroupHOW.pretend_to_be([Cool, Any, Mu]);
Perl6::Metamodel::ParametricRoleGroupHOW.configure_punning(
    Perl6::Metamodel::ClassHOW,
    hash( ACCEPTS => Mu ));
Perl6::Metamodel::ParametricRoleHOW.pretend_to_be([Cool, Any, Mu]);
Perl6::Metamodel::ParametricRoleHOW.configure_punning(
    Perl6::Metamodel::ClassHOW,
    hash( ACCEPTS => Mu ));
Perl6::Metamodel::CurriedRoleHOW.pretend_to_be([Cool, Any, Mu]);
Perl6::Metamodel::CurriedRoleHOW.configure_punning(
    Perl6::Metamodel::ClassHOW,
    hash( ACCEPTS => Mu ));
    
# Similar for packages and modules, but just has methods from Any.
Perl6::Metamodel::PackageHOW.pretend_to_be([Any, Mu]);
Perl6::Metamodel::PackageHOW.delegate_methods_to(Any);
Perl6::Metamodel::ModuleHOW.pretend_to_be([Any, Mu]);
Perl6::Metamodel::ModuleHOW.delegate_methods_to(Any);

# Let ClassHOW know about the invocation handler.
Perl6::Metamodel::ClassHOW.set_invoke_forwarder(
        Mu.HOW.parrot_vtable_mappings(Mu, :local(1))<invoke>);

# Set this Stash type for the various types of package (not persisted as it ends
# up in a lexical...)
Perl6::Metamodel::PackageHOW.set_stash_type(Stash, EnumMap);
Perl6::Metamodel::ModuleHOW.set_stash_type(Stash, EnumMap);
Perl6::Metamodel::NativeHOW.set_stash_type(Stash, EnumMap);
Perl6::Metamodel::ClassHOW.set_stash_type(Stash, EnumMap);
Perl6::Metamodel::GrammarHOW.set_stash_type(Stash, EnumMap);
Perl6::Metamodel::ParametricRoleHOW.set_stash_type(Stash, EnumMap);
Perl6::Metamodel::ParametricRoleGroupHOW.set_stash_type(Stash, EnumMap);

# Register default parent types.
Perl6::Metamodel::ClassHOW.set_default_parent_type(Any);
Perl6::Metamodel::GrammarHOW.set_default_parent_type(Grammar);

# Put PROCESS in place.
my $hll_ns := pir::get_root_global__PS('perl6');
$hll_ns<PROCESS> := PROCESS;
