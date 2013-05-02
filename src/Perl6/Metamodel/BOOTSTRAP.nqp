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

# Bootstrapping Attribute class that we eventually replace with the real
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
    method positional_delegate() { 0 }
    method associative_delegate() { 0 }
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
my stub Junction metaclass Perl6::Metamodel::ClassHOW { ... };
my stub Metamodel metaclass Perl6::Metamodel::PackageHOW { ... };
my stub ForeignCode metaclass Perl6::Metamodel::ClassHOW { ... };

# We stick all the declarative bits inside of a BEGIN, so they get
# serialized.
BEGIN {
#?if parrot
    # Ensure Rakudo dynops are initialized.
    pir::rakudo_dynop_setup__v();
#?endif

    # class Mu { ... }
#?if parrot
    Mu.HOW.add_parrot_vtable_mapping(Mu, 'get_integer',
        nqp::getstaticcode(sub ($self) {
            nqp::unbox_i($self.Int())
        }));
    Mu.HOW.add_parrot_vtable_mapping(Mu, 'get_number',
        nqp::getstaticcode(sub ($self) {
            nqp::unbox_n($self.Num())
        }));
    Mu.HOW.add_parrot_vtable_mapping(Mu, 'get_string',
        nqp::getstaticcode(sub ($self) {
            nqp::unbox_s($self.Str())
        }));
    Mu.HOW.add_parrot_vtable_mapping(Mu, 'defined',
        nqp::getstaticcode(sub ($self) { nqp::istrue($self.defined()) }));
#?endif
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
    #     has int $!positional_delegate;
    #     has int $!associative_delegate;
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
    Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(:name<$!positional_delegate>, :type(int), :package(Attribute)));
    Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(:name<$!associative_delegate>, :type(int), :package(Attribute)));

    # Need new and accessor methods for Attribute in here for now.
    Attribute.HOW.add_method(Attribute, 'new',
        nqp::getstaticcode(sub ($self, :$name!, :$type!, :$package!, :$has_accessor,
                :$positional_delegate = 0, :$associative_delegate = 0, *%other) {
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
            nqp::bindattr_i($attr, Attribute, '$!positional_delegate', $positional_delegate);
            nqp::bindattr_i($attr, Attribute, '$!associative_delegate', $associative_delegate);
            $attr
        }));
    Attribute.HOW.add_method(Attribute, 'name', nqp::getstaticcode(sub ($self) {
            nqp::getattr_s(nqp::decont($self),
                Attribute, '$!name');
        }));
    Attribute.HOW.add_method(Attribute, 'type', nqp::getstaticcode(sub ($self) {
            nqp::getattr(nqp::decont($self),
                Attribute, '$!type');
        }));
    Attribute.HOW.add_method(Attribute, 'container_descriptor', nqp::getstaticcode(sub ($self) {
            nqp::getattr(nqp::decont($self),
                Attribute, '$!container_descriptor');
        }));
    Attribute.HOW.add_method(Attribute, 'auto_viv_container', nqp::getstaticcode(sub ($self) {
            nqp::getattr(nqp::decont($self),
                Attribute, '$!auto_viv_container');
        }));
    Attribute.HOW.add_method(Attribute, 'has_accessor', nqp::getstaticcode(sub ($self) {
            nqp::p6bool(nqp::getattr_i(nqp::decont($self),
                Attribute, '$!has_accessor'));
        }));
    Attribute.HOW.add_method(Attribute, 'rw', nqp::getstaticcode(sub ($self) {
            nqp::p6bool(nqp::getattr_i(nqp::decont($self),
                Attribute, '$!rw'));
        }));
    Attribute.HOW.add_method(Attribute, 'set_rw', nqp::getstaticcode(sub ($self) {
            nqp::bindattr_i(nqp::decont($self),
                Attribute, '$!rw', 1);
            nqp::p6bool(1)
        }));
    Attribute.HOW.add_method(Attribute, 'set_readonly', nqp::getstaticcode(sub ($self) {
            nqp::bindattr_i(nqp::decont($self),
                Attribute, '$!ro', 1);
            nqp::p6bool(1)
        }));
    Attribute.HOW.add_method(Attribute, 'default_to_rw', nqp::getstaticcode(sub ($self) {
            my $dcself := nqp::decont($self);
            unless nqp::getattr_i($dcself, Attribute, '$!ro') {
                nqp::bindattr_i($dcself, Attribute, '$!rw', 1);
            }
            nqp::p6bool(1)
        }));
    Attribute.HOW.add_method(Attribute, 'set_build', nqp::getstaticcode(sub ($self, $closure) {
            nqp::bindattr(nqp::decont($self), Attribute, '$!build_closure', $closure);
            $self
        }));
    Attribute.HOW.add_method(Attribute, 'build', nqp::getstaticcode(sub ($self) {
            nqp::getattr(nqp::decont($self),
                Attribute, '$!build_closure');
        }));
    Attribute.HOW.add_method(Attribute, 'set_box_target', nqp::getstaticcode(sub ($self) {
            nqp::bindattr_i(nqp::decont($self),
                Attribute, '$!box_target', 1);
            nqp::p6bool(1)
        }));
    Attribute.HOW.add_method(Attribute, 'box_target', nqp::getstaticcode(sub ($self) {
            nqp::getattr_i(nqp::decont($self),
                Attribute, '$!box_target')
        }));
    Attribute.HOW.add_method(Attribute, 'positional_delegate', nqp::getstaticcode(sub ($self) {
            nqp::getattr_i(nqp::decont($self), Attribute, '$!positional_delegate');
        }));
    Attribute.HOW.add_method(Attribute, 'associative_delegate', nqp::getstaticcode(sub ($self) {
            nqp::getattr_i(nqp::decont($self), Attribute, '$!associative_delegate')
        }));
    Attribute.HOW.add_method(Attribute, 'is_generic', nqp::getstaticcode(sub ($self) {
            my $dcself   := nqp::decont($self);
            my $type := nqp::getattr(nqp::decont($dcself),
                Attribute, '$!type');
            my $package := nqp::getattr(nqp::decont($dcself),
                Attribute, '$!package');
            my $build := nqp::getattr(nqp::decont($dcself),
                Attribute, '$!build_closure');
            nqp::p6bool($type.HOW.archetypes.generic || $package.HOW.archetypes.generic || nqp::defined($build));
        }));
    Attribute.HOW.add_method(Attribute, 'instantiate_generic', nqp::getstaticcode(sub ($self, $type_environment) {
            my $dcself   := nqp::decont($self);
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
                my $avc_var  := nqp::p6var($avc);
                my $avc_copy := nqp::clone($avc_var);
                my @avc_mro  := $avc_var.HOW.mro($avc_var);
                my $i := 0;
                $i := $i + 1 while @avc_mro[$i].HOW.is_mixin(@avc_mro[$i]);
                nqp::bindattr($avc_copy, @avc_mro[$i], '$!descriptor', $cd_ins);
                nqp::bindattr($ins, Attribute, '$!auto_viv_container', $avc_copy);
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
    Scalar.HOW.add_method(Scalar, 'is_generic', nqp::getstaticcode(sub ($self) {
        my $dcself := nqp::decont($self);
        nqp::getattr($dcself, Scalar, '$!descriptor').is_generic()
    }));
    Scalar.HOW.add_method(Scalar, 'instantiate_generic', nqp::getstaticcode(sub ($self, $type_environment) {
        my $dcself := nqp::decont($self);
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
    nqp::setcontspec(Scalar, 'rakudo_scalar', nqp::null());

    # class Proxy is Any {
    #    has &!FETCH;
    #    has &!STORE;
    #    method FETCH() { ... }
    #    method STORE(\$v) { ... }
    # }
    my $PROXY_FETCH;
    my $PROXY_STORE;
    Proxy.HOW.add_parent(Proxy, Any);
    Proxy.HOW.add_attribute(Proxy, BOOTSTRAPATTR.new(:name<&!FETCH>, :type(Mu), :package(Proxy)));
    Proxy.HOW.add_attribute(Proxy, BOOTSTRAPATTR.new(:name<&!STORE>, :type(Mu), :package(Proxy)));
    Proxy.HOW.add_method(Proxy, 'FETCH', ($PROXY_FETCH := nqp::getstaticcode(sub ($cont) {
        nqp::decont(
            nqp::getattr($cont, Proxy, '&!FETCH')(nqp::p6var($cont)))
    })));
    Proxy.HOW.add_method(Proxy, 'STORE', ($PROXY_STORE := nqp::getstaticcode(sub ($cont, $val) {
        nqp::getattr($cont, Proxy, '&!STORE')(nqp::p6var($cont), $val)
    })));
    Proxy.HOW.add_method(Proxy, 'new', nqp::getstaticcode(sub ($type, :$FETCH, :$STORE) {
        my $cont := nqp::create(Proxy);
        nqp::bindattr($cont, Proxy, '&!FETCH', $FETCH);
        nqp::bindattr($cont, Proxy, '&!STORE', $STORE);
        $cont
    }));
    Proxy.HOW.compose(Proxy);
    nqp::setcontspec(Proxy, 'code_pair', nqp::hash(
        'fetch', $PROXY_FETCH,
        'store', $PROXY_STORE
    ));
    Proxy.HOW.compose_repr(Proxy);

    # Helper for creating a scalar attribute. Sets it up as a real Perl 6
    # Attribute instance, complete with container desciptor and auto-viv
    # container.
    sub scalar_attr($name, $type, $package, :$associative_delegate) {
        my $cd := Perl6::Metamodel::ContainerDescriptor.new(
            :of($type), :rw(1), :name($name));
        my $scalar := nqp::create(Scalar);
        nqp::bindattr($scalar, Scalar, '$!descriptor', $cd);
        nqp::bindattr($scalar, Scalar, '$!value', $type);
        return Attribute.new( :name($name), :type($type), :package($package),
            :container_descriptor($cd), :auto_viv_container($scalar),
            :$associative_delegate);
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
    Signature.HOW.add_method(Signature, 'is_generic', nqp::getstaticcode(sub ($self) {
            # If any parameter is generic, so are we.
            my @params := nqp::getattr($self, Signature, '$!params');
            for @params {
                my $is_generic := $_.is_generic();
                if $is_generic { return $is_generic }
            }
            return nqp::p6bool(0);
        }));
    Signature.HOW.add_method(Signature, 'instantiate_generic', nqp::getstaticcode(sub ($self, $type_environment) {
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
    Signature.HOW.add_method(Signature, 'set_returns', nqp::getstaticcode(sub ($self, $type) {
            nqp::bindattr(nqp::decont($self),
                Signature, '$!returns', nqp::decont($type));
        }));
    Signature.HOW.add_method(Signature, 'has_returns', nqp::getstaticcode(sub ($self) {
            nqp::p6bool(
                nqp::not_i(
                    nqp::isnull(
                        nqp::getattr(nqp::decont($self),
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
    Parameter.HOW.add_method(Parameter, 'is_generic', nqp::getstaticcode(sub ($self) {
            # If nonimnal type is generic, so are we.
            my $type := nqp::getattr($self, Parameter, '$!nominal_type');
            nqp::p6bool($type.HOW.archetypes.generic)
        }));
    Parameter.HOW.add_method(Parameter, 'instantiate_generic', nqp::getstaticcode(sub ($self, $type_environment) {
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
    Parameter.HOW.add_method(Parameter, 'set_rw', nqp::getstaticcode(sub ($self) {
            my $SIG_ELEM_IS_RW       := 256;
            my $SIG_ELEM_IS_OPTIONAL := 2048;
            my $dcself := nqp::decont($self);
            my $flags  := nqp::getattr_i($dcself, Parameter, '$!flags');
            if $flags +& $SIG_ELEM_IS_OPTIONAL {
                nqp::die("Cannot use 'is rw' on an optional parameter");
            }
            my $cd := nqp::getattr($dcself, Parameter, '$!container_descriptor');
            if nqp::defined($cd) { $cd.set_rw(1) }
            nqp::bindattr_i($dcself, Parameter, '$!flags', $flags + $SIG_ELEM_IS_RW);
            nqp::p6bool(nqp::defined($dcself));
        }));
    Parameter.HOW.add_method(Parameter, 'set_copy', nqp::getstaticcode(sub ($self) {
            my $SIG_ELEM_IS_COPY := 512;
            my $dcself := nqp::decont($self);
            my $cd     := nqp::getattr($dcself, Parameter, '$!container_descriptor');
            if nqp::defined($cd) { $cd.set_rw(1) }
            nqp::bindattr_i($dcself, Parameter, '$!flags',
                nqp::getattr_i($dcself, Parameter, '$!flags') + $SIG_ELEM_IS_COPY);
            $dcself
        }));
    Parameter.HOW.add_method(Parameter, 'set_required', nqp::getstaticcode(sub ($self) {
            my $SIG_ELEM_IS_OPTIONAL := 2048;
            my $dcself := nqp::decont($self);
            my $flags := nqp::getattr_i($dcself, Parameter, '$!flags');
            if $flags +& $SIG_ELEM_IS_OPTIONAL {
                nqp::bindattr_i($dcself, Parameter, '$!flags',
                    $flags - $SIG_ELEM_IS_OPTIONAL);
            }
            $dcself
        }));
    Parameter.HOW.add_method(Parameter, 'set_parcel', nqp::getstaticcode(sub ($self) {
            my $SIG_ELEM_IS_PARCEL := 1024;
            my $dcself := nqp::decont($self);
            my $flags := nqp::getattr_i($dcself, Parameter, '$!flags');
            unless $flags +& $SIG_ELEM_IS_PARCEL {
                nqp::bindattr_i($dcself, Parameter, '$!flags',
                    $flags + $SIG_ELEM_IS_PARCEL);
            }
            $dcself
        }));
    Parameter.HOW.add_method(Parameter, 'set_coercion', nqp::getstaticcode(sub ($self, $type) {
            my $dcself := nqp::decont($self);
            nqp::bindattr_s($dcself, Parameter, '$!coerce_method', $type.HOW.name($type));
            nqp::bindattr($dcself, Parameter, '$!coerce_type', $type);
            $dcself
        }));
    Parameter.HOW.compose_repr(Parameter);
    
    # class Code {
    #     has $!do;                # Low level code object
    #     has $!signature;         # Signature object
    #     has $!compstuff;         # Place for the compiler to hang stuff
    #     ... # Uncomposed
    # }
    Code.HOW.add_parent(Code, Any);
    Code.HOW.add_attribute(Code, BOOTSTRAPATTR.new(:name<$!do>, :type(Mu), :package(Code)));
    Code.HOW.add_attribute(Code, BOOTSTRAPATTR.new(:name<$!signature>, :type(Mu), :package(Code)));
    Code.HOW.add_attribute(Code, BOOTSTRAPATTR.new(:name<$!compstuff>, :type(Mu), :package(Code)));

    # Need clone in here, plus generics instantiation.
    Code.HOW.add_method(Code, 'clone', nqp::getstaticcode(sub ($self) {
            my $dcself    := nqp::decont($self);
            my $cloned    := nqp::clone($dcself);
            my $do        := nqp::getattr($dcself, Code, '$!do');
            my $do_cloned := nqp::clone($do);
            nqp::bindattr($cloned, Code, '$!do', $do_cloned);
            nqp::setcodeobj($do_cloned, $cloned);
            my $compstuff := nqp::getattr($dcself, Code, '$!compstuff');
            unless nqp::isnull($compstuff) {
                $compstuff[2]($do, $cloned);
            }
            $cloned
        }));
    Code.HOW.add_method(Code, 'is_generic', nqp::getstaticcode(sub ($self) {
            # Delegate to signature, since it contains all the type info.
            my $dc_self := nqp::decont($self);
            nqp::getattr($dc_self, Code, '$!signature').is_generic()
        }));
    Code.HOW.add_method(Code, 'instantiate_generic', nqp::getstaticcode(sub ($self, $type_environment) {
            # Clone the code object, then instantiate the generic signature. Also
            # need to clone dispatchees list.
            my $dcself := nqp::decont($self);
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
    Code.HOW.add_method(Code, 'name', nqp::getstaticcode(sub ($self) {
            nqp::getcodename(nqp::getattr(nqp::decont($self),
                Code, '$!do'))
        }));
    Code.HOW.add_method(Code, 'set_name', nqp::getstaticcode(sub ($self, $name) {
            nqp::setcodename(
                nqp::getattr(nqp::decont($self), Code, '$!do'),
                $name)
        }));
    Code.HOW.add_method(Code, 'id', nqp::getstaticcode(sub ($self) {
            nqp::where(nqp::getattr(nqp::decont($self),
                Code, '$!do'))
        }));
    Code.HOW.compose_repr(Code);
        
    # Need to actually run the code block. Also need this available before we finish
    # up the stub.
    Code.HOW.set_invocation_attr(Code, 'invoke', '$!do');
    Code.HOW.compose_invocation(Code);

    # class Block is Code { ... }
    Block.HOW.add_parent(Block, Code);
    Block.HOW.add_attribute(Block, BOOTSTRAPATTR.new(:name<$!state_vars>, :type(Mu), :package(Block)));
    Block.HOW.add_attribute(Block, BOOTSTRAPATTR.new(:name<$!phasers>, :type(Mu), :package(Block)));
    Block.HOW.add_method(Block, 'clone', nqp::getstaticcode(sub ($self) {
            my $dcself    := nqp::decont($self);
            my $cloned    := nqp::clone($dcself);
            my $do        := nqp::getattr($dcself, Code, '$!do');
            my $do_cloned := nqp::clone($do);
            nqp::bindattr($cloned, Code, '$!do', $do_cloned);
            nqp::setcodeobj($do_cloned, $cloned);
            my $compstuff := nqp::getattr($dcself, Code, '$!compstuff');
            unless nqp::isnull($compstuff) {
                $compstuff[2]($do, $cloned);
            }
            nqp::bindattr($cloned, Block, '$!state_vars', nqp::null());
            $cloned
        }));
    Block.HOW.compose_repr(Block);
    Block.HOW.compose_invocation(Block);

    # class Routine is Block { ... }
    Routine.HOW.add_parent(Routine, Block);
    Routine.HOW.add_attribute(Routine, BOOTSTRAPATTR.new(:name<$!dispatchees>, :type(Mu), :package(Routine)));
    Routine.HOW.add_attribute(Routine, BOOTSTRAPATTR.new(:name<$!dispatcher_cache>, :type(Mu), :package(Routine)));
    Routine.HOW.add_attribute(Routine, BOOTSTRAPATTR.new(:name<$!dispatcher>, :type(Mu), :package(Routine)));
    Routine.HOW.add_attribute(Routine, BOOTSTRAPATTR.new(:name<$!rw>, :type(int), :package(Routine)));
    Routine.HOW.add_attribute(Routine, BOOTSTRAPATTR.new(:name<$!inline_info>, :type(Mu), :package(Routine)));
    Routine.HOW.add_attribute(Routine, BOOTSTRAPATTR.new(:name<$!yada>, :type(int), :package(Routine)));
    Routine.HOW.add_attribute(Routine, BOOTSTRAPATTR.new(:name<$!package>, :type(Mu), :package(Routine)));
    Routine.HOW.add_attribute(Routine, BOOTSTRAPATTR.new(:name<$!onlystar>, :type(int), :package(Routine)));
    Routine.HOW.add_attribute(Routine, BOOTSTRAPATTR.new(:name<$!dispatch_order>, :type(Mu), :package(Routine)));
    Routine.HOW.add_attribute(Routine, BOOTSTRAPATTR.new(:name<$!dispatch_cache>, :type(Mu), :package(Routine)));
    
    Routine.HOW.add_method(Routine, 'is_dispatcher', nqp::getstaticcode(sub ($self) {
            my $dc_self   := nqp::decont($self);
            my $disp_list := nqp::getattr($dc_self, Routine, '$!dispatchees');
            nqp::p6bool(nqp::defined($disp_list));
        }));
    Routine.HOW.add_method(Routine, 'add_dispatchee', nqp::getstaticcode(sub ($self, $dispatchee) {
            my $dc_self   := nqp::decont($self);
            my $disp_list := nqp::getattr($dc_self, Routine, '$!dispatchees');
            if nqp::defined($disp_list) {
                $disp_list.push($dispatchee);
                nqp::bindattr(nqp::decont($dispatchee),
                    Routine, '$!dispatcher', $dc_self);
                nqp::scwbdisable();
                nqp::bindattr($dc_self, Routine, '$!dispatch_order', nqp::null());
                nqp::bindattr($dc_self, Routine, '$!dispatch_cache', nqp::null());
                nqp::bindattr($dc_self, Routine, '$!dispatcher_cache', nqp::null());
                nqp::scwbenable();
                $dc_self
            }
            else {
                nqp::die("Cannot add a dispatchee to a non-dispatcher code object");
            }
        }));
    Routine.HOW.add_method(Routine, 'derive_dispatcher', nqp::getstaticcode(sub ($self) {
            my $clone := $self.clone();
            nqp::bindattr($clone, Routine, '$!dispatchees',
                nqp::clone(nqp::getattr($self, Routine, '$!dispatchees')));
            $clone
        }));
    Routine.HOW.add_method(Routine, 'dispatcher', nqp::getstaticcode(sub ($self) {
            nqp::getattr(nqp::decont($self),
                Routine, '$!dispatcher')
        }));
    Routine.HOW.add_method(Routine, 'dispatchees', nqp::getstaticcode(sub ($self) {
            nqp::getattr(nqp::decont($self),
                Routine, '$!dispatchees')
        }));
    Routine.HOW.add_method(Routine, '!sort_dispatchees_internal', nqp::getstaticcode(sub ($self) {
            my int $SLURPY_ARITY      := nqp::bitshiftl_i(1, 30);
            my int $EDGE_REMOVAL_TODO := -1;
            my int $EDGE_REMOVED      := -2;
            my int $DEFCON_NONE       := 0;
            my int $DEFCON_DEFINED    := 1;
            my int $DEFCON_UNDEFINED  := 2;
            my int $DEFCON_MASK       := $DEFCON_DEFINED +| $DEFCON_UNDEFINED;
            my int $TYPE_NATIVE_INT   := 4;
            my int $TYPE_NATIVE_NUM   := 8;
            my int $TYPE_NATIVE_STR   := 16;
            my int $TYPE_NATIVE_MASK  := $TYPE_NATIVE_INT +| $TYPE_NATIVE_NUM +| $TYPE_NATIVE_STR;

            my int $SIG_ELEM_SLURPY_POS          := 8;
            my int $SIG_ELEM_SLURPY_NAMED        := 16;
            my int $SIG_ELEM_MULTI_INVOCANT      := 128;
            my int $SIG_ELEM_IS_OPTIONAL         := 2048;
            my int $SIG_ELEM_IS_CAPTURE          := 32768;
            my int $SIG_ELEM_UNDEFINED_ONLY      := 65536;
            my int $SIG_ELEM_DEFINED_ONLY        := 131072;
            my int $SIG_ELEM_NOMINAL_GENERIC     := 524288;
            my int $SIG_ELEM_NATIVE_INT_VALUE    := 2097152;
            my int $SIG_ELEM_NATIVE_NUM_VALUE    := 4194304;
            my int $SIG_ELEM_NATIVE_STR_VALUE    := 8388608;
            
            # Takes two candidates and determines if the first one is narrower than the
            # second. Returns a true value if they are.
            sub is_narrower(%a, %b) {
                # Work out how many parameters to compare, factoring in slurpiness
                # and optionals.
                my int $types_to_check;
                if %a<num_types> == %b<num_types> {
                    $types_to_check := %a<num_types>;
                }
                elsif %a<min_arity> == %b<min_arity> {
                    $types_to_check := %a<num_types> > %b<num_types>
                        ?? %b<num_types>
                        !! %a<num_types>;
                }
                elsif %a<max_arity> != $SLURPY_ARITY && %b<max_arity> == $SLURPY_ARITY {
                    return 1;
                }
                else {
                    return 0;
                }

                # Analyse each parameter in the two candidates.
                my int $i := 0;
                my int $narrower := 0;
                my int $tied := 0;
                while $i < $types_to_check {
                    my $type_obj_a := %a<types>[$i];
                    my $type_obj_b := %b<types>[$i];
                    if nqp::eqaddr($type_obj_a, $type_obj_b) {
                        # Same type; narrower if first has constraints and other doesn't;
                        # tied if neither has constraints or both have constraints. */
                        if %a<constraints>[$i] && !%b<constraints>[$i] {
                            $narrower++;
                        }
                        elsif (!%a<constraints>[$i] && !%b<constraints>[$i])
                           || (%a<constraints>[$i] && %b<constraints>[$i]) {
                            $tied++;
                        }
                    }
                    elsif (%a<type_flags>[$i] +& $TYPE_NATIVE_MASK)
                      && !(%b<type_flags>[$i] +& $TYPE_NATIVE_MASK) {
                        # Narrower because natives always are.
                        $narrower++;
                    }
                    elsif (%b<type_flags>[$i] +& $TYPE_NATIVE_MASK)
                      && !(%a<type_flags>[$i] +& $TYPE_NATIVE_MASK) {
                        # Wider; skip over here so we don't go counting this as tied in
                        # the next branch.
                    }
                    else {
                        if nqp::istype($type_obj_a, $type_obj_b) {
                            # Narrower - note it and we're done.
                            $narrower++;
                        }
                        else {
                            # Make sure it's tied, rather than the other way around.
                            unless nqp::istype($type_obj_b, $type_obj_a) {
                                $tied++;
                            }
                        }
                    }
                    $i++;
                }

                # If one is narrower than the other from current analysis, we're done.
                if $narrower >= 1 && $narrower + $tied == $types_to_check {
                    return 1;
                }

                # If they aren't tied, we're also done.
                elsif $tied != $types_to_check {
                    return 0;
                }
                
                # Otherwise, we see if one has a slurpy and the other not. A lack of
                # slurpiness makes the candidate narrower.
                if %a<max_arity> != $SLURPY_ARITY && %b<max_arity> == $SLURPY_ARITY {
                    return 1;
                }

                # Also narrower if the first needs a bind check and the second doesn't, if
                # we wouldn't deem the other one narrower than this one int terms of
                # slurpyness. Otherwise, they're tied.
                return !(%b<max_arity> != $SLURPY_ARITY && %a<max_arity> == $SLURPY_ARITY)
                    && (%a<bind_check> && !%b<bind_check>);
            }
            
            my $dcself     := nqp::decont($self);
            my @candidates := nqp::getattr($dcself, Routine, '$!dispatchees');
            
            # Create a node for each candidate in the graph.
            my @graph;
            for @candidates -> $candidate {
                # Get hold of signature.
                my $sig    := nqp::getattr($candidate, Code, '$!signature');
                my @params := nqp::getattr($sig, Signature, '$!params');

                # Create it an entry.
                my %info := nqp::hash(
                    'sub',          $candidate,
                    'signature',    $sig,
                    'types',        [],
                    'type_flags',   nqp::list_i(),
                    'constraints',  [],
                    'min_arity',    0,
                    'max_arity',    0,
                    'num_types',    0,
                );
                my int $significant_param := 0;
                for @params -> $param {
                    # If it's a required named (and not slurpy) don't need its type info
                    # but we will need a bindability check during the dispatch for it.
                    my int $flags   := nqp::getattr_i($param, Parameter, '$!flags');
                    my $named_names := nqp::getattr($param, Parameter, '$!named_names');
                    unless nqp::isnull($named_names) {
                        if !($flags +& $SIG_ELEM_IS_OPTIONAL) {
                            if nqp::elems($named_names) == 1 {
                                %info<req_named> := nqp::atpos_s($named_names, 0);
                            }
                        }
                        %info<bind_check> := 1;
                        next;
                    }

                    # If it's got a sub-signature, also need a bind check.
                    unless nqp::isnull(nqp::getattr($param, Parameter, '$!sub_signature')) {
                        %info<bind_check> := 1;
                    }

                    # If it's named slurpy, we're done, also we don't need a bind
                    # check on account of nameds since we take them all.
                    if $flags +& $SIG_ELEM_SLURPY_NAMED {
                        last;
                    }

                    # Otherwise, positional or slurpy and contributes to arity.
                    if $flags +& $SIG_ELEM_SLURPY_POS || $flags +& $SIG_ELEM_IS_CAPTURE {
                        %info<max_arity> := $SLURPY_ARITY;
                        last;
                    }
                    elsif $flags +& $SIG_ELEM_IS_OPTIONAL {
                        %info<max_arity>++;
                    }
                    else {
                        %info<max_arity>++;
                        %info<min_arity>++;
                    }

                    # Record type info for this parameter.
                    if $flags +& $SIG_ELEM_NOMINAL_GENERIC {
                        %info<bind_check> := 1;
                        %info<types>[$significant_param] := Any;
                    }
                    else {
                        %info<types>[$significant_param] :=
                            nqp::getattr($param, Parameter, '$!nominal_type');
                    }
                    unless nqp::isnull(nqp::getattr($param, Parameter, '$!post_constraints')) {
                        %info<constraints>[$significant_param] := 1;
                        %info<bind_check> := 1;
                    }
                    if $flags +& $SIG_ELEM_MULTI_INVOCANT {
                        %info<num_types>++;
                    }
                    if $flags +& $SIG_ELEM_DEFINED_ONLY {
                        nqp::bindpos_i(%info<type_flags>, $significant_param, $DEFCON_DEFINED);
                    }
                    elsif $flags +& $SIG_ELEM_UNDEFINED_ONLY {
                        nqp::bindpos_i(%info<type_flags>, $significant_param, $DEFCON_UNDEFINED);
                    }
                    if $flags +& $SIG_ELEM_NATIVE_INT_VALUE {
                        nqp::bindpos_i(%info<type_flags>, $significant_param,
                            $TYPE_NATIVE_INT + nqp::atpos_i(%info<type_flags>, $significant_param));
                    }
                    elsif $flags +& $SIG_ELEM_NATIVE_NUM_VALUE {
                        nqp::bindpos_i(%info<type_flags>, $significant_param,
                            $TYPE_NATIVE_NUM + nqp::atpos_i(%info<type_flags>, $significant_param));
                    }
                    elsif $flags +& $SIG_ELEM_NATIVE_STR_VALUE {
                        nqp::bindpos_i(%info<type_flags>, $significant_param,
                            $TYPE_NATIVE_STR + nqp::atpos_i(%info<type_flags>, $significant_param));
                    }
                    $significant_param++;
                }

                # Add it to graph node, and initialize list of edges.
                nqp::push(@graph, nqp::hash(
                    'info',      %info,
                    'edges',     [],
                    'edges_in',  0,
                    'edges_out', 0
                ));
            }

            # Now analyze type narrowness of the candidates relative to each other
            # and create the edges.
            my int $i := 0;
            my int $j;
            my int $n := nqp::elems(@candidates);
            while $i < $n {
                $j := 0;
                while $j < $n {
                    unless $i == $j {
                        if is_narrower(@graph[$i]<info>, @graph[$j]<info>) {
                            @graph[$i]<edges>[@graph[$i]<edges_out>] := @graph[$j];
                            @graph[$i]<edges_out>++;
                            @graph[$j]<edges_in>++;
                        }
                    }
                    $j++;
                }
                $i++;
            }

            # Perform the topological sort.
            my int $candidates_to_sort := nqp::elems(@candidates);
            my @result;
            while $candidates_to_sort > 0 {
                my int $rem_results := nqp::elems(@result);

                # Find any nodes that have no incoming edges and add them to
                # results.
                $i := 0;
                while $i < $n {
                    if @graph[$i]<edges_in> == 0 {
                        # Add to results.
                        nqp::push(@result, @graph[$i]<info>);
                        $candidates_to_sort--;
                        @graph[$i]<edges_in> := $EDGE_REMOVAL_TODO;
                    }
                    $i++;
                }
                if $rem_results == nqp::elems(@result) {
                    nqp::die("Circularity detected in multi sub types");
                }

                # Now we need to decrement edges in counts for things that had
                # edges from candidates we added here.
                $i := 0;
                while $i < $n {
                    if @graph[$i]<edges_in> == $EDGE_REMOVAL_TODO {
                        $j := 0;
                        while $j < @graph[$i]<edges_out> {
                            @graph[$i]<edges>[$j]<edges_in>--;
                            $j++;
                        }
                        @graph[$i]<edges_in> := $EDGE_REMOVED;
                    }
                    $i++;
                }

                # This is end of a tied group, so leave a gap.
                nqp::push(@result, Mu);
            }
            
            # Add final null sentinel.
            nqp::push(@result, Mu);

            @result
        }));
    Routine.HOW.add_method(Routine, 'sort_dispatchees', nqp::getstaticcode(sub ($self) {
        my $dcself := nqp::decont($self);
        unless nqp::isnull(nqp::getattr($dcself, Routine, '$!dispatch_order')) {
            nqp::bindattr($dcself, Routine, '$!dispatch_order',
                $self.'!sort_dispatchees_internal'());
        }
    }));
    Routine.HOW.add_method(Routine, 'find_best_dispatchee', nqp::getstaticcode(sub ($self, $capture, int $many = 0) {        
            my int $DEFCON_DEFINED    := 1;
            my int $DEFCON_UNDEFINED  := 2;
            my int $DEFCON_MASK       := $DEFCON_DEFINED +| $DEFCON_UNDEFINED;
            my int $TYPE_NATIVE_INT   := 4;
            my int $TYPE_NATIVE_NUM   := 8;
            my int $TYPE_NATIVE_STR   := 16;
            my int $TYPE_NATIVE_MASK  := $TYPE_NATIVE_INT +| $TYPE_NATIVE_NUM +| $TYPE_NATIVE_STR;
            my int $BIND_VAL_OBJ      := 0;
            my int $BIND_VAL_INT      := 1;
            my int $BIND_VAL_NUM      := 2;
            my int $BIND_VAL_STR      := 3;
            
            # Count arguments.
            my int $num_args := nqp::captureposelems($capture);

            # Get list and number of candidates, triggering a sort if there are none.
            my $dcself := nqp::decont($self);
            my @candidates := nqp::getattr($dcself, Routine, '$!dispatch_order');
            if nqp::isnull(@candidates) {
                nqp::scwbdisable();
                @candidates := $dcself.'!sort_dispatchees_internal'();
                nqp::bindattr($dcself, Routine, '$!dispatch_order', @candidates);
                nqp::scwbenable();
            }
            my $num_candidates := nqp::elems(@candidates);

            # Iterate over the candidates and collect best ones; terminate
            # when we see two type objects (indicating end).
            my int $cur_idx := 0;
            my $cur_candidate;
            my int $type_check_count;
            my int $type_mismatch;
            my int $i;
            my int $pure_type_result := 1;
            my $many_res := $many ?? [] !! Mu;
            my @possibles;
            my int $done := 0;
            until $done {
                $cur_candidate := nqp::atpos(@candidates, $cur_idx);

                if nqp::isconcrete($cur_candidate) {
                    # Check if it's admissable by arity.
                    unless $num_args < nqp::atkey($cur_candidate, 'min_arity')
                    || $num_args > nqp::atkey($cur_candidate, 'max_arity') {
                        # Arity OK; now check if it's admissable by type.
                        $type_check_count := nqp::atkey($cur_candidate, 'num_types') > $num_args
                            ?? $num_args
                            !! nqp::atkey($cur_candidate, 'num_types');
                        $type_mismatch := 0;

                        $i := 0;
                        while $i < $type_check_count && !$type_mismatch {
                            my $type_obj     := nqp::atpos(nqp::atkey($cur_candidate, 'types'), $i);
                            my $type_flags   := nqp::atpos_i(nqp::atkey($cur_candidate, 'type_flags'), $i);
                            my int $got_prim := nqp::captureposprimspec($capture, $i);
                            if $type_flags +& $TYPE_NATIVE_MASK {
                                # Looking for a natively typed value. Did we get one?
                                if $got_prim == $BIND_VAL_OBJ {
                                    # Object; won't do.
                                    $type_mismatch := 1;
                                }
                                elsif (($type_flags +& $TYPE_NATIVE_INT) && $got_prim != $BIND_VAL_INT) ||
                                   (($type_flags +& $TYPE_NATIVE_NUM) && $got_prim != $BIND_VAL_NUM) ||
                                   (($type_flags +& $TYPE_NATIVE_STR) && $got_prim != $BIND_VAL_STR) {
                                    # Mismatch.
                                    $type_mismatch := 1;
                                }
                            }
                            else {
                                my $param;
                                if $got_prim == $BIND_VAL_OBJ {
                                    $param := nqp::hllizefor(
                                        nqp::captureposarg($capture, $i),
                                        'perl6');
                                }
                                else {
                                    $param := $got_prim == $BIND_VAL_INT ?? Int !!
                                              $got_prim == $BIND_VAL_NUM ?? Num !!
                                                                            Str;
                                }
                                unless nqp::eqaddr($type_obj, Mu) || nqp::istype($param, $type_obj) {
                                    $type_mismatch := 1;
                                }
                                if !$type_mismatch && $type_flags +& $DEFCON_MASK {
                                    my int $defined := $got_prim != $BIND_VAL_OBJ || nqp::isconcrete($param);
                                    my int $desired := $type_flags +& $DEFCON_MASK;
                                    if ($defined && $desired == $DEFCON_UNDEFINED) ||
                                       (!$defined && $desired == $DEFCON_DEFINED) {
                                        $type_mismatch := 1;
                                    }
                                }
                            }
                            $i++;
                        }
                        
                        unless $type_mismatch {
                            # It's an admissable candidate; add to list.
                            nqp::push(@possibles, $cur_candidate);
                        }
                    }

                    $cur_idx++;
                } else {
                    # We've hit the end of a tied group now. If any of them have a
                    # bindability check requirement, we'll do any of those now.
                    if nqp::elems(@possibles) {
                        my $new_possibles;
                        my %info;
                        $i := 0;
                        while $i < nqp::elems(@possibles) {
                            %info := nqp::atpos(@possibles, $i);
                            
                            # First, if there's a required named parameter and it was
                            # not passed, we can very quickly eliminate this candidate
                            # without doing a full bindability check.
                            if nqp::existskey(%info, 'req_named')
                            && !nqp::captureexistsnamed($capture, nqp::atkey(%info, 'req_named')) {
                                # Required named arg not passed, so we eliminate
                                # it right here. Flag that we've built a list of
                                # new possibles, and that this was not a pure
                                # type-based result that we can cache.
                                $new_possibles := [] unless nqp::islist($new_possibles);
                            }

                            # Otherwise, may need full bind check.
                            elsif nqp::existskey(%info, 'bind_check') {
                                my $sub := nqp::atkey(%info, 'sub');
                                my $cs := nqp::getattr($sub, Code, '$!compstuff');
                                unless nqp::isnull($cs) {
                                    # We need to do the tie-break on something not yet compiled.
                                    # Get it compiled.
                                    my $ctf := $cs[1];
                                    $ctf() if $ctf;
                                }
                                
                                # Since we had to do a bindability check, this is not
                                # a result we can cache on nominal type.
                                $pure_type_result := 0;
                                
                                # If we haven't got a possibles storage space, allocate it now.
                                $new_possibles := [] unless nqp::islist($new_possibles);
                                
                                my $sig := nqp::getattr($sub, Code, '$!signature');
                                if nqp::p6isbindable($sig, $capture) {
                                    nqp::push($new_possibles, nqp::atpos(@possibles, $i));
                                    unless $many {
                                        # Terminate the loop.
                                        $i := nqp::elems(@possibles);
                                    }
                                }
                            }
                            
                            # Otherwise, it's just nominal; accept it.
                            elsif $new_possibles {
                                nqp::push($new_possibles, nqp::atpos(@possibles, $i));
                            }
                            else {
                                $new_possibles := [nqp::atpos(@possibles, $i)];
                            }
                            $i++;
                        }

                        # If we have an updated list of possibles, use this
                        # new one from here on in.
                        if nqp::islist($new_possibles) {
                            @possibles := $new_possibles;
                        }
                    }

                    # Now we have eliminated any that fail the bindability check.
                    # See if we need to push it onto the many list and continue.
                    # Otherwise, we have the result we were looking for.
                    if $many {
                        while @possibles {
                            nqp::push($many_res, nqp::atkey(nqp::shift(@possibles), 'sub'))
                        }
                        $cur_idx++;
                        unless nqp::isconcrete(nqp::atpos(@candidates, $cur_idx)) {
                            $done := 1;
                        }
                    }
                    elsif @possibles {
                        $done := 1;
                    }
                    else {
                        # Keep looping and looking, unless we really hit the end.
                        $cur_idx++;
                        unless nqp::isconcrete(nqp::atpos(@candidates, $cur_idx)) {
                            $done := 1;
                        }
                    }
                }
            }
            
            # If we were looking for many candidates, we're done now.
            if $many {
                return $many_res;
            }

            # Check is default trait if we still have multiple options and we want one.
            if nqp::elems(@possibles) > 1 {
                # Locate any default candidates; if we find multiple defaults, this is
                # no help, so we'll not bother collecting just which ones are good.
                my $default_cand;
                for @possibles {
                    my $sub := nqp::atkey($_, 'sub');
                    if nqp::can($sub, 'default') && $sub.default {
                        if nqp::isconcrete($default_cand) {
                            $default_cand := Mu;
                        }
                        else {
                            $default_cand := $_;
                        }
                    }
                }
                if nqp::isconcrete($default_cand) {
                    nqp::pop(@possibles) while @possibles;
                    @possibles[0] := $default_cand;
                }
            }

            # If we're at a single candidate here, and we also know there's no
            # type constraints that follow, we can cache the result.
            if nqp::elems(@possibles) == 1 && $pure_type_result {
                unless nqp::capturehasnameds($capture) {
                    nqp::scwbdisable();
                    nqp::bindattr($dcself, Routine, '$!dispatch_cache',
                        nqp::multicacheadd(
                            nqp::getattr($dcself, Routine, '$!dispatch_cache'),
                            $capture,
                            nqp::atkey(nqp::atpos(@possibles, 0), 'sub')));
                    nqp::scwbenable();
                }
            }

            # Perhaps we found nothing but have junctional arguments?
            my $junctional_res;
            if nqp::elems(@possibles) == 0 {
                my int $has_junc_args := 0;
                $i := 0;
                while $i < $num_args {
                    if !nqp::captureposprimspec($capture, $i) {
                        if nqp::istype(nqp::captureposarg($capture, $i), Junction) {
                            $has_junc_args := 1;
                        }
                    }
                    $i++;
                }
                if $has_junc_args {
                    $junctional_res := -> *@pos, *%named {
                        Junction.AUTOTHREAD($self, |@pos, |%named)
                    }
                }
            }

            # Need a unique candidate.
            if nqp::elems(@possibles) == 1 {
                nqp::atkey(nqp::atpos(@possibles, 0), 'sub')
            }
            elsif nqp::isconcrete($junctional_res) {
                $junctional_res;
            }
            elsif nqp::elems(@possibles) == 0 {
                my %ex := nqp::gethllsym('perl6', 'P6EX');
                if nqp::isnull(%ex) || !nqp::existskey(%ex, 'X::Multi::NoMatch') {
                    nqp::die("Cannot call " ~ $self.name() ~
                        "; no signatures match");
                }
                else {
                    nqp::atkey(%ex, 'X::Multi::NoMatch')($self)
                }
            }
            else {
                my %ex := nqp::gethllsym('perl6', 'P6EX');
                if nqp::isnull(%ex) || !nqp::existskey(%ex, 'X::Multi::Ambiguous') {
                    nqp::die("Ambiguous call to " ~ $self.name());
                }
                else {
                    my @ambig;
                    for @possibles {
                        nqp::push(@ambig, $_<sub>);
                    }
                    nqp::atkey(%ex, 'X::Multi::Ambiguous')($self, @ambig)
                }
            }
        }));
    Routine.HOW.add_method(Routine, 'analyze_dispatch', nqp::getstaticcode(sub ($self, @args, @flags) {
            # Compile time dispatch result.
            my $MD_CT_NOT_SURE :=  0;  # Needs a runtime dispatch.
            my $MD_CT_DECIDED  :=  1;  # Worked it out; see result.
            my $MD_CT_NO_WAY   := -1;  # Proved it'd never manage to dispatch.
            
            # Other constants we need.
            my int $DEFCON_DEFINED    := 1;
            my int $DEFCON_UNDEFINED  := 2;
            my int $DEFCON_MASK       := $DEFCON_DEFINED +| $DEFCON_UNDEFINED;
            my int $TYPE_NATIVE_INT   := 4;
            my int $TYPE_NATIVE_NUM   := 8;
            my int $TYPE_NATIVE_STR   := 16;
            my int $TYPE_NATIVE_MASK  := $TYPE_NATIVE_INT +| $TYPE_NATIVE_NUM +| $TYPE_NATIVE_STR;
            my int $BIND_VAL_OBJ      := 0;
            my int $BIND_VAL_INT      := 1;
            my int $BIND_VAL_NUM      := 2;
            my int $BIND_VAL_STR      := 3;
            
            # Count arguments.
            my int $num_args := nqp::elems(@args);
            
            # Get list and number of candidates, triggering a sort if there are none.
            my $dcself := nqp::decont($self);
            my @candidates := nqp::getattr($dcself, Routine, '$!dispatch_order');
            if nqp::isnull(@candidates) {
                nqp::scwbdisable();
                @candidates := $dcself.'!sort_dispatchees_internal'();
                nqp::bindattr($dcself, Routine, '$!dispatch_order', @candidates);
                nqp::scwbenable();
            }
            my $num_candidates := nqp::elems(@candidates);
            
            # Look through the candidates. If we see anything that needs a bind
            # check or a definedness check, we can't decide it at compile time,
            # so bail out immediately.
            my int $all_native     := 1;
            my int $cur_idx        := 0;
            my int $seen_all       := 0;
            my int $arity_possible := 0;
            my int $type_possible  := 0;
            my int $used_defcon;
            my int $type_mismatch;
            my int $type_check_count;
            my int $type_match_possible;
            my int $i;
            my $cur_candidate;
            my $cur_result;
            while 1 {
                $cur_candidate := nqp::atpos(@candidates, $cur_idx);
                $used_defcon := 0;

                # Did we reach the end of a tied group? If so, note we can only
                # consider the narrowest group, *unless* they are all natively
                # typed candidates in which case we can look a bit further.
                # We also exit if we found something.
                unless nqp::isconcrete($cur_candidate) {
                    $cur_idx++;
                    if nqp::isconcrete(nqp::atpos(@candidates, $cur_idx))
                    && $all_native && !nqp::isconcrete($cur_result) {
                        next;
                    }
                    else {
                        $seen_all := !nqp::isconcrete(nqp::atpos(@candidates, $cur_idx));
                        last;
                    }
                }

                # Check if it's admissable by arity.
                if $num_args < nqp::atkey($cur_candidate, 'min_arity')
                || $num_args > nqp::atkey($cur_candidate, 'max_arity') {
                    $cur_idx++;
                    next;
                }
                
                # If we got this far, something at least matched on arity.
                $arity_possible := 1;

                # Check if it's admissable by type.
                $type_check_count := nqp::atkey($cur_candidate, 'num_types') > $num_args
                    ?? $num_args
                    !! nqp::atkey($cur_candidate, 'num_types');
                $type_mismatch := 0;
                $type_match_possible := 1;
                $i := 0;
                while $i < $type_check_count {
                    my $type_obj     := nqp::atpos(nqp::atkey($cur_candidate, 'types'), $i);
                    my $type_flags   := nqp::atpos_i(nqp::atkey($cur_candidate, 'type_flags'), $i);
                    my int $got_prim := nqp::atpos(@flags, $i);
                    if $type_flags +& $TYPE_NATIVE_MASK {
                        # Looking for a natively typed value. Did we get one?
                        if $got_prim == $BIND_VAL_OBJ {
                            # Object; won't do.
                            $type_mismatch := 1;
                            last;
                        }
                        if (($type_flags +& $TYPE_NATIVE_INT) && $got_prim != $BIND_VAL_INT)
                        || (($type_flags +& $TYPE_NATIVE_NUM) && $got_prim != $BIND_VAL_NUM)
                        || (($type_flags +& $TYPE_NATIVE_STR) && $got_prim != $BIND_VAL_STR) {
                            # Mismatch.
                            $type_mismatch := 1;
                            $type_match_possible := 0;
                            last;
                        }
                    }
                    else {
                        # Work out parameter.
                        my $param :=
                            $got_prim == $BIND_VAL_OBJ ?? nqp::atpos(@args, $i) !!
                            $got_prim == $BIND_VAL_INT ?? Int !!
                            $got_prim == $BIND_VAL_NUM ?? Num !!
                                                          Str;
                
                        # If we're here, it's a non-native.
                        $all_native := 0;
                        
                        # Check type. If that doesn't rule it out, then check if it's
                        # got definedness constraints. If it does, note that; if we
                        # match but depend on definedness constraints we can't do
                        # any more.
                        if !nqp::eqaddr($type_obj, Mu) && !nqp::istype($param, $type_obj) {
                            $type_mismatch := 1;
                            
                            # We didn't match, but that doesn't mean we cannot at
                            # runtime (e.g. the most we know about the type could
                            # be that it's Any, but at runtime that feasibly could
                            # be Int). In some cases we never could though (Str
                            # passed to an Int parameter).
                            if !nqp::istype($type_obj, $param) {
                                $type_match_possible := 0;
                            }
                        }
                        elsif $type_flags +& $DEFCON_MASK {
                            $used_defcon := 1;
                        }
                    }
                    $i++;
                }
                if $type_match_possible {
                    $type_possible := 1;
                }
                if $type_mismatch {
                    $cur_idx++;
                    next;
                }
                if ($used_defcon) {
                    return [$MD_CT_NOT_SURE, NQPMu];
                }

                # If it's possible but needs a bind check, we're not going to be
                # able to decide it. */
                if nqp::existskey($cur_candidate, 'bind_check') {
                    return [$MD_CT_NOT_SURE, NQPMu];
                }

                # If we get here, it's the result. Well, unless we already had one,
                # in which case we're in bother 'cus we don't know how to disambiguate
                # at compile time.
                if nqp::isconcrete($cur_result) {
                    return [$MD_CT_NOT_SURE, NQPMu];
                }
                else {
                    $cur_result := nqp::atkey($cur_candidate, 'sub');
                    $cur_idx++;
                }
            }
            
            # If we saw all the candidates, and got no result, and the arity never
            # matched or when it did there was no way any candidates could get
            # passed matching types, then we know it would never work.
            if $seen_all && (!$arity_possible || !$type_possible) && !nqp::isconcrete($cur_result) {
                # Ensure no junctional args before we flag the failure.
                for @args {
                    if nqp::istype($_, Junction) {
                        return [$MD_CT_NOT_SURE, NQPMu];
                    }
                }
                return [$MD_CT_NO_WAY, NQPMu];
            }
            
            # If we got a result, return it.
            if nqp::isconcrete($cur_result) {
                return [$MD_CT_DECIDED, $cur_result];
            }

            # Otherwise, dunno...we'll have to find out at runtime.
            return [$MD_CT_NOT_SURE, NQPMu];
        }));
    Routine.HOW.add_method(Routine, 'set_rw', nqp::getstaticcode(sub ($self) {
            my $dcself := nqp::decont($self);
            nqp::bindattr_i($dcself, Routine, '$!rw', 1);
            $dcself
        }));
    Routine.HOW.add_method(Routine, 'set_inline_info', nqp::getstaticcode(sub ($self, $info) {
            my $dcself := nqp::decont($self);
            nqp::bindattr($dcself, Routine, '$!inline_info', $info);
            $dcself
        }));
    Routine.HOW.add_method(Routine, 'inline_info', nqp::getstaticcode(sub ($self) {
            my $dcself := nqp::decont($self);
            nqp::getattr($dcself, Routine, '$!inline_info')
        }));
    Routine.HOW.add_method(Routine, 'set_onlystar', nqp::getstaticcode(sub ($self) {
            my $dcself := nqp::decont($self);
            nqp::bindattr_i($dcself, Routine, '$!onlystar', 1);
            $dcself
        }));
    Routine.HOW.compose_repr(Routine);
    Routine.HOW.compose_invocation(Routine);

    # class Sub is Routine { ... }
    Sub.HOW.add_parent(Sub, Routine);
    Sub.HOW.compose_repr(Sub);
    Sub.HOW.compose_invocation(Sub);

    # class Method is Routine { ... }
    Method.HOW.add_parent(Method, Routine);
    Method.HOW.compose_repr(Method);
    Method.HOW.compose_invocation(Method);

    # class Submethod is Routine { ... }
    Submethod.HOW.add_parent(Submethod, Routine);
    Submethod.HOW.compose_repr(Submethod);
    Submethod.HOW.compose_invocation(Submethod);

    # class Regex is Method { ... }
    Regex.HOW.add_parent(Regex, Method);
    Regex.HOW.add_attribute(Regex, scalar_attr('$!caps', Mu, Regex));
    Regex.HOW.add_attribute(Regex, scalar_attr('$!nfa', Mu, Regex));
    Regex.HOW.add_attribute(Regex, scalar_attr('$!alt_nfas', Mu, Regex));
    Regex.HOW.add_method(Regex, 'SET_CAPS', nqp::getstaticcode(sub ($self, $caps) {
            nqp::bindattr(nqp::decont($self), Regex, '$!caps', $caps)
        }));
    Regex.HOW.add_method(Regex, 'SET_NFA', nqp::getstaticcode(sub ($self, $nfa) {
            nqp::bindattr(nqp::decont($self), Regex, '$!nfa', $nfa)
        }));
    Regex.HOW.add_method(Regex, 'SET_ALT_NFA', nqp::getstaticcode(sub ($self, str $name, $nfa) {
            my %alts := nqp::getattr(nqp::decont($self), Regex, '$!alt_nfas');
            unless %alts {
                %alts := nqp::hash();
                nqp::bindattr(nqp::decont($self), Regex, '$!alt_nfas', %alts);
            }
            nqp::bindkey(%alts, $name, $nfa);
        }));
    Regex.HOW.add_method(Regex, 'CAPS', nqp::getstaticcode(sub ($self) {
            nqp::getattr(nqp::decont($self), Regex, '$!caps')
        }));
    Regex.HOW.add_method(Regex, 'NFA', nqp::getstaticcode(sub ($self) {
            nqp::getattr(nqp::decont($self), Regex, '$!nfa')
        }));
    Regex.HOW.add_method(Regex, 'ALT_NFA', nqp::getstaticcode(sub ($self, str $name) {
            nqp::atkey(
                nqp::getattr(nqp::decont($self), Regex, '$!alt_nfas'),
                $name)
        }));
    Regex.HOW.compose_repr(Regex);
    Regex.HOW.compose_invocation(Regex);

    # class Str is Cool {
    #     has str $!value is box_target;
    #     ...
    # }
    Str.HOW.add_parent(Str, Cool);
    Str.HOW.add_attribute(Str, BOOTSTRAPATTR.new(:name<$!value>, :type(str), :box_target(1), :package(Str)));
    Str.HOW.set_boolification_mode(Str, 4);
    Str.HOW.publish_boolification_spec(Str);
#?if parrot
    Str.HOW.add_parrot_vtable_mapping(Str, 'get_string',
        nqp::getstaticcode(sub ($self) {
            nqp::unbox_s($self)
        }));
#?endif
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
    EnumMap.HOW.add_attribute(EnumMap, scalar_attr('$!storage', Mu, EnumMap, :associative_delegate));
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
    
    # class Junction is Mu {
    #     has $!items;
    #     has $!flattens;
    #     has $!nextiter;
    #     ...
    # }
    Junction.HOW.add_parent(Junction, Mu);
    Junction.HOW.add_attribute(Junction, scalar_attr('$!storage', Mu, Junction));
    Junction.HOW.add_attribute(Junction, scalar_attr('$!type', Mu, Junction));
    Junction.HOW.compose_repr(Junction);
    
    # class Bool is Cool {
    #     has int $!value;
    #     ...
    # }
    Bool.HOW.add_parent(Bool, Cool);
    Bool.HOW.add_attribute(Bool, BOOTSTRAPATTR.new(:name<$!value>, :type(int), :box_target(1), :package(Bool)));
    Bool.HOW.set_boolification_mode(Bool, 1);
    Bool.HOW.publish_boolification_spec(Bool);
    Bool.HOW.compose_repr(Bool);

    # class ObjAt {
    #     has str $!value;
    # }
    ObjAt.HOW.add_attribute(ObjAt, BOOTSTRAPATTR.new(:name<$!value>, :type(str), :box_target(1), :package(ObjAt)));
    ObjAt.HOW.compose_repr(ObjAt);
    
    # class ForeignCode {
    #     has $!do;                # Code object we delegate to
    #     ... # Uncomposed
    # }
    ForeignCode.HOW.add_parent(ForeignCode, Any);
    ForeignCode.HOW.add_attribute(ForeignCode, BOOTSTRAPATTR.new(:name<$!do>, :type(Mu), :package(ForeignCode)));
    ForeignCode.HOW.compose_repr(ForeignCode);
    ForeignCode.HOW.set_invocation_attr(ForeignCode, ForeignCode, '$!do');
    ForeignCode.HOW.compose_invocation(ForeignCode);

    # Set up Stash type, which is really just a hash.
    Stash.HOW.add_parent(Stash, Hash);
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
    Perl6::Metamodel::ClassHOW.add_stash(ForeignCode);

    # Default invocation behavior delegates off to postcircumfix:<( )>.
    my $invoke_forwarder :=
        nqp::getstaticcode(sub ($self, *@pos, *%named) {
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
    Mu.HOW.set_invocation_handler(Mu, $invoke_forwarder);
    Mu.HOW.compose_invocation(Mu);

    # If we don't already have a PROCESS, set it up.
    my $PROCESS := nqp::gethllsym('perl6', 'PROCESS');
    if nqp::isnull($PROCESS) {
        PROCESS.HOW.compose(PROCESS);
        Perl6::Metamodel::ModuleHOW.add_stash(PROCESS);
        $PROCESS := PROCESS;
        nqp::bindhllsym('perl6', 'PROCESS', $PROCESS);
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
    EXPORT::DEFAULT.WHO<Junction>  := Junction;
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
    EXPORT::DEFAULT.WHO<ForeignCode>         := ForeignCode;
}
EXPORT::DEFAULT.WHO<NQPCursorRole> := NQPCursorRole;

#?if parrot
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
#?endif

# Set up various type mappings.
nqp::p6settypes(EXPORT::DEFAULT.WHO);

# We'll build container descriptors for $_, $! and $/ that we can
# share with all of the magically/lazily created scalars.
#?if parrot
my $topic_cd := Perl6::Metamodel::ContainerDescriptor.new(
    :of(Mu), :rw(1), :name('$_'));
pir::new__PsP('Perl6LexPad', hash()).configure_magicals(
    $topic_cd, Scalar, Any, EnumMap, Hash, Block);
#?endif

# Tell parametric role groups how to create a dispatcher.
Perl6::Metamodel::ParametricRoleGroupHOW.set_selector_creator({
    my $sel := nqp::create(Sub);
    my $onlystar := sub (*@pos, *%named) {
        nqp::invokewithcapture(
            nqp::getcodeobj(nqp::curcode()).find_best_dispatchee(nqp::usecapture()),
            nqp::usecapture())
    };
    nqp::setcodeobj($onlystar, $sel);
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

# Let ClassHOW and EnumHOW know about the invocation handler.
Perl6::Metamodel::ClassHOW.set_default_invoke_handler(
    Mu.HOW.invocation_handler(Mu));
Perl6::Metamodel::EnumHOW.set_default_invoke_handler(
    Mu.HOW.invocation_handler(Mu));

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
nqp::bindhllsym('perl6', 'PROCESS', PROCESS);

# HLL interop configuration.
nqp::sethllconfig('perl6', nqp::hash(
    'null_value', Mu,
    'foreign_type_int', Int,
    'foreign_type_num', Num,
    'foreign_type_str', Str,
    'foreign_transform_array', -> $array {
        nqp::p6parcel($array, Mu)
    },
    'foreign_transform_hash', -> $hash {
        my $result := nqp::create(Hash);
        nqp::bindattr($result, EnumMap, '$!storage', $hash);
        $result
    },
    'foreign_transform_code', -> $code {
        my $result := nqp::create(ForeignCode);
        nqp::bindattr($result, ForeignCode, '$!do', $code);
        $result
    }
));
