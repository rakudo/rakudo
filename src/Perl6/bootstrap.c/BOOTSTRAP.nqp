use Perl6::Metamodel;
use QRegex;

# Here we start to piece together the top of the object model hierarchy.
# We can't just declare these bits in CORE.setting with normal Raku
# syntax due to circularity issues. Note that we don't compose any of
# these - which is equivalent to a { ... } body.
#
# One particular circularity we break here is that you can't have
# inheritance in Raku without traits, but that needs multiple
# dispatch, which can't function without some type hierarchy in
# place. It also needs us to be able to declare signatures with
# parameters and code objects with dispatchees, which in turn needs
# attributes. So, we set up quite a few bits in here, though the aim
# is to keep it "lagom". :-)

# Bootstrapping Attribute class that we eventually replace with the real
# one.
my class BOOTSTRAPATTR {
    has $!name;
    has $!type;
    has $!box_target;
    has $!package;
    has $!inlined;
    has $!dimensions;
    method name() { $!name }
    method type() { $!type }
    method box_target() { $!box_target }
    method package() { $!package }
    method inlined() { $!inlined }
    method dimensions() { $!dimensions }
    method is_built() { 0 }
    method is_bound() { 0 }
    method has_accessor() { 0 }
    method positional_delegate() { 0 }
    method associative_delegate() { 0 }
    method build() { }
    method is_generic() { $!type.HOW.archetypes.generic }
    method instantiate_generic($type_environment) {
        my $ins := $!type.HOW.instantiate_generic($!type, $type_environment);
        self.new(:name($!name), :box_target($!box_target), :type($ins))
    }
    method compose($obj, :$compiler_services) { }
    method gist() { $!type.HOW.name($!type) ~ ' ' ~ $!name }
    method perl() { 'BOOTSTRAPATTR.new' }
    method raku() { 'BOOTSTRAPATTR.new' }
    method Str()  { $!name }
}

# Stub all types.
my stub Mu metaclass Perl6::Metamodel::ClassHOW { ... };
my stub Any metaclass Perl6::Metamodel::ClassHOW { ... };
my stub Nil metaclass Perl6::Metamodel::ClassHOW { ... };
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
my stub List metaclass Perl6::Metamodel::ClassHOW { ... };
my stub Slip metaclass Perl6::Metamodel::ClassHOW { ... };
my stub Array metaclass Perl6::Metamodel::ClassHOW { ... };
my stub Map metaclass Perl6::Metamodel::ClassHOW { ... };
my stub Hash metaclass Perl6::Metamodel::ClassHOW { ... };
my stub Capture metaclass Perl6::Metamodel::ClassHOW { ... };
my stub Bool metaclass Perl6::Metamodel::EnumHOW { ... };
my stub ObjAt metaclass Perl6::Metamodel::ClassHOW { ... };
my stub ValueObjAt metaclass Perl6::Metamodel::ClassHOW { ... };
my stub Stash metaclass Perl6::Metamodel::ClassHOW { ... };
my stub PROCESS metaclass Perl6::Metamodel::ModuleHOW { ... };
my stub Grammar metaclass Perl6::Metamodel::ClassHOW { ... };
my stub Junction metaclass Perl6::Metamodel::ClassHOW { ... };
my stub Metamodel metaclass Perl6::Metamodel::PackageHOW { ... };
my stub ForeignCode metaclass Perl6::Metamodel::ClassHOW { ... };
my stub IntLexRef metaclass Perl6::Metamodel::NativeRefHOW { ... };
my stub NumLexRef metaclass Perl6::Metamodel::NativeRefHOW { ... };
my stub StrLexRef metaclass Perl6::Metamodel::NativeRefHOW { ... };
my stub IntAttrRef metaclass Perl6::Metamodel::NativeRefHOW { ... };
my stub NumAttrRef metaclass Perl6::Metamodel::NativeRefHOW { ... };
my stub StrAttrRef metaclass Perl6::Metamodel::NativeRefHOW { ... };
my stub IntPosRef metaclass Perl6::Metamodel::NativeRefHOW { ... };
my stub NumPosRef metaclass Perl6::Metamodel::NativeRefHOW { ... };
my stub StrPosRef metaclass Perl6::Metamodel::NativeRefHOW { ... };
my stub IntMultidimRef metaclass Perl6::Metamodel::NativeRefHOW { ... };
my stub NumMultidimRef metaclass Perl6::Metamodel::NativeRefHOW { ... };
my stub StrMultidimRef metaclass Perl6::Metamodel::NativeRefHOW { ... };

#?if js
my stub Int64LexRef metaclass Perl6::Metamodel::NativeRefHOW { ... };
my stub Int64AttrRef metaclass Perl6::Metamodel::NativeRefHOW { ... };
my stub Int64PosRef metaclass Perl6::Metamodel::NativeRefHOW { ... };
my stub Int64MultidimRef metaclass Perl6::Metamodel::NativeRefHOW { ... };
#?endif

# Implement the signature binder.
# The JVM backend really only uses trial_bind,
# so we exclude everything else.
my class Binder {
    # Flags that can be set on a signature element. See Parameter.pm6
    my int $SIG_ELEM_BIND_CAPTURE        := 1;
    my int $SIG_ELEM_BIND_PRIVATE_ATTR   := 2;
    my int $SIG_ELEM_BIND_PUBLIC_ATTR    := 4;
    my int $SIG_ELEM_BIND_ATTRIBUTIVE    := ($SIG_ELEM_BIND_PRIVATE_ATTR +| $SIG_ELEM_BIND_PUBLIC_ATTR);
    my int $SIG_ELEM_SLURPY_POS          := 8;
    my int $SIG_ELEM_SLURPY_NAMED        := 16;
    my int $SIG_ELEM_SLURPY_LOL          := 32;
    my int $SIG_ELEM_INVOCANT            := 64;
    my int $SIG_ELEM_MULTI_INVOCANT      := 128;
    my int $SIG_ELEM_IS_RW               := 256;
    my int $SIG_ELEM_IS_COPY             := 512;
    my int $SIG_ELEM_IS_RAW              := 1024;
    my int $SIG_ELEM_IS_OPTIONAL         := 2048;
    my int $SIG_ELEM_ARRAY_SIGIL         := 4096;
    my int $SIG_ELEM_HASH_SIGIL          := 8192;
    my int $SIG_ELEM_DEFAULT_FROM_OUTER  := 16384;
    my int $SIG_ELEM_IS_CAPTURE          := 32768;
    my int $SIG_ELEM_UNDEFINED_ONLY      := 65536;
    my int $SIG_ELEM_DEFINED_ONLY        := 131072;
    my int $SIG_ELEM_DEFINEDNES_CHECK    := ($SIG_ELEM_UNDEFINED_ONLY +| $SIG_ELEM_DEFINED_ONLY);
    my int $SIG_ELEM_TYPE_GENERIC        := 524288;
    my int $SIG_ELEM_DEFAULT_IS_LITERAL  := 1048576;
    my int $SIG_ELEM_NATIVE_INT_VALUE    := 2097152;
    my int $SIG_ELEM_NATIVE_NUM_VALUE    := 4194304;
    my int $SIG_ELEM_NATIVE_STR_VALUE    := 8388608;
    my int $SIG_ELEM_NATIVE_VALUE        := ($SIG_ELEM_NATIVE_INT_VALUE +| $SIG_ELEM_NATIVE_NUM_VALUE +| $SIG_ELEM_NATIVE_STR_VALUE);
    my int $SIG_ELEM_SLURPY_ONEARG       := 16777216;
    my int $SIG_ELEM_SLURPY              := ($SIG_ELEM_SLURPY_POS +| $SIG_ELEM_SLURPY_NAMED +| $SIG_ELEM_SLURPY_LOL +| $SIG_ELEM_SLURPY_ONEARG);
    my int $SIG_ELEM_CODE_SIGIL          := 33554432;
    my int $SIG_ELEM_IS_COERCIVE         := 67108864;

    # Binding result flags.
    my int $BIND_RESULT_OK       := 0;
    my int $BIND_RESULT_FAIL     := 1;
    my int $BIND_RESULT_JUNCTION := 2;

    my $autothreader;
    my $Positional;
    my $PositionalBindFailover;

#?if !jvm
    sub arity_fail($params, int $num_params, int $num_pos_args, int $too_many, $lexpad) {
        my str $error_prefix := $too_many ?? "Too many" !! "Too few";
        my int $count;
        my int $arity;

        my int $param_i := 0;
        while $param_i < $num_params {
            my $param := nqp::atpos($params, $param_i);
            my int $flags := nqp::getattr_i($param, Parameter, '$!flags');

            if !nqp::isnull(nqp::getattr($param, Parameter, '@!named_names')) {
            }
            elsif $flags +& $SIG_ELEM_SLURPY_NAMED {
            }
            elsif $flags +& $SIG_ELEM_SLURPY {
                $count := -1000;  # in case a pos can sneak past a slurpy somehow
            }
            elsif $flags +& $SIG_ELEM_IS_OPTIONAL {
                ++$count
            }
            else {
                ++$count;
                ++$arity;
            }

            ++$param_i;
        }
        my str $s := $arity == 1 ?? "" !! "s";
        my str $routine := nqp::getcodeobj(nqp::ctxcode($lexpad)).name;
        $routine := '<anon>' unless $routine;

        if $arity == $count {
            return "$error_prefix positionals passed to '$routine'; expected $arity argument$s but got $num_pos_args";
        } elsif $count < 0 {
            return "$error_prefix positionals passed to '$routine'; expected at least $arity argument$s but got only $num_pos_args";
        } else {
            my str $conj := $count == $arity+1 ?? "or" !! "to";
            return "$error_prefix positionals passed to '$routine'; expected $arity $conj $count arguments but got $num_pos_args";
        }
    }

    method set_autothreader($callable) {
        $autothreader := $callable;
    }

    method set_pos_bind_failover($pos, $pos_bind_failover) {
        $Positional := $pos;
        $PositionalBindFailover := $pos_bind_failover;
    }

    # Binds a single parameter.
    sub bind_one_param($lexpad, $sig, $param, int $no_param_type_check, $error,
                       int $got_native, $oval, int $ival, num $nval, str $sval) {
        # Grab flags and variable name.
        my int $flags       := nqp::getattr_i($param, Parameter, '$!flags');
        my str $varname     := nqp::getattr_s($param, Parameter, '$!variable_name');
        my int $has_varname := 1;
        if nqp::isnull_s($varname) {
            $varname := '<anon>';
            $has_varname := 0;
        }

        # Check if boxed/unboxed expectations are met.
        my int $desired_native := $flags +& $SIG_ELEM_NATIVE_VALUE;
        my int $is_rw          := $flags +& $SIG_ELEM_IS_RW;
        if $is_rw && $desired_native {
            if $desired_native == $SIG_ELEM_NATIVE_INT_VALUE {
                unless !$got_native && nqp::iscont_i($oval) {
                    if nqp::defined($error) {
                        $error[0] := "Expected a modifiable native int argument for '$varname'";
                    }
                    return $BIND_RESULT_FAIL;
                }
            }
            elsif $desired_native == $SIG_ELEM_NATIVE_NUM_VALUE {
                unless !$got_native && nqp::iscont_n($oval) {
                    if nqp::defined($error) {
                        $error[0] := "Expected a modifiable native num argument for '$varname'";
                    }
                    return $BIND_RESULT_FAIL;
                }
            }
            elsif $desired_native == $SIG_ELEM_NATIVE_STR_VALUE {
                unless !$got_native && nqp::iscont_s($oval) {
                    if nqp::defined($error) {
                        $error[0] := "Expected a modifiable native str argument for '$varname'";
                    }
                    return $BIND_RESULT_FAIL;
                }
            }
        }
        elsif $desired_native != $got_native {
            # Maybe we need to box the native.
            if $desired_native == 0 {
                if $got_native == $SIG_ELEM_NATIVE_INT_VALUE {
                    $oval := nqp::box_i($ival, Int);
                }
                elsif $got_native == $SIG_ELEM_NATIVE_NUM_VALUE {
                    $oval := nqp::box_n($nval, Num);
                }
                else {
                    $oval := nqp::box_s($sval, Str);
                }
                $got_native := 0;
            }

            # Otherwise, maybe we need to unbox.
            elsif !$got_native {
                # XXX Probably want to do this a little differently to get a
                # better error.
                if $desired_native == $SIG_ELEM_NATIVE_INT_VALUE {
                    $ival := nqp::unbox_i($oval);
                    $got_native := $SIG_ELEM_NATIVE_INT_VALUE;
                }
                elsif $desired_native == $SIG_ELEM_NATIVE_NUM_VALUE {
                    $nval := nqp::unbox_n($oval);
                    $got_native := $SIG_ELEM_NATIVE_NUM_VALUE;
                }
                else {
                    $sval := nqp::unbox_s($oval);
                    $got_native := $SIG_ELEM_NATIVE_STR_VALUE;
                }
            }

            # Otherwise, incompatible native types.
            else {
                if nqp::defined($error) {
                    $error[0] := "Incompatible native type passed for '$varname'";
                }
                return $BIND_RESULT_FAIL;
            }
        }

        # By this point, we'll either have an object that we might be able to
        # bind if it passes the type check, or a native value that needs no
        # further checking.
        my $param_type := nqp::getattr($param, Parameter, '$!type');
        unless $got_native || ($is_rw && $desired_native) {
            # HLL-ize.
            $oval := nqp::hllizefor($oval, 'Raku');

            # Skip nominal type check if not needed.
            unless $no_param_type_check {
                # Is the nominal type generic and in need of instantiation? (This
                # can happen in (::T, T) where we didn't learn about the type until
                # during the signature bind).
                if $flags +& $SIG_ELEM_TYPE_GENERIC {
                    $param_type := $param_type.HOW.instantiate_generic($param_type, $lexpad);
                }

                # If the expected type is Positional, see if we need to do the
                # positional bind failover.
                if nqp::istype($param_type, $Positional) && nqp::istype($oval, $PositionalBindFailover) {
                    $oval := $oval.cache;
                }

                # If not, do the check. If the wanted nominal type is Mu, then
                # anything goes.
                unless $param_type =:= Mu || nqp::istype($oval, $param_type) {
                    # Type check failed; produce error if needed.

                    # Try to figure out the most helpful name for the expected
                    my $expected := (
                      (my $post := nqp::getattr($param, Parameter,
                        '@!post_constraints'))
                      && ! nqp::istype(nqp::atpos($post, 0), Code)
                    ) ?? nqp::atpos($post, 0) !! $param_type;

                    if nqp::defined($error) {
                        $error[0] := {
                            Perl6::Metamodel::Configuration.throw_or_die(
                                'X::TypeCheck::Binding::Parameter',
                                "Nominal type check failed for parameter '" ~ $varname
                                    ~ "'; expected " ~ $expected.HOW.name($expected)
                                    ~ " but got " ~ $oval.HOW.name($oval),
                                :got($oval),
                                :expected($expected.WHAT),
                                :symbol(nqp::hllizefor($varname, 'Raku')),
                                :parameter($param))
                        };
                    }

                    # Report junction failure mode if it's a junction.
                    return $oval.WHAT =:= Junction && nqp::isconcrete($oval)
                        ?? $BIND_RESULT_JUNCTION
                        !! $BIND_RESULT_FAIL;
                }

                # Also enforce definedness constraints.
                if $flags +& $SIG_ELEM_DEFINEDNES_CHECK {
                    if (my $should_be_concrete := $flags +& $SIG_ELEM_DEFINED_ONLY   && !nqp::isconcrete($oval)) ||
                                                  $flags +& $SIG_ELEM_UNDEFINED_ONLY &&  nqp::isconcrete($oval)
                    {
                        if nqp::defined($error) {
                            my $method := nqp::getcodeobj(nqp::ctxcode($lexpad)).name;
                            my $class  := $param_type.HOW.name($param_type);
                            my $got    := $oval.HOW.name($oval);
                            my $die_msg := $flags +& $SIG_ELEM_INVOCANT
                                  ?? $should_be_concrete
                                       ?? "Invocant of method '$method' must be an object instance of type '$class', not a type object of type '$got'.  Did you forget a '.new'?"
                                       !! "Invocant of method '$method' must be a type object of type '$class', not an object instance of type '$got'.  Did you forget a 'multi'?"
                                  !! $should_be_concrete
                                       ?? "Parameter '$varname' of routine '$method' must be an object instance of type '$class', not a type object of type '$got'.  Did you forget a '.new'?"
                                       !! "Parameter '$varname' of routine '$method' must be a type object of type '$class', not an object instance of type '$got'.  Did you forget a 'multi'?";
                            $error[0] := {
                                Perl6::Metamodel::Configuration.throw_or_die(
                                    'X::Parameter::InvalidConcreteness',
                                    $die_msg,
                                    :expected($class),
                                    :got($got),
                                    :routine($method),
                                    :param($varname),
                                    :should-be-concrete(nqp::hllboolfor($should_be_concrete, 'Raku')),
                                    :param-is-invocant(nqp::hllboolfor($flags +& $SIG_ELEM_INVOCANT, 'Raku'))
                                );
                            };
                        }
                        return $oval.WHAT =:= Junction && nqp::isconcrete($oval)
                            ?? $BIND_RESULT_JUNCTION
                            !! $BIND_RESULT_FAIL;
                    }
                }
            }
        }

        # Do we have any type captures to bind?
        my $type_caps := nqp::getattr($param, Parameter, '@!type_captures');
        unless nqp::isnull($type_caps) {
            my int $num_type_caps := nqp::elems($type_caps);
            my int $i := -1;
            while ++$i < $num_type_caps {
                nqp::bindkey($lexpad, nqp::atpos_s($type_caps, $i), $oval.WHAT);
            }
        }

        # Do a coercion, if one is needed.
        if $param.coercive {
            # Coercing natives not possible - nothing to call a method on.
            if $got_native {
                if nqp::defined($error) {
                    $error[0] := "Unable to coerce natively typed parameter '$varname'";
                }
                return $BIND_RESULT_FAIL;
            }

            my $coercion_type := $param_type.HOW.wrappee($param_type, :coercion);
            $oval := $coercion_type.HOW.coerce($coercion_type, $oval);
        }

        # If it's not got attributive binding, we'll go about binding it into the
        # lex pad.
        my int $is_attributive := $flags +& $SIG_ELEM_BIND_ATTRIBUTIVE;
        unless $is_attributive {
            # Is it native? If so, just go ahead and bind it.
            if $got_native {
                if $got_native == $SIG_ELEM_NATIVE_INT_VALUE {
                    nqp::bindkey_i($lexpad, $varname, $ival);
                }
                elsif $got_native == $SIG_ELEM_NATIVE_NUM_VALUE {
                    nqp::bindkey_n($lexpad, $varname, $nval);
                }
                else {
                    nqp::bindkey_s($lexpad, $varname, $sval);
                }
            }

            # Otherwise it's some objecty case.
            elsif $is_rw {
                if nqp::isrwcont($oval) {
                    nqp::bindkey($lexpad, $varname, $oval) if $has_varname;
                }
                else {
                    if nqp::defined($error) {
                        $error[0] := {
                            Perl6::Metamodel::Configuration.throw_or_die(
                                'X::Parameter::RW',
                                "Parameter '$varname' expected a writable container, but got an " ~
                                    ~ $oval.HOW.name($oval) ~ " value",
                                :got($oval),
                                :symbol($varname)
                            )
                        };
                    }
                    return $BIND_RESULT_FAIL;
                }
            }
            elsif $has_varname {
                if $flags +& $SIG_ELEM_IS_RAW {
                    # Just bind the thing as is into the lexpad.
                    nqp::bindkey($lexpad, $varname, $oval);
                }
                else {
                    # If it's an array, copy means make a new one and store,
                    # and a normal bind is a straightforward binding.
                    if $flags +& $SIG_ELEM_ARRAY_SIGIL {
                        if $flags +& $SIG_ELEM_IS_COPY {
                            my $bindee := nqp::create(Array);
                            $bindee.STORE(nqp::decont($oval));
                            nqp::bindkey($lexpad, $varname, $bindee);
                        }
                        else {
                            nqp::bindkey($lexpad, $varname, nqp::decont($oval));
                        }
                    }

                    # If it's a hash, similar approach to array.
                    elsif $flags +& $SIG_ELEM_HASH_SIGIL {
                        if $flags +& $SIG_ELEM_IS_COPY {
                            my $bindee := nqp::create(Hash);
                            $bindee.STORE(nqp::decont($oval));
                            nqp::bindkey($lexpad, $varname, $bindee);
                        }
                        else {
                            nqp::bindkey($lexpad, $varname, nqp::decont($oval));
                        }
                    }

                    # If it's a scalar, we always need to wrap it into a new
                    # container and store it; the container descriptor will be
                    # provided and make it rw if it's an `is copy`.
                    else {
                        my $new_cont := nqp::create(Scalar);
                        nqp::bindattr($new_cont, Scalar, '$!descriptor',
                            nqp::getattr($param, Parameter, '$!container_descriptor'));
                        nqp::bindattr($new_cont, Scalar, '$!value', nqp::decont($oval));
                        nqp::bindkey($lexpad, $varname, $new_cont);
                    }
                }
            }
        }

        # Is it the invocant? If so, also have to bind to self lexical.
        if $flags +& $SIG_ELEM_INVOCANT {
            nqp::bindkey($lexpad, 'self', nqp::decont($oval));
        }

        # Handle any constraint types (note that they may refer to the parameter by
        # name, so we need to have bound it already).
        my $post_cons := nqp::getattr($param, Parameter, '@!post_constraints');
        unless nqp::isnull($post_cons) {
            my int $n := nqp::elems($post_cons);
            my int $i := -1;
            while ++$i < $n {
                # Check we meet the constraint.
                my $cons_type := nqp::atpos($post_cons, $i);
                if nqp::istype($cons_type, Code) {
                    $cons_type := nqp::p6capturelexwhere($cons_type.clone());
                }
                my $result;
                my $bad_value;
                if $got_native == 0 {
                    $result := $cons_type.ACCEPTS($oval);
                    $bad_value := $oval unless $result;
                }
                elsif $got_native == $SIG_ELEM_NATIVE_INT_VALUE {
                    $result := $cons_type.ACCEPTS($ival);
                    $bad_value := $ival unless $result;
                }
                elsif $got_native == $SIG_ELEM_NATIVE_NUM_VALUE {
                    $result := $cons_type.ACCEPTS($nval);
                    $bad_value := $nval unless $result;
                }
                elsif $got_native == $SIG_ELEM_NATIVE_STR_VALUE {
                    $result := $cons_type.ACCEPTS($sval);
                    $bad_value := $sval unless $result;
                }
                unless $result {
                    if nqp::defined($error) {
                        $error[0] := {
                            Perl6::Metamodel::Configuration.throw_or_die(
                                'X::TypeCheck::Binding::Parameter',
                                "Constraint type check failed for parameter '$varname'",
                                :got($bad_value),
                                :expected($cons_type),
                                :symbol($varname),
                                :parameter($param),
                                :constraint(nqp::hllboolfor(1, 'Raku'))
                            )
                        };
                    }
                    return $BIND_RESULT_FAIL;
                }
            }
        }

        # If it's attributive, now we assign it.
        if $is_attributive {
            # Find self.
            my $self;
            if nqp::existskey($lexpad, 'self') {
                $self := nqp::atkey($lexpad, 'self');
            } else {
                if nqp::defined($error) {
                    $error[0] := "Unable to bind attributive parameter '$varname'; could not find self";
                }
                return $BIND_RESULT_FAIL;
            }

            # Ensure it's not native; NYI.
            if $got_native {
                if nqp::defined($error) {
                    $error[0] := "Binding to natively typed attributive parameter '$varname' not supported";
                }
                return $BIND_RESULT_FAIL;
            }

            # If it's private, just need to fetch the attribute.
            my $assignee;
            if ($flags +& $SIG_ELEM_BIND_PRIVATE_ATTR) {
                $assignee := nqp::getattr($self,
                    nqp::getattr($param, Parameter, '$!attr_package'),
                    $varname);
            }

            # Otherwise if it's public, do a method call to get the assignee.
            else {
                $assignee := $self."$varname"();
            }

            nqp::iscont($assignee)
                ?? nqp::assign($assignee, $oval)
                !! $assignee.STORE(nqp::decont($oval));
        }

        # If it has a sub-signature, bind that.
        my $subsig := nqp::getattr($param, Parameter, '$!sub_signature');
        unless nqp::isnull($subsig) {
            # Turn value into a capture, unless we already have one.
            my $capture;
            if $flags +& $SIG_ELEM_IS_CAPTURE {
                $capture := $oval;
            }
            else {
                if nqp::can($oval, 'Capture') {
                    $capture := $oval.Capture;
                }
                else {
                    if nqp::defined($error) {
                        $error[0] := "Could not turn argument into capture";
                    }
                    return $BIND_RESULT_FAIL;
                }
            }

            # Recurse into signature binder.
            my $result := bind(make_vm_capture($capture), $subsig, $lexpad,
                $no_param_type_check, $error);
            unless $result == $BIND_RESULT_OK {
                if $error && nqp::isstr($error[0]) {
                    # Note in the error message that we're in a sub-signature.
                    $error[0] := $error[0] ~ " in sub-signature";
                    if $has_varname {
                        $error[0] := $error[0] ~ " of parameter " ~ $varname;
                    }
                }
                return $result;
            }
        }

        # Binding of this parameter was thus successful - we're done.
        $BIND_RESULT_OK
    }

    # This takes a signature element and either runs the closure to get a default
    # value if there is one, or creates an appropriate undefined-ish thingy.
    sub handle_optional($param, int $flags, $lexpad) {
        # Is the "get default from outer" flag set?
        if $flags +& $SIG_ELEM_DEFAULT_FROM_OUTER {
            nqp::atkey(
                nqp::ctxouter($lexpad),
                nqp::getattr_s($param, Parameter, '$!variable_name'))
        }

        # Do we have a default value or value closure?
        elsif !nqp::isnull(my $default_value := nqp::getattr($param, Parameter, '$!default_value')) {
            if $flags +& $SIG_ELEM_DEFAULT_IS_LITERAL {
                $default_value;
            }
            else {
                nqp::p6capturelexwhere($default_value.clone)();
            }
        }

        # Otherwise, go by sigil to pick the correct default type of value.
        else {
            if $flags +& $SIG_ELEM_ARRAY_SIGIL {
                nqp::create(Array)
            }
            elsif $flags +& $SIG_ELEM_HASH_SIGIL {
                nqp::create(Hash)
            }
            else {
                nqp::getattr($param, Parameter, '$!type');
            }
        }
    }

    # Drives the overall binding process.
    sub bind($capture, $sig, $lexpad, int $no_param_type_check, $error) {
        # Get params.
        my @params := nqp::getattr($sig, Signature, '@!params');

        # If we do have some named args, we get hold of a hash of them. We
        # can safely delete from this as we go.
        my $named_args;
        if nqp::capturehasnameds($capture) {
            $named_args := nqp::capturenamedshash($capture);
        }

        # Now we'll walk through the signature and go about binding things.
        my $named_names;
        my int $cur_pos_arg := 0;
        my int $num_pos_args := nqp::captureposelems($capture);
        my int $suppress_arity_fail := 0;
        my int $num_params := nqp::elems(@params);
        my int $i := 0;
        while $i < $num_params {
            # Get parameter object and its flags.
            my $param := nqp::atpos(@params, $i);
            my int $flags := nqp::getattr_i($param, Parameter, '$!flags');
            my str $var_name := nqp::getattr_s($param, Parameter, '$!variable_name');
            ++$i;

            # Is it looking for us to bind a capture here?
            my int $bind_fail;
            my int $got_prim;
            if $flags +& $SIG_ELEM_IS_CAPTURE {
                # Capture the arguments from this point forwards into a Capture.
                # Of course, if there's no variable name we can (cheaply) do pretty
                # much nothing.
                if nqp::isnull_s($var_name)
                    && !nqp::getattr($param, Parameter, '$!sub_signature')
                    && !nqp::getattr($param, Parameter, '@!post_constraints') {
                    $bind_fail := $BIND_RESULT_OK;
                }
                else {
                    my $capsnap := nqp::create(Capture);

                    my @pos_args;
                    my int $k := $cur_pos_arg;
                    while $k < $num_pos_args {
                        $got_prim := nqp::captureposprimspec($capture, $k);
                        if $got_prim == 0 {
                            nqp::push(@pos_args, nqp::captureposarg($capture, $k));
                        }
                        elsif $got_prim == 1 {
                            nqp::push(@pos_args, nqp::box_i(nqp::captureposarg_i($capture, $k), Int));
                        }
                        elsif $got_prim == 2 {
                            nqp::push(@pos_args, nqp::box_n(nqp::captureposarg_n($capture, $k), Num));
                        }
                        else {
                            nqp::push(@pos_args, nqp::box_s(nqp::captureposarg_s($capture, $k), Str));
                        }
                        ++$k;
                    }
                    nqp::bindattr($capsnap, Capture, '@!list', @pos_args);

                    if $named_args {
                        nqp::bindattr($capsnap, Capture, '%!hash', nqp::clone($named_args));
                    }
                    else {
                        nqp::bindattr($capsnap, Capture, '%!hash', nqp::hash());
                    }

                    $bind_fail := bind_one_param($lexpad, $sig, $param, $no_param_type_check, $error,
                        0, $capsnap, 0, 0.0, '');
                }
                if ($bind_fail) {
                    return $bind_fail;
                }
                elsif $i == $num_params {
                    # Since a capture acts as "the ultimate slurpy" in a sense, if
                    # this is the last parameter in the signature we can return
                    # success right off the bat.
                    return $BIND_RESULT_OK;
                }
                else {
                    my $next_param := nqp::atpos(@params, $i);
                    my int $next_flags := nqp::getattr_i($next_param, Parameter, '$!flags');
                    if $next_flags +& ($SIG_ELEM_SLURPY_POS +| $SIG_ELEM_SLURPY_NAMED) {
                        $suppress_arity_fail := 1;
                    }
                }
            }

            # Could it be a named slurpy?
            elsif $flags +& $SIG_ELEM_SLURPY_NAMED {
                # We'll either take the current named arguments copy hash which
                # will by definition contain all unbound named arguments and use
                # that. If there are none, just keep the storage uninitialized
                # and rely on autovivification to build up an empty nqp::hash
                # whenever needed.
                my $hash := nqp::create(Hash);
                nqp::bindattr($hash, Map, '$!storage', $named_args) if $named_args;
                $bind_fail := bind_one_param($lexpad, $sig, $param, $no_param_type_check, $error,
                    0, $hash, 0, 0.0, '');
                return $bind_fail if $bind_fail;

                # Undefine named arguments hash now we've consumed it, to mark all
                # is well.
                $named_args := NQPMu;
            }

            # Otherwise, maybe it's a positional.
            elsif nqp::isnull($named_names := nqp::getattr($param, Parameter, '@!named_names')) {
                # Slurpy or LoL-slurpy?
                if $flags +& ($SIG_ELEM_SLURPY_POS +| $SIG_ELEM_SLURPY_LOL +| $SIG_ELEM_SLURPY_ONEARG) {
                    # Create Perl 6 array, create VM Array of all remaining things, then
                    # store it.
                    my $temp := nqp::list();
                    while $cur_pos_arg < $num_pos_args {
                        $got_prim := nqp::captureposprimspec($capture, $cur_pos_arg);
                        if $got_prim == 0 {
                            nqp::push($temp, nqp::captureposarg($capture, $cur_pos_arg));
                        }
                        elsif $got_prim == 1 {
                            nqp::push($temp, nqp::box_i(nqp::captureposarg_i($capture, $cur_pos_arg), Int));
                        }
                        elsif $got_prim == 2 {
                            nqp::push($temp, nqp::box_n(nqp::captureposarg_n($capture, $cur_pos_arg), Num));
                        }
                        else {
                            nqp::push($temp, nqp::box_s(nqp::captureposarg_s($capture, $cur_pos_arg), Str));
                        }
                        ++$cur_pos_arg;
                    }
                    my $slurpy_type := $flags +& $SIG_ELEM_IS_RAW || $flags +& $SIG_ELEM_IS_RW ?? List !! Array;
                    my $bindee := $flags +& $SIG_ELEM_SLURPY_ONEARG
                        ?? $slurpy_type.from-slurpy-onearg($temp)
                        !! $flags +& $SIG_ELEM_SLURPY_POS
                        ?? $slurpy_type.from-slurpy-flat($temp)
                        !! $slurpy_type.from-slurpy($temp);
                    $bind_fail := bind_one_param($lexpad, $sig, $param, $no_param_type_check, $error,
                        0, $bindee, 0, 0.0, '');
                    return $bind_fail if $bind_fail;
                }

                # Otherwise, a positional.
                else {
                    # Do we have a value?
                    if $cur_pos_arg < $num_pos_args {
                        # Easy - just bind that.
                        $got_prim := nqp::captureposprimspec($capture, $cur_pos_arg);
                        if $got_prim == 0 {
                            $bind_fail := bind_one_param($lexpad, $sig, $param, $no_param_type_check, $error,
                                0, nqp::captureposarg($capture, $cur_pos_arg), 0, 0.0, '');
                        }
                        elsif $got_prim == 1 {
                            $bind_fail := bind_one_param($lexpad, $sig, $param, $no_param_type_check, $error,
                                $SIG_ELEM_NATIVE_INT_VALUE, nqp::null(), nqp::captureposarg_i($capture, $cur_pos_arg), 0.0, '');
                        }
                        elsif $got_prim == 2 {
                            $bind_fail := bind_one_param($lexpad, $sig, $param, $no_param_type_check, $error,
                                $SIG_ELEM_NATIVE_NUM_VALUE, nqp::null(), 0, nqp::captureposarg_n($capture, $cur_pos_arg), '');
                        }
                        else {
                            $bind_fail := bind_one_param($lexpad, $sig, $param, $no_param_type_check, $error,
                                $SIG_ELEM_NATIVE_STR_VALUE, nqp::null(), 0, 0.0, nqp::captureposarg_s($capture, $cur_pos_arg));
                        }
                        return $bind_fail if $bind_fail;
                        ++$cur_pos_arg;
                    }
                    else {
                        # No value. If it's optional, fetch a default and bind that;
                        # if not, we're screwed. Note that we never nominal type check
                        # an optional with no value passed.
                        if $flags +& $SIG_ELEM_IS_OPTIONAL {
                            $bind_fail := bind_one_param($lexpad, $sig, $param, $no_param_type_check, $error,
                                0, handle_optional($param, $flags, $lexpad), 0, 0.0, '');
                            return $bind_fail if $bind_fail;
                        }
                        else {
                            if nqp::defined($error) {
                                $error[0] := arity_fail(@params, $num_params, $num_pos_args, 0, $lexpad);
                            }
                            return $BIND_RESULT_FAIL;
                        }
                    }
                }
            }

            # Else, it's a non-slurpy named.
            else {
                # Try and get hold of value.
                my $value := nqp::null();
                if $named_args {
                    my int $num_names := nqp::elems($named_names);
                    my int $j := -1;
                    my str $cur_name;
                    while ++$j < $num_names {
                        $cur_name := nqp::atpos_s($named_names, $j);
                        $value := nqp::atkey($named_args, $cur_name);
                        unless nqp::isnull($value) {
                            nqp::deletekey($named_args, $cur_name);
                            $j := $num_names;
                        }
                    }
                }

                # Did we get one?
                if nqp::isnull($value) {
                    # Nope. We'd better hope this param was optional...
                    if $flags +& $SIG_ELEM_IS_OPTIONAL {
                        $bind_fail := bind_one_param($lexpad, $sig, $param, $no_param_type_check, $error,
                            0, handle_optional($param, $flags, $lexpad), 0, 0.0, '');
                    }
                    elsif !$suppress_arity_fail {
                        if nqp::defined($error) {
                            $error[0] := "Required named argument '" ~
                                nqp::atpos_s($named_names,0) ~ "' not passed";
                        }
                        return $BIND_RESULT_FAIL;
                    }
                }
                else {
                    $bind_fail := bind_one_param($lexpad, $sig, $param, $no_param_type_check, $error,
                        0, $value, 0, 0.0, '');
                }

                # If we got a binding failure, return it.
                return $bind_fail if $bind_fail;
            }
        }

        # Do we have any left-over args?
        if $cur_pos_arg < $num_pos_args && !$suppress_arity_fail {
            # Oh noes, too many positionals passed.
            if nqp::defined($error) {
                $error[0] := arity_fail(@params, $num_params, $num_pos_args, 1, $lexpad);
            }
            return $BIND_RESULT_FAIL;
        }
        if $named_args {
            # Oh noes, unexpected named args.
            if nqp::defined($error) {
                my int $num_extra := nqp::elems($named_args);
                my @names;
                for $named_args {
                    nqp::push(@names, $_.key);
                }
                if $num_extra == 1 {
                    $error[0] := "Unexpected named argument '" ~ @names[0] ~ "' passed";
                }
                else {
                    $error[0] := $num_extra ~ " unexpected named arguments passed (" ~
                        nqp::join(",", @names) ~")";
                }
            }
            return $BIND_RESULT_FAIL;
        }

        # If we get here, we're done.
        return $BIND_RESULT_OK;
    }

    method bind($capture, $sig, $lexpad, int $no_param_type_check, $error) {
        bind($capture, $sig, $lexpad, $no_param_type_check, $error);
    }

    method bind_sig($capture) {
        # Get signature and lexpad.
        my $caller := nqp::getcodeobj(nqp::callercode());
        my $sig    := nqp::getattr($caller, Code, '$!signature');
        my $lexpad := nqp::ctxcaller(nqp::ctx());

        # Call binder.
        my @error;
        my int $bind_res := bind($capture, $sig, $lexpad, 0, @error);
        if $bind_res {
            if $bind_res == $BIND_RESULT_JUNCTION {
                my @pos_args;
                my int $num_pos_args := nqp::captureposelems($capture);
                my int $k := -1;
                my int $got_prim;
                while ++$k < $num_pos_args {
                    $got_prim := nqp::captureposprimspec($capture, $k);
                    if $got_prim == 0 {
                        nqp::push(@pos_args, nqp::captureposarg($capture, $k));
                    }
                    elsif $got_prim == 1 {
                        nqp::push(@pos_args, nqp::box_i(nqp::captureposarg_i($capture, $k), Int));
                    }
                    elsif $got_prim == 2 {
                        nqp::push(@pos_args, nqp::box_n(nqp::captureposarg_n($capture, $k), Num));
                    }
                    else {
                        nqp::push(@pos_args, nqp::box_s(nqp::captureposarg_s($capture, $k), Str));
                    }
                }
                my %named_args := nqp::capturenamedshash($capture);
                return Junction.AUTOTHREAD($caller,
                        |@pos_args,
                        |%named_args);
            }
            else {
                nqp::isinvokable(@error[0]) ?? @error[0]() !! nqp::die(@error[0]);
            }
        }
        nqp::null();
    }

    sub make_vm_capture($capture) {
        sub vm_capture(*@pos, *%named) { nqp::savecapture() }
        my @list := nqp::getattr($capture, Capture, '@!list');
        @list    := nqp::list() unless nqp::islist(@list);
        my %hash := nqp::getattr($capture, Capture, '%!hash');
        %hash    := nqp::hash() unless nqp::ishash(%hash);
        vm_capture(|@list, |%hash)
    }

    method is_bindable($sig, $capture) {
        unless nqp::reprname($capture) eq 'MVMCallCapture' {
            $capture := make_vm_capture($capture);
        }
        nqp::p6invokeunder(
            nqp::getattr($sig, Signature, '$!code'),
            -> { bind($capture, $sig, nqp::ctxcaller(nqp::ctx()), 0, NQPMu) != $BIND_RESULT_FAIL })
    }

    method bind_cap_to_sig($sig, $cap) {
        my $capture := make_vm_capture($cap);
        my $lexpad  := nqp::ctxcaller(nqp::ctx());
        my @error;
        if bind($capture, $sig, $lexpad, 0, @error) != $BIND_RESULT_OK {
            nqp::isinvokable(@error[0]) ?? @error[0]() !! nqp::die(@error[0]);
        }
        $sig
    }

    method get_return_type($code) {
        nqp::getattr(nqp::getattr($code, Code, '$!signature'), Signature, '$!returns')
    }
#?endif

    my int $TRIAL_BIND_NOT_SURE :=  0;   # Plausible, but need to check at runtime.
    my int $TRIAL_BIND_OK       :=  1;   # Bind will always work out.
    my int $TRIAL_BIND_NO_WAY   := -1;   # Bind could never work out.
    method trial_bind($sig, $args, $sigflags) {
        my @params         := nqp::getattr($sig, Signature, '@!params');
        my int $num_params := nqp::elems(@params);

        # If there's a single capture parameter, then we're OK. (Worth
        # handling especially as it's the common case for protos).
        if $num_params == 1 {
            if nqp::getattr_i(@params[0], Parameter, '$!flags') +& $SIG_ELEM_IS_CAPTURE
            && nqp::isnull(
              nqp::getattr(@params[0], Parameter, '@!post_constraints')) {
                return $TRIAL_BIND_OK;
            }
        }

        # Walk through the signature and consider the parameters.
        my int $num_pos_args := nqp::elems($args);
        my int $cur_pos_arg  := 0;
        my int $i            := -1;
        while ++$i < $num_params {
            my $param := @params[$i];

            # If the parameter is anything other than a boring old
            # positional parameter, we won't analyze it and will bail out,
            # unless it's a slurpy named param, in which case just ignore it
            my int $flags := nqp::getattr_i($param, Parameter, '$!flags');
            if $flags +& $SIG_ELEM_SLURPY_NAMED
              && nqp::isnull(
                nqp::getattr($param, Parameter, '@!post_constraints')) {
                  next
            }
            if $flags +& nqp::bitneg_i(
                    $SIG_ELEM_MULTI_INVOCANT +| $SIG_ELEM_IS_RAW +|
                    $SIG_ELEM_IS_COPY +| $SIG_ELEM_ARRAY_SIGIL +|
                    $SIG_ELEM_HASH_SIGIL +| $SIG_ELEM_NATIVE_VALUE +|
                    $SIG_ELEM_IS_OPTIONAL) || $flags +& $SIG_ELEM_IS_RW {
                return $TRIAL_BIND_NOT_SURE;
            }
            unless nqp::isnull(nqp::getattr($param, Parameter, '@!named_names')) {
                return $TRIAL_BIND_NOT_SURE;
            }
            unless nqp::isnull(nqp::getattr($param, Parameter, '@!post_constraints')) {
                return $TRIAL_BIND_NOT_SURE;
            }
            unless nqp::isnull(nqp::getattr($param, Parameter, '@!type_captures')) {
                return $TRIAL_BIND_NOT_SURE;
            }
            if $param.coercive {
                return $TRIAL_BIND_NOT_SURE;
            }

            # Do we have an argument for this parameter?
            if $cur_pos_arg >= $num_pos_args {
                # No; if it's not optional, fail.
                unless $flags +& $SIG_ELEM_IS_OPTIONAL {
                    return $TRIAL_BIND_NO_WAY;
                }
            }
            else {
                # Yes, need to consider type
                my int $got_prim := $sigflags[$cur_pos_arg] +& 0xF;
                if $flags +& $SIG_ELEM_NATIVE_VALUE {
                    if $got_prim == 0 {
                        # We got an object; if we aren't sure we can unbox, we can't
                        # be sure about the dispatch.
                        if $flags +& $SIG_ELEM_NATIVE_INT_VALUE {
                            return $TRIAL_BIND_NOT_SURE unless nqp::isint($args[$cur_pos_arg]);
                        }
                        elsif $flags +& $SIG_ELEM_NATIVE_NUM_VALUE {
                            return $TRIAL_BIND_NOT_SURE unless nqp::isnum($args[$cur_pos_arg]);
                        }
                        elsif $flags +& $SIG_ELEM_NATIVE_STR_VALUE {
                            return $TRIAL_BIND_NOT_SURE unless nqp::isstr($args[$cur_pos_arg]);
                        }
                        else {
                            # WTF...
                            return $TRIAL_BIND_NOT_SURE;
                        }
                    }
                    else {
                        # If it's the wrong type of native, there's no way it
                        # can ever bind.
                        if (($flags +& $SIG_ELEM_NATIVE_INT_VALUE) && $got_prim != 1) ||
                           (($flags +& $SIG_ELEM_NATIVE_NUM_VALUE) && $got_prim != 2) ||
                           (($flags +& $SIG_ELEM_NATIVE_STR_VALUE) && $got_prim != 3) {
                            return $TRIAL_BIND_NO_WAY;
                        }
                    }
                }
                else {
                    # Work out a parameter type to consider, and see if it matches.
                    my $arg := $got_prim == 1 ?? Int !!
                               $got_prim == 2 ?? Num !!
                               $got_prim == 3 ?? Str !!
                               $args[$cur_pos_arg];
                    my $param_type := nqp::getattr($param, Parameter, '$!type');
                    unless $param_type =:= Mu || nqp::istype($arg, $param_type) {
                        # If it failed because we got a junction, may auto-thread;
                        # hand back 'not sure' for now.
                        if $arg.WHAT =:= Junction {
                            return $TRIAL_BIND_NOT_SURE;
                        }

                        # It failed to, but that doesn't mean it can't work at runtime;
                        # we perhaps want an Int, and the most we know is we have an Any,
                        # which would include Int. However, the Int ~~ Str case can be
                        # rejected now, as there's no way it'd ever match. Basically, we
                        # just flip the type check around.
                        return nqp::istype($param_type, $arg.WHAT)
                            ?? $TRIAL_BIND_NOT_SURE
                            !! $TRIAL_BIND_NO_WAY;
                    }
                }
            }

            # Continue to next argument.
            ++$cur_pos_arg;
        }

        # If we have any left over arguments, it's a binding fail.
        if $cur_pos_arg < $num_pos_args {
            return $TRIAL_BIND_NO_WAY;
        }

        # Otherwise, if we get there, all is well.
        return $TRIAL_BIND_OK;
    }
}
BEGIN { nqp::p6setbinder(Binder); } # We need it in for the next BEGIN block
nqp::p6setbinder(Binder);           # The load-time case.

# Container descriptors come here so that they can refer to Perl 6 types.
class ContainerDescriptor {
    has     $!of;
    has str $!name;
    has     $!default;
    has int $!dynamic;

    method BUILD(:$of, str :$name, :$default, int :$dynamic) {
        $!of := $of;
        $!name := $name;
        $!default := $default;
        $!dynamic := $dynamic;
    }

    method of() { $!of }
    method name() { $!name }
    method default() { $!default }
    method dynamic() { $!dynamic }

    method set_of($of) { $!of := $of; self }
    method set_default($default) { $!default := $default; self }
    method set_dynamic($dynamic) { $!dynamic := $dynamic; self }

    method is_generic() {
        $!of.HOW.archetypes.generic
    }

    method is_default_generic() {
        $!default.HOW.archetypes.generic
    }

    method instantiate_generic($type_environment) {
        my $ins_of := $!of.HOW.instantiate_generic($!of, $type_environment);
        my $ins_default := self.is_default_generic ?? $!default.HOW.instantiate_generic($!default, $type_environment) !! $!default;
        my $ins := nqp::clone(self);
        nqp::bindattr($ins, $?CLASS, '$!of', $ins_of);
        nqp::bindattr($ins, $?CLASS, '$!default', $ins_default);
        $ins
    }
}
class ContainerDescriptor::Untyped is ContainerDescriptor {
    # Container descriptor for when the type is Mu; the type of this
    # container descriptor is used as a marker
}
role ContainerDescriptor::Whence {
    has $!next-descriptor;

    method next() {
        my $next := $!next-descriptor;
        nqp::isconcrete($next)
            ?? $next
            !! ($!next-descriptor := nqp::gethllsym('Raku', 'default_cont_spec'))
    }
    method of() { self.next.of }
    method default() { self.next.default }
    method dynamic() { self.next.dynamic }
}
class ContainerDescriptor::BindArrayPos does ContainerDescriptor::Whence {
    has $!target;
    has int $!pos;

    method new($desc, $target, int $pos) {
        my $self := nqp::create(self);
        nqp::bindattr($self, ContainerDescriptor::BindArrayPos,
            '$!next-descriptor', $desc);
        nqp::bindattr($self, ContainerDescriptor::BindArrayPos,
            '$!target', $target);
        nqp::bindattr_i($self, ContainerDescriptor::BindArrayPos,
            '$!pos', $pos);
        $self
    }

    method name() { self.next.name ~ '[' ~ $!pos ~ ']' }
    method assigned($scalar) {
        nqp::bindpos($!target, $!pos, $scalar);
    }
}
class ContainerDescriptor::BindArrayPos2D does ContainerDescriptor::Whence {
    has $!target;
    has int $!one;
    has int $!two;

    method new($desc, $target, int $one, int $two) {
        my $self := nqp::create(self);
        nqp::bindattr($self, ContainerDescriptor::BindArrayPos2D,
            '$!next-descriptor', $desc);
        nqp::bindattr($self, ContainerDescriptor::BindArrayPos2D,
            '$!target', $target);
        nqp::bindattr_i($self, ContainerDescriptor::BindArrayPos2D,
            '$!one', $one);
        nqp::bindattr_i($self, ContainerDescriptor::BindArrayPos2D,
            '$!two', $two);
        $self
    }

    method name() {
        'element at [' ~ $!one ~ ',' ~ $!two ~ ']'  # XXX name ?
    }
    method assigned($scalar) {
        nqp::bindpos2d($!target, $!one, $!two, $scalar);
    }
}
class ContainerDescriptor::BindArrayPos3D does ContainerDescriptor::Whence {
    has $!target;
    has int $!one;
    has int $!two;
    has int $!three;

    method new($desc, $target, int $one, int $two, int $three) {
        my $self := nqp::create(self);
        nqp::bindattr($self, ContainerDescriptor::BindArrayPos3D,
            '$!next-descriptor', $desc);
        nqp::bindattr($self, ContainerDescriptor::BindArrayPos3D,
            '$!target', $target);
        nqp::bindattr_i($self, ContainerDescriptor::BindArrayPos3D,
            '$!one', $one);
        nqp::bindattr_i($self, ContainerDescriptor::BindArrayPos3D,
            '$!two', $two);
        nqp::bindattr_i($self, ContainerDescriptor::BindArrayPos3D,
            '$!three', $three);
        $self
    }

    method name() {
        'element at [' ~ $!one ~ ',' ~ $!two ~ ',' ~ $!three ~ ']'
    }
    method assigned($scalar) {
        nqp::bindpos3d($!target, $!one, $!two, $!three, $scalar);
    }
}
class ContainerDescriptor::BindArrayPosND does ContainerDescriptor::Whence {
    has $!target;
    has $!idxs;

    method new($desc, $target, $idxs) {
        my $self := nqp::create(self);
        nqp::bindattr($self, ContainerDescriptor::BindArrayPosND,
            '$!next-descriptor', $desc);
        nqp::bindattr($self, ContainerDescriptor::BindArrayPosND,
            '$!target', $target);
        nqp::bindattr($self, ContainerDescriptor::BindArrayPosND,
            '$!idxs', $idxs);
        $self
    }

    method name() { 'element of ' ~ self.next.name }  # XXX show indexes
    method assigned($scalar) {
        nqp::bindposnd($!target, $!idxs, $scalar);
    }
}
class ContainerDescriptor::BindHashPos does ContainerDescriptor::Whence {
    has $!target;
    has $!key;

    method new($desc, $target, $key) {
        my $self := nqp::create(self);
        nqp::bindattr($self, ContainerDescriptor::BindHashPos,
            '$!next-descriptor', $desc);
        nqp::bindattr($self, ContainerDescriptor::BindHashPos,
            '$!target', $target);
        nqp::bindattr($self, ContainerDescriptor::BindHashPos,
            '$!key', $key);
        $self
    }

    method name() { self.next.name ~ "\{'" ~ $!key ~ "'\}" }
    method assigned($scalar) {
        my $hash := nqp::getattr($!target, Map, '$!storage');
        $hash := nqp::bindattr($!target, Map, '$!storage', nqp::hash())
            unless nqp::isconcrete($hash);
        nqp::bindkey($hash, $!key, $scalar);
    }
}
class ContainerDescriptor::BindObjHashKey does ContainerDescriptor::Whence {
    has $!target;
    has $!key;
    has $!which;
    has $!pair;

    method new($desc, $target, $key, $which, $pair) {
        my $self := nqp::create(self);
        nqp::bindattr($self, ContainerDescriptor::BindObjHashKey,
            '$!next-descriptor', $desc);
        nqp::bindattr($self, ContainerDescriptor::BindObjHashKey,
            '$!target', $target);
        nqp::bindattr($self, ContainerDescriptor::BindObjHashKey,
            '$!key', $key);
        nqp::bindattr($self, ContainerDescriptor::BindObjHashKey,
            '$!which', $which);
        nqp::bindattr($self, ContainerDescriptor::BindObjHashKey,
            '$!pair', $pair);
        $self
    }

    method name() { 'element of ' ~ self.next.name }  # XXX correct key
    method assigned($scalar) {
        my $hash := nqp::getattr($!target, Map, '$!storage');
        $hash := nqp::bindattr($!target, Map, '$!storage', nqp::hash())
            unless nqp::isconcrete($hash);
        nqp::bindkey($hash, $!which, $!pair.new($!key, $scalar));
    }
}
class ContainerDescriptor::VivifyArray does ContainerDescriptor::Whence {
    has $!target;
    has int $!pos;

    method new($target, int $pos) {
        my $self := nqp::create(self);
        nqp::bindattr($self, ContainerDescriptor::VivifyArray,
            '$!target', $target);
        nqp::bindattr_i($self, ContainerDescriptor::VivifyArray,
            '$!pos', $pos);
        $self
    }

    method name() { self.next.name ~ '[' ~ $!pos ~ ']' }
    method assigned($scalar) {
        my $target := $!target;
        my $array := nqp::isconcrete($target)
            ?? $target
            !! nqp::assign($target, Array.new);
        $array.BIND-POS($!pos, $scalar);
    }
}
class ContainerDescriptor::VivifyHash does ContainerDescriptor::Whence {
    has $!target;
    has $!key;

    method new($target, $key) {
        my $self := nqp::create(self);
        nqp::bindattr($self, ContainerDescriptor::VivifyHash,
            '$!target', $target);
        nqp::bindattr($self, ContainerDescriptor::VivifyHash,
            '$!key', $key);
        $self
    }

    method name() { self.next.name ~ "\{'" ~ $!key ~ "'\}" }
    method assigned($scalar) {
        my $target := $!target;
        my $array := nqp::isconcrete($target)
            ?? $target
            !! nqp::assign($target, Hash.new);
        $array.BIND-KEY($!key, $scalar);
    }
}

# We stick all the declarative bits inside of a BEGIN, so they get
# serialized.
BEGIN {
    # Ensure Rakudo runtime support is initialized.
    nqp::p6init();

    # class Mu { ... }
    Mu.HOW.compose_repr(Mu);

    # class Any is Mu { ... }
    Any.HOW.add_parent(Any, Mu);
    Any.HOW.compose_repr(Any);

    # class Cool is Any { ... }
    Cool.HOW.add_parent(Cool, Any);
    Cool.HOW.compose_repr(Cool);

    # class Attribute is Any {
    #     has str $!name;
    #     has int $!rw;
    #     has int $!is_built;
    #     has int $!is_bound;
    #     has int $!has_accessor;
    #     has Mu $!type;
    #     has Mu $!container_descriptor;
    #     has Mu $!auto_viv_container;
    #     has Mu $!build_closure;
    #     has Mu $!package;
    #     has int $!inlined;
    #     has Mu $!dimensions;
    #     has int $!positional_delegate;
    #     has int $!associative_delegate;
    #     has Mu $!why;
    #     has Mu $!container_initializer;
    #     has Attribute $!original; # original attribute object used for instantiation
    Attribute.HOW.add_parent(Attribute, Any);
    Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(:name<$!name>, :type(str), :package(Attribute)));
    # The existence of both $!rw and $!ro might be confusing, but they're needed for late trait application with
    # `also is rw`. In this case we must remember the earlier applied per-attribute traits.
    Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(:name<$!rw>, :type(int), :package(Attribute)));
    Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(:name<$!ro>, :type(int), :package(Attribute)));
    Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(:name<$!required>, :type(Mu), :package(Attribute)));
    Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(:name<$!is_built>, :type(int), :package(Attribute)));
    Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(:name<$!is_bound>, :type(int), :package(Attribute)));
    Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(:name<$!has_accessor>, :type(int), :package(Attribute)));
    Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(:name<$!type>, :type(Mu), :package(Attribute)));
    Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(:name<$!container_descriptor>, :type(Mu), :package(Attribute)));
    Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(:name<$!auto_viv_container>, :type(Mu), :package(Attribute)));
    Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(:name<$!build_closure>, :type(Mu), :package(Attribute)));
    Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(:name<$!package>, :type(Mu), :package(Attribute)));
    Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(:name<$!inlined>, :type(int), :package(Attribute)));
    Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(:name<$!dimensions>, :type(Mu), :package(Attribute)));
    Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(:name<$!box_target>, :type(int), :package(Attribute)));
    Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(:name<$!positional_delegate>, :type(int), :package(Attribute)));
    Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(:name<$!associative_delegate>, :type(int), :package(Attribute)));
    Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(:name<$!why>, :type(Mu), :package(Attribute)));
    Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(:name<$!container_initializer>, :type(Mu), :package(Attribute)));
    Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(:name<$!original>, :type(Attribute), :package(Attribute)));
    Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(:name<$!composed>, :type(int), :package(Attribute)));

    # Need new and accessor methods for Attribute in here for now.
    Attribute.HOW.add_method(Attribute, 'new',
        nqp::getstaticcode(sub ($self, :$name!, :$type!, :$package!,
          :$inlined = 0, :$has_accessor = 0, :$is_built = $has_accessor,
          :$is_bound = 0, :$positional_delegate = 0, :$associative_delegate = 0,
          *%other) {
            my $attr := nqp::create($self);
            nqp::bindattr_s($attr, Attribute, '$!name', $name);
            nqp::bindattr($attr, Attribute, '$!type', nqp::decont($type));
            nqp::bindattr_i($attr, Attribute, '$!is_built', $is_built);
            nqp::bindattr_i($attr, Attribute, '$!is_bound', $is_bound);
            nqp::bindattr_i($attr, Attribute, '$!has_accessor', $has_accessor);
            nqp::bindattr($attr, Attribute, '$!package', $package);
            nqp::bindattr_i($attr, Attribute, '$!inlined', $inlined);
            nqp::bindattr($attr, Attribute, '$!original', $attr);
            if nqp::existskey(%other, 'auto_viv_primitive') {
                nqp::bindattr($attr, Attribute, '$!auto_viv_container',
                    %other<auto_viv_primitive>);
            }
            elsif nqp::existskey(%other, 'container_descriptor') {
                nqp::bindattr($attr, Attribute, '$!container_descriptor', %other<container_descriptor>);
                if nqp::existskey(%other, 'auto_viv_container') {
                    nqp::bindattr($attr, Attribute, '$!auto_viv_container',
                        %other<auto_viv_container>);
                }
            }
            else {
                my $cd := ContainerDescriptor.new(:of($type), :$name);
                my $scalar := nqp::create(Scalar);
                nqp::bindattr($scalar, Scalar, '$!descriptor', $cd);
                nqp::bindattr($scalar, Scalar, '$!value', $type);
                nqp::bindattr($attr, Attribute, '$!container_descriptor', $cd);
                nqp::bindattr($attr, Attribute, '$!auto_viv_container', $scalar);
            }
            if nqp::existskey(%other, 'container_initializer') {
                nqp::bindattr($attr, Attribute, '$!container_initializer',
                    %other<container_initializer>);
            }
            nqp::bindattr_i($attr, Attribute, '$!positional_delegate', $positional_delegate);
            nqp::bindattr_i($attr, Attribute, '$!associative_delegate', $associative_delegate);
            if nqp::existskey(%other, 'build') {
                $attr.set_build(%other<build>);
            }
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
    Attribute.HOW.add_method(Attribute, 'is_built', nqp::getstaticcode(sub ($self) {
            nqp::hllboolfor(nqp::getattr_i(nqp::decont($self),
                Attribute, '$!is_built'), "Raku");
        }));
    Attribute.HOW.add_method(Attribute, 'is_bound', nqp::getstaticcode(sub ($self) {
            nqp::hllboolfor(nqp::getattr_i(nqp::decont($self),
                Attribute, '$!is_bound'), "Raku");
        }));
    Attribute.HOW.add_method(Attribute, 'has_accessor', nqp::getstaticcode(sub ($self) {
            nqp::hllboolfor(nqp::getattr_i(nqp::decont($self),
                Attribute, '$!has_accessor'), "Raku");
        }));
    Attribute.HOW.add_method(Attribute, 'rw', nqp::getstaticcode(sub ($self) {
            nqp::hllboolfor(nqp::getattr_i(nqp::decont($self),
                Attribute, '$!rw'), "Raku");
        }));
    Attribute.HOW.add_method(Attribute, 'set_rw', nqp::getstaticcode(sub ($self) {
            nqp::bindattr_i(nqp::decont($self),
                Attribute, '$!rw', 1);
            nqp::hllboolfor(1, "Raku")
        }));
    Attribute.HOW.add_method(Attribute, 'set_readonly', nqp::getstaticcode(sub ($self) {
            nqp::bindattr_i(nqp::decont($self),
                Attribute, '$!ro', 1);
            # Explicit set of readonly must reset rw as it might be a result of `is rw` trait.
            nqp::bindattr_i(nqp::decont($self),
                Attribute, '$!rw', 0);
            nqp::hllboolfor(1, "Raku")
        }));
    Attribute.HOW.add_method(Attribute, 'set_required', nqp::getstaticcode(sub ($self, $value) {
            $*W.add_object_if_no_sc($value);
            nqp::bindattr(nqp::decont($self),
                Attribute, '$!required', $value);
            nqp::hllboolfor(1, "Raku")
        }));
    Attribute.HOW.add_method(Attribute, 'required', nqp::getstaticcode(sub ($self) {
            nqp::getattr(nqp::decont($self),
                Attribute, '$!required');
        }));
    Attribute.HOW.add_method(Attribute, 'default_to_rw', nqp::getstaticcode(sub ($self) {
            my $dcself := nqp::decont($self);
            unless nqp::getattr_i($dcself, Attribute, '$!ro') {
                nqp::bindattr_i($dcself, Attribute, '$!rw', 1);
            }
            nqp::hllboolfor(1, "Raku")
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
            nqp::hllboolfor(1, "Raku")
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
    Attribute.HOW.add_method(Attribute, 'container_initializer', nqp::getstaticcode(sub ($self) {
            nqp::getattr(nqp::decont($self),
                Attribute, '$!container_initializer');
        }));
    Attribute.HOW.add_method(Attribute, 'original', nqp::getstaticcode(sub ($self) {
            nqp::getattr(nqp::decont($self),
                Attribute, '$!original');
        }));
    Attribute.HOW.add_method(Attribute, 'is_generic', nqp::getstaticcode(sub ($self) {
            my $dcself   := nqp::decont($self);
            my $type := nqp::getattr(nqp::decont($dcself),
                Attribute, '$!type');
            my $package := nqp::getattr(nqp::decont($dcself),
                Attribute, '$!package');
            my $build := nqp::getattr(nqp::decont($dcself),
                Attribute, '$!build_closure');
            nqp::hllboolfor($type.HOW.archetypes.generic || $package.HOW.archetypes.generic || nqp::defined($build), "Raku");
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
                my $avc_copy := nqp::clone_nd($avc);
                my @avc_mro  := nqp::how_nd($avc).mro($avc);
                my int $i := 0;
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
    #     has Mu $!descriptor;
    #     has Mu $!value;
    Scalar.HOW.add_parent(Scalar, Any);
    Scalar.HOW.add_attribute(Scalar, BOOTSTRAPATTR.new(:name<$!descriptor>, :type(Mu), :package(Scalar)));
    Scalar.HOW.add_attribute(Scalar, BOOTSTRAPATTR.new(:name<$!value>, :type(Mu), :package(Scalar)));
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
        $dcself
    }));
    Scalar.HOW.compose_repr(Scalar);

    # To preserve historical behavior, we never repossess a Scalar container.
    nqp::neverrepossess(Scalar);

    # Scalar needs to be registered as a container type. Also provide the
    # slow-path implementation of various container operations.
    nqp::setcontspec(Scalar, 'value_desc_cont', nqp::hash(
        'attrs_class', Scalar,
        'descriptor_attr', '$!descriptor',
        'value_attr', '$!value',
        'store', nqp::getstaticcode(sub ($cont, $val) {
            my $desc := nqp::getattr($cont, Scalar, '$!descriptor');
            if nqp::isconcrete($desc) {
                $val := $desc.default if nqp::eqaddr($val.WHAT, Nil);
                my $type := $desc.of;
                if nqp::eqaddr($type, Mu) || nqp::istype($val, $type) {
                    if $type.HOW.archetypes.coercive {
                        my $coercion_type := $type.HOW.wrappee($type, :coercion);
                        nqp::bindattr($cont, Scalar, '$!value', $coercion_type.HOW.coerce($coercion_type, $val));
                    }
                    else {
                        nqp::bindattr($cont, Scalar, '$!value', $val);
                    }
                    unless nqp::eqaddr($desc.WHAT, ContainerDescriptor) ||
                           nqp::eqaddr($desc.WHAT, ContainerDescriptor::Untyped) {
                        $desc.assigned($cont);
                        nqp::bindattr($cont, Scalar, '$!descriptor', $desc.next);
                    }
                }
                else {
                    Perl6::Metamodel::Configuration.throw_or_die(
                        'X::TypeCheck::Assignment',
                        "Type check failed in assignment",
                        :symbol($desc.name),
                        :got($val),
                        :expected($type)
                    );
                }
            }
            else {
                nqp::die("Cannot assign to a readonly variable or a value");
            }
        }),
        'store_unchecked', nqp::getstaticcode(sub ($cont, $val) {
            nqp::bindattr($cont, Scalar, '$!value', $val);
            my $desc := nqp::getattr($cont, Scalar, '$!descriptor');
            unless nqp::eqaddr($desc.WHAT, ContainerDescriptor) ||
                   nqp::eqaddr($desc.WHAT, ContainerDescriptor::Untyped) {
                $desc.assigned($cont);
                nqp::bindattr($cont, Scalar, '$!descriptor', $desc.next);
            }
        }),
        'cas', nqp::getstaticcode(sub ($cont, $expected, $val) {
            my $desc := nqp::getattr($cont, Scalar, '$!descriptor');
            if nqp::isconcrete($desc) {
                $val := $desc.default if nqp::eqaddr($val.WHAT, Nil);
                my $type := $desc.of;
                if nqp::eqaddr($type, Mu) || nqp::istype($val, $type) {
                    nqp::casattr($cont, Scalar, '$!value', $expected, $val);
                }
                else {
                    Perl6::Metamodel::Configuration.throw_or_die(
                        'X::TypeCheck::Assignment',
                        "Type check failed in assignment",
                        :symbol($desc.name),
                        :got($val),
                        :expected($type)
                    );
                }
            }
            else {
                nqp::die("Cannot assign to a readonly variable or a value");
            }
        }),
        'atomic_store', nqp::getstaticcode(sub ($cont, $val) {
            my $desc := nqp::getattr($cont, Scalar, '$!descriptor');
            if nqp::isconcrete($desc) {
                $val := $desc.default if nqp::eqaddr($val.WHAT, Nil);
                my $type := $desc.of;
                if nqp::eqaddr($type, Mu) || nqp::istype($val, $type) {
                    nqp::atomicbindattr($cont, Scalar, '$!value', $val);
                }
                else {
                    Perl6::Metamodel::Configuration.throw_or_die(
                        'X::TypeCheck::Assignment',
                        "Type check failed in assignment",
                        :symbol($desc.name),
                        :got($val),
                        :expected($type)
                    );
                }
            }
            else {
                nqp::die("Cannot assign to a readonly variable or a value");
            }
        }),
    ));

    # Cache a single default Scalar container spec, to ensure we only get
    # one of them.
    Scalar.HOW.cache_add(Scalar, 'default_cont_spec',
        ContainerDescriptor::Untyped.new(
            :of(Mu), :default(Any), :name('element')));

    # Set up various native reference types.
    sub setup_native_ref_type($type, $primitive, $ref_kind) {
        $type.HOW.add_parent($type, Any);
        $type.HOW.set_native_type($type, $primitive);
        $type.HOW.set_ref_kind($type, $ref_kind);
        $type.HOW.compose_repr($type);
        nqp::setcontspec($type, 'native_ref', nqp::null());
    }
    setup_native_ref_type(IntLexRef, int, 'lexical');
    setup_native_ref_type(NumLexRef, num, 'lexical');
    setup_native_ref_type(StrLexRef, str, 'lexical');
    setup_native_ref_type(IntAttrRef, int, 'attribute');
    setup_native_ref_type(NumAttrRef, num, 'attribute');
    setup_native_ref_type(StrAttrRef, str, 'attribute');
    setup_native_ref_type(IntPosRef, int, 'positional');
    setup_native_ref_type(NumPosRef, num, 'positional');
    setup_native_ref_type(StrPosRef, str, 'positional');
    setup_native_ref_type(IntMultidimRef, int, 'multidim');
    setup_native_ref_type(NumMultidimRef, num, 'multidim');
    setup_native_ref_type(StrMultidimRef, str, 'multidim');

#?if js
    setup_native_ref_type(Int64LexRef, int64, 'lexical');
    setup_native_ref_type(Int64AttrRef, int64, 'attribute');
    setup_native_ref_type(Int64PosRef, int64, 'positional');
    setup_native_ref_type(Int64MultidimRef, int64, 'multidim');
#?endif

    # class Proxy is Any {
    #    has Mu &!FETCH;
    #    has Mu &!STORE;
    my $PROXY_FETCH;
    my $PROXY_STORE;
    Proxy.HOW.add_parent(Proxy, Any);
    Proxy.HOW.add_attribute(Proxy, BOOTSTRAPATTR.new(:name<&!FETCH>, :type(Mu), :package(Proxy)));
    Proxy.HOW.add_attribute(Proxy, BOOTSTRAPATTR.new(:name<&!STORE>, :type(Mu), :package(Proxy)));
    Proxy.HOW.add_method(Proxy, 'FETCH', ($PROXY_FETCH := nqp::getstaticcode(sub ($cont) {
        my $var := nqp::create(Scalar);
        nqp::bindattr($var, Scalar, '$!value', $cont);
        nqp::decont(nqp::getattr($cont, Proxy, '&!FETCH')($var))
    })));
    Proxy.HOW.add_method(Proxy, 'STORE', ($PROXY_STORE := nqp::getstaticcode(sub ($cont, $val) {
        my $var := nqp::create(Scalar);
        nqp::bindattr($var, Scalar, '$!value', $cont);
        nqp::getattr($cont, Proxy, '&!STORE')($var, $val)
    })));
    Proxy.HOW.add_method(Proxy, 'new', nqp::getstaticcode(sub ($type, :$FETCH!, :$STORE!) {
        my $cont := nqp::create(nqp::decont($type));
        nqp::bindattr($cont, Proxy, '&!FETCH', $FETCH);
        nqp::bindattr($cont, Proxy, '&!STORE', $STORE);
        $cont
    }));
    Proxy.HOW.set_container_spec(Proxy, nqp::hash(
        'fetch', $PROXY_FETCH,
        'store', $PROXY_STORE
    ));
    Proxy.HOW.compose(Proxy);
    Proxy.HOW.compose_repr(Proxy);

    # Helper for creating a scalar attribute. Sets it up as a real Perl 6
    # Attribute instance, complete with container descriptor and optional
    # auto-viv container.
    sub scalar_attr($name, $type, $package, :$associative_delegate, :$auto_viv_container = 1) {
        my $cd := ContainerDescriptor.new(:of($type), :$name);
        if $auto_viv_container {
            my $scalar := nqp::create(Scalar);
            nqp::bindattr($scalar, Scalar, '$!descriptor', $cd);
            nqp::bindattr($scalar, Scalar, '$!value', $type);
            return Attribute.new( :$name, :$type, :$package,
                :container_descriptor($cd), :auto_viv_container($scalar),
                :$associative_delegate );
        }
        else {
            return Attribute.new( :$name, :$type, :$package,
                :container_descriptor($cd), :$associative_delegate );
        }
    }

    # Helper for creating an attribute that vivifies to a clone of some VM
    # storage type (or, if it's a type object, is just initialized with that
    # type object); used for the storage slots of arrays and hashes.
    sub storage_attr($name, $type, $package, $clonee, :$associative_delegate) {
        return Attribute.new( :$name, :$type, :$package, :auto_viv_primitive($clonee),
            :$associative_delegate );
    }

    # class Signature is Any{
    #    has @!params;
    #    has Mu $!returns;
    #    has int $!arity;
    #    has Num $!count;
    #    has Code $!code;
    Signature.HOW.add_parent(Signature, Any);
    Signature.HOW.add_attribute(Signature, Attribute.new(:name<@!params>, :type(List), :package(Signature)));
    Signature.HOW.add_attribute(Signature, scalar_attr('$!returns', Mu, Signature, :!auto_viv_container));
    Signature.HOW.add_attribute(Signature, Attribute.new(:name<$!arity>, :type(int), :package(Signature)));
    Signature.HOW.add_attribute(Signature, Attribute.new(:name<$!count>, :type(Num), :package(Signature)));
    Signature.HOW.add_attribute(Signature, Attribute.new(:name<$!code>, :type(Code), :package(Signature)));
    Signature.HOW.add_method(Signature, 'is_generic', nqp::getstaticcode(sub ($self) {
            # If any parameter is generic, so are we.
            my @params := nqp::getattr($self, Signature, '@!params');
            for @params {
                my $is_generic := $_.is_generic();
                if $is_generic { return $is_generic }
            }
            return nqp::hllboolfor(0, "Raku");
        }));
    Signature.HOW.add_method(Signature, 'instantiate_generic', nqp::getstaticcode(sub ($self, $type_environment) {
            # Go through parameters, builidng new list. If any
            # are generic, instantiate them. Otherwise leave them
            # as they are.
            my $ins    := nqp::clone($self);
            my @params := nqp::getattr($self, Signature, '@!params');
            my @ins_params;
            for @params {
                if $_.is_generic() {
                    @ins_params.push($_.instantiate_generic($type_environment))
                }
                else {
                    @ins_params.push($_);
                }
            }
            nqp::bindattr($ins, Signature, '@!params', @ins_params);
            my $returns := nqp::getattr($self, Signature, '$!returns');
            if !nqp::isnull($returns) && $returns.HOW.archetypes.generic {
                nqp::bindattr($ins, Signature, '$!returns',
                    $returns.HOW.instantiate_generic($returns, $type_environment));
            }
            $ins
        }));
    Signature.HOW.add_method(Signature, 'returns', nqp::getstaticcode(sub ($self) {
        nqp::getattr(nqp::decont($self),Signature,'$!returns')
        }));
    Signature.HOW.add_method(Signature, 'set_returns', nqp::getstaticcode(sub ($self, $type) {
            nqp::bindattr(nqp::decont($self),
                Signature, '$!returns', nqp::decont($type));
        }));
    Signature.HOW.add_method(Signature, 'has_returns', nqp::getstaticcode(sub ($self) {
            nqp::hllboolfor(
                nqp::not_i(
                    nqp::isnull(
                        nqp::getattr(nqp::decont($self),
                            Signature, '$!returns')
                    )
                ),
                'Raku'
            );
        }));
    Signature.HOW.compose_repr(Signature);

    # class Parameter is Any {
    #     has str $!variable_name
    #     has @!named_names
    #     has @!type_captures
    #     has int $!flags
    #     has Mu $!type
    #     has @!post_constraints
    #     has Signature $!sub_signature
    #     has Code $!default_value
    #     has Mu $!container_descriptor;
    #     has Mu $!attr_package;
    #     has Mu $!why;
    Parameter.HOW.add_parent(Parameter, Any);
    Parameter.HOW.add_attribute(Parameter, Attribute.new(:name<$!variable_name>, :type(str), :package(Parameter)));
    Parameter.HOW.add_attribute(Parameter, scalar_attr('@!named_names', Mu, Parameter, :!auto_viv_container));
    Parameter.HOW.add_attribute(Parameter, scalar_attr('@!type_captures', Mu, Parameter, :!auto_viv_container));
    Parameter.HOW.add_attribute(Parameter, Attribute.new(:name<$!flags>, :type(int), :package(Parameter)));
    Parameter.HOW.add_attribute(Parameter, Attribute.new(:name<$!type>, :type(Mu), :package(Parameter)));
    Parameter.HOW.add_attribute(Parameter, scalar_attr('@!post_constraints', List, Parameter, :!auto_viv_container));
    Parameter.HOW.add_attribute(Parameter, scalar_attr('$!sub_signature', Signature, Parameter, :!auto_viv_container));
    Parameter.HOW.add_attribute(Parameter, scalar_attr('$!default_value', Code, Parameter, :!auto_viv_container));
    Parameter.HOW.add_attribute(Parameter, scalar_attr('$!container_descriptor', Mu, Parameter, :!auto_viv_container));
    Parameter.HOW.add_attribute(Parameter, Attribute.new(:name<$!attr_package>, :type(Mu), :package(Parameter)));
    Parameter.HOW.add_attribute(Parameter, Attribute.new(:name<$!why>, :type(Mu), :package(Parameter)));
    Parameter.HOW.add_method(Parameter, 'is_generic', nqp::getstaticcode(sub ($self) {
            # If nonimnal type or attr_package is generic, so are we.
            my $type := nqp::getattr($self, Parameter, '$!type');
            my $ap   := nqp::getattr($self, Parameter, '$!attr_package');
            nqp::hllboolfor($type.HOW.archetypes.generic ||
                (!nqp::isnull($ap) && $ap.HOW.archetypes.generic), "Raku")
        }));
    Parameter.HOW.add_method(Parameter, 'instantiate_generic', nqp::getstaticcode(sub ($self, $type_environment) {
            # Clone with the type instantiated.
            my int $SIG_ELEM_TYPE_GENERIC := 524288;
            my int $SIG_ELEM_IS_COERCIVE  := 67108864;
            my $ins      := nqp::clone($self);
            my $type     := nqp::getattr($self, Parameter, '$!type');
            my $cd       := nqp::getattr($self, Parameter, '$!container_descriptor');
            my $ap       := nqp::getattr($self, Parameter, '$!attr_package');
            my $ins_type := $type;
            my $ins_cd   := $cd;
            if $type.HOW.archetypes.generic {
                $ins_type := $type.HOW.instantiate_generic($type, $type_environment);
                $ins_cd   := nqp::isnull($cd) ?? $cd !! $cd.instantiate_generic($type_environment);
            }
            my $ins_ap :=
                !nqp::isnull($ap) && $ap.HOW.archetypes.generic
                    ?? $ap.HOW.instantiate_generic($ap, $type_environment)
                    !! $ap;
            my int $flags := nqp::getattr_i($ins, Parameter, '$!flags');
            unless $ins_type.HOW.archetypes.generic {
                if $flags +& $SIG_ELEM_TYPE_GENERIC {
                    nqp::bindattr_i($ins, Parameter, '$!flags', $flags - $SIG_ELEM_TYPE_GENERIC);
                }
            }
            my $archetypes := $ins_type.HOW.archetypes;
            if nqp::can($archetypes, 'coercive') && $archetypes.coercive {
                nqp::bindattr_i($ins, Parameter, '$!flags', $flags +| $SIG_ELEM_IS_COERCIVE);
            }
            nqp::bindattr($ins, Parameter, '$!type', $ins_type);
            nqp::bindattr($ins, Parameter, '$!container_descriptor', $ins_cd);
            nqp::bindattr($ins, Parameter, '$!attr_package', $ins_ap);
            $ins
        }));
    Parameter.HOW.add_method(Parameter, 'set_rw', nqp::getstaticcode(sub ($self) {
            my $SIG_ELEM_IS_RW       := 256;
            my $SIG_ELEM_IS_OPTIONAL := 2048;
            my $dcself := nqp::decont($self);
            my str $varname := nqp::getattr_s($dcself, Parameter, '$!variable_name');
            unless nqp::isnull_s($varname) || nqp::eqat($varname, '$', 0) {
                my $error;
                if nqp::eqat($varname, '%', 0) || nqp::eqat($varname, '@', 0)  {
                    my $sig := nqp::substr($varname, 0, 1);
                    $error := "For parameter '$varname', '$sig' sigil containers don't need 'is rw' to be writable\n";
                }
                $error := $error ~ "Can only use 'is rw' on a scalar ('\$' sigil) parameter, not '$varname'";
                nqp::die($error);
            }
            my int $flags := nqp::getattr_i($dcself, Parameter, '$!flags');
            if $flags +& $SIG_ELEM_IS_OPTIONAL {
                Perl6::Metamodel::Configuration.throw_or_die(
                    'X::Trait::Invalid',
                    "Cannot use 'is rw' on optional parameter '$varname'",
                    :type('is'),
                    :subtype('rw'),
                    :declaring('optional parameter'),
                    :name($varname)
                );
            }
            nqp::bindattr_i($dcself, Parameter, '$!flags', $flags + $SIG_ELEM_IS_RW);
            $dcself
        }));
    Parameter.HOW.add_method(Parameter, 'set_copy', nqp::getstaticcode(sub ($self) {
            my $SIG_ELEM_IS_COPY := 512;
            my $dcself := nqp::decont($self);
            nqp::bindattr_i($dcself, Parameter, '$!flags',
                nqp::getattr_i($dcself, Parameter, '$!flags') + $SIG_ELEM_IS_COPY);
            $dcself
        }));
    Parameter.HOW.add_method(Parameter, 'set_required', nqp::getstaticcode(sub ($self) {
            my $SIG_ELEM_IS_OPTIONAL := 2048;
            my $dcself := nqp::decont($self);
            my int $flags := nqp::getattr_i($dcself, Parameter, '$!flags');
            if $flags +& $SIG_ELEM_IS_OPTIONAL {
                nqp::bindattr_i($dcself, Parameter, '$!flags',
                    $flags - $SIG_ELEM_IS_OPTIONAL);
            }
            $dcself
        }));
    Parameter.HOW.add_method(Parameter, 'set_raw', nqp::getstaticcode(sub ($self) {
            my $SIG_ELEM_IS_RAW := 1024;
            my $dcself := nqp::decont($self);
            my int $flags := nqp::getattr_i($dcself, Parameter, '$!flags');
            unless $flags +& $SIG_ELEM_IS_RAW {
                nqp::bindattr_i($dcself, Parameter, '$!flags',
                    $flags + $SIG_ELEM_IS_RAW);
            }
            $dcself
        }));
    Parameter.HOW.add_method(Parameter, 'set_onearg', nqp::getstaticcode(sub ($self) {
            my $SIG_ELEM_SLURPY_ONEARG := 16777216;
            my $dcself := nqp::decont($self);
            my int $flags := nqp::getattr_i($dcself, Parameter, '$!flags');
            unless $flags +& $SIG_ELEM_SLURPY_ONEARG {
                nqp::bindattr_i($dcself, Parameter, '$!flags',
                    $flags + $SIG_ELEM_SLURPY_ONEARG);
            }
            $dcself
        }));
    Parameter.HOW.add_method(Parameter, 'WHY', nqp::getstaticcode(sub ($self) {
            my $why := nqp::getattr(nqp::decont($self), Parameter, '$!why');
            if nqp::isnull($why) || !$why {
                Nil
            } else {
                $why.set_docee($self);
                $why
            }
        }));
    Parameter.HOW.add_method(Parameter, 'coercive', nqp::getstaticcode(sub ($self) {
            #my int $SIG_ELEM_IS_COERCIVE  := 67108864;
            nqp::if(nqp::bitand_i(nqp::getattr(nqp::decont($self), Parameter, '$!flags'), 67108864), 1, 0)
        }));
    Parameter.HOW.compose_repr(Parameter);

    # class Code {
    #     has Code $!do;              # Low level code object
    #     has Signature $!signature;  # Signature object
    #     has @!compstuff;            # Place for the compiler to hang stuff
    Code.HOW.add_parent(Code, Any);
    Code.HOW.add_attribute(Code, Attribute.new(:name<$!do>, :type(Code), :package(Code)));
    Code.HOW.add_attribute(Code, Attribute.new(:name<$!signature>, :type(Signature), :package(Code)));
    Code.HOW.add_attribute(Code, scalar_attr('@!compstuff', List, Code, :!auto_viv_container));

    # Need clone in here, plus generics instantiation.
    Code.HOW.add_method(Code, 'clone', nqp::getstaticcode(sub ($self) {
            my $dcself    := nqp::decont($self);
            return $dcself unless nqp::isconcrete($dcself);

            my $cloned    := nqp::clone($dcself);
            my $do        := nqp::getattr($dcself, Code, '$!do');
            my $do_cloned := nqp::clone($do);
            nqp::bindattr($cloned, Code, '$!do', $do_cloned);
            nqp::setcodeobj($do_cloned, $cloned);
            my $compstuff := nqp::getattr($dcself, Code, '@!compstuff');
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
            if nqp::defined(nqp::getattr($dcself, Routine, '@!dispatchees')) {
                nqp::bindattr($ins, Routine, '@!dispatchees',
                    nqp::clone(nqp::getattr($dcself, Routine, '@!dispatchees')));
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
    Code.HOW.set_invocation_attr(Code, Code, '$!do');
    Code.HOW.compose_invocation(Code);

    # class Block is Code {
    #     has Mu $!phasers;                # phasers for this block
    #     has Mu $!why;
    Block.HOW.add_parent(Block, Code);
    Block.HOW.add_attribute(Block, BOOTSTRAPATTR.new(:name<$!phasers>, :type(Mu), :package(Block)));
    Block.HOW.add_attribute(Block, scalar_attr('$!why', Mu, Block, :!auto_viv_container));
    Block.HOW.add_method(Block, 'clone', nqp::getstaticcode(sub ($self) {
            my $dcself := nqp::decont($self);
            if nqp::isconcrete($dcself) {
                my $cloned    := nqp::clone($dcself);
                my $do        := nqp::getattr($dcself, Code, '$!do');
                my $do_cloned := nqp::clone($do);
                nqp::bindattr($cloned, Code, '$!do', $do_cloned);
                nqp::setcodeobj($do_cloned, $cloned);
#?if !jvm
                my $phasers := nqp::getattr($dcself, Block, '$!phasers');
                if nqp::isconcrete($phasers) {
                    $dcself."!clone_phasers"($cloned, $phasers);
                }
#?endif
                my $compstuff := nqp::getattr($dcself, Code, '@!compstuff');
                unless nqp::isnull($compstuff) {
                    nqp::atpos($compstuff, 2)($do, $cloned);
                }
                # XXX this should probably be done after the clone that installs
                #     the sub
                my $why := nqp::getattr($dcself, Block, '$!why');
                unless nqp::isnull($why) {
                    $why.set_docee($cloned);
                }
                $cloned
            }
            else {
                $dcself
            }
        }));
    Block.HOW.add_method(Block, '!clone_phasers', nqp::getstaticcode(sub ($self, $cloned, $phasers) {
#?if !jvm
            my int $next := nqp::existskey($phasers, 'NEXT');
            my int $last := nqp::existskey($phasers, 'LAST');
            my int $quit := nqp::existskey($phasers, 'QUIT');
            my int $close := nqp::existskey($phasers, 'CLOSE');
            if $next +| $last +| $quit +| $close {
                my %pclone := nqp::clone($phasers);
                if $next {
                    my @nexts := nqp::clone($phasers<NEXT>);
                    my int $i := -1;
                    while ++$i < nqp::elems(@nexts) {
                        @nexts[$i] := @nexts[$i].clone();
                    }
                    %pclone<NEXT> := @nexts;
                }
                if $last {
                    my @lasts := nqp::clone($phasers<LAST>);
                    my int $i := -1;
                    while ++$i < nqp::elems(@lasts) {
                        nqp::captureinnerlex(nqp::getattr(
                            (@lasts[$i] := @lasts[$i].clone()),
                            Code, '$!do'));
                    }
                    %pclone<LAST> := @lasts;
                }
                if $quit {
                    my @quits := nqp::clone($phasers<QUIT>);
                    my int $i := -1;
                    while ++$i < nqp::elems(@quits) {
                        nqp::captureinnerlex(nqp::getattr(
                            (@quits[$i] := @quits[$i].clone()),
                            Code, '$!do'));
                    }
                    %pclone<QUIT> := @quits;
                }
                if $close {
                    my @closes := nqp::clone($phasers<CLOSE>);
                    my int $i := -1;
                    while ++$i < nqp::elems(@closes) {
                        nqp::captureinnerlex(nqp::getattr(
                            (@closes[$i] := @closes[$i].clone()),
                            Code, '$!do'));
                    }
                    %pclone<CLOSE> := @closes;
                }
                nqp::bindattr($cloned, Block, '$!phasers', %pclone);
            }
#?endif
        }));
    Block.HOW.add_method(Block, '!capture_phasers', nqp::getstaticcode(sub ($self) {
            my $dcself    := nqp::decont($self);
#?if !jvm
            my $phasers   := nqp::getattr($dcself, Block, '$!phasers');
            if nqp::isconcrete($phasers) {
                my @next := nqp::atkey($phasers, 'NEXT');
                if nqp::islist(@next) {
                    my int $i := -1;
                    while ++$i < nqp::elems(@next) {
                        nqp::p6capturelexwhere(@next[$i]);
                    }
                }
                my @last := nqp::atkey($phasers, 'LAST');
                if nqp::islist(@last) {
                    my int $i := -1;
                    while ++$i < nqp::elems(@last) {
                        nqp::p6capturelexwhere(@last[$i]);
                    }
                }
                my @quit := nqp::atkey($phasers, 'QUIT');
                if nqp::islist(@quit) {
                    my int $i := -1;
                    while ++$i < nqp::elems(@quit) {
                        nqp::p6capturelexwhere(@quit[$i]);
                    }
                }
                my @close := nqp::atkey($phasers, 'CLOSE');
                if nqp::islist(@close) {
                    my int $i := -1;
                    while ++$i < nqp::elems(@close) {
                        nqp::p6capturelexwhere(@close[$i]);
                    }
                }
            }
#?endif
            $dcself
    }));
    Block.HOW.compose_repr(Block);
    Block.HOW.compose_invocation(Block);

    # class Routine is Block {
    #     has @!dispatchees;
    #     has Mu $!dispatcher_cache;
    #     has Mu $!dispatcher;
    #     has int $!flags;
    #     has Mu $!inline_info;
    #     has Mu $!package;
    #     has int $!onlystar;
    #     has @!dispatch_order;
    #     has Mu $!dispatch_cache;
    Routine.HOW.add_parent(Routine, Block);
    Routine.HOW.add_attribute(Routine, Attribute.new(:name<@!dispatchees>, :type(List), :package(Routine)));
    Routine.HOW.add_attribute(Routine, Attribute.new(:name<$!dispatcher_cache>, :type(Mu), :package(Routine)));
    Routine.HOW.add_attribute(Routine, Attribute.new(:name<$!dispatcher>, :type(Mu), :package(Routine)));
    Routine.HOW.add_attribute(Routine, Attribute.new(:name<$!flags>, :type(int), :package(Routine)));
    Routine.HOW.add_attribute(Routine, Attribute.new(:name<$!inline_info>, :type(Mu), :package(Routine)));
    Routine.HOW.add_attribute(Routine, Attribute.new(:name<$!package>, :type(Mu), :package(Routine)));
    Routine.HOW.add_attribute(Routine, Attribute.new(:name<$!onlystar>, :type(int), :package(Routine)));
    Routine.HOW.add_attribute(Routine, scalar_attr('@!dispatch_order', List, Routine, :!auto_viv_container));
    Routine.HOW.add_attribute(Routine, Attribute.new(:name<$!dispatch_cache>, :type(Mu), :package(Routine)));

    Routine.HOW.add_method(Routine, 'is_dispatcher', nqp::getstaticcode(sub ($self) {
            my $dc_self   := nqp::decont($self);
            my $disp_list := nqp::getattr($dc_self, Routine, '@!dispatchees');
            nqp::hllboolfor(nqp::defined($disp_list), "Raku");
        }));
    Routine.HOW.add_method(Routine, 'add_dispatchee', nqp::getstaticcode(sub ($self, $dispatchee) {
            my $dc_self   := nqp::decont($self);
            my $disp_list := nqp::getattr($dc_self, Routine, '@!dispatchees');
            if nqp::defined($disp_list) {
                $disp_list.push($dispatchee);
                nqp::bindattr(nqp::decont($dispatchee),
                    Routine, '$!dispatcher', $dc_self);
                nqp::scwbdisable();
                nqp::bindattr($dc_self, Routine, '@!dispatch_order', nqp::null());
                nqp::bindattr($dc_self, Routine, '$!dispatch_cache', nqp::null());
                nqp::bindattr($dc_self, Routine, '$!dispatcher_cache', nqp::null());
                nqp::scwbenable();
                $dc_self
            }
            else {
                nqp::die("Cannot add dispatchee '" ~ $dispatchee.name() ~
                         "' to non-dispatcher code object '" ~ $self.name() ~ "'");
            }
        }));
    Routine.HOW.add_method(Routine, 'derive_dispatcher', nqp::getstaticcode(sub ($self) {
            my $clone := $self.clone();
            nqp::bindattr($clone, Routine, '@!dispatchees',
                nqp::clone(nqp::getattr($self, Routine, '@!dispatchees')));
            $clone
        }));
    Routine.HOW.add_method(Routine, 'dispatcher', nqp::getstaticcode(sub ($self) {
            nqp::getattr(nqp::decont($self),
                Routine, '$!dispatcher')
        }));
    Routine.HOW.add_method(Routine, 'dispatchees', nqp::getstaticcode(sub ($self) {
            nqp::getattr(nqp::decont($self),
                Routine, '@!dispatchees')
        }));
    Routine.HOW.add_method(Routine, '!configure_positional_bind_failover',
        nqp::getstaticcode(sub ($self, $Positional, $PositionalBindFailover) {
            nqp::bindhllsym('Raku', 'MD_Pos', $Positional);
            nqp::bindhllsym('Raku', 'MD_PBF', $PositionalBindFailover);
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
            my int $SIG_ELEM_SLURPY_LOL          := 32;
            my int $SIG_ELEM_MULTI_INVOCANT      := 128;
            my int $SIG_ELEM_IS_RW               := 256;
            my int $SIG_ELEM_IS_OPTIONAL         := 2048;
            my int $SIG_ELEM_IS_CAPTURE          := 32768;
            my int $SIG_ELEM_UNDEFINED_ONLY      := 65536;
            my int $SIG_ELEM_DEFINED_ONLY        := 131072;
            my int $SIG_ELEM_TYPE_GENERIC        := 524288;
            my int $SIG_ELEM_NATIVE_INT_VALUE    := 2097152;
            my int $SIG_ELEM_NATIVE_NUM_VALUE    := 4194304;
            my int $SIG_ELEM_NATIVE_STR_VALUE    := 8388608;
            my int $SIG_ELEM_SLURPY_ONEARG       := 16777216;

            # Takes two candidates and determines if the first one is narrower
            # than the second. Returns a true value if they are.
            sub is_narrower(%a, %b) {
                # Work out how many parameters to compare, factoring in
                # slurpiness and optionals.
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
                my int $narrower := 0;
                my int $tied := 0;
                my int $i := -1;
                while ++$i < $types_to_check {
                    my $type_obj_a := %a<types>[$i];
                    my $type_obj_b := %b<types>[$i];
                    if nqp::eqaddr($type_obj_a, $type_obj_b) {
                        # Same type; narrower if first has constraints and other doesn't;
                        # narrower if first is rw and second isn't; tied if neither has
                        # constraints or both have constraints.
                        if %a<constraints>[$i] && !%b<constraints>[$i] {
                            ++$narrower;
                        }
                        elsif nqp::atpos_i(%a<rwness>, $i) > nqp::atpos_i(%b<rwness>, $i) {
                            ++$narrower;
                        }
                        elsif (!%a<constraints>[$i] && !%b<constraints>[$i])
                           || (%a<constraints>[$i] && %b<constraints>[$i]) {
                            ++$tied;
                        }
                    }
                    elsif (nqp::atpos_i(%a<type_flags>, $i) +& $TYPE_NATIVE_MASK)
                      && !(nqp::atpos_i(%b<type_flags>, $i) +& $TYPE_NATIVE_MASK) {
                        # Narrower because natives always are.
                        ++$narrower;
                    }
                    elsif (nqp::atpos_i(%b<type_flags>, $i) +& $TYPE_NATIVE_MASK)
                      && !(nqp::atpos_i(%a<type_flags>, $i) +& $TYPE_NATIVE_MASK) {
                        # Wider; skip over here so we don't go counting this as tied in
                        # the next branch.
                    }
                    else {
                        if nqp::istype($type_obj_a, $type_obj_b) {
                            # Narrower - note it and we're done.
                            ++$narrower;
                        }
                        else {
                            # Make sure it's tied, rather than the other way around.
                            unless nqp::istype($type_obj_b, $type_obj_a) {
                                ++$tied;
                            }
                        }
                    }
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
                # we wouldn't deem the other one narrower than this one in terms of
                # slurpyness. Otherwise, they're tied.
                return !(%b<max_arity> != $SLURPY_ARITY && %a<max_arity> == $SLURPY_ARITY)
                    && (%a<bind_check> && !%b<bind_check>);
            }

            my $dcself     := nqp::decont($self);
            my @candidates := nqp::getattr($dcself, Routine, '@!dispatchees');

            # Create a node for each candidate in the graph.
            my @graph;
            for @candidates -> $candidate {
                # Get hold of signature.
                my $sig    := nqp::getattr($candidate, Code, '$!signature');
                my @params := nqp::getattr($sig, Signature, '@!params');

                # Create it an entry.
                my %info := nqp::hash(
                    'sub',          $candidate,
                    'signature',    $sig,
                    'types',        [],
                    'type_flags',   nqp::list_i(),
                    'constraints',  [],
                    'rwness',       nqp::list_i()
                );
                if nqp::istype($candidate, Submethod) {
                    %info<exact_invocant> := 1;
                }
                my int $significant_param := 0;
                my int $min_arity         := 0;
                my int $max_arity         := 0;
                my int $num_types         := 0;
                my @coerce_type_idxs;
                my @coerce_type_objs;
                for @params -> $param {
                    # If it's got a sub-signature, also need a bind check and
                    # to check constraint on every dispatch. Same if it's got a
                    # `where` clause.
                    unless nqp::isnull(nqp::getattr($param, Parameter, '$!sub_signature')) &&
                           nqp::isnull(nqp::getattr($param, Parameter, '@!post_constraints')) {
                        %info<bind_check> := 1;
                        %info<constrainty> := 1;
                    }

                    # If it's a required named (and not slurpy) don't need its type info
                    # but we will need a bindability check during the dispatch for it.
                    my int $flags   := nqp::getattr_i($param, Parameter, '$!flags');
                    my $named_names := nqp::getattr($param, Parameter, '@!named_names');
                    unless nqp::isnull($named_names) {
                        if $flags +& $SIG_ELEM_MULTI_INVOCANT {
                            unless $flags +& $SIG_ELEM_IS_OPTIONAL {
                                if nqp::elems($named_names) == 1 {
                                    %info<req_named> := nqp::atpos_s($named_names, 0);
                                }
                            }
                            %info<bind_check> := 1;
                        }
                        next;
                    }

                    # If it's named slurpy, we're done, also we don't need a bind
                    # check on account of nameds since we take them all.
                    if $flags +& $SIG_ELEM_SLURPY_NAMED {
                        last;
                    }

                    # Otherwise, positional or slurpy and contributes to arity.
                    if $flags +& ($SIG_ELEM_SLURPY_POS +| $SIG_ELEM_SLURPY_LOL +| $SIG_ELEM_IS_CAPTURE +| $SIG_ELEM_SLURPY_ONEARG) {
                        $max_arity := $SLURPY_ARITY;
                        next;
                    }
                    elsif $flags +& $SIG_ELEM_IS_OPTIONAL {
                        ++$max_arity;
                    }
                    else {
                        ++$max_arity;
                        ++$min_arity;
                    }

                    # Record type info for this parameter.
                    if $flags +& $SIG_ELEM_TYPE_GENERIC {
                        %info<bind_check> := 1;
                        %info<constrainty> := 1;
                        %info<types>[$significant_param] := Any;
                    }
                    else {
                        my $ptype :=
                            nqp::getattr($param, Parameter, '$!type');
                        if $ptype.HOW.archetypes.coercive {
                            my $coercion_type := $ptype.HOW.wrappee($ptype, :coercion);
                            $ptype := $coercion_type.HOW.constraint_type($coercion_type);
                        }
                        %info<types>[$significant_param] := $ptype;
                    }
                    unless nqp::isnull(nqp::getattr($param, Parameter, '@!post_constraints')) {
                        %info<constraints>[$significant_param] := 1;
                    }
                    if $flags +& $SIG_ELEM_MULTI_INVOCANT {
                        ++$num_types;
                    }
                    if $flags +& $SIG_ELEM_IS_RW {
                        nqp::bindpos_i(%info<rwness>, $significant_param, 1);
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

                    # Keep track of coercion types; they'll need an extra entry
                    # in the things we sort.
                    if $param.coercive {
                        nqp::push(@coerce_type_idxs, $significant_param);
                        my $param_type := nqp::getattr($param, Parameter, '$!type');
                        nqp::push(@coerce_type_objs, $param_type.HOW.target_type($param_type));
                    }

                    ++$significant_param;
                }
                %info<min_arity> := $min_arity;
                %info<max_arity> := $max_arity;
                %info<num_types> := $num_types;

                # Add it to graph node, and initialize list of edges.
                nqp::push(@graph, nqp::hash(
                    'info',      %info,
                    'edges',     [],
                    'edges_in',  0,
                    'edges_out', 0
                ));

                # If there were any coercion types, then we also need to create
                # a candidate entry for the specific types.
                if @coerce_type_idxs {
                    my %c_info     := nqp::clone(%info);
                    %c_info<types> := nqp::clone(%c_info<types>);
                    my int $i      := 0;
                    while $i < nqp::elems(@coerce_type_idxs) {
                        %c_info<types>[@coerce_type_idxs[$i]] := @coerce_type_objs[$i];
                        ++$i;
                    }
                    nqp::push(@graph, nqp::hash(
                        'info',      %c_info,
                        'edges',     [],
                        'edges_in',  0,
                        'edges_out', 0
                    ));
                }
            }

            # Now analyze type narrowness of the candidates relative to each
            # other and create the edges.
            my int $j;
            my int $n := nqp::elems(@graph);
            my int $i := -1;
            while ++$i < $n {
                $j := -1;
                while ++$j < $n {
                    unless $i == $j {
                        if is_narrower(@graph[$i]<info>, @graph[$j]<info>) {
                            @graph[$i]<edges>[@graph[$i]<edges_out>] := @graph[$j];
                            ++@graph[$i]<edges_out>;
                            ++@graph[$j]<edges_in>;
                        }
                    }
                }
            }

            # Perform the topological sort.
            my int $candidates_to_sort := nqp::elems(@graph);
            my @result;
            while $candidates_to_sort > 0 {
                my int $rem_results := nqp::elems(@result);

                # Find any nodes that have no incoming edges and add them to
                # results.
                $i := -1;
                while ++$i < $n {
                    if @graph[$i]<edges_in> == 0 {
                        # Add to results.
                        nqp::push(@result, @graph[$i]<info>);
                        --$candidates_to_sort;
                        @graph[$i]<edges_in> := $EDGE_REMOVAL_TODO;
                    }
                }
                if $rem_results == nqp::elems(@result) {
                    nqp::die("Circularity detected in multi sub types" ~ ($self.name ?? " for &" ~ $self.name !! ''));
                }

                # Now we need to decrement edges in counts for things that had
                # edges from candidates we added here.
                $i := -1;
                while ++$i < $n {
                    if @graph[$i]<edges_in> == $EDGE_REMOVAL_TODO {
                        $j := -1;
                        while ++$j < @graph[$i]<edges_out> {
                            --@graph[$i]<edges>[$j]<edges_in>;
                        }
                        @graph[$i]<edges_in> := $EDGE_REMOVED;
                    }
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
        unless nqp::isnull(nqp::getattr($dcself, Routine, '@!dispatch_order')) {
            nqp::bindattr($dcself, Routine, '@!dispatch_order',
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
            my @candidates := nqp::getattr($dcself, Routine, '@!dispatch_order');
            if nqp::isnull(@candidates) {
                nqp::scwbdisable();
                @candidates := $dcself.'!sort_dispatchees_internal'();
                nqp::bindattr($dcself, Routine, '@!dispatch_order', @candidates);
                nqp::scwbenable();
            }

            # Iterate over the candidates and collect best ones; terminate
            # when we see two type objects (indicating end).
            my int $cur_idx := 0;
            my $cur_candidate;
            my int $type_check_count;
            my int $type_mismatch;
            my int $rwness_mismatch;
            my int $i;
            my int $pure_type_result := 1;
            my $many_res := $many ?? [] !! Mu;
            my @possibles;
            my int $done := 0;
            my int $done_bind_check := 0;
            my $Positional := nqp::gethllsym('Raku', 'MD_Pos');
            until $done {
                $cur_candidate := nqp::atpos(@candidates, $cur_idx);

                if nqp::isconcrete($cur_candidate) {
                    # Check if it's admissible by arity.
                    unless $num_args < nqp::atkey($cur_candidate, 'min_arity')
                    || $num_args > nqp::atkey($cur_candidate, 'max_arity') {
                        # Arity OK; now check if it's admissible by type.
                        $type_check_count := nqp::atkey($cur_candidate, 'num_types') > $num_args
                            ?? $num_args
                            !! nqp::atkey($cur_candidate, 'num_types');
                        $type_mismatch := 0;
                        $rwness_mismatch := 0;

                        $i := -1;
                        while ++$i < $type_check_count && !$type_mismatch && !$rwness_mismatch {
                            my $type_obj       := nqp::atpos(nqp::atkey($cur_candidate, 'types'), $i);
                            my int $type_flags := nqp::atpos_i(nqp::atkey($cur_candidate, 'type_flags'), $i);
                            my int $got_prim   := nqp::captureposprimspec($capture, $i);
                            my int $rwness     := nqp::atpos_i(nqp::atkey($cur_candidate, 'rwness'), $i);
                            if $rwness && !nqp::isrwcont(nqp::captureposarg($capture, $i)) {
                                # If we need a container but don't have one it clearly can't work.
                                $rwness_mismatch := 1;
                            }
                            elsif $type_flags +& $TYPE_NATIVE_MASK {
                                # Looking for a natively typed value. Did we get one?
                                if $got_prim == $BIND_VAL_OBJ {
                                    # Object, but could be a native container. If not, mismatch.
                                    my $contish := nqp::captureposarg($capture, $i);
                                    unless (($type_flags +& $TYPE_NATIVE_INT) && nqp::iscont_i($contish)) ||
                                           (($type_flags +& $TYPE_NATIVE_NUM) && nqp::iscont_n($contish)) ||
                                           (($type_flags +& $TYPE_NATIVE_STR) && nqp::iscont_s($contish)) {
                                        $type_mismatch := 1;
                                    }
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
                                my int $primish := 0;
                                if $got_prim == $BIND_VAL_OBJ {
                                    $param := nqp::captureposarg($capture, $i);
                                    if    nqp::iscont_i($param) { $param := Int; $primish := 1; }
                                    elsif nqp::iscont_n($param) { $param := Num; $primish := 1; }
                                    elsif nqp::iscont_s($param) { $param := Str; $primish := 1; }
                                    else { $param := nqp::hllizefor($param, 'Raku') }
                                }
                                else {
                                    $param := $got_prim == $BIND_VAL_INT ?? Int !!
                                              $got_prim == $BIND_VAL_NUM ?? Num !!
                                                                            Str;
                                    $primish := 1;
                                }
                                if nqp::eqaddr($type_obj, Mu) || nqp::istype($param, $type_obj) {
                                    if $i == 0 && nqp::existskey($cur_candidate, 'exact_invocant') {
                                        unless $param.WHAT =:= $type_obj {
                                            $type_mismatch := 1;
                                        }
                                    }
                                }
                                else {
                                    if $type_obj =:= $Positional {
                                        my $PositionalBindFailover := nqp::gethllsym('Raku', 'MD_PBF');
                                        unless nqp::istype($param, $PositionalBindFailover) {
                                            $type_mismatch := 1;
                                        }
                                    } else {
                                        $type_mismatch := 1;
                                    }
                                }
                                if !$type_mismatch && $type_flags +& $DEFCON_MASK {
                                    my int $defined := $primish || nqp::isconcrete($param);
                                    my int $desired := $type_flags +& $DEFCON_MASK;
                                    if ($defined && $desired == $DEFCON_UNDEFINED) ||
                                       (!$defined && $desired == $DEFCON_DEFINED) {
                                        $type_mismatch := 1;
                                    }
                                }
                            }
                        }

                        unless $type_mismatch || $rwness_mismatch {
                            # It's an admissible candidate; add to list.
                            nqp::push(@possibles, $cur_candidate);
                        }
                    }

                    ++$cur_idx;
                } else {
                    # We've hit the end of a tied group now. If any of them have a
                    # bindability check requirement, we'll do any of those now.
                    if nqp::elems(@possibles) {
                        my $new_possibles;
                        my %info;
                        $i := -1;
                        while ++$i < nqp::elems(@possibles) {
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
                                my $cs := nqp::getattr($sub, Code, '@!compstuff');
                                unless nqp::isnull($cs) {
                                    # We need to do the tie-break on something not yet compiled.
                                    # Get it compiled.
                                    my $ctf := $cs[1];
                                    $ctf() if $ctf;
                                }

                                # Since we had to do a bindability check, this is not
                                # a result we can cache on nominal type.
                                $pure_type_result := 0 if nqp::existskey(%info, 'constrainty');

                                # If we haven't got a possibles storage space, allocate it now.
                                $new_possibles := [] unless nqp::islist($new_possibles);

                                my $sig := nqp::getattr($sub, Code, '$!signature');
                                unless $done_bind_check {
                                    # Need a copy of the capture, as we may later do a
                                    # multi-dispatch when evaluating the constraint.
                                    $capture := nqp::clone($capture);
                                    $done_bind_check := 1;
                                }
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
                        ++$cur_idx;
                        unless nqp::isconcrete(nqp::atpos(@candidates, $cur_idx)) {
                            $done := 1;
                        }
                    }
                    elsif @possibles {
                        $done := 1;
                    }
                    else {
                        # Keep looping and looking, unless we really hit the end.
                        ++$cur_idx;
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

            # If we still have multiple options and we want one, then check default
            # trait and then, failing that, if we got an exact arity match on required
            # parameters (which will beat matches on optional parameters).
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

                # Failing that, look for exact arity match.
                if nqp::elems(@possibles) > 1 {
                    my $exact_arity;
                    for @possibles {
                        if nqp::atkey($_, 'min_arity') == $num_args &&
                           nqp::atkey($_, 'max_arity') == $num_args {
                            if nqp::isconcrete($exact_arity) {
                                $exact_arity := NQPMu;
                                last;
                            }
                            else {
                                $exact_arity := $_;
                            }
                        }
                    }
                    if nqp::isconcrete($exact_arity) {
                        nqp::pop(@possibles) while @possibles;
                        @possibles[0] := $exact_arity;
                    }
                }
            }

            # If we're at a single candidate here, and we also know there's no
            # type constraints that follow, we can cache the result.
            sub add_to_cache($entry) {
#?if !moar
                return 0 if nqp::capturehasnameds($capture);
#?endif
                nqp::scwbdisable();
                nqp::bindattr($dcself, Routine, '$!dispatch_cache',
                    nqp::multicacheadd(
                        nqp::getattr($dcself, Routine, '$!dispatch_cache'),
                        $capture, $entry));
                nqp::scwbenable();
            }
            if nqp::elems(@possibles) == 1 && $pure_type_result {
                add_to_cache(nqp::atkey(nqp::atpos(@possibles, 0), 'sub'));
            }

            # Perhaps we found nothing but have junctional arguments?
            my $junctional_res;
            if nqp::elems(@possibles) == 0 {
                my int $has_junc_args := 0;
                $i := -1;
                while ++$i < $num_args {
                    if !nqp::captureposprimspec($capture, $i) {
                        my $arg := nqp::captureposarg($capture, $i);
                        if nqp::istype($arg, Junction) && nqp::isconcrete($arg) {
                            $has_junc_args := 1;
                        }
                    }
                }
                if $has_junc_args {
                    $junctional_res := -> *@pos, *%named {
                        Junction.AUTOTHREAD($self, |@pos, |%named)
                    }
                    add_to_cache($junctional_res);
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
                Perl6::Metamodel::Configuration.throw_or_die(
                    'X::Multi::NoMatch',
                    "Cannot call " ~ $self.name() ~ "; no signatures match",
                    :dispatcher($self),
                    :capture($self.'!p6capture'($capture)));
            }
            else {
                my @ambiguous;
                for @possibles {
                    nqp::push(@ambiguous, $_<sub>);
                }
                Perl6::Metamodel::Configuration.throw_or_die(
                    'X::Multi::Ambiguous',
                    "Ambiguous call to " ~ $self.name(),
                    :dispatcher($self),
                    :@ambiguous,
                    :capture($self.'!p6capture'($capture)));
            }
        }));
    Routine.HOW.add_method(Routine, '!p6capture', nqp::getstaticcode(sub ($self, $capture) {
        sub assemble_capture(*@pos, *%named) {
            my $c := nqp::create(Capture);
            nqp::bindattr($c, Capture, '@!list', @pos);
            nqp::bindattr($c, Capture, '%!hash', %named);
            $c
        }
        nqp::invokewithcapture(&assemble_capture, $capture)
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
            my int $ARG_IS_LITERAL    := 32;

            # Count arguments.
            my int $num_args := nqp::elems(@args);

            # Get list and number of candidates, triggering a sort if there are none.
            my $dcself := nqp::decont($self);
            my @candidates := nqp::getattr($dcself, Routine, '@!dispatch_order');
            if nqp::isnull(@candidates) {
                nqp::scwbdisable();
                @candidates := $dcself.'!sort_dispatchees_internal'();
                nqp::bindattr($dcself, Routine, '@!dispatch_order', @candidates);
                nqp::scwbenable();
            }

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
                    ++$cur_idx;
                    if nqp::isconcrete(nqp::atpos(@candidates, $cur_idx))
                    && $all_native && !nqp::isconcrete($cur_result) {
                        next;
                    }
                    else {
                        $seen_all := !nqp::isconcrete(nqp::atpos(@candidates, $cur_idx));
                        last;
                    }
                }

                # Check if it's admissible by arity.
                if $num_args < nqp::atkey($cur_candidate, 'min_arity')
                || $num_args > nqp::atkey($cur_candidate, 'max_arity') {
                    ++$cur_idx;
                    next;
                }

                # If we got this far, something at least matched on arity.
                $arity_possible := 1;

                # Check if it's admissible by type.
                $type_check_count := nqp::atkey($cur_candidate, 'num_types') > $num_args
                    ?? $num_args
                    !! nqp::atkey($cur_candidate, 'num_types');
                $type_mismatch := 0;
                $type_match_possible := 1;
                $i := -1;
                while ++$i < $type_check_count {
                    my int $type_flags := nqp::atpos_i(nqp::atkey($cur_candidate, 'type_flags'), $i);
                    my int $got_prim   := nqp::atpos(@flags, $i) +& 0xF;
                    if $type_flags +& $TYPE_NATIVE_MASK {
                        # Looking for a natively typed value. Did we get one?
                        if $got_prim == $BIND_VAL_OBJ {
                            # Object; won't do.
                            $type_mismatch := 1;
                            $type_match_possible := 0;
                            last;
                        }

                        # Yes, but does it have the right type? Also look at rw-ness for literals.
                        my int $literal := nqp::atpos(@flags, $i) +& $ARG_IS_LITERAL;
                        if (($type_flags +& $TYPE_NATIVE_INT) && $got_prim != $BIND_VAL_INT)
                        || (($type_flags +& $TYPE_NATIVE_NUM) && $got_prim != $BIND_VAL_NUM)
                        || (($type_flags +& $TYPE_NATIVE_STR) && $got_prim != $BIND_VAL_STR)
                        || ($literal && nqp::atpos_i(nqp::atkey($cur_candidate, 'rwness'), $i)) {
                            # Mismatch.
                            $type_mismatch := 1;
                            $type_match_possible := 0;
                            last;
                        }
                    }
                    else {
                        my $type_obj := nqp::atpos(nqp::atkey($cur_candidate, 'types'), $i);

                        # Work out parameter.
                        my $param :=
                            $got_prim == $BIND_VAL_OBJ ?? nqp::atpos(@args, $i) !!
                            $got_prim == $BIND_VAL_INT ?? Int !!
                            $got_prim == $BIND_VAL_NUM ?? Num !!
                                                          Str;

                        # If we're here, it's a non-native.
                        $all_native := 0;

                        # A literal won't work with rw parameter.
                        my int $literal := nqp::atpos(@flags, $i) +& $ARG_IS_LITERAL;
                        if $literal && nqp::atpos_i(nqp::atkey($cur_candidate, 'rwness'), $i) {
                            $type_mismatch := 1;
                            $type_match_possible := 0;
                            last;
                        }

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
                                last;
                            }
                        }
                        elsif $type_flags +& $DEFCON_MASK {
                            $used_defcon := 1;
                        }
                    }
                }
                if $type_match_possible {
                    $type_possible := 1;
                }
                if $type_mismatch {
                    ++$cur_idx;
                    next;
                }
                if ($used_defcon) {
                    return [$MD_CT_NOT_SURE, NQPMu];
                }

                # If it's possible but needs a bind check, we're not going to be
                # able to decide it.
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
                    ++$cur_idx;
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
    Routine.HOW.add_method(Routine, 'set_flag', nqp::getstaticcode(sub ($self, $bit) {
            my $dcself := nqp::decont($self);
            nqp::bindattr_i($dcself, Routine, '$!flags',
              nqp::bitor_i(nqp::getattr_i($dcself, Routine, '$!flags'), $bit)
            );
            $dcself
        }));
    Routine.HOW.add_method(Routine, 'get_flag', nqp::getstaticcode(sub ($self, $bit) {
            my $dcself := nqp::decont($self);
            nqp::hllboolfor(
              nqp::bitand_i(nqp::getattr_i($dcself, Routine, '$!flags'), $bit),
              "Raku"
            );
        }));
    Routine.HOW.add_method(Routine, 'set_rw', nqp::getstaticcode(sub ($self) {
            $self.set_flag(0x01)
        }));
    Routine.HOW.add_method(Routine, 'rw', nqp::getstaticcode(sub ($self) {
            $self.get_flag(0x01)
        }));
    Routine.HOW.add_method(Routine, 'set_yada', nqp::getstaticcode(sub ($self) {
            $self.set_flag(0x02)
        }));
    Routine.HOW.add_method(Routine, 'yada', nqp::getstaticcode(sub ($self) {
            $self.get_flag(0x02)
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
    Routine.HOW.set_multi_invocation_attrs(Routine, Routine, '$!onlystar', '$!dispatch_cache');
    Routine.HOW.compose_invocation(Routine);

    # class Sub is Routine {
    Sub.HOW.add_parent(Sub, Routine);
    Sub.HOW.compose_repr(Sub);
    Sub.HOW.compose_invocation(Sub);

    # class Method is Routine {
    Method.HOW.add_parent(Method, Routine);
    Method.HOW.compose_repr(Method);
    Method.HOW.compose_invocation(Method);

    # class Submethod is Routine {
    Submethod.HOW.add_parent(Submethod, Routine);
    Submethod.HOW.compose_repr(Submethod);
    Submethod.HOW.compose_invocation(Submethod);

    # Capture store for SET_CAPS.
    my class RegexCaptures {
        # An integer array of positional capture counts.
        has @!pos-capture-counts;

        # A string array of named capture names and a matching integer array of
        # capture counts.
        has @!named-capture-names;
        has @!named-capture-counts;

        # Form this data structure from a capnames hash.
        method from-capnames(%capnames) {
            nqp::create(self).'!from-capnames'(%capnames)
        }

        method !from-capnames(%capnames) {
            # Initialize.
            @!pos-capture-counts := nqp::list_i();
            @!named-capture-names := nqp::list_s();
            @!named-capture-counts := nqp::list_i();

            # Go over the captures and build up the data structure.
            for %capnames {
                my $name := nqp::iterkey_s($_);
                if $name ne '' {
                    my $count := nqp::iterval($_);
                    if nqp::ord($name) != 36 && nqp::ord($name) < 58 {
                        nqp::bindpos_i(@!pos-capture-counts, +$name, $count);
                    }
                    else {
                        nqp::push_s(@!named-capture-names, $name);
                        nqp::push_i(@!named-capture-counts, $count);
                    }
                }
            }

            self
        }

        # Are there any captures?
        method has-captures() {
            nqp::elems(@!named-capture-counts) || nqp::elems(@!pos-capture-counts)
        }

        ## Raku Match object building
        ## (for use in standard Raku regexes)

        # Build a list of positional captures, or return a shared empty list if
        # there are none. This only populates the slots which need an array.
        my $EMPTY-LIST := nqp::list();
        my $EMPTY-HASH := nqp::hash();
        method prepare-raku-list() {
            my int $n := nqp::elems(@!pos-capture-counts);
            if $n > 0 {
                my $result := nqp::list();
                my int $i := -1;
                while ++$i < $n {
                    nqp::bindpos($result, $i, nqp::create(Array))
                        if nqp::atpos_i(@!pos-capture-counts, $i) >= 2;
                }
                $result
            }
            else {
#?if js
                # HACK js backend bug workaround
                nqp::list()
#?endif
#?if !js
                $EMPTY-LIST
#?endif
            }
        }

        # Build a hash of named captures, or return a shared empty hash if there
        # are none. This only populates the slots that need an array.
        method prepare-raku-hash() {
            my int $n := nqp::elems(@!named-capture-counts);
            if $n > 0 {
                my $result := nqp::hash();
                my int $i := -1;
                while ++$i < $n {
                    if nqp::atpos_i(@!named-capture-counts, $i) >= 2 {
                        nqp::bindkey($result,
                            nqp::atpos_s(@!named-capture-names, $i),
                            nqp::create(Array));
                    }
                }
                $result
            }
            else {
#?if js
                # HACK js backend bug workaround
                nqp::hash()
#?endif
#?if !js
                $EMPTY-HASH
#?endif
            }
        }

        ## NQP Match object building
        ## (for use when we override stuff from the Rakudo grammar)

        # Build a list of positional captures, or return a shared empty list if
        # there are none. This only populates the slots which need an array.
        method prepare-list() {
            my int $n := nqp::elems(@!pos-capture-counts);
            if $n > 0 {
                my $result := nqp::list();
                my int $i := -1;
                while ++$i < $n {
                    nqp::bindpos($result, $i, nqp::list())
                        if nqp::atpos_i(@!pos-capture-counts, $i) >= 2;
                }
                $result
            }
            else {
                $EMPTY-LIST
            }
        }

        # Build a hash of named camptures, or return a shared empty hash if there
        # are none. This only poplates the slots that need an array.
        method prepare-hash() {
            my int $n := nqp::elems(@!named-capture-counts);
            if $n > 0 {
                my $result := nqp::hash();
                my int $i := -1;
                while ++$i < $n {
                    if nqp::atpos_i(@!named-capture-counts, $i) >= 2 {
                        nqp::bindkey($result,
                            nqp::atpos_s(@!named-capture-names, $i),
                            nqp::list());
                    }
                }
                $result
            }
            else {
                $EMPTY-HASH
            }
        }

        # Get the name of the only capture, if there is only one.
        method onlyname() { '' }
    }
    # class Regex is Method {
    #     has $!caps;
    #     has Mu $!nfa;
    #     has @!alt_nfas;
    #     has str $!source;
    #     has $!topic;
    #     has $!slash;
    Regex.HOW.add_parent(Regex, Method);
    Regex.HOW.add_attribute(Regex, scalar_attr('$!caps', Mu, Regex));
    Regex.HOW.add_attribute(Regex, scalar_attr('$!nfa', Mu, Regex));
    Regex.HOW.add_attribute(Regex, scalar_attr('%!alt_nfas', Hash, Regex));
    Regex.HOW.add_attribute(Regex, scalar_attr('$!source', str, Regex));
    Regex.HOW.add_attribute(Regex, scalar_attr('$!topic', Mu, Regex));
    Regex.HOW.add_attribute(Regex, scalar_attr('$!slash', Mu, Regex));
    Regex.HOW.add_method(Regex, 'SET_CAPS', nqp::getstaticcode(sub ($self, $capnames) {
            nqp::bindattr(nqp::decont($self), Regex, '$!caps',
                RegexCaptures.from-capnames($capnames))
        }));
    Regex.HOW.add_method(Regex, 'SET_NFA', nqp::getstaticcode(sub ($self, $nfa) {
            nqp::bindattr(nqp::decont($self), Regex, '$!nfa', $nfa)
        }));
    Regex.HOW.add_method(Regex, 'SET_ALT_NFA', nqp::getstaticcode(sub ($self, str $name, $nfa) {
            my %alts := nqp::getattr(nqp::decont($self), Regex, '%!alt_nfas');
            unless %alts {
                %alts := nqp::hash();
                nqp::bindattr(nqp::decont($self), Regex, '%!alt_nfas', %alts);
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
                nqp::getattr(nqp::decont($self), Regex, '%!alt_nfas'),
                $name)
        }));
    Regex.HOW.add_method(Regex, 'ALT_NFAS', nqp::getstaticcode(sub ($self) {
            my $store := nqp::decont(nqp::getattr(nqp::decont($self), Regex, '%!alt_nfas'));
            if nqp::istype($store, Hash) {
                nqp::hash();
            } else {
                $store
            }
        }));
    Regex.HOW.compose_repr(Regex);
    Regex.HOW.compose_invocation(Regex);

    # class Str is Cool {
    #     has str $!value is box_target;
    Str.HOW.add_parent(Str, Cool);
    Str.HOW.add_attribute(Str, BOOTSTRAPATTR.new(:name<$!value>, :type(str), :box_target(1), :package(Str)));
    Str.HOW.set_boolification_mode(Str, 3);
    Str.HOW.publish_boolification_spec(Str);
    Str.HOW.compose_repr(Str);

    # class Int is Cool {
    #     has bigint $!value is box_target;
    Int.HOW.add_parent(Int, Cool);
    Int.HOW.add_attribute(Int, BOOTSTRAPATTR.new(:name<$!value>, :type(bigint), :box_target(1), :package(Int)));
    Int.HOW.set_boolification_mode(Int, 6);
    Int.HOW.publish_boolification_spec(Int);
    Int.HOW.compose_repr(Int);

    # class Num is Cool {
    #     has num $!value is box_target;
    Num.HOW.add_parent(Num, Cool);
    Num.HOW.add_attribute(Num, BOOTSTRAPATTR.new(:name<$!value>, :type(num), :box_target(1), :package(Num)));
    Num.HOW.set_boolification_mode(Num, 2);
    Num.HOW.publish_boolification_spec(Num);
    Num.HOW.compose_repr(Num);

    # class Nil is Cool {
    Nil.HOW.compose_repr(Nil);

    # class List is Cool {
    #     has Mu $!reified;
    #     has Mu $!todo;
    List.HOW.add_parent(List, Cool);
    List.HOW.add_attribute(List, storage_attr('$!reified', Mu, List, Mu));
    List.HOW.add_attribute(List, storage_attr('$!todo', Mu, List, Mu));
    List.HOW.compose_repr(List);

    # class Slip is List {
    Slip.HOW.add_parent(Slip, List);
    Slip.HOW.compose_repr(Slip);

    # class Array is List {
    #     has Mu $!descriptor;
    Array.HOW.add_parent(Array, List);
    Array.HOW.add_attribute(Array, storage_attr('$!descriptor', Mu, Array,
        Scalar.HOW.cache_get(Scalar, 'default_cont_spec')));
    Array.HOW.compose_repr(Array);

    # my class Map is Cool {
    #     has Mu $!storage;
    Map.HOW.add_parent(Map, Cool);
    Map.HOW.add_attribute(Map, storage_attr('$!storage', Mu, Map, nqp::hash(),
        :associative_delegate));
    Map.HOW.compose_repr(Map);
    nqp::settypehllrole(Map, 5);

    # my class Hash is Map {
    #     has Mu $!descriptor;
    Hash.HOW.add_parent(Hash, Map);
    Hash.HOW.add_attribute(Hash, storage_attr('$!descriptor', Mu, Hash,
        Scalar.HOW.cache_get(Scalar, 'default_cont_spec')));
    Hash.HOW.compose_repr(Hash);
    nqp::settypehllrole(Hash, 5);

    # class Capture is Any {
    #     has @!list;
    #     has %!hash;
    Capture.HOW.add_parent(Capture, Any);
    Capture.HOW.add_attribute(Capture, scalar_attr('@!list', List, Capture, :!auto_viv_container));
    Capture.HOW.add_attribute(Capture, scalar_attr('%!hash', Hash, Capture, :!auto_viv_container));
    Capture.HOW.compose_repr(Capture);

    # class Junction is Mu {
    #     has Mu $!eigenstates;
    #     has str $!type;
    Junction.HOW.add_parent(Junction, Mu);
    Junction.HOW.add_attribute(Junction, scalar_attr('$!eigenstates', Mu, Junction));
    Junction.HOW.add_attribute(Junction, scalar_attr('$!type', str, Junction));
    Junction.HOW.compose_repr(Junction);

    # class Bool is Int {
    #     has str $!key;
    #     has int $!value;
    Bool.HOW.set_base_type(Bool, Int);
    Bool.HOW.add_attribute(Bool, Attribute.new(:name<$!key>, :type(str), :package(Bool)));
    Bool.HOW.add_attribute(Bool, Attribute.new(:name<$!value>, :type(int), :package(Bool)));
    Bool.HOW.set_boolification_mode(Bool, 1);
    Bool.HOW.publish_boolification_spec(Bool);
    Bool.HOW.compose_repr(Bool);
    Bool.HOW.add_method(Bool, 'key', nqp::getstaticcode(sub ($self) {
            nqp::getattr_s(nqp::decont($self),
                Bool, '$!key');
        }));
    Bool.HOW.add_method(Bool, 'value', nqp::getstaticcode(sub ($self) {
            nqp::getattr_i(nqp::decont($self),
                Bool, '$!value');
        }));

    # class ObjAt is Any {
    #     has str $!value;
    ObjAt.HOW.add_parent(ObjAt, Any);
    ObjAt.HOW.add_attribute(ObjAt, BOOTSTRAPATTR.new(:name<$!value>, :type(str), :box_target(1), :package(ObjAt)));
    ObjAt.HOW.compose_repr(ObjAt);

    # class ValueObjAt is ObjAt {
    ValueObjAt.HOW.add_parent(ValueObjAt, ObjAt);
    ValueObjAt.HOW.compose_repr(ValueObjAt);

    # class ForeignCode {
    #     has Mu $!do;                # Code object we delegate to
    ForeignCode.HOW.add_parent(ForeignCode, Any);
    ForeignCode.HOW.add_attribute(ForeignCode, Attribute.new(:name<$!do>, :type(Code), :package(ForeignCode)));
    ForeignCode.HOW.compose_repr(ForeignCode);
    ForeignCode.HOW.set_invocation_attr(ForeignCode, ForeignCode, '$!do');
    ForeignCode.HOW.compose_invocation(ForeignCode);

    # Set up Stash type, which is really just a hash with a name.
    # class Stash is Hash {
	#     has str $!longname;
    Stash.HOW.add_parent(Stash, Hash);
    Stash.HOW.add_attribute(Stash, Attribute.new(:name<$!longname>, :type(str), :package(Stash)));
    Stash.HOW.compose_repr(Stash);

    # Configure the stash type.
    Perl6::Metamodel::Configuration.set_stash_type(Stash, Map);

    # Give everything we've set up so far a Stash.
    Perl6::Metamodel::ClassHOW.add_stash(Mu);
    Perl6::Metamodel::ClassHOW.add_stash(Any);
    Perl6::Metamodel::ClassHOW.add_stash(Nil);
    Perl6::Metamodel::ClassHOW.add_stash(Cool);
    Perl6::Metamodel::ClassHOW.add_stash(Attribute);
    Perl6::Metamodel::ClassHOW.add_stash(Scalar);
    Perl6::Metamodel::ClassHOW.add_stash(Proxy);
    Perl6::Metamodel::ClassHOW.add_stash(Signature);
    Perl6::Metamodel::ClassHOW.add_stash(Parameter);
    Perl6::Metamodel::ClassHOW.add_stash(Code);
    Perl6::Metamodel::ClassHOW.add_stash(Block);
    Perl6::Metamodel::ClassHOW.add_stash(Routine);
    Perl6::Metamodel::ClassHOW.add_stash(Sub);
    Perl6::Metamodel::ClassHOW.add_stash(Method);
    Perl6::Metamodel::ClassHOW.add_stash(Submethod);
    Perl6::Metamodel::ClassHOW.add_stash(Regex);
    Perl6::Metamodel::ClassHOW.add_stash(Str);
    Perl6::Metamodel::ClassHOW.add_stash(Int);
    Perl6::Metamodel::ClassHOW.add_stash(Num);
    Perl6::Metamodel::NativeRefHOW.add_stash(IntLexRef);
    Perl6::Metamodel::NativeRefHOW.add_stash(NumLexRef);
    Perl6::Metamodel::NativeRefHOW.add_stash(StrLexRef);
    Perl6::Metamodel::NativeRefHOW.add_stash(IntAttrRef);
    Perl6::Metamodel::NativeRefHOW.add_stash(NumAttrRef);
    Perl6::Metamodel::NativeRefHOW.add_stash(StrAttrRef);
    Perl6::Metamodel::NativeRefHOW.add_stash(IntPosRef);
    Perl6::Metamodel::NativeRefHOW.add_stash(NumPosRef);
    Perl6::Metamodel::NativeRefHOW.add_stash(StrPosRef);
    Perl6::Metamodel::NativeRefHOW.add_stash(IntMultidimRef);
    Perl6::Metamodel::NativeRefHOW.add_stash(NumMultidimRef);
    Perl6::Metamodel::NativeRefHOW.add_stash(StrMultidimRef);
#?if js
    Perl6::Metamodel::NativeRefHOW.add_stash(Int64LexRef);
    Perl6::Metamodel::NativeRefHOW.add_stash(Int64AttrRef);
    Perl6::Metamodel::NativeRefHOW.add_stash(Int64PosRef);
    Perl6::Metamodel::NativeRefHOW.add_stash(Int64MultidimRef);
#?endif
    Perl6::Metamodel::ClassHOW.add_stash(List);
    Perl6::Metamodel::ClassHOW.add_stash(Slip);
    Perl6::Metamodel::ClassHOW.add_stash(Array);
    Perl6::Metamodel::ClassHOW.add_stash(Map);
    Perl6::Metamodel::ClassHOW.add_stash(Hash);
    Perl6::Metamodel::ClassHOW.add_stash(Capture);
    Perl6::Metamodel::EnumHOW.add_stash(Bool);
    Perl6::Metamodel::ClassHOW.add_stash(ObjAt);
    Perl6::Metamodel::ClassHOW.add_stash(ValueObjAt);
    Perl6::Metamodel::ClassHOW.add_stash(Stash);
    Perl6::Metamodel::ClassHOW.add_stash(Grammar);
    Perl6::Metamodel::ClassHOW.add_stash(Junction);
    Perl6::Metamodel::ClassHOW.add_stash(ForeignCode);

    # Default invocation behavior delegates off to invoke.
    my $invoke_forwarder :=
        nqp::getstaticcode(sub ($self, *@pos, *%named) {
            if nqp::can($self, 'CALL-ME') {
                $self.CALL-ME(|@pos, |%named)
            }
            else {
                my $self_name := $self.HOW.name($self);
                if !nqp::isconcrete($self) && +@pos {
                    my $val;
                    if +@pos == 1 {
                        $val := @pos[0];
                    }
                    else {
                        $val := nqp::create(List);
                        nqp::bindattr($val, List, '$!reified', @pos);
                    }
                    Perl6::Metamodel::Configuration.throw_or_die(
                        'X::Coerce::Impossible',
                        "Cannot coerce to $self_name with named arguments",
                        :target-type($self.WHAT), :from-type($val.WHAT), :hint("named arguments passed")
                    ) if +%named;
                    my $how := $self.HOW;
                    my $coercion_type := Perl6::Metamodel::CoercionHOW.new_type(
                        (nqp::istype($how, Perl6::Metamodel::ClassHOW) && $how.is_pun($self)
                            ?? $self.HOW.pun_source($self)
                            !! $self.WHAT),
                        $val.WHAT);
                    nqp::hllizefor($coercion_type.HOW.coerce($coercion_type, $val), "Raku");
                }
                else {
                    Perl6::Metamodel::Configuration.throw_or_die(
                        'X::Method::NotFound',
                        "No such method 'CALL-ME' for invocant of type '$self_name'",
                        :invocant($self), :method(nqp::hllizefor('CALL-ME', "Raku")),
                        :typename(nqp::hllizefor($self_name, "Raku"))
                    );
                }
            }
        });
    Mu.HOW.set_invocation_handler(Mu, $invoke_forwarder);
    Mu.HOW.compose_invocation(Mu);

    # If we don't already have a PROCESS, set it up.
    my $PROCESS := nqp::gethllsym('Raku', 'PROCESS');
    if nqp::isnull($PROCESS) {
        PROCESS.HOW.compose(PROCESS);
        Perl6::Metamodel::ModuleHOW.add_stash(PROCESS);
        $PROCESS := PROCESS;
        nqp::bindhllsym('Raku', 'PROCESS', $PROCESS);
    }

    # Bool::False and Bool::True.
    my $false := nqp::box_i(0, Bool);
    nqp::bindattr_s($false, Bool, '$!key', 'False');
    nqp::bindattr_i($false, Bool, '$!value', 0);
    #nqp::bindattr($false, Int, '$!value', 0);
    Bool.HOW.add_enum_value(Bool, $false);
    (Bool.WHO)<False> := $false;
    my $true := nqp::box_i(1, Bool);
    nqp::bindattr_s($true, Bool, '$!key', 'True');
    nqp::bindattr_i($true, Bool, '$!value', 1);
    #nqp::bindattr($true, Int, '$!value', 1);
    Bool.HOW.add_enum_value(Bool, $true);
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
    EXPORT::DEFAULT.WHO<Mu>         := Mu;
    EXPORT::DEFAULT.WHO<Any>        := Any;
    EXPORT::DEFAULT.WHO<Cool>       := Cool;
    EXPORT::DEFAULT.WHO<Nil>        := Nil;
    EXPORT::DEFAULT.WHO<Attribute>  := Attribute;
    EXPORT::DEFAULT.WHO<Signature>  := Signature;
    EXPORT::DEFAULT.WHO<Parameter>  := Parameter;
    EXPORT::DEFAULT.WHO<Code>       := Code;
    EXPORT::DEFAULT.WHO<Block>      := Block;
    EXPORT::DEFAULT.WHO<Routine>    := Routine;
    EXPORT::DEFAULT.WHO<Sub>        := Sub;
    EXPORT::DEFAULT.WHO<Method>     := Method;
    EXPORT::DEFAULT.WHO<Submethod>  := Submethod;
    EXPORT::DEFAULT.WHO<Regex>      := Regex;
    EXPORT::DEFAULT.WHO<Str>        := Str;
    EXPORT::DEFAULT.WHO<Int>        := Int;
    EXPORT::DEFAULT.WHO<Num>        := Num;
    EXPORT::DEFAULT.WHO<List>       := List;
    EXPORT::DEFAULT.WHO<Slip>       := Slip;
    EXPORT::DEFAULT.WHO<Array>      := Array;
    EXPORT::DEFAULT.WHO<Map>        := Map;
    EXPORT::DEFAULT.WHO<Hash>       := Hash;
    EXPORT::DEFAULT.WHO<Capture>    := Capture;
    EXPORT::DEFAULT.WHO<ObjAt>      := ObjAt;
    EXPORT::DEFAULT.WHO<ValueObjAt> := ValueObjAt;
    EXPORT::DEFAULT.WHO<Stash>      := Stash;
    EXPORT::DEFAULT.WHO<Scalar>     := Scalar;
    EXPORT::DEFAULT.WHO<IntLexRef>  := IntLexRef;
    EXPORT::DEFAULT.WHO<NumLexRef>  := NumLexRef;
    EXPORT::DEFAULT.WHO<StrLexRef>  := StrLexRef;
    EXPORT::DEFAULT.WHO<IntAttrRef> := IntAttrRef;
    EXPORT::DEFAULT.WHO<NumAttrRef> := NumAttrRef;
    EXPORT::DEFAULT.WHO<StrAttrRef> := StrAttrRef;
    EXPORT::DEFAULT.WHO<IntPosRef>  := IntPosRef;
    EXPORT::DEFAULT.WHO<NumPosRef>  := NumPosRef;
    EXPORT::DEFAULT.WHO<StrPosRef>  := StrPosRef;
#?if js
    EXPORT::DEFAULT.WHO<Int64LexRef>  := Int64LexRef;
    EXPORT::DEFAULT.WHO<Int64AttrRef> := Int64AttrRef;
    EXPORT::DEFAULT.WHO<Int64PosRef>  := Int64PosRef;
#?endif
    EXPORT::DEFAULT.WHO<Proxy>      := Proxy;
    EXPORT::DEFAULT.WHO<Grammar>    := Grammar;
    EXPORT::DEFAULT.WHO<Junction>   := Junction;
    EXPORT::DEFAULT.WHO<PROCESS>    := $PROCESS;
    EXPORT::DEFAULT.WHO<Bool>       := Bool;
    EXPORT::DEFAULT.WHO<False>      := $false;
    EXPORT::DEFAULT.WHO<True>       := $true;
    EXPORT::DEFAULT.WHO<ContainerDescriptor> := ContainerDescriptor;
    EXPORT::DEFAULT.WHO<MethodDispatcher>    := Perl6::Metamodel::MethodDispatcher;
    EXPORT::DEFAULT.WHO<MultiDispatcher>     := Perl6::Metamodel::MultiDispatcher;
    EXPORT::DEFAULT.WHO<WrapDispatcher>      := Perl6::Metamodel::WrapDispatcher;
    EXPORT::DEFAULT.WHO<Metamodel>           := Metamodel;
    EXPORT::DEFAULT.WHO<ForeignCode>         := ForeignCode;
}
EXPORT::DEFAULT.WHO<NQPMatchRole> := NQPMatchRole;
EXPORT::DEFAULT.WHO<NQPdidMATCH> := NQPdidMATCH;

#?if !moar
# Set up various type mappings.
nqp::p6settypes(EXPORT::DEFAULT.WHO);
#?endif

# HLL configuration: interop, boxing and exit handling.
nqp::sethllconfig('Raku', nqp::hash(
    'int_box', Int,
    'num_box', Num,
    'str_box', Str,
    'null_value', Mu,
    'true_value', (Bool.WHO)<True>,
    'false_value', (Bool.WHO)<False>,
    'foreign_type_int', Int,
    'foreign_type_num', Num,
    'foreign_type_str', Str,
    'foreign_transform_array', -> $farray {
        my $list := nqp::create(List);
        nqp::bindattr($list, List, '$!reified', $farray);
        $list
    },
    'foreign_transform_hash', -> $hash {
        my $result := nqp::create(Hash);
        nqp::bindattr($result, Map, '$!storage', $hash);
        $result
    },
    'foreign_transform_code', -> $code {
        my $result := nqp::create(ForeignCode);
        nqp::bindattr($result, ForeignCode, '$!do', $code);
        $result
    },
    'exit_handler', -> $coderef, $resultish {
        unless nqp::p6inpre() {
            my %phasers :=
              nqp::getattr(nqp::getcodeobj($coderef),Block,'$!phasers');
            my @leaves := nqp::atkey(%phasers, '!LEAVE-ORDER');
            my @posts  := nqp::atkey(%phasers, 'POST');
            my @exceptions;
            unless nqp::isnull(@leaves) {

                # only have a single LEAVEish phaser, so no frills needed
                if nqp::elems(@leaves) == 1 && nqp::elems(%phasers) == 1 {
#?if jvm
                    nqp::decont(nqp::atpos(@leaves,0))();
#?endif
#?if !jvm
                    nqp::p6capturelexwhere(
                      nqp::decont(nqp::atpos(@leaves,0)).clone)();
#?endif
                    # don't bother to CATCH, there can only be one exception
                }

                # slow path here
                else {
                    my @keeps  := nqp::atkey(%phasers, 'KEEP');
                    my @undos  := nqp::atkey(%phasers, 'UNDO');
                    my int $n := nqp::elems(@leaves);
                    my int $i := -1;
                    my int $run;
                    my $phaser;
                    while ++$i < $n {
                        $phaser := nqp::decont(nqp::atpos(@leaves, $i));
                        $run := 1;
                        unless nqp::isnull(@keeps) {
                            for @keeps {
                                if nqp::eqaddr(nqp::decont($_),$phaser) {
                                    $run := !nqp::isnull($resultish) &&
                                             nqp::isconcrete($resultish) &&
                                             $resultish.defined;
                                    last;
                                }
                            }
                        }
                        unless nqp::isnull(@undos) {
                            for @undos {
                                if nqp::eqaddr(nqp::decont($_),$phaser) {
                                    $run := nqp::isnull($resultish) ||
                                            !nqp::isconcrete($resultish) ||
                                            !$resultish.defined;
                                    last;
                                }
                            }
                        }
                        if $run {
#?if jvm
                            $phaser();
#?endif
#?if !jvm
                            nqp::p6capturelexwhere($phaser.clone())();
#?endif
                            CATCH { nqp::push(@exceptions, $_) }
                        }
                    }
                }
            }

            unless nqp::isnull(@posts) {
                my $value := nqp::ifnull($resultish,Mu);
                my int $n := nqp::elems(@posts);
                my int $i := -1;
                while ++$i < $n {
#?if jvm
                    nqp::atpos(@posts, $i)($value);
#?endif
#?if !jvm
                    nqp::p6capturelexwhere(nqp::atpos(@posts,$i).clone)($value);
#?endif
                    CATCH { nqp::push(@exceptions, $_); last; }
                }
            }

            if @exceptions {
                if nqp::elems(@exceptions) > 1 {
                    Perl6::Metamodel::Configuration.throw_or_die(
                        'X::PhaserExceptions',
                        "Multiple exceptions were thrown by LEAVE/POST phasers",
                        :exceptions(@exceptions)
                    );
                }
                nqp::rethrow(@exceptions[0]);
            }
        }
    },
#?if !jvm
    'bind_error', -> $capture {
        # Get signature and lexpad.
        my $caller := nqp::getcodeobj(nqp::callercode());
        my $sig    := nqp::getattr($caller, Code, '$!signature');
        my $lexpad := nqp::ctxcaller(nqp::ctx());

        # Run full binder to produce an error.
        my @error;
        my int $bind_res := Binder.bind($capture, $sig, $lexpad, 0, @error);
        if $bind_res {
            if $bind_res == 2 {
                my @pos_args;
                my int $num_pos_args := nqp::captureposelems($capture);
                my int $got_prim;
                my int $k := -1;
                while ++$k < $num_pos_args {
                    $got_prim := nqp::captureposprimspec($capture, $k);
                    if $got_prim == 0 {
                        nqp::push(@pos_args, nqp::captureposarg($capture, $k));
                    }
                    elsif $got_prim == 1 {
                        nqp::push(@pos_args, nqp::box_i(nqp::captureposarg_i($capture, $k), Int));
                    }
                    elsif $got_prim == 2 {
                        nqp::push(@pos_args, nqp::box_n(nqp::captureposarg_n($capture, $k), Num));
                    }
                    else {
                        nqp::push(@pos_args, nqp::box_s(nqp::captureposarg_s($capture, $k), Str));
                    }
                }
                my %named_args := nqp::capturenamedshash($capture);
                Junction.AUTOTHREAD($caller,
                        |@pos_args,
                        |%named_args);
            }
            else {
                nqp::isinvokable(@error[0]) ?? @error[0]() !! nqp::die(@error[0]);
            }
        }
        else {
            nqp::die("Internal error: inconsistent bind result");
        }
    },
    'method_not_found_error', -> $obj, str $name {
        my $class := nqp::getlexcaller('$?CLASS');
        my $die_msg := "Method '$name' not found for invocant of class '{$obj.HOW.name($obj)}'";
        if $name eq 'STORE' {
            Perl6::Metamodel::Configuration.throw_or_die(
                'X::Assignment::RO',
                $die_msg,
                :value($obj)
            );
        }
        Perl6::Metamodel::Configuration.throw_or_die(
            'X::Method::NotFound',
            $die_msg,
            :invocant($obj),
            :method($name),
            :typename($obj.HOW.name($obj)),
            :private(nqp::hllboolfor(0, 'Raku')),
            :in-class-call(nqp::hllboolfor(nqp::eqaddr(nqp::what($obj), $class), 'Raku'))
        );
    },
#?endif
    'lexical_handler_not_found_error', -> int $cat, int $out_of_dyn_scope {
        if $cat == nqp::const::CONTROL_RETURN {
            Perl6::Metamodel::Configuration.throw_or_die(
                'X::ControlFlow::Return',
                'Attempt to return outside of any Routine',
                :out-of-dynamic-scope(nqp::hllboolfor($out_of_dyn_scope, 'Raku'))
            );
        }
        else {
            my $ex := nqp::newexception();
            nqp::setextype($ex, $cat);
            nqp::getcomp('Raku').handle-control($ex);
        }
    },
    'finalize_handler', -> @objs {
        for @objs -> $o {
            for $o.HOW.destroyers($o) -> $d {
                $d($o)
            }
        }
    },
    'int_lex_ref', IntLexRef,
    'num_lex_ref', NumLexRef,
    'str_lex_ref', StrLexRef,
    'int_attr_ref', IntAttrRef,
    'num_attr_ref', NumAttrRef,
    'str_attr_ref', StrAttrRef,
    'int_pos_ref', IntPosRef,
    'num_pos_ref', NumPosRef,
    'str_pos_ref', StrPosRef,
    'int_multidim_ref', IntMultidimRef,
    'num_multidim_ref', NumMultidimRef,
    'str_multidim_ref', StrMultidimRef,
#?if js
    'int64_lex_ref', Int64LexRef,
    'int64_attr_ref', Int64AttrRef,
    'int64_pos_ref', Int64PosRef,
    'int64_multidim_ref', Int64MultidimRef,
#?endif
#?if moar
    'max_inline_size', 384,
#?endif
));

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
    nqp::bindattr($sel, Routine, '@!dispatchees', []);
    $sel
});

# Roles pretend to be narrower than certain types for the purpose
# of type checking. Also, they pun to classes.
my %excluded := nqp::hash(
    'ACCEPTS', Mu, 'item', Mu, 'dispatch:<.=>', Mu, 'Bool', Mu,
    'gist', Mu, 'perl', Mu, 'raku', Mu, 'Str', Mu, 'sink', Mu, 'defined', Mu,
    'WHICH', Mu, 'WHERE', Mu, 'WHY', Mu, 'set_why', Mu, 'so', Mu, 'not', Mu,
    'Numeric', Mu, 'Real', Mu, 'Stringy', Mu, 'say', Mu, 'print', Mu,
    'put', Mu, 'note', Mu, 'DUMP', Mu, 'dispatch:<var>', Mu,
    'dispatch:<.?>', Mu, 'dispatch:<.^>', Mu);
Perl6::Metamodel::ParametricRoleGroupHOW.pretend_to_be([Cool, Any, Mu]);
Perl6::Metamodel::ParametricRoleGroupHOW.configure_punning(
    Perl6::Metamodel::ClassHOW, %excluded);
Perl6::Metamodel::ParametricRoleHOW.pretend_to_be([Cool, Any, Mu]);
Perl6::Metamodel::ParametricRoleHOW.configure_punning(
    Perl6::Metamodel::ClassHOW, %excluded);
Perl6::Metamodel::CurriedRoleHOW.pretend_to_be([Cool, Any, Mu]);
Perl6::Metamodel::CurriedRoleHOW.configure_punning(
    Perl6::Metamodel::ClassHOW, %excluded);

# Similar for packages and modules, but just has methods from Any.
Perl6::Metamodel::PackageHOW.pretend_to_be([Any, Mu]);
Perl6::Metamodel::PackageHOW.delegate_methods_to(Any);
Perl6::Metamodel::ModuleHOW.pretend_to_be([Any, Mu]);
Perl6::Metamodel::ModuleHOW.delegate_methods_to(Any);

# Make roles handle invocations.
my $role_invoke_handler := nqp::getstaticcode(sub ($self, *@pos, *%named) {
    $self.HOW.pun($self)(|@pos, |%named)
});
Perl6::Metamodel::ParametricRoleGroupHOW.set_default_invoke_handler($role_invoke_handler);
Perl6::Metamodel::ParametricRoleHOW.set_default_invoke_handler($role_invoke_handler);
Perl6::Metamodel::CurriedRoleHOW.set_default_invoke_handler($role_invoke_handler);

# Let ClassHOW and EnumHOW know about the invocation handler.
Perl6::Metamodel::ClassHOW.set_default_invoke_handler(
    Mu.HOW.invocation_handler(Mu));
Perl6::Metamodel::EnumHOW.set_default_invoke_handler(
    Mu.HOW.invocation_handler(Mu));

# Configure the MOP (not persisted as it ends up in a lexical...)
Perl6::Metamodel::Configuration.set_stash_type(Stash, Map);
Perl6::Metamodel::Configuration.set_submethod_type(Submethod);

# Register default parent types.
Perl6::Metamodel::ClassHOW.set_default_parent_type(Any);
Perl6::Metamodel::GrammarHOW.set_default_parent_type(Grammar);

# Put PROCESS in place, and ensure it's never repossessed.
nqp::neverrepossess(PROCESS.WHO);
nqp::neverrepossess(nqp::getattr(PROCESS.WHO, Map, '$!storage'));
nqp::bindhllsym('Raku', 'PROCESS', PROCESS);

# Stash Scalar and a default container spec away in the HLL state.
nqp::bindhllsym('Raku', 'Scalar', Scalar);
nqp::bindhllsym('Raku', 'default_cont_spec',
    Scalar.HOW.cache_get(Scalar, 'default_cont_spec'));
nqp::bindhllsym('Raku', 'Capture', Capture);

#?if jvm
# On JVM, set up JVM interop bits.
nqp::gethllsym('Raku', 'JavaModuleLoader').set_interop_loader(-> {
    nqp::jvmrakudointerop()
});
Perl6::Metamodel::JavaHOW.pretend_to_be([Any, Mu]);
#?endif

# vim: expandtab sw=4
