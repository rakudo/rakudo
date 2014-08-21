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

#?if moar
# On MoarVM, implement the signature binder.
my class Binder {
    # Flags that can be set on a signature element.
    my int $SIG_ELEM_BIND_CAPTURE        := 1;
    my int $SIG_ELEM_BIND_PRIVATE_ATTR   := 2;
    my int $SIG_ELEM_BIND_PUBLIC_ATTR    := 4;
    my int $SIG_ELEM_BIND_ATTRIBUTIVE    := ($SIG_ELEM_BIND_PRIVATE_ATTR +| $SIG_ELEM_BIND_PUBLIC_ATTR);
    my int $SIG_ELEM_SLURPY_POS          := 8;
    my int $SIG_ELEM_SLURPY_NAMED        := 16;
    my int $SIG_ELEM_SLURPY_LOL          := 32;
    my int $SIG_ELEM_SLURPY              := ($SIG_ELEM_SLURPY_POS +| SIG_ELEM_SLURPY_NAMED +| $SIG_ELEM_SLURPY_LOL);
    my int $SIG_ELEM_INVOCANT            := 64;
    my int $SIG_ELEM_MULTI_INVOCANT      := 128;
    my int $SIG_ELEM_IS_RW               := 256;
    my int $SIG_ELEM_IS_COPY             := 512;
    my int $SIG_ELEM_IS_PARCEL           := 1024;
    my int $SIG_ELEM_IS_OPTIONAL         := 2048;
    my int $SIG_ELEM_ARRAY_SIGIL         := 4096;
    my int $SIG_ELEM_HASH_SIGIL          := 8192;
    my int $SIG_ELEM_DEFAULT_FROM_OUTER  := 16384;
    my int $SIG_ELEM_IS_CAPTURE          := 32768;
    my int $SIG_ELEM_UNDEFINED_ONLY      := 65536;
    my int $SIG_ELEM_DEFINED_ONLY        := 131072;
    my int $SIG_ELEM_DEFINEDNES_CHECK    := ($SIG_ELEM_UNDEFINED_ONLY +| $SIG_ELEM_DEFINED_ONLY);
    my int $SIG_ELEM_NOMINAL_GENERIC     := 524288;
    my int $SIG_ELEM_DEFAULT_IS_LITERAL  := 1048576;
    my int $SIG_ELEM_NATIVE_INT_VALUE    := 2097152;
    my int $SIG_ELEM_NATIVE_NUM_VALUE    := 4194304;
    my int $SIG_ELEM_NATIVE_STR_VALUE    := 8388608;
    my int $SIG_ELEM_NATIVE_VALUE        := ($SIG_ELEM_NATIVE_INT_VALUE +| $SIG_ELEM_NATIVE_NUM_VALUE +| $SIG_ELEM_NATIVE_STR_VALUE);

    # Binding reuslt flags.
    my int $BIND_RESULT_OK       := 0;
    my int $BIND_RESULT_FAIL     := 1;
    my int $BIND_RESULT_JUNCTION := 2;

    my $autothreader;

    sub arity_fail($params, int $num_params, int $num_pos_args, int $too_many) {
        my str $error_prefix := $too_many ?? "Too many" !! "Not enough";
        my int $count;
        my int $arity;

        my int $param_i := 0;
        while $param_i < $num_params {
            my $param := nqp::atpos($params, $param_i);
            my $flags := nqp::getattr_i($param, Parameter, '$!flags');

            if !nqp::isnull(nqp::getattr($param, Parameter, '$!named_names')) {
            }
            elsif $flags +& $SIG_ELEM_SLURPY_NAMED {
            }
            elsif $flags +& $SIG_ELEM_SLURPY_POS {
                $count--;
            }
            elsif $flags +& $SIG_ELEM_IS_OPTIONAL {
                $count++
            }
            else {
                $count++;
                $arity++;
            }

            $param_i++;
        }

        if $arity == $count {
            return "$error_prefix positional parameters passed; got $num_pos_args but expected $arity";
        } elsif $count == -1 {
            return "$error_prefix positional parameters passed; got $num_pos_args but expected at least $arity";
        } else {
            return "$error_prefix positional parameters passed; got $num_pos_args but expected between $arity and $count";
        }
    }

    method set_autothreader($callable) {
        $autothreader := $callable;
    }

    # Binds a single parameter.
    sub bind_one_param($lexpad, $sig, $param, int $no_nom_type_check, $error,
                       $got_native, $oval, int $ival, num $nval, str $sval) {
        # Grab flags and variable name.
        my int $flags       := nqp::getattr_i($param, Parameter, '$!flags');
        my str $varname     := nqp::getattr_s($param, Parameter, '$!variable_name');
        my int $has_varname := 1;
        if nqp::isnull_s($varname) {
            $varname := '<anon>';
            $has_varname := 0;
        }

        # Check if boxed/unboxed expections are met.
        my int $desired_native := $flags +& $SIG_ELEM_NATIVE_VALUE;
        unless $desired_native == $got_native {
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
        my $nom_type;
        unless $got_native {
            # HLL-ize.
            $oval := nqp::hllizefor($oval, 'perl6');

            # Skip nominal type check if not needed.
            unless $no_nom_type_check {
                # Is the nominal type generic and in need of instantiation? (This
                # can happen in (::T, T) where we didn't learn about the type until
                # during the signature bind).
                $nom_type := nqp::getattr($param, Parameter, '$!nominal_type');
                if $flags +& $SIG_ELEM_NOMINAL_GENERIC {
                    $nom_type := $nom_type.HOW.instantiate_generic($nom_type, $lexpad);
                }

                # If not, do the check. If the wanted nominal type is Mu, then
                # anything goes.
                unless $nom_type =:= Mu || nqp::istype($oval, $nom_type) {
                    # Type check failed; produce error if needed.
                    if nqp::defined($error) {
                        my %ex := nqp::gethllsym('perl6', 'P6EX');
                        if nqp::isnull(%ex) || !nqp::existskey(%ex, 'X::TypeCheck::Binding') {
                            $error[0] := "Nominal type check failed for parameter '" ~ $varname ~
                                "'; expected " ~ $nom_type.HOW.name($nom_type) ~
                                " but got " ~ $oval.HOW.name($oval);
                        } else {
                            $error[0] := { nqp::atkey(%ex, 'X::TypeCheck::Binding')($oval.WHAT, $nom_type.WHAT, $varname) };
                        }
                    }

                    # Report junction failure mode if it's a junction.
                    return $oval.WHAT =:= Junction ?? $BIND_RESULT_JUNCTION !! $BIND_RESULT_FAIL;
                }
            
                # Also enforce definedness constraints.
                if $flags +& $SIG_ELEM_DEFINEDNES_CHECK {
                    if $flags +& $SIG_ELEM_UNDEFINED_ONLY && nqp::isconcrete($oval) {
                        if nqp::defined($error) {
                            if $flags +& $SIG_ELEM_INVOCANT {
                                $error[0] := "Invocant requires a type object, but an object instance was passed";
                            }
                            else {
                                $error[0] := "Parameter '$varname' requires a type object, but an object instance was passed";
                            }
                        }
                        return $oval.WHAT =:= Junction ?? $BIND_RESULT_JUNCTION !! $BIND_RESULT_FAIL;
                    }
                    if $flags +& $SIG_ELEM_DEFINED_ONLY && !nqp::isconcrete($oval) {
                        if nqp::defined($error) {
                            if $flags +& $SIG_ELEM_INVOCANT {
                                $error[0] := "Invocant requires an instance, but a type object was passed";
                            }
                            else {
                                $error[0] := "Parameter '$varname' requires an instance, but a type object was passed";
                            }
                        }
                        return $oval.WHAT =:= Junction ?? $BIND_RESULT_JUNCTION !! $BIND_RESULT_FAIL;
                    }
                }
            }
        }

        # Do we have any type captures to bind?
        my $type_caps := nqp::getattr($param, Parameter, '$!type_captures');
        unless nqp::isnull($type_caps) {
            my int $num_type_caps := nqp::elems($type_caps);
            my int $i := 0;
            while $i < $num_type_caps {
                nqp::bindkey($lexpad, nqp::atpos($type_caps, $i), $oval.WHAT);
                $i++;
            }
        }

        # Do a coercion, if one is needed.
        my $coerce_type := nqp::getattr($param, Parameter, '$!coerce_type');
        unless nqp::isnull($coerce_type) {
            # Coercing natives not possible - nothing to call a method on.
            if $got_native {
                if nqp::defined($error) {
                    $error[0] := "Unable to coerce natively typed parameter '$varname'";
                }
                return $BIND_RESULT_FAIL;
            }

            # Only coerce if we don't already have the correct type.
            unless nqp::istype($oval, $coerce_type) {
                my $coerce_method := nqp::getattr($param, Parameter, '$!coerce_method');
                if nqp::can($oval, $coerce_method) {
                    $oval := $oval."$coerce_method"();
                }
                else {
                    # No coercion method availale; whine and fail to bind.
                    if nqp::defined($error) {
                        $error[0] := "Unable to coerce value for '$varname' from " ~
                            $oval.HOW.name($oval) ~
                            " to $coerce_method; no coercion method defined";
                    }
                    return $BIND_RESULT_FAIL;
                }
            }
        }

        # If it's not got attributive binding, we'll go about binding it into the
        # lex pad.
        my int $is_attributive := $flags +& $SIG_ELEM_BIND_ATTRIBUTIVE;
        unless $is_attributive || !$has_varname {
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
            elsif $flags +& $SIG_ELEM_IS_RW {
                # XXX TODO Check if rw flag is set; also need to have a
                # wrapper container that carries extra constraints.
                nqp::bindkey($lexpad, $varname, $oval);
            }
            elsif $flags +& $SIG_ELEM_IS_PARCEL {
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
                # container and store it, for copy or ro case (the rw bit
                # in the container descriptor takes care of the rest).
                else {
                    my $new_cont := nqp::create(Scalar);
                    nqp::bindattr($new_cont, Scalar, '$!descriptor',
                        nqp::getattr($param, Parameter, '$!container_descriptor'));
                    nqp::bindattr($new_cont, Scalar, '$!value', nqp::decont($oval));
                    nqp::bindkey($lexpad, $varname, $new_cont);
                }
            }
        }

        # Is it the invocant? If so, also have to bind to self lexical.
        if $flags +& $SIG_ELEM_INVOCANT {
            nqp::bindkey($lexpad, 'self', nqp::decont($oval));
        }

        # Handle any constraint types (note that they may refer to the parameter by
        # name, so we need to have bound it already).
        my $post_cons := nqp::getattr($param, Parameter, '$!post_constraints');
        unless nqp::isnull($post_cons) {
            my int $n := nqp::elems($post_cons);
            my int $i := 0;
            while $i < $n {
                # Check we meet the constraint.
                my $cons_type := nqp::atpos($post_cons, $i);
                if nqp::istype($cons_type, Code) {
                    $cons_type := nqp::p6capturelexwhere($cons_type.clone());
                }
                my $result;
                if $got_native == 0 {
                    $result := $cons_type.ACCEPTS($oval);
                }
                elsif $got_native == $SIG_ELEM_NATIVE_INT_VALUE {
                    $result := $cons_type.ACCEPTS($ival);
                }
                elsif $got_native == $SIG_ELEM_NATIVE_NUM_VALUE {
                    $result := $cons_type.ACCEPTS($nval);
                }
                elsif $got_native == $SIG_ELEM_NATIVE_STR_VALUE {
                    $result := $cons_type.ACCEPTS($sval);
                }
                unless $result {
                    if nqp::defined($error) {
                        $error[0] := "Constraint type check failed for parameter '$varname'";
                    }
                    return $BIND_RESULT_FAIL;
                }
                $i++;
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
                    if $error {
                        $error[0] := "Could not turn argument into capture";
                    }
                    return $BIND_RESULT_FAIL;
                }
            }

            # Recurse into signature binder.
            my $result := bind(make_vm_capture($capture), $subsig, $lexpad,
                $no_nom_type_check, $error);
            unless $result == $BIND_RESULT_OK {
                if $error {
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
                $default_value()
            }
        }

        # Otherwise, go by sigil to pick the correct default type of value.
        else {
            if $flags +& $SIG_ELEM_ARRAY_SIGIL {
                my $result := nqp::create(Array);
                nqp::bindattr($result, List, '$!flattens', nqp::p6bool(1));
                $result
            }
            elsif $flags +& $SIG_ELEM_HASH_SIGIL {
                nqp::create(Hash)
            }
            else {
                nqp::getattr($param, Parameter, '$!nominal_type');
            }
        }
    }

    # Drives the overall binding process.
    sub bind($capture, $sig, $lexpad, int $no_nom_type_check, $error) {
        # Get params.
        my @params := nqp::getattr($sig, Signature, '$!params');
        
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
            $i := $i + 1;
            
            # Is it looking for us to bind a capture here?
            my int $bind_fail;
            my int $got_prim;
            if $flags +& $SIG_ELEM_IS_CAPTURE {
                # Capture the arguments from this point forwards into a Capture.
                # Of course, if there's no variable name we can (cheaply) do pretty
                # much nothing.
                if nqp::isnull_s($var_name) {
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
                        $k++;
                    }
                    nqp::bindattr($capsnap, Capture, '$!list', @pos_args);

                    if $named_args {
                        nqp::bindattr($capsnap, Capture, '$!hash', nqp::clone($named_args));
                    }
                    else {
                        nqp::bindattr($capsnap, Capture, '$!hash', nqp::hash());
                    }

                    $bind_fail := bind_one_param($lexpad, $sig, $param, $no_nom_type_check, $error,
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
                # will by definition contain all unbound named parameters and use
                # that. Otherwise, putting Mu in there is fine; Hash is smart
                # enough to know what to do.
                my $hash := nqp::create(Hash);
                nqp::bindattr($hash, EnumMap, '$!storage', $named_args || Mu);
                $bind_fail := bind_one_param($lexpad, $sig, $param, $no_nom_type_check, $error,
                    0, $hash, 0, 0.0, '');
                return $bind_fail if $bind_fail;
                
                # Undefine named arguments hash now we've consumed it, to mark all
                # is well.
                $named_args := NQPMu;
            }
            
            # Otherwise, maybe it's a positional.
            elsif nqp::isnull($named_names := nqp::getattr($param, Parameter, '$!named_names')) {
                # Slurpy or LoL-slurpy?
                if $flags +& ($SIG_ELEM_SLURPY_POS +| $SIG_ELEM_SLURPY_LOL) {
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
                        $cur_pos_arg++;
                    }
                    my int $flatten := $flags +& $SIG_ELEM_SLURPY_POS;
                    my $bindee := nqp::create($flatten
                        ?? ($flags +& $SIG_ELEM_IS_RW ?? List !! Array)
                        !! LoL);
                    my $listiter := nqp::create(ListIter);
                    nqp::bindattr($listiter, ListIter, '$!rest', $temp);
                    nqp::bindattr($listiter, ListIter, '$!list', $bindee);
                    nqp::bindattr($bindee, List, '$!nextiter', $listiter);
                    nqp::bindattr($bindee, List, '$!flattens', nqp::p6bool($flatten));
                    $bind_fail := bind_one_param($lexpad, $sig, $param, $no_nom_type_check, $error,
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
                            $bind_fail := bind_one_param($lexpad, $sig, $param, $no_nom_type_check, $error,
                                0, nqp::captureposarg($capture, $cur_pos_arg), 0, 0.0, '');
                        }
                        elsif $got_prim == 1 {
                            $bind_fail := bind_one_param($lexpad, $sig, $param, $no_nom_type_check, $error,
                                $SIG_ELEM_NATIVE_INT_VALUE, nqp::null(), nqp::captureposarg_i($capture, $cur_pos_arg), 0.0, '');
                        }
                        elsif $got_prim == 2 {
                            $bind_fail := bind_one_param($lexpad, $sig, $param, $no_nom_type_check, $error,
                                $SIG_ELEM_NATIVE_NUM_VALUE, nqp::null(), 0, nqp::captureposarg_n($capture, $cur_pos_arg), '');
                        }
                        else {
                            $bind_fail := bind_one_param($lexpad, $sig, $param, $no_nom_type_check, $error,
                                $SIG_ELEM_NATIVE_STR_VALUE, nqp::null(), 0, 0.0, nqp::captureposarg_s($capture, $cur_pos_arg));
                        }
                        return $bind_fail if $bind_fail;
                        $cur_pos_arg++;
                    }
                    else {
                        # No value. If it's optional, fetch a default and bind that;
                        # if not, we're screwed. Note that we never nominal type check
                        # an optional with no value passed.
                        if $flags +& $SIG_ELEM_IS_OPTIONAL {
                            $bind_fail := bind_one_param($lexpad, $sig, $param, $no_nom_type_check, $error,
                                0, handle_optional($param, $flags, $lexpad), 0, 0.0, '');
                            return $bind_fail if $bind_fail;
                        }
                        else {
                            if nqp::defined($error) {
                                $error[0] := arity_fail(@params, $num_params, $num_pos_args, 0);
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
                    my int $j := 0;
                    my str $cur_name;
                    while $j < $num_names {
                        $cur_name := nqp::atpos($named_names, $j);
                        $value := nqp::atkey($named_args, $cur_name);
                        unless nqp::isnull($value) {
                            nqp::deletekey($named_args, $cur_name);
                            $j := $num_names;
                        }
                        ++$j;
                    }
                }

                # Did we get one?
                if nqp::isnull($value) {
                    # Nope. We'd better hope this param was optional...
                    if $flags +& $SIG_ELEM_IS_OPTIONAL {
                        $bind_fail := bind_one_param($lexpad, $sig, $param, $no_nom_type_check, $error,
                            0, handle_optional($param, $flags, $lexpad), 0, 0.0, '');
                    }
                    elsif !$suppress_arity_fail {
                        if nqp::defined($error) {
                            $error[0] := "Required named parameter '" ~
                                $named_names[0] ~ "' not passed";
                        }
                        return $BIND_RESULT_FAIL;
                    }
                }
                else {
                    $bind_fail := bind_one_param($lexpad, $sig, $param, $no_nom_type_check, $error,
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
                $error[0] := arity_fail(@params, $num_params, $num_pos_args, 1);
            }
            return $BIND_RESULT_FAIL;
        }
        if $named_args {
            # Oh noes, unexpected named args.
            if $error {
                my int $num_extra := nqp::elems($named_args);
                my @names;
                for $named_args {
                    nqp::push(@names, $_.key);
                }
                if $num_extra == 1 {
                    $error[0] := "Unexpected named parameter '" ~ @names[0] ~ "' passed";
                }
                else {
                    $error[0] := $num_extra ~ " unexpected named parameters passed (" ~
                        nqp::join(",", @names) ~")";
                }
            }
            return $BIND_RESULT_FAIL;
        }
        
        # If we get here, we're done.
        return $BIND_RESULT_OK;
    }

    method bind($capture, $sig, $lexpad, int $no_nom_type_check, $error) {
        bind($capture, $sig, $lexpad, $no_nom_type_check, $error);
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
                my int $k := 0;
                my int $got_prim;
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
                    $k++;
                }
                my %named_args := nqp::capturenamedshash($capture);
                return Junction.AUTOTHREAD($caller,
                        |@pos_args,
                        |%named_args);
            }
            else {
                nqp::die(@error[0]);
            }
        }
        nqp::null();
    }
    
    sub make_vm_capture($capture) {
        sub vm_capture(*@pos, *%named) { nqp::savecapture() }                
        my @list := nqp::getattr($capture, Capture, '$!list');
        @list    := nqp::list() unless nqp::islist(@list);
        my %hash := nqp::getattr($capture, Capture, '$!hash');
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
            nqp::die(@error[0]);
        }
        $sig
    }

    my int $TRIAL_BIND_NOT_SURE :=  0;   # Plausible, but need to check at runtime.
    my int $TRIAL_BIND_OK       :=  1;   # Bind will always work out.
    my int $TRIAL_BIND_NO_WAY   := -1;   # Bind could never work out.
    method trial_bind($sig, $args, $sigflags) {
        my @params         := nqp::getattr($sig, Signature, '$!params');
        my int $num_params := nqp::elems(@params);

        # If there's a single capture parameter, then we're OK. (Worth
        # handling especially as it's the common case for protos).
        if $num_params == 1 {
            if nqp::getattr_i(@params[0], Parameter, '$!flags') +& $SIG_ELEM_IS_CAPTURE {
                return $TRIAL_BIND_OK;
            }
        }

        # Walk through the signature and consider the parameters.
        my int $num_pos_args := nqp::elems($args);
        my int $cur_pos_arg  := 0;
        my int $i            := 0;
        while $i < $num_params {
            my $param := @params[$i];
            $i++;

            # If the parameter is anything other than a boring old
            # positional parameter, we won't analyze it. */
            my int $flags := nqp::getattr_i($param, Parameter, '$!flags');
            if $flags +& nqp::bitneg_i(
                    $SIG_ELEM_MULTI_INVOCANT +| $SIG_ELEM_IS_PARCEL +|
                    $SIG_ELEM_IS_COPY +| $SIG_ELEM_ARRAY_SIGIL +|
                    $SIG_ELEM_HASH_SIGIL +| $SIG_ELEM_NATIVE_VALUE +|
                    $SIG_ELEM_IS_OPTIONAL) {
                return $TRIAL_BIND_NOT_SURE;
            }
            unless nqp::isnull(nqp::getattr($param, Parameter, '$!named_names')) {
                return $TRIAL_BIND_NOT_SURE;
            }
            unless nqp::isnull(nqp::getattr($param, Parameter, '$!post_constraints')) {
                return $TRIAL_BIND_NOT_SURE;
            }
            unless nqp::isnull(nqp::getattr($param, Parameter, '$!type_captures')) {
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
                my int $got_prim := $sigflags[$cur_pos_arg];
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
                    my $nom_type := nqp::getattr($param, Parameter, '$!nominal_type');
                    unless $nom_type =:= Mu || nqp::istype($arg, $nom_type) {
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
                        return nqp::istype($nom_type, $arg.WHAT)
                            ?? $TRIAL_BIND_NOT_SURE
                            !! $TRIAL_BIND_NO_WAY;
                    }
                }
            }

            # Continue to next argument.
            $cur_pos_arg++;
        }

        # If we have any left over arguments, it's a binding fail.
        if $cur_pos_arg < $num_pos_args {
            return $TRIAL_BIND_NO_WAY;
        }

        # Otherwise, if we get there, all is well.
        return $TRIAL_BIND_OK;
    }

    method get_return_type($code) {
        nqp::getattr(nqp::getattr($code, Code, '$!signature'), Signature, '$!returns')
    }
}
BEGIN { nqp::p6setbinder(Binder); } # We need it in for the next BEGIN block
nqp::p6setbinder(Binder);           # The load-time case.
#?endif

# We stick all the declarative bits inside of a BEGIN, so they get
# serialized.
BEGIN {
    # Ensure Rakudo runtime support is initialized.
    nqp::p6init();

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

    # class Attribute is Any {
    #     has str $!name;
    #     has int $!rw;
    #     has int $!has_accessor;
    #     has Mu $!type;
    #     has Mu $!container_descriptor;
    #     has Mu $!auto_viv_container;
    #     has Mu $!build_closure;
    #     has Mu $!package;
    #     has int $!positional_delegate;
    #     has int $!associative_delegate;
    #     has Mu $!why;
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
    Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(:name<$!why>, :type(Mu), :package(Attribute)));

    # Need new and accessor methods for Attribute in here for now.
    Attribute.HOW.add_method(Attribute, 'new',
        nqp::getstaticcode(sub ($self, :$name!, :$type!, :$package!, :$has_accessor,
                :$positional_delegate = 0, :$associative_delegate = 0, *%other) {
            my $attr := nqp::create($self);
            nqp::bindattr_s($attr, Attribute, '$!name', $name);
            nqp::bindattr($attr, Attribute, '$!type', nqp::decont($type));
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
    #     has Mu $!descriptor;
    #     has Mu $!value;
    #     has Mu $!whence;
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
    #    has Mu &!FETCH;
    #    has Mu &!STORE;
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
        
    # class Signature is Any{
    #    has Mu $!params;
    #    has Mu $!returns;
    #    has Mu $!arity;
    #    has Mu $!count;
    #    has Mu $!code;
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
        
    # class Parameter is Any {
    #     has str $!variable_name
    #     has Mu $!named_names
    #     has Mu $!type_captures
    #     has int $!flags
    #     has Mu $!nominal_type
    #     has Mu $!post_constraints
    #     has Mu $!coerce_type
    #     has str $!coerce_method
    #     has Mu $!sub_signature
    #     has Mu $!default_value
    #     has Mu $!container_descriptor;
    #     has Mu $!attr_package;
    #     has Mu $!why;
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
    Parameter.HOW.add_attribute(Parameter, BOOTSTRAPATTR.new(:name<$!why>, :type(Mu), :package(Parameter)));
    Parameter.HOW.add_method(Parameter, 'is_generic', nqp::getstaticcode(sub ($self) {
            # If nonimnal type or attr_package is generic, so are we.
            my $type := nqp::getattr($self, Parameter, '$!nominal_type');
            my $ap   := nqp::getattr($self, Parameter, '$!attr_package');
            nqp::p6bool($type.HOW.archetypes.generic ||
                (!nqp::isnull($ap) && $ap.HOW.archetypes.generic))
        }));
    Parameter.HOW.add_method(Parameter, 'instantiate_generic', nqp::getstaticcode(sub ($self, $type_environment) {
            # Clone with the type instantiated.
            my $SIG_ELEM_NOMINAL_GENERIC := 524288;
            my $ins      := nqp::clone($self);
            my $type     := nqp::getattr($self, Parameter, '$!nominal_type');
            my $cd       := nqp::getattr($self, Parameter, '$!container_descriptor');
            my $ap       := nqp::getattr($self, Parameter, '$!attr_package');
            my $ins_type := $type;
            my $ins_cd   := $cd;
            if $type.HOW.archetypes.generic {
                $ins_type := $type.HOW.instantiate_generic($type, $type_environment);
                $ins_cd   := nqp::isnull($cd) ?? $cd !! $cd.instantiate_generic($type_environment);
            }
            my $ins_ap := !nqp::isnull($ap) && $ap.HOW.archetypes.generic
                ?? $ap.HOW.instantiate_generic($ap, $type_environment)
                !! $ap;
            unless $ins_type.HOW.archetypes.generic {
                my $flags := nqp::getattr_i($ins, Parameter, '$!flags');
                if $flags +& $SIG_ELEM_NOMINAL_GENERIC {
                    nqp::bindattr_i($ins, Parameter, '$!flags',
                        $flags - $SIG_ELEM_NOMINAL_GENERIC)
                }
            }
            nqp::bindattr($ins, Parameter, '$!nominal_type', $ins_type);
            nqp::bindattr($ins, Parameter, '$!container_descriptor', $ins_cd);
            nqp::bindattr($ins, Parameter, '$!attr_package', $ins_ap);
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
            $dcself
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
            nqp::bindattr($dcself, Parameter, '$!coerce_type', nqp::decont($type));
            $dcself
        }));
    Parameter.HOW.compose_repr(Parameter);
    
    # class Code {
    #     has Mu $!do;                # Low level code object
    #     has Mu $!signature;         # Signature object
    #     has Mu $!compstuff;         # Place for the compiler to hang stuff
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
    Code.HOW.set_invocation_attr(Code, Code, '$!do');
    Code.HOW.compose_invocation(Code);

    # class Block is Code {
    #     has Mu $!phasers;                # phasers for this block
    Block.HOW.add_parent(Block, Code);
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
            $cloned
        }));
    Block.HOW.compose_repr(Block);
    Block.HOW.compose_invocation(Block);

    # class Routine is Block {
    #     has Mu $!dispatchees;
    #     has Mu $!dispatcher_cache;
    #     has Mu $!dispatcher;
    #     has int $!rw;
    #     has Mu $!inline_info;
    #     has int $!yada;
    #     has Mu $!package;
    #     has int $!onlystar;
    #     has Mu $!dispatch_order;
    #     has Mu $!dispatch_cache;
    #     has Mu $!why;
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
    Routine.HOW.add_attribute(Routine, BOOTSTRAPATTR.new(:name<$!why>, :type(Mu), :package(Routine)));
    
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
            my int $SIG_ELEM_SLURPY_LOL          := 32;
            my int $SIG_ELEM_MULTI_INVOCANT      := 128;
            my int $SIG_ELEM_IS_OPTIONAL         := 2048;
            my int $SIG_ELEM_IS_CAPTURE          := 32768;
            my int $SIG_ELEM_UNDEFINED_ONLY      := 65536;
            my int $SIG_ELEM_DEFINED_ONLY        := 131072;
            my int $SIG_ELEM_NOMINAL_GENERIC     := 524288;
            my int $SIG_ELEM_NATIVE_INT_VALUE    := 2097152;
            my int $SIG_ELEM_NATIVE_NUM_VALUE    := 4194304;
            my int $SIG_ELEM_NATIVE_STR_VALUE    := 8388608;
            
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
                    elsif (nqp::atpos_i(%a<type_flags>, $i) +& $TYPE_NATIVE_MASK)
                      && !(nqp::atpos_i(%b<type_flags>, $i) +& $TYPE_NATIVE_MASK) {
                        # Narrower because natives always are.
                        $narrower++;
                    }
                    elsif (nqp::atpos_i(%b<type_flags>, $i) +& $TYPE_NATIVE_MASK)
                      && !(nqp::atpos_i(%a<type_flags>, $i) +& $TYPE_NATIVE_MASK) {
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
                                %info<req_named> := nqp::atpos($named_names, 0);
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
                    if $flags +& ($SIG_ELEM_SLURPY_POS +| $SIG_ELEM_SLURPY_LOL +| $SIG_ELEM_IS_CAPTURE) {
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

            # Now analyze type narrowness of the candidates relative to each
            # other and create the edges.
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
                    nqp::die("Circularity detected in multi sub types" ~ ($self.name ?? " for &" ~ $self.name !! ''));
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
            my int $done_bind_check := 0;
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
#?if !parrot
                                unless $done_bind_check {
                                    # Need a copy of the capture, as we may later do a
                                    # multi-dispatch when evaluating the constraint.
                                    $capture := nqp::clone($capture);
                                    $done_bind_check := 1;
                                }
#?endif
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
            sub add_to_cache($entry) {
                unless nqp::capturehasnameds($capture) {
                    nqp::scwbdisable();
                    nqp::bindattr($dcself, Routine, '$!dispatch_cache',
                        nqp::multicacheadd(
                            nqp::getattr($dcself, Routine, '$!dispatch_cache'),
                            $capture, $entry));
                    nqp::scwbenable();
                }
            }
            if nqp::elems(@possibles) == 1 && $pure_type_result {
                add_to_cache(nqp::atkey(nqp::atpos(@possibles, 0), 'sub'));
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
    Routine.HOW.add_method(Routine, 'rw', nqp::getstaticcode(sub ($self) {
            my $dcself := nqp::decont($self);
            nqp::p6bool(nqp::getattr_i($dcself, Routine, '$!rw'));
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
    Routine.HOW.add_method(Routine, 'clone', nqp::getstaticcode(sub ($self) {
            my $dcself := nqp::decont($self);
            my $cloned := nqp::findmethod(Block, 'clone')($self);

            # XXX this should probably be done after the clone that installs
            #     the sub
            my $why := nqp::getattr($dcself, Routine, '$!why');
            unless nqp::isnull($why) {
                $why.set_docee($cloned);
            }
            $cloned
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
    Method.HOW.add_method(Method, 'clone', nqp::getstaticcode(sub ($self) {
            my $dcself := nqp::decont($self);
            my $clone  := nqp::findmethod(Routine, 'clone')($self);

            # XXX this should probably be done after the clone that installs
            #     the method
            my $why := nqp::getattr($dcself, Routine, '$!why');
            unless nqp::isnull($why) {
                $why.set_docee($self);
            }

            $clone
        }));
    Method.HOW.compose_repr(Method);
    Method.HOW.compose_invocation(Method);

    # class Submethod is Routine {
    Submethod.HOW.add_parent(Submethod, Routine);
    Submethod.HOW.compose_repr(Submethod);
    Submethod.HOW.compose_invocation(Submethod);

    # class Regex is Method {
    #     has Mu $!caps;
    #     has Mu $!nfa;
    #     has Mu $!alt_nfas
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

    # class Parcel is Cool {
    #     has Mu $!storage;    # VM's array of Parcel's elements
    Parcel.HOW.add_parent(Parcel, Cool);
    Parcel.HOW.add_attribute(Parcel, scalar_attr('$!storage', Mu, Parcel));
    Parcel.HOW.compose_repr(Parcel);

    # class Iterable is Any {
    Iterable.HOW.add_parent(Iterable, Any);
    Iterable.HOW.compose_repr(Iterable);

    # class Iterator is Iterable {
    Iterator.HOW.add_parent(Iterator, Iterable);
    Iterator.HOW.compose_repr(Iterator);

    # class Nil is Iterator {
    Nil.HOW.add_parent(Nil, Iterator);
    Nil.HOW.compose_repr(Nil);
    
    # class ListIter is Iterator {
    #     has Mu $!reified;
    #     has Mu $!nextiter;
    #     has Mu $!rest;
    #     has Mu $!list;
    ListIter.HOW.add_parent(ListIter, Iterator);
    ListIter.HOW.add_attribute(ListIter, scalar_attr('$!reified', Mu, ListIter));
    ListIter.HOW.add_attribute(ListIter, scalar_attr('$!nextiter', Mu, ListIter));
    ListIter.HOW.add_attribute(ListIter, scalar_attr('$!rest', Mu, ListIter));
    ListIter.HOW.add_attribute(ListIter, scalar_attr('$!list', Mu, ListIter));
    ListIter.HOW.compose_repr(ListIter);
    
    # class List is Iterable is Cool {
    #     has Mu $!items;
    #     has Mu $!flattens;
    #     has Mu $!nextiter;
    List.HOW.add_parent(List, Iterable);
    List.HOW.add_parent(List, Cool);
    List.HOW.add_attribute(List, scalar_attr('$!items', Mu, List));
    List.HOW.add_attribute(List, scalar_attr('$!flattens', Mu, List));
    List.HOW.add_attribute(List, scalar_attr('$!nextiter', Mu, List));
    List.HOW.compose_repr(List);

    # class Array is List {
    #     has Mu $!descriptor;
    Array.HOW.add_parent(Array, List);
    Array.HOW.add_attribute(Array, BOOTSTRAPATTR.new(:name<$!descriptor>, :type(Mu), :package(Array)));
    Array.HOW.compose_repr(Array);

    # class LoL is List {
    #     has Mu $!descriptor;
    LoL.HOW.add_parent(LoL, List);
    LoL.HOW.add_attribute(LoL, BOOTSTRAPATTR.new(:name<$!descriptor>, :type(Mu), :package(LoL)));
    LoL.HOW.compose_repr(LoL);

    # my class EnumMap is Iterable is Cool {
    #     has Mu $!storage;
    EnumMap.HOW.add_parent(EnumMap, Iterable);
    EnumMap.HOW.add_parent(EnumMap, Cool);
    EnumMap.HOW.add_attribute(EnumMap, scalar_attr('$!storage', Mu, EnumMap, :associative_delegate));
    EnumMap.HOW.compose_repr(EnumMap);

    # my class Hash is EnumMap {
    #     has Mu $!descriptor;
    Hash.HOW.add_parent(Hash, EnumMap);
    Hash.HOW.add_attribute(Hash, BOOTSTRAPATTR.new(:name<$!descriptor>, :type(Mu), :package(Hash)));
    Hash.HOW.compose_repr(Hash);

    # class Capture is Any {
    #     has Mu $!list;
    #     has Mu $!hash;
    Capture.HOW.add_parent(Capture, Any);
    Capture.HOW.add_attribute(Capture, BOOTSTRAPATTR.new(:name<$!list>, :type(Mu), :package(Capture)));
    Capture.HOW.add_attribute(Capture, BOOTSTRAPATTR.new(:name<$!hash>, :type(Mu), :package(Capture)));
    Capture.HOW.compose_repr(Capture);
    
    # class Junction is Mu {
    #     has Mu $!storage;
    #     has Mu $!type;
    Junction.HOW.add_parent(Junction, Mu);
    Junction.HOW.add_attribute(Junction, scalar_attr('$!storage', Mu, Junction));
    Junction.HOW.add_attribute(Junction, scalar_attr('$!type', Mu, Junction));
    Junction.HOW.compose_repr(Junction);
    
    # class Bool is Cool {
    #     has int $!value;
    Bool.HOW.add_parent(Bool, Cool);
    Bool.HOW.add_attribute(Bool, BOOTSTRAPATTR.new(:name<$!value>, :type(int), :box_target(1), :package(Bool)));
    Bool.HOW.set_boolification_mode(Bool, 1);
    Bool.HOW.publish_boolification_spec(Bool);
    Bool.HOW.compose_repr(Bool);

    # class ObjAt is Any {
    #     has str $!value;
    ObjAt.HOW.add_parent(ObjAt, Any);
    ObjAt.HOW.add_attribute(ObjAt, BOOTSTRAPATTR.new(:name<$!value>, :type(str), :box_target(1), :package(ObjAt)));
    ObjAt.HOW.compose_repr(ObjAt);
    
    # class ForeignCode {
    #     has Mu $!do;                # Code object we delegate to
    ForeignCode.HOW.add_parent(ForeignCode, Any);
    ForeignCode.HOW.add_attribute(ForeignCode, BOOTSTRAPATTR.new(:name<$!do>, :type(Mu), :package(ForeignCode)));
    ForeignCode.HOW.compose_repr(ForeignCode);
    ForeignCode.HOW.set_invocation_attr(ForeignCode, ForeignCode, '$!do');
    ForeignCode.HOW.compose_invocation(ForeignCode);

    # Set up Stash type, which is really just a hash.
    # class Stash is Hash {
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

    # Default invocation behavior delegates off to invoke.
    my $invoke_forwarder :=
        nqp::getstaticcode(sub ($self, *@pos, *%named) {
            if !nqp::isconcrete($self) && !nqp::can($self, 'invoke') && !nqp::can($self, 'postcircumfix:<( )>') {
                my $coercer_name := $self.HOW.name($self);
                nqp::die("Cannot coerce to $coercer_name with named parameters")
                  if +%named;
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
                nqp::can($self, 'invoke') ?? $self.invoke($c) !! $self.postcircumfix:<( )>($c);
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
    EXPORT::DEFAULT.WHO<Nil>       := Nil;
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
    hash( ACCEPTS => Mu, item => Mu ));
Perl6::Metamodel::ParametricRoleHOW.pretend_to_be([Cool, Any, Mu]);
Perl6::Metamodel::ParametricRoleHOW.configure_punning(
    Perl6::Metamodel::ClassHOW,
    hash( ACCEPTS => Mu, item => Mu ));
Perl6::Metamodel::CurriedRoleHOW.pretend_to_be([Cool, Any, Mu]);
Perl6::Metamodel::CurriedRoleHOW.configure_punning(
    Perl6::Metamodel::ClassHOW,
    hash( ACCEPTS => Mu, item => Mu ));
    
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

# HLL configuration: interop, boxing and exit handling.
nqp::sethllconfig('perl6', nqp::hash(
    'int_box', Int,
    'num_box', Num,
    'str_box', Str,
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
    },
#?if !parrot
    'exit_handler', -> $coderef, $resultish {
        my $code := nqp::getcodeobj($coderef);
        my %phasers := nqp::getattr($code, Block, '$!phasers');
        unless nqp::isnull(%phasers) || nqp::p6inpre() {
            my @leaves := nqp::atkey(%phasers, '!LEAVE-ORDER');
            my @keeps  := nqp::atkey(%phasers, 'KEEP');
            my @undos  := nqp::atkey(%phasers, 'UNDO');
            unless nqp::isnull(@leaves) {
                my int $n := nqp::elems(@leaves);
                my int $i := 0;
                my int $run;
                my $phaser;
                while $i < $n {
                    $phaser := nqp::decont(nqp::atpos(@leaves, $i));
                    $run := 1;
                    unless nqp::isnull(@keeps) {
                        for @keeps {
                            if nqp::decont($_) =:= $phaser {
                                $run := !nqp::isnull($resultish) && 
                                         nqp::isconcrete($resultish) &&
                                         $resultish.defined;
                                last;
                            }
                        }
                    }
                    unless nqp::isnull(@undos) {
                        for @undos {
                            if nqp::decont($_) =:= $phaser {
                                $run := nqp::isnull($resultish) ||
                                        !nqp::isconcrete($resultish) ||
                                        !$resultish.defined;
                                last;
                            }
                        }
                    }
                    if $run {
#?endif
#?if jvm
                        $phaser();
#?endif
#?if moar
                        nqp::p6capturelexwhere($phaser.clone())();
#?endif
#?if !parrot
                    }
                    $i++;
                }
            }
            
            my @posts := nqp::atkey(%phasers, 'POST');
            unless nqp::isnull(@posts) {
                my int $n := nqp::elems(@posts);
                my int $i := 0;
                while $i < $n {
#?endif
#?if jvm
                    nqp::atpos(@posts, $i)(nqp::ifnull($resultish, Mu));
#?endif
#?if moar
                    nqp::p6capturelexwhere(nqp::atpos(@posts, $i).clone())(
                        nqp::ifnull($resultish, Mu));
#?endif
#?if !parrot
                    $i++;
                }
            }
        }
    },
#?endif
#?if moar
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
                my int $k := 0;
                my int $got_prim;
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
                    $k++;
                }
                my %named_args := nqp::capturenamedshash($capture);
                Junction.AUTOTHREAD($caller,
                        |@pos_args,
                        |%named_args);
            }
            else {
                if nqp::isinvokable(@error[0]) {
                    @error[0]();
                }
                else {
                    nqp::die(@error[0]);
                }
            }
        }
        else {
            nqp::die("Internal error: inconsistent bind result");
        }
    },
    'method_not_found_error', -> $obj, str $name {
        my $type := $obj.HOW.name($obj);
        if $name eq 'STORE' {
            my %ex := nqp::gethllsym('perl6', 'P6EX');
            if !nqp::isnull(%ex) && nqp::existskey(%ex,'X::Assignment::RO') {
                nqp::atkey(%ex, 'X::Assignment::RO')($type);
            }
        }
        nqp::die("Method '$name' not found for invocant of class '$type'");
    }
#?endif
));

#?if jvm
# On JVM, set up JVM interop bits.
nqp::gethllsym('perl6', 'JavaModuleLoader').set_interop_loader(-> {
    nqp::jvmrakudointerop()
});
Perl6::Metamodel::JavaHOW.pretend_to_be([Any, Mu]);
#?endif
