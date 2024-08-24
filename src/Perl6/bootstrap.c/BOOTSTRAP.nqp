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
    method is_generic() { $!type.HOW.archetypes($!type).generic }
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
my stub ScalarVAR metaclass Perl6::Metamodel::ClassHOW { ... };
my stub Proxy metaclass Perl6::Metamodel::ClassHOW { ... };
my stub Signature metaclass Perl6::Metamodel::ClassHOW { ... };
my stub Parameter metaclass Perl6::Metamodel::ClassHOW { ... };
my stub Code metaclass Perl6::Metamodel::ClassHOW { ... };
my stub Block metaclass Perl6::Metamodel::ClassHOW { ... };
my stub Routine metaclass Perl6::Metamodel::ClassHOW { ... };
my stub Sub metaclass Perl6::Metamodel::ClassHOW { ... };
my stub Operator metaclass Perl6::Metamodel::ClassHOW { ... };
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
my stub array metaclass Perl6::Metamodel::ClassHOW is repr('VMArray') { ... };
my stub IterationBuffer metaclass Perl6::Metamodel::ClassHOW is repr('VMArray') { ... };
my stub Map metaclass Perl6::Metamodel::ClassHOW { ... };
my stub Hash metaclass Perl6::Metamodel::ClassHOW { ... };
my stub TypeEnv metaclass Perl6::Metamodel::ClassHOW { ... };
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
my stub Version metaclass Perl6::Metamodel::ClassHOW { ... };
my stub IntLexRef metaclass Perl6::Metamodel::NativeRefHOW { ... };
my stub UIntLexRef metaclass Perl6::Metamodel::NativeRefHOW { ... };
my stub NumLexRef metaclass Perl6::Metamodel::NativeRefHOW { ... };
my stub StrLexRef metaclass Perl6::Metamodel::NativeRefHOW { ... };
my stub IntAttrRef metaclass Perl6::Metamodel::NativeRefHOW { ... };
my stub UIntAttrRef metaclass Perl6::Metamodel::NativeRefHOW { ... };
my stub NumAttrRef metaclass Perl6::Metamodel::NativeRefHOW { ... };
my stub StrAttrRef metaclass Perl6::Metamodel::NativeRefHOW { ... };
my stub IntPosRef metaclass Perl6::Metamodel::NativeRefHOW { ... };
my stub UIntPosRef metaclass Perl6::Metamodel::NativeRefHOW { ... };
my stub NumPosRef metaclass Perl6::Metamodel::NativeRefHOW { ... };
my stub StrPosRef metaclass Perl6::Metamodel::NativeRefHOW { ... };
my stub IntMultidimRef metaclass Perl6::Metamodel::NativeRefHOW { ... };
my stub UIntMultidimRef metaclass Perl6::Metamodel::NativeRefHOW { ... };
my stub NumMultidimRef metaclass Perl6::Metamodel::NativeRefHOW { ... };
my stub StrMultidimRef metaclass Perl6::Metamodel::NativeRefHOW { ... };

#?if js
my stub Int64LexRef metaclass Perl6::Metamodel::NativeRefHOW { ... };
my stub Int64AttrRef metaclass Perl6::Metamodel::NativeRefHOW { ... };
my stub Int64PosRef metaclass Perl6::Metamodel::NativeRefHOW { ... };
my stub Int64MultidimRef metaclass Perl6::Metamodel::NativeRefHOW { ... };
#?endif

#- Binder ----------------------------------------------------------------------
# Implement the signature binder.
# The JVM backend really only uses trial_bind,
# so we exclude everything else.
my class Binder {

#?if !jvm

    my $autothreader;
    my $Positional;
    my $PositionalBindFailover;

    method set_autothreader($callable) {
        $autothreader := $callable;
    }

    method set_pos_bind_failover($pos, $pos_bind_failover) {
        $Positional := $pos;
        $PositionalBindFailover := $pos_bind_failover;
    }

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
            elsif $flags +& nqp::const::SIG_ELEM_SLURPY_NAMED {
            }
            elsif $flags +& nqp::const::SIG_ELEM_IS_SLURPY {
                $count := -1000;  # in case a pos can sneak past a slurpy somehow
            }
            elsif $flags +& nqp::const::SIG_ELEM_IS_OPTIONAL {
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

    # Helper sub to create block with concreteness fail error message
    sub concreteness_fail(
      :$lexpad,
      :$param_type,
      :$oval,
      :$varname,
      :$should_be_concrete,
      :$flags,
    ) {
        my $method := nqp::getcodeobj(nqp::ctxcode($lexpad)).name;
        my $class  := $param_type.HOW.name($param_type);
        my $got    := $oval.HOW.name($oval);

        my $msg := $flags +& nqp::const::SIG_ELEM_INVOCANT
          ?? $should_be_concrete
            ?? "Invocant of method '$method' must be an object instance of type '$class', not a type object of type '$got'.  Did you forget a '.new'?"
            !! "Invocant of method '$method' must be a type object of type '$class', not an object instance of type '$got'.  Did you forget a 'multi'?"
          !! $should_be_concrete
            ?? "Parameter '$varname' of routine '$method' must be an object instance of type '$class', not a type object of type '$got'.  Did you forget a '.new'?"
            !! "Parameter '$varname' of routine '$method' must be a type object of type '$class', not an object instance of type '$got'.  Did you forget a 'multi'?";

        -> {
          Perl6::Metamodel::Configuration.throw_or_die(
            'X::Parameter::InvalidConcreteness',
            $msg,
            :expected($class),
            :$got,
            :routine($method),
            :param($varname),
            :should-be-concrete(
              nqp::hllboolfor($should_be_concrete, 'Raku')
            ),
            :param-is-invocant(
              nqp::hllboolfor($flags +& nqp::const::SIG_ELEM_INVOCANT, 'Raku')
            )
          );
        }
    }

    # Helper sub to create block with typecheck fail error message
    sub typecheck_fail(
      :$param,
      :$varname,
      :$oval,
      :$param_type,
    ) {
        # Try to figure out the most helpful name for the
        # expected
        my $expected := (
          (my $post := nqp::getattr($param, Parameter,
            '@!post_constraints'))
          && nqp::not_i(nqp::istype(nqp::atpos($post, 0), Code))
        ) ?? nqp::atpos($post, 0)
          !! $param_type;

        my $msg :=
          "Nominal type check failed for parameter '"
          ~ $varname
          ~ "'; expected "
          ~ $expected.HOW.name($expected)
          ~ " but got "
          ~ $oval.HOW.name($oval);

        # A lot of beginners make mistakes when typing
        # array parameters, so let's try and catch some
        # of the common ones
        if nqp::eqaddr($param_type, $Positional) {

            # Positionals have an `of` method
            if nqp::istype($expected.of, Array) {
                $msg := $msg
                  ~ ". Did you mean to expect an array of Arrays?"
            }

            # but we don't know what $!got is and it may
            # not have an `of` method
            elsif nqp::can($oval, 'of') && nqp::istype($oval.of, Mu) {
                $msg := $msg
                  ~ ". You have to pass an explicitly typed array, not one that just might happen to contain elements of the correct type.";
            }
        }

        -> {
            Perl6::Metamodel::Configuration.throw_or_die(
              'X::TypeCheck::Binding::Parameter',
              $msg,
              :got($oval),
              :expected($expected.WHAT),
              :symbol(nqp::hllizefor($varname, 'Raku')),
              :parameter($param))
        }
    }

    # Helper sub to take a signature element and either runs the closure to
    # get a default value if there is one, or creates an appropriate
    # undefined-ish thingy.
    sub handle_optional($param, int $flags, $lexpad) {

        # Is the "get default from outer" flag set?
        if $flags +& nqp::const::SIG_ELEM_DEFAULT_FROM_OUTER {
            nqp::atkey(
              nqp::ctxouter($lexpad),
              nqp::getattr_s($param, Parameter, '$!variable_name')
            )
        }

        # Otherwise, go by sigil to pick the correct default type of value.
        elsif nqp::isnull(my $default_value := nqp::getattr(
          $param, Parameter, '$!default_value'
        )) {
            my $type := nqp::getattr($param, Parameter, '$!type');

            $flags +& nqp::const::SIG_ELEM_ARRAY_SIGIL
              ?? nqp::create(nqp::eqaddr($type, Positional)
                   ?? Array
                   !! Array.HOW.parameterize(Array, $type.of)
                 )
              !! $flags +& nqp::const::SIG_ELEM_HASH_SIGIL
                ?? nqp::create(nqp::eqaddr($type, Associative)
                     ?? Hash
                     !! Hash.HOW.parameterize(Hash, $type.of, $type.keyof)
                   )
                !! $type
        }

        # Do we have a default value or value closure?
        else {
            $flags +& nqp::const::SIG_ELEM_DEFAULT_IS_LITERAL
              ?? $default_value
              !! nqp::p6capturelexwhere($default_value.clone)()
        }
    }

    # Binds a single parameter.
    sub bind_one_param(
      $sig,
      $lexpad,
      int $no_param_type_check,
      $error,
      $param,
      int $got_native,
      $oval,
      int $ival,
      num $nval,
      str $sval
    ) {
        # Grab flags and variable name.
        my int $flags   := nqp::getattr_i($param, Parameter, '$!flags');
        my str $varname := nqp::getattr_s($param, Parameter, '$!variable_name');
        my int $has_varname;
        nqp::isnull_s($varname)
          ?? ($varname := '<anon>')
          !! ($has_varname := 1);

        # Check if boxed/unboxed expectations are met.
        my int $desired_native := $flags +& nqp::const::SIG_ELEM_NATIVE_VALUE;
        my int $is_rw          := $flags +& nqp::const::SIG_ELEM_IS_RW;
        if $is_rw && $desired_native {
            if $got_native {
                if $desired_native == nqp::const::SIG_ELEM_NATIVE_INT_VALUE
                  && nqp::not_i(nqp::iscont_i($oval))
                  ?? "int"
                  !! $desired_native == nqp::const::SIG_ELEM_NATIVE_UINT_VALUE
                       && nqp::not_i(nqp::iscont_u($oval))
                    ?? "unsigned int"
                    !! $desired_native == nqp::const::SIG_ELEM_NATIVE_NUM_VALUE
                         && nqp::not_i(nqp::iscont_n($oval))
                      ?? "num"
                      # SIG_ELEM_NATIVE_STR_VALUE
                      !! nqp::not_i(nqp::iscont_s($oval))
                        ?? "str"
                        !! 0 -> $expected {
                    nqp::bindpos($error, 0,
                      "Expected a modifiable native $expected argument for '$varname'"
                    ) if nqp::defined($error);

                    return nqp::const::BIND_RESULT_FAIL;
                }
            }
        }

        elsif $desired_native != $got_native {
            # Maybe we need to box the native.
            if $desired_native == 0 {
                $got_native == nqp::const::SIG_ELEM_NATIVE_INT_VALUE
                  ?? ($oval := nqp::box_i($ival, Int))
                  !! $got_native == nqp::const::SIG_ELEM_NATIVE_UINT_VALUE
                    ?? ($oval := nqp::box_u($ival, Int))
                    !! $got_native == nqp::const::SIG_ELEM_NATIVE_NUM_VALUE
                      ?? ($oval := nqp::box_n($nval, Num))
                      # assume SIG_ELEM_NATIVE_STR_VALUE
                      !! ($oval := nqp::box_s($sval, Str));
            }

            # Otherwise, maybe we need to unbox.
            elsif nqp::not_i($got_native) {

                # XXX Probably want to do this a little differently to get a
                # better error.
                $desired_native == nqp::const::SIG_ELEM_NATIVE_INT_VALUE
                  ?? ($ival := nqp::unbox_i($oval))
                  !! $desired_native == nqp::const::SIG_ELEM_NATIVE_UINT_VALUE
                    ?? ($ival := nqp::unbox_u($oval))
                    !! $desired_native == nqp::const::SIG_ELEM_NATIVE_NUM_VALUE
                      ?? ($nval := nqp::unbox_n($oval))
                      # assume  SIG_ELEM_NATIVE_STR_VALUE
                      !! ($sval := nqp::unbox_s($oval));
            }

            # Otherwise, incompatible native types.
            else {
                nqp::bindpos($error, 0,
                  "Incompatible native type passed for '$varname'"
                ) if nqp::defined($error);

                return nqp::const::BIND_RESULT_FAIL;
            }

            $got_native := $desired_native;
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
                # Is the nominal type generic and in need of instantiation?
                # (This can happen in (::T, T) where we didn't learn about
                # the type until during the signature bind).
                if $flags +& nqp::const::SIG_ELEM_TYPE_GENERIC {
                    $param_type :=
                      $param_type.HOW.instantiate_generic($param_type, $lexpad);
                }

                # If the expected type is Positional, see if we need to do the
                # positional bind failover.
                $oval := $oval.cache
                  if nqp::eqaddr($param_type, $Positional)
                  && nqp::istype($oval, $PositionalBindFailover);

                # If not, do the check. If the wanted nominal type is Mu, then
                # anything goes.
                unless nqp::eqaddr($param_type, Mu)
                  || nqp::istype($oval, $param_type) {

                    # Report junction failure mode if it's a junction.
                    return nqp::const::BIND_RESULT_JUNCTION
                      if nqp::eqaddr($oval.WHAT, Junction)
                      && nqp::isconcrete($oval);

                    # Type check failed; produce error if needed.
                    nqp::bindpos($error, 0, typecheck_fail(
                      :$oval, :$param, :$param_type, :$varname,
                    )) if nqp::defined($error);
                    return nqp::const::BIND_RESULT_FAIL;
                }

                # Also enforce definedness constraints.
                if $flags +& nqp::const::SIG_ELEM_DEFINEDNES_CHECK {
                    my int $isconcrete := nqp::isconcrete($oval);
                    my $should_be_concrete :=
                      $flags +& nqp::const::SIG_ELEM_DEFINED_ONLY
                      && nqp::not_i($isconcrete);

                    if $should_be_concrete
                      || $flags +& nqp::const::SIG_ELEM_UNDEFINED_ONLY
                           && $isconcrete {

                        # Report junction failure mode if it's a junction.
                        return nqp::const::BIND_RESULT_JUNCTION
                          if $isconcrete
                          && nqp::eqaddr($oval.WHAT, Junction);

                        # Concreteness check failed; produce error if needed.
                        nqp::bindpos($error, 0, concreteness_fail(
                          :$flags, :$lexpad, :$oval, :$param_type,
                          :$should_be_concrete, :$varname,
                        )) if nqp::defined($error);
                        return nqp::const::BIND_RESULT_FAIL;
                    }
                }
            }
        }

        # Do we have any type captures to bind?
        my $type_caps := nqp::getattr($param, Parameter, '@!type_captures');
        unless nqp::isnull($type_caps) {
            my int $num_type_caps := nqp::elems($type_caps);
            my int $i;
            while $i < $num_type_caps {
                nqp::bindkey($lexpad, nqp::atpos_s($type_caps, $i), $oval.WHAT);
                ++$i;
            }
        }

        # Do a coercion, if one is needed.
        if $param.coercive {
            # Coercing natives not possible - nothing to call a method on.
            if $got_native {
                nqp::bindpos($error, 0,
                  "Unable to coerce natively typed parameter '$varname'"
                ) if nqp::defined($error);
                return nqp::const::BIND_RESULT_FAIL;
            }

            my $coercion_type := $param_type.HOW.wrappee($param_type,:coercion);
            $oval := $coercion_type.HOW.coerce($coercion_type, $oval);
        }

        # If it's not got attributive binding, we'll go about binding it into
        # the lex pad.
        my int $is_attributive :=
          $flags +& nqp::const::SIG_ELEM_BIND_ATTRIBUTIVE;
        unless $is_attributive {

            # Is it native? If so, just go ahead and bind it.
            if $got_native {
                $got_native == nqp::const::SIG_ELEM_NATIVE_INT_VALUE
                  #FIXME bindkey_u missing
                  || $got_native == nqp::const::SIG_ELEM_NATIVE_UINT_VALUE
                  ?? nqp::bindkey_i($lexpad, $varname, $ival)
                  !! $got_native == nqp::const::SIG_ELEM_NATIVE_NUM_VALUE
                    ?? nqp::bindkey_n($lexpad, $varname, $nval)
                    # assume SIG_ELEM_NATIVE_STR_VALUE
                    !! nqp::bindkey_s($lexpad, $varname, $sval);
            }

            # Otherwise it's some objecty case.
            elsif $is_rw {
                if nqp::isrwcont($oval) {
                    nqp::bindkey($lexpad, $varname, $oval) if $has_varname;
                }

                # No rw container founnd, error out
                else {
                    nqp::bindpos($error, 0, {
                        Perl6::Metamodel::Configuration.throw_or_die(
                          'X::Parameter::RW',
                          "Parameter '$varname' expected a writable container, but got an " ~
                            ~ $oval.HOW.name($oval) ~ " value",
                          :got($oval),
                          :symbol($varname)
                        )
                    }) if nqp::defined($error);
                    return nqp::const::BIND_RESULT_FAIL;
                }
            }

            elsif $has_varname {
                my $bindee;

                if $flags +& nqp::const::SIG_ELEM_IS_RAW {
                    # Just bind the thing as is into the lexpad.
                    $bindee := $oval;
                }

                # Always need the deconted bindee by default
                else {
                    $bindee := nqp::decont($oval);

                    # Helper sub to store value in a copy of a Hash/Array
                    sub STORE($type, $value) {
                        my $stored := nqp::create($type);
                        $stored.STORE($value);
                        $stored
                    }

                    # If it's an array, copy means make a new one and store,
                    # and a normal bind is a straightforward binding.
                    if $flags +& nqp::const::SIG_ELEM_ARRAY_SIGIL {
                        $bindee := STORE(Array, $bindee)
                          if $flags +& nqp::const::SIG_ELEM_IS_COPY;
                    }

                    # If it's a hash, similar approach to array.
                    elsif $flags +& nqp::const::SIG_ELEM_HASH_SIGIL {
                        $bindee := STORE(Hash, $bindee)
                          if $flags +& nqp::const::SIG_ELEM_IS_COPY;
                    }

                    # If it's a scalar, we always need to wrap it into a new
                    # container and store it; the container descriptor will be
                    # provided and make it rw if it's an `is copy`.
                    else {
                        my $new_cont := nqp::create(Scalar);
                        nqp::bindattr($new_cont, Scalar, '$!descriptor',
                          nqp::getattr($param, Parameter, '$!container_descriptor')
                        );
                        nqp::bindattr($new_cont, Scalar, '$!value', $bindee);
                        $bindee := $new_cont;
                    }
                }

                # Do the actual bind
                nqp::bindkey($lexpad, $varname, $bindee);
            }
        }

        # Is it the invocant? If so, also have to bind to self lexical.
        nqp::bindkey($lexpad, 'self', nqp::decont($oval))
          if $flags +& nqp::const::SIG_ELEM_INVOCANT;

        # We have a signature constraint
        my $sigc := nqp::getattr($param, Parameter, '$!signature_constraint');
        if nqp::defined($sigc) {

            # Assume argument not passed if it is undefined and is the same
            # as parameter default type
            unless !nqp::isconcrete($oval)
              && nqp::eqaddr(
                   nqp::decont($oval),
                   nqp::getattr($param, Parameter, '$!type')
                 ) {

                my $can_signature := nqp::can($oval, 'signature');
                unless $can_signature
                  && ($sigc.is_generic
                       ?? ($sigc := $sigc.instantiate_generic($lexpad))
                       !! $sigc
                     ).ACCEPTS($oval.signature) {

                    nqp::bindpos($error, 0, {
                        Perl6::Metamodel::Configuration.throw_or_die(
                          'X::TypeCheck::Binding::Parameter',
                          "Signature check failed for parameter '$varname'",
                          :got($can_signature ?? $oval.signature !! Nil),
                          :expected($sigc),
                          :symbol($varname),
                          :parameter($param),
                          :what("Signature constraint")
                        )
                    }) if nqp::defined($error);

                    return nqp::const::BIND_RESULT_FAIL;
                }
            }
        }

        # Handle any constraint types (note that they may refer to the
        # parameter by name, so we need to have bound it already).
        my $post_cons := nqp::getattr($param, Parameter, '@!post_constraints');
        unless nqp::isnull($post_cons) {
            my int $n := nqp::elems($post_cons);
            my int $i;
            while $i < $n {
                # Check we meet the constraint.
                my $cons_type := nqp::atpos($post_cons, $i);
                $cons_type := nqp::p6capturelexwhere($cons_type.clone)
                  if nqp::istype($cons_type, Code);

                my $got := $got_native
                  ?? $got_native == nqp::const::SIG_ELEM_NATIVE_STR_VALUE
                    ?? $sval
                    !! $got_native == nqp::const::SIG_ELEM_NATIVE_NUM_VALUE
                      ?? $nval
                      !! $ival  # assume native int or uint
                  !! $oval;

                # Alas, no luck
                unless $cons_type.ACCEPTS($got) {
                    nqp::bindpos($error, 0, {
                        Perl6::Metamodel::Configuration.throw_or_die(
                          'X::TypeCheck::Binding::Parameter',
                          "Constraint type check failed for parameter '$varname'",
                          :$got,
                          :expected($cons_type),
                          :symbol($varname),
                          :parameter($param),
                          :constraint(nqp::hllboolfor(1, 'Raku'))
                        )
                    }) if nqp::defined($error);

                    return nqp::const::BIND_RESULT_FAIL;
                }

                ++$i;
            }
        }

        # If it's attributive, now we assign it.
        if $is_attributive {

            # No self?
            if nqp::not_i(nqp::existskey($lexpad, 'self')) {
                nqp::bindpos($error, 0,
                  "Unable to bind attributive parameter '$varname'; could not find self"
                ) if nqp::defined($error);

                return nqp::const::BIND_RESULT_FAIL;
            }

            # Ensure it's not native; NYI.
            elsif $got_native {
                nqp::bindpos($error, 0,
                  "Binding to natively typed attributive parameter '$varname' not supported"
                ) if nqp::defined($error);

                return nqp::const::BIND_RESULT_FAIL;
            }

            # Find self and get the attribute container
            my $self     := nqp::atkey($lexpad, 'self');
            my $assignee := $flags +& nqp::const::SIG_ELEM_BIND_PRIVATE_ATTR
              # If it's private, just need to fetch the attribute.
              ?? nqp::getattr(
                   $self,
                   nqp::getattr($param, Parameter, '$!attr_package'),
                   $varname
                 )
              # Otherwise if it's public, do a method call to get the assignee.
              !! $self."$varname"();

            nqp::iscont($assignee)
              ?? nqp::assign($assignee, $oval)
              !! $assignee.STORE(nqp::decont($oval));
        }

        # If it has a sub-signature, bind that.
        my $subsig := nqp::getattr($param, Parameter, '$!sub_signature');
        unless nqp::isnull($subsig) {

            # Turn value into a capture, unless we already have one.
            my $capture;
            if $flags +& nqp::const::SIG_ELEM_IS_CAPTURE {
                $capture := $oval;
            }
            elsif nqp::can($oval, 'Capture') {
                $capture := $oval.Capture;
            }
            else {
                nqp::bindpos(
                  $error, 0, "Could not turn argument into capture"
                ) if nqp::defined($error);

                return nqp::const::BIND_RESULT_FAIL;
            }

            # Recurse into signature binder.
            my $result := bind(
              make_vm_capture($capture),
              $subsig,
              $lexpad,
              $no_param_type_check,
              $error
            );
            unless $result == nqp::const::BIND_RESULT_OK {
                if nqp::defined($error)
                  && nqp::isstr(my $message := nqp::atpos($error, 0)) {

                    # Note in error message that we're in a sub-signature.
                    $message := $message ~ " in sub-signature";
                    $message := $message ~ " or parameter $varname"
                      if $has_varname;

                    nqp::bindpos($error, 0, $message);
                }

                return nqp::const::BIND_RESULT_FAIL;
            }
        }

        # Binding of this parameter was thus successful - we're done.
        nqp::const::BIND_RESULT_OK
    }

    # Drives the overall binding process.
    sub bind(
      $capture,
      $sig,
      $lexpad,
      int $no_param_type_check,
      $error
    ) {
        # Get params.
        my @params := nqp::getattr($sig, Signature, '@!params');

        # If we do have some named args, we get hold of a hash of them. We
        # can safely delete from this as we go.
        my $named_args;
        if nqp::capturehasnameds($capture) {
            $named_args := nqp::capturenamedshash($capture);
        }
        # Fail on arity error by default
        my int $arity_fail := 1;

        # Now we'll walk through the signature and go about binding things.
        my $named_names;
        my int $cur_pos_arg;
        my int $num_pos_args := nqp::captureposelems($capture);

        my int $num_params := nqp::elems(@params);
        my int $i;
        while $i < $num_params {
            # Get parameter object and its flags.
            my $param := nqp::atpos(@params, $i);
            my int $flags := nqp::getattr_i($param, Parameter, '$!flags');
            my str $var_name := nqp::getattr_s($param, Parameter, '$!variable_name');

            # Increment now to allow more easily determining whether this
            # is the last parameter and to more easily obtain info on the
            # next parameter.
            ++$i;

            # Is it looking for us to bind a capture here?
            my int $bind_fail;
            my int $got_prim;
            if $flags +& nqp::const::SIG_ELEM_IS_CAPTURE {

                # Capture the arguments from this point forwards into a
                # Capture.  Of course, if there's no variable name we can
                # (cheaply) do pretty much nothing.
                if nqp::isnull_s($var_name)
                  && !nqp::getattr($param, Parameter, '$!sub_signature')
                  && !nqp::getattr($param, Parameter, '@!post_constraints') {
                    $bind_fail := nqp::const::BIND_RESULT_OK;
                }

                # Has a name, so we need to do the work.
                else {
                    my $capsnap := nqp::create(Capture);

                    my @pos_args;
                    my int $k := $cur_pos_arg;
                    while $k < $num_pos_args {
                        $got_prim := nqp::captureposprimspec($capture, $k);
                        nqp::push(@pos_args, $got_prim
                          ?? $got_prim == nqp::const::BIND_VAL_STR
                            ?? nqp::box_s(
                                 nqp::captureposarg_s($capture, $k), Str
                               )
                            !! $got_prim == nqp::const::BIND_VAL_NUM
                              ?? nqp::box_n(
                                   nqp::captureposarg_n($capture, $k), Num
                                 )
                              !! nqp::box_i(  # INT | UINT
                                   $got_prim == nqp::const::BIND_VAL_UINT
                                     ?? nqp::captureposarg_u($capture, $k)
                                     !! nqp::captureposarg_i($capture, $k),
                                   Int
                                 )
                          !! nqp::captureposarg($capture, $k)
                        );
                        ++$k;
                    }

                    nqp::bindattr($capsnap, Capture, '@!list', @pos_args);
                    nqp::bindattr($capsnap, Capture, '%!hash',
                      $named_args ?? nqp::clone($named_args) !! nqp::hash
                    );

                    # Bind the parameter
                    $bind_fail := bind_one_param(
                      $sig, $lexpad, $no_param_type_check, $error,
                      $param, 0, $capsnap, 0, 0.0, '');
                }

                return $bind_fail if $bind_fail;

                # Since a capture acts as "the ultimate slurpy" in a sense, if
                # this is the last parameter in the signature we can return
                # success right off the bat.
                return nqp::const::BIND_RESULT_OK if $i == $num_params;

                # Not the last parameter, reset flag if next is slurpy
                $arity_fail := 0
                  if nqp::getattr_i(nqp::atpos(@params,$i),Parameter,'$!flags')
                  +& (nqp::const::SIG_ELEM_SLURPY_POS
                       +| nqp::const::SIG_ELEM_SLURPY_NAMED);
            }

            # Could it be a named slurpy?
            elsif $flags +& nqp::const::SIG_ELEM_SLURPY_NAMED {

                # We'll either take the current named arguments copy hash
                # which will by definition contain all unbound named arguments
                # and use that. If there are none, just keep the storage
                # uninitialized and rely on autovivification to build up an
                # empty nqp::hash whenever needed.
                my $hash := nqp::create(Hash);
                nqp::bindattr($hash, Map, '$!storage', $named_args)
                  if $named_args;
                $bind_fail := bind_one_param(
                  $sig, $lexpad, $no_param_type_check, $error,
                  $param, 0, $hash, 0, 0.0, '');

                # Done if failed
                return $bind_fail if $bind_fail;

                # Undefine named arguments hash now we've consumed it, to mark
                # all is well.
                $named_args := NQPMu;
            }

            # Otherwise, maybe it's a positional.
            elsif nqp::isnull(
              $named_names := nqp::getattr($param, Parameter, '@!named_names')
            ) {

                # Slurpy or LoL-slurpy?
                if $flags +& (nqp::const::SIG_ELEM_SLURPY_POS
                               +| nqp::const::SIG_ELEM_SLURPY_LOL
                               +| nqp::const::SIG_ELEM_SLURPY_ONEARG
                             ) {

                    # Create Raku array, create VM Array of all remaining
                    # things, then store it.
                    my $temp := nqp::list;
                    while $cur_pos_arg < $num_pos_args {
                        $got_prim :=
                          nqp::captureposprimspec($capture, $cur_pos_arg);

                        nqp::push($temp, $got_prim
                          ?? $got_prim == nqp::const::BIND_VAL_STR
                            ?? nqp::box_s(
                                 nqp::captureposarg_s($capture, $cur_pos_arg),
                                 Str
                               )
                            !! $got_prim == nqp::const::BIND_VAL_NUM
                              ?? nqp::box_n(
                                   nqp::captureposarg_n($capture, $cur_pos_arg),
                                   Num
                                 )
                              !! nqp::box_i(  # INT | UINT
                                   $got_prim == nqp::const::BIND_VAL_UINT
                                     ?? nqp::captureposarg_u(
                                          $capture, $cur_pos_arg
                                        )
                                     !! nqp::captureposarg_i(
                                          $capture, $cur_pos_arg
                                        ),
                                   Int
                                 )
                          !! nqp::captureposarg($capture, $cur_pos_arg)
                        );
                        ++$cur_pos_arg;
                    }

                    my $slurpy_type := $flags +& nqp::const::SIG_ELEM_IS_RAW
                      || $flags +& nqp::const::SIG_ELEM_IS_RW
                      ?? List
                      !! Array;

                    my $bindee := $flags +& nqp::const::SIG_ELEM_SLURPY_ONEARG
                      ?? $slurpy_type.from-slurpy-onearg($temp)
                      !! $flags +& nqp::const::SIG_ELEM_SLURPY_POS
                        ?? $slurpy_type.from-slurpy-flat($temp)
                        !! $slurpy_type.from-slurpy($temp);

                    $bind_fail := bind_one_param(
                      $sig, $lexpad, $no_param_type_check, $error,
                      $param, 0, $bindee, 0, 0.0, '');
                    return $bind_fail if $bind_fail;
                }

                # Otherwise, a positional.   Do we have a value?
                elsif $cur_pos_arg < $num_pos_args {
                    # Easy - just bind that.
                    $got_prim :=
                      nqp::captureposprimspec($capture, $cur_pos_arg);

                    $bind_fail := $got_prim
                      ?? $got_prim == nqp::const::BIND_VAL_STR
                        ?? bind_one_param(
                             $sig, $lexpad, $no_param_type_check, $error,
                             $param,
                             nqp::const::SIG_ELEM_NATIVE_STR_VALUE,
                             nqp::null,
                             0,
                             0.0,
                             nqp::captureposarg_s($capture, $cur_pos_arg)
                           )
                        !! $got_prim == nqp::const::BIND_VAL_NUM
                          ?? bind_one_param(
                               $sig, $lexpad, $no_param_type_check, $error,
                               $param,
                               nqp::const::SIG_ELEM_NATIVE_NUM_VALUE,
                               nqp::null,
                               0,
                               nqp::captureposarg_n($capture, $cur_pos_arg),
                               ''
                             )
                          !! bind_one_param(  # INT | UINT
                               $sig, $lexpad, $no_param_type_check, $error,
                               $param,
                               nqp::const::SIG_ELEM_NATIVE_INT_VALUE,
                               nqp::null,
                               $got_prim == nqp::const::BIND_VAL_UINT
                                 ?? nqp::captureposarg_u($capture,$cur_pos_arg)
                                 !! nqp::captureposarg_i($capture,$cur_pos_arg),
                               0.0,
                               ''
                             )
                      !! bind_one_param(
                           $sig, $lexpad, $no_param_type_check, $error,
                           $param,
                           0,
                           nqp::captureposarg($capture, $cur_pos_arg),
                           0,
                           0.0,
                           ''
                         );

                    return $bind_fail if $bind_fail;
                    ++$cur_pos_arg;
                }

                # No value. If it's optional, fetch a default and bind that;
                # Note that we never nominal type check an optional with no
                # value passed.
                elsif $flags +& nqp::const::SIG_ELEM_IS_OPTIONAL {
                    $bind_fail := bind_one_param(
                      $sig, $lexpad, $no_param_type_check, $error,
                      $param,
                      0,
                      handle_optional($param, $flags, $lexpad),
                      0,
                      0.0,
                      ''
                    );

                    return $bind_fail if $bind_fail;
                }

                # Optional and no default
                else {
                    nqp::bindpos($error, 0, arity_fail(
                      @params, $num_params, $num_pos_args, 0, $lexpad
                    )) if nqp::defined($error);

                    return nqp::const::BIND_RESULT_FAIL;
                }
            }

            # Else, it's a non-slurpy named.
            else {
                # Try and get hold of value.
                my $value := nqp::null;
                if $named_args {
                    my str $cur_name;

                    my int $num_names := nqp::elems($named_names);
                    my int $j;
                    while $j < $num_names {
                        $cur_name := nqp::atpos_s($named_names, $j);
                        $value    := nqp::atkey($named_args, $cur_name);

                        if nqp::isnull($value) {
                            ++$j;
                        }
                        else {
                            nqp::deletekey($named_args, $cur_name);
                            $j := $num_names;
                        }

                    }
                }

                # Did we get one?
                if nqp::isnull($value) {

                    # Nope. We'd better hope this param was optional...
                    if $flags +& nqp::const::SIG_ELEM_IS_OPTIONAL {
                        $bind_fail := bind_one_param(
                          $sig, $lexpad, $no_param_type_check, $error,
                          $param,
                          0,
                          handle_optional($param, $flags, $lexpad),
                          0,
                          0.0,
                          ''
                        );
                    }
                    elsif $arity_fail {
                        nqp::bindpos($error, 0, "Required named argument '"
                          ~ nqp::atpos_s($named_names,0)
                          ~ "' not passed"
                        ) if nqp::defined($error);

                        return nqp::const::BIND_RESULT_FAIL;
                    }
                }

                # Found a value
                else {
                    $bind_fail := bind_one_param(
                      $sig, $lexpad, $no_param_type_check, $error,
                      $param,
                      0,
                      $value,
                      0,
                      0.0,
                      ''
                    );
                }

                # If we got a binding failure, return it.
                return $bind_fail if $bind_fail;
            }
        }

        # Do we have any left-over args?
        if $cur_pos_arg < $num_pos_args && $arity_fail {

            # Oh noes, too many positionals passed.
            nqp::bindpos($error, 0, arity_fail(
              @params, $num_params, $num_pos_args, 1, $lexpad
            )) if nqp::defined($error);

            nqp::const::BIND_RESULT_FAIL
        }

        # Oh noes, unexpected named args.
        elsif $named_args {

            if nqp::defined($error) {
                my int $num_extra := nqp::elems($named_args);
                my @names;
                for $named_args {
                    nqp::push(@names, $_.key);
                }

                nqp::bindpos($error, 0, $num_extra == 1
                  ?? "Unexpected named argument '"
                       ~ nqp::atpos(@names, 0)
                       ~ "' passed"
                  !! "$num_extra  unexpected named arguments passed ("
                       ~ nqp::join(",", @names)
                       ~")"
                );
            }
            nqp::const::BIND_RESULT_FAIL
        }

        # If we get here, we're done.
        else {
            nqp::const::BIND_RESULT_OK
        }
    }

    method bind($capture, $sig, $lexpad, int $no_param_type_check, $error) {
        bind($capture, $sig, $lexpad, $no_param_type_check, $error);
    }

    method try_bind_sig($capture) {
        # Call binder, and return non-zero if the bind is successful.
        nqp::not_i(
          bind(
            $capture,
            nqp::getattr(nqp::getcodeobj(nqp::callercode), Code, '$!signature'),
            nqp::ctxcaller(nqp::ctx),
            0,
            NQPMu
          )
        )
    }

    method bind_sig($capture) {
        # Get signature and lexpad.
        my $caller := nqp::getcodeobj(nqp::callercode);
        my $sig    := nqp::getattr($caller, Code, '$!signature');
        my $lexpad := nqp::ctxcaller(nqp::ctx);

        # Call binder.
        my @error;
        my int $bind_res := bind($capture, $sig, $lexpad, 0, @error);
        if $bind_res {
            if $bind_res == nqp::const::BIND_RESULT_JUNCTION {
                my @pos_args;

                my int $num_pos_args := nqp::captureposelems($capture);
                my int $k;
                while $k < $num_pos_args {
                    my $got_prim := nqp::captureposprimspec($capture, $k);
                    nqp::push(@pos_args, $got_prim
                      ?? $got_prim == 3
                        ?? nqp::box_s(nqp::captureposarg_s($capture, $k), Str)
                        !! $got_prim == 1
                          ?? nqp::box_i(nqp::captureposarg_i($capture, $k), Int)
                          !! $got_prim == 2
                            ?? nqp::box_n(
                                 nqp::captureposarg_n($capture, $k), Num)
                            !! nqp::box_u(  # assume 10
                                 nqp::captureposarg_u($capture, $k), Int)
                      !! nqp::captureposarg($capture, $k)
                    );

                    ++$k;
                }

                Junction.AUTOTHREAD(
                  $caller,
                  |@pos_args,
                  |nqp::capturenamedshash($capture)
                );
            }
            else {
                my $error := nqp::atpos(@error, 0);
                nqp::isinvokable($error) ?? $error() !! nqp::die($error);
            }
        }
        else {
            nqp::null;
        }
    }

    sub make_vm_capture($capture) {
        sub vm_capture(*@pos, *%named) { nqp::savecapture }

        my @list := nqp::getattr($capture, Capture, '@!list');
        @list    := nqp::list unless nqp::islist(@list);
        my %hash := nqp::getattr($capture, Capture, '%!hash');
        %hash    := nqp::hash unless nqp::ishash(%hash);
        vm_capture(|@list, |%hash)
    }

    method is_bindable($sig, $capture) {
        $capture := make_vm_capture($capture)
          unless nqp::reprname($capture) eq 'MVMCapture';

        nqp::p6invokeunder(
          nqp::getattr(nqp::getattr($sig, Signature, '$!code'), Code, '$!do'),
          -> {
              bind(
                $capture, $sig, nqp::ctxcaller(nqp::ctx), 0, NQPMu
              ) != nqp::const::BIND_RESULT_FAIL
             }
        )
    }

    method bind_cap_to_sig($sig, $capture) {
        if bind(
             make_vm_capture($capture),
             $sig,
             nqp::ctxcaller(nqp::ctx),
             0,
             my @error
        ) {
            my $error := nqp::atpos(@error, 0);
            nqp::isinvokable($error) ?? $error() !! nqp::die($error);
        }
        $sig
    }

    method get_return_type($code) {
        my $type := nqp::getattr(
          nqp::getattr($code, Code, '$!signature'), Signature, '$!returns'
        );
        nqp::eqaddr($type,Mu) || nqp::eqaddr($type,NQPMu)
          ?? nqp::null
          !! $type
    }
#?endif

    my int $TRIAL_BIND_NOT_SURE :=  0;   # Plausible, need to check at runtime.
    my int $TRIAL_BIND_OK       :=  1;   # Bind will always work out.
    my int $TRIAL_BIND_NO_WAY   := -1;   # Bind could never work out.

    method trial_bind($sig, $args, $sigflags) {
        my @params         := nqp::getattr($sig, Signature, '@!params');
        my int $num_params := nqp::elems(@params);

        # If there's a single capture parameter, then we're OK. (Worth
        # handling especially as it's the common case for protos).
        return $TRIAL_BIND_OK
          if $num_params == 1
          && nqp::getattr_i(
               nqp::atpos(@params, 0), Parameter, '$!flags'
             ) +& nqp::const::SIG_ELEM_IS_CAPTURE
          && nqp::isnull(nqp::getattr(
               nqp::atpos(@params, 0), Parameter, '@!post_constraints'
             ));

        # Walk through the signature and consider the parameters.
        my int $cur_pos_arg;

        my int $num_pos_args := nqp::elems($args);
        my int $i;
        while $i < $num_params {
            my $param := nqp::atpos(@params, $i);

            # If the parameter is anything other than a boring old
            # positional parameter, we won't analyze it and will bail out,
            # unless it's a slurpy named param, in which case just ignore it
            my int $flags := nqp::getattr_i($param, Parameter, '$!flags');
            if $flags +& nqp::const::SIG_ELEM_SLURPY_NAMED
              && nqp::isnull(
                   nqp::getattr($param, Parameter, '@!post_constraints')
                 ) {
                  ++$i;
                  next
            }

            return $TRIAL_BIND_NOT_SURE
              if $flags +& nqp::bitneg_i(
                   nqp::const::SIG_ELEM_MULTI_INVOCANT
                +| nqp::const::SIG_ELEM_IS_RAW
                +| nqp::const::SIG_ELEM_IS_COPY
                +| nqp::const::SIG_ELEM_ARRAY_SIGIL
                +| nqp::const::SIG_ELEM_HASH_SIGIL
                +| nqp::const::SIG_ELEM_NATIVE_VALUE
                +| nqp::const::SIG_ELEM_IS_OPTIONAL
              )
              || $flags +& nqp::const::SIG_ELEM_IS_RW;

            return $TRIAL_BIND_NOT_SURE
              unless nqp::isnull(
                   nqp::getattr($param, Parameter, '@!named_names')
                 )
              || nqp::isnull(
                   nqp::getattr($param, Parameter, '@!post_constraints')
                 )
              || nqp::isnull(
                   nqp::getattr($param, Parameter, '@!type_captures')
                 );

            return $TRIAL_BIND_NOT_SURE
              if $param.coercive;

            # Do we have an argument for this parameter?
            if $cur_pos_arg >= $num_pos_args {

                # No; if it's not optional, fail.
                return $TRIAL_BIND_NO_WAY
                  unless $flags +& nqp::const::SIG_ELEM_IS_OPTIONAL;
            }

            # An argument for this position
            else {
                # Yes, need to consider type
                my int $got_prim := nqp::atpos($sigflags, $cur_pos_arg) +& 0xF;

                # Parameter is a native
                if $flags +& nqp::const::SIG_ELEM_NATIVE_VALUE {

                    # Got a native
                    if $got_prim {

                        # If it's the wrong type of native, there's no way it
                        # can ever bind.
                        return $TRIAL_BIND_NO_WAY
                          if (($flags +& nqp::const::SIG_ELEM_NATIVE_STR_VALUE)
                               && $got_prim != nqp::const::BIND_VAL_STR)
                          || (($flags +& nqp::const::SIG_ELEM_NATIVE_INT_VALUE)
                               && $got_prim != nqp::const::BIND_VAL_INT)
                          || (($flags +& nqp::const::SIG_ELEM_NATIVE_UINT_VALUE)
                               && $got_prim != nqp::const::BIND_VAL_UINT
                               && $got_prim != nqp::const::BIND_VAL_INT)
                          || (($flags +& nqp::const::SIG_ELEM_NATIVE_NUM_VALUE)
                               && $got_prim != nqp::const::BIND_VAL_NUM);
                    }

                    # We got an object; if we aren't sure we can unbox,
                    # we can't be sure about the dispatch.
                    elsif $flags +& nqp::const::SIG_ELEM_NATIVE_STR_VALUE {
                        return $TRIAL_BIND_NOT_SURE
                          unless nqp::isstr(nqp::atpos($args, $cur_pos_arg));
                    }
                    elsif $flags +& (
                      nqp::const::SIG_ELEM_NATIVE_INT_VALUE
                      +| nqp::const::SIG_ELEM_NATIVE_UINT_VALUE) {
                        return $TRIAL_BIND_NOT_SURE
                          unless nqp::isint(nqp::atpos($args, $cur_pos_arg));
                    }
                    elsif $flags +& nqp::const::SIG_ELEM_NATIVE_NUM_VALUE {
                        return $TRIAL_BIND_NOT_SURE
                          unless nqp::isnum(nqp::atpos($args, $cur_pos_arg));
                    }
                    else {
                        # WTF...
                        return $TRIAL_BIND_NOT_SURE;
                    }
                }

                # Parameter is an object
                else {
                    # Work out a parameter type to consider, and see if it
                    # matches.
                    my $arg := $got_prim
                      ?? $got_prim == nqp::const::BIND_VAL_STR
                        ?? Str
                        !! $got_prim == nqp::const::BIND_VAL_INT
                          || $got_prim == nqp::const::BIND_VAL_UINT
                          ?? Int
                          !! Num  # assume $got_prim == BIND_VAL_NUM
                      !! nqp::atpos($args, $cur_pos_arg);

                    my $param_type := nqp::getattr($param, Parameter, '$!type');
                    unless nqp::eqaddr($param_type, Mu)
                      || nqp::istype($arg, $param_type) {

                        # If it failed because we got a junction, may
                        # auto-thread; hand back 'not sure' for now.
                        return $TRIAL_BIND_NOT_SURE
                          if nqp::eqaddr($arg.WHAT, Junction);

                        # It failed to, but that doesn't mean it can't work
                        # at runtime; we perhaps want an Int, and the most
                        # we know is we have an Any, which would include Int.
                        # However, the Int ~~ Str case can be rejected now,
                        # as there's no way it'd ever match. Basically, we
                        # just flip the type check around.
                        return nqp::istype($param_type, $arg.WHAT)
                          ?? $TRIAL_BIND_NOT_SURE
                          !! $TRIAL_BIND_NO_WAY;
                    }
                }
            }

            # Continue to next argument.
            ++$cur_pos_arg;
            ++$i;
        }

        $cur_pos_arg < $num_pos_args
          # If we have any left over arguments, it's a binding fail.
          ?? $TRIAL_BIND_NO_WAY
          # Otherwise, if we get there, all is well.
          !! $TRIAL_BIND_OK
    }
}

BEGIN { nqp::p6setbinder(Binder); } # We need it in for the next BEGIN block
nqp::p6setbinder(Binder);           # The load-time case.

# Container descriptors come here so that they can refer to Raku types.

#- ContainerDescriptor ---------------------------------------------------------
class ContainerDescriptor does Perl6::Metamodel::Explaining {
    has     $!of;
    has str $!name;
    has     $!default;
    has int $!dynamic;

    method new(:$of, str :$name, :$default, int :$dynamic) {
        my $obj := nqp::create(self);
        nqp::bindattr(  $obj, ContainerDescriptor, '$!of',      $of     );
        nqp::bindattr_s($obj, ContainerDescriptor, '$!name',    $name   );
        nqp::bindattr(  $obj, ContainerDescriptor, '$!default', $default);
        nqp::bindattr_i($obj, ContainerDescriptor, '$!dynamic', $dynamic);
        $obj
    }

    method of()      { $!of      }
    method name()    { $!name    }
    method default() { $!default }
    method dynamic() { $!dynamic }

    method set_of     ($of     ) { $!of      := $of;      self }
    method set_default($default) { $!default := $default; self }
    method set_dynamic($dynamic) { $!dynamic := $dynamic; self }

    method is_generic() {
        $!of.HOW.archetypes($!of).generic || self.is_default_generic
    }

    method is_default_generic() {
        $!default.HOW.archetypes($!default).generic
    }

    method instantiate_generic($type_environment) {
        my $obj := nqp::clone(self);

        nqp::bindattr($obj, $?CLASS, '$!of', $!of.HOW.archetypes($!of).generic
          ?? $!of.HOW.instantiate_generic($!of, $type_environment)
          !! $!of
        );
        nqp::bindattr($obj, $?CLASS, '$!default', self.is_default_generic
          ?? $!default.HOW.instantiate_generic($!default, $type_environment)
          !! $!default
        );

        $obj
    }
}

#- ContainerDescriptor::Untyped ------------------------------------------------
# Container descriptor for when the type is Mu; the type of this container
# descriptor is used as a marker
class ContainerDescriptor::Untyped is ContainerDescriptor { }

#- ContainerDescriptor::Whence -------------------------------------------------
# Role for container descriptors that need to bind the container to some place
# (hence the "whence") on first assignment.  Most commonly used for elements
# in arrays and keys in hashes.
role ContainerDescriptor::Whence {
    has $!next-descriptor;

    method next() {
        my $next := $!next-descriptor;
        nqp::isconcrete($next)
          ?? $next
          !! ($!next-descriptor := nqp::gethllsym('Raku', 'default_cont_spec'))
    }
    method of()      { self.next.of      }
    method default() { self.next.default }
    method dynamic() { self.next.dynamic }
}

#- ContainerDescriptor::BindArrayPos -------------------------------------------
# Container descriptor that will bind to a one-dimensional array on first
# assignment
class ContainerDescriptor::BindArrayPos does ContainerDescriptor::Whence {
    has     $!target;
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

#- ContainerDescriptor::BindArrayPos2D -----------------------------------------
# Container descriptor that will bind to a two-dimensional array on first
# assignment
class ContainerDescriptor::BindArrayPos2D does ContainerDescriptor::Whence {
    has     $!target;
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

#- ContainerDescriptor::BindArrayPos3D -----------------------------------------
# Container descriptor that will bind to a three-dimensional array on first
# assignment
class ContainerDescriptor::BindArrayPos3D does ContainerDescriptor::Whence {
    has     $!target;
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

#- ContainerDescriptor::BindArrayPosND -----------------------------------------
# Container descriptor that will bind to a N-dimensional array on first
# assignment
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

#- ContainerDescriptor::BindHashKey --------------------------------------------
# Container descriptor that will bind to a key in a hash on first
# assignment
class ContainerDescriptor::BindHashKey does ContainerDescriptor::Whence {
    has $!target;
    has $!key;

    method new($desc, $target, $key) {
        my $self := nqp::create(self);
        nqp::bindattr($self, ContainerDescriptor::BindHashKey,
            '$!next-descriptor', $desc);
        nqp::bindattr($self, ContainerDescriptor::BindHashKey,
            '$!target', $target);
        nqp::bindattr($self, ContainerDescriptor::BindHashKey,
            '$!key', $key);
        $self
    }

    method name() { self.next.name ~ "\{'" ~ $!key ~ "'\}" }
    method assigned($scalar) {
        my $hash := nqp::getattr($!target, Map, '$!storage');
        $hash := nqp::bindattr($!target, Map, '$!storage', nqp::hash)
          unless nqp::isconcrete($hash);
        nqp::bindkey($hash, $!key, $scalar);
    }
}

#- ContainerDescriptor::BindObjHashKey -----------------------------------------
# Container descriptor that will bind to a key in an object hash on first
# assignment
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
        $hash := nqp::bindattr($!target, Map, '$!storage', nqp::hash)
          unless nqp::isconcrete($hash);
        nqp::bindkey($hash, $!which, $!pair.new($!key, $scalar));
    }
}

#- ContainerDescriptor::VivifyArray --------------------------------------------
# Container descriptor that will bind to position in an array to be vivified
# on first assignment
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

        (nqp::isconcrete($target)
          ?? $target
          !! nqp::assign($target, Array.new)
        ).BIND-POS($!pos, $scalar)
    }
}

#- ContainerDescriptor::VivifyHash ---------------------------------------------
# Container descriptor that will bind to position in an array to be vivified
# on first assignment
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

        (nqp::isconcrete($target)
          ?? $target
          !! nqp::assign($target, Hash.new)
        ).BIND-KEY($!key, $scalar)
    }
}

#- ContainerDescriptor::UninitializedAttribute ---------------------------------
# Attributes that are either required or have a default need us to detect if
# they have been initialized. We do this by starting them out with a descriptor
# that indicates they are uninitialized, and then swapping it out for a the
# underlying one upon assignment.
class ContainerDescriptor::UninitializedAttribute {
    has $!next-descriptor;

    method new($next) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, ContainerDescriptor::UninitializedAttribute,
            '$!next-descriptor', $next);
        $obj
    }

    method next() {
        my $next := $!next-descriptor;
        nqp::isconcrete($next)
          ?? $next
          !! ($!next-descriptor := nqp::gethllsym('Raku', 'default_cont_spec'))
    }

    method of()      { self.next.of      }
    method name()    { self.next.name    }
    method default() { self.next.default }
    method dynamic() { self.next.dynamic }

    method instantiate_generic($type_environment) {
        self.new(self.next.instantiate_generic($type_environment))
    }
    method is_generic()         { self.next.is_generic         }
    method is_default_generic() { self.next.is_default_generic }
}

#?if !moar
#- UninitializedAttributeChecker -----------------------------------------------
# On MoarVM we have a dispatcher for checking if an attribute is not
# initialized, this is the portable fallback for other VMs.
my class UninitializedAttributeChecker {
    method check($attr) {
        # If there's a non-concrete object observed, then we bound a non-container
        # in place, so trivially initialized.
        if !nqp::isconcrete_nd($attr) {
            1
        }

        # Otherwise, might be a container that was assigned. Look for the
        # descriptor.
        else {
            my $desc;
            if nqp::istype_nd($attr, Scalar) {
                $desc := nqp::getattr($attr, Scalar, '$!descriptor');
            }
            elsif nqp::istype_nd($attr, Array) {
                my $storage := nqp::getattr($attr, List, '$!reified');
                unless nqp::isconcrete($storage) && nqp::elems($storage) {
                    $desc := nqp::getattr($attr, Array, '$!descriptor');
                }
            }
            elsif nqp::istype_nd($attr, Hash) {
                my $storage := nqp::getattr($attr, Map, '$!storage');
                unless nqp::isconcrete($storage) && nqp::elems($storage) {
                    $desc := nqp::getattr($attr, Hash, '$!descriptor');
                }
            }
            else {
                try {
                    my $base := nqp::how_nd($attr).mixin_base($attr);
                    $desc := nqp::getattr($attr, $base, '$!descriptor');
                }
            }
            !nqp::eqaddr($desc.WHAT, ContainerDescriptor::UninitializedAttribute);
        }
    }
}
nqp::bindhllsym('Raku', 'UninitializedAttributeChecker', UninitializedAttributeChecker);
#?endif

# We stick all the declarative bits inside of a BEGIN, so they get
# serialized.
BEGIN {
    # Ensure Rakudo runtime support is initialized.
    nqp::p6init();

#?if moar
    # On MoarVM, to get us through the bootstrap, put the NQP dispatchers in
    # place as the Raku ones; they will get replaced later in the bootstrap.
    nqp::sethllconfig('Raku', nqp::hash(
      'call_dispatcher',        'nqp-call',
      'method_call_dispatcher', 'nqp-meth-call',
      'find_method_dispatcher', 'nqp-find-meth',
    ));
#?endif

#- Mu --------------------------------------------------------------------------
# class Mu {
    Mu.HOW.compose_repr(Mu);

#- Any -------------------------------------------------------------------------
# class Any is Mu {
    Any.HOW.add_parent(Any, Mu);
    Any.HOW.compose_repr(Any);

#- Cool ------------------------------------------------------------------------
# class Cool is Any {
    Cool.HOW.add_parent(Cool, Any);
    Cool.HOW.compose_repr(Cool);

#- Attribute -------------------------------------------------------------------
# class Attribute is Any {
#     has str $!name;
#     has int $!rw;
#     has int $!ro;
#     has Mu  $!required;
#     has int $!is_built;
#     has int $!is_bound;
#     has int $!has_accessor;
#     has Mu  $!type;
#     has Mu  $!container_descriptor;
#     has Mu  $!auto_viv_container;
#     has Mu  $!build_closure;
#     has Mu  $!package;
#     has int $!inlined;
#     has Mu  $!dimensions;
#     has int $!box_target;
#     has int $!positional_delegate;
#     has int $!associative_delegate;
#     has Mu  $!why;
#     has Mu  $!container_initializer;
#     # original attribute object used for instantiation
#     has Attribute $!original;
#     has int $!composed;

    Attribute.HOW.add_parent(Attribute, Any);

    Attribute.HOW.add_attribute(Attribute,
      BOOTSTRAPATTR.new(:name<$!name>, :type(str), :package(Attribute))
    );

    # The existence of both $!rw and $!ro might be confusing, but they're
    # needed for late trait application with `also is rw`. In this case we
    # must remember the earlier applied per-attribute traits.
    Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(
      :name<$!rw>, :type(int), :package(Attribute)
    ));
    Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(
      :name<$!ro>, :type(int), :package(Attribute)
    ));
    Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(
      :name<$!required>, :type(Mu), :package(Attribute)
    ));
    Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(
      :name<$!is_built>, :type(int), :package(Attribute)
    ));
    Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(
      :name<$!is_bound>, :type(int), :package(Attribute)
    ));
    Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(
      :name<$!has_accessor>, :type(int), :package(Attribute)
    ));
    Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(
      :name<$!type>, :type(Mu), :package(Attribute)
    ));
    Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(
      :name<$!container_descriptor>, :type(Mu), :package(Attribute)
    ));
    Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(
      :name<$!auto_viv_container>, :type(Mu), :package(Attribute)
    ));
    Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(
      :name<$!build_closure>, :type(Mu), :package(Attribute)
    ));
    Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(
      :name<$!package>, :type(Mu), :package(Attribute)
    ));
    Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(
      :name<$!inlined>, :type(int), :package(Attribute)
    ));
    Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(
      :name<$!dimensions>, :type(Mu), :package(Attribute)
    ));
    Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(
      :name<$!box_target>, :type(int), :package(Attribute)
    ));
    Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(
      :name<$!positional_delegate>, :type(int), :package(Attribute)
    ));
    Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(
      :name<$!associative_delegate>, :type(int), :package(Attribute)
    ));
    Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(
      :name<$!why>, :type(Mu), :package(Attribute)
    ));
    Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(
      :name<$!container_initializer>, :type(Mu), :package(Attribute)
    ));
    Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(
      :name<$!original>, :type(Attribute), :package(Attribute)
    ));
    Attribute.HOW.add_attribute(Attribute, BOOTSTRAPATTR.new(
      :name<$!composed>, :type(int), :package(Attribute)
    ));

    # Need new and accessor methods for Attribute in here for now.
    Attribute.HOW.add_method(Attribute, 'new',
      nqp::getstaticcode(sub ($self, :$name!, :$type!, :$package!, *%_) {
        my $attr := nqp::create($self);
        $type    := nqp::decont($type);

        nqp::bindattr_s($attr, Attribute, '$!name', $name);

        nqp::bindattr($attr, Attribute, '$!type',     $type);
        nqp::bindattr($attr, Attribute, '$!package',  nqp::decont($package));
        nqp::bindattr($attr, Attribute, '$!original', $attr);

        nqp::bindattr_i($attr, Attribute, '$!has_accessor',
          my int $has_accessor := nqp::ifnull(nqp::atkey(%_,'has_accessor'),0));
        nqp::bindattr_i($attr, Attribute, '$!is_built',
          nqp::ifnull(nqp::atkey(%_, 'is_bound'), $has_accessor));

        nqp::bindattr_i($attr, Attribute, '$!is_bound',
          nqp::ifnull(nqp::atkey(%_, 'is_bound'), 0));
        nqp::bindattr_i($attr, Attribute, '$!inlined',
          nqp::ifnull(nqp::atkey(%_, 'inlined'), 0));
        nqp::bindattr_i($attr, Attribute, '$!positional_delegate',
          nqp::ifnull(nqp::atkey(%_, 'positional_delegate'), 0));
        nqp::bindattr_i($attr, Attribute, '$!associative_delegate',
          nqp::ifnull(nqp::atkey(%_, 'associative_delegate'), 0));

        if nqp::existskey(%_, 'auto_viv_primitive') {
            nqp::bindattr($attr, Attribute, '$!auto_viv_container',
              nqp::atkey(%_, 'auto_viv_primitive')
            );
        }

        elsif nqp::existskey(%_, 'container_descriptor') {
            nqp::bindattr($attr, Attribute, '$!container_descriptor',
              nqp::atkey(%_, 'container_descriptor'));

            nqp::bindattr($attr, Attribute, '$!auto_viv_container',
              nqp::atkey(%_, 'auto_viv_container')
            ) if nqp::existskey(%_, 'auto_viv_container');
        }
        else {
            my $cd     := ContainerDescriptor.new(:of($type), :$name);
            my $scalar := nqp::create(Scalar);
            nqp::bindattr($scalar, Scalar, '$!descriptor', $cd);
            nqp::bindattr($scalar, Scalar, '$!value',      $type);
            nqp::bindattr($attr, Attribute, '$!container_descriptor', $cd);
            nqp::bindattr($attr, Attribute, '$!auto_viv_container',   $scalar);
        }

        nqp::bindattr($attr, Attribute, '$!container_initializer',
          nqp::atkey(%_, 'container_initializer')
        ) if nqp::existskey(%_, 'container_initializer');

        nqp::existskey(%_, 'build')
          ?? $attr.set_build(nqp::atkey(%_, 'build'))
          !! $attr
    }));

    Attribute.HOW.add_method(Attribute, 'name',
      nqp::getstaticcode(sub ($self) {
        $self := nqp::decont($self);

        nqp::getattr_s($self, Attribute, '$!name')
    }));

    Attribute.HOW.add_method(Attribute, 'type',
      nqp::getstaticcode(sub ($self) {
        $self := nqp::decont($self);

        nqp::getattr($self, Attribute, '$!type')
    }));

    Attribute.HOW.add_method(Attribute, 'container_descriptor',
      nqp::getstaticcode(sub ($self) {
        $self := nqp::decont($self);

        nqp::getattr($self, Attribute, '$!container_descriptor')
    }));

    Attribute.HOW.add_method(Attribute, 'auto_viv_container',
      nqp::getstaticcode(sub ($self) {
        $self := nqp::decont($self);

        my $cont := nqp::getattr($self, Attribute, '$!auto_viv_container');
        if nqp::isconcrete_nd($cont)
          && (nqp::getattr($self, Attribute, '$!required')
               || nqp::isconcrete(
                    nqp::getattr($self, Attribute, '$!build_closure')
                  )
             ) {

            try {
                my $base := nqp::how_nd($cont).mixin_base($cont);
                $cont    := nqp::clone_nd($cont);
                nqp::bindattr(
                  $cont,
                  $base,
                  '$!descriptor',
                  ContainerDescriptor::UninitializedAttribute.new(
                    nqp::getattr($cont, $base, '$!descriptor')
                  )
                );
            }
        }

        $cont
    }));

    Attribute.HOW.add_method(Attribute, 'is_built',
      nqp::getstaticcode(sub ($self) {
        $self := nqp::decont($self);

        nqp::hllboolfor(
          nqp::getattr_i($self, Attribute, '$!is_built'),
          "Raku"
        )
    }));

    Attribute.HOW.add_method(Attribute, 'is_bound',
      nqp::getstaticcode(sub ($self) {
        $self := nqp::decont($self);

        nqp::hllboolfor(
          nqp::getattr_i($self, Attribute, '$!is_bound'),
          "Raku"
        )
    }));

    Attribute.HOW.add_method(Attribute, 'has_accessor',
      nqp::getstaticcode(sub ($self) {
        $self := nqp::decont($self);

        nqp::hllboolfor(
          nqp::getattr_i($self, Attribute, '$!has_accessor'),
          "Raku"
        )
    }));

    Attribute.HOW.add_method(Attribute, 'rw',
      nqp::getstaticcode(sub ($self) {
        $self := nqp::decont($self);

        nqp::hllboolfor(
          nqp::getattr_i($self, Attribute, '$!rw'),
          "Raku"
        );
    }));

    Attribute.HOW.add_method(Attribute, 'set_rw',
      nqp::getstaticcode(sub ($self) {
        $self := nqp::decont($self);

        nqp::bindattr_i($self, Attribute, '$!rw', 1);
        nqp::hllboolfor(1, "Raku")
    }));

    Attribute.HOW.add_method(Attribute, 'set_readonly',
      nqp::getstaticcode(sub ($self) {
        $self := nqp::decont($self);

        # Explicit set of readonly must reset rw as it might be a result
        # of `is rw` trait.
        nqp::bindattr_i($self, Attribute, '$!rw', 0);

        nqp::bindattr_i($self, Attribute, '$!ro', 1);
        nqp::hllboolfor(1, "Raku")
    }));

    Attribute.HOW.add_method(Attribute, 'set_required',
      nqp::getstaticcode(sub ($self, $value) {
        $self := nqp::decont($self);

        # The value can also be a string that will be shown in the error
        # message if this named argument is *not* specified.  Make sure it
        # is added to the serialization context.
        $*W.add_object_if_no_sc($value);  # XXX RakuAST
        nqp::bindattr($self, Attribute, '$!required', $value);
        nqp::hllboolfor(1, "Raku")
    }));

    Attribute.HOW.add_method(Attribute, 'required',
      nqp::getstaticcode(sub ($self) {
        $self := nqp::decont($self);

        nqp::getattr($self, Attribute, '$!required')
    }));

    Attribute.HOW.add_method(Attribute, 'default_to_rw',
      nqp::getstaticcode(sub ($self) {
        $self := nqp::decont($self);

        nqp::bindattr_i($self, Attribute, '$!rw', 1)
          unless nqp::getattr_i($self, Attribute, '$!ro');

        nqp::hllboolfor(1, "Raku")
    }));

    Attribute.HOW.add_method(Attribute, 'set_build',
      nqp::getstaticcode(sub ($self, $closure) {
        $self := nqp::decont($self);

        nqp::bindattr($self, Attribute, '$!build_closure', $closure);
        $self
    }));

    Attribute.HOW.add_method(Attribute, 'build',
      nqp::getstaticcode(sub ($self) {
        $self := nqp::decont($self);

        nqp::getattr($self, Attribute, '$!build_closure');
    }));

    Attribute.HOW.add_method(Attribute, 'set_box_target',
      nqp::getstaticcode(sub ($self) {
        $self := nqp::decont($self);

        nqp::bindattr_i($self, Attribute, '$!box_target', 1);
        nqp::hllboolfor(1, "Raku")
    }));

    Attribute.HOW.add_method(Attribute, 'box_target',
      nqp::getstaticcode(sub ($self) {
        $self := nqp::decont($self);

        nqp::getattr_i($self, Attribute, '$!box_target')
    }));

    Attribute.HOW.add_method(Attribute, 'positional_delegate',
      nqp::getstaticcode(sub ($self) {
        $self := nqp::decont($self);

        nqp::getattr_i($self, Attribute, '$!positional_delegate');
    }));

    Attribute.HOW.add_method(Attribute, 'associative_delegate',
      nqp::getstaticcode(sub ($self) {
        $self := nqp::decont($self);

        nqp::getattr_i($self, Attribute, '$!associative_delegate')
    }));

    Attribute.HOW.add_method(Attribute, 'container_initializer',
      nqp::getstaticcode(sub ($self) {
        $self := nqp::decont($self);

        nqp::getattr($self, Attribute, '$!container_initializer')
    }));

    Attribute.HOW.add_method(Attribute, 'original',
      nqp::getstaticcode(sub ($self) {
        $self := nqp::decont($self);

        nqp::getattr($self, Attribute, '$!original')
    }));

    Attribute.HOW.add_method(Attribute, 'is_generic',
      nqp::getstaticcode(sub ($self) {
        $self := nqp::decont($self);

        my $type    := nqp::getattr($self, Attribute, '$!type'         );
        my $package := nqp::getattr($self, Attribute, '$!package'      );
        my $build   := nqp::getattr($self, Attribute, '$!build_closure');

        nqp::hllboolfor(
          $type.HOW.archetypes($type).generic
            || $package.HOW.archetypes($package).generic
            || nqp::defined($build),
          "Raku"
        );
    }));

    Attribute.HOW.add_method(Attribute, 'instantiate_generic',
      nqp::getstaticcode(sub ($self, $type_environment) {
        $self    := nqp::decont($self);

        my $ins  := nqp::clone($self);
        my $type := nqp::getattr($self, Attribute, '$!type');
        my $cd   := nqp::getattr($self, Attribute, '$!container_descriptor');
        my $pkg  := nqp::getattr($self, Attribute, '$!package');
        my $avc  := nqp::getattr($self, Attribute, '$!auto_viv_container');
        my $bc   := nqp::getattr($self, Attribute, '$!build_closure');
        my $ci   := nqp::getattr($self, Attribute, '$!container_initializer');

        nqp::bindattr($ins, Attribute, '$!type',
          $type.HOW.instantiate_generic($type, $type_environment)
        ) if $type.HOW.archetypes($type).generic;

        nqp::bindattr($ins, Attribute, '$!container_initializer',
#?if !jvm
          nqp::p6capturelexwhere($ci.clone)
#?endif
#?if jvm
          $ci.clone
#?endif
        ) if nqp::isconcrete($ci);

        my $cd_ins := $cd;
        if $cd.is_generic {
            $cd_ins := $cd.instantiate_generic($type_environment);
            nqp::bindattr($ins, Attribute, '$!container_descriptor', $cd_ins);
        }

        my $avc-copy;
        my $avc-is-generic;
        if nqp::iscont($avc) {
            # If $avc is a container then simulate nqp::p6var (.VAR) behavior
            my $avcv := nqp::create(ScalarVAR);
            nqp::bindattr($avcv, Scalar, '$!value', nqp::clone_nd($avc));
            $avc-is-generic := $avcv.is_generic;

            $avc-copy := $avc-is-generic
              ?? $avcv.instantiate_generic($type_environment)
              !! $avc;
        }
        else {
            $avc-is-generic := $avc.HOW.archetypes($avc).generic
              || $avc.is-generic
              || $type.HOW.archetypes($type).generic;

            $avc-copy := $avc-is-generic
              ?? $avc.HOW.instantiate_generic($avc, $type_environment)
              !! $avc;
        }

        if $avc-is-generic {
            if nqp::isconcrete_nd($avc-copy) {
                my @avc_mro  := nqp::how_nd($avc-copy).mro($avc-copy);
                my $avc_mro;

                my int $i;
                ++$i while (
                  $avc_mro := nqp::atpos(@avc_mro, $i)
                ).HOW.is_mixin($avc_mro);

                nqp::bindattr($avc-copy, $avc_mro, '$!descriptor', $cd_ins)
                  if $avc_mro.HOW.has_attribute($avc_mro, '$!descriptor');
            }
            nqp::bindattr($ins, Attribute, '$!auto_viv_container', $avc-copy);
        }

        if $pkg.HOW.archetypes($pkg).generic {
            nqp::bindattr($ins, Attribute, '$!package',
              $pkg.HOW.instantiate_generic($pkg, $type_environment)
            );
        }
        nqp::bindattr($ins, Attribute, '$!build_closure',
#?if !jvm
          nqp::p6capturelexwhere($bc.clone)
#?endif
#?if jvm
          $bc.clone
#?endif
        ) if nqp::defined($bc);

        $ins
    }));

    Attribute.HOW.compose_repr(Attribute);

#- Scalar ----------------------------------------------------------------------
# class Scalar is Any {
#     has Mu $!descriptor;
#     has Mu $!value;

    Scalar.HOW.add_parent(Scalar, Any);

    Scalar.HOW.add_attribute(Scalar, BOOTSTRAPATTR.new(
      :name<$!descriptor>, :type(Mu), :package(Scalar)
    ));

    Scalar.HOW.add_attribute(Scalar, BOOTSTRAPATTR.new(
      :name<$!value>, :type(Mu), :package(Scalar)
    ));

    Scalar.HOW.add_method(Scalar, 'is_generic',
      nqp::getstaticcode(sub ($self) {
        $self := nqp::decont($self);

        my $descr := nqp::getattr($self, Scalar, '$!descriptor');
        $descr.is_generic || $descr.is_default_generic
    }));

    Scalar.HOW.add_method(Scalar, 'instantiate_generic',
      nqp::getstaticcode(sub ($self, $type_environment) {
        $self := nqp::decont($self);

        nqp::bindattr($self, Scalar, '$!descriptor',
          nqp::getattr($self, Scalar, '$!descriptor').instantiate_generic(
            $type_environment
          )
        );

        my $value := nqp::getattr($self, Scalar, '$!value');
        nqp::bindattr($self, Scalar, '$!value',
          $value.HOW.instantiate_generic($value, $type_environment)
        ) if $value.HOW.archetypes($value).generic;

        $self
    }));

    Scalar.HOW.compose_repr(Scalar);

    # To preserve historical behavior, we never repossess a Scalar container.
    nqp::neverrepossess(Scalar);

    # Scalar needs to be registered as a container type. Also provide the
    # slow-path implementation of various container operations.
    sub setup_scalar_contspec($type) {
        nqp::setcontspec($type, 'value_desc_cont', nqp::hash(
            'attrs_class', Scalar,
            'descriptor_attr', '$!descriptor',
            'value_attr', '$!value',
            'store', nqp::getstaticcode(sub ($cont, $val) {
                my $desc := nqp::getattr($cont, Scalar, '$!descriptor');
                if nqp::isconcrete($desc) {
                    $val := $desc.default if nqp::eqaddr($val.WHAT, Nil);
                    my $type := $desc.of;
                    if nqp::eqaddr($type, Mu) || nqp::istype($val, $type) {
                        if $type.HOW.archetypes($type).coercive {
                            my $coercion_type := $type.HOW.wrappee($type, :coercion);
#?if moar
                            nqp::bindattr($cont, Scalar, '$!value', nqp::dispatch('raku-coercion', $coercion_type, $val));
#?endif
#?if !moar
                            nqp::bindattr($cont, Scalar, '$!value', $coercion_type.HOW.coerce($coercion_type, $val));
#?endif
                        }
                        else {
                            nqp::bindattr($cont, Scalar, '$!value', $val);
                        }
                        my $what := $desc.WHAT;
                        unless nqp::eqaddr($what, ContainerDescriptor) ||
                               nqp::eqaddr($what, ContainerDescriptor::Untyped) {
                            $desc.assigned($cont)
                                unless nqp::eqaddr($what, ContainerDescriptor::UninitializedAttribute);
                            nqp::bindattr($cont, Scalar, '$!descriptor', $desc.next);
                        }
                    }
                    else {
                        Perl6::Metamodel::Configuration.throw_or_die(
                            'X::TypeCheck::Assignment',
                            "Type check failed in assignment",
                            :symbol($desc.name),
                            :$desc,
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
                my $what := $desc.WHAT;
                unless nqp::eqaddr($what, ContainerDescriptor) ||
                       nqp::eqaddr($what, ContainerDescriptor::Untyped) {
                    $desc.assigned($cont)
                        unless nqp::eqaddr($what, ContainerDescriptor::UninitializedAttribute);
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
                            :$desc,
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
                            :$desc,
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
    }
    setup_scalar_contspec(Scalar);

    # Cache a single default Scalar container spec, to ensure we only get
    # one of them.
    Scalar.HOW.cache_add(
      Scalar,
      'default_cont_spec',
      ContainerDescriptor::Untyped.new(:of(Mu), :default(Any), :name('element'))
    );

#- ScalarVAR -------------------------------------------------------------------
# class ScalarVAR is Scalar {
    ScalarVAR.HOW.add_parent(ScalarVAR, Scalar);
    ScalarVAR.HOW.compose_repr(ScalarVAR);
    setup_scalar_contspec(ScalarVAR);

#- xxxRef ----------------------------------------------------------------------
# Set up various native reference types.

    sub setup_native_ref_type($type, $primitive, $ref_kind) {
        my $HOW := $type.HOW;

        $HOW.add_parent($type, Any);
        $HOW.set_native_type($type, $primitive);
        $HOW.set_ref_kind($type, $ref_kind);
        $HOW.compose_repr($type);

        nqp::setcontspec($type, 'native_ref', nqp::null);
    }

    setup_native_ref_type(IntLexRef,        int, 'lexical');
    setup_native_ref_type(UIntLexRef,      uint, 'lexical');
    setup_native_ref_type(NumLexRef,        num, 'lexical');
    setup_native_ref_type(StrLexRef,        str, 'lexical');

    setup_native_ref_type(IntAttrRef,       int, 'attribute');
    setup_native_ref_type(UIntAttrRef,     uint, 'attribute');
    setup_native_ref_type(NumAttrRef,       num, 'attribute');
    setup_native_ref_type(StrAttrRef,       str, 'attribute');

    setup_native_ref_type(IntPosRef,        int, 'positional');
    setup_native_ref_type(UIntPosRef,      uint, 'positional');
    setup_native_ref_type(NumPosRef,        num, 'positional');
    setup_native_ref_type(StrPosRef,        str, 'positional');

    setup_native_ref_type(IntMultidimRef,   int, 'multidim');
    setup_native_ref_type(UIntMultidimRef, uint, 'multidim');
    setup_native_ref_type(NumMultidimRef,   num, 'multidim');
    setup_native_ref_type(StrMultidimRef,   str, 'multidim');

#?if js
    setup_native_ref_type(Int64LexRef,      int64, 'lexical'   );
    setup_native_ref_type(Int64AttrRef,     int64, 'attribute' );
    setup_native_ref_type(Int64PosRef,      int64, 'positional');
    setup_native_ref_type(Int64MultidimRef, int64, 'multidim'  );
#?endif

#- Proxy -----------------------------------------------------------------------
# class Proxy is Any {
#    has Mu &!FETCH;
#    has Mu &!STORE;
    my $PROXY_FETCH := nqp::getstaticcode(sub ($cont) {
        my $var := nqp::create(Scalar);
        nqp::bindattr($var, Scalar, '$!value', $cont);
        nqp::decont(nqp::getattr($cont, Proxy, '&!FETCH')($var))
    });

    my $PROXY_STORE := nqp::getstaticcode(sub ($cont, $val) {
        my $var := nqp::create(Scalar);
        nqp::bindattr($var, Scalar, '$!value', $cont);
        nqp::getattr($cont, Proxy, '&!STORE')($var, $val)
    });

    Proxy.HOW.add_parent(Proxy, Any);

    Proxy.HOW.add_attribute(Proxy, BOOTSTRAPATTR.new(
      :name<&!FETCH>, :type(Mu), :package(Proxy)
    ));

    Proxy.HOW.add_attribute(Proxy, BOOTSTRAPATTR.new(
      :name<&!STORE>, :type(Mu), :package(Proxy)
    ));

    Proxy.HOW.add_method(Proxy, 'FETCH', $PROXY_FETCH);
    Proxy.HOW.add_method(Proxy, 'STORE', $PROXY_STORE);

    Proxy.HOW.add_method(Proxy, 'new',
      nqp::getstaticcode(sub ($type, :$FETCH!, :$STORE!) {
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

#- Signature -------------------------------------------------------------------

    # Helper for creating a scalar attribute. Sets it up as a real Raku
    # Attribute instance, complete with container descriptor and optional
    # auto-viv container.
    sub scalar_attr(
      $name, $type, $package, :$associative_delegate, :$auto_viv_container
    ) {
        my $container_descriptor := ContainerDescriptor.new(:$name, :of($type));

        if $auto_viv_container {
            $auto_viv_container := nqp::create(Scalar);
            nqp::bindattr($auto_viv_container, Scalar, '$!descriptor',
              $container_descriptor);
            nqp::bindattr($auto_viv_container, Scalar, '$!value', $type);
        }
        else {
            $auto_viv_container := nqp::null;
        }

        Attribute.new(
          :$name, :$type, :$package,
          :$container_descriptor, :$auto_viv_container, :$associative_delegate
        )
    }

    # Helper for creating an attribute that vivifies to a clone of some VM
    # storage type (or, if it's a type object, is just initialized with that
    # type object); used for the storage slots of arrays and hashes.
    sub storage_attr(
      $name,
      $type,
      $package,
      $auto_viv_primitive,
      :$associative_delegate
    ) {
        Attribute.new(
          :$name, :$type, :$package,
          :$auto_viv_primitive, :$associative_delegate
        )
    }

# class Signature is Any {
#    has @!params;
#    has Mu $!returns;
#    has int $!arity;
#    has Num $!count;
#    has Code $!code;
#    has int $!readonly;

    Signature.HOW.add_parent(Signature, Any);

    Signature.HOW.add_attribute(Signature, Attribute.new(
      :name<@!params>, :type(List), :package(Signature)
    ));

    Signature.HOW.add_attribute(Signature, scalar_attr(
      '$!returns', Mu, Signature
    ));

    Signature.HOW.add_attribute(Signature, Attribute.new(
      :name<$!arity>, :type(int), :package(Signature)
    ));

    Signature.HOW.add_attribute(Signature, Attribute.new(
      :name<$!count>, :type(Num), :package(Signature)
    ));

    Signature.HOW.add_attribute(Signature, Attribute.new(
      :name<$!code>, :type(Code), :package(Signature)
    ));

    Signature.HOW.add_attribute(Signature, Attribute.new(
      :name<$!readonly>, :type(int), :package(Signature)
    ));

    Signature.HOW.add_method(Signature, 'is_generic',
      nqp::getstaticcode(sub ($self) {
        # XXX this should be an attribute, set at build time

        # If any parameter is generic, so are we.
        my @params := nqp::getattr($self, Signature, '@!params');
        my int $m := nqp::elems(@params);
        my int $i;
        while $i < $m {
            nqp::atpos(@params, $i).is_generic
              ?? (return 1)
              !! ++$i;
        }

        0
    }));

    Signature.HOW.add_method(Signature, 'instantiate_generic',
      nqp::getstaticcode(sub ($self, $type_environment) {

        # Go through parameters, builidng new list. If any
        # are generic, instantiate them. Otherwise leave them
        # as they are.
        my $ins    := nqp::clone($self);
        nqp::bindattr($ins, Signature, '@!params', my @ins_params);

        my @params := nqp::getattr($self, Signature, '@!params');
        my $m := nqp::elems(@params);
        my int $i;
        while $i < $m {
            my $param := nqp::atpos(@params, $i);
            nqp::push(
              @ins_params,
              $param.is_generic
                ?? $param.instantiate_generic($type_environment)
                !! $param
            );
            ++$i;
        }

        my $returns := nqp::getattr($self, Signature, '$!returns');
        nqp::bindattr($ins, Signature, '$!returns',
          $returns.HOW.instantiate_generic($returns, $type_environment)
        ) if nqp::not_i(nqp::isnull($returns))
          && $returns.HOW.archetypes($returns).generic;

        $ins
    }));

    Signature.HOW.add_method(Signature, 'returns',
      nqp::getstaticcode(sub ($self) {
        $self := nqp::decont($self);

        nqp::getattr($self,Signature,'$!returns')
    }));

    Signature.HOW.add_method(Signature, 'set_returns',
      nqp::getstaticcode(sub ($self, $type) {
        $self := nqp::decont($self);

        nqp::bindattr(
          $self, Signature, '$!returns', nqp::decont($type)
        )
    }));

    Signature.HOW.add_method(Signature, 'has_returns',
      nqp::getstaticcode(sub ($self) {
        $self := nqp::decont($self);

        nqp::hllboolfor(
          nqp::not_i(nqp::isnull(
            nqp::getattr($self, Signature, '$!returns')
          )),
          'Raku'
        )
    }));

    Signature.HOW.compose_repr(Signature);

#- Parameter -------------------------------------------------------------------
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
#     has Signature $!signature_constraint

    Parameter.HOW.add_parent(Parameter, Any);

    Parameter.HOW.add_attribute(Parameter, Attribute.new(
      :name<$!variable_name>, :type(str), :package(Parameter)
    ));

    Parameter.HOW.add_attribute(Parameter, scalar_attr(
      '@!named_names', Mu, Parameter
    ));

    Parameter.HOW.add_attribute(Parameter, scalar_attr(
      '@!type_captures', Mu, Parameter
    ));

    Parameter.HOW.add_attribute(Parameter, Attribute.new(
      :name<$!flags>, :type(int), :package(Parameter)
    ));

    Parameter.HOW.add_attribute(Parameter, Attribute.new(
      :name<$!type>, :type(Mu), :package(Parameter)
    ));

    Parameter.HOW.add_attribute(Parameter, scalar_attr(
      '@!post_constraints', List, Parameter
    ));

    Parameter.HOW.add_attribute(Parameter, scalar_attr(
      '$!sub_signature', Signature, Parameter
    ));

    Parameter.HOW.add_attribute(Parameter, scalar_attr(
      '$!default_value', Code, Parameter
    ));

    Parameter.HOW.add_attribute(Parameter, scalar_attr(
      '$!container_descriptor', Mu, Parameter
    ));

    Parameter.HOW.add_attribute(Parameter, Attribute.new(
      :name<$!attr_package>, :type(Mu), :package(Parameter)
    ));

    Parameter.HOW.add_attribute(Parameter, Attribute.new(
      :name<$!why>, :type(Mu), :package(Parameter)
    ));

    Parameter.HOW.add_attribute(Parameter, scalar_attr(
      '$!signature_constraint', Signature, Parameter
    ));

    Parameter.HOW.add_method(Parameter, 'is_generic',
      nqp::getstaticcode(sub ($self) {
        # XXX this should be an attribute set at build time

        # If nominal type or attr_package is generic, so are we.
        my $type := nqp::getattr($self, Parameter, '$!type');
        my int $generic := $type.HOW.archetypes($type).generic;

        unless $generic {
            my $ap := nqp::getattr($self, Parameter, '$!attr_package');
            $generic := nqp::not_i(nqp::isnull($ap))
              && $ap.HOW.archetypes($ap).generic;
        }
        unless $generic {
            my $sigc := nqp::getattr($self,Parameter, '$!signature_constraint');
            $generic := nqp::defined($sigc) && $sigc.is_generic;
        }

        nqp::hllboolfor($generic, "Raku")
    }));

    Parameter.HOW.add_method(Parameter, 'instantiate_generic',
      nqp::getstaticcode(sub ($self, $type_environment) {

        # Clone with the type instantiated.
        my $ins  := nqp::clone($self);
        my $type := my $ins_type :=
          nqp::getattr($self, Parameter, '$!type');
        my $cd   := my $ins_cd :=
          nqp::getattr($self, Parameter, '$!container_descriptor');
        my $ap   := my $ins_ap :=
          nqp::getattr($self, Parameter, '$!attr_package');
        my $sigc := my $ins_sigc :=
          nqp::getattr($self, Parameter, '$!signature_constraint');
        my int $flags := nqp::getattr_i($self, Parameter, '$!flags');

        if $type.HOW.archetypes($type).generic {
            $ins_type := $type.HOW.instantiate_generic($type,$type_environment);
            $ins_cd   := $cd.instantiate_generic($type_environment)
              unless nqp::isnull($cd);
        }

        $ins_ap := $ap.HOW.instantiate_generic($ap, $type_environment)
          if nqp::not_i(nqp::isnull($ap))
          && $ap.HOW.archetypes($ap).generic;

        $ins_sigc := $sigc.instantiate_generic($type_environment)
          if nqp::defined($sigc) && $sigc.is_generic;

        unless $ins_type.HOW.archetypes($ins_type).generic {
            nqp::bindattr_i($ins, Parameter, '$!flags',
              $flags - nqp::const::SIG_ELEM_TYPE_GENERIC
            ) if $flags +& nqp::const::SIG_ELEM_TYPE_GENERIC;
        }

        nqp::bindattr_i($ins, Parameter, '$!flags',
          $flags +| nqp::const::SIG_ELEM_IS_COERCIVE
        ) if $ins_type.HOW.archetypes($ins_type).coercive;

        nqp::bindattr($ins, Parameter, '$!type',                 $ins_type);
        nqp::bindattr($ins, Parameter, '$!container_descriptor', $ins_cd  );
        nqp::bindattr($ins, Parameter, '$!attr_package',         $ins_ap  );
        nqp::bindattr($ins, Parameter, '$!signature_constraint', $ins_sigc);

        $ins
    }));

    Parameter.HOW.add_method(Parameter, 'set_rw',
      nqp::getstaticcode(sub ($self) {
        $self := nqp::decont($self);

        # XXX This error handling doesn't belong here
        my str $name := nqp::getattr_s($self, Parameter, '$!variable_name');
        unless nqp::isnull_s($name) || nqp::eqat($name, '$', 0) {
            my $error;
            $error := "For parameter '$name', '"
              ~ nqp::substr($name, 0, 1)
              ~ "' sigil containers don't need 'is rw' to be writable\n"
              if nqp::eqat($name, '%', 0) || nqp::eqat($name, '@', 0);

            $error := $error
              ~ "Can only use 'is rw' on a scalar ('\$' sigil) parameter, not '$name'";
            nqp::die($error);
        }

        my int $flags := nqp::getattr_i($self, Parameter, '$!flags');
        if $flags +& nqp::const::SIG_ELEM_IS_OPTIONAL {
            Perl6::Metamodel::Configuration.throw_or_die(
              'X::Trait::Invalid',
              "Cannot use 'is rw' on optional parameter '$name'",
              :type<is>,
              :subtype<rw>,
              :declaring('optional parameter'),
              :$name
            );
        }

        nqp::bindattr_i($self, Parameter, '$!flags',
          $flags +| nqp::const::SIG_ELEM_IS_RW);

        $self
    }));

    Parameter.HOW.add_method(Parameter, 'set_copy',
      nqp::getstaticcode(sub ($self) {
        $self := nqp::decont($self);

        nqp::bindattr_i($self, Parameter, '$!flags',
          nqp::getattr_i($self, Parameter, '$!flags')
            +| nqp::const::SIG_ELEM_IS_COPY
        );

        $self
    }));

    Parameter.HOW.add_method(Parameter, 'set_required',
      nqp::getstaticcode(sub ($self) {
        $self := nqp::decont($self);

        my int $flags := nqp::getattr_i($self, Parameter, '$!flags');
        nqp::bindattr_i($self, Parameter, '$!flags',
          $flags - nqp::const::SIG_ELEM_IS_OPTIONAL
        ) if $flags +& nqp::const::SIG_ELEM_IS_OPTIONAL;

        $self
    }));

    Parameter.HOW.add_method(Parameter, 'set_raw',
      nqp::getstaticcode(sub ($self) {
        $self := nqp::decont($self);

        nqp::bindattr_i($self, Parameter, '$!flags',
          nqp::getattr_i($self, Parameter, '$!flags')
            +| nqp::const::SIG_ELEM_IS_RAW
        );

        $self
    }));

    Parameter.HOW.add_method(Parameter, 'set_item',
      nqp::getstaticcode(sub ($self) {
        $self := nqp::decont($self);

        nqp::bindattr_i($self, Parameter, '$!flags',
          nqp::getattr_i($self, Parameter, '$!flags')
            +| nqp::const::SIG_ELEM_IS_ITEM
        );

        $self
    }));

    Parameter.HOW.add_method(Parameter, 'set_onearg',
      nqp::getstaticcode(sub ($self) {
        $self := nqp::decont($self);

        nqp::bindattr_i($self, Parameter, '$!flags',
          nqp::getattr_i($self, Parameter, '$!flags')
            +| nqp::const::SIG_ELEM_SLURPY_ONEARG
        );

        $self
    }));

    Parameter.HOW.add_method(Parameter, 'WHY',
      nqp::getstaticcode(sub ($self) {
        $self := nqp::decont($self);

        my $why := nqp::getattr($self, Parameter, '$!why');
        if nqp::isnull($why) || !$why {
            Nil
        }
        else {
            $why.set_docee($self);
            $why
        }
    }));

    Parameter.HOW.add_method(Parameter, 'container_descriptor',
      nqp::getstaticcode(sub ($self) {
        $self := nqp::decont($self);

        nqp::getattr($self, Parameter, '$!container_descriptor')
    }));

    Parameter.HOW.add_method(Parameter, 'coercive',
      nqp::getstaticcode(sub ($self) {
        $self := nqp::decont($self);

        (nqp::getattr_i($self, Parameter, '$!flags')
          +& nqp::const::SIG_ELEM_IS_COERCIVE) && 1
    }));

    Parameter.HOW.compose_repr(Parameter);

#- Code ------------------------------------------------------------------------
# class Code {
#     has Code      $!do;         # Low level code object
#     has Signature $!signature;  # Signature object
#     has           @!compstuff;  # Place for the compiler to hang stuff

    Code.HOW.add_parent(Code, Any);
    Code.HOW.add_attribute(Code, Attribute.new(
      :name<$!do>, :type(Code), :package(Code)
    ));
    Code.HOW.add_attribute(Code, Attribute.new(
      :name<$!signature>, :type(Signature), :package(Code)
    ));
    Code.HOW.add_attribute(Code, scalar_attr(
      '@!compstuff', List, Code
    ));

    # Need clone in here, plus generics instantiation.
    Code.HOW.add_method(Code, 'clone',
      nqp::getstaticcode(sub ($self) {
        $self := nqp::decont($self);

        if nqp::isconcrete($self) {
            $self    := nqp::clone($self);
            my $do   := nqp::getattr($self, Code, '$!do');
            my $cldo := nqp::clone($do);
            nqp::setcodeobj(
              nqp::bindattr($self, Code, '$!do', $cldo),
              $self
            );

            my $compstuff := nqp::getattr($self, Code, '@!compstuff');
            nqp::atpos($compstuff, 2)($do, $self)  # XXX will $cldo do?
              unless nqp::isnull($compstuff);
        }

        $self
    }));

    Code.HOW.add_method(Code, 'name',
      nqp::getstaticcode(sub ($self) {
        $self := nqp::decont($self);

        nqp::getcodename(nqp::getattr($self, Code, '$!do'))
    }));

    Code.HOW.add_method(Code, 'set_name',
      nqp::getstaticcode(sub ($self, $name) {
        $self := nqp::decont($self);

        nqp::setcodename(nqp::getattr($self, Code, '$!do'), $name)
    }));

    Code.HOW.add_method(Code, 'id',
      nqp::getstaticcode(sub ($self) {
        $self := nqp::decont($self);

        nqp::where(nqp::getattr($self, Code, '$!do'))
    }));

    Code.HOW.compose_repr(Code);

#?if !moar
    # Need to actually run the code block. Also need this available before
    # we finish up the stub.
    Code.HOW.set_invocation_attr(Code, Code, '$!do');
    Code.HOW.compose_invocation(Code);
#?endif

#- Block -----------------------------------------------------------------------
# class Block is Code {
#     has Mu $!phasers;                # phasers for this block
#     has Mu $!why;

    Block.HOW.add_parent(Block, Code);

    Block.HOW.add_attribute(Block, Attribute.new(
      :name<$!phasers>, :type(Mu), :package(Block), :auto_viv_primitive(NQPMu)
    ));

    Block.HOW.add_attribute(Block, scalar_attr(
      '$!why', Mu, Block
    ));

    Block.HOW.add_method(Block, 'clone',
      nqp::getstaticcode(sub ($self) {
        $self := nqp::decont($self);

        if nqp::isconcrete($self) {
            my $cloned := nqp::clone($self);
            my $do     := nqp::getattr($self, Code, '$!do');
            my $cldo   := nqp::clone($do);

            nqp::setcodeobj(
              nqp::bindattr($cloned, Code, '$!do', $cldo),
              $cloned
            );
#?if !jvm
            my $phasers := nqp::getattr($cloned, Block, '$!phasers');
            $self."!clone_phasers"($cloned, $phasers) if nqp::ishash($phasers);
#?endif

            my $compstuff := nqp::getattr($cloned, Code, '@!compstuff');
            nqp::atpos($compstuff, 2)($do, $cloned)
              unless nqp::isnull($compstuff);

            # XXX this should probably be done after the clone that installs
            #     the sub
            my $why := nqp::getattr($cloned, Block, '$!why');
            $why.set_docee($cloned) unless nqp::isnull($why);
            $cloned
        }
        else {
            $self
        }
    }));

    Block.HOW.add_method(Block, '!clone_phasers',
      nqp::getstaticcode(sub ($self, $cloned, $phasers) {
#?if !jvm

        # Helper sub for phasers that require innerlex capturing
        my $cl_phasers := nqp::null;
        sub clone_if_phasers(str $name) {
            my $list   := nqp::atkey($phasers, $name);
            my int $m  := nqp::elems($list);
            my @blocks := nqp::create(IterationBuffer);

            my int $i;
            while $i < $m {
                nqp::captureinnerlex(
                  nqp::getattr(
                    nqp::push(@blocks, nqp::atpos($list, $i).clone),
                    Code,
                    '$!do'
                  )
                );
                ++$i;
            }

            nqp::bindkey(
              nqp::ifnull($cl_phasers, $cl_phasers := nqp::clone($phasers)),
              $name,
              @blocks
            );
        }

        # XXX don't capture innerlex, why?
        if nqp::existskey($phasers, 'NEXT') {
            my $next   := nqp::atkey($phasers, 'NEXT');
            my int $m  := nqp::elems($next);
            my @blocks := nqp::create(IterationBuffer);

            my int $i;
            while $i < $m {
                nqp::push(@blocks, nqp::atpos($next, $i).clone);
                ++$i;
            }

            nqp::bindkey(
              nqp::ifnull($cl_phasers, $cl_phasers := nqp::clone($phasers)),
              'NEXT',
              @blocks
            );
        }

        clone_if_phasers('LAST')  if nqp::existskey($phasers, 'LAST' );
        clone_if_phasers('QUIT')  if nqp::existskey($phasers, 'QUIT' );
        clone_if_phasers('CLOSE') if nqp::existskey($phasers, 'CLOSE');

        nqp::bindattr($cloned, Block, '$!phasers', $cl_phasers)
          unless nqp::isnull($cl_phasers);
#?endif
    }));

    Block.HOW.add_method(Block, '!capture_phasers', nqp::getstaticcode(sub ($self) {
            $self  := nqp::decont($self);
#?if !jvm
            my $phasers := nqp::getattr($self, Block, '$!phasers');
            if nqp::ishash($phasers) {

                sub capture_phaser(str $name) {
                    my @blocks := nqp::atkey($phasers, $name);

                    my int $m := nqp::elems(@blocks);
                    my int $i;
                    while $i < $m {
                        nqp::p6capturelexwhere(nqp::atpos(@blocks, $i));
                        ++$i;
                    }
                }

                capture_phaser('NEXT')  if nqp::existskey($phasers, 'NEXT' );
                capture_phaser('LAST')  if nqp::existskey($phasers, 'LAST' );
                capture_phaser('QUIT')  if nqp::existskey($phasers, 'QUIT' );
                capture_phaser('CLOSE') if nqp::existskey($phasers, 'CLOSE');
            }
#?endif
            $self
    }));

    Block.HOW.compose_repr(Block);
#?if !moar
    Block.HOW.compose_invocation(Block);
#?endif

#- Routine ---------------------------------------------------------------------
# class Routine is Block {
#     has Mu $!dispatcher;
#     has int $!flags;
#     has Mu $!inline_info;
#     has Mu $!package;
#     has Mu $!op_props;  # to be DEPRECATED
#--- proto specific ---
#     has @!dispatchees;
#     has Mu $!dispatch_info;
#     has @!dispatch_order;
#     has Mu $!dispatch_cache;  # NOT on MoarVM

    Routine.HOW.add_parent(Routine, Block);

    Routine.HOW.add_attribute(Routine, Attribute.new(
      :name<$!dispatcher>, :type(Mu), :package(Routine),
      :auto_viv_primitive(NQPMu)
    ));

    Routine.HOW.add_attribute(Routine, Attribute.new(
      :name<$!flags>, :type(int), :package(Routine)
    ));

    Routine.HOW.add_attribute(Routine, Attribute.new(
      :name<$!inline_info>, :type(Mu), :package(Routine)
    ));

    Routine.HOW.add_attribute(Routine, Attribute.new(
      :name<$!package>, :type(Mu), :package(Routine)
    ));

    Routine.HOW.add_attribute(Routine, Attribute.new(
      :name<$!op_props>, :type(Mu), :package(Routine)  # to be DEPRECATED
    ));

    Routine.HOW.add_attribute(Routine, Attribute.new(
      :name<@!dispatchees>, :type(List), :package(Routine),
      :auto_viv_primitive(NQPMu)
    ));

    Routine.HOW.add_attribute(Routine, Attribute.new(
      :name<$!dispatch_info>, :type(Mu), :package(Routine)
    ));

    Routine.HOW.add_attribute(Routine, scalar_attr(
      '@!dispatch_order', List, Routine
    ));

#?if !moar
    Routine.HOW.add_attribute(Routine, Attribute.new(
      :name<$!dispatch_cache>, :type(Mu), :package(Routine)
    ));
#?endif

    Routine.HOW.add_method(Routine, 'is_generic',
      nqp::getstaticcode(sub ($self) {
        $self := nqp::decont($self);

        # Delegate to signature, since it contains all the type info.
        nqp::getattr($self, Code, '$!signature').is_generic
    }));

    Routine.HOW.add_method(Routine, 'instantiate_generic',
      nqp::getstaticcode(sub ($self, $type_environment) {
        $self := nqp::decont($self);

        # Clone the code object, then instantiate the generic signature.
        # Also need to clone dispatchees list.
        my $ins := $self.clone;

        my $dispatchees := nqp::getattr($self, Routine, '@!dispatchees');
        nqp::bindattr($ins, Routine, '@!dispatchees', nqp::clone($dispatchees))
          if nqp::defined($dispatchees);

        my $sig := nqp::getattr($self, Code, '$!signature');
        nqp::bindattr($ins, Code, '$!signature',
          $sig.instantiate_generic($type_environment)
        );

        $ins
    }));

    Routine.HOW.add_method(Routine, 'is_dispatcher',
      nqp::getstaticcode(sub ($self) {
        $self := nqp::decont($self);

        nqp::hllboolfor(
          nqp::defined(nqp::getattr($self, Routine, '@!dispatchees')),
          "Raku"
        );
    }));

    Routine.HOW.add_method(Routine, 'add_dispatchee',
      nqp::getstaticcode(sub ($self, $dispatchee) {
        $self := nqp::decont($self);

        my $dispatchees := nqp::getattr($self, Routine, '@!dispatchees');
        nqp::die("Cannot add dispatchee '"
          ~ $dispatchee.name
          ~ "' to non-dispatcher code object '"
          ~ $self.name()
          ~ "'"
        ) unless nqp::defined($dispatchees);

        nqp::push($dispatchees, $dispatchee);
        nqp::bindattr($dispatchee, Routine, '$!dispatcher', $self);

        nqp::scwbdisable;
        nqp::bindattr($self, Routine, '@!dispatch_order', nqp::null);
#?if !moar
        nqp::bindattr($self, Routine, '$!dispatch_cache', nqp::null);
#?endif
        nqp::scwbenable;

        $self
    }));

    Routine.HOW.add_method(Routine, 'derive_dispatcher',
      nqp::getstaticcode(sub ($self) {
        $self  := $self.clone;

        nqp::bindattr($self, Routine, '@!dispatchees',
          nqp::clone(nqp::getattr($self, Routine, '@!dispatchees'))
        );

        $self
    }));

    Routine.HOW.add_method(Routine, 'dispatcher',
      nqp::getstaticcode(sub ($self) {
        $self := nqp::decont($self);

        nqp::getattr($self, Routine, '$!dispatcher')
    }));

    Routine.HOW.add_method(Routine, 'dispatchees',
      nqp::getstaticcode(sub ($self) {
        $self := nqp::decont($self);

        nqp::getattr($self, Routine, '@!dispatchees')
    }));

    Routine.HOW.add_method(Routine, 'dispatch_info',
      nqp::getstaticcode(sub ($self) {
        $self := nqp::decont($self);

        my $info := nqp::getattr($self, Routine, '$!dispatch_info');
        nqp::isconcrete($info)
          ?? $info
          !! $self."!create_dispatch_info"()
    }));

    Routine.HOW.add_method(Routine, '!create_dispatch_info',
      nqp::getstaticcode(sub ($self) {
        $self := nqp::decont($self);

        # XXX convert to nqp::const::xxx
        my int $SLURPY_ARITY := 1073741824;  # 1 +< 30

        # Get hold of signature.
        my $sig    := nqp::getattr($self, Code, '$!signature');
        my @params := nqp::getattr($sig, Signature, '@!params');

        my @types       := nqp::list;
        my @type_flags  := nqp::list_i;
        my @constraints := nqp::list;
        my @rwness      := nqp::list_i;

        # Create it an entry.
        my %info := nqp::hash(
          'sub',         $self,
          'signature',   $sig,
          'types',       @types,
          'type_flags',  @type_flags,
          'constraints', @constraints,
          'rwness',      @rwness,
        );

        nqp::bindkey(%info, 'exact_invocant', 1)
          if nqp::istype($self, Submethod);

        my int $significant_param;
        my int $min_arity;
        my int $max_arity;
        my int $num_types;
        my @coerce_type_idxs;
        my @coerce_type_objs;
        my int $item_disambiguation;

        my int $n := nqp::elems(@params);
        my int $j;
        while $j < $n {
            my $param := nqp::atpos(@params, $j);
            ++$j;  # increment here so we can "next"

            # If it's got a sub-signature, also need a bind check and
            # to check constraint on every dispatch. Same if it's got a
            # `where` clause.
            unless nqp::isnull(
              nqp::getattr($param, Parameter, '$!sub_signature')
            ) && nqp::isnull(
              nqp::getattr($param, Parameter, '@!post_constraints')
            ) && nqp::not_i(nqp::defined(
              nqp::getattr($param, Parameter, '$!signature_constraint')
            )) {
                nqp::bindkey(%info, 'bind_check',  1);
                nqp::bindkey(%info, 'constrainty', 1);
            }

            # For named arguments:
            # * Under the legacy dispatcher (not on MoarVM, which uses
            #   new-disp) we leave named argument checking to be done via
            #   a bind check. We only set that if it's a required named.
            # * For the new-disp based dispatcher, we collect a list of
            #   required named arguments and allowed named arguments, and
            #   filter those out without the bind check.
            my int $flags :=
              nqp::getattr_i($param, Parameter, '$!flags');
            my $named_names :=
              nqp::getattr($param, Parameter, '@!named_names');

            unless nqp::isnull($named_names) {
                if $flags +& nqp::const::SIG_ELEM_MULTI_INVOCANT {
                    unless $flags +& nqp::const::SIG_ELEM_IS_OPTIONAL {
                        nqp::push(
                          nqp::ifnull(
                            nqp::atkey(%info, 'required_names'),
                            nqp::bindkey(%info, 'required_names', nqp::list)
                          ),
                          $named_names
                        );
                        nqp::bindkey(
                          %info, 'req_named', nqp::atpos_s($named_names, 0)
                        ) if nqp::elems($named_names) == 1;
                    }
                    nqp::bindkey(%info, 'bind_check', 1);
                }

                my $allowed_names := nqp::ifnull(
                  nqp::atkey(%info, 'allowed_names'),
                  nqp::bindkey(%info, 'allowed_names', nqp::hash)
                );

                my int $o := nqp::elems($named_names);
                my int $k;
                while $k < $o {
                    nqp::bindkey(
                      $allowed_names,
                      nqp::atpos_s($named_names, $k),
                      NQPMu
                    );
                    ++$k;
                }
                # we can't use binding in the case of SIG_ELEM_IS_ITEM due
                # to a bug that deconts arguments as passed to ParamTypeCheck
                # in 'callstatic' and 'callmethod'. instead we do a type check
                # of named arguments in the case of ambiguous dispatch.
                if $flags +& nqp::const::SIG_ELEM_IS_ITEM {
                    ++$item_disambiguation;
                    my $named_types := nqp::ifnull(
                        nqp::atkey(%info, 'named_types'),
                        nqp::bindkey(%info, 'named_types', nqp::hash)
                    );
                    if $flags +& nqp::const::SIG_ELEM_TYPE_GENERIC {
                        nqp::bindkey(%info, 'bind_check', 1);
                        nqp::bindkey(%info, 'constrainty', 1);
                        nqp::bindkey($named_types, nqp::atpos_s($named_names,0), Any);
                    }
                    else {
                        my $ptype := nqp::getattr($param, Parameter, '$!type');
                        if $ptype.HOW.archetypes($ptype).coercive {
                            my $ctype := $ptype.HOW.wrappee($ptype, :coercion);
                            $ptype    := $ctype.HOW.constraint_type($ctype);
                        }
                        nqp::bindkey($named_types, nqp::atpos_s($named_names,0), $ptype);
                    }
                    # TODO: Coercion types ...
                }
                next;
            }

            if $flags +& nqp::const::SIG_ELEM_ALL_NAMES_OK {
                nqp::bindkey(%info, 'allows_all_names', 1);
                nqp::deletekey(%info, 'allowed_names');
            }
            last if $flags +& nqp::const::SIG_ELEM_SLURPY_NAMED;

            # Otherwise, positional or slurpy and contributes to arity.
            if $flags +& nqp::const::SIG_ELEM_SLURPY_ARITY {
                $max_arity := $SLURPY_ARITY;
                next;
            }
            elsif $flags +& nqp::const::SIG_ELEM_IS_OPTIONAL {
                ++$max_arity;
            }
            else {
                ++$max_arity;
                ++$min_arity;
            }

            # Record type info for this parameter.
            if $flags +& nqp::const::SIG_ELEM_TYPE_GENERIC {
                nqp::bindkey(%info, 'bind_check', 1);
                nqp::bindkey(%info, 'constrainty', 1);
                nqp::bindpos(@types, $significant_param, Any);
            }
            else {
                my $ptype := nqp::getattr($param, Parameter, '$!type');
                if $ptype.HOW.archetypes($ptype).coercive {
                    my $ctype := $ptype.HOW.wrappee($ptype, :coercion);
                    $ptype    := $ctype.HOW.constraint_type($ctype);
                }
                nqp::bindpos(@types, $significant_param, $ptype);
            }

            nqp::bindpos(@constraints, $significant_param, 1)
              unless nqp::isnull(
                nqp::getattr($param, Parameter, '@!post_constraints')
              ) && nqp::not_i(nqp::defined(
                nqp::getattr($param, Parameter, '$!signature_constraint')
              ));

            ++$num_types
              if $flags +& nqp::const::SIG_ELEM_MULTI_INVOCANT;

            nqp::bindpos_i(@rwness, $significant_param, 1)
              if $flags +& nqp::const::SIG_ELEM_IS_RW;

            if $flags +& nqp::const::SIG_ELEM_DEFINED_ONLY {
                nqp::bindpos_i(
                  @type_flags,
                  $significant_param,
                  nqp::const::DEFCON_DEFINED
                );
            }
            elsif $flags +& nqp::const::SIG_ELEM_UNDEFINED_ONLY {
                nqp::bindpos_i(
                  @type_flags,
                  $significant_param,
                  nqp::const::DEFCON_UNDEFINED
                );
            }

            if $flags +& nqp::const::SIG_ELEM_NATIVE_VALUE {
                nqp::bindpos_i(
                  @type_flags,
                  $significant_param,
                  ($flags +& nqp::const::SIG_ELEM_NATIVE_STR_VALUE
                    ?? nqp::const::TYPE_NATIVE_STR
                    !! $flags +& nqp::const::SIG_ELEM_NATIVE_INT_VALUE
                      ?? nqp::const::TYPE_NATIVE_INT
                      !! $flags +& nqp::const::SIG_ELEM_NATIVE_UINT_VALUE
                        ?? nqp::const::TYPE_NATIVE_UINT
                        !! nqp::const::TYPE_NATIVE_NUM  # SIG_ELEM_NATIVE_NUM_VALUE
                  ) + nqp::atpos_i(@type_flags, $significant_param)
                )
            }

            if $flags +& nqp::const::SIG_ELEM_IS_ITEM {
                nqp::bindpos_i(
                  @type_flags,
                  $significant_param,
                  nqp::atpos_i(@type_flags, $significant_param) + nqp::const::SIG_ELEM_IS_ITEM
                );
                ++$item_disambiguation;
            }

            # Keep track of coercion types; they'll need an extra entry
            # in the things we sort.
            if $param.coercive {
                nqp::push(@coerce_type_idxs, $significant_param);
                my $ptype := nqp::getattr($param, Parameter, '$!type');
                my $ctype := $ptype.HOW.wrappee($ptype, :coercion);
                nqp::push(
                  @coerce_type_objs, $ctype.HOW.target_type($ctype)
                );
            }

            ++$significant_param;
        }
        nqp::bindkey(%info, 'min_arity', $min_arity);
        nqp::bindkey(%info, 'max_arity', $max_arity);
        nqp::bindkey(%info, 'num_types', $num_types);

        nqp::bindkey(%info, 'item_disambiguation', 1)
            if $item_disambiguation;

       if $self.revision-gated {
           nqp::bindkey(%info, 'required_revision', $self.REQUIRED-REVISION);
       }

        if nqp::elems(@coerce_type_idxs) {
            nqp::bindkey(%info, 'coerce_type_idxs', @coerce_type_idxs);
            nqp::bindkey(%info, 'coerce_type_objs', @coerce_type_objs);
        }

        nqp::bindattr($self, Routine, '$!dispatch_info', %info)
    }));

    # Helper class to handle sorting by abstracting the edges logic into
    # a better optimizable object.
    class Node {
        has     $!possible;
        has     $!edges;
        has int $!edges_in;

        method new($possible) {
            my $obj := nqp::create(self);
            nqp::bindattr($obj, Node, '$!possible', $possible);
            nqp::bindattr($obj, Node, '$!edges',    nqp::list);
            $obj
        }

        method possible() { $!possible  }

        method push_outer_edge($node) {
            nqp::push($!edges, $node);
            nqp::bindattr_i($node, Node, '$!edges_in',
              nqp::getattr($node, Node, '$!edges_in') + 1
            );
        }

        method accepted(@result) {
            # Note: returns 0 if the "if" failed, -1 if successful
            if $!edges_in == 0 {
                nqp::push(@result, $!possible);
                nqp::bindattr_i(self, Node, '$!edges_in', -1);
            }
        }

        method remove_if_accepted() {
            if $!edges_in == -1 {
                my     $edges    := $!edges;
                my int $nr_edges := nqp::elems($edges);

                my int $i;
                while $i < $nr_edges {
                    my $node := nqp::atpos($edges, $i);
                    nqp::bindattr_i($node, Node, '$!edges_in',
                      nqp::getattr_i($node, Node, '$!edges_in') - 1
                    );
                    ++$i;
                }
                nqp::bindattr_i(self, Node, '$!edges_in', -2);
            }
        }
    }

    Routine.HOW.add_method(Routine, '!sort_dispatchees_internal',
      nqp::getstaticcode(sub ($self) {
        $self := nqp::decont($self);

        # XXX convert to nqp::const::xxx
        my int $SLURPY_ARITY      := 1073741824;  # 1 +< 30

        # Takes two candidates and determines if the first one is narrower
        # than the second. Returns a true value if they are.
        sub is_narrower(%a, %b) {

            # Work out how many parameters to compare, factoring in
            # slurpiness and optionals.
            my int $types_to_check := nqp::atkey(%a, 'num_types');
            my int $num_types_b    := nqp::atkey(%b, 'num_types');

            my int $max_arity_a := nqp::atkey(%a, 'max_arity');
            my int $max_arity_b := nqp::atkey(%b, 'max_arity');

            if $types_to_check == $num_types_b {
                # already set
            }
            elsif nqp::atkey(%a, 'min_arity') == nqp::atkey(%b, 'min_arity') {
                $types_to_check := $num_types_b
                  if $types_to_check > $num_types_b
            }
            else {
                return $max_arity_a != $SLURPY_ARITY
                    && $max_arity_b == $SLURPY_ARITY;
            }

            # Shortcuts to lists
            my $types_a := nqp::atkey(%a, 'types');
            my $types_b := nqp::atkey(%b, 'types');

            # Analyse each parameter in the two candidates.
            my int $narrower;
            my int $tied;
            my int $i;
            while $i < $types_to_check {
                my $type_obj_a := nqp::atpos($types_a, $i);
                my $type_obj_b := nqp::atpos($types_b, $i);

                if nqp::eqaddr($type_obj_a, $type_obj_b) {

                    # Same type; narrower if first has constraints and other
                    # doesn't; narrower if first is rw and second isn't; tied
                    # if neither has constraints or both have constraints.
                    my $constraints_a :=
                      nqp::atpos(nqp::atkey(%a, 'constraints'), $i);
                    my $constraints_b :=
                      nqp::atpos(nqp::atkey(%b, 'constraints'), $i);

                    if $constraints_a && !$constraints_b {
                        ++$narrower;
                    }
                    elsif nqp::atpos_i(nqp::atkey(%a, 'rwness'), $i)
                        > nqp::atpos_i(nqp::atkey(%b, 'rwness'), $i) {
                        ++$narrower;
                    }
                    elsif !$constraints_a && !$constraints_b
                       ||  $constraints_a &&  $constraints_b {
                        ++$tied;
                    }
                }

                # Different types
                else {
                    my int $type_flags_a :=
                      nqp::atpos_i(nqp::atkey(%a, 'type_flags'), $i);
                    my int $type_flags_b :=
                      nqp::atpos_i(nqp::atkey(%b, 'type_flags'), $i);

                    if $type_flags_a +& nqp::const::TYPE_NATIVE_MASK
                      && nqp::not_i(
                           $type_flags_b +& nqp::const::TYPE_NATIVE_MASK
                         ) {
                        # Narrower because natives always are.
                        ++$narrower;
                    }

                    elsif $type_flags_b +& nqp::const::TYPE_NATIVE_MASK
                      && nqp::not_i(
                           $type_flags_a +& nqp::const::TYPE_NATIVE_MASK
                         ) {
                        # Wider; skip over here so we don't go counting this
                        # as tied in the next branch.
                    }
                    elsif nqp::istype($type_obj_a, $type_obj_b) {
                        # Narrower - note it and we're done.
                        ++$narrower;
                    }

                    elsif nqp::not_i(nqp::istype($type_obj_b, $type_obj_a)) {
                        # Make sure it's tied, rather than the other way around.
                        ++$tied;
                    }
                }

                ++$i;
            }

            # And now for the result
            $narrower && $narrower + $tied == $types_to_check
              # If one is narrower than the other from current analysis,
              # we're done.
              ?? 1
              !! $tied != $types_to_check
                # If they aren't tied, we're also done.
                ?? 0
                !! $max_arity_a != $SLURPY_ARITY
                     && $max_arity_b == $SLURPY_ARITY
                  # Otherwise, we see if one has a slurpy and the other not.
                  # A lack of slurpiness makes the candidate narrower.
                  ?? 1
                  # Also narrower if the first needs a bind check and the
                  # second doesn't, if we wouldn't deem the other one narrower
                  # than this one in terms of slurpyness. Otherwise, they're
                  # tied.
                  !! nqp::not_i($max_arity_b != $SLURPY_ARITY
                             && $max_arity_a == $SLURPY_ARITY
                     ) &&  nqp::atkey(%a, 'bind_check')
                       && !nqp::atkey(%b, 'bind_check')
        }

        my @candidates := nqp::getattr($self, Routine, '@!dispatchees');
        my @graph;

        # Create a node for each candidate in the graph.
        my int $m := nqp::elems(@candidates);
        my int $i;
        while $i < $m {
            my %info := nqp::atpos(@candidates, $i).dispatch_info;

            # Add it to graph node, and initialize list of edges.
            nqp::push(@graph, Node.new(%info));

            # If there were any coercion types, then we also need to create
            # a candidate entry for the specific types.
            if nqp::existskey(%info, 'coerce_type_idxs') {

                my %c_info  := nqp::clone(%info);
                my @c_types := nqp::clone(nqp::atkey(%info, 'types'));
                nqp::bindkey(%c_info, 'types', @c_types);

                my @coerce_type_idxs := nqp::atkey(%info, 'coerce_type_idxs');
                my @coerce_type_objs := nqp::atkey(%info, 'coerce_type_objs');
                my int $m := nqp::elems(@coerce_type_idxs);
                my int $i;
                while $i < $m {
                    nqp::bindpos(
                      @c_types,
                      nqp::atpos(@coerce_type_idxs, $i),
                      nqp::atpos(@coerce_type_objs, $i)
                    );
                    ++$i;
                }
                nqp::push(@graph, Node.new(%c_info));
            }

            ++$i;
        }

        # Now analyze type narrowness of the candidates relative to each
        # other and create the edges.
        $m := nqp::elems(@graph);
        $i := 0;
        while $i < $m {
            my $node_i := nqp::atpos(@graph, $i);

            my int $j;
            while $j < $m {
                unless $i == $j {
                    my $node_j := nqp::atpos(@graph, $j);

                    $node_i.push_outer_edge($node_j)
                      if is_narrower($node_i.possible, $node_j.possible);
                }
                ++$j;
            }
            ++$i;
        }

        # Perform the topological sort.
        my int $candidates_to_sort := nqp::elems(@graph);
        my @result;
        while $candidates_to_sort > 0 {
            my int $rem_results := nqp::elems(@result);

            # Find any nodes that have no incoming edges and add them to
            # results.
            $i := 0;
            while $i < $m {
                --$candidates_to_sort
                  if nqp::atpos(@graph, $i).accepted(@result);
                ++$i;
            }

            nqp::die("Circularity detected in multi sub types"
              ~ ($self.name ?? " for &" ~ $self.name !! '')
            ) if $rem_results == nqp::elems(@result);

            # Now we need to decrement edges in counts for things that had
            # edges from candidates we added here.
            $i := 0;
            while $i < $m {
                nqp::atpos(@graph, $i).remove_if_accepted;
                ++$i;
            }

            # This is end of a tied group, so leave a gap.
            nqp::push(@result, Mu);
        }

        # Add final null sentinel.
        nqp::push(@result, Mu);

        @result
    }));

    Routine.HOW.add_method(Routine, 'sort_dispatchees',
      nqp::getstaticcode(sub ($self) {
        $self := nqp::decont($self);

        unless nqp::isnull(nqp::getattr($self, Routine, '@!dispatch_order')) {
            nqp::bindattr($self, Routine, '@!dispatch_order',
                $self.'!sort_dispatchees_internal'());
        }
    }));

    Routine.HOW.add_method(Routine, 'dispatch_order',
      nqp::getstaticcode(sub ($self) {
        $self := nqp::decont($self);

        my $dispatch_order := nqp::getattr($self, Routine, '@!dispatch_order');
        if nqp::isnull($dispatch_order) {
            nqp::scwbdisable;
            nqp::bindattr($self, Routine, '@!dispatch_order',
              $dispatch_order := $self.'!sort_dispatchees_internal'()
            );
            nqp::scwbenable;
        }
        $dispatch_order
    }));

    Routine.HOW.add_method(Routine, 'find_best_dispatchee',
      nqp::getstaticcode(sub ($self, $capture, int $many = 0) {
        $self := nqp::decont($self);

        # Count arguments.
        my int $num_args := nqp::captureposelems($capture);

        # Get list and number of candidates, triggering a sort if there are none.
        my @candidates := $self.dispatch_order;

        # Iterate over the candidates and collect best ones; terminate
        # when we see two type objects (indicating end).
        my int $cur_idx;
        my int $pure_type_result := 1;
        my $many_res := $many ?? nqp::list !! Mu;
        my @possibles;
        my int $done_bind_check;

        # Only do itemized argument disambiguation when the group contains
        # a candidate with params having the 'is item' trait.
        my int $candidate-with-itemized-params;

        # Core types to be initialized lazily once needed
        my $Positional             := nqp::null;
        my $PositionalBindFailover := nqp::null;
        my $Associative            := nqp::null;

        my $caller-revision := nqp::getlexcaller('$?LANGUAGE-REVISION');

        my int $done;
        until $done {
            my $candidate := nqp::atpos(@candidates, $cur_idx);

            if nqp::isconcrete($candidate) {

                if nqp::isconcrete(my $required-revision := nqp::atkey($candidate, 'required_revision'))
                && $caller-revision < $required-revision {
                    $cur_idx++;
                    next;
                }

                # Mark this group for disambigation via is item traits on params
                if ! $candidate-with-itemized-params
                && nqp::atkey($candidate, 'item_disambiguation') {
                    $candidate-with-itemized-params := 1;
                }

                # Admissible by arity.
                unless $num_args < nqp::atkey($candidate, 'min_arity')
                    || $num_args > nqp::atkey($candidate, 'max_arity') {

                    # Arity OK; now check if it's admissible by type.
                    my int $type_check_count :=
                      nqp::atkey($candidate, 'num_types');
                    $type_check_count := $num_args
                      if $type_check_count > $num_args;

                    my int $no_mismatch := 1;
                    my int $i;
                    while $i < $type_check_count
                      && $no_mismatch {

                        my $type_obj := nqp::atpos(
                          nqp::atkey($candidate, 'types'), $i
                        );
                        my int $flags := nqp::atpos_i(
                          nqp::atkey($candidate, 'type_flags'), $i
                        );
                        my $arg := nqp::captureposarg($capture, $i);
                        my int $got_prim :=
                          nqp::captureposprimspec($capture, $i);

                        # If we need a container but don't have one it
                        # clearly can't work.
                        if nqp::atpos_i(nqp::atkey($candidate, 'rwness'), $i)
                          && nqp::not_i(nqp::isrwcont($arg)) {
                            $no_mismatch := 0;
                        }

                        # A natively typed value?
                        elsif $flags +& nqp::const::TYPE_NATIVE_MASK {

                            # Looking for a natively typed value. Did we
                            # get one?
                            if $got_prim == nqp::const::BIND_VAL_OBJ {

                                # Object, but could be a native container.
                                # If not, mismatch.
                                $no_mismatch := 0
                                  unless (
                                    ($flags +& nqp::const::TYPE_NATIVE_STR)
                                      && nqp::iscont_s($arg)
                                  ) || (
                                    ($flags +& nqp::const::TYPE_NATIVE_INT)
                                      && nqp::iscont_i($arg)
                                  ) || (
                                    ($flags +& nqp::const::TYPE_NATIVE_UINT)
                                      && nqp::iscont_u($arg)
                                  ) || nqp::iscont_n($arg);  # NATIVE_NUM
                            }

                            # Got a native, does it match?
                            elsif (
                              ($flags +& nqp::const::TYPE_NATIVE_STR)
                                && $got_prim != nqp::const::BIND_VAL_STR
                            ) || (
                              ($flags +& nqp::const::TYPE_NATIVE_INT)
                                && $got_prim != nqp::const::BIND_VAL_INT
                            ) || (
                              ($flags +& nqp::const::TYPE_NATIVE_UINT)
                                && $got_prim != nqp::const::BIND_VAL_UINT
                            ) || $got_prim != nqp::const::BIND_VAL_NUM {  # NATIVE_NUM

                                # Mismatch.
                                $no_mismatch := 0;
                            }
                        }

                        # Not a native parameter, but we may getting a native
                        # value, possibly in a container.
#?if !jvm
                        else {

                            # Assume a native value unless proven otherwise
                            my int $primish := 1;

                            # Set type to fall back to if no native involved
                            my $type := $arg.WHAT;

                            # Adapt type to any native argument (whether in a
                            # container or not)
                            $got_prim == nqp::const::BIND_VAL_OBJ
                              ?? nqp::iscont_s($arg)
                                ?? ($type := Str)
                                !! nqp::iscont_i($arg) || nqp::iscont_u($arg)
                                  ?? ($type := Int)
                                  !! nqp::iscont_n($arg)
                                    ?? ($type := Num)
                                    !! ($primish := 0)  # not a native container
                              !! $got_prim == nqp::const::BIND_VAL_STR
                                ?? ($type := Str)
                                !! ($got_prim == nqp::const::BIND_VAL_INT
                                     || $got_prim == nqp::const::BIND_VAL_UINT)
                                  ?? ($type := Int)
                                  !! ($type := Num);  # BIND_VAL_NUM

                            if nqp::eqaddr($type_obj, Mu) || nqp::istype($arg, $type_obj) {
                                if $i == 0 && nqp::existskey($candidate, 'exact_invocant') {
                                    unless $type.WHAT =:= $type_obj {
                                        $no_mismatch := 0;
                                    }
                                }
                            }

                            # Positional param needs PositionalBindFailover
                            elsif nqp::eqaddr(
                              $type_obj,
                              nqp::ifnull(
                                $Positional,
                                $Positional := nqp::gethllsym('Raku', 'MD_Pos')
                              )
                            ) {
                                $no_mismatch := 0 unless nqp::istype(
                                  $arg,
                                  nqp::ifnull(
                                    $PositionalBindFailover,
                                    $PositionalBindFailover :=
                                      nqp::gethllsym('Raku', 'MD_PBF')
                                  )
                                );
                            }

                            # Alas, no more ways to accept
                            else {
                                $no_mismatch := 0;
                            }

                            # Check conreteness if makes sense and so required
                            $no_mismatch := 0
                              if $no_mismatch
                              && (my int $mask :=
                                   $flags +& nqp::const::DEFCON_MASK)
                              && ($primish || nqp::isconcrete($arg)
                                   ?? nqp::const::DEFCON_DEFINED
                                   !! nqp::const::DEFCON_UNDEFINED
                                 ) != $mask;
#?endif
#?if jvm
                        else {
                            my int $primish;
                            my $param := $arg;
                            if $got_prim == nqp::const::BIND_VAL_OBJ {
                                if    nqp::iscont_i($param) { $param := Int; $primish := 1; }
                                elsif nqp::iscont_u($param) { $param := Int; $primish := 1; }
                                elsif nqp::iscont_n($param) { $param := Num; $primish := 1; }
                                elsif nqp::iscont_s($param) { $param := Str; $primish := 1; }
                                else { $param := nqp::hllizefor($param, 'Raku') }
                            }
                            else {
                                $param := $got_prim == nqp::const::BIND_VAL_INT ?? Int !!
                                          $got_prim == nqp::const::BIND_VAL_UINT ?? Int !!
                                          $got_prim == nqp::const::BIND_VAL_NUM ?? Num !!
                                                                        Str;
                                $primish := 1;
                            }
                            if nqp::eqaddr($type_obj, Mu) || nqp::istype($param, $type_obj) {
                                if $i == 0 && nqp::existskey($candidate, 'exact_invocant') {
                                    unless $param.WHAT =:= $type_obj {
                                        $no_mismatch := 0;
                                    }
                                }
                            }
                            elsif nqp::eqaddr(
                              $type_obj,
                              nqp::ifnull(
                                $Positional,
                                $Positional := nqp::gethllsym('Raku', 'MD_Pos')
                              )
                            ) {
                                $no_mismatch := 0 unless nqp::istype(
                                  $param,
                                  nqp::ifnull(
                                    $PositionalBindFailover,
                                    $PositionalBindFailover :=
                                      nqp::gethllsym('Raku', 'MD_PBF')
                                  )
                                );
                            }
                            else {
                                $no_mismatch := 0;
                            }

                            # Check for definedness if it still makes sense
                            if $no_mismatch && $flags +& nqp::const::DEFCON_MASK {
                                my int $defined := $primish || nqp::isconcrete($param);
                                my int $desired := $flags +& nqp::const::DEFCON_MASK;
                                if ($defined && $desired == nqp::const::DEFCON_UNDEFINED) ||
                                   (!$defined && $desired == nqp::const::DEFCON_DEFINED) {
                                    $no_mismatch := 0;
                                }
                            }
#?endif
                        }

                        ++$i;
                    }

                    # If it's an admissible candidate; add to list.
                    nqp::push(@possibles, $candidate) if $no_mismatch;
                }

                ++$cur_idx;
            }

            # We've hit the end of a tied group now. If any of them have a
            # bindability check requirement, we'll do any of those now.
            else {

                # Need to further check any possible candidates
                if nqp::elems(@possibles) {

                    my $new_possibles := nqp::null;
                    my int $m := nqp::elems(@possibles);
                    my int $i;
                    while $i < $m {
                        my %info := nqp::atpos(@possibles, $i);

                        # First, if there's a required named parameter and
                        # it was not passed, we can very quickly eliminate
                        # this candidate without doing a full bindability check
                        if nqp::existskey(%info, 'req_named')
                          && nqp::not_i(nqp::captureexistsnamed(
                               $capture,
                               nqp::atkey(%info, 'req_named')
                             )) {

                            # Required named arg not passed, so we eliminate
                            # it right here. Flag that we've built a list of
                            # new possibles, and that this was not a pure
                            # type-based result that we can cache.
                            $new_possibles := nqp::list
                              if nqp::isnull($new_possibles);
                        }

                        # Otherwise, may need full bind check.
                        elsif nqp::existskey(%info, 'bind_check')
                            && !$candidate-with-itemized-params
                        {
                            my $sub := nqp::atkey(%info, 'sub');
                            my $cs  := nqp::getattr($sub, Code, '@!compstuff');

                            unless nqp::isnull($cs) {
                                # We need to do the tie-break on something
                                # not yet compiled.  Get it compiled.
                                my $ctf := $cs[1];
                                $ctf() if $ctf;
                            }

                            # Since we had to do a bindability check, this is
                            # not a result we can cache on nominal type.
                            $pure_type_result := 0
                              if nqp::existskey(%info, 'constrainty');

                            # If we haven't got a possibles storage space,
                            # allocate it now.
                            $new_possibles := nqp::list
                              if nqp::isnull($new_possibles);

                            my $sig := nqp::getattr($sub, Code, '$!signature');
                            unless $done_bind_check {
                                # Need a copy of the capture, as we may later
                                # do a multi-dispatch when evaluating the
                                # constraint.
                                $capture := nqp::clone($capture);
                                $done_bind_check := 1;
                            }

                            # Accept if we can bind the sig to the capture
                            if nqp::p6isbindable($sig, $capture) {
                                nqp::push(
                                  $new_possibles,
                                  nqp::atpos(@possibles, $i)
                                );

                                # Terminate the loop if we only want one result
                                $i := $m unless $many;
                            }
                        }

                        # Otherwise, it's just nominal; accept it.
                        else {
                            nqp::push(
                              nqp::ifnull(
                                $new_possibles,
                                $new_possibles := nqp::list
                              ),
                              nqp::atpos(@possibles, $i)
                            )
                        }

                        ++$i;
                    }

                    # If we have an updated list of possibles, use this
                    # new one from here on in.
                    @possibles := $new_possibles
                      unless nqp::isnull($new_possibles);
                }

                # Now we have eliminated any that fail the bindability check.
                # See if we need to push it onto the many list and continue.
                # Otherwise, we have the result we were looking for.
                if $many {
                    while nqp::elems(@possibles) {
                        nqp::push(
                          $many_res,
                          nqp::atkey(nqp::shift(@possibles), 'sub')
                        )
                    }
                }

                $done := 1
                  # Found what we were looking for
                  if @possibles
                  # Or really reached the end (incrementing index on the fly)
                  || nqp::not_i(nqp::isconcrete(
                       nqp::atpos(@candidates, ++$cur_idx)
                     ));
            }
        }

        # If we were looking for many candidates, we're done now.
        return $many_res if $many;

        # If we still have multiple options and we want one, then check default
        # trait and then, failing that, if we got an exact arity match on
        # required parameters (which will beat matches on optional parameters).
        if nqp::elems(@possibles) > 1 {

            # Locate any default candidates; if we find multiple defaults,
            # this is no help, so we'll not bother collecting just which
            # ones are good.
            my $default_cand := nqp::null;
            my int $m := nqp::elems(@possibles);
            my int $i;
            while $i < $m {
                my $possibility := nqp::atpos(@possibles, $i);
                my $sub         := nqp::atkey($possibility, 'sub');

                # Is the routine marked with "is default"?
                if nqp::can($sub, 'default') && $sub.default {

                    # Set default if first or reset if not first
                    if nqp::isnull($default_cand) {
                        $default_cand := $possibility;
                    }
                    else {
                        $default_cand := nqp::null;
                        last;
                    }
                }

                ++$i;
            }

            # No single default found among the possibilities, so look for
            # exact arity match.
            if nqp::isnull($default_cand) {
                my $exact_arity := nqp::null;
                my int $i;
                while $i < $m {
                    my $possibility := nqp::atpos(@possibles, $i);
                    if nqp::atkey($possibility, 'min_arity') == $num_args &&
                       nqp::atkey($possibility, 'max_arity') == $num_args {

                        if nqp::isnull($exact_arity) {
                            $exact_arity := $possibility;
                        }
                        else {
                            $exact_arity := nqp::null;
                            last;
                        }
                    }
                    ++$i;
                }

                @possibles := nqp::list($exact_arity)
                  unless nqp::isnull($exact_arity);
            }

            # Reset possibilities to default: only one default default found
            else {
                @possibles := nqp::list($default_cand);
            }
        }

#?if !moar
        # If we're at a single candidate here, and we also know there's no
        # type constraints that follow, we can cache the result.
        sub add_to_cache($entry) {
            return 0 if nqp::capturehasnameds($capture);
            nqp::scwbdisable();
            nqp::bindattr($self, Routine, '$!dispatch_cache',
                nqp::multicacheadd(
                    nqp::getattr($self, Routine, '$!dispatch_cache'),
                    $capture, $entry));
            nqp::scwbenable();
        }
        if nqp::elems(@possibles) == 1 && $pure_type_result {
            add_to_cache(nqp::atkey(nqp::atpos(@possibles, 0), 'sub'));
        }
#?endif

        # Found nothing but have junctional arguments?
        unless nqp::elems(@possibles) {
            my int $i;
            while $i < $num_args {
                unless nqp::captureposprimspec($capture, $i) {
                    my $arg := nqp::captureposarg($capture, $i);
                    if nqp::istype($arg, Junction) && nqp::isconcrete($arg) {
                        my $junctional_res := -> *@pos, *%named {
                            Junction.AUTOTHREAD($self, |@pos, |%named)
                        }
#?if !moar
                        add_to_cache($junctional_res);
#?endif
                        # We're done: it's a Junction, deal with it!
                        return $junctional_res;
                    }
                }
                ++$i;
            }
        }

        # Disambiguate based on $param.is-item and itemized capture arguments
        if $candidate-with-itemized-params
        && (my int $num-candidates := nqp::elems(@possibles)) > 1 {
            my @fresh-possibles    := nqp::list;
            my @capture-item-assoc := nqp::list_i;
            my @capture-item-pos   := nqp::list_i;
            my $num-capture-args   := nqp::captureposelems($capture);
            my int $x;
            while $x < $num-capture-args {
                my $arg := nqp::captureposarg($capture, $x);
                if nqp::iscont($arg) {
                    if nqp::istype($arg,
                        nqp::ifnull($Associative, $Associative := nqp::gethllsym('Raku', 'Associative')))
                    {
                        nqp::push_i(@capture-item-assoc, $x);
                    } elsif nqp::istype($arg,
                        nqp::ifnull($Positional, $Positional := nqp::gethllsym('Raku', 'MD_Pos')))
                    {
                        nqp::push_i(@capture-item-pos, $x);
                    }
                }
                ++$x;
            }

            my %capture-named-item-assoc := nqp::hash;
            my %capture-named-item-pos   := nqp::hash;
            if my %nameds := nqp::capturenamedshash($capture) {
                my $it := nqp::iterator(%nameds);
                while $it {
                    my $elem := nqp::shift($it);
                    my $arg-name := nqp::iterkey_s($elem);
                    my $arg := nqp::iterval($elem);
                    if nqp::iscont($arg) {
                        if nqp::istype($arg,
                            nqp::ifnull($Associative,
                                $Associative := nqp::gethllsym('Raku', 'Associative')))
                        {
                            nqp::bindkey(%capture-named-item-assoc, $arg-name, 1);
                        } elsif nqp::istype($arg,
                            nqp::ifnull($Positional,
                                $Positional := nqp::gethllsym('Raku', 'MD_Pos')))
                        {
                            nqp::bindkey(%capture-named-item-pos, $arg-name, 1);
                        }
                    }
                }
            }

            $x := 0 if $x;
            my int $possible-candidate;
            while $x < $num-candidates {
                my $candidate := nqp::atpos(@possibles, $x);
                my $signature := nqp::atkey($candidate, 'signature');
                my @params    := nqp::getattr($signature, Signature, '@!params');

                my %cand-named-item-assoc := nqp::hash;
                my %cand-named-item-pos   := nqp::hash;

                my @cand-item-assoc := nqp::list_i;
                my @cand-item-pos   := nqp::list_i;
                my $num-cand-params := nqp::elems(@params);
                my @cand-types      := nqp::atkey($candidate, 'types');
                my %named-types     := nqp::atkey($candidate, 'named_types');
                my int $named-types := nqp::defined(%named-types);
                my int $y;
                while $y < $num-cand-params {
                    my $param := nqp::atpos(@params, $y);
                    my int $is-named-param := $param.named;
                    my $type  := $is-named-param && $named-types
                                    ?? nqp::atkey(%named-types, $param.usage-name)
                                    !! nqp::atpos(@cand-types, $y);
                    if $param.is-item {
                        if nqp::istype($type,
                            nqp::ifnull($Associative,
                                $Associative := nqp::gethllsym('Raku', 'Associative')))
                        {
                            $is-named-param
                                    ?? nqp::bindkey(%cand-named-item-assoc, $param.usage-name, 1)
                                    !! nqp::push_i(@cand-item-assoc, $y);
                        } elsif nqp::istype($type,
                            nqp::ifnull($Positional,
                                $Positional := nqp::gethllsym('Raku', 'MD_Pos')))
                        {
                            $is-named-param
                                    ?? nqp::bindkey(%cand-named-item-pos, $param.usage-name, 1)
                                    !! nqp::push_i(@cand-item-pos, $y);
                        }
                    }
                    ++$y;
                }

                if     (my int $num-assoc   := nqp::elems(@capture-item-assoc))       == nqp::elems(@cand-item-assoc)
                    && (my int $num-pos     := nqp::elems(@capture-item-pos))         == nqp::elems(@cand-item-pos)
                    && (my int $named-assoc := nqp::elems(%capture-named-item-assoc)) == nqp::elems(%cand-named-item-assoc)
                    && (my int $named-pos   := nqp::elems(%capture-named-item-pos))   == nqp::elems(%cand-named-item-pos)
                {
                    $possible-candidate := 1;
                    my int $z;
                    while $possible-candidate && $z < $num-assoc {
                        $possible-candidate := $possible-candidate
                            && nqp::atpos_i(@capture-item-assoc, $z) == nqp::atpos_i(@cand-item-assoc, $z);
                        ++$z;
                    }

                    $z := 0 if $z;
                    while $possible-candidate && $z < $num-pos {
                        $possible-candidate := $possible-candidate
                            && nqp::atpos_i(@capture-item-pos, $z) == nqp::atpos_i(@cand-item-pos, $z);
                        ++$z;
                    }

                    if $named-assoc {
                        my $it := nqp::iterator(%capture-named-item-assoc);
                        while $it {
                            $possible-candidate := $possible-candidate
                                && nqp::atkey(%cand-named-item-assoc, nqp::iterkey_s(nqp::shift($it)));
                        }
                    }

                    if $named-pos {
                        my $it := nqp::iterator(%capture-named-item-pos);
                        while $it {
                            $possible-candidate := $possible-candidate
                                && nqp::atkey(%cand-named-item-pos, nqp::iterkey_s(nqp::shift($it)));
                        }
                    }
                } elsif $possible-candidate {
                    # in case it became true on a previous candidate
                    $possible-candidate := 0;
                }
                nqp::push(@fresh-possibles, $candidate) if $possible-candidate;
                ++$x;
            }
            @possibles := @fresh-possibles if nqp::elems(@fresh-possibles);
        }

        # Need a unique candidate.
        if nqp::elems(@possibles) == 1 {
            nqp::atkey(nqp::atpos(@possibles, 0), 'sub')
        }

        # Alas, throw the appropriate error.  Don't care about optmizinng,
        # we're about to throw anyway
        else {
            my @ambiguous;
            for @possibles {
                nqp::push(@ambiguous, $_<sub>);
            }
            my str $name := $self.name;
#?if !moar
            sub assemble_capture(*@pos, *%named) {
                my $c := nqp::create(Capture);
                nqp::bindattr($c, Capture, '@!list', @pos);
                nqp::bindattr($c, Capture, '%!hash', %named);
                $c
            }
            my $raku_capture :=
              nqp::invokewithcapture(&assemble_capture, $capture);
#?endif
#?if moar
            my $raku_capture := nqp::create(Capture);
            nqp::bindattr($raku_capture, Capture, '@!list',
              nqp::syscall('capture-pos-args', $capture));
            nqp::bindattr($raku_capture, Capture, '%!hash',
              nqp::syscall('capture-named-args', $capture));
#?endif

            nqp::elems(@possibles)
              ?? Perl6::Metamodel::Configuration.throw_or_die(
                   'X::Multi::Ambiguous',
                   "Ambiguous call to $name",
                   :dispatcher($self),
                   :@ambiguous,
                   :capture($raku_capture)
                 )
              !! Perl6::Metamodel::Configuration.throw_or_die(
                   'X::Multi::NoMatch',
                   "Cannot call $name; no signatures match",
                   :dispatcher($self),
                   :capture($raku_capture)
                 );
        }
    }));

    Routine.HOW.add_method(Routine, 'analyze_dispatch',
      nqp::getstaticcode(sub ($self, @args, @flags) {
        # Compile time dispatch result.
        my $MD_CT_NOT_SURE :=  0;  # Needs a runtime dispatch.
        my $MD_CT_DECIDED  :=  1;  # Worked it out; see result.
        my $MD_CT_NO_WAY   := -1;  # Proved it'd never manage to dispatch.

        # Other constants we need.
        my int $ARG_IS_LITERAL    := 32;

        # Count arguments.
        my int $num_args := nqp::elems(@args);

        # Get the candidates in sorted order
        $self := nqp::decont($self);
        my @candidates := $self.dispatch_order;

        # Look through the candidates. If we see anything that needs a bind
        # check or a definedness check, we can't decide it at compile time,
        # so bail out immediately.
        my int $all_native := 1;
        my int $seen_all;
        my int $arity_possible;
        my int $type_possible;
        my int $type_mismatch;

        my $result := nqp::null;
        my int $cur_idx;
        while 1 {
            my $candidate := nqp::atpos(@candidates, $cur_idx);
            # increment for easier "next"ing and next candidate checking
            ++$cur_idx;

            # Did we reach the end of a tied group? If so, note we can only
            # consider the narrowest group, *unless* they are all natively
            # typed candidates in which case we can look a bit further.
            # We also exit if we found something.
            unless nqp::isconcrete($candidate) {
                my $next := nqp::atpos(@candidates, $cur_idx);

                if nqp::isconcrete($next)
                  && $all_native
                  && nqp::isnull($result) {
                    next;
                }

                else {
                    $seen_all := nqp::not_i(nqp::isconcrete($next));
                    last;
                }
            }

            # Check if it's admissible by arity.
            next
              if $num_args < nqp::atkey($candidate, 'min_arity')
              || $num_args > nqp::atkey($candidate, 'max_arity');

            # If we got this far, something at least matched on arity.
            $arity_possible := 1;

            # Check if it's admissible by type.
            my $type_check_count := nqp::atkey($candidate, 'num_types');
            $type_check_count := $num_args if $type_check_count > $num_args;

            my int $used_defcon;
            my int $type_match_impossible;
            $type_mismatch              := 0;

            my int $i;
            while $i < $type_check_count {
                my int $type_flags :=
                  nqp::atpos_i(nqp::atkey($candidate, 'type_flags'), $i);
                my int $got_prim := nqp::atpos(@flags, $i) +& 0xF;

                # Looking for a natively typed value
                if $type_flags +& nqp::const::TYPE_NATIVE_MASK {

                    # Did we get one?
                    if $got_prim == nqp::const::BIND_VAL_OBJ {

                        # Object; won't do.
                        $type_mismatch := 1;
                        last;
                    }

                    # Got a native, but does it have the right type? Also
                    # look at rw-ness for literals.
                    elsif ($type_flags +& nqp::const::TYPE_NATIVE_STR
                           && $got_prim != nqp::const::BIND_VAL_STR)
                      || ($type_flags +& nqp::const::TYPE_NATIVE_INT
                           && $got_prim != nqp::const::BIND_VAL_INT)
                      || ($type_flags +& nqp::const::TYPE_NATIVE_UINT
                           && $got_prim != nqp::const::BIND_VAL_UINT)
                      || ($type_flags +& nqp::const::TYPE_NATIVE_NUM
                           && $got_prim != nqp::const::BIND_VAL_NUM)
                      || (nqp::atpos(@flags, $i) +& $ARG_IS_LITERAL
                           && nqp::atpos_i(
                                nqp::atkey($candidate, 'rwness'), $i
                              )
                         ) {
                        # Mismatch.
                        $type_mismatch         := 1;
                        $type_match_impossible := 1;
                        last;
                    }
                }

                # Parameter is not native
                else {
                    $all_native := 0;

                    my $type_obj := nqp::atpos(
                      nqp::atkey($candidate, 'types'), $i
                    );

                    # Work out parameter.
                    my $type := $got_prim == nqp::const::BIND_VAL_OBJ
                      ?? nqp::atpos(@args, $i).WHAT
                      !! $got_prim == nqp::const::BIND_VAL_STR
                        ?? Str
                        !! $got_prim == nqp::const::BIND_VAL_INT
                             || $got_prim == nqp::const::BIND_VAL_UINT
                          ?? Int
                          !! Num;  # assume BIND_VAL_NUM

                    # A literal won't work with rw parameter.
                    if nqp::atpos(@flags, $i) +& $ARG_IS_LITERAL
                      && nqp::atpos_i(nqp::atkey($candidate, 'rwness'), $i) {
                        $type_mismatch         := 1;
                        $type_match_impossible := 1;
                        last;
                    }

                    # Check type. If that doesn't rule it out, then check
                    # if it's got definedness constraints. If it does,
                    # note that; if we match but depend on definedness
                    # constraints we can't do any more.
                    elsif nqp::not_i(nqp::eqaddr($type_obj, Mu))
                      && nqp::not_i(nqp::istype($type, $type_obj)) {
                        $type_mismatch := 1;

                        # We didn't match, but that doesn't mean we cannot at
                        # runtime (e.g. the most we know about the type could
                        # be that it's Any, but at runtime that feasibly could
                        # be Int). In some cases we never could though (Str
                        # passed to an Int parameter).
                        unless nqp::istype($type_obj, $type) {
                            $type_match_impossible := 1;
                            last;
                        }
                    }

                    # Need to check on definedness
                    elsif $type_flags +& nqp::const::DEFCON_MASK {
                        $used_defcon := 1;
                    }
                }

                ++$i;
            }

            $type_possible := 1 unless $type_match_impossible;
            next if $type_mismatch;

            return nqp::list($MD_CT_NOT_SURE, NQPMu)
              # If a definite check needs being done, we can't decide now
              if $used_defcon
              # If it's possible but needs a bind check, we're not going
              # to be able to decide it.
              || nqp::existskey($candidate, 'bind_check')
              # We already had result, in which case we're in bother 'cus
              # we don't know how to disambiguate at compile time.
              || nqp::not_i(nqp::isnull($result));

            # If we get here, it's the result
            $result := nqp::atkey($candidate, 'sub');
        }

        # If we saw all the candidates, and got no result, and the arity never
        # matched or when it did there was no way any candidates could get
        # passed matching types, then we know it would never work.
        if $seen_all
          && nqp::isnull($result)
          && (nqp::not_i($arity_possible) || nqp::not_i($type_possible)) {

            # Ensure no junctional args before we flag the failure.
            my int $m := nqp::elems(@args);
            my int $i;
            while $i < $m {
                nqp::istype(nqp::atpos(@args, $i), Junction)
                  ?? (return nqp::list($MD_CT_NOT_SURE, NQPMu))
                  !! ++$i;
            }
            nqp::list($MD_CT_NO_WAY, NQPMu);
        }

        # If we got a result, return it.  Otherwise, dunno...we'll have to
        # find out at runtime.
        else {
            nqp::isnull($result)
              ?? nqp::list($MD_CT_NOT_SURE, NQPMu)
              !! nqp::list($MD_CT_DECIDED, $result)
        }
    }));

    Routine.HOW.add_method(Routine, 'set_flag',
      nqp::getstaticcode(sub ($self, $bit) {
        $self := nqp::decont($self);

        nqp::bindattr_i($self, Routine, '$!flags',
          nqp::bitor_i(nqp::getattr_i($self, Routine, '$!flags'), $bit)
        );

        $self
    }));

    Routine.HOW.add_method(Routine, 'get_flag',
      nqp::getstaticcode(sub ($self, $bit) {
        $self := nqp::decont($self);

        nqp::hllboolfor(
          nqp::bitand_i(nqp::getattr_i($self, Routine, '$!flags'), $bit),
          "Raku"
        )
    }));

    Routine.HOW.add_method(Routine, 'set_rw',
      nqp::getstaticcode(sub ($self) {
        $self.set_flag(0x01)
    }));

    Routine.HOW.add_method(Routine, 'rw',
      nqp::getstaticcode(sub ($self) {
        $self.get_flag(0x01)
    }));

    Routine.HOW.add_method(Routine, 'set_yada',
      nqp::getstaticcode(sub ($self) {
        $self.set_flag(0x02)
    }));

    Routine.HOW.add_method(Routine, 'yada',
      nqp::getstaticcode(sub ($self) {
        $self.get_flag(0x02)
    }));

    Routine.HOW.add_method(Routine, 'set_inline_info',
      nqp::getstaticcode(sub ($self, $info) {
        $self := nqp::decont($self);

        nqp::bindattr($self, Routine, '$!inline_info', $info);

        $self
    }));

    Routine.HOW.add_method(Routine, 'inline_info',
      nqp::getstaticcode(sub ($self) {
        $self := nqp::decont($self);

        nqp::getattr($self, Routine, '$!inline_info')
    }));

    Routine.HOW.add_method(Routine, 'set_onlystar',
      nqp::getstaticcode(sub ($self) {
        $self.set_flag(0x04)
    }));

    Routine.HOW.add_method(Routine, 'onlystar',
      nqp::getstaticcode(sub ($self) {
        $self.get_flag(0x04)
    }));

    Routine.HOW.add_method(Routine, 'set-revision-gated',
      nqp::getstaticcode(sub ($self) {
        $self.set_flag(0x08);
    }));

    Routine.HOW.add_method(Routine, 'revision-gated',
      nqp::getstaticcode(sub ($self) {
        $self.get_flag(0x08);
    }));

    Routine.HOW.compose_repr(Routine);
#?if !moar
    Routine.HOW.compose_invocation(Routine);
#?endif

#- Sub -------------------------------------------------------------------------
# class Sub is Routine {
    Sub.HOW.add_parent(Sub, Routine);
    Sub.HOW.compose_repr(Sub);
#?if !moar
    Sub.HOW.compose_invocation(Sub);
#?endif

#- Operator --------------------------------------------------------------------
# class Operator is Sub {
    #     has Mu $!properties;
    Operator.HOW.add_parent(Operator, Sub);

    Operator.HOW.add_attribute(Operator, Attribute.new(
      :name<$!properties>, :type(Mu), :package(Operator)
    ));

    Operator.HOW.compose_repr(Operator);
#?if !moar
    Operator.HOW.compose_invocation(Operator);
#?endif

#- Method ----------------------------------------------------------------------
# class Method is Routine {
    Method.HOW.add_parent(Method, Routine);

    Method.HOW.compose_repr(Method);
#?if !moar
    Method.HOW.compose_invocation(Method);
#?endif

#- Submethod -------------------------------------------------------------------
# class Submethod is Routine {
    Submethod.HOW.add_parent(Submethod, Routine);

    Submethod.HOW.compose_repr(Submethod);
#?if !moar
    Submethod.HOW.compose_invocation(Submethod);
#?endif

#- Regex -----------------------------------------------------------------------
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
            @!pos-capture-counts   := nqp::list_i;
            @!named-capture-names  := nqp::list_s;
            @!named-capture-counts := nqp::list_i;

            # Go over the captures and build up the data structure.
            for %capnames {
                my str $name := nqp::iterkey_s($_);
                if $name ne '' {
                    my int $count := nqp::iterval($_);

                    # Positional
                    if nqp::ord($name) != 36 && nqp::ord($name) < 58 {
                        nqp::bindpos_i(@!pos-capture-counts, +$name, $count);
                    }

                    # Named
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
            nqp::elems(@!named-capture-counts)
              || nqp::elems(@!pos-capture-counts)
        }

        ## Raku Match object building
        ## (for use in standard Raku regexes)

        # Build a list of positional captures, or return a shared empty list if
        # there are none. This only populates the slots which need an array.
        my @EMPTY-LIST;
        my %EMPTY-HASH;
        method prepare-raku-list() {

            my int $n := nqp::elems(@!pos-capture-counts);
            if $n > 0 {
                my $result := nqp::list;
                my int $i;
                while $i < $n {
                    nqp::bindpos($result, $i, nqp::create(Array))
                      if nqp::atpos_i(@!pos-capture-counts, $i) >= 2;
                    ++$i;
                }
                $result
            }
            else {
#?if js
                # HACK js backend bug workaround
                nqp::list
#?endif
#?if !js
                @EMPTY-LIST
#?endif
            }
        }

        # Build a hash of named captures, or return a shared empty hash if there
        # are none. This only populates the slots that need an array.
        method prepare-raku-hash() {

            my int $n := nqp::elems(@!named-capture-counts);
            if $n > 0 {
                my $result := nqp::hash;
                my int $i;
                while $i < $n {
                    nqp::bindkey(
                      $result,
                      nqp::atpos_s(@!named-capture-names, $i),
                      nqp::create(Array)
                    ) if nqp::atpos_i(@!named-capture-counts, $i) >= 2;
                    ++$i;
                }
                $result
            }
            else {
#?if js
                # HACK js backend bug workaround
                nqp::hash
#?endif
#?if !js
                %EMPTY-HASH
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
                my $result := nqp::list;
                my int $i;
                while $i < $n {
                    nqp::bindpos($result, $i, nqp::list)
                      if nqp::atpos_i(@!pos-capture-counts, $i) >= 2;
                    ++$i;
                }
                $result
            }
            else {
                @EMPTY-LIST
            }
        }

        # Build a hash of named captures, or return a shared empty hash if
        # there are none. This only populates the slots that need an array.
        method prepare-hash() {
            my int $n := nqp::elems(@!named-capture-counts);
            if $n > 0 {
                my $result := nqp::hash;
                my int $i;
                while $i < $n {
                    nqp::bindkey(
                      $result,
                      nqp::atpos_s(@!named-capture-names, $i),
                      nqp::list
                    ) if nqp::atpos_i(@!named-capture-counts, $i) >= 2;
                    ++$i;
                }
                $result
            }
            else {
                %EMPTY-HASH
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

    Regex.HOW.add_attribute(Regex, scalar_attr(
      '$!caps', Mu, Regex, :auto_viv_container)
    );

    Regex.HOW.add_attribute(Regex, scalar_attr(
      '$!nfa', Mu, Regex, :auto_viv_container)
    );

    Regex.HOW.add_attribute(Regex, scalar_attr(
      '%!alt_nfas', Hash, Regex, :auto_viv_container)
    );

    Regex.HOW.add_attribute(Regex, scalar_attr(
      '$!source', str, Regex, :auto_viv_container)
    );

    Regex.HOW.add_attribute(Regex, scalar_attr(
      '$!topic', Mu, Regex, :auto_viv_container)
    );

    Regex.HOW.add_attribute(Regex, scalar_attr(
      '$!slash', Mu, Regex, :auto_viv_container)
    );

    Regex.HOW.add_method(Regex, 'SET_CAPS',
      nqp::getstaticcode(sub ($self, $capnames) {
        $self := nqp::decont($self);

        nqp::bindattr($self, Regex, '$!caps',
          RegexCaptures.from-capnames($capnames))
    }));

    Regex.HOW.add_method(Regex, 'SET_NFA',
      nqp::getstaticcode(sub ($self, $nfa) {
        $self := nqp::decont($self);

        nqp::bindattr($self, Regex, '$!nfa', $nfa)
    }));

    Regex.HOW.add_method(Regex, 'SET_ALT_NFA',
      nqp::getstaticcode(sub ($self, str $name, $nfa) {
        $self := nqp::decont($self);

        my %alts := nqp::getattr($self, Regex, '%!alt_nfas');
        nqp::bindattr($self, Regex, '%!alt_nfas', %alts := nqp::hash)
          unless %alts;
        nqp::bindkey(%alts, $name, $nfa)
    }));

    Regex.HOW.add_method(Regex, 'CAPS',
      nqp::getstaticcode(sub ($self) {
        $self := nqp::decont($self);

        nqp::getattr($self, Regex, '$!caps')
    }));

    Regex.HOW.add_method(Regex, 'NFA',
      nqp::getstaticcode(sub ($self) {
        $self := nqp::decont($self);

        nqp::getattr($self, Regex, '$!nfa')
    }));

    Regex.HOW.add_method(Regex, 'ALT_NFA',
      nqp::getstaticcode(sub ($self, str $name) {
        $self := nqp::decont($self);

        nqp::atkey(nqp::getattr($self, Regex, '%!alt_nfas'), $name)
    }));

    Regex.HOW.add_method(Regex, 'ALT_NFAS',
      nqp::getstaticcode(sub ($self) {
        $self := nqp::decont($self);

        my $store := nqp::decont(nqp::getattr($self, Regex, '%!alt_nfas'));
        nqp::istype($store, Hash)
          ?? nqp::hash
          !! $store
    }));

    Regex.HOW.compose_repr(Regex);
#?if !moar
    Regex.HOW.compose_invocation(Regex);
#?endif

#- Str -------------------------------------------------------------------------
# class Str is Cool {
#     has str $!value is box_target;

    Str.HOW.add_parent(Str, Cool);

    Str.HOW.add_attribute(Str, BOOTSTRAPATTR.new(
      :name<$!value>, :type(str), :box_target(1), :package(Str))
    );

    Str.HOW.set_boolification_mode(Str, 3);
    Str.HOW.publish_boolification_spec(Str);
    Str.HOW.compose_repr(Str);

#- Int -------------------------------------------------------------------------
# class Int is Cool {
#     has bigint $!value is box_target;

    Int.HOW.add_parent(Int, Cool);

    Int.HOW.add_attribute(Int, BOOTSTRAPATTR.new(
      :name<$!value>, :type(bigint), :box_target(1), :package(Int))
    );

    Int.HOW.set_boolification_mode(Int, 6);
    Int.HOW.publish_boolification_spec(Int);
    Int.HOW.compose_repr(Int);

#- Num -------------------------------------------------------------------------
# class Num is Cool {
#     has num $!value is box_target;

    Num.HOW.add_parent(Num, Cool);

    Num.HOW.add_attribute(Num, BOOTSTRAPATTR.new(
      :name<$!value>, :type(num), :box_target(1), :package(Num))
    );

    Num.HOW.set_boolification_mode(Num, 2);
    Num.HOW.publish_boolification_spec(Num);
    Num.HOW.compose_repr(Num);

#- Nil -------------------------------------------------------------------------
# class Nil is Cool {

    Nil.HOW.compose_repr(Nil);

#- List ------------------------------------------------------------------------
# class List is Cool {
#     has Mu $!reified;
#     has Mu $!todo;

    List.HOW.add_parent(List, Cool);

    List.HOW.add_attribute(List, storage_attr(
      '$!reified', Mu, List, Mu
    ));

    List.HOW.add_attribute(List, storage_attr(
      '$!todo', Mu, List, Mu
    ));

    List.HOW.compose_repr(List);

#- Slip ------------------------------------------------------------------------
# class Slip is List {

    Slip.HOW.add_parent(Slip, List);

    Slip.HOW.compose_repr(Slip);

#- Array -----------------------------------------------------------------------
# class Array is List {
#     has Mu $!descriptor;

    Array.HOW.add_parent(Array, List);

    Array.HOW.add_attribute(Array, storage_attr(
      '$!descriptor', Mu, Array,
      Scalar.HOW.cache_get(Scalar, 'default_cont_spec')
    ));

    Array.HOW.compose_repr(Array);

#- array -----------------------------------------------------------------------
# class array does Iterable does Positional {

    array.HOW.compose_repr(array);

#- IterationBuffer -------------------------------------------------------------
# class IterationBuffer {

    IterationBuffer.HOW.compose_repr(IterationBuffer);

#- Map -------------------------------------------------------------------------
# my class Map is Cool {
#     has Mu $!storage;

    Map.HOW.add_parent(Map, Cool);

    Map.HOW.add_attribute(Map, storage_attr(
      '$!storage', Mu, Map, nqp::hash, :associative_delegate
    ));

    Map.HOW.compose_repr(Map);
    nqp::settypehllrole(Map, 5);

#- Hash ------------------------------------------------------------------------
# my class Hash is Map {
#     has Mu $!descriptor;

    Hash.HOW.add_parent(Hash, Map);

    Hash.HOW.add_attribute(Hash, storage_attr(
      '$!descriptor', Mu, Hash,
      Scalar.HOW.cache_get(Scalar, 'default_cont_spec')
    ));

    Hash.HOW.compose_repr(Hash);
    nqp::settypehllrole(Hash, 5);

#- TypeEnv ---------------------------------------------------------------------
# my class TypeEnv is Map {

    TypeEnv.HOW.add_parent(TypeEnv, Map);

    TypeEnv.HOW.add_attribute(TypeEnv, Attribute.new(
      :name<$!primary>, :type(Bool), :package(TypeEnv)
    ));

    TypeEnv.HOW.add_attribute(TypeEnv, Attribute.new(
      :name<$!WHICH>, :type(ValueObjAt), :package(TypeEnv)
    ));

    TypeEnv.HOW.compose_repr(TypeEnv);
    nqp::settypehllrole(TypeEnv, 5);

#- Capture ---------------------------------------------------------------------
# class Capture is Any {
#     has @!list;
#     has %!hash;

    Capture.HOW.add_parent(Capture, Any);

    Capture.HOW.add_attribute(Capture, scalar_attr(
      '@!list', List, Capture
    ));

    Capture.HOW.add_attribute(Capture, scalar_attr(
      '%!hash', Hash, Capture
    ));

    Capture.HOW.compose_repr(Capture);

#- Junction --------------------------------------------------------------------
# class Junction is Mu {
#     has Mu         $!eigenstates;
#     has str        $!type;
#     has ValueObjAt $!WHICH;

    Junction.HOW.add_parent(Junction, Mu);

    Junction.HOW.add_attribute(Junction, scalar_attr(
      '$!eigenstates', Mu, Junction, :auto_viv_container
    ));

    Junction.HOW.add_attribute(Junction, scalar_attr(
      '$!type', str, Junction, :auto_viv_container
    ));

    Junction.HOW.add_attribute(Junction, Attribute.new(
      :name<$!WHICH>, :type(ValueObjAt), :package(Junction),
      :auto_viv_container
    ));

    Junction.HOW.compose_repr(Junction);

#- Bool ------------------------------------------------------------------------
# class Bool is Int {
#     has str $!key;
#     has int $!value;

    Bool.HOW.set_base_type(Bool, Int);

    Bool.HOW.add_attribute(Bool, Attribute.new(
      :name<$!key>, :type(str), :package(Bool)
    ));

    Bool.HOW.add_attribute(Bool, Attribute.new(
      :name<$!value>, :type(int), :package(Bool)
    ));

    Bool.HOW.add_method(Bool, 'key',
      nqp::getstaticcode(sub ($self) {
        $self := nqp::decont($self);

        nqp::getattr_s($self, Bool, '$!key')
    }));

    Bool.HOW.add_method(Bool, 'value',
      nqp::getstaticcode(sub ($self) {
        $self := nqp::decont($self);

        nqp::getattr_i($self, Bool, '$!value')
    }));

    Bool.HOW.set_boolification_mode(Bool, 1);
    Bool.HOW.publish_boolification_spec(Bool);
    Bool.HOW.compose_repr(Bool);

#- ObjAt -----------------------------------------------------------------------
# class ObjAt is Any {
#     has str $!value;

    ObjAt.HOW.add_parent(ObjAt, Any);

    ObjAt.HOW.add_attribute(ObjAt, BOOTSTRAPATTR.new(
      :name<$!value>, :type(str), :box_target, :package(ObjAt)
    ));

    ObjAt.HOW.compose_repr(ObjAt);

#- ValueObjAt ------------------------------------------------------------------
# class ValueObjAt is ObjAt {

    ValueObjAt.HOW.add_parent(ValueObjAt, ObjAt);

    ValueObjAt.HOW.compose_repr(ValueObjAt);

#- ForeignCode -----------------------------------------------------------------
# class ForeignCode {
#     has Mu $!do;                # Code object we delegate to

    ForeignCode.HOW.add_parent(ForeignCode, Any);

    ForeignCode.HOW.add_attribute(ForeignCode, Attribute.new(
      :name<$!do>, :type(Code), :package(ForeignCode)
    ));

    ForeignCode.HOW.compose_repr(ForeignCode);
#?if !moar
    ForeignCode.HOW.set_invocation_attr(ForeignCode, ForeignCode, '$!do');
    ForeignCode.HOW.compose_invocation(ForeignCode);
#?endif

#- Version ---------------------------------------------------------------------
# class Version {
#     has     $!parts;
#     has int $!plus;
#     has str $!string;

    Version.HOW.add_parent(Version, Any);

    Version.HOW.add_attribute(Version, Attribute.new(
      :name('$!parts'), :type(Mu), :package(Version)
    ));

    Version.HOW.add_attribute(Version, Attribute.new(
      :name('$!plus'), :type(int), :package(Version)
    ));

    Version.HOW.add_attribute(Version, Attribute.new(
      :name('$!string'), :type(str), :package(Version)
    ));

    Version.HOW.compose_repr(Version);

#- Stash -----------------------------------------------------------------------
# Set up Stash type, which is really just a hash with a name.
# class Stash is Hash {
#     has str $!longname;
#     has $!lock;

    Stash.HOW.add_parent(Stash, Hash);

    Stash.HOW.add_attribute(Stash, Attribute.new(
      :name<$!longname>, :type(str), :package(Stash)
    ));

    Stash.HOW.add_attribute(Stash, Attribute.new(
      :name<$!lock>, :type(Any), :package(Stash)
    ));

    Stash.HOW.compose_repr(Stash);

#- (epilogue) ------------------------------------------------------------------

    # Configure the stash type.
    Perl6::Metamodel::Configuration.set_stash_type(Stash, Map);

    # Give everything we've set up so far a Stash.
    Perl6::Metamodel::ClassHOW.add_stash(Mu);
    Perl6::Metamodel::ClassHOW.add_stash(Any);
    Perl6::Metamodel::ClassHOW.add_stash(Nil);
    Perl6::Metamodel::ClassHOW.add_stash(Cool);
    Perl6::Metamodel::ClassHOW.add_stash(Attribute);
    Perl6::Metamodel::ClassHOW.add_stash(Scalar);
    Perl6::Metamodel::ClassHOW.add_stash(ScalarVAR);
    Perl6::Metamodel::ClassHOW.add_stash(Proxy);
    Perl6::Metamodel::ClassHOW.add_stash(Signature);
    Perl6::Metamodel::ClassHOW.add_stash(Parameter);
    Perl6::Metamodel::ClassHOW.add_stash(Code);
    Perl6::Metamodel::ClassHOW.add_stash(Block);
    Perl6::Metamodel::ClassHOW.add_stash(Routine);
    Perl6::Metamodel::ClassHOW.add_stash(Sub);
    Perl6::Metamodel::ClassHOW.add_stash(Operator);
    Perl6::Metamodel::ClassHOW.add_stash(Method);
    Perl6::Metamodel::ClassHOW.add_stash(Submethod);
    Perl6::Metamodel::ClassHOW.add_stash(Regex);
    Perl6::Metamodel::ClassHOW.add_stash(Str);
    Perl6::Metamodel::ClassHOW.add_stash(Int);
    Perl6::Metamodel::ClassHOW.add_stash(Num);
    Perl6::Metamodel::NativeRefHOW.add_stash(IntLexRef);
    Perl6::Metamodel::NativeRefHOW.add_stash(UIntLexRef);
    Perl6::Metamodel::NativeRefHOW.add_stash(NumLexRef);
    Perl6::Metamodel::NativeRefHOW.add_stash(StrLexRef);
    Perl6::Metamodel::NativeRefHOW.add_stash(IntAttrRef);
    Perl6::Metamodel::NativeRefHOW.add_stash(UIntAttrRef);
    Perl6::Metamodel::NativeRefHOW.add_stash(NumAttrRef);
    Perl6::Metamodel::NativeRefHOW.add_stash(StrAttrRef);
    Perl6::Metamodel::NativeRefHOW.add_stash(IntPosRef);
    Perl6::Metamodel::NativeRefHOW.add_stash(UIntPosRef);
    Perl6::Metamodel::NativeRefHOW.add_stash(NumPosRef);
    Perl6::Metamodel::NativeRefHOW.add_stash(StrPosRef);
    Perl6::Metamodel::NativeRefHOW.add_stash(IntMultidimRef);
    Perl6::Metamodel::NativeRefHOW.add_stash(UIntMultidimRef);
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
    Perl6::Metamodel::ClassHOW.add_stash(array);
    Perl6::Metamodel::ClassHOW.add_stash(IterationBuffer);
    Perl6::Metamodel::ClassHOW.add_stash(Map);
    Perl6::Metamodel::ClassHOW.add_stash(Hash);
    Perl6::Metamodel::ClassHOW.add_stash(TypeEnv);
    Perl6::Metamodel::ClassHOW.add_stash(Capture);
    Perl6::Metamodel::EnumHOW.add_stash(Bool);
    Perl6::Metamodel::ClassHOW.add_stash(ObjAt);
    Perl6::Metamodel::ClassHOW.add_stash(ValueObjAt);
    Perl6::Metamodel::ClassHOW.add_stash(Stash);
    Perl6::Metamodel::ClassHOW.add_stash(Grammar);
    Perl6::Metamodel::ClassHOW.add_stash(Junction);
    Perl6::Metamodel::ClassHOW.add_stash(ForeignCode);
    Perl6::Metamodel::ClassHOW.add_stash(Version);

#?if !moar
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
#?endif

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
        nqp::bindkey(Metamodel.WHO, $_.key, $_.value);
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
    EXPORT::DEFAULT.WHO<Operator>   := Operator;
    EXPORT::DEFAULT.WHO<Method>     := Method;
    EXPORT::DEFAULT.WHO<Submethod>  := Submethod;
    EXPORT::DEFAULT.WHO<Regex>      := Regex;
    EXPORT::DEFAULT.WHO<Str>        := Str;
    EXPORT::DEFAULT.WHO<Int>        := Int;
    EXPORT::DEFAULT.WHO<Num>        := Num;
    EXPORT::DEFAULT.WHO<List>       := List;
    EXPORT::DEFAULT.WHO<Slip>       := Slip;
    EXPORT::DEFAULT.WHO<Array>      := Array;
    EXPORT::DEFAULT.WHO<array>      := array;
    EXPORT::DEFAULT.WHO<IterationBuffer> := IterationBuffer;
    EXPORT::DEFAULT.WHO<Map>        := Map;
    EXPORT::DEFAULT.WHO<Hash>       := Hash;
    EXPORT::DEFAULT.WHO<TypeEnv>    := TypeEnv;
    EXPORT::DEFAULT.WHO<Capture>    := Capture;
    EXPORT::DEFAULT.WHO<ObjAt>      := ObjAt;
    EXPORT::DEFAULT.WHO<ValueObjAt> := ValueObjAt;
    EXPORT::DEFAULT.WHO<Stash>      := Stash;
    EXPORT::DEFAULT.WHO<Scalar>     := Scalar;
    EXPORT::DEFAULT.WHO<ScalarVAR>  := ScalarVAR;
    EXPORT::DEFAULT.WHO<IntLexRef>  := IntLexRef;
    EXPORT::DEFAULT.WHO<UIntLexRef> := UIntLexRef;
    EXPORT::DEFAULT.WHO<NumLexRef>  := NumLexRef;
    EXPORT::DEFAULT.WHO<StrLexRef>  := StrLexRef;
    EXPORT::DEFAULT.WHO<IntAttrRef> := IntAttrRef;
    EXPORT::DEFAULT.WHO<UIntAttrRef> := UIntAttrRef;
    EXPORT::DEFAULT.WHO<NumAttrRef> := NumAttrRef;
    EXPORT::DEFAULT.WHO<StrAttrRef> := StrAttrRef;
    EXPORT::DEFAULT.WHO<IntPosRef>  := IntPosRef;
    EXPORT::DEFAULT.WHO<UIntPosRef> := UIntPosRef;
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
    EXPORT::DEFAULT.WHO<Version>             := Version;
}
EXPORT::DEFAULT.WHO<NQPMatchRole> := NQPMatchRole;
EXPORT::DEFAULT.WHO<NQPdidMATCH>  := NQPdidMATCH;

#?if !moar
# Set up various type mappings.
nqp::p6settypes(EXPORT::DEFAULT.WHO);
#?endif

# HLL configuration: interop, boxing and exit handling.
nqp::sethllconfig('Raku', nqp::hash(
    'int_box',          Int,
    'num_box',          Num,
    'str_box',          Str,
    'null_value',       Mu,
    'true_value',       (Bool.WHO)<True>,
    'false_value',      (Bool.WHO)<False>,
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
        unless nqp::p6inpre {

            # When we get here, we assume the $!phasers attribute is concrete.
            # if it is *not* a hash, it is a lone LEAVE phaser, the most
            # commonly used phaser (in the core at least).
            my $phasers := nqp::getattr(
              nqp::getcodeobj($coderef), Block, '$!phasers'
            );

            # slow path here
            if nqp::ishash($phasers) {
                my @exceptions;

                my @leaves := nqp::atkey($phasers, '!LEAVE-ORDER');
                unless nqp::isnull(@leaves) {
                    my $valid :=
                     nqp::hllizefor(nqp::decont($resultish),'Raku').defined;

                    my int $m := nqp::elems(@leaves);
                    my int $i;
                    while $i < $m {
                        CATCH { nqp::push(@exceptions, $_); ++$i }

                        # a KEEP/UNDO phaser
                        my $phaser := nqp::atpos(@leaves, $i);
                        if nqp::islist($phaser) {
                            my str $name := nqp::atpos($phaser, 0);
                            if ($name eq 'KEEP' && $valid)
                              || ($name eq 'UNDO' && !$valid) {
#?if jvm
                                nqp::atpos($phaser, 1)();
#?endif
#?if !jvm
                                nqp::p6capturelexwhere(
                                  nqp::atpos($phaser, 1).clone
                                )();
#?endif
                            }
                        }

                        # an ordinary LEAVE phaser
                        else {
#?if jvm
                            $phaser();
#?endif
#?if !jvm
                            nqp::p6capturelexwhere($phaser.clone)();
#?endif
                        }
                        ++$i;
                    }
                }

                my @posts := nqp::atkey($phasers, 'POST');
                unless nqp::isnull(@posts) {
                    my $value := nqp::ifnull($resultish,Mu);

                    my int $m := nqp::elems(@posts);
                    my int $i;
                    while $i < $m {
#?if jvm
                        nqp::atpos(@posts, $i)($value);
#?endif
#?if !jvm
                        nqp::p6capturelexwhere(
                          nqp::atpos(@posts, $i).clone
                        )($value);
#?endif
                        ++$i;
                    }
                }

                if nqp::elems(@exceptions) -> $nr_exceptions {
                    $nr_exceptions > 1
                      ?? Perl6::Metamodel::Configuration.throw_or_die(
                           'X::PhaserExceptions',
                           "Multiple exceptions were thrown by LEAVE phasers",
                           :@exceptions
                         )
                      !! nqp::rethrow(nqp::atpos(@exceptions, 0));
                }
            }

            # only have a lone LEAVE phaser, so no frills needed
            # don't bother to CATCH, there can only be one exception
            else {
#?if jvm
                $phasers();
#?endif
#?if !jvm
                nqp::p6capturelexwhere($phasers.clone)();
#?endif
            }
        }
    },
#?if !jvm

    'bind_error', -> $capture {

        # Get signature and lexpad.
        my $caller := nqp::getcodeobj(nqp::callercode());
        my $sig    := nqp::getattr($caller, Code, '$!signature');
        my $lexpad := nqp::ctxcaller(nqp::ctx);

        # Run full binder to produce an error.
        my @error;
        my int $bind_res := Binder.bind($capture, $sig, $lexpad, 0, @error);

        if $bind_res {

            # A Junction result
            if $bind_res == nqp::const::BIND_RESULT_JUNCTION {
                my @pos_args;
                my int $num_pos_args := nqp::captureposelems($capture);
                my int $i;
                while $i < $num_pos_args {
                    my $got_prim := nqp::captureposprimspec($capture, $i);
                    nqp::push(@pos_args, $got_prim
                      ?? $got_prim == nqp::const::BIND_VAL_STR
                        ?? nqp::box_s(nqp::captureposarg_s($capture, $i), Str)
                        !! $got_prim == nqp::const::BIND_VAL_INT
                          ?? nqp::box_i(
                               nqp::captureposarg_i($capture, $i), Int
                             )
                          !! $got_prim == nqp::const::BIND_VAL_UINT
                            ?? nqp::box_u(
                                 nqp::captureposarg_u($capture, $i), Int
                               )
                            !! nqp::box_n(  # got_prim == BIND_VAL_NUM
                                 nqp::captureposarg_n($capture, $i), Num
                               )
                      !! nqp::captureposarg($capture, $i)
                    );

                    ++$i;
                }
                my %named_args := nqp::capturenamedshash($capture);
                Junction.AUTOTHREAD($caller, |@pos_args, |%named_args);
            }
            else {
                my $error := nqp::atpos(@error, 0);
                nqp::isinvokable($error) ?? $error() !! nqp::die($error);
            }
        }
        else {
            nqp::die("Internal error: inconsistent bind result");
        }
    },

    'method_not_found_error', -> $obj, str $name, *@pos, *%named {
        my $class := nqp::getlexcaller('$?CLASS');
        my str $typename := $obj.HOW.name($obj);
        my $message :=
          "Method '$name' not found for invocant of class '$typename'";

        $name eq 'STORE'
          ?? Perl6::Metamodel::Configuration.throw_or_die(
               'X::Assignment::RO', $message, :value($obj)
             )
          !! Perl6::Metamodel::Configuration.throw_or_die(
               'X::Method::NotFound',
               $message,
               :invocant($obj),
               :method($name),
               :$typename,
               :private(nqp::hllboolfor(0, 'Raku')),
               :in-class-call(
                  nqp::hllboolfor(nqp::eqaddr(nqp::what($obj), $class), 'Raku')
               )
             );
    },
#?endif

    'lexical_handler_not_found_error', -> $cat, $out_of_dyn_scope {
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
        # Reinstate $*STACK-ID if invoked in a specilized finalization thread.
        # Preserve the current stack ID otherwise.
        my $*STACK-ID := nqp::ifnull(
          nqp::getlexreldyn(nqp::ctxcaller(nqp::ctx), '$*STACK-ID'),
          Perl6::Metamodel::Configuration.next_id
        );

        my int $m := nqp::elems(@objs);
        my int $i;
        while $i < $m {
            my $obj := nqp::atpos(@objs, $i);
            my @destroyers := $obj.HOW.destroyers($obj);

            my int $n := nqp::elems(@destroyers);
            my int $j;
            while $j < $n {
                nqp::atpos(@destroyers, $j)($obj);
                ++$j;
            }

            ++$i;
        }
    },

    'int_lex_ref',       IntLexRef,
    'uint_lex_ref',      UIntLexRef,
    'num_lex_ref',       NumLexRef,
    'str_lex_ref',       StrLexRef,
    'int_attr_ref',      IntAttrRef,
    'uint_attr_ref',     UIntAttrRef,
    'num_attr_ref',      NumAttrRef,
    'str_attr_ref',      StrAttrRef,
    'int_pos_ref',       IntPosRef,
    'uint_pos_ref',      UIntPosRef,
    'num_pos_ref',       NumPosRef,
    'str_pos_ref',       StrPosRef,
    'int_multidim_ref',  IntMultidimRef,
    'uint_multidim_ref', UIntMultidimRef,
    'num_multidim_ref',  NumMultidimRef,
    'str_multidim_ref',  StrMultidimRef,
#?if js
    'int64_lex_ref',      Int64LexRef,
    'int64_attr_ref',     Int64AttrRef,
    'int64_pos_ref',      Int64PosRef,
    'int64_multidim_ref', Int64MultidimRef,
#?endif

#?if moar
    'call_dispatcher',         'raku-call',
    'method_call_dispatcher',  'raku-meth-call',
    'find_method_dispatcher',  'raku-find-meth',
    'resume_error_dispatcher', 'raku-resume-error',
    'hllize_dispatcher',       'raku-hllize',
    # Can write a Raku one later for more opts
    'istype_dispatcher',       'nqp-istype',
    'isinvokable_dispatcher',  'raku-isinvokable',
    'max_inline_size',         384,
#?endif
));

#?if moar
my @types_for_hll_role := nqp::list(Mu, Int, Num, Str, List, Hash, ForeignCode);
my @transform_type := nqp::list(
    Mu,
    -> $int { nqp::box_i($int, Int) },
    -> $num { nqp::box_n($num, Num) },
    -> $str { nqp::box_s($str, Str) },
    -> $farray {
        my $list := nqp::create(List);
        nqp::bindattr($list, List, '$!reified', $farray);
        $list
    },
    -> $hash {
        my $result := nqp::create(Hash);
        nqp::bindattr($result, Map, '$!storage', $hash);
        $result
    },
    -> $code {
        my $result := nqp::create(ForeignCode);
        nqp::bindattr($result, ForeignCode, '$!do', $code);
        $result
    },
    -> $uint { nqp::box_u($uint, Int) },
);

nqp::register('raku-hllize', -> $capture {

    my $arg := nqp::track('arg', $capture, 0);
    nqp::guard('type', $arg);
    nqp::guard('concreteness', $arg);

    my int $got_prim := nqp::captureposprimspec($capture, 0);

    # Got a native value
    if $got_prim {
        nqp::delegate('lang-call',
          nqp::syscall('dispatcher-insert-arg-literal-obj',
            $capture,
            0,
            nqp::atpos(
              @transform_type,  # XXX use a translation table
              $got_prim == nqp::const::BIND_VAL_UINT
                ?? 7
                !! $got_prim > nqp::const::BIND_VAL_STR
                  ?? 1
                  !! $got_prim
            )
          )
        );
    }

    # Not a native
    else {
        my $obj := nqp::captureposarg($capture, 0);

        if nqp::isnull($obj) {
            nqp::delegate('boot-constant',
              nqp::syscall('dispatcher-insert-arg-literal-obj',
                nqp::syscall('dispatcher-drop-arg', $capture, 0),
                0,
                Mu
              )
            );
        }
        else {
            my $role := nqp::gettypehllrole($obj);
            if $role > 0 {
                if nqp::isconcrete($obj) {
                    nqp::delegate('lang-call',
                      nqp::syscall('dispatcher-insert-arg-literal-obj',
                        $capture, 0, nqp::atpos(@transform_type, $role)
                      )
                    );
                }
                else {
                    nqp::delegate('boot-constant',
                      nqp::syscall('dispatcher-insert-arg-literal-obj',
                        nqp::syscall('dispatcher-drop-arg', $capture, 0),
                        0,
                        nqp::atpos(@types_for_hll_role, $role)
                      )
                    );
                }
            }
            else {
                nqp::delegate('boot-value', $capture);
            }
        }
    }
});
#?endif

# Tell parametric role groups how to create a dispatcher.
Perl6::Metamodel::ParametricRoleGroupHOW.set_selector_creator({
    my $sel := nqp::create(Sub);
#?if moar
    my $onlystar := sub (*@pos, *%named) {
        nqp::dispatch('boot-resume', nqp::const::DISP_ONLYSTAR)
    };
#?endif
#?if !moar
    my $onlystar := sub (*@pos, *%named) {
        nqp::invokewithcapture(
            nqp::getcodeobj(nqp::curcode()).find_best_dispatchee(nqp::usecapture()),
            nqp::usecapture())
    };
#?endif
    nqp::setcodeobj($onlystar, $sel);
    nqp::bindattr($sel, Code, '$!do', $onlystar);
    nqp::bindattr($sel, Routine, '@!dispatchees', nqp::list);
    $sel
});

# Roles pretend to be narrower than certain types for the purpose
# of type checking. Also, they pun to classes.

# Lookup of methods and the type object they're expected to be found in
# for these roles
my %excluded := nqp::hash(
  'ACCEPTS',        Mu,
  'Bool',           Mu,
  'defined',        Mu,
  'dispatch:<.?>',  Mu,
  'dispatch:<.^>',  Mu,
  'dispatch:<.=>',  Mu,
  'dispatch:<var>', Mu,
  'DUMP',           Mu,
  'gist',           Mu,
  'item',           Mu,
  'not',            Mu,
  'note',           Mu,
  'Numeric',        Mu,
  'perl',           Mu,
  'print',          Mu,
  'put',            Mu,
  'raku',           Mu,
  'Real',           Mu,
  'say',            Mu,
  'set_why',        Mu,
  'sink',           Mu,
  'so',             Mu,
  'Str',            Mu,
  'Stringy',        Mu,
  'WHERE',          Mu,
  'WHICH',          Mu,
  'WHY',            Mu,
);

Perl6::Metamodel::ParametricRoleGroupHOW.pretend_to_be(
  nqp::list(Cool,Any,Mu)
);
Perl6::Metamodel::ParametricRoleGroupHOW.configure_punning(
   Perl6::Metamodel::ClassHOW, %excluded
);

Perl6::Metamodel::ParametricRoleHOW.pretend_to_be(
  nqp::list(Cool, Any, Mu)
);
Perl6::Metamodel::ParametricRoleHOW.configure_punning(
  Perl6::Metamodel::ClassHOW, %excluded
);

Perl6::Metamodel::CurriedRoleHOW.pretend_to_be(
  nqp::list(Cool, Any, Mu)
);
Perl6::Metamodel::CurriedRoleHOW.configure_punning(
  Perl6::Metamodel::ClassHOW, %excluded
);

# Similar for packages and modules, but just has methods from Any.
Perl6::Metamodel::PackageHOW.pretend_to_be(
  nqp::list(Any, Mu)
);
Perl6::Metamodel::PackageHOW.delegate_methods_to(Any);

Perl6::Metamodel::ModuleHOW.pretend_to_be(
  nqp::list(Any, Mu)
);
Perl6::Metamodel::ModuleHOW.delegate_methods_to(Any);

#?if !moar
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
#?endif

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
nqp::bindhllsym('Raku', 'ScalarVAR', ScalarVAR);
nqp::bindhllsym('Raku', 'default_cont_spec',
    Scalar.HOW.cache_get(Scalar, 'default_cont_spec'));
nqp::bindhllsym('Raku', 'Capture', Capture);
nqp::bindhllsym('Raku', 'Version', Version);

#?if jvm
# On JVM, set up JVM interop bits.
nqp::gethllsym('Raku', 'JavaModuleLoader').set_interop_loader(-> {
    nqp::jvmrakudointerop()
});
Perl6::Metamodel::JavaHOW.pretend_to_be([Any, Mu]);
#?endif

# vim: expandtab sw=4
