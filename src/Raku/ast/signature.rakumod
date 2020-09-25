# A signature, typically part of a block though also contained within a
# signature literal or a signature-based variable declarator.
class RakuAST::Signature is RakuAST::Meta is RakuAST::Attaching {
    has List $.parameters;
    has int $!is-on-method;
    has RakuAST::Package $!method-package;
    has RakuAST::Parameter $!implicit-invocant;
    has RakuAST::Parameter $!implicit-slurpy-hash;

    method new(List :$parameters) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Signature, '$!parameters',
            self.IMPL-WRAP-LIST($parameters // []));
        $obj
    }

    method attach(RakuAST::Resolver $resolver) {
        # If we're the signature for a method...
        my $owner := $resolver.find-attach-target('block');
        if nqp::istype($owner, RakuAST::Method) {
            # Stash away the fact we should generate implicit parameters, and
            # also retrieve the enclosing package so we can set an implicit
            # invocant parameter up correctly.
            nqp::bindattr_i(self, RakuAST::Signature, '$!is-on-method', 1);
            nqp::bindattr(self, RakuAST::Signature, '$!method-package',
                $resolver.find-attach-target('package'));
        }
        else {
            nqp::bindattr_i(self, RakuAST::Signature, '$!is-on-method', 0);
        }
    }

    method IMPL-ENSURE-IMPLICITS() {
        if $!is-on-method && !($!implicit-invocant || $!implicit-slurpy-hash) {
            my @param-asts := self.IMPL-UNWRAP-LIST($!parameters);
            unless @param-asts && @param-asts[0].invocant {
                # TODO set type of this
                nqp::bindattr(self, RakuAST::Signature, '$!implicit-invocant',
                    RakuAST::Parameter.new(:invocant));
            }
            # TODO implicit slurpy hash
        }
    }

    method PRODUCE-META-OBJECT() {
        # Produce meta-objects for each parameter.
        self.IMPL-ENSURE-IMPLICITS();
        my @parameters;
        if $!implicit-invocant {
            @parameters.push($!implicit-invocant.meta-object);
        }
        for self.IMPL-UNWRAP-LIST($!parameters) {
            @parameters.push($_.meta-object);
        }
        if $!implicit-slurpy-hash {
            @parameters.push($!implicit-slurpy-hash.meta-object);
        }

        # Build signature object.
        my $signature := nqp::create(Signature);
        nqp::bindattr($signature, Signature, '@!params', @parameters);
        nqp::bindattr_i($signature, Signature, '$!arity', self.arity);
        nqp::bindattr($signature, Signature, '$!count', self.count);
        $signature
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        self.IMPL-ENSURE-IMPLICITS();
        my $bindings := QAST::Stmts.new();
        if $!implicit-invocant {
            $bindings.push($!implicit-invocant.IMPL-TO-QAST($context));
        }
        for self.IMPL-UNWRAP-LIST($!parameters) {
            $bindings.push($_.IMPL-TO-QAST($context));
        }
        if $!implicit-slurpy-hash {
            $bindings.push($!implicit-slurpy-hash.IMPL-TO-QAST($context));
        }
        $bindings
    }

    method arity() {
        my int $arity := 0;
        for self.IMPL-UNWRAP-LIST($!parameters) {
            last unless $_.is-positional && !$_.optional;
            $arity++;
        }
        nqp::box_i($arity, Int)
    }

    method count() {
        my int $count := 0;
        for self.IMPL-UNWRAP-LIST($!parameters) {
            if $_.is-positional {
                $count++;
            }
            elsif !($_.slurpy =:= RakuAST::Parameter::Slurpy) && $_.target.sigil ne '%' {
                return nqp::box_n(nqp::inf, Num);
            }
        }
        nqp::box_i($count, Int)
    }

    method visit-children(Code $visitor) {
        for self.IMPL-UNWRAP-LIST($!parameters) {
            $visitor($_);
        }
    }
}

# A parameter within a signature. A parameter may result in binding or assignment
# into a target; this is modeled by a RakuAST::ParameterTarget, which is optional.
class RakuAST::Parameter is RakuAST::Meta is RakuAST::Attaching {
    has RakuAST::Type $.type;
    has int $!default-to-any;
    has RakuAST::ParameterTarget $.target;
    has Mu $!names;
    has Bool $.invocant;
    has Bool $.optional;
    has RakuAST::Parameter::Slurpy $.slurpy;

    method new(RakuAST::Type :$type, RakuAST::ParameterTarget :$target,
            List :$names, Bool :$invocant, Bool :$optional,
            RakuAST::Parameter::Slurpy :$slurpy) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Parameter, '$!type', $type // RakuAST::Type);
        nqp::bindattr($obj, RakuAST::Parameter, '$!target', $target // RakuAST::ParameterTarget);
        nqp::bindattr($obj, RakuAST::Parameter, '$!names', self.IMPL-NAMES($names));
        nqp::bindattr($obj, RakuAST::Parameter, '$!invocant', $invocant ?? True !! False);
        nqp::bindattr($obj, RakuAST::Parameter, '$!optional', $optional ?? True !! False);
        nqp::bindattr($obj, RakuAST::Parameter, '$!slurpy',
            nqp::istype($slurpy, RakuAST::Parameter::Slurpy)
                ?? $slurpy
                !! RakuAST::Parameter::Slurpy);
        $obj
    }

    method set-type(RakuAST::Type $type) {
        nqp::bindattr(self, RakuAST::Parameter, '$!type', $type);
        Nil
    }

    method set-invocant(Bool $invocant) {
        nqp::bindattr(self, RakuAST::Parameter, '$!invocant', $invocant ?? True !! False);
        Nil
    }

    method set-optional(Bool $optional) {
        nqp::bindattr(self, RakuAST::Parameter, '$!optional', $optional ?? True !! False);
        Nil
    }

    method set-slurpy(RakuAST::Parameter::Slurpy $slurpy) {
        nqp::bindattr(self, RakuAST::Parameter, '$!slurpy', $slurpy);
        Nil
    }

    method set-names(List $names) {
        nqp::bindattr(self, RakuAST::Parameter, '$!names', self.IMPL-NAMES($names));
        Nil
    }

    method add-name(Str $name) {
        nqp::push($!names, $name);
        Nil
    }

    method names() {
        self.IMPL-WRAP-LIST($!names)
    }

    # Tests if the parameter is a simple positional parameter.
    method is-positional() {
        $!names || !($!slurpy =:= RakuAST::Parameter::Slurpy) ?? False !! True
    }

    method IMPL-NAMES(Mu $names) {
        my @names;
        if $names {
            for self.IMPL-UNWRAP-LIST($names) {
                if nqp::isstr($_) || nqp::istype($_, Str) {
                    @names.push($_);
                }
                else {
                    nqp::die('Parameter names list must be a list of Str');
                }
            }
        }
        @names
    }

    method attach(RakuAST::Resolver $resolver) {
        # If we're on a routine, then the default nominal type will be Any.
        my $owner := $resolver.find-attach-target('block');
        nqp::bindattr_i(self, RakuAST::Parameter, '$!default-to-any',
            nqp::istype($owner, RakuAST::Routine));
    }

    method visit-children(Code $visitor) {
        $visitor($!type) if $!type;
        $visitor($!target) if $!target;
    }

    method PRODUCE-META-OBJECT() {
        my $parameter := nqp::create(Parameter);
        if $!target {
            nqp::bindattr_s($parameter, Parameter, '$!variable_name',
                $!target.introspection-name);
        }
        if $!names {
            my $names-str-list := nqp::list_s();
            for $!names {
                nqp::push_s($names-str-list, $_);
            }
            nqp::bindattr($parameter, Parameter, '@!named_names', $names-str-list);
        }
        nqp::bindattr_i($parameter, Parameter, '$!flags', self.IMPL-FLAGS());
        nqp::bindattr($parameter, Parameter, '$!nominal_type', self.IMPL-NOMINAL-TYPE());
        # TODO further setup
        $parameter
    }

    method IMPL-FLAGS() {
        my constant SIG_ELEM_INVOCANT            := 64;
        my constant SIG_ELEM_IS_OPTIONAL         := 2048;
        my constant SIG_ELEM_ARRAY_SIGIL         := 4096;
        my constant SIG_ELEM_HASH_SIGIL          := 8192;
        my constant SIG_ELEM_CODE_SIGIL          := 33554432;
        my $sigil := $!target.sigil;
        my int $flags;
        $flags := $flags + SIG_ELEM_INVOCANT if $!invocant;
        $flags := $flags + SIG_ELEM_IS_OPTIONAL if $!optional;
        if $sigil eq '@' {
            $flags := $flags + SIG_ELEM_ARRAY_SIGIL;
        }
        elsif $sigil eq '%' {
            $flags := $flags + SIG_ELEM_HASH_SIGIL;
        }
        elsif $sigil eq '&' {
            $flags := $flags + SIG_ELEM_CODE_SIGIL;
        }
        $flags := $flags + $!slurpy.IMPL-FLAGS($sigil);
        $flags
    }

    method IMPL-NOMINAL-TYPE() {
        if $!type {
            $!type.resolution.compile-time-value
        }
        else {
            $!default-to-any ?? Any !! Mu
        }
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        # Get the parameter meta-object, since traits can change some things.
        my $param-obj := self.meta-object;

        # Take the parameter into a temporary local.
        my $name := QAST::Node.unique("__lowered_param");
        my $param-qast := QAST::Var.new( :decl('param'), :scope('local'), :$name );
        my $temp-qast := QAST::Var.new( :name($name), :scope('local') );

        # Deal with names.
        my int $was-slurpy;
        if $!names {
            $param-qast.named(nqp::elems($!names) == 1 ?? $!names[0] !! $!names);
        }
        elsif !($!slurpy =:= RakuAST::Parameter::Slurpy) {
            $!slurpy.IMPL-TRANSFORM-PARAM-QAST($context, $param-qast, $temp-qast,
                $!target.sigil);
            $was-slurpy := 1;
        }

        # HLLize before type checking unless it was a slurpy (in which
        # case we know full well what we produced).
        # TODO decont does not belong here
        unless $was-slurpy {
            $param-qast.push(QAST::Op.new(
                :op('bind'),
                $temp-qast,
                QAST::Op.new( :op('decont'), QAST::Op.new( :op('hllize'), $temp-qast ) )
            ));
        }

        # Do type checks.
        # TODO really more involved than this
        # TODO decont handling probably needs a tweak
        my $nominal-type := nqp::getattr($param-obj, Parameter, '$!nominal_type');
        unless $nominal-type =:= Mu {
            $context.ensure-sc($nominal-type);
            $param-qast.push(QAST::ParamTypeCheck.new(QAST::Op.new(
                :op('istype_nd'),
                $temp-qast,
                QAST::WVal.new( :value($nominal-type) )
            )));
        }

        # If it's optional, do any default handling.
        if $!optional {
            # TODO default value, non-Scalar sigil, etc.
            $param-qast.default(QAST::WVal.new( :value($nominal-type) ));
        }

        # Bind parameter into its target.
        if self.invocant {
            $param-qast.push(QAST::Op.new(
                :op('bind'),
                QAST::Var.new( :name('self'), :scope('lexical') ),
                $temp-qast
            ));
        }
        if nqp::isconcrete($!target) {
            $param-qast.push($!target.IMPL-BIND-QAST($context, $temp-qast));
        }

        $param-qast
    }
}

# A parameter target is a symbol that a parameter is bound into. A parameter
# need not be bound into anything (it may be being used only as a matcher, or
# destructured). This serves primarily as a marker for the different kinds of
# parameter target.
class RakuAST::ParameterTarget is RakuAST::Node {
    method sigil() { '' }
}

# A binding of a parameter into a lexical variable (with sigil).
class RakuAST::ParameterTarget::Var is RakuAST::ParameterTarget is RakuAST::Declaration {
    has str $.name;

    method new(str $name!) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::ParameterTarget::Var, '$!name', $name);
        $obj
    }

    method lexical-name() {
        $!name
    }

    method introspection-name() {
        $!name
    }

    # Generate a lookup of this parameter, already resolved to this declaration.
    method generate-lookup() {
        my $lookup := RakuAST::Var::Lexical.new($!name);
        $lookup.set-resolution(self);
        $lookup
    }

    method sigil() {
        nqp::substr($!name, 0, 1)
    }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        QAST::Var.new( :decl('var'), :scope('lexical'), :name($!name) )
    }

    method IMPL-BIND-QAST(RakuAST::IMPL::QASTContext $context, Mu $source-qast) {
        QAST::Op.new(
            :op('bind'),
            QAST::Var.new( :scope('lexical'), :name($!name) ),
            $source-qast
        )
    }

    method IMPL-LOOKUP-QAST(RakuAST::IMPL::QASTContext $context) {
        my str $scope := 'lexical';
        QAST::Var.new( :name($!name), :$scope )
    }

    method default-scope() { 'my' }

    method allowed-scopes() { self.IMPL-WRAP-LIST(['my']) }
}

# Marker for all kinds of slurpy behavior.
class RakuAST::Parameter::Slurpy {
    method IMPL-FLAGS(str $sigil) {
        # Not slurpy, so no flags
        0
    }

    method IMPL-TRANSFORM-PARAM-QAST(RakuAST::IMPL::QASTContext $context,
            Mu $param-qast, Mu $temp-qast, str $sigil) {
        # Not slurply, so nothing to do
        $param-qast
    }

    method IMPL-QAST-LISTY-SLURP(Mu $param-qast, Mu $temp-qast, List $type, str $method) {
        $param-qast.slurpy(1);
        $param-qast.push(QAST::Op.new(
            :op('bind'),
            $temp-qast,
            QAST::Op.new(
                :op('callmethod'), :name($method),
                QAST::WVal.new( :value($type) ),
                $temp-qast
            )
        ));
    }
}

# Flattening slurpy (the * quantifier).
class RakuAST::Parameter::Slurpy::Flattened is RakuAST::Parameter::Slurpy {
    method IMPL-FLAGS(str $sigil) {
        my constant SIG_ELEM_SLURPY_POS   := 8;
        my constant SIG_ELEM_SLURPY_NAMED := 16;
        $sigil eq '@' ?? SIG_ELEM_SLURPY_POS !!
        $sigil eq '%' ?? SIG_ELEM_SLURPY_NAMED !!
                         0
    }

    method IMPL-TRANSFORM-PARAM-QAST(RakuAST::IMPL::QASTContext $context,
            Mu $param-qast, Mu $temp-qast, str $sigil) {
        if $sigil eq '@' {
            self.IMPL-QAST-LISTY-SLURP($param-qast, $temp-qast, Array, 'from-slurpy-flat');
        }
        elsif $sigil eq '%' {
            $param-qast.slurpy(1);
            $param-qast.named(1);
            $param-qast.push(QAST::Op.new(
                :op('bind'),
                $temp-qast,
                QAST::Op.new(
                    :op('p6bindattrinvres'),
                    QAST::Op.new(
                        :op('create'),
                        QAST::WVal.new( :value(Hash) )
                    ),
                    QAST::WVal.new( :value(Map) ),
                    QAST::SVal.new( :value('$!storage') ),
                    $temp-qast
                )
            ));
        }
        else {
            nqp::die("Parameter * quantifier not applicable to sigil '$sigil'");
        }
    }
}

# Non-flattening slurpy (the ** quantifier).
class RakuAST::Parameter::Slurpy::Unflattened is RakuAST::Parameter::Slurpy {
    method IMPL-FLAGS(str $sigil) {
        my constant SIG_ELEM_SLURPY_LOL := 32;
        $sigil eq '@' ?? SIG_ELEM_SLURPY_LOL !! 0
    }

    method IMPL-TRANSFORM-PARAM-QAST(RakuAST::IMPL::QASTContext $context,
            Mu $param-qast, Mu $temp-qast, str $sigil) {
        if $sigil eq '@' {
            self.IMPL-QAST-LISTY-SLURP($param-qast, $temp-qast, Array, 'from-slurpy');
        }
        else {
            nqp::die("Parameter ** quantifier not applicable to sigil '$sigil'");
        }
    }
}

# Single argument rule slurpy (the + quantifier).
class RakuAST::Parameter::Slurpy::SingleArgument is RakuAST::Parameter::Slurpy {
    method IMPL-FLAGS(str $sigil) {
        my constant SIG_ELEM_SLURPY_ONEARG := 16777216;
        $sigil eq '@' || $sigil eq '' ?? SIG_ELEM_SLURPY_ONEARG !! 0
    }

    method IMPL-TRANSFORM-PARAM-QAST(RakuAST::IMPL::QASTContext $context,
            Mu $param-qast, Mu $temp-qast, str $sigil) {
        if $sigil eq '@' || $sigil eq '' {
            self.IMPL-QAST-LISTY-SLURP($param-qast, $temp-qast, Array, 'from-slurpy-onearg');
        }
        else {
            nqp::die("Parameter + quantifier not applicable to sigil '$sigil'");
        }
    }
}
