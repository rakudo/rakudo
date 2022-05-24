# A signature, typically part of a block though also contained within a
# signature literal or a signature-based variable declarator.
class RakuAST::Signature is RakuAST::Meta is RakuAST::Attaching {
    has List $.parameters;
    has RakuAST::Node $.returns;
    has int $!is-on-method;
    has RakuAST::Package $!method-package;
    has RakuAST::Parameter $!implicit-invocant;
    has RakuAST::Parameter $!implicit-slurpy-hash;

    method new(List :$parameters, RakuAST::Node :$returns) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Signature, '$!parameters',
            self.IMPL-WRAP-LIST($parameters // []));
        nqp::bindattr($obj, RakuAST::Signature, '$!returns', $returns // RakuAST::Node);
        nqp::bindattr_i($obj, RakuAST::Signature, '$!is-on-method', 0);
        $obj
    }

    method attach(RakuAST::Resolver $resolver) {
        # If we're the signature for a method...
        if $!is-on-method {
            # ... retrieve the enclosing package so we can set an implicit
            # invocant parameter up correctly.
            nqp::bindattr(self, RakuAST::Signature, '$!method-package',
                $resolver.find-attach-target('package'));
        }
    }

    method set-is-on-method(Bool $is-on-method) {
        # Stash away the fact whether we should generate implicit parameters
        nqp::bindattr_i(self, RakuAST::Signature, '$!is-on-method', $is-on-method ?? 1 !! 0);
    }

    method provides-return-value() {
        if $!returns {
            my $value := self.IMPL-RETURN-VALUE();
            nqp::isconcrete($value) || nqp::eqaddr($value, Nil) ?? True !! False
        }
        else {
            False
        }
    }

    method IMPL-ENSURE-IMPLICITS() {
        if $!is-on-method && !($!implicit-invocant || $!implicit-slurpy-hash) {
            my @param-asts := self.IMPL-UNWRAP-LIST($!parameters);
            unless @param-asts && @param-asts[0].invocant {
                # TODO set type of this
                my $type;
                if nqp::isconcrete($!method-package) {
                    my $package := $!method-package.stubbed-meta-object;
                    my $package-name := $package.HOW.name($package);
                    $type := RakuAST::Type::Simple.new(RakuAST::Name.from-identifier($package-name));
                    $type.set-resolution(RakuAST::VarDeclaration::Implicit::Constant.new(:name($package-name), :value($package), :scope<lexical>));
                }
                nqp::bindattr(self, RakuAST::Signature, '$!implicit-invocant',
                    RakuAST::Parameter.new(:invocant, :$type));
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

        # Figure out and set return type.
        nqp::bindattr($signature, Signature, '$!returns', $!returns
            ?? self.IMPL-RETURN-VALUE()
            !! nqp::null());

        $signature
    }

    method IMPL-RETURN-VALUE() {
        if nqp::istype($!returns, RakuAST::Type) {
            $!returns.resolution.compile-time-value
        }
        elsif nqp::istype($!returns, RakuAST::CompileTimeValue) {
            $!returns.compile-time-value
        }
        else {
            nqp::die('--> return constraint must be a type or a constant value');
        }
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context, :$needs-full-binder) {
        self.IMPL-ENSURE-IMPLICITS();
        my $bindings := QAST::Stmts.new();
        my $parameters := self.IMPL-UNWRAP-LIST($!parameters);
        if $needs-full-binder {
            $bindings.push(QAST::Op.new(
                :op('if'),
                QAST::Op.new(
                    :op('dispatch'),
                    QAST::SVal.new( :value('boot-syscall') ),
                    QAST::SVal.new( :value('bind-will-resume-on-failure') )
                ),
                QAST::Op.new(
                    :op('assertparamcheck'),
                    QAST::Op.new( :op('p6trybindsig') )
                ),
                QAST::Op.new( :op('p6bindsig') )
            ));
        }
        else {
            if $!implicit-invocant {
                $bindings.push($!implicit-invocant.IMPL-TO-QAST($context));
            }
            for $parameters {
                $bindings.push($_.IMPL-TO-QAST($context));
            }
            if $!implicit-slurpy-hash {
                $bindings.push($!implicit-slurpy-hash.IMPL-TO-QAST($context));
            }
        }
        $bindings
    }

    method set-default-type(RakuAST::Type $type) {
        for self.IMPL-UNWRAP-LIST($!parameters) {
            $_.set-default-type($type);
        }
    }

    method arity() {
        my int $arity := 0;
        for self.IMPL-UNWRAP-LIST($!parameters) {
            last unless $_.is-positional && !$_.is-optional;
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
        $visitor($!returns) if $!returns;
    }
}

# A parameter within a signature. A parameter may result in binding or assignment
# into a target; this is modeled by a RakuAST::ParameterTarget, which is optional.
class RakuAST::Parameter is RakuAST::Meta is RakuAST::Attaching
                         is RakuAST::ImplicitLookups is RakuAST::TraitTarget
                         is RakuAST::BeginTime is RakuAST::CheckTime {
    has RakuAST::Type $.type;
    has RakuAST::ParameterTarget $.target;
    has Mu $!names;
    has Bool $.invocant;
    has Bool $!optional;
    has RakuAST::Parameter::Slurpy $.slurpy;
    has RakuAST::Expression $.default;
    has RakuAST::Expression $.where;
    has RakuAST::Node $!owner;
    has RakuAST::Signature $.sub-signature;

    method new(RakuAST::Type :$type, RakuAST::ParameterTarget :$target,
            List :$names, Bool :$invocant, Bool :$optional,
            RakuAST::Parameter::Slurpy :$slurpy, List :$traits,
            RakuAST::Expression :$default, RakuAST::Expression :$where,
            RakuAST::Signature :$sub-signature) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Parameter, '$!type', $type // RakuAST::Type);
        nqp::bindattr($obj, RakuAST::Parameter, '$!target', $target // RakuAST::ParameterTarget);
        nqp::bindattr($obj, RakuAST::Parameter, '$!names', self.IMPL-NAMES($names));
        nqp::bindattr($obj, RakuAST::Parameter, '$!invocant', $invocant ?? True !! False);
        nqp::bindattr($obj, RakuAST::Parameter, '$!optional', nqp::defined($optional)
            ?? ($optional ?? True !! False)
            !! Bool);
        nqp::bindattr($obj, RakuAST::Parameter, '$!slurpy',
            nqp::istype($slurpy, RakuAST::Parameter::Slurpy)
                ?? $slurpy
                !! RakuAST::Parameter::Slurpy);
        $obj.set-traits($traits);
        nqp::bindattr($obj, RakuAST::Parameter, '$!default', $default // RakuAST::Expression);
        nqp::bindattr($obj, RakuAST::Parameter, '$!where', $where // RakuAST::Expression);
        nqp::bindattr($obj, RakuAST::Parameter, '$!sub-signature', $sub-signature // RakuAST::Signature);
        $obj
    }

    method set-type(RakuAST::Type $type) {
        nqp::bindattr(self, RakuAST::Parameter, '$!type', $type);
        Nil
    }

    method set-default-type(RakuAST::Type $type) {
        my str $sigil := $!target.sigil;
        unless $sigil eq '@' || $sigil eq '%' || $sigil eq '&' {
            nqp::bindattr(self, RakuAST::Parameter, '$!type', $type)
                unless $!type;
        }
        Nil
    }

    method set-invocant(Bool $invocant) {
        nqp::bindattr(self, RakuAST::Parameter, '$!invocant', $invocant ?? True !! False);
        Nil
    }

    method set-optional() {
        nqp::bindattr(self, RakuAST::Parameter, '$!optional', True);
        Nil
    }

    method set-required() {
        nqp::bindattr(self, RakuAST::Parameter, '$!optional', False);
        Nil
    }

    method clear-optionality() {
        nqp::bindattr(self, RakuAST::Parameter, '$!optional', Bool);
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

    method set-default(RakuAST::Expression $default) {
        nqp::bindattr(self, RakuAST::Parameter, '$!default', $default);
        Nil
    }

    method set-where(RakuAST::Expression $where) {
        nqp::bindattr(self, RakuAST::Parameter, '$!where', $where);
        Nil
    }

    # Tests if the parameter is a simple positional parameter.
    method is-positional() {
        $!names || !($!slurpy =:= RakuAST::Parameter::Slurpy) ?? False !! True
    }

    # Tests if the parameter has been explicitly marked optional.
    method is-declared-optional() {
        nqp::eqaddr($!optional, True)
    }

    # Tests if the parameter has been explicitly marked required.
    method is-declared-required() {
        nqp::eqaddr($!optional, False)
    }

    # Tests if the parameter is optional, which may be because it was
    # declared that way, or alternatively may be because it has a
    # default value or is named.
    method is-optional() {
        $!optional // ($!default || $!names ?? True !! False)
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
        nqp::bindattr(self, RakuAST::Parameter, '$!owner',
            $resolver.find-attach-target('block'));
    }

    method visit-children(Code $visitor) {
        $visitor($!type) if $!type;
        $visitor($!target) if $!target;
        $visitor($!default) if $!default;
        $visitor($!where) if $!where;
        $visitor($!sub-signature) if $!sub-signature;
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        my str $sigil := $!target.sigil;
        my str $sigil-type;
        if $sigil eq '@' { $sigil-type := 'Positional' }
        elsif $sigil eq '%' { $sigil-type := 'Associative' }
        elsif $sigil eq '&' { $sigil-type := 'Callable' }
        else { $sigil-type := '' }
        self.IMPL-WRAP-LIST($sigil-type
            ?? [RakuAST::Type::Setting.new(RakuAST::Name.from-identifier($sigil-type))]
            !! [])
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
        my $type := self.IMPL-NOMINAL-TYPE();
        nqp::bindattr($parameter, Parameter, '$!type', $type);
        if $!target {
            my $name := $!target.introspection-name;
            my $cd := ContainerDescriptor.new(:of($type), :$name, :default($type), :dynamic(0));
            nqp::bindattr($parameter, Parameter, '$!container_descriptor', $cd);
        }
        if $!where {
            nqp::bindattr($parameter, Parameter, '@!post_constraints', nqp::list($!where.meta-object));
        }
        if $!sub-signature {
            nqp::bindattr($parameter, Parameter, '$!sub_signature', $!sub-signature.meta-object);
        }
        # TODO further setup
        $parameter
    }

    method IMPL-FLAGS() {
        my constant SIG_ELEM_INVOCANT            := 64;
        my constant SIG_ELEM_MULTI_INVOCANT      := 128;
        my constant SIG_ELEM_IS_RAW              := 1024;
        my constant SIG_ELEM_IS_OPTIONAL         := 2048;
        my constant SIG_ELEM_ARRAY_SIGIL         := 4096;
        my constant SIG_ELEM_HASH_SIGIL          := 8192;
        my constant SIG_ELEM_CODE_SIGIL          := 33554432;
        my $sigil := $!target.sigil;
        my int $flags;
        $flags := $flags +| SIG_ELEM_INVOCANT if $!invocant;
        $flags := $flags +| SIG_ELEM_MULTI_INVOCANT;
        $flags := $flags +| SIG_ELEM_IS_OPTIONAL if self.is-optional;
        if $sigil eq '@' {
            $flags := $flags +| SIG_ELEM_ARRAY_SIGIL;
        }
        elsif $sigil eq '%' {
            $flags := $flags +| SIG_ELEM_HASH_SIGIL;
        }
        elsif $sigil eq '&' {
            $flags := $flags +| SIG_ELEM_CODE_SIGIL;
        }
        if nqp::istype($!target, RakuAST::ParameterTarget::Term) {
            $flags := $flags +| SIG_ELEM_IS_RAW;
        }
        $flags := $flags +| $!slurpy.IMPL-FLAGS($sigil);
        $flags
    }

    method IMPL-NOMINAL-TYPE() {
        my str $sigil := $!target.sigil;
        if $sigil eq '@' || $sigil eq '%' || $sigil eq '&' {
            my @lookups := self.IMPL-UNWRAP-LIST(self.get-implicit-lookups());
            my $sigil-type := @lookups[0].resolution.compile-time-value;
            $!type
                ?? $sigil-type.HOW.parameterize($sigil-type,
                        $!type.resolution.compile-time-value)
                !! $sigil-type
        }
        else {
            $!type
                ?? $!type.meta-object
                !! Mu
        }
    }

    method PERFORM-BEGIN(RakuAST::Resolver $resolver) {
        self.apply-traits($resolver, self);

        if $!where && ! nqp::istype($!where, RakuAST::Code) {
            nqp::bindattr(self, RakuAST::Parameter, '$!where',
                RakuAST::Block.new(
                    body => RakuAST::Blockoid.new(
                        RakuAST::StatementList.new(
                            RakuAST::Statement::Expression.new(
                                expression => RakuAST::ApplyPostfix.new(
                                    operand => RakuAST::ApplyPostfix.new(
                                        operand => $!where,
                                        postfix => RakuAST::Call::Method.new(
                                            name => RakuAST::Name.from-identifier('ACCEPTS'),
                                            args => RakuAST::ArgList.new(
                                                RakuAST::Var::Lexical.new('$_'),
                                            ),
                                        ),
                                    ),
                                    postfix => RakuAST::Call::Method.new(
                                        name => RakuAST::Name.from-identifier('Bool'),
                                    ),
                                ),
                            ),
                        ),
                    ),
                ),
            );
        }
    }

    method PERFORM-CHECK(RakuAST::Resolver $resolver) {
        if nqp::istype($!owner, RakuAST::Routine) {
            my $name := $!owner.name;
            if $name && $name.is-identifier && $name.canonicalize eq 'MAIN' {
                self.add-worry: $resolver.build-exception: 'X::AdHoc',
                    payload => "'is rw' on parameters of 'sub MAIN' usually cannot be satisfied.\nDid you mean 'is copy'?"
            }
        }

        if $!default {
            # Ensure this is something that a default can go on.
            if nqp::isconcrete($!slurpy) {
                self.add-sorry: $resolver.build-exception: 'X::Parameter::Default',
                    how => 'slurpy', parameter => $!target.name;
            }
            if self.is-declared-required {
                self.add-sorry: $resolver.build-exception: 'X::Parameter::Default',
                    how => 'required', parameter => $!target.name;
            }

            # If it doesn't have a compile-time value, we'll need to thunk it.
            unless nqp::istype($!default, RakuAST::CompileTimeValue) {
                $!default.wrap-with-thunk(RakuAST::ParameterDefaultThunk.new(self));
            }
        }

        if $!sub-signature {
            $!owner.set-custom-args;
        }
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        # Flag constants we need to pay attention to.
        my constant SIG_ELEM_IS_RW               := 256;
        my constant SIG_ELEM_IS_COPY             := 512;
        my constant SIG_ELEM_IS_RAW              := 1024;

        # Get the parameter meta-object, since traits can change some things.
        my $param-obj := self.meta-object;
        my int $flags := nqp::getattr_i($param-obj, Parameter, '$!flags');

        # Take the parameter into a temporary local.
        my $name := QAST::Node.unique("__lowered_param");
        my $param-qast := QAST::Var.new( :decl('param'), :scope('local'), :$name );
        my $temp-qast-var := QAST::Var.new( :name($name), :scope('local') );

        # Deal with nameds and slurpies.
        my int $was-slurpy;
        my @prepend;
        if $!names {
            $param-qast.named(nqp::elems($!names) == 1 ?? $!names[0] !! $!names);
        }
        elsif !($!slurpy =:= RakuAST::Parameter::Slurpy) {
            $!slurpy.IMPL-TRANSFORM-PARAM-QAST($context, $param-qast, $temp-qast-var,
                $!target.sigil, @prepend);
            $was-slurpy := 1;
        }

        # HLLize before type checking unless it was a slurpy (in which
        # case we know full well what we produced).
        unless $was-slurpy {
            $param-qast.push(QAST::Op.new(
                :op('bind'),
                $temp-qast-var,
                QAST::Op.new( :op('hllize'), $temp-qast-var )
            ));
        }

        # We may need to decontainerize it; produce that lazily if so.
        my $decont-qast-var := $was-slurpy ?? $temp-qast-var !! Nil;
        my $get-decont-var := -> {
            unless $decont-qast-var {
                my str $name := QAST::Node.unique("__decont_param");
                $param-qast.push(QAST::Op.new(
                    :op('bind'),
                    QAST::Var.new( :$name, :scope('local'), :decl('var') ),
                    QAST::Op.new( :op('decont'), $temp-qast-var )
                ));
                $decont-qast-var := QAST::Var.new( :$name, :scope('local') );
            }
            $decont-qast-var
        }

        # Do type checks.
        # TODO really more involved than this
        my $nominal-type := nqp::getattr($param-obj, Parameter, '$!type');
        unless $nominal-type =:= Mu {
            $context.ensure-sc($nominal-type);

            if $nominal-type.HOW.archetypes.definite {
                $param-qast.push(QAST::ParamTypeCheck.new(QAST::Op.new(
                    :op('istype_nd'),
                    $get-decont-var(),
                    QAST::WVal.new( :value($nominal-type.HOW.base_type($nominal-type)) )
                )));
                my $concreteness := QAST::Op.new(
                    :op('isconcrete_nd'),
                    $get-decont-var(),
                );
                unless $nominal-type.HOW.definite($nominal-type) {
                    $concreteness := QAST::Op.new(:op('not_i'), $concreteness);
                }
                $param-qast.push(QAST::ParamTypeCheck.new($concreteness));
            }
            else {
                $param-qast.push(QAST::ParamTypeCheck.new(QAST::Op.new(
                    :op('istype_nd'),
                    $get-decont-var(),
                    QAST::WVal.new( :value($nominal-type) )
                )));
            }
        }

        # If marked `is rw`, do rw check.
        if $flags +& SIG_ELEM_IS_RW {
            $param-qast.push(QAST::ParamTypeCheck.new(QAST::Op.new(
                :op('isrwcont'),
                $temp-qast-var
            )));
        }

        # If it's optional, do any default handling.
        if self.is-optional {
            if nqp::istype($!default, RakuAST::CompileTimeValue) {
                # Literal default value, so just insert it.
                $param-qast.default($!default.IMPL-TO-QAST($context));
            }
            elsif $!default {
                # Default has been thunked, so call the produced thunk.
                $param-qast.default(QAST::Op.new(
                    :op('call'),
                    $!default.IMPL-TO-QAST($context)
                ));
            }
            else {
                # TODO non-Scalar sigil and native types
                $param-qast.default(QAST::WVal.new( :value($nominal-type) ));
            }
        }

        if $!where {
            $param-qast.push(
                QAST::ParamTypeCheck.new(
                    QAST::Op.new(
                        :op('call'),
                        $!where.IMPL-TO-QAST($context),
                        $temp-qast-var
                    )
                )
            );
        }

        $context.ensure-sc(nqp::getattr($param-obj, Parameter, '$!container_descriptor'));

        # Bind parameter into its target.
        if self.invocant {
            $param-qast.push(QAST::Op.new(
                :op('bind'),
                QAST::Var.new( :name('self'), :scope('lexical') ),
                $get-decont-var()
            ));
        }
        if nqp::isconcrete($!target) {
            if $flags +& (SIG_ELEM_IS_RW +| SIG_ELEM_IS_RAW) {
                # Don't do any decontainerization (rw or raw).
                $param-qast.push($!target.IMPL-BIND-QAST($context, $temp-qast-var));
            }
            else {
                my $value := $get-decont-var();

                if $flags +& SIG_ELEM_IS_COPY {
                    my $container_descriptor := $param-obj.container_descriptor;
                    if $container_descriptor {
                        $value := QAST::Op.new(
                            :op<p6scalarwithvalue>,
                            QAST::WVal.new(:value($container_descriptor)),
                            $value
                        );
                    }
                    else {
                        $value := QAST::Op.new(
                            :op('p6bindattrinvres'),
                            QAST::Op.new(
                                :op('create'),
                                QAST::WVal.new( :value(Scalar) )
                            ),
                            QAST::WVal.new( :value(Scalar) ),
                            QAST::SVal.new( :value('$!value') ),
                            $value
                        );
                    }
                }

                # Give the decontainerized thing.
                $param-qast.push($!target.IMPL-BIND-QAST($context, $value));
            }
        }

        @prepend ?? QAST::Stmts.new( |@prepend, $param-qast ) !! $param-qast
    }
}

# A parameter target is a symbol that a parameter is bound into. A parameter
# need not be bound into anything (it may be being used only as a matcher, or
# destructured). This serves primarily as a marker for the different kinds of
# parameter target.
class RakuAST::ParameterTarget is RakuAST::Node {
    method sigil() { '' }
    method name() { '' }
}

# A binding of a parameter into a lexical variable (with sigil).
class RakuAST::ParameterTarget::Var is RakuAST::ParameterTarget is RakuAST::Declaration
                                    is RakuAST::Meta is RakuAST::ContainerCreator {
    has str $.name;
    has RakuAST::Type $.type;
    has Mu $!of;

    method new(str $name!) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::ParameterTarget::Var, '$!name', $name);
        nqp::bindattr($obj, RakuAST::ParameterTarget::Var, '$!type', Mu);
        nqp::bindattr($obj, RakuAST::ParameterTarget::Var, '$!of', Mu);
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

    method twigil() {
        ''
    }

    method set-container-type(Mu $type, Mu $of) {
        nqp::bindattr(self, RakuAST::ParameterTarget::Var, '$!type', $type);
        nqp::bindattr(self, RakuAST::ParameterTarget::Var, '$!of', $of);
    }

    method PRODUCE-META-OBJECT() {
        self.IMPL-CONTAINER($!of)
    }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        if $!type {
            my $container := self.meta-object;
            $context.ensure-sc($container);
            QAST::Var.new( :decl('contvar'), :scope('lexical'), :name($!name), :value($container) )
        }
        else {
            QAST::Var.new( :decl('var'), :scope('lexical'), :name($!name) )
        }
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

# A binding of a parameter into a lexical term.
class RakuAST::ParameterTarget::Term is RakuAST::ParameterTarget is RakuAST::Declaration {
    has RakuAST::Name $.name;

    method new(RakuAST::Name $name!) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::ParameterTarget::Term, '$!name', $name);
        $obj
    }

    method lexical-name() {
        $!name.canonicalize
    }

    method introspection-name() {
        $!name.canonicalize
    }

    # Generate a lookup of this parameter, already resolved to this declaration.
    method generate-lookup() {
        my $lookup := RakuAST::Term::Name.new($!name);
        $lookup.set-resolution(self);
        $lookup
    }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        QAST::Var.new( :decl('var'), :scope('lexical'), :name($!name.canonicalize) )
    }

    method IMPL-BIND-QAST(RakuAST::IMPL::QASTContext $context, Mu $source-qast) {
        QAST::Op.new(
            :op('bind'),
            QAST::Var.new( :scope('lexical'), :name($!name.canonicalize) ),
            $source-qast
        )
    }

    method IMPL-LOOKUP-QAST(RakuAST::IMPL::QASTContext $context) {
        my str $scope := 'lexical';
        QAST::Var.new( :name($!name.canonicalize), :$scope )
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
            Mu $param-qast, Mu $temp-qast, str $sigil, @prepend) {
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
            Mu $param-qast, Mu $temp-qast, str $sigil, @prepend) {
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
            Mu $param-qast, Mu $temp-qast, str $sigil, @prepend) {
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
            Mu $param-qast, Mu $temp-qast, str $sigil, @prepend) {
        if $sigil eq '@' || $sigil eq '' {
            self.IMPL-QAST-LISTY-SLURP($param-qast, $temp-qast, Array, 'from-slurpy-onearg');
        }
        else {
            nqp::die("Parameter + quantifier not applicable to sigil '$sigil'");
        }
    }
}

# Capture slurpy (the | quantifier).
class RakuAST::Parameter::Slurpy::Capture is RakuAST::Parameter::Slurpy {
    method IMPL-FLAGS(str $sigil) {
        my constant SIG_ELEM_IS_CAPTURE := 32768;
        SIG_ELEM_IS_CAPTURE
    }

    method IMPL-TRANSFORM-PARAM-QAST(RakuAST::IMPL::QASTContext $context,
            Mu $param-qast, Mu $temp-qast, str $sigil, @prepend) {
        # Sneak in a slurpy hash parameter too.
        $param-qast.slurpy(1);
        my $hash-param-name := $temp-qast.name ~ '_hash';
        @prepend.push(QAST::Var.new(
            :name($hash-param-name), :scope('local'), :decl('param'),
            :slurpy(1), :named(1)
        ));

        # Build a capture object.
        my $capture-wval := QAST::WVal.new( :value(Capture) );
        $param-qast.push(QAST::Op.new(
            :op('bind'),
            $temp-qast,
            QAST::Op.new(
                :op('p6bindattrinvres'),
                QAST::Op.new(
                    :op('p6bindattrinvres'),
                    QAST::Op.new( :op('create'), $capture-wval ),
                    $capture-wval,
                    QAST::SVal.new( :value('@!list') ),
                    $temp-qast
                ),
                $capture-wval,
                QAST::SVal.new( :value('%!hash') ),
                QAST::Var.new( :name($hash-param-name), :scope('local') )
            )));
    }
}

# Thunk for a default parameter.
class RakuAST::ParameterDefaultThunk is RakuAST::ExpressionThunk {
    has RakuAST::Parameter $!parameter;

    method new(RakuAST::Parameter $parameter) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::ParameterDefaultThunk, '$!parameter', $parameter);
        $obj
    }

    method thunk-kind() {
        'Parameter default'
    }

    method IMPL-THUNK-META-OBJECT-PRODUCED(Mu $code) {
        nqp::bindattr($!parameter.meta-object, Parameter, '$!default_value', $code);
    }
}
