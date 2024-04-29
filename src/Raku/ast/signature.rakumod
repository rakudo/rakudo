# A signature, typically part of a block though also contained within a
# signature literal or a signature-based variable declarator.
class RakuAST::Signature
  is RakuAST::Meta
  is RakuAST::ImplicitLookups
  is RakuAST::Attaching
  is RakuAST::Term
{
    has List $.parameters;
    has RakuAST::Node $.returns;
    has int $!is-on-method;
    has int $!is-on-named-method;
    has int $!is-on-meta-method;
    has int $!is-on-role-method;
    has int $!is-on-role-body;
    has RakuAST::Package $!method-package;
    has RakuAST::Parameter $.implicit-invocant;
    has RakuAST::Parameter $!implicit-slurpy-hash;

    method new(List :$parameters, RakuAST::Node :$returns) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Signature, '$!parameters',
          self.IMPL-UNWRAP-LIST($parameters)
        ) if $parameters;
        nqp::bindattr($obj, RakuAST::Signature, '$!returns', $returns // RakuAST::Node);
        nqp::bindattr_i($obj, RakuAST::Signature, '$!is-on-method', 0);
        nqp::bindattr_i($obj, RakuAST::Signature, '$!is-on-named-method', 0);
        nqp::bindattr_i($obj, RakuAST::Signature, '$!is-on-meta-method', 0);
        nqp::bindattr_i($obj, RakuAST::Signature, '$!is-on-role-body', 0);
        nqp::bindattr_i($obj, RakuAST::Signature, '$!is-on-role-method', 0);
        $obj
    }

    method parameters() {
        self.IMPL-WRAP-LIST($!parameters // [])
    }
    method parameters-initialized() {
        nqp::isconcrete($!parameters) ?? True !! False
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

    method set-is-on-named-method(Bool $is-on-named-method) {
        # Stash away the fact whether we should put a type constraint on the implicit invocant
        nqp::bindattr_i(self, RakuAST::Signature, '$!is-on-named-method', $is-on-named-method ?? 1 !! 0);
    }

    method set-is-on-meta-method(Bool $is-on-meta-method) {
        # Stash away the fact whether we should put a type constraint on the implicit invocant
        nqp::bindattr_i(self, RakuAST::Signature, '$!is-on-meta-method', $is-on-meta-method ?? 1 !! 0);
    }

    method set-is-on-role-method(Bool $is-on-role-method) {
        # We need to know this later when drafting the type for the implicit invocant
        nqp::bindattr_i(self, RakuAST::Signature, '$!is-on-role-method', $is-on-role-method ?? 1 !! 0);
    }

    method set-is-on-role-body(Bool $is-on-role-body) {
        # Stash away the fact whether we should generate implicit parameters
        nqp::bindattr_i(self, RakuAST::Signature, '$!is-on-role-body', $is-on-role-body ?? 1 !! 0);
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

    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST([
            RakuAST::Type::Setting.new(RakuAST::Name.from-identifier('Mu')),
            RakuAST::Var::Compiler::Lookup.new('$?CLASS')
        ])
    }

    method IMPL-HAS-PARAMETER(Str $name) {
        return True if $name eq '%_' && $!implicit-slurpy-hash;
        if $!parameters {
            for $!parameters {
                return True if $_.target && $_.target.lexical-name eq $name;
            }
        }
        False
    }

    method IMPL-ENSURE-IMPLICITS() {
        if $!is-on-method && !($!implicit-invocant || $!implicit-slurpy-hash) {
            my @param-asts := $!parameters // [];
            unless @param-asts && @param-asts[0].invocant {
                my $type;
                if $!is-on-meta-method {
                    $type := self.get-implicit-lookups.AT-POS(0);
                }
                elsif $!is-on-named-method {
                    if nqp::isconcrete($!method-package) {
                        my $Class := self.get-implicit-lookups.AT-POS(1);
                        if $!is-on-role-method && $Class.is-resolved {
                            $type := RakuAST::Type::Simple.new(RakuAST::Name.from-identifier('$?CLASS'));
                            $type.set-resolution($Class.resolution);
                        } else {
                            my $package := $!method-package.stubbed-meta-object;
                            my $package-name := $package.HOW.name($package);
                            $type := RakuAST::Type::Simple.new(RakuAST::Name.from-identifier($package-name));
                            $type.set-resolution(RakuAST::VarDeclaration::Implicit::Constant.new(
                                :name($package-name), :value($package), :scope<lexical>));
                        }
                    }
                }
                nqp::bindattr(self, RakuAST::Signature, '$!implicit-invocant',
                    RakuAST::Parameter.new(:invocant, :$type));
            }
            unless $!is-on-meta-method {
                my int $slurpy-hash-seen;
                for @param-asts {
                    if !($_.slurpy =:= RakuAST::Parameter::Slurpy) && $_.target && $_.target.sigil eq '%' {
                        $slurpy-hash-seen := 1;
                        last;
                    }
                }
                unless $slurpy-hash-seen {
                    nqp::bindattr(self, RakuAST::Signature, '$!implicit-slurpy-hash',
                        RakuAST::Parameter.new(
                          :slurpy(RakuAST::Parameter::Slurpy::Flattened.new),
                          :target(RakuAST::ParameterTarget::Var.new(:name<%_>))
                        )
                    );
                }
            }
        }
        if $!is-on-role-body && !$!implicit-invocant {
            my @param-asts := $!parameters // [];
            unless @param-asts && @param-asts[0].invocant {
                my $invocant := RakuAST::Parameter.new();
                $invocant.add-type-capture(
                    RakuAST::Type::Capture.new(RakuAST::Name.from-identifier('$?CLASS'))
                );
                $invocant.add-type-capture(
                    RakuAST::Type::Capture.new(RakuAST::Name.from-identifier('::?CLASS'))
                );
                nqp::bindattr(self, RakuAST::Signature, '$!implicit-invocant', $invocant);
            }
        }
    }

    method PRODUCE-META-OBJECT() {
        # Produce meta-objects for each parameter.
        self.IMPL-ENSURE-IMPLICITS();
        my @parameters;
        if $!implicit-invocant {
            @parameters.push($!implicit-invocant.meta-object);
        }
        if $!parameters {
            for $!parameters {
                @parameters.push($_.meta-object);
            }
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

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        my $signature := self.meta-object;
        $context.ensure-sc($signature);
        QAST::WVal.new(:value($signature))
    }

    method IMPL-QAST-BINDINGS(RakuAST::IMPL::QASTContext $context, :$needs-full-binder) {
        self.IMPL-ENSURE-IMPLICITS();
        my $bindings := QAST::Stmts.new();
        my $parameters := $!parameters // [];
        if $needs-full-binder {
            $bindings.push(QAST::Op.new(
                :op('if'),
                QAST::Op.new(
                    :op('syscall'),
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
        if $!parameters {
            for $!parameters {
                $_.set-default-type($type);
            }
        }
    }

    method arity() {
        my int $arity;
        ++$arity if $!implicit-invocant;
        if $!parameters {
            for $!parameters {
                last unless $_.is-positional && !$_.is-optional;
                ++$arity;
            }
        }
        nqp::box_i($arity, Int)
    }

    method count() {
        my int $count;
        ++$count if $!implicit-invocant;
        if $!parameters {
            for $!parameters {
                if $_.is-positional {
                    ++$count;
                }
                elsif !($_.slurpy =:= RakuAST::Parameter::Slurpy)
                         && $_.target.sigil ne '%' {
                    return nqp::box_n(nqp::inf, Num);
                }
            }
        }
        nqp::box_i($count, Int)
    }

    method visit-children(Code $visitor) {
        $visitor($!implicit-invocant) if $!implicit-invocant;
        if $!parameters {
            for $!parameters {
                $visitor($_);
            }
        }
        $visitor($!implicit-slurpy-hash) if $!implicit-slurpy-hash;
        $visitor($!returns) if $!returns;
    }

    has Mu $!ins_params;
    method IMPL-SIGNATURE-PARAMS(Mu $var) {
        unless $!ins_params {
            nqp::bindattr(self, RakuAST::Signature, '$!ins_params', QAST::Node.unique('__lowered_parameters_'));
            $var.push(
                QAST::Op.new(
                    :op('bind'),
                    QAST::Var.new(:name($!ins_params), :scope('local'), :decl('var')),
                    QAST::Op.new( # Get @!params on the signature
                        :op('getattr'),
                        QAST::Op.new( # Get signature object
                            :op('getattr'),
                            QAST::Op.new( :op('getcodeobj'), QAST::Op.new( :op('curcode') ) ),
                            QAST::WVal.new(:value(Code)),
                            QAST::SVal.new(:value('$!signature'))
                        ),
                        QAST::WVal.new(:value(Signature)),
                        QAST::SVal.new(:value('@!params'))
                    )
                )
            );
        }
        QAST::Var.new(:name($!ins_params), :scope('local'))
    }
}

class RakuAST::FakeSignature
  is RakuAST::Meta
  is RakuAST::Term
  is RakuAST::LexicalScope
{
    has RakuAST::Signature $.signature;

    method new($signature) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::FakeSignature, '$!signature', $signature);
        $obj
    }

    method PRODUCE-META-OBJECT() {
        $!signature.meta-object
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        $!signature.IMPL-TO-QAST($context)
    }

    method visit-children(Code $visitor) {
        $visitor($!signature)
    }
}

# A parameter within a signature. A parameter may result in binding or
# assignment into a target; this is modeled by a RakuAST::ParameterTarget,
# which is optional.
class RakuAST::Parameter
  is RakuAST::Meta
  is RakuAST::Attaching
  is RakuAST::ImplicitLookups
  is RakuAST::TraitTarget
  is RakuAST::BeginTime
  is RakuAST::CheckTime
  is RakuAST::Doc::DeclaratorTarget
{
    has RakuAST::Type              $.type;
    has RakuAST::ParameterTarget   $.target;
    has Mu                         $!names;
    has Bool                       $.invocant;
    has Bool                       $.optional;
    has Bool                       $.default-rw;
    has Bool                       $.default-raw;
    has RakuAST::Parameter::Slurpy $.slurpy;
    has RakuAST::Expression        $.default;
    has RakuAST::Expression        $.where;
    has RakuAST::Node              $!owner;
    has RakuAST::Package           $!package;
    has RakuAST::Signature         $.sub-signature;
    has List                       $!type-captures;
    has Mu                         $.value;

    method new(  RakuAST::Type :$type,
      RakuAST::ParameterTarget :$target,
                          List :$names,
                          Bool :$invocant,
                          Bool :$optional,
                          Bool :$default-rw,
                          Bool :$default-raw,
    RakuAST::Parameter::Slurpy :$slurpy,
                          List :$traits,
           RakuAST::Expression :$default,
           RakuAST::Expression :$where,
            RakuAST::Signature :$sub-signature,
                          List :$type-captures,
                            Mu :$value,
      RakuAST::Doc::Declarator :$WHY
    ) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Parameter, '$!type',
          $type // RakuAST::Type);
        if $target {
            $target.set-type($type)
              if nqp::can($target, 'set-type') && $type;
        }
        nqp::bindattr($obj, RakuAST::Parameter, '$!target',
          $target // RakuAST::ParameterTarget);
        nqp::bindattr($obj, RakuAST::Parameter, '$!names',
          self.IMPL-NAMES($names));
        nqp::bindattr($obj, RakuAST::Parameter, '$!invocant',
          $invocant ?? True !! False);
        nqp::bindattr($obj, RakuAST::Parameter, '$!optional',
          nqp::defined($optional)
            ?? ($optional ?? True !! False)
            !! Bool);
        nqp::bindattr($obj, RakuAST::Parameter, '$!default-rw',
          nqp::defined($default-rw)
            ?? ($default-rw ?? True !! False)
            !! Bool);
        nqp::bindattr($obj, RakuAST::Parameter, '$!default-raw',
          nqp::defined($default-raw)
            ?? ($default-raw ?? True !! False)
            !! Bool);
        nqp::bindattr($obj, RakuAST::Parameter, '$!slurpy',
          nqp::istype($slurpy, RakuAST::Parameter::Slurpy)
            ?? $slurpy
            !! RakuAST::Parameter::Slurpy);
        $obj.set-traits($traits);
        nqp::bindattr($obj, RakuAST::Parameter, '$!default',
          $default // RakuAST::Expression);
        nqp::bindattr($obj, RakuAST::Parameter, '$!where',
          $where // RakuAST::Expression);
        nqp::bindattr($obj, RakuAST::Parameter, '$!sub-signature',
          $sub-signature // RakuAST::Signature);
        nqp::bindattr($obj, RakuAST::Parameter, '$!type-captures',
          nqp::defined($type-captures)
            ?? self.IMPL-TYPE-CAPTURES($type-captures)
            !! []);
        nqp::bindattr($obj, RakuAST::Parameter, '$!value', $value)
          if nqp::defined($value);
        $obj.set-WHY($WHY);
        $obj
    }

    method set-type(RakuAST::Type $type) {
        nqp::bindattr(self, RakuAST::Parameter, '$!type', $type);
        $!target.set-type($type) if $!target && nqp::can($!target, 'set-type');
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

    method set-bindable() {
        $!target.set-bindable(True);
        Nil
    }

    method set-default-rw() {
        nqp::bindattr(self, RakuAST::Parameter, '$!default-rw', True);
        $!target.set-rw;
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

    method set-value(Mu $value) {
        nqp::bindattr(self, RakuAST::Parameter, '$!value', $value);
        Nil
    }

    method set-sub-signature(RakuAST::Expression $sub-signature) {
        nqp::bindattr(self, RakuAST::Parameter, '$!sub-signature', $sub-signature);
        Nil
    }

    method set-type-captures(List $type-captures) {
        nqp::bindattr(self, RakuAST::Parameter, '$!type-captures',
            self.IMPL-TYPE-CAPTURES($type-captures));
    }

    method add-type-capture(RakuAST::Type::Capture $type-capture) {
        nqp::push($!type-captures, $type-capture);
        Nil
    }

    method type-captures() {
        self.IMPL-WRAP-LIST($!type-captures)
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

    method IMPL-TYPE-CAPTURES(Mu $type-captures) {
        my @type-captures;
        if $type-captures {
            for self.IMPL-UNWRAP-LIST($type-captures) {
                if nqp::istype($_, RakuAST::Type::Capture) {
                    @type-captures.push($_);
                }
                else {
                    nqp::die('Parameter type-captures list must be a list of RakuAST::Type::Captures');
                }
            }
        }
        @type-captures
    }

    method attach(RakuAST::Resolver $resolver) {
        nqp::bindattr(self, RakuAST::Parameter, '$!owner',
            $resolver.find-attach-target('block'));
        nqp::bindattr(self, RakuAST::Parameter, '$!package',
            $resolver.find-attach-target('package'));
    }

    method visit-children(Code $visitor) {
        $visitor($!type)          if $!type;
        $visitor($!target)        if $!target;
        $visitor($!default)       if $!default;
        $visitor($!where)         if $!where;
        $visitor($!sub-signature) if $!sub-signature;
        $visitor(self.WHY)        if self.WHY;
        self.visit-traits($visitor);
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        my @lookups;
        my str $sigil := $!target.sigil;
        my str $sigil-type;
        if $sigil eq '@' { nqp::push(@lookups, 'Positional'); nqp::push(@lookups, 'PositionalBindFailover') }
        elsif $sigil eq '%' { nqp::push(@lookups, 'Associative') }
        elsif $sigil eq '&' { nqp::push(@lookups, 'Callable') }

        my @types;
        for @lookups {
            nqp::push(@types, RakuAST::Type::Setting.new(RakuAST::Name.from-identifier($_)));
        }
        self.IMPL-WRAP-LIST(@types)
    }

    method IMPL-DEFAULT-RW() {
        return True if $!default-rw;

        for self.IMPL-UNWRAP-LIST(self.traits) {
            if nqp::istype($_, RakuAST::Trait::Is) && $_.name.canonicalize eq 'rw' {
                return True;
            }
        }

        False
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
        nqp::bindattr_i($parameter, Parameter, '$!flags', self.IMPL-FLAGS);
        my $type := self.IMPL-NOMINAL-TYPE();
        nqp::bindattr($parameter, Parameter, '$!type', $type.WHAT); # "Type" could be a concrete value
        if $!target {
            my $name := $!target.introspection-name;
            for self.IMPL-UNWRAP-LIST(self.traits) {
                if $!default-rw
                    || nqp::istype($_, RakuAST::Trait::Is) && (
                        $_.name.canonicalize eq 'copy'
                        || $_.name.canonicalize eq 'rw'
                    )
                {
                    my $cd := ContainerDescriptor.new(:of($type), :$name, :default($type), :dynamic(0));
                    nqp::bindattr($parameter, Parameter, '$!container_descriptor', $cd);
                    $!target.set-bindable(True);
                    $!target.set-rw;
                    last;
                }
            }
        }
        my @post_constraints;
        if $!where {
            nqp::push(@post_constraints, $!where.IMPL-CURRIED || $!where.meta-object);
        }
        if nqp::defined($!value) {
            nqp::push(@post_constraints, $!value);
        }
        if nqp::defined($!type) && nqp::isconcrete($!type.meta-object) { # Not really a type at all
            nqp::push(@post_constraints, $!type.meta-object);
        }
        if nqp::defined($!type) && $!type.meta-object.HOW.archetypes.nominalizable {
            nqp::push(@post_constraints, $!type.meta-object);
        }
        if nqp::elems(@post_constraints) {
            nqp::bindattr($parameter, Parameter, '@!post_constraints', @post_constraints);
        }
        if $!sub-signature {
            nqp::bindattr($parameter, Parameter, '$!sub_signature', $!sub-signature.meta-object);
        }
        if $!type-captures {
            my @type-captures := nqp::list_s;
            for $!type-captures {
                nqp::push_s(@type-captures, $_.lexical-name);
            }
            nqp::bindattr($parameter, Parameter, '@!type_captures', @type-captures);
        }
        # TODO further setup
        $parameter
    }

    method IMPL-FLAGS() {

        my $sigil := $!target.sigil;
        my int $flags;
        $flags := $flags +| nqp::const::SIG_ELEM_INVOCANT if $!invocant;
        $flags := $flags +| nqp::const::SIG_ELEM_MULTI_INVOCANT;
        $flags := $flags +| nqp::const::SIG_ELEM_IS_OPTIONAL if self.is-optional;
        if $sigil eq '@' {
            $flags := $flags +| nqp::const::SIG_ELEM_ARRAY_SIGIL;
        }
        elsif $sigil eq '%' {
            $flags := $flags +| nqp::const::SIG_ELEM_HASH_SIGIL;
        }
        elsif $sigil eq '&' {
            $flags := $flags +| nqp::const::SIG_ELEM_CODE_SIGIL;
        }
        if $!default-rw {
            $flags := $flags +| nqp::const::SIG_ELEM_IS_RW;
        }
        if $!default-raw {
            $flags := $flags +| nqp::const::SIG_ELEM_IS_RAW;
        }
        if nqp::istype($!target, RakuAST::ParameterTarget::Term) {
            $flags := $flags +| nqp::const::SIG_ELEM_IS_RAW;
        }
        if nqp::istype($!type, RakuAST::Type::Definedness) {
            $flags := $flags +| ($!type.definite
              ?? nqp::const::SIG_ELEM_DEFINED_ONLY
              !! nqp::const::SIG_ELEM_UNDEFINED_ONLY
            );
        }
        if $!type.is-coercive {
            $flags := $flags +| nqp::const::SIG_ELEM_IS_COERCIVE;
        }
        if $!type {
            my $meta-object := $!type.meta-object;
            if $meta-object.HOW.archetypes.generic {
                $flags := $flags +| nqp::const::SIG_ELEM_TYPE_GENERIC;
            }
            my $primspec := nqp::objprimspec($!type.meta-object);
            if $primspec == nqp::const::BIND_VAL_INT {
                $flags := $flags +| nqp::const::SIG_ELEM_NATIVE_INT_VALUE;
            }
            elsif $primspec == nqp::const::BIND_VAL_NUM {
                $flags := $flags +| nqp::const::SIG_ELEM_NATIVE_NUM_VALUE;
            }
            elsif $primspec == nqp::const::BIND_VAL_STR {
                $flags := $flags +| nqp::const::SIG_ELEM_NATIVE_STR_VALUE;
            }
            elsif $primspec == nqp::const::BIND_VAL_UINT {
                $flags := $flags +| nqp::const::SIG_ELEM_NATIVE_UINT_VALUE;
            }
        }
        $flags := $flags +| $!slurpy.IMPL-FLAGS($sigil);
        $flags
    }

    method IMPL-NOMINAL-TYPE() {
        my str $sigil := $!target.sigil;
        if $sigil eq '@' || $sigil eq '%' || $sigil eq '&' {
            my $sigil-type :=
              self.get-implicit-lookups.AT-POS(0).resolution.compile-time-value;
            $!type
                ?? $sigil-type.HOW.parameterize($sigil-type,
                        $!type.resolution.compile-time-value)
                !! $sigil-type
        }
        else {
            if $!type {
                my $type       := $!type.meta-object;
                my $archetypes := $type.HOW.archetypes;
                $archetypes.nominal
                  || $archetypes.coercive
                  || $archetypes.generic
                  ?? $type
                  !! $archetypes.nominalizable
                    ?? $type.HOW.nominalize($type)
                    !! $type
            }
            else {
                Mu
            }
        }
    }

    method PERFORM-BEGIN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        self.apply-traits($resolver, $context, self);

        if $!where && (! nqp::istype($!where, RakuAST::Code) || nqp::istype($!where, RakuAST::RegexThunk)) && !$!where.IMPL-CURRIED {
            my $block := RakuAST::Block.new(
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
            );
            $block.IMPL-CHECK($resolver, $context, False);
            nqp::bindattr(self, RakuAST::Parameter, '$!where', $block);
        }
    }

    method PERFORM-CHECK(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        if nqp::istype($!owner, RakuAST::Routine) {
            my $name := $!owner.name;
            if $name && $name.is-identifier && $name.canonicalize eq 'MAIN' {
                for self.IMPL-UNWRAP-LIST(self.traits) {
                    if nqp::istype($_, RakuAST::Trait::Is) && $_.name.canonicalize eq 'copy' {
                        self.add-worry:
                          $resolver.build-exception: 'X::AdHoc',
                            payload => "'is rw' on parameters of 'sub MAIN' usually cannot be satisfied.\nDid you mean 'is copy'?";
                        last;
                    }
                }
            }
        }

        if $!default {
            # Ensure this is something that a default can go on.
            if nqp::isconcrete($!slurpy) {
                self.add-sorry:
                  $resolver.build-exception: 'X::Parameter::Default',
                    how => 'slurpy', parameter => $!target.name;
            }
            if self.is-declared-required {
                self.add-sorry:
                  $resolver.build-exception: 'X::Parameter::Default',
                    how => 'required', parameter => $!target.name;
            }

            # If it doesn't have a compile-time value, we'll need to thunk it.
            unless nqp::istype($!default, RakuAST::CompileTimeValue) {
                $!default.wrap-with-thunk(RakuAST::ParameterDefaultThunk.new(self));
                $!default.visit-thunks(-> $thunk { $thunk.ensure-begin-performed($resolver, $context) });
            }
        }

        if $!sub-signature {
            $!owner.set-custom-args;
        }

        my $param-obj := self.meta-object;
        my $param-type := nqp::getattr($param-obj, Parameter, '$!type');
        my $ptype-archetypes := $param-type.HOW.archetypes($param-type);
        my int $is-generic  := $ptype-archetypes.generic;
        my int $is-coercive := $ptype-archetypes.coercive;
        my int $was-slurpy := !($!slurpy =:= RakuAST::Parameter::Slurpy);

        if $is-generic && $is-coercive && !$was-slurpy && !($param-type =:= Mu) && !self.invocant {
            $!owner.set-custom-args;
        }

        my $sigil := $!target.sigil;
        if self.meta-object.is-item && ($sigil eq '$' || $sigil eq '&') {
            self.add-sorry:
                $resolver.build-exception:  'X::Comp::Trait::Invalid',
                                            name        => $!target.name,
                                            reason      => "only '\@' or '\%' sigiled parameters can be constrained to itemized arguments",
                                            declaring   => 'parameter',
                                            type        => 'is',
                                            subtype     => 'item';
        }
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        nqp::die('shouldnt get here') if $!sub-signature;
        # Flag constants we need to pay attention to.
        my @iscont-ops := ['iscont', 'iscont_i', 'iscont_n', 'iscont_s', 'iscont_i', 'iscont_i', 'iscont_i', 'iscont_u', 'iscont_u', 'iscont_u', 'iscont_u'];

        # Get the parameter meta-object, since traits can change some things.
        my $param-obj := self.meta-object;
        my int $flags := nqp::getattr_i($param-obj, Parameter, '$!flags');
        my int $is-rw := $flags +& nqp::const::SIG_ELEM_IS_RW;

        my $param-type := nqp::getattr($param-obj, Parameter, '$!type');
        my $ptype-archetypes := $param-type.HOW.archetypes($param-type);
        my int $is-generic  := $ptype-archetypes.generic;
        my int $is-coercive := $ptype-archetypes.coercive;
        my $nominal-type := !$is-generic && $ptype-archetypes.nominalizable
                            ?? $param-type.HOW.nominalize($param-type)
                            !! $param-type;
        my int $spec  := nqp::objprimspec($nominal-type);

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
        # case we know full well what we produced) or it's a native type
        if !$was-slurpy && ($is-generic || !$spec) {
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

        # Add type checks.
        if !$is-generic && $spec {
            if $is-rw {
                $param-qast.push(QAST::ParamTypeCheck.new(QAST::Op.new(
                    :op(@iscont-ops[$spec]),
                    $temp-qast-var
                )));
            }
            else {
                $context.ensure-sc($param-type);
                $param-qast.returns($param-type);
            }
        }
        elsif !$was-slurpy {
            # Type-check, unless it's Mu, in which case skip it.
            if $is-generic && !$is-coercive {
                my $genericname := $param-type.HOW.name($!package.stubbed-meta-object);
                $param-qast.push(QAST::ParamTypeCheck.new(QAST::Op.new(
                    :op('istype_nd'),
                    $get-decont-var(),
                    QAST::Var.new( :name($genericname), :scope<typevar> )
                )));
            }
            elsif !($param-type =:= Mu) {
                if !$ptype-archetypes.generic {
                    if $!target.sigil eq '@' {
                        my $PositionalBindFailover := self.get-implicit-lookups.AT-POS(1).resolution.compile-time-value;
                        $param-qast.push(QAST::Op.new(
                            :op('if'),
                            QAST::Op.new(
                                :op('istype_nd'),
                                $get-decont-var(),
                                QAST::WVal.new( :value($PositionalBindFailover) )
                            ),
                            QAST::Op.new(
                                :op('bind'),
                                $get-decont-var(),
                                QAST::Op.new(
                                    :op('decont'),
                                    QAST::Op.new(
                                        :op('bind'),
                                        $temp-qast-var,
                                        QAST::Op.new(
                                            :op('callmethod'), :name('cache'),
                                            $get-decont-var()
                                        ))))));
                    }

                    # Try to be smarter with coercions. We don't have to do full typecheck on them, which results in
                    # additional call to a HOW method. Instead it's ok to check if value matches target or
                    # constraint types.
                    if $is-coercive && nqp::can($param-type.HOW, 'target_type') {
                        my $constraint-type := $param-type.HOW.constraint_type($param-type);
                        $context.ensure-sc($constraint-type);
                        $param-qast.push(QAST::ParamTypeCheck.new(QAST::Op.new(
                                :op('unless'),
                                QAST::Op.new(
                                    :op('istype_nd'),
                                    $get-decont-var(),
                                    QAST::WVal.new( :value($param-type.HOW.target_type($param-type) ))),
                                QAST::Op.new(
                                    :op('istype_nd'),
                                    $get-decont-var(),
                                    QAST::WVal.new( :value($constraint-type))))));
                    }
                    else {
                        $context.ensure-sc($param-type);
                        $param-qast.push(QAST::ParamTypeCheck.new(QAST::Op.new(
                            :op('istype_nd'),
                            $get-decont-var(),
                            QAST::WVal.new( :value($param-type) )
                        )));
                    }
                }
            }
            if nqp::istype($!type.IMPL-TARGET-TYPE, RakuAST::Type::Definedness) {
                if $!type.IMPL-TARGET-TYPE.definite {
                    $param-qast.push(QAST::ParamTypeCheck.new(QAST::Op.new(
                        :op('isconcrete_nd'),
                        $get-decont-var()
                    )));
                }
                else {
                    $param-qast.push(QAST::ParamTypeCheck.new(QAST::Op.new(
                        :op('not_i'),
                        QAST::Op.new(
                            :op('isconcrete_nd'),
                            $get-decont-var()
                        ))));
                }
            }
            # If marked `is rw`, do rw check.
            if $is-rw {
                $param-qast.push(QAST::ParamTypeCheck.new(QAST::Op.new(
                    :op('isrwcont'),
                    $temp-qast-var,
                )));
            }
            if nqp::defined($!type) && nqp::isconcrete($!type.meta-object) {
                my $value := $!type.meta-object;
                $context.ensure-sc($value);
                $param-qast.push(QAST::ParamTypeCheck.new(QAST::Op.new(
                    :op<istrue>,
                    QAST::Op.new(
                        :op<callmethod>,
                        :name<ACCEPTS>,
                        QAST::WVal.new( :$value ),
                        $get-decont-var(),
                    )
                )));
            }
        }

        my $inst-param;
        # Make sure we have (possibly instantiated) parameter object ready when we need it
        if $is-generic || $!sub-signature {
            my $inst-param-name := QAST::Node.unique('__lowered_param_obj_');
            my $i := 0; #FIXME TODO XXX Need a way to know which parameter this is in the signature!
            $param-qast.push( # Fetch instantiated Parameter object
                QAST::Op.new(
                    :op('bind'),
                    QAST::Var.new( :name($inst-param-name), :scope('local'), :decl('var') ),
                    QAST::Op.new(
                        :op('atpos'),
                        $!owner.signature.IMPL-SIGNATURE-PARAMS($param-qast),
                        QAST::IVal.new(:value($i)))));
            $inst-param := QAST::Var.new(:name($inst-param-name), :scope<local>);
        }

        # Handle coercion.
        # For a generic we can't know beforehand if it's going to be a coercive or any other nominalizable. Thus
        # we have to fetch the instantiated parameter object and do run-time processing.
        if $is-generic {
            # For a generic-typed parameter get its instantiated clone and see if its type is a coercion.
            $get-decont-var := -> { NQPMu }
            my $low-param-type := QAST::Node.unique('__lowered_param_type');
            $param-qast.push( # Get actual parameter type
                            QAST::Op.new(
                                :op('bind'),
                                QAST::Var.new(:name($low-param-type), :scope('local'), :decl('var')),
                                QAST::Op.new(
                                    :op('getattr'),
                                    $inst-param,
                                    QAST::WVal.new(:value(Parameter)),
                                    QAST::SVal.new(:value('$!type')))));
            $param-qast.push(
                QAST::Op.new(
                    :op('if'),
                    QAST::Op.new(
                        :op('istype'),
                        $temp-qast-var,
                        QAST::Var.new(:name($low-param-type), :scope('local'))
                    ),
                    QAST::Op.new(
                        :op('if'),
                        QAST::Op.new(
                            :op('bitand_i'),
                            QAST::Op.new(
                                :op('getattr'),
                                $inst-param,
                                QAST::WVal.new(:value(Parameter)),
                                QAST::SVal.new(:value('$!flags'))
                            ),
                            QAST::IVal.new(:value(nqp::const::SIG_ELEM_IS_COERCIVE))
                        ),
                        QAST::Op.new(
                            :op('bind'),
                            $temp-qast-var,
                            QAST::Op.new(
                                :op<dispatch>,
                                QAST::SVal.new(:value<raku-coercion>),
                                QAST::Var.new(:name($low-param-type), :scope<local>),
                                $temp-qast-var)))));
        }
        elsif $is-coercive {
            $get-decont-var := -> { NQPMu }
            my $coercion-type := $param-type.HOW.wrappee($param-type, :coercion);
            my $target-type := $coercion-type.HOW.target_type($coercion-type);
            $context.ensure-sc($param-type);
            $context.ensure-sc($target-type);
            $param-qast.push(
                QAST::Op.new(
                    :op('unless'),
                    QAST::Op.new(
                        :op('istype'),
                        $temp-qast-var,
                        QAST::WVal.new( :value($target-type) )
                    ),
                    QAST::Op.new(
                        :op('bind'),
                        $temp-qast-var,
                        QAST::Op.new(
                            :op<dispatch>,
                            QAST::SVal.new(:value<raku-coercion>),
                            QAST::WVal.new(:value($param-type)),
                            $temp-qast-var))));
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
                my $sigil := $!target.sigil;
                if (my $is-array := $sigil eq '@') || $sigil eq '%' {
                    my $role := self.get-implicit-lookups.AT-POS(0).resolution.compile-time-value;
                    my $base-type := $is-array ?? Array !! Hash;
                    my $value := nqp::istype($nominal-type, $role) && nqp::can($nominal-type.HOW, 'role_arguments')
                        ?? $base-type.HOW.parameterize($base-type, |$nominal-type.HOW.role_arguments($nominal-type))
                        !! $base-type;
                    $context.ensure-sc($value);
                    $param-qast.default(
                        QAST::Op.new(
                            :op<create>,
                            QAST::WVal.new(value => $value)
                        )
                    );
                }
                else {
                    if $spec == 1 {
                        $param-qast.default(QAST::IVal.new( :value(0) ));
                    }
                    elsif $spec == 2 {
                        $param-qast.default(QAST::NVal.new( :value(0.0) ));
                    }
                    elsif $spec == 3 {
                        $param-qast.default(QAST::SVal.new( :value('') ));
                    }
                    elsif $spec == 10 {
                        $param-qast.default(QAST::IVal.new( :value(0) ));
                    }
                    else {
                        $param-qast.default(QAST::WVal.new( :value($nominal-type) ));
                    }
                }
            }
        }

        if nqp::defined($!value) {
            my $value := $!value;
            my $wval  := QAST::WVal.new: :value($value);
            $context.ensure-sc($value);
            my $type := $!type.name.canonicalize;
            $param-qast.push: QAST::ParamTypeCheck.new:
                $type eq 'Int'
                    ?? QAST::Op.new(:op<if>,
                        QAST::Op.new(:op<isconcrete>, $temp-qast-var),
                        QAST::Op.new(:op<iseq_I>, $wval,
                          QAST::Op.new: :op<decont>, $temp-qast-var))
                    !! $type eq 'Num'
                      ?? QAST::Op.new(:op<if>,
                          QAST::Op.new(:op<isconcrete>, $temp-qast-var),
                          QAST::Op.new(:op<unless>,
                            QAST::Op.new(:op<iseq_n>, $wval, $temp-qast-var), # equal
                            QAST::Op.new(:op<if>, # or both are NaNs
                              QAST::Op.new(:op<isne_n>, $wval, $wval),
                              QAST::Op.new(:op<isne_n>, $temp-qast-var, $temp-qast-var))))
                      !! QAST::Op.new(:op<if>,
                          QAST::Op.new(:op<isconcrete>, $temp-qast-var),
                          QAST::Op.new(:op<iseq_s>, $wval, $temp-qast-var));
        }

        $context.ensure-sc(nqp::getattr($param-obj, Parameter, '$!container_descriptor'));

        # Bind parameter into its target.
        if self.invocant {
            $param-qast.push(QAST::Op.new(
                :op('bind'),
                QAST::Var.new( :name('self'), :scope('lexical') ),
                $get-decont-var() // QAST::Op.new(
                        :op('decont'),
                        $temp-qast-var,
                    )));
        }
        if nqp::isconcrete($!target) {
            if $flags +& (
              nqp::const::SIG_ELEM_IS_RW +| nqp::const::SIG_ELEM_IS_RAW
            ) {
                # Don't do any decontainerization (rw or raw).
                $param-qast.push($!target.IMPL-BIND-QAST($context, $temp-qast-var));
            }
            else {
                my $value := $get-decont-var() // $temp-qast-var;

                if $flags +& nqp::const::SIG_ELEM_IS_COPY {
                    my $sigil := $!target.sigil;
                    if (my $is-array := $sigil eq '@') || $sigil eq '%' {
                        my $copy-var := $name ~ '_copy';
                        $param-qast.push(
                            QAST::Op.new( :op<bind>,
                                QAST::Var.new( :name($copy-var), :scope<local>, :decl<var> ),
                                QAST::Op.new( :op<create>,
                                    QAST::WVal.new( :value($is-array ?? Array !! Hash) )
                                )
                            )
                        );
                        $param-qast.push(
                            QAST::Op.new( :op<callmethod>, :name<STORE>,
                                QAST::Var.new( :name($copy-var), :scope<local> ),
                                $value
                            )
                        );
                        $value := QAST::Var.new( :name($copy-var), :scope<local> );
                    }
                    else {
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
                }

                # Give the decontainerized thing.
                $param-qast.push($!target.IMPL-BIND-QAST($context, $value));
            }
        }
        for $!type-captures {
            $param-qast.push($_.IMPL-BIND-QAST($context, $temp-qast-var));
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

        # Take care of checking against provided subset types and other nominalizable types.
        # TODO: Investigate breakage -- No such method 'ACCEPTS' for invocant of type '::?CLASS:D'
        #        if nqp::defined($!type) && $!type.meta-object.HOW.archetypes.nominalizable {
        if nqp::defined($!type) && nqp::istype($!type.meta-object.HOW, Perl6::Metamodel::SubsetHOW) {
            my $type-object := $!type.meta-object;
            $context.ensure-sc($type-object);
            $param-qast.push(
                QAST::ParamTypeCheck.new(
                    QAST::Op.new(
                        :op<istrue>,
                        QAST::Op.new(
                            :op('callmethod'), :name('ACCEPTS'),
                            QAST::WVal.new( :value($type-object) ),
                            $temp-qast-var
                        )
                    )
                )
            );
        }

        @prepend ?? QAST::Stmts.new( |@prepend, $param-qast ) !! $param-qast
    }
}

# A parameter target is a symbol that a parameter is bound into. A parameter
# need not be bound into anything (it may be being used only as a matcher, or
# destructured). This serves primarily as a marker for the different kinds of
# parameter target.
class RakuAST::ParameterTarget
  is RakuAST::Node
{
    method set-rw() { }
    method sigil() { '' }
    method name() { '' }
}

# A binding of a parameter into a lexical variable (with sigil).
class RakuAST::ParameterTarget::Var
  is RakuAST::ParameterTarget
  is RakuAST::TraitTarget
  is RakuAST::Meta
  is RakuAST::ContainerCreator
  is RakuAST::BeginTime
  is RakuAST::CheckTime
  is RakuAST::Attaching
{
    has str $.name;
    has RakuAST::Type $.type;
    has Mu $!of;
    has RakuAST::Package $!attribute-package;
    has RakuAST::VarDeclaration::Simple $!declaration;
    has str $!scope;
    has Bool $!is-bindable;

    method new(str :$name!, Bool :$forced-dynamic) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::ParameterTarget::Var, '$!name', $name);
        nqp::bindattr($obj, RakuAST::ParameterTarget::Var, '$!type', Mu);
        nqp::bindattr($obj, RakuAST::ParameterTarget::Var, '$!of', Mu);
        nqp::bindattr($obj, RakuAST::ParameterTarget::Var, '$!is-bindable', False);
        nqp::bindattr($obj, RakuAST::ContainerCreator, '$!forced-dynamic',
          $forced-dynamic ?? True !! False);
        my $sigil := $obj.sigil;
        my $twigil := $obj.twigil;
        nqp::bindattr(
            $obj,
            RakuAST::ParameterTarget::Var,
            '$!declaration',
            nqp::chars($name) == 1
              ?? RakuAST::VarDeclaration::Anonymous.new(
                   :scope('anon'),
                   :sigil($name),
                   :type(Mu),
                   :is-parameter,
                 )
              !! RakuAST::VarDeclaration::Simple.new(
                  :scope($obj.scope),
                  :desigilname(RakuAST::Name.from-identifier($obj.desigilname)),
                  :$sigil,
                  :$twigil,
                  :type(Mu),
                  :$forced-dynamic,
                  :is-parameter,
                )
        );
        $obj
    }

    # Can be resolved if the parameter is not anonymous
    method can-be-resolved() {
        nqp::not_i(
          nqp::istype($!declaration,RakuAST::VarDeclaration::Anonymous)
        )
    }

    method lexical-name() {
        $!name
    }

    method introspection-name() {
        self.can-be-resolved ?? $!name !! nqp::null_s
    }

    method scope() {
        $!scope // self.default-scope
    }

    method replace-scope($scope) {
        nqp::bindattr_s(self, RakuAST::ParameterTarget::Var, '$!scope', $scope);
        if $!declaration {
            $!declaration.replace-scope($scope);
        }
    }

    # Generate a lookup of this parameter, already resolved to this declaration.
    method generate-lookup() {
        $!declaration.generate-lookup
    }

    method sigil() {
        nqp::substr($!name, 0, 1)
    }

    method twigil() {
        if nqp::chars($!name) > 2 {
            my str $twigil := nqp::substr($!name, 1, 1);
            nqp::index('.!^:*?=~', $twigil) >= 0 ?? $twigil !! ''
        }
        else {
            ''
        }
    }

    method desigilname() {
        nqp::substr($!name, self.twigil ?? 2 !! 1)
    }

    method can-be-bound-to() {
        $!is-bindable
    }

    method build-bind-exception(RakuAST::Resolver $resolver) {
        $resolver.build-exception: 'X::Bind::Rebind',
            :target(self.lexical-name)
    }

    method set-type(RakuAST::Type $type) {
        nqp::bindattr(self, RakuAST::ParameterTarget::Var, '$!type', $type);
        $!declaration.set-type($type) if $!declaration;
        Nil
    }

    method set-name(str $name) {
        nqp::bindattr_s(self, RakuAST::ParameterTarget::Var, '$!name', $name);
    }

    method set-container-type(Mu $type, Mu $of) {
        nqp::bindattr(self, RakuAST::ParameterTarget::Var, '$!type', $type);
        nqp::bindattr(self, RakuAST::ParameterTarget::Var, '$!of', $of);
    }

    method set-bindable(Bool $bindable) {
        nqp::bindattr(self, RakuAST::ParameterTarget::Var, '$!is-bindable', $bindable);
        $!declaration.set-bindable($bindable);
    }

    method set-rw() {
        $!declaration.set-rw;
    }

    method attach(RakuAST::Resolver $resolver) {
        $!declaration.attach($resolver);
    }

    method PRODUCE-META-OBJECT() {
        nqp::bindattr(
            self,
            RakuAST::ParameterTarget::Var,
            '$!of',
            $!type.resolution.compile-time-value
        ) if $!type && nqp::eqaddr($!of, Mu);

        $!declaration.meta-object
    }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        $!declaration.IMPL-QAST-DECL($context)
    }

    method IMPL-BIND-QAST(RakuAST::IMPL::QASTContext $context, Mu $source-qast) {
        $!declaration.IMPL-BIND-QAST($context, $source-qast)
    }

    method IMPL-LOOKUP-QAST(RakuAST::IMPL::QASTContext $context) {
        $!declaration.IMPL-LOOKUP-QAST($context)
    }

    method PERFORM-BEGIN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        my $var := $!declaration;
        for self.IMPL-UNWRAP-LIST(self.traits) {
            $var.add-trait($_);
        }
    }

    method PERFORM-CHECK(
      RakuAST::Resolver $resolver,
      RakuAST::IMPL::QASTContext $context
    ) {
        # Check for illegal post declaration is actually performed by RakuAST::Scope
        # Need PERFORM-CHECK because we're a RakuAST::CheckTime and we need to be
        # that to carry the sorry generated by the check.
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        [ self.IMPL-SIGIL-LOOKUP ]
    }

    method IMPL-SIGIL-TYPE() {
       self.get-implicit-lookups.AT-POS(0).resolution.compile-time-value
    }

    method default-scope() { 'my' }

    method allowed-scopes() { self.IMPL-WRAP-LIST(['my', 'our', 'has', 'HAS']) }

    method is-begin-performed-before-children() { True }

    method visit-children(Code $visitor) {
        # We don't want to visit the declaration if it is a topic variable,
        # as that will result in multiple declarations because blocks already
        # automatically declare a `$_`. And if the signature is not on a block
        # it will still find a lexical `$_` somewhere in scope.
        $visitor($!declaration) unless $!name eq '$_';
    }
}

# A binding of a parameter into a lexical term.
class RakuAST::ParameterTarget::Term
  is RakuAST::ParameterTarget
  is RakuAST::Declaration
{
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

    method visit-children(Code $visitor) {
        $visitor($!name);
    }
}

# These nodes generate their own unique names by default but can be passed existing
# names in order to reference existing RakuAST::ParamaterTarget::Whatever nodes.
class RakuAST::ParameterTarget::Whatever
  is RakuAST::ParameterTarget::Term
{
    method new(str $name) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::ParameterTarget::Term, '$!name',
            RakuAST::Name.from-identifier($name // QAST::Node.unique('$whatevercode_arg')));
        $obj
    }

    # Generate a lookup of this parameter, already resolved to this declaration.
    method generate-lookup() {
        my $name := nqp::getattr(self, RakuAST::ParameterTarget::Term, '$!name');
        my $lookup := RakuAST::WhateverCode::Argument.new($name);
        $lookup.set-resolution(self);
        $lookup
    }
}

# Marker for all kinds of slurpy behavior.
class RakuAST::Parameter::Slurpy {

    # These classes purely exist as markers and don't need to be
    # instantiated.  However, some people might do that and then
    # find deparsing doesn't work because deparsing only checks
    # for the type objects.  Alternately, we could make calling
    # .new here a worry, but that also seems a bit over the top.
    # So just return the type object as if .new was never called.
    method new() { self }

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
class RakuAST::Parameter::Slurpy::Flattened
  is RakuAST::Parameter::Slurpy
{
    method IMPL-FLAGS(str $sigil) {
        $sigil eq '@' ?? nqp::const::SIG_ELEM_SLURPY_POS !!
        $sigil eq '%' ?? nqp::const::SIG_ELEM_SLURPY_NAMED !!
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
class RakuAST::Parameter::Slurpy::Unflattened
  is RakuAST::Parameter::Slurpy
{
    method IMPL-FLAGS(str $sigil) {
        $sigil eq '@' ?? nqp::const::SIG_ELEM_SLURPY_LOL !! 0
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
class RakuAST::Parameter::Slurpy::SingleArgument
  is RakuAST::Parameter::Slurpy
{
    method IMPL-FLAGS(str $sigil) {
        $sigil eq '@' || $sigil eq ''
          ?? nqp::const::SIG_ELEM_SLURPY_ONEARG
          !! 0
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
class RakuAST::Parameter::Slurpy::Capture
  is RakuAST::Parameter::Slurpy
{
    method IMPL-FLAGS(str $sigil) {
        nqp::const::SIG_ELEM_IS_CAPTURE
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
class RakuAST::ParameterDefaultThunk
  is RakuAST::ExpressionThunk
{
    has RakuAST::Parameter $!parameter;

    method new(RakuAST::Parameter $parameter) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::ParameterDefaultThunk, '$!parameter', $parameter);
        $obj
    }

    method thunk-kind() {
        'Parameter default'
    }

    method thunk-details() {
        ''
    }

    method IMPL-THUNK-META-OBJECT-PRODUCED(Mu $code) {
        nqp::bindattr($!parameter.meta-object, Parameter, '$!default_value', $code);
    }
}
