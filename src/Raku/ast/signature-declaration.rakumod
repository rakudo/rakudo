class RakuAST::VarDeclaration::Signature
  is RakuAST::Term
  does RakuAST::Declaration
  does RakuAST::ImplicitLookups
  does RakuAST::ImplicitDeclarations
  does RakuAST::TraitTarget
  does RakuAST::BeginTime
  does RakuAST::StateInitGuard
{
    has RakuAST::Signature $.signature;
    has RakuAST::Type $.type;
    has RakuAST::Initializer $.initializer;
    has RakuAST::Package $!attribute-package;
    has Mu $!package;
    # True for the signature literal form `my :($a, $b)`, which binds only
    # and requires an initializer, versus the list form `my ($a, $b)`.
    has Bool $.sig-literal;

    method new(RakuAST::Signature :$signature!, RakuAST::Type :$type, RakuAST::Initializer :$initializer,
               str :$scope, Bool :$sig-literal) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::VarDeclaration::Signature, '$!signature', $signature);
        $obj.replace-scope($scope);
        nqp::bindattr($obj, RakuAST::VarDeclaration::Signature, '$!type', $type // RakuAST::Type);
        nqp::bindattr($obj, RakuAST::VarDeclaration::Signature, '$!initializer',
            $initializer // RakuAST::Initializer);
        nqp::bindattr($obj, RakuAST::VarDeclaration::Signature, '$!sig-literal',
            $sig-literal ?? True !! False);
        # The variable a parameter declares takes the declared type and
        # makes a subset of it from the where constraint, and a walk
        # begins it before this declaration
        my @parameters;
        $signature.IMPL-COLLECT-PARAMETERS(@parameters);
        for @parameters {
            if $_.target {
                if $type {
                    $_.target.set-type($type, :outer);
                    # a slurpy with a type is a compile error and a
                    # capture takes any type, so only their variables
                    # carry it
                    $_.IMPL-SET-OUTER-TYPE($type)
                        if nqp::eqaddr($_.slurpy, RakuAST::Parameter::Slurpy);
                }
                $_.target.set-where($_.where) if $_.where;
                $_.target.set-var-declaration;
            }
        }
        # A parameter that is assigned to, not bound, holds a container
        # of its own, decided before its meta-object exists
        unless nqp::isconcrete($initializer) && $initializer.is-binding {
            for @parameters {
                $_.set-default-rw;
            }
        }
        $obj
    }

    method visit-children(Code $visitor) {
        $visitor($!signature);
        my $type := $!type;
        $visitor($type) if nqp::isconcrete($type);
        my $initializer := $!initializer;
        $visitor($initializer) if nqp::isconcrete($initializer);
        self.visit-traits($visitor);
    }

    method default-scope() {
        'my'
    }

    method allowed-scopes() {
        self.IMPL-WRAP-LIST(['my', 'state', 'our', 'has', 'HAS'])
    }

    method is-simple-lexical-declaration() {
        # The ParameterTargets in our signature will take care of declarations
        False
    }

    method is-lexical(--> Bool) {
        # Overridden here because our-scoped variables are really lexical aliases.
        my constant SCOPES := nqp::hash('my', 1, 'state', 1, 'our', 1);
        nqp::existskey(SCOPES,self.scope)
    }

    # A list declaration such as `my (::T, $x) := ...` binds its type captures
    # into the enclosing scope. Surface them so they get a frame slot, matching
    # how a routine or block declares its own signature's captures.
    method PRODUCE-IMPLICIT-DECLARATIONS() {
        my @declarations;
        self.signature.IMPL-COLLECT-TYPE-CAPTURES(@declarations);
        nqp::push(@declarations, self.IMPL-STATE-INIT-GUARD) if self.IMPL-GUARDS-STATE-INIT;
        @declarations
    }

    method PRODUCE-IMPLICIT-LOOKUPS() {
        my @lookups;
        # If it's our-scoped, we need the package to bind it from.
        my str $scope := self.scope;
        if $scope eq 'our' {
            @lookups.push(RakuAST::Var::Compiler::Lookup.new('$?PACKAGE'));
        }
        # If we have a type, we need to resolve that.
        elsif $!type {
            @lookups.push($!type);
        }
        # If we're has/HAS scope, we need Nil to evaluate to.
        if $scope eq 'has' || $scope eq 'HAS' {
            @lookups.push(RakuAST::Type::Setting.from-identifier('Nil'));
        }
        @lookups
    }

    # A declaration taking over the default of its parameter covers the
    # text of the parameter, default included, and so does its target.
    method IMPL-WIDEN-TO-PARAMETER(RakuAST::Node $declaration, RakuAST::Node $param) {
        my $param-origin := $param.origin;
        if nqp::isconcrete($param-origin) {
            for [$param.target, $declaration] {
                my $origin := $_.origin;
                if nqp::isconcrete($origin) {
                    nqp::bindattr_i($origin, RakuAST::Origin, '$!from', $param-origin.from)
                      if $param-origin.from < $origin.from;
                    nqp::bindattr_i($origin, RakuAST::Origin, '$!to', $param-origin.to)
                      if $param-origin.to > $origin.to;
                }
            }
        }
    }

    method PERFORM-BEGIN(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        my $traits := self.IMPL-UNWRAP-LIST(self.traits);
        my str $scope := self.scope;
        my @parameters;
        self.signature.IMPL-COLLECT-PARAMETERS(@parameters);
        for @parameters -> $param {
            if $param.target {
                $param.target.replace-scope($scope);
                for $traits {
                    $param.target.add-trait(nqp::clone($_));
                }
            }
        }
        if $scope eq 'has' || $scope eq 'HAS' {
            my $attribute-package := $resolver.find-attach-target('package');
            if $attribute-package {
                nqp::bindattr(self, RakuAST::VarDeclaration::Signature, '$!attribute-package',
                    $attribute-package);
                # The declarator scope is only known here on the owning
                # declaration; push it into each target before their begin
                # time turns them into attribute declarations. The targets
                # find their own attribute package at parse time.
                for self.IMPL-UNWRAP-LIST(self.signature.parameters) {
                    if $_.target && nqp::istype($_.target, RakuAST::ParameterTarget::Var) {
                        $_.target.replace-scope($scope);
                    }
                }
            }
            else {
                # Each named parameter target declares its own attribute,
                # whose begin time reports the missing package.
            }

            # An attribute declaration list is not a callable signature:
            # move each parameter's traits and default onto the attribute
            # declaration it carries, and turn the required marker into
            # the `is required` trait, so `has ($.a is rw, $.b = 42, $.c!)`
            # means the same as separate `has` declarations.
            for self.IMPL-UNWRAP-LIST(self.signature.parameters) -> $param {
                my $target := $param.target;
                if $target
                    && nqp::istype($target, RakuAST::ParameterTarget::Var)
                    && $target.declaration {
                    my $declaration := $target.declaration;
                    for self.IMPL-UNWRAP-LIST($param.traits) {
                        $declaration.add-trait($_);
                    }
                    $param.set-traits([]);
                    if $param.is-declared-required {
                        $declaration.add-trait(RakuAST::Trait::Is.new(
                            :name(RakuAST::Name.from-identifier('required'))));
                    }
                    $param.clear-optionality;
                    if $param.default {
                        $declaration.set-initializer(
                            RakuAST::Initializer::Assign.new($param.default));
                        self.IMPL-WIDEN-TO-PARAMETER($declaration, $param);
                        $param.set-default(RakuAST::Expression);
                    }
                    $param.IMPL-SET-ATTRIBUTE-DECLARATION;
                }
            }
        }
        elsif $scope eq 'our' {
            my $package := $resolver.current-package;
            # There is always a package, even if it's just GLOBALish
            nqp::bindattr(self, RakuAST::VarDeclaration::Signature, '$!package',
                $package);
        }

        # A lexical list declaration without a binding initializer is not
        # destructuring: `my ($a is default(42), $b = 5) = 1` is a list
        # of declarations followed by list assignment. Move each
        # parameter's traits and default onto the variable declaration it
        # carries, and drop optionality markers, which only mean something
        # when binding.
        if ($scope eq 'my' || $scope eq 'state' || $scope eq 'our')
            && !$!sig-literal
            && !(nqp::isconcrete($!initializer) && $!initializer.is-binding) {
            for self.IMPL-UNWRAP-LIST(self.signature.parameters) -> $param {
                my $target := $param.target;
                if $target
                    && nqp::istype($target, RakuAST::ParameterTarget::Var)
                    && $target.declaration {
                    my $declaration := $target.declaration;
                    # A trait with no variable meaning stays on the
                    # parameter, where it is inert, since nothing ever
                    # binds through the signature.
                    my constant PARAM-ONLY-TRAITS := nqp::hash(
                        'readonly', 1,  'rw',  1,  'copy',   1,
                        'required', 1,  'raw', 1,  'onearg', 1,
                        'item',     1
                    );
                    my @keep;
                    for self.IMPL-UNWRAP-LIST($param.traits) {
                        if nqp::istype($_, RakuAST::Trait::Is)
                            && $_.name
                            && nqp::existskey(PARAM-ONLY-TRAITS, $_.name.canonicalize) {
                            nqp::push(@keep, $_);
                        }
                        else {
                            $declaration.add-trait($_);
                        }
                    }
                    $param.set-traits(@keep);
                    $param.clear-optionality;
                    if $param.default {
                        $declaration.set-initializer(
                            RakuAST::Initializer::Assign.new($param.default));
                        self.IMPL-WIDEN-TO-PARAMETER($declaration, $param);
                        $param.set-default(RakuAST::Expression);
                    }
                }
            }
        }

        # a constraint a parameter received after this declaration was
        # built is too late for its variable, whose container has been
        # made by now
        for @parameters -> $param {
            if $param.where
              && nqp::istype($param.target, RakuAST::ParameterTarget::Var)
              && (my $declaration := $param.target.declaration)
              && !nqp::eqaddr($declaration.where, $param.where) {
                self.add-sorry: $resolver.build-exception: 'X::AdHoc',
                  payload => "Cannot constrain variable '" ~ $declaration.name
                    ~ "' with a where clause after its declaration was built."
                    ~ " Pass the constraint to RakuAST::Parameter.new";
            }
        }

        my $binding := self.initializer && self.initializer.is-binding;
        for self.IMPL-UNWRAP-LIST(self.signature.parameters) -> $param {
            if nqp::defined($param.value) && !$param.target {
                # We don't have a target that can carry the where clause. Have to synthesize one here
                my $value := $param.value;
                my $type := $value.WHAT;
                my $type-name := $value.HOW.name($value);
                my $type-ast := RakuAST::Type::Simple.from-identifier($type-name);
                $type-ast.set-resolution(RakuAST::Declaration::ResolvedConstant.new(compile-time-value => $type));
                my $where := RakuAST::Term::Declaration.new(RakuAST::Declaration::ResolvedConstant.new(compile-time-value => $value));
                my $target := RakuAST::ParameterTarget::Var.new(:name('$'), :var-declaration);
                $param.set-target($target);
                $param.set-where($where);
                $target.set-type($type-ast);
                $target.set-where($where);
                $target.IMPL-BEGIN($resolver, $context);
            }
            $param.set-bindable(False) if $binding;
            $param.set-default-rw unless $binding;
            $param.target.set-var-declaration if $param.target;
        }
        self.signature.to-begin-time($resolver, $context);
    }

    method PERFORM-CHECK(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        self.add-trait-sorries;

        unless $!initializer {
            if $!sig-literal {
                self.add-sorry($resolver.build-exception:
                    'X::Syntax::Variable::SignatureWithoutInitializer');
            }
            else {
                for self.IMPL-UNWRAP-LIST(self.signature.parameters) -> $param {
                    if nqp::istype($param.target, RakuAST::ParameterTarget::Term) {
                        self.add-sorry($resolver.build-exception: 'X::Syntax::Term::MissingInitializer');
                        last;
                    }
                }
            }
        }

        if $!sig-literal && nqp::istype($!initializer, RakuAST::Initializer::Assign) {
            self.add-sorry($resolver.build-exception:
                'X::Syntax::Variable::SignatureAssignment');
        }

        # Attributes only exist once there is an instance, so a has
        # scoped list offers nothing to assign to.
        my str $scope := self.scope;
        if ($scope eq 'has' || $scope eq 'HAS')
            && nqp::isconcrete($!initializer)
            && !$!initializer.is-binding {
            self.add-sorry($resolver.build-exception: 'X::AdHoc',
                payload => "Cannot assign to a list of 'has' scoped declarations");
        }
    }

    # The list of what the parameters store into: the variable a
    # parameter declares, a typed placeholder for an anonymous parameter,
    # and for a sub-signature its own list, or when assigning a
    # placeholder that the sub-signature's list is assigned from after
    # the whole list, queued in @groups with its source. Terms go to
    # @terms to be rebound afterwards, declarations with an initializer
    # to @var-inits.
    method IMPL-STORE-LIST-QAST(
      RakuAST::IMPL::QASTContext $context,
      @params,
      @terms,
      @groups,
      @var-inits,
      int $attribute,
      int $assign
    ) {
        my $value-list := QAST::Op.new( :op('call'), :name('&infix:<,>') );
        for @params {
            my $target := $_.target;
            nqp::push(@terms, $target) if nqp::istype($target, RakuAST::ParameterTarget::Term);
            my $nested := $_.sub-signature
                ?? self.IMPL-UNWRAP-LIST($_.sub-signature.parameters)
                !! Mu;
            if $target {
                # A default from the parameter list is the variable's
                # initializer, so emit the declaration expression, which
                # runs it, rather than a plain lookup. An attribute
                # declaration's initializer runs at construction, not here.
                my $declaration := nqp::istype($target, RakuAST::ParameterTarget::Var)
                    ?? $target.declaration
                    !! RakuAST::VarDeclaration::Simple;
                if !$attribute
                    && nqp::isconcrete($declaration)
                    && nqp::isconcrete($declaration.initializer) {
                    nqp::push(@var-inits, $declaration);
                    $value-list.push: $declaration.IMPL-TO-QAST($context);
                }
                else {
                    $value-list.push: $target.IMPL-LOOKUP-QAST($context);
                }
                if $nested {
                    $assign
                        ?? nqp::push(@groups, [$target.IMPL-LOOKUP-QAST($context), $nested])
                        !! self.IMPL-STORE-LIST-QAST($context, $nested, @terms, @groups, @var-inits, $attribute, $assign);
                }
            }
            elsif $nested {
                if $assign {
                    # the placeholder holds Nil until assigned, so a
                    # short list leaves the sub-signature's variables at
                    # their defaults
                    my str $name := QAST::Node.unique('list_declaration_group');
                    my $desc := RakuAST::IMPL::Containers.create-descriptor(
                        :of(Mu), :default(Nil), :dynamic(0), :name('anon'));
                    $context.ensure-sc($desc);
                    $value-list.push: QAST::Op.new(
                        :op('bind'),
                        QAST::Var.new( :name($name), :scope('local'), :decl('var') ),
                        QAST::Op.new(
                            :op('p6scalarfromdesc'), QAST::WVal.new(:value($desc))
                        )
                    );
                    nqp::push(@groups, [QAST::Var.new( :name($name), :scope('local') ), $nested]);
                }
                else {
                    $value-list.push: self.IMPL-STORE-LIST-QAST($context, $nested, @terms, @groups, @var-inits, $attribute, $assign);
                }
            }
            elsif $_.type {
                # Anonymous typed parameter (e.g. Int or Any:D) in a
                # list declaration. Create a typed placeholder container
                # so it consumes and type-checks the RHS value at this
                # position.
                my $of := $_.type.meta-object;
                my $desc := RakuAST::IMPL::Containers.create-descriptor(
                    :$of, :default($of), :dynamic(0), :name('anon'));
                $context.ensure-sc($desc);
                $value-list.push: QAST::Op.new(
                    :op('p6scalarfromdesc'), QAST::WVal.new(:value($desc))
                );
            }
        }
        $value-list
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my @params := self.IMPL-UNWRAP-LIST($!signature.parameters);
        my @terms;
        my @groups;
        my @var-inits;
        my str $scope := self.scope;
        my int $attribute := $scope eq 'has' || $scope eq 'HAS';
        my int $assign := nqp::istype($!initializer, RakuAST::Initializer::Assign);
        my $value-list := self.IMPL-STORE-LIST-QAST($context,
            @params, @terms, @groups, @var-inits, $attribute, $assign);
        my int $has-var-inits := nqp::elems(@var-inits) ?? 1 !! 0;

        # With no initializer a lexical declaration's value is the list of its
        # own containers, so it can be used as an rvalue, e.g. bound on the
        # right of `:=` in `my (\a, \b) := my ($x, $y)`. A sunk declaration
        # wants nothing, so it keeps the null it used to always return,
        # unless a variable's initializer needs to run. An attribute declaration
        # has no container to hand back at class-body time, so it stays
        # null.
        unless nqp::isconcrete($!initializer) {
            return $attribute || self.sunk && !$has-var-inits
                ?? QAST::Op.new(:op<null>)
                !! $value-list;
        }

        my $perform-init-qast;
        if nqp::istype($!initializer, RakuAST::Initializer::Assign) {
            my $init-qast := $!initializer.IMPL-TO-QAST($context);
            my $list := QAST::Op.new( :op('p6store'), $value-list, $init-qast);
            if nqp::elems(@groups) || nqp::elems(@terms) {
                my $stmts := QAST::Stmts.new(:resultchild(0), $list);
                # a sub-signature inside a group queues its own group
                my int $i;
                while $i < nqp::elems(@groups) {
                    my $group := @groups[$i];
                    $stmts.push(QAST::Op.new(
                        :op('p6store'),
                        self.IMPL-STORE-LIST-QAST($context,
                            $group[1], @terms, @groups, @var-inits, $attribute, $assign),
                        QAST::Op.new( :op('decont'), $group[0] )
                    ));
                    $i++;
                }
                for @terms {
                    $stmts.push(
                        $_.IMPL-BIND-QAST(
                            $context,
                            QAST::Op.new(:op('decont'), $_.IMPL-LOOKUP-QAST($context))
                        )
                    );
                }
                $perform-init-qast := $stmts;
            }
            else {
                $perform-init-qast := $list;
            }
        }
        elsif nqp::istype($!initializer, RakuAST::Initializer::Bind) {
            $!signature.IMPL-BIND-LITERAL-DEFAULTS;
            my $signature := $!signature.meta-object;
            $context.ensure-sc($signature);
            my $init-qast := $!initializer.IMPL-TO-QAST($context);
            $perform-init-qast := QAST::Op.new(
                :op('p6bindcaptosig'),
                QAST::WVal.new( :value($signature) ),
                QAST::Op.new(
                    :op('callmethod'), :name('Capture'),
                    $init-qast
                )
            );
        }
        else {
            nqp::die('Not yet supported signature initializer: ' ~ $!initializer.HOW.name($!initializer));
        }
        if self.scope eq 'state' {
            # the value is the containers, built again since the
            # assignment's targets declare their placeholder locals and
            # would yield those instead
            my @later-terms;
            my @later-groups;
            my @later-var-inits;
            $perform-init-qast := QAST::Op.new(
              :op('if'),
              self.IMPL-STATE-INIT-CONDITION-QAST($context),
              $perform-init-qast,
              self.IMPL-STORE-LIST-QAST($context, @params,
                  @later-terms, @later-groups, @later-var-inits, $attribute, 0)
            )
        }
        $perform-init-qast
    }

    method needs-sink-call() { False }
}
