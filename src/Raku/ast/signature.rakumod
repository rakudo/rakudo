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
            # TODO calculate properly here
            $arity++;
        }
        nqp::box_i($arity, Int)
    }

    method count() {
        my int $count := 0;
        for self.IMPL-UNWRAP-LIST($!parameters) {
            # TODO calculate properly here
            $count++;
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
class RakuAST::Parameter is RakuAST::Meta {
    has RakuAST::ParameterTarget $.target;
    has Bool $.invocant;

    method new(RakuAST::ParameterTarget :$target, Bool :$invocant) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Parameter, '$!target', $target // RakuAST::ParameterTarget);
        nqp::bindattr($obj, RakuAST::Parameter, '$!invocant', $invocant ?? True !! False);
        $obj
    }

    method visit-children(Code $visitor) {
        $visitor($!target);
    }

    method PRODUCE-META-OBJECT() {
        my $parameter := nqp::create(Parameter);
        # TODO set it up
        $parameter
    }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        # HLL-ize and decont the parameter.
        # TODO this is a cheat, we need to handle parameters in all their diversity
        my $name := QAST::Node.unique("__lowered_param");
        my $param-qast := QAST::Var.new( :decl('param'), :scope('local'), :$name );
        my $temp-qast := QAST::Var.new( :name($name), :scope('local') );
        $param-qast.push(QAST::Op.new(
            :op('bind'),
            $temp-qast,
            QAST::Op.new( :op('decont'), QAST::Op.new( :op('hllize'), $temp-qast ) )
        ));
        
        # Bind parameter into its target.
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
