# Done by anything that implies a lexical scope.
class RakuAST::LexicalScope is RakuAST::Node {
    # Caching of lexical declarations in this scope due to AST nodes.
    has List $!declarations-cache;
    has Mu $!lexical-lookup-hash;

    # Generated lexical declarations thanks to BEGIN-time constructs
    # (from `use`, `import`, generated `proto` subs, etc.)
    has Mu $!generated-lexical-declarations;
    has Mu $!generated-lexical-lookup-hash;

    # Handlers related to this scope.
    has int $!need-succeed-handler;
    has Mu $!catch-handlers;
    has Mu $!control-handlers;

    method IMPL-QAST-DECLS(RakuAST::IMPL::QASTContext $context) {
        my $stmts := QAST::Stmts.new();

        # Visit code objects that need to make a declaration entry. We don't
        # visit any code objects immediately under an ImmediateBlockUser (but
        # should visit their other nodes).
        my @code-todo := [self];
        while @code-todo {
            my $visit := @code-todo.shift;
            $visit.visit-children: -> $node {
                if nqp::istype($node, RakuAST::Code) {
                    unless nqp::istype($visit, RakuAST::IMPL::ImmediateBlockUser) {
                        $stmts.push($node.IMPL-QAST-DECL-CODE($context));
                    }
                }
                unless nqp::istype($node, RakuAST::LexicalScope) {
                    @code-todo.push($node);
                }
            }
        }

        # Visit declarations and produce declaration QAST.
        for self.IMPL-UNWRAP-LIST(self.ast-lexical-declarations()) {
            $stmts.push($_.IMPL-QAST-DECL($context));
        }
        for self.IMPL-UNWRAP-LIST(self.generated-lexical-declarations()) {
            $stmts.push($_.IMPL-QAST-DECL($context));
        }

        # If there's handler block declarations, add those.
        if $!catch-handlers {
            $stmts.push(QAST::Op.new(
                :op('bind'),
                QAST::Var.new( :decl('var'), :name('__CATCH_HANDLER'), :scope('lexical') ),
                $!catch-handlers[0].body.IMPL-CLOSURE-QAST()
            ));
        }
        if $!control-handlers {
            $stmts.push(QAST::Op.new(
                :op('bind'),
                QAST::Var.new( :decl('var'), :name('__CONTROL_HANDLER'), :scope('lexical') ),
                $!control-handlers[0].body.IMPL-CLOSURE-QAST()
            ));
        }

        $stmts
    }

    # Find the lexical declarations due to AST nodes. This does not include
    # any generated ones thanks to BEGIN-time effects (for example, those
    # due to `import` or `use`). These are cached rather than walking the
    # tree every time.
    method ast-lexical-declarations() {
        unless nqp::isconcrete($!declarations-cache) {
            my @declarations;
            self.visit: -> $node {
                if nqp::istype($node, RakuAST::Declaration) && $node.is-simple-lexical-declaration {
                    nqp::push(@declarations, $node);
                }
                if $node =:= self || !nqp::istype($node, RakuAST::LexicalScope) {
                    if nqp::istype($node, RakuAST::ImplicitDeclarations) {
                        for self.IMPL-UNWRAP-LIST($node.get-implicit-declarations()) -> $decl {
                            if $decl.is-simple-lexical-declaration {
                                nqp::push(@declarations, $decl);
                            }
                        }
                    }
                    1 # visit children
                }
                else {
                    0 # it's an inner scope, don't visit its children
                }
            }
            nqp::bindattr(self, RakuAST::LexicalScope, '$!declarations-cache', @declarations);
        }
        $!declarations-cache
    }

    # Get a list of generated lexical declarations. These are symbols that are
    # produced thanks to BEGIN-time side-effects.
    method generated-lexical-declarations() {
        self.IMPL-WRAP-LIST($!generated-lexical-declarations // [])
    }

    # Add a generated lexical declaration.
    method add-generated-lexical-declaration(RakuAST::Declaration $declaration) {
        unless $!generated-lexical-declarations {
            nqp::bindattr(self, RakuAST::LexicalScope, '$!generated-lexical-declarations', []);
        }
        nqp::push($!generated-lexical-declarations, $declaration);
        nqp::bindattr(self, RakuAST::LexicalScope, '$!generated-lexical-lookup-hash', Mu);
        Nil
    }

    # Find a lexical, only considering those that are declared by AST nodes.
    # The lookup table is cached.
    method find-ast-lexical(Str $name) {
        my %lookup := $!lexical-lookup-hash;
        unless nqp::isconcrete(%lookup) {
            %lookup := {};
            for self.IMPL-UNWRAP-LIST(self.ast-lexical-declarations) {
                %lookup{$_.lexical-name} := $_;
            }
            nqp::bindattr(self, RakuAST::LexicalScope, '$!lexical-lookup-hash', %lookup);
        }
        %lookup{$name} // Nil
    }

    # Find a lexical, only considering those that are generated at BEGIN time.
    # This will be up to date.
    method find-generated-lexical(Str $name) {
        my %lookup := $!generated-lexical-lookup-hash;
        unless nqp::isconcrete(%lookup) {
            %lookup := {};
            for self.IMPL-UNWRAP-LIST(self.generated-lexical-declarations) {
                %lookup{$_.lexical-name} := $_;
            }
            nqp::bindattr(self, RakuAST::LexicalScope, '$!generated-lexical-lookup-hash', %lookup);
        }
        %lookup{$name} // Nil
    }

    # Find a lexical, regardless of whether it is declared by an AST node or
    # generated by a BEGIN-time effect. The AST lexical lookup table will be
    # cached on first use.
    method find-lexical(Str $name) {
        self.find-ast-lexical($name) // self.find-generated-lexical($name)
    }

    method require-succeed-handler() {
        nqp::bindattr_i(self, RakuAST::LexicalScope, '$!need-succeed-handler', 1);
        Nil
    }

    method attach-catch-handler(RakuAST::Statement::Catch $catch) {
        if $!catch-handlers {
            nqp::push($!catch-handlers, $catch);
        }
        else {
            nqp::bindattr(self, RakuAST::LexicalScope, '$!catch-handlers', [$catch]);
        }
        Nil
    }

    method attach-control-handler(RakuAST::Statement::Control $control) {
        if $!control-handlers {
            nqp::push($!control-handlers, $control);
        }
        else {
            nqp::bindattr(self, RakuAST::LexicalScope, '$!control-handlers', [$control]);
        }
        Nil
    }

    # This is only accurate after resolution - but since we only need it at
    # code-gen time, that's fine.
    method IMPL-HAS-CATCH-HANDLER() {
        $!catch-handlers ?? True !! False
    }

    method clear-handler-attachments() {
        nqp::bindattr_i(self, RakuAST::LexicalScope, '$!need-succeed-handler', 0);
        nqp::bindattr(self, RakuAST::LexicalScope, '$!catch-handlers', Mu);
        nqp::bindattr(self, RakuAST::LexicalScope, '$!control-handlers', Mu);
        Nil
    }

    method IMPL-WRAP-SCOPE-HANDLER-QAST(RakuAST::IMPL::QASTContext $context, Mu $statements,
                                        Bool :$is-handler) {
        # If it's an exception handler, add rethrow logic.
        if $is-handler {
            $statements := QAST::Stmts.new(
                # Set up a generic exception rethrow, so that exception handlers
                # from unwanted frames will get skipped if the code in our handler
                # throws an exception.
                QAST::Op.new(
                    :op('handle'),
                    $statements,
                    'CATCH',
                    QAST::Op.new(
                        :op('rethrow'),
                        QAST::Op.new( :op('exception') )
                    )
                ),
                # Otherwise, rethrow the exception if we reach the end of the handler
                # without `succeed`ing (that handler is wrapped outside of this one,
                # just below)
                QAST::Op.new(
                    :op('rethrow'),
                    QAST::Var.new( :name('EXCEPTION'), :scope('local') )
                )
            );
        }

        # Now wrap our own handlers.
        if $!need-succeed-handler || $!catch-handlers || $!control-handlers {
            my $handle := QAST::Op.new( :op('handle'), $statements );
            if $!need-succeed-handler {
                $handle.push('SUCCEED');
                $handle.push(QAST::Op.new( :op('getpayload'), QAST::Op.new( :op('exception') ) ));
            }
            if $!catch-handlers {
                self.IMPL-ADD-HANDLER($handle, 'CATCH');
            }
            if $!control-handlers {
                self.IMPL-ADD-HANDLER($handle, 'CONTROL');
            }
            $handle
        }
        else {
            $statements
        }
    }

    method IMPL-ADD-HANDLER(Mu $handle, str $handler) {
        $handle.push($handler);
        $handle.push(QAST::Stmt.new(
            QAST::Op.new(
                :op('call'),
                QAST::Var.new( :name('__' ~ $handler ~ '_HANDLER'), :scope('lexical') ),
                QAST::Op.new( :op('exception') )
            ),
            QAST::WVal.new( :value(Nil) )
        ));
    }
}

# Done by anything that is a declaration - that is, declares a symbol.
class RakuAST::Declaration is RakuAST::Node {
    has str $!scope;

    # Returns the default scope of this kind of declaration.
    method default-scope() {
        nqp::die('default-scope is not implemented on ' ~ self.HOW.name(self))
    }

    # Returns the list of allowed scopes for this kind of declaration.
    method allowed-scopes() {
        nqp::die('allowed-scopes is not implemented on ' ~ self.HOW.name(self))
    }

    # Gets the scope of this declaration.
    method scope() {
        my str $scope := $!scope;
        nqp::isnull_s($scope) || $scope eq ''
            ?? self.default-scope
            !! $scope
    }

    # Tests if this is a lexical declaration (`my` or `state` scope).
    method is-lexical() {
        my str $scope := self.scope;
        $scope eq 'my' || $scope eq 'state'
    }

    # Tests if this declaration should be gathered as a lexical declaration.
    # By default, anything that is lexically scoped will be, however some
    # things (such as packages) perform their own, more imperative, logic
    # for doing installation.
    method is-simple-lexical-declaration() {
        self.is-lexical
    }
}

# Done by anything that may make implicit declarations. For example, a package
# declares $?PACKAGE inside of it, a sub declares a fresh $_, $/, and $!, etc.
# While a declaration is considered something external to a node, and so exposed
# to the enclosing lexical scope, implicit declarations are considered as being
# on the inside; this makes a difference in the case the node is also doing
# RakuAST::LexicalScope and is thus a lexical scope boundary.
class RakuAST::ImplicitDeclarations is RakuAST::Node {
    has List $!implicit-declarations-cache;

    # A node typically implements this to specify the implicit declarations
    # that it makes. This is called once per instance of a node and then
    # remains constant. Nodes that may be mutated must instead implement
    # get-implicit-declarations and handle the caching themselves.
    method PRODUCE-IMPLICIT-DECLARATIONS() {
        self.IMPL-WRAP-LIST(nqp::list())
    }

    # Get a list of the implicit declarations.
    method get-implicit-declarations() {
        $!implicit-declarations-cache //
            nqp::bindattr(self, RakuAST::ImplicitDeclarations,
                '$!implicit-declarations-cache',
                self.PRODUCE-IMPLICIT-DECLARATIONS())
    }
}

# A lexical declaration that comes from an external symbol (for example, the
# setting or an EVAL).
class RakuAST::Declaration::External is RakuAST::Declaration {
    has str $.lexical-name;
    has Mu $!native-type;

    method new(str :$lexical-name, Mu :$native-type) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Declaration::External, '$!lexical-name', $lexical-name);
        nqp::bindattr($obj, RakuAST::Declaration::External, '$!native-type', $native-type);
        $obj
    }

    method IMPL-LOOKUP-QAST(RakuAST::IMPL::QASTContext $context, Mu :$rvalue) {
        my str $scope := 'lexical';
        unless $rvalue {
            # Potentially l-value native lookups need a lexicalref.
            if nqp::objprimspec($!native-type) {
                $scope := 'lexicalref';
            }
        }
        QAST::Var.new( :name($!lexical-name), :$scope, :returns($!native-type) )
    }

    method default-scope() { 'my' }

    method allowed-scopes() { self.IMPL-WRAP-LIST(['my']) }
}

# A lexical declaration that comes with an external symbol, which has a fixed
# value available during compilation.
class RakuAST::Declaration::External::Constant is RakuAST::Declaration::External
        is RakuAST::CompileTimeValue {
    has Mu $.compile-time-value;

    method new(str :$lexical-name!, Mu :$compile-time-value!) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Declaration::External, '$!lexical-name', $lexical-name);
        nqp::bindattr($obj, RakuAST::Declaration::External::Constant,
            '$!compile-time-value', $compile-time-value);
        $obj
    }

    method type() { $!compile-time-value.WHAT }

    method IMPL-LOOKUP-QAST(RakuAST::IMPL::QASTContext $context, Mu :$rvalue) {
        my $value := $!compile-time-value;
        $context.ensure-sc($value);
        QAST::WVal.new( :$value )
    }
}

# An imported lexical declaration. Has a compile-time value. Must create a
# lexical slot for itself in the scope it is installed in.
class RakuAST::Declaration::Import is RakuAST::Declaration::External::Constant {
    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        QAST::Var.new(
            :scope('lexical'), :decl('static'), :name(self.lexical-name),
            :value(self.compile-time-value)
        )
    }
}

# A lexical declaration that points to a package. Generated as part of package
# installation in RakuAST::Package, and installed as a generated lexical in a
# RakuAST::LexicalScope.
class RakuAST::Declaration::LexicalPackage is RakuAST::Declaration is RakuAST::CompileTimeValue {
    has str $.lexical-name;
    has Mu $.compile-time-value;

    method new(str :$lexical-name!, Mu :$compile-time-value!) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Declaration::LexicalPackage,
            '$!lexical-name', $lexical-name);
        nqp::bindattr($obj, RakuAST::Declaration::LexicalPackage,
            '$!compile-time-value', $compile-time-value);
        $obj
    }

    method type() { $!compile-time-value.WHAT }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        QAST::Var.new(
            :scope('lexical'), :decl('static'), :name($!lexical-name),
            :value($!compile-time-value)
        )
    }

    method IMPL-LOOKUP-QAST(RakuAST::IMPL::QASTContext $context, Mu :$rvalue) {
        my $value := $!compile-time-value;
        $context.ensure-sc($value);
        QAST::WVal.new( :$value )
    }
}

# A constant resolved in a package. Has a compile time value, and the resolution
# always compiles into that. The name it was looked up under is not preserved.
class RakuAST::Declaration::PackageConstant is RakuAST::Declaration is RakuAST::CompileTimeValue {
    has Mu $.compile-time-value;

    method new(Mu :$compile-time-value!) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Declaration::PackageConstant,
            '$!compile-time-value', $compile-time-value);
        $obj
    }

    method type() { $!compile-time-value.WHAT }

    method IMPL-LOOKUP-QAST(RakuAST::IMPL::QASTContext $context, Mu :$rvalue) {
        my $value := $!compile-time-value;
        $context.ensure-sc($value);
        QAST::WVal.new( :$value )
    }

    method default-scope() { 'package' }

    method allowed-scopes() { self.IMPL-WRAP-LIST(['package']) }
}

# Done by anything that is a lookup of a symbol. May or may not need resolution
# at compile time.
class RakuAST::Lookup is RakuAST::Node {
    has RakuAST::Declaration $!resolution;

    method needs-resolution() { True }

    method is-resolved() {
        nqp::isconcrete($!resolution) ?? True !! False
    }

    method resolution() {
        nqp::isconcrete($!resolution)
            ?? $!resolution
            !! nqp::die('This element has not been resolved. Type: ' ~ self.HOW.name(self))
    }

    method set-resolution(RakuAST::Declaration $resolution) {
        nqp::bindattr(self, RakuAST::Lookup, '$!resolution', $resolution)
    }
}

# Some program elements are not really lookups, but require the resolution
# of symbols as part of their compilation. For example, a positional regex
# access depends on `&postcircumfix:<[ ]>` and `$/`, while an `unless`
# statement depends on `Empty` (as that's what it evaluates to in the case
# there the condition is not matched).
class RakuAST::ImplicitLookups is RakuAST::Node {
    has List $!implicit-lookups-cache;

    # A node typically implements this to specify the implicit lookups
    # that it needs. This is called once per instance of a node and then
    # remains constant. Nodes that may be mutated must instead implement
    # get-implicit-lookups and handle the caching themselves.
    method PRODUCE-IMPLICIT-LOOKUPS() {
        self.IMPL-WRAP-LIST(nqp::list())
    }

    # Get a list of the implicit lookups.
    method get-implicit-lookups() {
        $!implicit-lookups-cache //
            nqp::bindattr(self, RakuAST::ImplicitLookups, '$!implicit-lookups-cache',
                self.PRODUCE-IMPLICIT-LOOKUPS())
    }

    # Resolve the implicit lookups if needed.
    method resolve-implicit-lookups-with(RakuAST::Resolver $resolver) {
        for self.IMPL-UNWRAP-LIST(self.get-implicit-lookups()) {
            unless $_.is-resolved {
                $_.resolve-with($resolver);
            }
        }
    }
}
