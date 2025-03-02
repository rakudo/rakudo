# Done by anything that implies a lexical scope.
class RakuAST::LexicalScope
  is RakuAST::CheckTime
  is RakuAST::MayCreateBlock
  is RakuAST::Node
{
    # Caching of lexical declarations in this scope due to AST nodes.
    has List $!declarations-cache;
    has Mu $!lexical-lookup-hash;
    # Caching of lexical declarations and variables for checking order.
    has List $!variables-cache;

    # Generated lexical declarations thanks to BEGIN-time constructs
    # (from `use`, `import`, generated `proto` subs, etc.)
    has Mu $!generated-lexical-declarations;
    has Mu $!generated-lexical-lookup-hash;

    # Handlers related to this scope.
    has int $!need-succeed-handler;
    has Mu $!catch-handlers;
    has Mu $!control-handlers;

    has Bool $.fatal;

    method set-fatal(Bool $on) {
        nqp::bindattr(self, RakuAST::LexicalScope, '$!fatal', $on);
    }

    method creates-block() {
        True
    }

    method IMPL-QAST-DECLS(RakuAST::IMPL::QASTContext $context) {
        my $stmts := QAST::Stmts.new();

        # Visit declarations and produce declaration QAST.
        my %seen-decl;
        for self.IMPL-UNWRAP-LIST(self.ast-lexical-declarations()) {
            $stmts.push($_.IMPL-QAST-DECL($context)) unless $_ =:= self;
            %seen-decl{nqp::objectid($_)} := 1;
        }
        for self.IMPL-UNWRAP-LIST(self.generated-lexical-declarations()) {
            $stmts.push($_.IMPL-QAST-DECL($context)) unless $_ =:= self || %seen-decl{nqp::objectid($_)};
            %seen-decl{nqp::objectid($_)} := 1;
        }

        # Visit code objects that need to make a declaration entry. We don't
        # visit any code objects immediately under an ImmediateBlockUser (but
        # should visit their other nodes).
        my @code-todo := [self];
        while @code-todo {
            my $visit := @code-todo.shift;
            $visit.visit-children: -> $node {
                if nqp::istype($node, RakuAST::FakeSignature) {
                    $stmts.push($node.block.IMPL-QAST-DECL-CODE($context));
                }
                if nqp::istype($node, RakuAST::LexicalScope) {
                    if nqp::istype($node, RakuAST::TraitTarget) {
                        $node.visit-traits(-> $trait { @code-todo.push($trait) });
                    }
                }
                else {
                    unless nqp::istype($node, RakuAST::MayCreateBlock) && $node.creates-block {
                        @code-todo.push($node);
                    }
                }
            }
        }

        my $nested-blocks := self.IMPL-QAST-NESTED-BLOCK-DECLS($context);
        $stmts.push($nested-blocks) if nqp::elems($nested-blocks.list);

        # If there's handler block declarations, add those.
        if $!catch-handlers {
            $stmts.push(QAST::Op.new(
                :op('bind'),
                QAST::Var.new( :decl('var'), :name('__CATCH_HANDLER'), :scope('lexical') ),
                $!catch-handlers[0].body.IMPL-CLOSURE-QAST($context)
            ));
        }
        if $!control-handlers {
            $stmts.push(QAST::Op.new(
                :op('bind'),
                QAST::Var.new( :decl('var'), :name('__CONTROL_HANDLER'), :scope('lexical') ),
                $!control-handlers[0].body.IMPL-CLOSURE-QAST($context)
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
            my @variables;
            my @not-if-duplicate;
            self.visit-dfs: -> $node {
                if nqp::istype($node, RakuAST::Declaration) && $node.is-simple-lexical-declaration {
                    nqp::push(@declarations, $node);
                    nqp::push(@variables, $node) unless nqp::istype($node, RakuAST::Routine);
                }
                elsif nqp::istype($node, RakuAST::Var::Lexical) || nqp::istype($node, RakuAST::Var::Dynamic) {
                    nqp::push(@variables, $node);
                }
                if $node =:= self || !nqp::istype($node, RakuAST::LexicalScope) {
                    if nqp::istype($node, RakuAST::ImplicitDeclarations) {
                        for self.IMPL-UNWRAP-LIST($node.get-implicit-declarations()) -> $decl {
                            if $decl.is-simple-lexical-declaration {
                                if nqp::istype($decl, RakuAST::VarDeclaration::Implicit::BlockTopic) && $decl.IMPL-NOT-IF-DUPLICATE {
                                    nqp::push(@not-if-duplicate, $decl);
                                }
                                else {
                                    nqp::push(@declarations, $decl);
                                    nqp::push(@variables, $decl);
                                }
                            }
                        }
                    }
                    1 # visit children
                }
                else {
                    0 # it's an inner scope, don't visit its children
                }
            }
            for @not-if-duplicate -> $decl {
                my $found := 0;
                for @declarations {
                    if $_.lexical-name eq '$_' && !($_ =:= $decl) {
                        $found := 1;
                        last;
                    }
                }
                unless $found {
                    # Implicits are declared right at the start of a lexical scope anyway,
                    # so it should be safe to unshift them. We need them to be declared before
                    # they are first used.
                    nqp::unshift(@declarations, $decl);
                    nqp::unshift(@variables, $decl);
                }
            }
            nqp::bindattr(self, RakuAST::LexicalScope, '$!declarations-cache', @declarations);
            nqp::bindattr(self, RakuAST::LexicalScope, '$!variables-cache', @variables);
        }
        $!declarations-cache
    }

    # Get a list of generated lexical declarations. These are symbols that are
    # produced thanks to BEGIN-time side-effects.
    method generated-lexical-declarations() {
        self.IMPL-WRAP-LIST($!generated-lexical-declarations // [])
    }

    method lexical-declarations() {
        my $declarations := nqp::clone(self.IMPL-UNWRAP-LIST(self.ast-lexical-declarations));
        for ($!generated-lexical-declarations // []) {
            nqp::push($declarations, $_);
        }
        self.IMPL-WRAP-LIST($declarations)
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

    method merge-generated-lexical-declaration(RakuAST::Declaration $declaration, RakuAST::Resolver :$resolver!, :$force) {
        unless $!generated-lexical-declarations {
            nqp::bindattr(self, RakuAST::LexicalScope, '$!generated-lexical-declarations', []);
        }
        for $!generated-lexical-declarations {
            if ($_.lexical-name // '') eq $declaration.lexical-name {
                if $_.compile-time-value =:= $declaration.compile-time-value {
                    return Nil
                }
                elsif nqp::decont($_.compile-time-value) =:= nqp::decont($declaration.compile-time-value) {
                    return Nil
                }
                elsif $force {
                    $_.set-value($declaration.compile-time-value);
                    return Nil
                }
                else {
                    $_.merge($declaration, :$resolver);
                    return Nil;
                }
            }
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
                my $lexical-name := $_.lexical-name;
                %lookup{$lexical-name} := $_ if $lexical-name;
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

    method PERFORM-CHECK(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        my %lookup;
        for self.IMPL-UNWRAP-LIST(self.generated-lexical-declarations) {
            my $lexical-name := $_.lexical-name;
            if $lexical-name {
                if nqp::existskey(%lookup, $lexical-name) {
                    self.add-sorry:
                      $resolver.build-exception: 'X::Redeclaration',
                        :symbol($lexical-name), :what($_.declaration-kind);
                }
                else {
                    %lookup{$lexical-name} := $_;
                }
            }
        }
        for self.IMPL-UNWRAP-LIST(self.ast-lexical-declarations) {
            my $lexical-name := $_.lexical-name;
            if $lexical-name && ! $_ =:= self {
                if nqp::existskey(%lookup, $lexical-name) {
                    self.add-sorry:
                      $resolver.build-exception: 'X::Redeclaration',
                        :symbol($lexical-name), :what($_.declaration-kind);
                }
                else {
                    %lookup{$lexical-name} := $_;
                }
            }
        }
        # Check for variables that are declared after they have already been used.
        # That's possible with dynamics and with variables also declared in outer scope.
        my %declarations;
        while $!variables-cache {
            my $var := nqp::pop($!variables-cache);
            if nqp::istype($var, RakuAST::Declaration) {
                %declarations{$var.lexical-name} := $var if $var.report-redeclaration;
            }
            else {
                if $var.is-resolved && nqp::existskey(%declarations, $var.name) {
                    my $decl := %declarations{$var.name};
                    $decl.add-sorry:
                      $resolver.build-exception:
                        $var.postdeclaration-exception-name,
                        :symbol($decl.lexical-name);
                    $resolver.add-node-with-check-time-problems($decl);
                }
            }
        }
    }

    # This is only accurate after resolution - but since we only need it at
    # code-gen time, that's fine.
    method IMPL-HAS-CATCH-HANDLER() {
        $!catch-handlers ?? True !! False
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

    # Should return the lookup result for Failure
    method IMPL-FATALIZE {
        nqp::die("IMPL-FATALIZE NYI on " ~ self.HOW.name(self));
    }

    method IMPL-MAYBE-FATALIZE-QAST($qast) {
        self.IMPL-FATALIZE-QAST($qast, 0) if $!fatal;

        $qast
    }

    method IMPL-FATALIZE-QAST($qast, $bool-context) {
        my &FATALIZE := self.IMPL-FATALIZE;
        # comes from setting, so guaranteed to be in an SC
        my $FATALIZE := QAST::WVal.new(:value(&FATALIZE));
        my %boolify_first_child_ops := nqp::hash(
            'if', 1, 'unless', 1, 'defor', 1, 'hllbool', 1,
            'while', 1, 'until', 1, 'repeat_while', 1, 'repeat_until', 1,
        );
        my %boolify_first_child_calls := nqp::hash(
            '&prefix:<?>', 1, '&prefix:<so>', 1,
            '&prefix:<!>', 1, '&prefix:<not>', 1,
            '&defined', 1
        );
        if nqp::istype($qast, QAST::Op) {
            my str $op := $qast.op;
            if $op eq 'call' && nqp::istype($qast[0], QAST::WVal) && $qast[0].value =:= &FATALIZE {
                # We've been here before (tree with shared bits, presumably).
            }
            elsif nqp::existskey(%boolify_first_child_ops, $op) ||
                    $op eq 'call' && nqp::existskey(%boolify_first_child_calls, $qast.name) {
                my int $first := 1;
                for @($qast) {
                    if $first {
                        self.IMPL-FATALIZE-QAST($_, 1);
                        $first := 0;
                    }
                    else {
                        self.IMPL-FATALIZE-QAST($_, 0);
                    }
                }
            }
            elsif $op eq 'hllize' {
                self.IMPL-FATALIZE-QAST($_, $bool-context) for @($qast);
            }
            else {
                 self.IMPL-FATALIZE-QAST($_, 0) for @($qast);
                 if !$bool-context && ($op eq 'call' || $op eq 'callmethod') {
                    if $qast.name eq '&fail' {
                        $qast.name('&die');
                    }
                    else {
                        my $new-node := QAST::Op.new( :node($qast.node), :$op, :name($qast.name), :returns($qast.returns) );
                        $new-node.push($qast.shift) while @($qast);
                        $qast.op('call');
                        $qast.name('');

                        $qast.push($FATALIZE);
                        $qast.push($new-node);
                    }
                 }
            }
        }
        elsif nqp::istype($qast, QAST::Block) || nqp::istype($qast, QAST::Stmt) || nqp::istype($qast, QAST::Stmts) || nqp::istype($qast, QAST::Want) {
            self.IMPL-FATALIZE-QAST($_, 0) for @($qast);
        }
    }
}

# Done by anything that is a declaration - that is, declares a symbol.
class RakuAST::Declaration
  is RakuAST::Node
{
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

    # Change the scope of this declaration. Passing the empty string will set
    # it back to the default scope for this element type.
    method replace-scope(str $scope) {
        nqp::bindattr_s(self, RakuAST::Declaration, '$!scope',
            $scope eq '' ?? nqp::null_s !! $scope);
        Nil
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

    method report-redeclaration() {
        True
    }

    method declaration-kind() {
        'symbol'
    }
}

# Done by anything that may make implicit declarations. For example, a package
# declares $?PACKAGE inside of it, a sub declares a fresh $_, $/, and $!, etc.
# While a declaration is considered something external to a node, and so exposed
# to the enclosing lexical scope, implicit declarations are considered as being
# on the inside; this makes a difference in the case the node is also doing
# RakuAST::LexicalScope and is thus a lexical scope boundary.
class RakuAST::ImplicitDeclarations
  is RakuAST::Node
{
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
class RakuAST::Declaration::External
  is RakuAST::Declaration
{
    has str $.lexical-name;
    has Mu $!native-type;
    has Mu $.maybe-compile-time-value; # Must not be assumed constant in this base-class.

    method new(str :$lexical-name, Mu :$native-type, Mu :$maybe-compile-time-value is raw) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Declaration::External, '$!lexical-name', $lexical-name);
        nqp::bindattr($obj, RakuAST::Declaration::External, '$!native-type', $native-type);
        nqp::bindattr($obj, RakuAST::Declaration::External,
            '$!maybe-compile-time-value', $maybe-compile-time-value);
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

    method generate-lookup() {
        my $lookup := RakuAST::Var::Lexical.new($!lexical-name);
        $lookup.set-resolution(self);
        $lookup
    }
}

class RakuAST::Declaration::Mergeable {
    method is-stub() {
        return True if nqp::istype(self, RakuAST::Declaration::LexicalPackage) && self.package-is-stub;
        my $how  := self.return-type.HOW;
        my $name := $how.HOW.name($how);
        $name eq 'Perl6::Metamodel::PackageHOW' || $name eq 'KnowHOW'
    }

    method merge(RakuAST::Declaration $other, RakuAST::Resolver :$resolver!) {
        my $target := self.compile-time-value;
        my $source := $other.compile-time-value;

        my $loader := nqp::gethllsym('Raku', 'ModuleLoader');
        if $other.is-stub {
            # Source is a stub. We can safely merge the symbols
            # from source into the target that's importing them.
            $loader.merge_globals($target.WHO, $source.WHO);
        }
        elsif self.is-stub {
            # The tricky case: here the interesting package is the
            # one in the module. So we merge the other way around
            # and install that as the result.
            $loader.merge_globals($source.WHO, $target.WHO);
            self.set-value($source);
        }
        elsif nqp::can(self, 'lexical-name') && nqp::eqat(self.lexical-name, '&', 0) {
            # There's already a symbol. However, we may be able to merge
            # if both are multis and have onlystar dispatchers.
            if nqp::can($target, 'is_dispatcher') && $target.is_dispatcher
            && nqp::can($source, 'is_dispatcher') && $source.is_dispatcher
            && $target.onlystar && $source.onlystar {
                # Replace installed one with a derived one, to avoid any
                # weird action at a distance.
                $target := $target.derive_dispatcher;
                self.set-value($target);

                # Incorporate dispatchees of foreign proto, avoiding
                # duplicates.
                my %seen;
                for $target.dispatchees {
                    %seen{$_.static_id} := $_;
                }
                for $source.dispatchees {
                    unless nqp::existskey(%seen, $_.static_id) {
                        $target.add_dispatchee($_);
                    }
                }
                return;
            }

            # "Latest wins" semantics for functions
            self.set-value($source);
        }
        else {
            $resolver.panic(
                $resolver.build-exception(
                    'X::Redeclaration',
                    :symbol(nqp::can(self, 'lexical-name') ?? self.lexical-name !! '')
                )
            );
        }
    }

    method set-value(Mu $value) {
        nqp::die('set-value not implemented on ' ~ self.HOW.name(self));
    }
}

# A lexical declaration that comes with an external symbol, which has a fixed
# value available during compilation.
class RakuAST::Declaration::External::Constant
  is RakuAST::Declaration::External
  is RakuAST::CompileTimeValue
  is RakuAST::Declaration::Mergeable
{
    has Mu $.compile-time-value;

    method new(str :$lexical-name!, Mu :$compile-time-value! is raw) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Declaration::External, '$!lexical-name', $lexical-name);
        nqp::bindattr($obj, RakuAST::Declaration::External::Constant,
            '$!compile-time-value', $compile-time-value);
        $obj
    }

    method set-value(Mu $compile-time-value is raw) {
        nqp::bindattr(self, RakuAST::Declaration::External::Constant,
            '$!compile-time-value', $compile-time-value);
    }

    method return-type() { $!compile-time-value.WHAT }

    method IMPL-LOOKUP-QAST(RakuAST::IMPL::QASTContext $context, Mu :$rvalue) {
        my $value := $!compile-time-value;
        $context.ensure-sc($value);
        QAST::WVal.new( :$value )
    }
}

# A lexical declaration that comes with an external symbol, which has a fixed
# value available during compilation but still has to be looked up during
# runtime.
class RakuAST::Declaration::External::Setting
  is RakuAST::Declaration::External
  is RakuAST::CompileTimeValue
  is RakuAST::Declaration::Mergeable
{
    has Mu $.compile-time-value;

    method new(str :$lexical-name!, Mu :$compile-time-value! is raw) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Declaration::External, '$!lexical-name', $lexical-name);
        nqp::bindattr($obj, RakuAST::Declaration::External::Setting,
            '$!compile-time-value', $compile-time-value);
        $obj
    }

    method set-value(Mu $compile-time-value is raw) {
        nqp::bindattr(self, RakuAST::Declaration::External::Setting,
            '$!compile-time-value', $compile-time-value);
    }

    method return-type() { $!compile-time-value.WHAT }
}

# An imported lexical declaration. Has a compile-time value. Must create a
# lexical slot for itself in the scope it is installed in.
class RakuAST::Declaration::Import
  is RakuAST::Declaration::External::Constant
{
    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        my $value := self.compile-time-value;
        $context.ensure-sc($value);
        QAST::Var.new(
            :scope('lexical'), :decl('static'), :name(self.lexical-name), :$value
        )
    }
}

# A lexical declaration that points to a package. Generated as part of package
# installation in RakuAST::Package, and installed as a generated lexical in a
# RakuAST::LexicalScope.
class RakuAST::Declaration::LexicalPackage
  is RakuAST::Declaration
  is RakuAST::CompileTimeValue
  is RakuAST::Declaration::Mergeable
{
    has str $.lexical-name;
    has Mu $.compile-time-value;
    has RakuAST::Package $.package;

    method new(str :$lexical-name!, Mu :$compile-time-value! is raw, RakuAST::Package :$package!) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::Declaration::LexicalPackage,
            '$!lexical-name', $lexical-name);
        nqp::bindattr($obj, RakuAST::Declaration::LexicalPackage,
            '$!compile-time-value', $compile-time-value);
        nqp::bindattr($obj, RakuAST::Declaration::LexicalPackage,
            '$!package', $package);
        $obj
    }

    method set-value(Mu $compile-time-value is raw) {
        nqp::bindattr(self, RakuAST::Declaration::LexicalPackage,
            '$!compile-time-value', $compile-time-value);
    }

    method package-is-stub() {
        $!package.is-stub
    }

    method return-type() { $!compile-time-value.WHAT }

    method IMPL-QAST-DECL(RakuAST::IMPL::QASTContext $context) {
        $context.ensure-sc($!compile-time-value);
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

# A constant value that has been resolved. Has a compile time value, and the
# resolution always compiles into that. The name it was looked up under is
# not preserved.
class RakuAST::Declaration::ResolvedConstant
  is RakuAST::Declaration
  is RakuAST::CompileTimeValue
{
    has Mu $.compile-time-value;

    method new(Mu :$compile-time-value! is raw) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Declaration::ResolvedConstant,
            '$!compile-time-value', $compile-time-value);
        $obj
    }

    method return-type() { $!compile-time-value.WHAT }

    method IMPL-TO-QAST(RakuAST::IMPL::QASTContext $context) {
        self.IMPL-LOOKUP-QAST($context)
    }

    method IMPL-LOOKUP-QAST(RakuAST::IMPL::QASTContext $context, Mu :$rvalue) {
        my $value := $!compile-time-value;
        $context.ensure-sc($value);
        QAST::WVal.new( :$value )
    }

    method default-scope() { 'package' }

    method allowed-scopes() { self.IMPL-WRAP-LIST(['package']) }

    method IMPL-CAN-INTERPRET() {
        True
    }

    method IMPL-INTERPRET(RakuAST::IMPL::InterpContext $ctx) {
        $!compile-time-value
    }
}

# Done by anything that is a lookup of a symbol. May or may not need resolution
# at compile time.
class RakuAST::Lookup
  is RakuAST::Node
{
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

    # Returns information to report in an X::Undeclared::Symbols exception.
    # Returns Nil if it should not be reported there, otherwise should be
    # an instance of RakuAST::UndeclaredSymbolDescription.
    method undeclared-symbol-details() {
        Nil
    }
}

# Details about an undeclared symbol.
class RakuAST::UndeclaredSymbolDescription {
    has str $.name;

    method new(str $name) {
        my $obj := nqp::create(self);
        nqp::bindattr_s($obj, RakuAST::UndeclaredSymbolDescription, '$!name', $name);
        $obj
    }
}
class RakuAST::UndeclaredSymbolDescription::Routine
  is RakuAST::UndeclaredSymbolDescription
{
    method IMPL-REPORT(RakuAST::Lookup $node, Mu $types, Mu $routines, Mu $other) {
        nqp::bindkey($routines, self.name, [$node.origin.as-match.line]);
    }
}

# Some program elements are not really lookups, but require the resolution
# of symbols as part of their compilation. For example, a positional regex
# access depends on `&postcircumfix:<[ ]>` and `$/`, while an `unless`
# statement depends on `Empty` (as that's what it evaluates to in the case
# there the condition is not matched). Implicit lookups are not children of
# the node, but they will receive their parse/begin time prior to the node's
# parse time.
class RakuAST::ImplicitLookups
  is RakuAST::Node
{
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
        nqp::isconcrete(self)
            ?? $!implicit-lookups-cache //
                nqp::bindattr(self, RakuAST::ImplicitLookups, '$!implicit-lookups-cache',
                    self.PRODUCE-IMPLICIT-LOOKUPS())
            !! self.IMPL-WRAP-LIST([])
    }

    # Drive the implicit lookups to their begin time.
    method implicit-lookups-to-begin-time(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        for self.IMPL-UNWRAP-LIST(self.get-implicit-lookups()) {
            # We use null to pad out lookup lists so that they always
            # have the same number of elements regardless of whether
            # some lookups can only be conditionally included in the list
            $_.to-begin-time($resolver, $context) unless nqp::isnull($_);
        }
    }
}

# Anything that needs to stub packages into existence -- or to fill in stubbed packages -- does RakuAST::PackageInstaller
class RakuAST::PackageInstaller {
    ### Consuming classes must define:
    #    method IMPL-GENERATE-LEXICAL-DECLARATION(RakuAST::Name $name, Mu $type-object) { ... }

    method IMPL-INSTALL-PACKAGE(
        RakuAST::Resolver $resolver,
        str $scope,
        RakuAST::Name $name,
        RakuAST::Package $current-package,
        Mu :$meta-object
     ) {
        my $target;
        my $final;
        my $lexical;
        my $type-object := nqp::eqaddr($meta-object, Mu) ?? self.stubbed-meta-object !! $meta-object;
        my $pure-package-installation := nqp::istype(self, RakuAST::Package);

        my $illegal-pseudo-package := $name.contains-pseudo-package-illegal-for-declaration;
        $resolver.add-sorry: $resolver.build-exception:
            'X::PseudoPackage::InDeclaration',
                pseudo-package => $illegal-pseudo-package,
                action => self.dba ~ ' name'
            if $illegal-pseudo-package;

        if $name.is-identifier {
            $final := $name.canonicalize(:colonpairs(0));
            $lexical := $resolver.resolve-lexical-constant($final);
            if $pure-package-installation || !$lexical {
                $resolver.current-scope.merge-generated-lexical-declaration:
                    :$resolver,
                    self.IMPL-GENERATE-LEXICAL-DECLARATION($final, $meta-object);
            }
            # If `our`-scoped, also put it into the current package.
            if $scope eq 'our' {
                $target := $current-package;

                my %stash := $resolver.IMPL-STASH-HASH($target);
                if nqp::existskey(%stash, $final) && !(%stash{$final} =:= $type-object || nqp::istype(%stash{$final}.HOW, Perl6::Metamodel::PackageHOW)) {
                    $resolver.add-sorry: $resolver.build-exception:
                        'X::Redeclaration', :symbol($final);
                }
            }
        }
        else {
            my @parts := nqp::clone(self.IMPL-UNWRAP-LIST($name.parts));
            $final := nqp::pop(@parts).name;
            my $first := @parts[0].name;
            my $resolved := $resolver.partially-resolve-name-constant(RakuAST::Name.new(|@parts));

            if $resolved { # first parts of the name found
                $resolved := self.IMPL-UNWRAP-LIST($resolved);
                $target := $resolved[0];
                if $scope eq 'our' && nqp::elems(@parts) >= 1 && $resolved[2] eq 'lexical' {
                    # Upgrade lexically imported top level package to global
                    ($resolver.get-global.WHO){$first} := $resolver.resolve-lexical($first).compile-time-value;
                }
                my $parts  := $resolved[1];
                @parts := self.IMPL-UNWRAP-LIST($parts);
                $scope := 'our'; # Ensure we install the package into the parent stash
                if nqp::elems(@parts) {
                    my $longname := $target.HOW.name($target);

                    for @parts {
                        $longname := $longname ~ '::' ~ $_.name;
                        my $package := Perl6::Metamodel::PackageHOW.new_type(name => $longname);
                        $package.HOW.compose($package);
                        my %stash := $resolver.IMPL-STASH-HASH($target);
                        %stash{$_.name} := $package;
                        $target := $package;
                    }
                }
            }
            else {
                my $first := nqp::shift(@parts).name;
                $target := Perl6::Metamodel::PackageHOW.new_type(name => $first);
                $target.HOW.compose($target);
                $resolver.current-scope.merge-generated-lexical-declaration:
                    :$resolver,
                    RakuAST::Declaration::LexicalPackage.new:
                        :lexical-name($first),
                        :compile-time-value($target),
                        :package($pure-package-installation ?? self !! $current-package);
                if $scope eq 'our' {
                    # TODO conflicts
                    my %stash := $resolver.IMPL-STASH-HASH($current-package);
                    %stash{$first} := $target;
                }
                $scope := 'our'; # Ensure we install the package into the generated stub

                my $longname := $first;
                for @parts {
                    $longname := $longname ~ '::' ~ $_.name;
                    my $package := Perl6::Metamodel::PackageHOW.new_type(name => $longname);
                    $package.HOW.compose($package);
                    my %stash := $resolver.IMPL-STASH-HASH($target);
                    %stash{$_.name} := $package;
                    $target := $package;
                }
            }
        }

        my %stash := $resolver.IMPL-STASH-HASH($target);
        # upgrade a lexically imported package stub to package scope if it exists
        if $lexical {
            %stash{$final} := $lexical.compile-time-value;
        }
        if $scope eq 'our' {
            if nqp::existskey(%stash, $final) && !(%stash{$final} =:= $type-object) {
                nqp::setwho($type-object, %stash{$final}.WHO);
            }
            %stash{$final} := $type-object;
        }
    }
}
