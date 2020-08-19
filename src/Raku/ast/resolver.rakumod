class RakuAST::Resolver {
    # The setting.
    has Mu $!setting;

    # Our outer context. When not an EVAL, this is the same as $!setting.
    has Mu $!outer;

    # Current attachment target stacks (a hash keyed on the target name, where each value
    # is a stack, where the top is the active target).
    has Mu $!attach-targets;

    # Push an attachment target, so children can attach to it.
    method push-attach-target(RakuAST::AttachTarget $target) {
        $target.clear-attachments();
        for $target.IMPL-UNWRAP-LIST($target.attach-target-names()) -> str $name {
            my @stack := $!attach-targets{$name};
            unless nqp::isconcrete(@stack) {
                @stack := [];
                $!attach-targets{$name} := @stack;
            }
            nqp::push(@stack, $target);
        }
        Nil
    }

    # Pop an attachment target, so it will no longer be found.
    method pop-attach-target(RakuAST::AttachTarget $target) {
        for $target.IMPL-UNWRAP-LIST($target.attach-target-names()) -> str $name {
            my @stack := $!attach-targets{$name};
            unless nqp::pop(@stack) =:= $target {
                nqp::die('Inconsistent attachment target stack for ' ~ $target.HOW.name($target));
            }
        }
        Nil
    }

    # Find an attachment target with the specified name.
    method find-attach-target(str $name) {
        my @stack := $!attach-targets{$name};
        if nqp::isconcrete(@stack) {
            my int $n := nqp::elems(@stack);
            $n > 0 ?? @stack[$n - 1] !! Nil
        }
        else {
            Nil
        }
    }

    # Name-mangle an infix operator and resolve it.
    method resolve-infix(Str $operator-name) {
        self.resolve-lexical('&infix' ~ self.IMPL-CANONICALIZE-PAIR('', $operator-name))
    }

    # Name-mangle a prefix operator and resolve it.
    method resolve-prefix(Str $operator-name) {
        self.resolve-lexical('&prefix' ~ self.IMPL-CANONICALIZE-PAIR('', $operator-name))
    }

    # Name-mangle a postfix operator and resolve it.
    method resolve-postfix(Str $operator-name) {
        self.resolve-lexical('&postfix' ~ self.IMPL-CANONICALIZE-PAIR('', $operator-name))
    }

    # Resolve a RakuAST::Name, optionally adding the specified sigil to the
    # final component.
    method resolve-name(RakuAST::Name $name, Str :$sigil) {
        unless $name.is-identifier {
            nqp::die('Resovling complex names NYI')
        }
        my $trailing := $name.IMPL-UNWRAP-LIST($name.parts)[0].name;
        my str $lexical-name := $sigil ?? $sigil ~ $trailing !! $trailing;
        self.resolve-lexical($lexical-name)
    }

    # Resolve a RakuAST::Name to a constant.
    method resolve-name-constant(RakuAST::Name $name) {
        if $name.is-identifier {
            self.resolve-lexical-constant($name.IMPL-UNWRAP-LIST($name.parts)[0].name)
        }
        else {
            nqp::die('Resovling complex names NYI')
        }
    }

    # Resolves a lexical in the chain of outer contexts.
    method resolve-lexical-in-outer(Str $name, Bool :$current-scope-only) {
        # Look through the contexts for the name.
        my $ctx := $!outer;
        while !nqp::isnull($ctx) {
            if nqp::existskey($ctx, $name) {
                my $prim-spec := nqp::lexprimspec($ctx, $name);
                if $prim-spec == 0 {
                    return RakuAST::Declaration::External.new(:lexical-name($name));
                }
                elsif $prim-spec == 1 {
                    return RakuAST::Declaration::External.new(:lexical-name($name), :native-type(int));
                }
                elsif $prim-spec == 2 {
                    return RakuAST::Declaration::External.new(:lexical-name($name), :native-type(num));
                }
                else {
                    return RakuAST::Declaration::External.new(:lexical-name($name), :native-type(str));
                }
            }
            $ctx := nqp::ctxouter($ctx);
        }

        # Nothing found.
        return Nil;
    }

    # Resolves a lexical using the outer contexts. The declaration must have a
    # compile-time value.
    method resolve-lexical-constant-in-outer(Str $name) {
        my $ctx := $!outer;
        while !nqp::isnull($ctx) {
            if nqp::existskey($ctx, $name) {
                my $compile-time-value := nqp::atkey($ctx, $name);
                return RakuAST::Declaration::External::Constant.new(:lexical-name($name),
                    :$compile-time-value);
            }
            $ctx := nqp::ctxouter($ctx);
        }
        return Nil;
    }

    method IMPL-CANONICALIZE-PAIR(Str $k, Str $v) {
        if $v ~~ /<[ < > ]>/ && !($v ~~ /<[ « » $ \\ " ' ]>/) {
            ':' ~ $k ~ '«' ~ $v ~ '»'
        }
        else {
            my $new := '';
            my int $i := 0;
            my int $e := nqp::chars($v);
            while $i < $e {
                my $ch := nqp::substr($v,$i,1);
                $new := $new ~ '\\' if $ch eq '<' || $ch eq '>';
                $new := $new ~ $ch;
                ++$i;
            }
            ':' ~ $k ~ '<' ~ $new ~ '>';
        }
    }
}

# The EVAL resolver is used when we are given an AST as a whole, and visit it
# to perform resolutions. We expect a context and GLOBAL to be provided in this
# mode.
class RakuAST::Resolver::EVAL is RakuAST::Resolver {
    # The stack of scopes we are in (an array of RakuAST::LexicalScope).
    has Mu $!scopes;

    # The global symbol table.
    has Mu $!global;

    method new(Mu :$global!, Mu :$context!) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Resolver, '$!outer', $context);
        nqp::bindattr($obj, RakuAST::Resolver, '$!setting', $context); # XXX TODO
        nqp::bindattr($obj, RakuAST::Resolver, '$!attach-targets', nqp::hash());
        nqp::bindattr($obj, RakuAST::Resolver::EVAL, '$!global', $global);
        nqp::bindattr($obj, RakuAST::Resolver::EVAL, '$!scopes', []);
        $obj
    }

    # Pushes an active lexical scope to be considered in lookup.
    method push-scope(RakuAST::LexicalScope $scope) {
        $!scopes.push($scope);
        if nqp::istype($scope, RakuAST::AttachTarget) {
            self.push-attach-target($scope);
        }
        Nil
    }

    # Pops the top active lexical scope.
    method pop-scope() {
        my $scope := $!scopes.pop;
        if nqp::istype($scope, RakuAST::AttachTarget) {
            self.pop-attach-target($scope);
        }
        Nil
    }

    # Resolves a lexical to its declaration. The declaration need not have a
    # compile-time value.
    method resolve-lexical(Str $name, Bool :$current-scope-only) {
        # If it's in the current scope only, we just look at the top one.
        if $current-scope-only {
            my @scopes := $!scopes;
            my int $i := nqp::elems(@scopes);
            return $i > 0 ?? @scopes[$i - 1].find-lexical($name) !! Nil;
        }

        # Walk active scopes, most nested first.
        my @scopes := $!scopes;
        my int $i := nqp::elems(@scopes);
        while $i-- {
            my $scope := @scopes[$i];
            my $found := $scope.find-lexical($name);
            return $found if nqp::isconcrete($found);
        }

        self.resolve-lexical-in-outer($name);
    }

    # Resolves a lexical to its declaration. The declaration must have a
    # compile-time value.
    method resolve-lexical-constant(Str $name) {
        # TODO walk scopes we've encountered while walking

        self.resolve-lexical-constant-in-outer($name);
    }
}

# The compiler resolver is used in the situation we are parsing code and
# building up a RakuAST as we go. We thus need to provide symbol resolutions
# for the sake of parse disambiguation, as well as to handle BEGIN-time code.
# A resolver may be created using an existing context, or it may be for a
# compilation unit whose outer scope is some version of the setting.
class RakuAST::Resolver::Compile is RakuAST::Resolver {
    # Scopes stack; an array of RakuAST::Resolver::Compile::Scope.
    has Mu $!scopes;

    method new(Mu :$setting!, Mu :$outer!) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Resolver, '$!setting', $setting);
        nqp::bindattr($obj, RakuAST::Resolver, '$!outer', $outer);
        nqp::bindattr($obj, RakuAST::Resolver, '$!attach-targets', nqp::hash());
        nqp::bindattr($obj, RakuAST::Resolver::Compile, '$!scopes', []);
        $obj
    }

    # Create a resolver from a context and existing global. Used when we are
    # compiling a textual EVAL.
    method from-context(Mu :$context!, Mu :$global!) {
        nqp::die('from-context NYI');
    }

    # Create a resolver for a fresh compilation unit of the specified language
    # version.
    method from-setting(Str :$setting-name!) {
        my $loader := nqp::gethllsym('Raku', 'ModuleLoader');
        my $setting := $loader.load_setting($setting-name);
        self.new(:$setting, :outer($setting))
    }

    # Pushes an active lexical scope to be considered in lookup. Used only in
    # batch resolve mode.
    method push-scope(RakuAST::LexicalScope $scope) {
        nqp::push($!scopes, RakuAST::Resolver::Compile::Scope.new(:$scope, :batch-mode));
        if nqp::istype($scope, RakuAST::AttachTarget) {
            self.push-attach-target($scope);
        }
        Nil
    }

    # Pops the top active lexical scope. Used only in batch resolve mode.
    method pop-scope() {
        my $scope := nqp::pop($!scopes);
        $scope.batch-mode ||
            nqp::die('pop-scope should only be used on batch mode scopes');
        if nqp::istype($scope, RakuAST::AttachTarget) {
            self.pop-attach-target($scope);
        }
        Nil
    }

    # Enters a new scope. Used in compilation mode. In this case, declarations
    # are registered as they are made, so we don't have to have them immediately
    # linked into the tree.
    method enter-scope(RakuAST::LexicalScope $scope) {
        nqp::push($!scopes, RakuAST::Resolver::Compile::Scope.new(:$scope, :!batch-mode));
        if nqp::istype($scope, RakuAST::AttachTarget) {
            self.push-attach-target($scope);
        }
        Nil
    }

    # Indicates that any implicit declarations for the current scope should now
    # come into force. Only used in compilation mode. Called by the compiler at
    # the appropriate point.
    method create-scope-implicits() {
        $!scopes[nqp::elems($!scopes) - 1].create-implicits();
        Nil
    }

    # Leaves a lexical scope. Used in compilation mode.
    method leave-scope() {
        my $scope := nqp::pop($!scopes);
        $scope.batch-mode &&
            nqp::die('leave-scope should never be used on batch mode scopes');
        if nqp::istype($scope, RakuAST::AttachTarget) {
            self.pop-attach-target($scope);
        }
        Nil
    }

    # Add a lexical declaration. Used when the compiler produces the declaration,
    # so that we can resovle it without requiring it to be linked into the tree.
    method declare-lexical(RakuAST::Declaration $decl) {
        $!scopes[nqp::elems($!scopes) - 1].declare-lexical($decl);
    }

    # Resolves a lexical to its declaration. The declaration need not have a
    # compile-time value.
    method resolve-lexical(Str $name, Bool :$current-scope-only) {
        # If it's in the current scope only, we just look at the top one.
        if $current-scope-only {
            my @scopes := $!scopes;
            my int $i := nqp::elems(@scopes);
            return $i > 0 ?? @scopes[$i - 1].find-lexical($name) !! Nil;
        }

        # Walk active scopes, most nested first.
        my @scopes := $!scopes;
        my int $i := nqp::elems(@scopes);
        while $i-- {
            my $scope := @scopes[$i];
            my $found := $scope.find-lexical($name);
            return $found if nqp::isconcrete($found);
        }

        self.resolve-lexical-in-outer($name);
    }

    # Resolves a lexical to its declaration. The declaration must have a
    # compile-time value.
    method resolve-lexical-constant(Str $name) {
        # TODO scope handling
        self.resolve-lexical-constant-in-outer($name);
    }

    # Check if an identifier is a known type.
    method is-identifier-type(Str $identifier) {
        # Can optimize this later to avoid the throwaway name creation
        self.is-name-type(RakuAST::Name.from-identifier($identifier))
    }

    # Check if a name is a known type.
    method is-name-type(RakuAST::Name $name) {
        my $constant := self.resolve-name-constant($name);
        if nqp::isconcrete($constant) {
            # Name resolves, but is it an instance or a type object?
            !nqp::isconcrete($constant.compile-time-value)
        }
        else {
            # Name doesn't resolve to a constant at all, so can't be a type.
            0
        }
    }
}

# Information about a lexical scope that we are currently compiling.
class RakuAST::Resolver::Compile::Scope is RakuAST::Resolver {
    # The scope.
    has RakuAST::LexicalScope $.scope;

    # If we are in batch mode. When we are, then we have a fully-formed tree
    # and can just look at it, assume it's immutable, etc. If not, then we
    # instead look at our live declaration map.
    has int $!batch-mode;

    # The live declaration map, used when not in batch mode.
    has Mu $!live-decl-map;

    method new(RakuAST::LexicalScope :$scope!, int :$batch-mode) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Resolver::Compile::Scope, '$!scope', $scope);
        nqp::bindattr_i($obj, RakuAST::Resolver::Compile::Scope, '$!batch-mode', $batch-mode);
        unless $batch-mode {
            nqp::bindattr($obj, RakuAST::Resolver::Compile::Scope, '$!live-decl-map', {});
        }
        $obj
    }

    method batch-mode() {
        $!batch-mode ?? True !! False
    }

    method find-lexical(Str $name) {
        if $!batch-mode {
            $!scope.find-lexical($name) // Nil
        }
        else {
            $!live-decl-map{$name} // Nil
        }
    }

    method declare-lexical(RakuAST::Declaration $decl) {
        nqp::die('Should not be calling declare-lexical in batch mode') if $!batch-mode;
        $!live-decl-map{$decl.lexical-name} := $decl;
        Nil
    }

    method create-implicits() {
        nqp::die('Should not be calling create-implicits in batch mode') if $!batch-mode;
        if nqp::istype($!scope, RakuAST::ImplicitDeclarations) {
            for $!scope.IMPL-UNWRAP-LIST($!scope.get-implicit-declarations) -> $decl {
                if $decl.is-lexical {
                    $!live-decl-map{$decl.lexical-name} := $decl;
                }
            }
        }
        Nil
    }
}
