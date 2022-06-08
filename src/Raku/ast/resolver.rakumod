class RakuAST::Resolver {
    # The setting.
    has Mu $!setting;

    # Our outer context. When not an EVAL, this is the same as $!setting.
    has Mu $!outer;

    # Our current idea of the global symbol table.
    has Mu $!global;

    # Current attachment target stacks (a hash keyed on the target name, where each value
    # is a stack, where the top is the active target).
    has Mu $!attach-targets;

    # Packages we're currently in.
    has Mu $!packages;

    # Nodes with check-time problems to report.
    has Mu $!nodes-with-check-time-problems;

    # Nodes that are unresolved after check time.
    has Mu $!nodes-unresolved-after-check-time;

    # The current comp unit's EXPORT package.
    has Mu $!export-package;

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

    # Set the global package when we're starting a fresh compilation unit.
    method set-global(Mu $global) {
        nqp::die("This resolver's GLOBAL is already set") unless nqp::eqaddr($!global, Mu);
        nqp::bindattr(self, RakuAST::Resolver, '$!global', $global);
        Nil
    }

    method set-export-package(Mu $export-package) {
        nqp::die("This resolver's EXPORT is already set") unless nqp::eqaddr($!export-package, Mu);
        nqp::bindattr(self, RakuAST::Resolver, '$!export-package', $export-package);
        Nil
    }

    # Push a package, at the point we enter it.
    method push-package(RakuAST::Package $package) {
        nqp::push($!packages, $package);
        self.push-attach-target($package);
        Nil
    }

    # Obtains the current package. This is not a RakuAST::Package node, but
    # rather the type object of the package we are currently in.
    method current-package() {
        int $n := nqp::elems($!packages);
        $n == 0 ?? $!global !! $!packages[$n - 1].compile-time-value
    }

    # Pops a package, at the point we leave it.
    method pop-package() {
        my $package := nqp::pop($!packages);
        self.pop-attach-target($package);
        Nil
    }

    # Current package stack as NQP list
    method packages() {
        my $result := nqp::create(List);
        nqp::bindattr($result, List, '$!reified', $!packages);
        $result
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

    # Name-mangle a term and resolve it.
    method resolve-term(Str $term-name) {
        self.resolve-lexical('&term' ~ self.IMPL-CANONICALIZE-PAIR('', $term-name))
    }

    # Resolve a RakuAST::Name, optionally adding the specified sigil to the
    # final component.
    method resolve-name(RakuAST::Name $name, Str :$sigil) {
        if $name.is-identifier {
            # Single-part name, so look lexically.
            my str $bare-name := $name.IMPL-UNWRAP-LIST($name.parts)[0].name;
            my str $lexical-name := $sigil ?? $sigil ~ $bare-name !! $bare-name;
            self.resolve-lexical($lexical-name)
        }
        else {
            # All package name installations happen via the symbol table as
            # BEGIN-time effects, so chase it down as if it were a constant.
            self.resolve-name-constant($name, :$sigil)
        }
    }

    # Resolve a RakuAST::Name to a constant.
    method resolve-name-constant(RakuAST::Name $name, str :$sigil) {
        self.IMPL-RESOLVE-NAME-CONSTANT($name, :$sigil)
    }

    # Resolve a RakuAST::Name to a constant looking only in the setting.
    method resolve-name-constant-in-setting(RakuAST::Name $name) {
        self.IMPL-RESOLVE-NAME-CONSTANT($name, :setting)
    }

    method IMPL-RESOLVE-NAME-CONSTANT(RakuAST::Name $name, Bool :$setting, str :$sigil) {
        if $name.is-identifier {
            my str $identifier := $name.IMPL-UNWRAP-LIST($name.parts)[0].name;
            $setting
                ?? self.resolve-lexical-constant-in-setting($identifier)
                !! self.resolve-lexical-constant($identifier)
        }
        else {
            # Obtain parts.
            my @parts := $name.IMPL-UNWRAP-LIST($name.parts);
            if nqp::elems(@parts) == 0 {
                nqp::die('0-part name lookup not possible as a constant');
            }

            # See if we can obtain the first part lexically.
            # TODO pseudo-packages
            # TODO GLOBALish fallback
            if nqp::istype(@parts[0], RakuAST::Name::Part::Empty) {
                return Nil;
            }
            elsif nqp::istype(@parts[0], RakuAST::Name::Part::Expression) {
                return Nil;
            }
            else {
                my $cur-symbol;
                if @parts[0].name eq 'EXPORT' {
                    $cur-symbol := $!export-package;
                }
                else {
                    my $first-resolved := $setting
                        ?? self.resolve-lexical-constant-in-setting(@parts[0].name)
                        !! self.resolve-lexical-constant(@parts[0].name);
                    return Nil unless $first-resolved;
                    $cur-symbol := $first-resolved.compile-time-value;
                }

                # Now chase down through the packages until we find something.
                my int $i := 1;
                my int $n := nqp::elems(@parts);
                while $i < $n {
                    my %hash := self.IMPL-STASH-HASH($cur-symbol);
                    my $name := @parts[$i].name;
                    $cur-symbol := nqp::atkey(%hash, $i < $n - 1 ?? $name !! $sigil ~ $name);
                    return Nil if nqp::isnull($cur-symbol);
                    $i++;
                }

                # Wrap it.
                RakuAST::Declaration::ResolvedConstant.new(compile-time-value => $cur-symbol)
            }
        }
    }

    # Resolve a RakuAST::Name to a constant.
    method partially-resolve-name-constant(RakuAST::Name $name, str :$sigil) {
        self.IMPL-PARTIALLY-RESOLVE-NAME-CONSTANT($name, :$sigil)
    }

    method IMPL-PARTIALLY-RESOLVE-NAME-CONSTANT(RakuAST::Name $name, Bool :$setting, str :$sigil) {
        if $name.is-identifier {
            my str $identifier := $name.IMPL-UNWRAP-LIST($name.parts)[0].name;
            $setting
                ?? self.resolve-lexical-constant-in-setting($identifier)
                !! self.resolve-lexical-constant($identifier)
        }
        else {
            # Obtain parts.
            my @parts := $name.IMPL-UNWRAP-LIST($name.parts);
            if nqp::elems(@parts) == 0 {
                nqp::die('0-part name lookup not possible as a constant');
            }

            # See if we can obtain the first part lexically.
            # TODO pseudo-packages
            # TODO GLOBALish fallback
            my $cur-symbol;
            if @parts[0].name eq 'EXPORT' {
                $cur-symbol := $!export-package;
            }
            else {
                my $first-resolved := $setting
                    ?? self.resolve-lexical-constant-in-setting(@parts[0].name)
                    !! self.resolve-lexical-constant(@parts[0].name);
                return Nil unless $first-resolved;
                $cur-symbol := $first-resolved.compile-time-value;
            }

            # Now chase down through the packages until we find something.
            @parts := nqp::clone(@parts); # make manipulations safe
            while @parts {
                my $part := nqp::shift(@parts);
                my %hash := self.IMPL-STASH-HASH($cur-symbol);
                my $name-part := $part.name;
                my $next-symbol := nqp::atkey(%hash, @parts ?? $name-part !! $sigil ~ $name-part);
                return ($cur-symbol, $name.IMPL-WRAP-LIST(@parts)) if nqp::isnull($next-symbol);
                $cur-symbol := $next-symbol;
            }

            # Wrap it.
            (RakuAST::Declaration::ResolvedConstant.new(compile-time-value => $cur-symbol), List.new)
        }
    }

    method IMPL-STASH-HASH(Mu $pkg) {
        my $hash := $pkg.WHO;
        unless nqp::ishash($hash) {
            $hash := $hash.FLATTENABLE_HASH();
        }
        $hash
    }

    # Resolves a lexical in the chain of outer contexts.
    method resolve-lexical-in-outer(Str $name, Bool :$current-scope-only) {
        # Look through the contexts for the name.
        my $ctx := $!outer;
        my int $seen-setting;
        while !nqp::isnull($ctx) {
            if nqp::existskey($ctx, 'CORE-SETTING-REV') {
                $seen-setting := 1;
            }
            if nqp::existskey($ctx, $name) {
                my $prim-spec := nqp::lexprimspec($ctx, $name);
                if $prim-spec == 0 {
                    # Things in the setting are assumed constant.
                    if $seen-setting {
                        my $compile-time-value := nqp::atkey($ctx, $name);
                        return RakuAST::Declaration::External::Constant.new(
                            :lexical-name($name), :$compile-time-value);
                    }
                    else {
                        return RakuAST::Declaration::External.new(:lexical-name($name));
                    }
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

    # Resolves a lexical using the outer contexts. The declaration must have a
    # compile-time value.
    method resolve-lexical-constant-in-setting(Str $name) {
        my $ctx := $!setting;
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

    method IMPL-SETTING-FROM-CONTEXT(Mu $context) {
        # TODO locate the setting frame
        $context
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

    # Check if an identifier is a known type.
    method is-identifier-type(Str $identifier) {
        # Can optimize this later to avoid the throwaway name creation
        self.is-name-type(RakuAST::Name.from-identifier($identifier))
    }

    # Check if a name is a known type.
    method is-name-type(RakuAST::Name $name) {
        my $constant := self.resolve-name($name);
        if nqp::istype($constant, RakuAST::CompileTimeValue) {
            # Name resolves, but is it an instance or a type object?
            nqp::isconcrete($constant.compile-time-value) ?? False !! True
        }
        else {
            # Name doesn't resolve to a constant at all, so can't be a type.
            False
        }
    }

    # Check if an identifier is known (declared) at all.
    method is-identifier-known(Str $identifier) {
        nqp::isconcrete(self.resolve-lexical($identifier)) ?? True !! False
    }

    # Check if a name is known (declared) at all.
    method is-name-known(RakuAST::Name $name) {
        nqp::isconcrete(self.resolve-name($name)) ?? True !! False
    }

    # Build an exception object for a check-time exception.
    method build-exception(Str $type-name, *%opts) {
        my $name := RakuAST::Name.from-identifier-parts(|nqp::split('::', $type-name));
        my $type-res := self.resolve-name-constant-in-setting($name);
        my $XComp-res := self.resolve-name-constant-in-setting:
            RakuAST::Name.from-identifier-parts('X', 'Comp');
        if $type-res && $XComp-res {
            # Successfully resolved. Maka sure it is an X::Comp.
            my $type := $type-res.compile-time-value;
            my $XComp := $XComp-res.compile-time-value;
            unless nqp::istype($type, $XComp) {
                $type := $type.HOW.mixin($type, $XComp);
            }

            # Ensure that the options are Raku types.
            for %opts -> $p {
                if nqp::islist($p.value) {
                    my @a := [];
                    for $p.value {
                        nqp::push(@a, nqp::hllizefor($_, 'Raku'));
                    }
                    %opts{$p.key} := nqp::hllizefor(@a, 'Raku');
                }
                else {
                    %opts{$p.key} := nqp::hllizefor($p.value, 'Raku');
                }
            }

            # Construct the exception object and return it.
            %opts<is-compile-time> := True;
            $type.new(|%opts)
        }
        else {
            # Could not find exception type, so build a fake (typically happens
            # during CORE.setting compilation).
            nqp::die('nyi missing exception type fallback')
        }
    }

    # Add a node to the list of those with check-time problems. 
    method add-node-with-check-time-problems(RakuAST::CheckTime $node) {
        unless $!nodes-with-check-time-problems {
            nqp::bindattr(self, RakuAST::Resolver, '$!nodes-with-check-time-problems', []);
        }
        nqp::push($!nodes-with-check-time-problems, $node);
        Nil
    }

    # Add a node to the list of those unresolved at check time.
    method add-node-unresolved-after-check-time(RakuAST::Lookup $node) {
        unless $!nodes-unresolved-after-check-time {
            nqp::bindattr(self, RakuAST::Resolver, '$!nodes-unresolved-after-check-time', []);
        }
        nqp::push($!nodes-unresolved-after-check-time, $node);
        Nil
    }

    # Produce an exception with any compile-time errors, optionally using the
    # specified one as a the main "panic" exception. Incorporates any sorries
    # and worries from check time, and also those registered by specific
    # resolvers (for example, the compiler resolver may also add syntax level
    # problems). If there are no problems, produces Nil.
    method produce-compilation-exception(Any :$panic) {
        my $sorries := self.all-sorries;
        my $worries := self.all-worries;
        my int $num-sorries := nqp::elems(RakuAST::Node.IMPL-UNWRAP-LIST($sorries));
        my int $num-worries := nqp::elems(RakuAST::Node.IMPL-UNWRAP-LIST($worries));
        if $panic && $num-sorries == 0 && $num-worries == 0 {
            # There's just the panic, so return it without an enclosing group.
            return $panic;
        }
        elsif !$panic && $num-sorries == 1 && $num-worries == 0 {
            # Only one sorry and no worries, so no need to wrap that either.
            return RakuAST::Node.IMPL-UNWRAP-LIST($sorries)[0];
        }
        elsif $num-sorries || $num-worries {
            # Resolve the group exception type.
            my $XCompGroup-res := self.resolve-name-constant-in-setting:
                RakuAST::Name.from-identifier-parts('X', 'Comp', 'Group');
            if $XCompGroup-res {
                my $XCompGroup := $XCompGroup-res.compile-time-value;
                return $panic
                    ?? $XCompGroup.new(:$panic, :sorrows($sorries), :$worries)
                    !! $XCompGroup.new(:sorrows($sorries), :$worries)
            }
            # Fallback if missing group.
            elsif $panic {
                return $panic;
            }
            else {
                my @sorries := RakuAST::Node.IMPL-UNWRAP-LIST($sorries);
                return @sorries[0] if @sorries;
            } 
        }
        Nil
    }

    # Returns True if there are any compilation errors (worries don't count).
    method has-compilation-errors() {
        RakuAST::Node.IMPL-UNWRAP-LIST(self.all-sorries) ?? True !! False
    }

    # Gathers all sorries (from check time, if performed, and any specific to
    # a given resolver).
    method all-sorries() {
        my @sorries;
        if $!nodes-with-check-time-problems {
            for $!nodes-with-check-time-problems -> $node {
                for $node.IMPL-UNWRAP-LIST($node.sorries) {
                    @sorries.push($_);
                }
            }
        }
        for RakuAST::Node.IMPL-UNWRAP-LIST(self.unresolved-symbol-exceptions()) {
            @sorries.push($_);
        }
        RakuAST::Node.IMPL-WRAP-LIST(@sorries)
    }

    # Gathers all worries (from check time, if performed, and any specific to
    # a given resolver).
    method all-worries() {
        my @worries;
        if $!nodes-with-check-time-problems {
            for $!nodes-with-check-time-problems -> $node {
                for $node.IMPL-UNWRAP-LIST($node.worries) {
                    @worries.push($_);
                }
            }
        }
        RakuAST::Node.IMPL-WRAP-LIST(@worries)
    }

    # Form an undeclared symbols exception for all undeclared routines and
    # types, along with any other unresolved symbol exceptions.
    method unresolved-symbol-exceptions() {
        my %types;
        my %routines;
        my @exceptions;
        if $!nodes-unresolved-after-check-time {
            for $!nodes-unresolved-after-check-time -> $node {
                my $problem := $node.undeclared-symbol-details();
                if $problem {
                    $problem.IMPL-REPORT($node, %types, %routines, @exceptions);
                }
            }
        }
        if %routines || %types {
            @exceptions.push: self.build-exception: 'X::Undeclared::Symbols',
                :unk_types(nqp::hllizefor(%types, 'Raku')),
                :unk_routines(nqp::hllizefor(%routines, 'Raku'));
        }
        RakuAST::Node.IMPL-WRAP-LIST(@exceptions)
    }
}

# The EVAL resolver is used when we are given an AST as a whole, and visit it
# to perform resolutions. We expect a context and GLOBAL to be provided in this
# mode.
class RakuAST::Resolver::EVAL is RakuAST::Resolver {
    # The stack of scopes we are in (an array of RakuAST::LexicalScope).
    has Mu $!scopes;

    method new(Mu :$global!, Mu :$context!) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Resolver, '$!outer', $context);
        nqp::bindattr($obj, RakuAST::Resolver, '$!setting',
            self.IMPL-SETTING-FROM-CONTEXT($context));
        nqp::bindattr($obj, RakuAST::Resolver, '$!global', $global);
        nqp::bindattr($obj, RakuAST::Resolver, '$!attach-targets', nqp::hash());
        my $cur-package := $obj.resolve-lexical-constant-in-outer('$?PACKAGE');
        nqp::bindattr($obj, RakuAST::Resolver, '$!packages',
            $cur-package ?? [$cur-package] !! []);
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

    # Gets the current RakuAST::LexicalScope we are in during resolution.
    method current-scope() {
        $!scopes[nqp::elems($!scopes) - 1]
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
        my @scopes := $!scopes;
        my int $i := nqp::elems(@scopes);
        while $i-- {
            my $scope := @scopes[$i];
            my $found := $scope.find-lexical($name);
            if nqp::isconcrete($found) {
                if nqp::istype($found, RakuAST::CompileTimeValue) {
                    return $found;
                }
                else {
                    nqp::die("Symbol '$name' does not have a compile-time value");
                }
            }
        }
        self.resolve-lexical-constant-in-outer($name)
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

    # Sorries and worries produced by the compiler.
    has Mu $!sorries;
    has Mu $!worries;

    method new(Mu :$setting!, Mu :$outer!, Mu :$global!) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Resolver, '$!setting', $setting);
        nqp::bindattr($obj, RakuAST::Resolver, '$!outer', $outer);
        nqp::bindattr($obj, RakuAST::Resolver, '$!attach-targets', nqp::hash());
        nqp::bindattr($obj, RakuAST::Resolver, '$!global', $global);
        nqp::bindattr($obj, RakuAST::Resolver, '$!packages', []);
        nqp::bindattr($obj, RakuAST::Resolver::Compile, '$!scopes', []);
        $obj
    }

    # Create a resolver from a context and existing global. Used when we are
    # compiling a textual EVAL.
    method from-context(Mu :$context!, Mu :$global!) {
        my $setting := self.IMPL-SETTING-FROM-CONTEXT($context);
        self.new(:$setting, :outer($context), :$global)
    }

    # Create a resolver for a fresh compilation unit of the specified language
    # version.
    method from-setting(Str :$setting-name!) {
        my $loader := nqp::gethllsym('Raku', 'ModuleLoader');
        my $setting := $loader.load_setting($setting-name);
        # We can't actually have the global yet, since the resolver is
        # needed in order to look up the package meta-object used to
        # create it. Thus it's set later.
        self.new(:$setting, :outer($setting), :global(Mu))
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

    # Gets the current RakuAST::LexicalScope we are in during resolution.
    method current-scope() {
        $!scopes[nqp::elems($!scopes) - 1].scope
    }

    # Pops the top active lexical scope. Used only in batch resolve mode.
    method pop-scope() {
        my $scope := nqp::pop($!scopes);
        $scope.batch-mode ||
            nqp::die('pop-scope should only be used on batch mode scopes');
        if nqp::istype($scope.scope, RakuAST::AttachTarget) {
            self.pop-attach-target($scope.scope);
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
        if nqp::istype($scope.scope, RakuAST::AttachTarget) {
            self.pop-attach-target($scope.scope);
        }
        Nil
    }

    # Add a lexical declaration. Used when the compiler produces the declaration,
    # so that we can resovle it without requiring it to be linked into the tree.
    method declare-lexical(RakuAST::Declaration $decl) {
        $!scopes[nqp::elems($!scopes) - 1].declare-lexical($decl);
    }

    # Add a lexical declaration in the outer scope relative to the current one.
    # Used when the compiler produces the declaration, but already entered into
    # that declaration's inner scope.
    method declare-lexical-in-outer(RakuAST::Declaration $decl) {
        $!scopes[nqp::elems($!scopes) - 2].declare-lexical($decl);
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
        # Walk active scopes, most nested first.
        my @scopes := $!scopes;
        my int $i := nqp::elems(@scopes);
        while $i-- {
            my $scope := @scopes[$i];
            my $found := $scope.find-lexical($name);
            if nqp::isconcrete($found) {
                if nqp::istype($found, RakuAST::CompileTimeValue) {
                    return $found;
                }
                else {
                    nqp::die("Symbol '$name' does not have a compile-time value");
                }
            }
        }

        # Fall back to looking in outer scopes.
        self.resolve-lexical-constant-in-outer($name);
    }

    # Add a sorry check-time problem produced by the compiler.
    method add-sorry(Any $exception) {
        unless $!sorries {
            nqp::bindattr(self, RakuAST::Resolver::Compile, '$!sorries', []);
        }
        nqp::push($!sorries, $exception);
        Nil
    }

    # Add a worry check-time problem produced by the compiler.
    method add-worry(Any $exception) {
        unless $!worries {
            nqp::bindattr(self, RakuAST::Resolver::Compile, '$!worries', []);
        }
        nqp::push($!worries, $exception);
        Nil
    }

    # Panic with the specified exception. This immediately throws it,
    # incorporating any sorries and worries.
    method panic(Any $exception) {
        self.produce-compilation-exception(:panic($exception)).throw
    }

    # Gathers all sorries (from check time, if performed, and syntactic).
    method all-sorries() {
        my @sorries := RakuAST::Node.IMPL-UNWRAP-LIST:
            nqp::findmethod(RakuAST::Resolver, 'all-sorries')(self);
        if $!sorries {
            for $!sorries {
                @sorries.push($_);
            }
        }
        RakuAST::Node.IMPL-WRAP-LIST(@sorries)
    }

    # Gathers all worries (from check time, if performed, and syntactic).
    method all-worries() {
        my @worries := RakuAST::Node.IMPL-UNWRAP-LIST:
            nqp::findmethod(RakuAST::Resolver, 'all-worries')(self);
        if $!worries {
            for $!worries {
                @worries.push($_);
            }
        }
        RakuAST::Node.IMPL-WRAP-LIST(@worries)
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
            $!live-decl-map{$name} // $!scope.find-generated-lexical($name) // Nil
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
