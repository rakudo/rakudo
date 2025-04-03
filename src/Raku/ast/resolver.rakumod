# Role for common resolver functionality
class RakuAST::Resolver {
    # The setting.
    has Mu $.setting;

    # Our outer context. When not an EVAL, this is the same as $!setting.
    has Mu $!outer;

    # Our current idea of the global symbol table.
    has Mu $!global;

    # Current attachment target stacks (a hash keyed on the target name,
    # where each value is a stack, where the top is the active target).
    has Mu $!attach-targets;

    # Packages we're currently in.
    has Mu $!packages;

    # Nodes with check-time problems to report.
    has Mu $!nodes-with-check-time-problems;

    # Nodes that are unresolved after check time.
    has Mu $!nodes-unresolved-after-check-time;

    # The current comp unit's EXPORT package.
    has Mu $!export-package;

    # Create a shallow clone, but deep clone attach targets and packages
    method clone() {
        my $clone := nqp::clone(self);
        nqp::bindattr($clone,RakuAST::Resolver,'$!attach-targets',
          nqp::clone($!attach-targets));
        nqp::bindattr($clone,RakuAST::Resolver,'$!packages',
          nqp::clone($!packages));
        $clone
    }

    method IMPL-CLONE-ATTACH-TARGETS() {
        my %attach-targets;
        for $!attach-targets {
            %attach-targets{$_.key} := nqp::clone($_.value);
        }
        %attach-targets
    }

    # Push an attachment target, so children can attach to it.
    method push-attach-target(RakuAST::AttachTarget $target) {
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
            unless nqp::eqaddr(nqp::pop(@stack),$target) {
                nqp::die('Inconsistent attachment target stack for '
                  ~ $target.HOW.name($target));
            }
        }
        Nil
    }

    # Find the current (most recently pushed) attachment target with the
    # specified name, or return Nil if there is no target by the given name,
    # or no targets left for the given name.
    method find-attach-target(str $name, Bool :$skip-first) {
        my @stack := $!attach-targets{$name};
        nqp::isconcrete(@stack) && nqp::elems(@stack) > +$skip-first
          ?? @stack[nqp::elems(@stack) - (1 + $skip-first)]
          !! Nil
    }

    # Set the global package when we're starting a fresh compilation unit.
    method set-global(Mu $global) {
        nqp::die("This resolver's GLOBAL is already set")
          unless nqp::eqaddr($!global,Mu);
        nqp::bindattr(self, RakuAST::Resolver, '$!global', $global);
        Nil
    }

    method get-global() { $!global }

    # Set the EXPORT package when we're starting a fresh compilation unit.
    method set-export-package(Mu $package) {
        nqp::die("This resolver's EXPORT is already set")
          unless nqp::eqaddr($!export-package,Mu);
        nqp::bindattr(self,RakuAST::Resolver,'$!export-package',$package);
        Nil
    }

    # Push a package, intended to be used at a point a new package is "entered"
    method push-package(RakuAST::Package $package) {
        nqp::push($!packages, $package);
        self.push-attach-target($package);
        Nil
    }

    # Obtains the current package. This is not a RakuAST::Package node, but
    # rather the type object of the package we are currently in.
    method current-package() {
        if nqp::elems($!packages) {
            $!packages[nqp::elems($!packages) - 1].compile-time-value
        }
        else {
            my $current := $!outer;
            until nqp::eqaddr($current, $!setting) {
                return nqp::atkey($current, '$?PACKAGE')
                  if nqp::existskey($current, '$?PACKAGE');
                $current := nqp::ctxouterskipthunks($current);
            }
            $!global
        }
    }

    # Pops a package, intended to be use at the point a package is "left"
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
    method resolve-infix(Str $name) {
        self.resolve-lexical('&infix' ~ self.IMPL-CANONICALIZE-PAIR($name))
    }

    # Name-mangle a prefix operator and resolve it.
    method resolve-prefix(Str $name) {
        self.resolve-lexical('&prefix' ~ self.IMPL-CANONICALIZE-PAIR($name))
    }

    # Name-mangle a postfix operator and resolve it.
    method resolve-postfix(Str $name) {
        self.resolve-lexical('&postfix' ~ self.IMPL-CANONICALIZE-PAIR($name))
    }

    # Name-mangle a term and resolve it.
    method resolve-term(Str $name) {
        self.resolve-lexical('&term' ~ self.IMPL-CANONICALIZE-PAIR($name))
    }

    # Return the global package wrapped in a node
    method global-package() {
        RakuAST::VarDeclaration::Implicit::Constant.new(
          :name<GLOBAL>, :value($!global)
        )
    }

    # Resolve a RakuAST::Name, optionally adding the specified sigil to the
    # final component.
    method resolve-name(RakuAST::Name $Rname, str :$sigil) {
        my $found;
        if $Rname.is-identifier {
            my str $name := $Rname.canonicalize;
            return self.global-package() if $name eq 'GLOBAL';

            # Single-part name, so look lexically.
            $name  := $sigil ~ $name if $sigil;
            $found := self.resolve-lexical($name)
        }
        else {
            # All package name installations happen via the symbol table as
            # BEGIN-time effects, so chase it down as if it were a constant.
            $found := self.resolve-name-constant($Rname, :$sigil)
        }

        $found || self.IMPL-RESOLVE-NAME-IN-PACKAGES($Rname, :$sigil)
    }

    # Resolve a RakuAST::Name to a constant.
    method resolve-name-constant(RakuAST::Name $Rname, str :$sigil, :$current-scope-only) {
        self.IMPL-RESOLVE-NAME-CONSTANT($Rname, :$sigil, :$current-scope-only)
          // ($current-scope-only ?? Nil !! self.IMPL-RESOLVE-NAME-IN-PACKAGES($Rname, :$sigil))
    }

    # Resolve a RakuAST::Name to a constant looking only in the setting.
    method resolve-name-constant-in-setting(RakuAST::Name $Rname) {
        self.IMPL-RESOLVE-NAME-CONSTANT($Rname, :setting)
    }

    # Directly convert given name part(s) to setting type object
    method type-from-setting(*@parts) {
        self.resolve-name-constant-in-setting(
          +@parts == 1
            ?? RakuAST::Name.from-identifier(@parts[0])
            !! RakuAST::Name.from-identifier-parts(|@parts)
        ).compile-time-value
    }

    # Helper method to create node for name in given stash
    method external-constant(Mu $stash, str $lexical-name) {
        RakuAST::Declaration::External::Constant.new(
          :$lexical-name, :compile-time-value(nqp::atkey($stash,$lexical-name))
        )
    }

    # Resolve a constant in the currently known packages, or GLOBAL
    method IMPL-RESOLVE-NAME-IN-PACKAGES($Rname, :$sigil, Bool :$partial) {
        # Try looking in the packages
        my str $name := $Rname.canonicalize;
# This breaks "our &foo" lookup.  But if the sigil isn't needed, why is
# it being passed as an argument then???   XXX
#        $name := $sigil ~ $name if $sigil;

        for $!packages {
            my $stash := self.IMPL-STASH-HASH($_.compile-time-value);
            return $partial
                ?? ($stash{$name}, List.new, 'global')
                !! self.external-constant($stash, $name)
                if nqp::existskey($stash,$name);
        }

        my $symbol := $!global;
        my @parts := nqp::clone($Rname.IMPL-UNWRAP-LIST($Rname.parts));
        while @parts {
            my $part := @parts.shift;
            $name    := nqp::istype($part,RakuAST::Name::Part::Simple)
              ?? $part.name
              !! '';

            # Add any sigil for last iteration
            $name := $sigil ~ $name unless @parts;

            # Lookup in the current symbol's stash
            my $next := nqp::atkey(self.IMPL-STASH-HASH($symbol), $name);
            if nqp::isnull($next) {
                if $partial && ! $symbol =:= $!global {
                    # put the symbol we failed to resolve back into the list
                    nqp::unshift(@parts, $part);
                    return ($symbol, $Rname.IMPL-WRAP-LIST(@parts), 'global');
                }
                else {
                    return Nil
                }
            }
            $symbol := $next;
        }

        $symbol =:= $!global
            ?? Nil
            !! $partial
                ?? ($symbol, List.new, 'global')
                !! RakuAST::Declaration::External::Constant.new(
                    lexical-name => $name, compile-time-value => $symbol
                )
    }

    method IMPL-RESOLVE-NAME-CONSTANT(
      RakuAST::Name  $constant,
               Bool :$setting,
               Bool :$partial,
               Bool :$current-scope-only,
                str :$sigil
    ) {
        nqp::die('Empty name lookup not possible as a constant')
            if $constant.is-empty;
        my @parts := nqp::clone($constant.IMPL-UNWRAP-LIST($constant.parts));
        nqp::shift(@parts) if nqp::istype(@parts[0], RakuAST::Name::Part::Empty);

        my $root := @parts.shift;
        # TODO pseudo-packages
        # TODO GLOBALish fallback
        if nqp::istype($root, RakuAST::Name::Part::Empty) {
            return Nil;
        }
        elsif nqp::istype($root, RakuAST::Name::Part::Expression) && !$root.has-compile-time-name {
            return Nil;
        }

        # Resolve the root part.
        my $name     := $root.name;
        my str $setting-rev;
        if ($name eq 'CORE') {
            $root := nqp::shift(@parts);
            if nqp::istype($root, RakuAST::Name::Part::Empty) {
                return Nil;
            }
            elsif nqp::istype($root, RakuAST::Name::Part::Expression) && !$root.has-compile-time-name {
                return Nil;
            }
            $name := $root.name;
            $setting := True;
            if (nqp::chars($name) == 3 && nqp::index($name, 'v6') == 0) {
                $setting-rev := $name;
                $root := nqp::shift(@parts);
                $name := $root.name;
            }
        }
        my $resolved := $name eq 'GLOBAL'
          ?? self.global-package()
          !! $name eq 'EXPORT'
            ?? RakuAST::Declaration::ResolvedConstant.new(
                 compile-time-value => $!export-package
               )
            !! $setting
              ?? self.resolve-lexical-constant-in-setting($name, :$setting-rev)
              !! self.resolve-lexical-constant($name, :$current-scope-only);
        $resolved
          ?? (my $symbol := $resolved.compile-time-value)
          !! (return Nil);

        # Other parts to resolve.
        if @parts {
            # Chase down through the packages until we find something.
            while @parts {
                my $part := @parts.shift;
                $name    := nqp::istype($part,RakuAST::Name::Part::Simple)
                  ?? $part.name
                  !! '';

                # Add any sigil for last iteration
                $name := $sigil ~ $name ~ $constant.colonpair-suffix unless @parts;

                # Lookup in the current symbol's stash
                my $next := nqp::atkey(self.IMPL-STASH-HASH($symbol),$name);
                if nqp::isnull($next) {
                    if $partial {
                        # put the symbol we failed to resolve back into the list
                        nqp::unshift(@parts, $part);
                        return ($symbol, $constant.IMPL-WRAP-LIST(@parts), 'lexical');
                    }
                    else {
                        return Nil
                    }
                }
                $symbol := $next;
            }

            # Wrap it.
            $resolved := RakuAST::Declaration::ResolvedConstant.new(
              compile-time-value => $symbol
            );
        }

        $partial
            ?? (
                $symbol,
                nqp::stmts(
                    (my $list := nqp::create(List)),
                    nqp::bindattr($list, List, '$!reified', nqp::create(IterationBuffer)),
                    $list
                ),
                'lexical'
            )
            !! $resolved
    }

    # Resolve a RakuAST::Name to a constant.
    method partially-resolve-name-constant(RakuAST::Name $Rname, str :$sigil) {
        self.IMPL-RESOLVE-NAME-CONSTANT($Rname, :$sigil, :partial)
            // self.IMPL-RESOLVE-NAME-IN-PACKAGES($Rname, :$sigil, :partial)
    }

    method IMPL-STASH-HASH(Mu $pkg) {
        nqp::ishash(my $hash := $pkg.WHO)
          ?? $hash
          !! nqp::getattr($hash, Map, '$!storage')
    }

    # Resolves a lexical in the chain of outer contexts.
    method resolve-lexical-in-outer(Str $name, Bool :$current-scope-only) {

        # Mapping primspec to apprpriate native type
        my constant PRIMSPEC-TO-TYPE :=
          nqp::list(Mu, int, num, str, Mu, Mu, Mu, Mu, Mu, Mu, uint);

        # Look through the contexts for the name.
        my $context := $!outer;
        my int $seen-setting;
        until nqp::isnull($context) {
            $seen-setting := 1 if nqp::existskey($context,'CORE-SETTING-REV');

            # found it!
            if nqp::existskey($context,$name) {
                my $prim-spec := nqp::lexprimspec($context,$name);
                if $prim-spec {
                    return RakuAST::Declaration::External.new(
                      :lexical-name($name),
                      :native-type(PRIMSPEC-TO-TYPE[$prim-spec])
                    );
                }

                # Non-native things in the setting are assumed constant
                elsif $seen-setting {
                    return RakuAST::Declaration::External::Setting.new(
                      :lexical-name($name),
                      :compile-time-value(nqp::atkey($context, $name))
                    );
                }

                else {
                    return RakuAST::Declaration::External.new(
                      :lexical-name($name),
                      :maybe-compile-time-value(nqp::atkey($context, $name))
                    );
                }
            }

            # not found, and requested to only look in this scope
            elsif $current-scope-only {
                return Nil;
            }

            $context := nqp::ctxouter($context);
        }

        # Nothing found.
        Nil
    }

    # Helper method for grammar / actions to quickly access any type from
    # the setting.
    method setting-constant(*@name) {
        nqp::isconcrete(
          my $resolved := self.resolve-name-constant-in-setting(
              RakuAST::Name.from-identifier-parts(|@name)
            )
        ) ?? $resolved.compile-time-value
          !! $resolved
    }

    # Helper method to resolve a lexical in given context. The declaration
    # must have a compile-time value.
    method resolve-lexical-constant-in-context(Mu $context, Str $name) {
        until nqp::isnull($context) {
            nqp::existskey($context, $name)
              ?? (return self.external-constant($context, $name))
              !! ($context := nqp::ctxouter($context));
        }
        Nil
    }

    # Resolves a lexical using the outer contexts. The declaration must have a
    # compile-time value.
    method resolve-lexical-constant-in-outer(Str $name) {
        self.resolve-lexical-constant-in-context($!outer, $name)
    }

    # Resolves a lexical using the outer contexts. The declaration must have a
    # compile-time value.
    method resolve-lexical-constant-in-setting(Str $name, str :$setting-rev) {
        if $setting-rev {
            my $setting := $!setting;
            my $rev := nqp::substr($setting-rev, 2, 1);
            until nqp::isnull($setting) {
                nqp::existskey(nqp::ctxlexpad($setting), 'CORE-SETTING-REV') && nqp::ctxlexpad($setting)<CORE-SETTING-REV> eq $rev
                  ?? (return self.external-constant($setting, $name))
                  !! ($setting := nqp::ctxouter($setting));
            }
            nqp::die("Could not find setting revision $setting-rev trying to look up $name");
        }
        else {
            $!setting
                ?? self.resolve-lexical-constant-in-context($!setting, $name)
                !! self.resolve-lexical-constant($name) # Compiling CORE.setting
        }
    }

    method IMPL-SETTING-FROM-CONTEXT(Mu $context) {
        # TODO locate the setting frame
        until nqp::isnull($context) {
            nqp::existskey(nqp::ctxlexpad($context),'CORE-SETTING-REV')
              ?? (return $context)
              !! ($context := nqp::ctxouterskipthunks($context));
        }
        Nil
    }

    # Helper method to handle proper embedding of a name in the
    # appropriat pointy brackets, with a colon prefixed, to be used
    # in naming prefix / infix / postfix / terms.
    method IMPL-CANONICALIZE-PAIR(Str $v) {
        if $v ~~ /<[ < > ]>/ && !($v ~~ /<[ « » $ \\ " ' ]>/) {
            ':«' ~ $v ~ '»'
        }
        else {
            my $new := '';
            my int $i;
            my int $e := nqp::chars($v);
            while $i < $e {
                my $ch := nqp::substr($v,$i,1);
                $new := $new ~ '\\' if $ch eq '<' || $ch eq '>';
                $new := $new ~ $ch;
                ++$i;
            }
            ':<' ~ $new ~ '>';
        }
    }

    # Check if an identifier is a known type.
    method is-identifier-type(Str $identifier) {
        # Can optimize this later to avoid the throwaway name creation
        self.is-name-type(RakuAST::Name.from-identifier($identifier))
    }

    # Check if a name is a known type.
    method is-name-type(RakuAST::Name $Rname) {
        my $constant := self.resolve-name($Rname);
        if nqp::istype($constant, RakuAST::CompileTimeValue) {
            # Name resolves, but is it an instance or a type object?
            my $meta-object := $constant.compile-time-value;
            nqp::isnull($meta-object) || nqp::isconcrete_nd($meta-object)
                ?? False
                !! True
        }
        else {
             # Name doesn't resolve to a constant at all, so can't be a type.
            False
        }
    }

    # Check if an identifier is known (declared) at all.
    method is-identifier-known(Str $identifier, :$exact) {
        nqp::isconcrete(self.resolve-lexical($identifier))
          ?? True
          !! $exact
            ?? False
            !! nqp::isconcrete(self.resolve-lexical('&' ~ $identifier))
              ?? True
              !! False
    }

    # Check if a name is known (declared) at all.
    method is-name-known(RakuAST::Name $Rname) {
        $Rname.is-pseudo-package || nqp::isconcrete(self.resolve-name($Rname.base-name))
          ?? True
          !! False
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
            my $message := $type-name;
            $message := "$message(";
            for %opts {
                $message := $message ~ $_.key ~ " => " ~ $_.value ~ ", ";
            }
            $message := "$message)";
            RakuAST::BOOTException.new($message);
        }
    }

    method convert-exception(Mu $ex) {
        my $Exception := self.resolve-name-constant-in-setting(RakuAST::Name.from-identifier('Exception'));
        return $ex unless $Exception;
        unless nqp::istype($ex, $Exception.compile-time-value) {
            my $coercer := self.resolve-name-constant-in-setting(RakuAST::Name.from-identifier('&COMP_EXCEPTION'));
            if $coercer {
                $ex := $coercer.compile-time-value()($ex);
            }
            else {
                return $ex;
            }
        }
        unless nqp::can($ex, 'SET_FILE_LINE') {
            try {
                my $XComp := self.resolve-name-constant-in-setting(RakuAST::Name.from-identifier-parts('X', 'Comp'));
                $ex.HOW.mixin($ex, $XComp.compile-time-value).BUILD_LEAST_DERIVED(nqp::hash());
            }
        }
        $ex
    }

    method convert-begin-time-exception(Mu $ex) {
        $ex := self.convert-exception($ex);
        my $xcbt := self.resolve-name(RakuAST::Name.from-identifier-parts('X', 'Comp', 'BeginTime'));
        $ex := $xcbt.compile-time-value.new(:exception($ex), :use-case('evaluating a BEGIN')) if $xcbt;
        $ex
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

    method have-check-time-problems() {
        ($!nodes-with-check-time-problems || $!nodes-unresolved-after-check-time)
            ?? True !! False
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
            $panic
        }
        elsif !$panic && $num-sorries == 1 && $num-worries == 0 {
            # Only one sorry and no worries, so no need to wrap that either.
            $sorries.AT-POS(0)
        }
        elsif $num-sorries || $num-worries {
            # Resolve the group exception type.
            my $XCompGroup-res := self.resolve-name-constant-in-setting:
                RakuAST::Name.from-identifier-parts('X', 'Comp', 'Group');
            if $XCompGroup-res {
                my $XCompGroup := $XCompGroup-res.compile-time-value;
                if !$panic && $num-sorries {
                    $panic := nqp::pop(RakuAST::Node.IMPL-UNWRAP-LIST($sorries));
                }
                $panic
                  ?? $XCompGroup.new(:$panic, :$sorries, :$worries)
                  !! $XCompGroup.new(         :$sorries, :$worries)
            }
            # Fallback if missing group.
            else {
                $panic || $sorries.AT-POS(0)
            }
        }
        else {
            Nil
        }
    }

    # Returns True if there are any compilation errors (worries don't count).
    method has-compilation-errors() { self.all-sorries.Bool }

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
        for RakuAST::Node.IMPL-UNWRAP-LIST(self.unresolved-symbol-exceptions) {
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
        my $filename := nqp::null;
        if $!nodes-unresolved-after-check-time {
            for $!nodes-unresolved-after-check-time -> $node {
                if nqp::isnull($filename) {
                    $filename := $node.origin
                        ?? $node.origin.source.original-file
                        !! '<unknown file>';
                }
                my $problem := $node.undeclared-symbol-details();
                if $problem {
                    $problem.IMPL-REPORT($node, %types, %routines, @exceptions);
                }
            }
        }
        if %routines || %types {
            my %routine-suggestion;
            my %type-suggestion;
            my %post-types;
            for %routines {
                my $name := $_.key;
                my @suggestions := self.suggest-routines($name);
                %routine-suggestion{$name} := @suggestions;

                my $constant := self.resolve-name(RakuAST::Name.from-identifier($name));
                if nqp::istype($constant, RakuAST::CompileTimeValue) {
                    # Name resolves, but is it an instance or a type object?
                    my $meta-object := $constant.compile-time-value;
                    unless nqp::isnull($meta-object) || nqp::isconcrete_nd($meta-object) {
                        %post-types{$name} := [] unless nqp::existskey(%post-types, $name);
                        nqp::push(%post-types{$name}, $constant.origin ?? $constant.origin.from !! -1);
                    }
                }
            }
            for %types {
                my $name := $_.key;
                my @suggestions := self.suggest-routines($name);
                %type-suggestion{$name} := @suggestions;
            }

            if nqp::elems(%routines) == 0 && nqp::elems(%types) == 1 && nqp::elems(%post-types) == 0 {
                for %types {
                    @exceptions.push: self.build-exception: 'X::Undeclared',
                        :$filename,
                        :what<Type>,
                        :suggestions(nqp::hllizefor(%type-suggestion, 'Raku')),
                        :symbol($_.key),
                }
            }
            else {
                @exceptions.push: self.build-exception: 'X::Undeclared::Symbols',
                    :$filename,
                    :routine_suggestion(nqp::hllizefor(%routine-suggestion, 'Raku')),
                    :type_suggestion(nqp::hllizefor(%type-suggestion, 'Raku')),
                    :unk_types(nqp::hllizefor(%types, 'Raku')),
                    :unk_routines(nqp::hllizefor(%routines, 'Raku')),
                    :post_types(nqp::hllizefor(%post-types, 'Raku'));
            }
        }
        RakuAST::Node.IMPL-WRAP-LIST(@exceptions)
    }
}

#-------------------------------------------------------------------------------
# The EVAL resolver is used when we are given an AST as a whole, and visit it
# to perform resolutions. We expect a context and GLOBAL to be provided in this
# mode.
class RakuAST::Resolver::EVAL
  is RakuAST::Resolver
{
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

    # Return a clone, with a clone of all scopes
    method clone() {
        my $clone := nqp::findmethod(RakuAST::Resolver, 'clone')(self);
        nqp::bindattr($clone, RakuAST::Resolver::EVAL, '$!scopes', nqp::clone($!scopes));
        $clone
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
    method outer-scope() {
        $!scopes[nqp::elems($!scopes) - 2]
    }

    # Pops the top active lexical scope.
    method pop-scope() {
        my $scope := $!scopes.pop;
        self.pop-attach-target($scope)
          if nqp::istype($scope, RakuAST::AttachTarget);
        Nil
    }

    # Walks scopes from inner to outer and returns the first concrete value
    # returned from the evaluator.
    method find-scope-property(Code $evaluator) {
        # Walk active scopes, most nested first.
        my @scopes := $!scopes;
        my int $i := nqp::elems(@scopes);
        while $i-- {
            my $scope := @scopes[$i];
            my $res := $evaluator($scope);
            return $res if nqp::isconcrete($res);
        }
        Nil
    }

    # Resolves a lexical to its declaration. The declaration need not have a
    # compile-time value.
    method resolve-lexical(Str $name, Bool :$current-scope-only) {
        my @scopes := $!scopes;

        # If it's in the current scope only, just look at the top one, if any
        if $current-scope-only {
            return nqp::elems(@scopes)
              ?? @scopes[nqp::elems(@scopes) - 1].find-lexical($name)
              !! Nil;
        }

        # No need to look further
        if $name eq 'GLOBAL' {
            self.global-package;
        }

        # Walk active scopes, most nested first.
        else {
            my int $i := nqp::elems(@scopes);
            while $i-- {
                my $found := @scopes[$i].find-lexical($name);
                return $found if nqp::isconcrete($found);
            }

            # Fallback handling
            self.resolve-lexical-in-outer($name)
        }
    }

    # Resolves a lexical to its declaration. The declaration must have a
    # compile-time value.
    method resolve-lexical-constant(Str $name, Bool :$current-scope-only) {

        # No need to look further
        if $name eq 'GLOBAL' {
            self.global-package;
        }

        # If it's in the current scope only, just look at the top one, if any
        elsif $current-scope-only {
            my @scopes := $!scopes;
            my $found := nqp::elems(@scopes)
                ?? @scopes[nqp::elems(@scopes) - 1].find-lexical($name)
                !! Nil;
            if nqp::isconcrete($found) {
                if nqp::istype($found,RakuAST::CompileTimeValue) {
                    return $found;
                }
                else {
                    nqp::die("Symbol '$name' does not have a compile-time value");
                }
            }
            Nil
        }

        # Walk active scopes, most nested first.
        else {
            my @scopes := $!scopes;
            my int $i  := nqp::elems(@scopes);
            while $i-- {
                my $found := @scopes[$i].find-lexical($name);
                if nqp::isconcrete($found) {
                    nqp::istype($found,RakuAST::CompileTimeValue)
                      ?? (return $found)
                      !! nqp::die(
                           "Symbol '$name' does not have a compile-time value"
                         );
                }
            }

            # Fallback handling
            self.resolve-lexical-constant-in-outer($name)
        }
    }
}

#-------------------------------------------------------------------------------
# The compiler resolver is used in the situation we are parsing code and
# building up a RakuAST as we go. We thus need to provide symbol resolutions
# for the sake of parse disambiguation, as well as to handle BEGIN-time code.
# A resolver may be created using an existing context, or it may be for a
# compilation unit whose outer scope is some version of the setting.
class RakuAST::Resolver::Compile
  is RakuAST::Resolver
{
    # Scopes stack; an array of RakuAST::Resolver::Compile::Scope.
    has Mu $!scopes;

    # Sorries and worries produced by the compiler.
    has Mu $!sorries;
    has Mu $!worries;

    # Create a resolver from given arguments
    method new(Mu :$setting!, Mu :$outer!, Mu :$global!, Mu :$scopes, Mu :$attach-targets) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Resolver, '$!setting', $setting);
        nqp::bindattr($obj, RakuAST::Resolver, '$!outer', $outer);
        nqp::bindattr($obj, RakuAST::Resolver, '$!attach-targets', $attach-targets // nqp::hash());
        nqp::bindattr($obj, RakuAST::Resolver, '$!global', $global);
        nqp::bindattr($obj, RakuAST::Resolver, '$!packages', []);

        nqp::bindattr($obj, RakuAST::Resolver::Compile, '$!scopes',
          $scopes // []);
        nqp::bindattr($obj, RakuAST::Resolver::Compile, '$!sorries', []);
        nqp::bindattr($obj, RakuAST::Resolver::Compile, '$!worries', []);
        $obj
    }

    # Create a resolver from a context and existing global. Used when we are
    # compiling a textual EVAL.
    method from-context(Mu :$context!, Mu :$global!, RakuAST::Resolver :$resolver) {
        my $setting := $resolver
            ?? nqp::getattr($resolver, RakuAST::Resolver, '$!setting')
            !! self.IMPL-SETTING-FROM-CONTEXT($context);
        self.new(
            :$setting,
            :outer($resolver ?? $setting !! $context),
            :$global,
            :scopes($resolver ?? nqp::clone(nqp::getattr($resolver, RakuAST::Resolver::Compile, '$!scopes')) !! Mu),
            :attach-targets($resolver ?? $resolver.IMPL-CLONE-ATTACH-TARGETS !! Mu),
        )
    }

    method clone() {
        my $clone := nqp::findmethod(RakuAST::Resolver, 'clone')(self);
        nqp::bindattr($clone, RakuAST::Resolver::Compile, '$!scopes', nqp::clone($!scopes));
        $clone
    }

    # Create a resolver for a fresh compilation unit of the specified language
    # version.
    method from-setting(Str :$setting-name!) {
        my $loader := nqp::gethllsym('Raku', 'ModuleLoader');
        my $setting := $setting-name eq 'NULL.c' ?? nqp::null !! $loader.load_setting($setting-name);
        # We can't actually have the global yet, since the resolver is
        # needed in order to look up the package meta-object used to
        # create it. Thus it's set later.
        self.new(:$setting, :outer($setting), :global(Mu))
    }

    method set-setting(Str :$setting-name!) {
        my $loader := nqp::gethllsym('Raku', 'ModuleLoader');
        my $setting := $loader.load_setting($setting-name);
        nqp::bindattr(self, RakuAST::Resolver, '$!setting', $setting);
        nqp::bindattr(self, RakuAST::Resolver, '$!outer', $setting);
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
    method outer-scope() {
        $!scopes[nqp::elems($!scopes) - 2].scope
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

    method re-enter-scope(RakuAST::Resolver::Compile::Scope $scope) {
        nqp::push($!scopes, $scope);
        if nqp::istype($scope.scope, RakuAST::AttachTarget) {
            self.push-attach-target($scope.scope);
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
        $scope
    }

    # Walks scopes from inner to outer and returns the first concrete value
    # returned from the evaluator.
    method find-scope-property(Code $evaluator) {
        # Walk active scopes, most nested first.
        my @scopes := $!scopes;
        my int $i := nqp::elems(@scopes);
        while $i-- {
            my $scope := @scopes[$i];
            my $res := $evaluator($scope.scope);
            return $res if nqp::isconcrete($res);
        }
        Nil
    }

    # Add a lexical declaration. Used when the compiler produces the
    # declaration, so that we can resolve it without requiring it to be
    # linked into the tree.
    method declare-lexical(RakuAST::Declaration $decl) {
        CATCH {
            if nqp::istype(nqp::getpayload($_), RakuAST::Exception::TooComplex) {
                self.build-exception('X::Syntax::Extension::TooComplex', name => nqp::getpayload($_).name).throw;
            }
            nqp::rethrow($_);
        }
        $!scopes[nqp::elems($!scopes) - 1].declare-lexical($decl)
    }

    # Add a lexical declaration in the outer scope relative to the current one.
    # Used when the compiler produces the declaration, but already entered into
    # that declaration's inner scope.
    method declare-lexical-in-outer(RakuAST::Declaration $decl) {
        CATCH {
            if nqp::istype(nqp::getpayload($_), RakuAST::Exception::TooComplex) {
                self.build-exception('X::Syntax::Extension::TooComplex', name => nqp::getpayload($_).name).throw;
            }
            nqp::rethrow($_);
        }
        $!scopes[nqp::elems($!scopes) - 2].declare-lexical($decl)
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

        if $name eq 'GLOBAL' {
            return self.global-package;
        }

        # Walk active scopes, most nested first.
        my @scopes := $!scopes;
        my int $i := nqp::elems(@scopes);
        while $i-- {
            my $scope := @scopes[$i];
            my $found := $scope.find-lexical($name);
            return $found if nqp::isconcrete($found);
        }

        self.resolve-lexical-in-outer($name)
    }

    # Resolves a lexical to its declaration. The declaration must have a
    # compile-time value.
    method resolve-lexical-constant(Str $name, Bool :$current-scope-only) {
        if $name eq 'GLOBAL' {
            return self.global-package;
        }

        # If it's in the current scope only, just look at the top one, if any
        if $current-scope-only {
            my @scopes := $!scopes;
            my int $i := nqp::elems(@scopes);
            if ($i > 0) {
                my $found := @scopes[$i - 1].find-lexical($name);
                if nqp::isconcrete($found) {
                    if nqp::istype($found, RakuAST::CompileTimeValue) {
                        return $found;
                    }
                    else {
                        nqp::die("Symbol '$name' does not have a compile-time value");
                    }
                }
            }
            return Nil;
        }

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
        nqp::push($!sorries, $exception);
        Nil
    }
    method has-sorries() { nqp::elems($!sorries) > 0 }

    # Add a worry check-time problem produced by the compiler.
    method add-worry(Any $exception) {
        my $worries := self.find-scope-property(-> $scope { $scope.tell-worries });
        if !nqp::isconcrete($worries) || $worries {
            nqp::push($!worries, $exception);
        }
        Nil
    }
    method has-worries() { nqp::elems($!worries) > 0 }

    # Panic with the specified exception. This immediately throws it,
    # incorporating any sorries and worries.
    method panic(Any $exception) {
        self.produce-compilation-exception(:panic($exception)).throw
    }

    # Gathers all sorries (from check time, if performed, and syntactic).
    method all-sorries() {
        my @sorries := RakuAST::Node.IMPL-UNWRAP-LIST:
            nqp::findmethod(RakuAST::Resolver,'all-sorries')(self);
        for $!sorries {
            @sorries.push($_);
        }
        RakuAST::Node.IMPL-WRAP-LIST(@sorries)
    }

    # Gathers all worries (from check time, if performed, and syntactic).
    method all-worries() {
        my @worries := RakuAST::Node.IMPL-UNWRAP-LIST:
            nqp::findmethod(RakuAST::Resolver,'all-worries')(self);
        for $!worries {
            @worries.push($_);
        }
        RakuAST::Node.IMPL-WRAP-LIST(@worries)
    }

    # this levenshtein implementation is used to suggest good alternatives
    # when deriving from an unknown/typo'd class.
    method levenshtein($a, $b) {
        my %memo;
        my int $alen := nqp::chars($a);
        my int $blen := nqp::chars($b);

        return 0 if $alen == 0 || $blen == 0;

        my sub changecost(str $ac, str $bc) {
            my sub issigil($_) { nqp::index('$@%&|', $_) != -1 };
            return 0 if $ac eq $bc;
            return 0.1 if nqp::fc($ac) eq nqp::fc($bc);
            return 0.5 if issigil($ac) && issigil($bc);
            1;
        }

        my sub levenshtein_impl(int $apos, int $bpos, num $estimate) {
            my $key := "$apos:$bpos";

            return %memo{$key} if nqp::existskey(%memo, $key);

            # if either cursor reached the end of the respective string,
            # the result is the remaining length of the other string.
            my sub check(int $pos1, int $len1, int $pos2, int $len2) {
                if $pos2 == $len2 {
                    return $len1 - $pos1;
                }
                -1;
            }

            my int $check := check($apos, $alen, $bpos, $blen);
            return $check unless $check == -1;
            $check := check($bpos, $blen, $apos, $alen);
            return $check unless $check == -1;

            my str $achar := nqp::substr($a, $apos, 1);
            my str $bchar := nqp::substr($b, $bpos, 1);

            my num $cost := changecost($achar, $bchar);

            # hyphens and underscores cost half when adding/deleting.
            my num $addcost := 1;
            $addcost := 0.5 if $bchar eq "-" || $bchar eq "_";

            my num $delcost := 1;
            $delcost := 0.5 if $achar eq "-" || $achar eq "_";

            my num $ca := nqp::add_n(levenshtein_impl($apos+1, $bpos,   nqp::add_n($estimate, $delcost)), $delcost); # what if we remove the current letter from A?
            my num $cb := nqp::add_n(levenshtein_impl($apos,   $bpos+1, nqp::add_n($estimate, $addcost)), $addcost); # what if we add the current letter from B?
            my num $cc := nqp::add_n(levenshtein_impl($apos+1, $bpos+1, nqp::add_n($estimate, $cost)), $cost); # what if we change/keep the current letter?

            # the result is the shortest of the three sub-tasks
            my num $distance;
            $distance := $ca if nqp::isle_n($ca, $cb) && nqp::isle_n($ca, $cc);
            $distance := $cb if nqp::isle_n($cb, $ca) && nqp::isle_n($cb, $cc);
            $distance := $cc if nqp::isle_n($cc, $ca) && nqp::isle_n($cc, $cb);

            # switching two letters costs only 1 instead of 2.
            if $apos + 1 <= $alen && $bpos + 1 <= $blen &&
               nqp::eqat($a, $bchar, $apos + 1) && nqp::eqat($b, $achar, $bpos + 1) {
                my num $cd := nqp::add_n(levenshtein_impl($apos+2, $bpos+2, nqp::add_n($estimate, 1)), 1);
                $distance := $cd if nqp::islt_n($cd, $distance);
            }

            %memo{$key} := $distance;
        }

        return levenshtein_impl(0, 0, 0e0);
    }

    method make_levenshtein_evaluator($orig_name, @candidates) {
        my int $find-count;
        my int $try-count;
        my &inner := my sub ($name) {
            # difference in length is a good lower bound.
            ++$try-count;
            return 0 if $find-count > 20 || $try-count > 1000;
            my $parlen := nqp::chars($orig_name);
            my $lendiff := nqp::chars($name) - $parlen;
            $lendiff := -$lendiff if $lendiff < 0;
            return 1 if nqp::isge_n($lendiff, nqp::mul_n($parlen, 0.3));

            my num $dist := nqp::div_n(self.levenshtein($orig_name, $name), $parlen);
            my $target := -1;
            $target := @candidates[0] if nqp::isle_n($dist, 0.1);
            $target := @candidates[1] if nqp::islt_n(0.1, $dist) && nqp::isle_n($dist, 0.2);
            $target := @candidates[2] if nqp::islt_n(0.2, $dist) && nqp::isle_n($dist, 0.35);
            if $target != -1 {
                my $name-str := nqp::box_s($name, Str);
                nqp::push($target, $name-str);
                ++$find-count;
            }
            1;
        }
        return &inner;
    }

    method levenshtein_candidate_heuristic(@candidates, $target) {
        # only take a few suggestions
        my $to-add := 5;
        for @candidates[0] {
            $target.push($_) if $to-add > 0;
            $to-add := $to-add - 1;
        }
        $to-add := $to-add - 1 if +@candidates[0] > 0;
        for @candidates[1] {
            $target.push($_) if $to-add > 0;
            $to-add := $to-add - 1;
        }
        $to-add := $to-add - 2 if +@candidates[1] > 0;
        for @candidates[2] {
            $target.push($_) if $to-add > 0;
            $to-add := $to-add - 1;
        }
    }

    method walk-scopes(Hash $seen, Code $inner-evaluator) {
        # Walk active scopes, most nested first.
        my @scopes := $!scopes;
        my int $i := nqp::elems(@scopes);
        while $i-- {
            my $scope := @scopes[$i];
            for $scope.lexical-declarations {
                my $name := $_.lexical-name;
                next if nqp::existskey($seen, $name);
                $seen{$name} := 1;
                $inner-evaluator($name);
            }
        }

        my $ctx := nqp::getattr(self, RakuAST::Resolver, '$!outer');
        while !nqp::isnull($ctx) {
            for $ctx -> $name {
                next if nqp::existskey($seen, $name);
                $seen{$name} := 1;
                $inner-evaluator($name);
            }
            $ctx := nqp::ctxouter($ctx);
        }
    }

    method suggest-lexicals(Str $name) {
        my @suggestions;
        my @candidates := [[], [], []];
        my &inner-evaluator := self.make_levenshtein_evaluator($name, @candidates);
        my %seen;
        %seen{$name} := 1;

        self.walk-scopes(%seen, &inner-evaluator);

        self.levenshtein_candidate_heuristic(@candidates, @suggestions);
        return @suggestions;
    }

    method suggest-routines(Str $name) {
        my $with_sigil := nqp::eqat($name, '&', 0);
        $name := '&' ~ $name unless $with_sigil;
        my @suggestions;
        my @candidates := [[], [], []];
        my &inner-evaluator := self.make_levenshtein_evaluator($name, @candidates);
        my %seen;
        %seen{$name} := 1;

        # RT 126264
        # Since there's no programmatic way to get a list of all phasers
        # applicable to the current scope, just check against this list
        # of all of them that aren't already the names of routines
        for <&BEGIN &CHECK &INIT &ENTER &LEAVE &KEEP &UNDO &PRE &POST &CATCH &CONTROL> -> $phaser {
            &inner-evaluator($phaser);
        }

        self.walk-scopes(%seen, &inner-evaluator);

        self.levenshtein_candidate_heuristic(@candidates, @suggestions);
        if !$with_sigil {
            my @no_sigils;  # can't do in-place $_ alteration
            for @suggestions {
                nqp::push( @no_sigils, nqp::substr($_,1,nqp::chars($_) - 1) );
            }
            @suggestions := @no_sigils;
        }
        if $name eq '&length' {
            @suggestions.push: $with_sigil ?? '&elems'  !! 'elems';
            @suggestions.push: $with_sigil ?? '&chars'  !! 'chars';
            @suggestions.push: $with_sigil ?? '&codes'  !! 'codes';
        }
        elsif $name eq '&bytes' {
            @suggestions.push: '.encode($encoding).bytes';
        }
        elsif $name eq '&break' {
            @suggestions.push: 'last';
        }
        elsif $name eq '&skip' {
            @suggestions.push: 'next';
        }
        elsif $name eq '&continue' {
            @suggestions.push: 'NEXT';
            @suggestions.push: 'proceed';
            @suggestions.push: 'succeed';
        }
        return @suggestions;
    }

    method suggest-typename(Str $name) {
        # Set up lookup for newbie type errors in typenames
        my $newbies := nqp::hash(
          'Integer',   ('Int',),
          'integer',   ('Int','int'),
          'Float',     ('Num',),
          'float',     ('Num','num'),
          'Number',    ('Num',),
          'number',    ('Num','num'),
          'String',    ('Str',),
          'string',    ('Str','str'),
        );

        my %seen;
        %seen{$name} := 1;
        my @candidates := [[], [], []];
        my &inner-evaluator := self.make_levenshtein_evaluator($name, @candidates);
        my @suggestions;

        if (my @alternates := nqp::atkey($newbies, $name)) {
            for @alternates {
                nqp::push(@suggestions, $_);
            }
        }

        my &evaluator := -> $name {
            # only care about type objects
            my $first := nqp::substr($name, 0, 1);
            unless $first eq '$' || $first eq '%' || $first eq '@' || $first eq '&' || $first eq ':' {
                #unless !$has_object || (nqp::isconcrete($object) && !nqp::istype($object.HOW, Perl6::Metamodel::EnumHOW)) {
                &inner-evaluator($name);
            }
        }
        self.walk-scopes(%seen, &evaluator);

        self.levenshtein_candidate_heuristic(@candidates, @suggestions);

        return @suggestions;
    }
}

#-------------------------------------------------------------------------------
# Information about a lexical scope that we are currently compiling.
class RakuAST::Resolver::Compile::Scope
  is RakuAST::Resolver
{
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

    method lexical-declarations() {
        my $declarations := $!scope.lexical-declarations;
        my @declarations := $!scope.IMPL-UNWRAP-LIST($declarations);
        unless $!batch-mode {
            for $!live-decl-map {
                nqp::push(@declarations, $_.value);
            }
        }
        @declarations
    }

    method declare-lexical(RakuAST::Declaration $decl) {
        nqp::die('Should not be calling declare-lexical in batch mode')
          if $!batch-mode;
        my $name    := $decl.lexical-name;
        my $existed := nqp::atkey($!live-decl-map, $name);
        $!live-decl-map{$decl.lexical-name} := $decl;
        $existed
    }

    method create-implicits() {
        nqp::die('Should not be calling create-implicits in batch mode')
          if $!batch-mode;
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
