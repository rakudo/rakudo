class RakuAST::Resolver {
    has Mu $!scopes;

    # Pushes an active lexical scope to be considered in lookup.
    method push-scope(RakuAST::LexicalScope $scope) {
        $!scopes.push($scope);
    }

    # Pops the top active lexical scope.
    method pop-scope() {
        $!scopes.pop
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
    has Mu $!global;
    has Mu $!context; # XXX Should be PseudoStash

    method new(Mu :$global!, Mu :$context!) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Resolver::EVAL, '$!global', $global);
        nqp::bindattr($obj, RakuAST::Resolver::EVAL, '$!context', $context);
        nqp::bindattr($obj, RakuAST::Resolver, '$!scopes', []);
        $obj
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

        # Look through the contexts for the name.
        my $ctx := $!context;
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

    # Resolves a lexical to its declaration. The declaration must have a
    # compile-time value.
    method resolve-lexical-constant(Str $name) {
        # TODO walk scopes we've encountered while walking

        # Look through the contexts for the name.
        my $ctx := $!context;
        while !nqp::isnull($ctx) {
            if nqp::existskey($ctx, $name) {
                my $compile-time-value := nqp::atkey($ctx, $name);
                return RakuAST::Declaration::External::Constant.new(:lexical-name($name),
                    :$compile-time-value);
            }
            $ctx := nqp::ctxouter($ctx);
        }

        # Nothing found.
        return Nil;
    }
}

# The compiler resolver is used in the situation we are parsing code and
# building up a RakuAST as we go. We thus need to provide symbol resolutions
# for the sake of parse disambiguation, as well as to handle BEGIN-time code.
# A resolver may be created using an existing context, or it may be for a
# compilation unit whose outer scope is some version of the setting.
class RakuAST::Resolver::Compile is RakuAST::Resolver {
    # The setting.
    has Mu $!setting;

    # Our outer context. When not an EVAL, this is the same as $!setting.
    has Mu $!outer;

    method new(Mu :$setting!, Mu :$outer!) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Resolver, '$!scopes', []);
        nqp::bindattr($obj, RakuAST::Resolver::Compile, '$!setting', $setting);
        nqp::bindattr($obj, RakuAST::Resolver::Compile, '$!outer', $outer);
        $obj
    }

    # Create a resolver from a context and existing global. Used when we are
    # compiling a textual EVAL.
    method from-context(Mu :$context!, Mu :$global!) {
        nqp::die('from-context NYI');
    }

    # Create a resolver a fresh compilation unit of the specified language
    # version.
    method from-setting(Str :$name, Str :$version) {
        $name := 'CORE' unless $name;
        $version := nqp::substr(nqp::getcomp('Raku').config<language-version>, 2)
            unless $version;
        my $loader := nqp::gethllsym('Raku', 'ModuleLoader');
        my $to_load := $loader.transform_setting_name("$name.$version");
        my $setting := $loader.load_setting($to_load);
        self.new(:$setting, :outer($setting))
    }
}
