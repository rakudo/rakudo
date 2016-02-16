my class X::Bind { ... }
my class X::Caller::NotDynamic { ... }

my class PseudoStash is Map {
    has Mu $!ctx;
    has int $!mode;

    # Lookup modes.
    my int constant PICK_CHAIN_BY_NAME = 0;
    my int constant STATIC_CHAIN       = 1;
    my int constant DYNAMIC_CHAIN      = 2;
    my int constant PRECISE_SCOPE      = 4;
    my int constant REQUIRE_DYNAMIC    = 8;

    method new() {
        my $obj := nqp::create(self);
        my $ctx := nqp::ctxcaller(nqp::ctx());
        nqp::bindattr($obj, PseudoStash, '$!ctx', $ctx);
        nqp::bindattr($obj, Map, '$!storage', nqp::ctxlexpad($ctx));
        $obj
    }

    my %pseudoers =
        'MY' => sub ($cur) {
            my $stash := nqp::clone($cur);
            nqp::bindattr_i($stash, PseudoStash, '$!mode', PRECISE_SCOPE);
            nqp::setwho(
                Metamodel::ModuleHOW.new_type(:name('MY')),
                $stash);
        },
        'CORE' => sub ($cur) {
            my Mu $ctx := nqp::getattr(nqp::decont($cur), PseudoStash, '$!ctx');
            until nqp::existskey(nqp::ctxlexpad($ctx), '!CORE_MARKER') {
                $ctx := nqp::ctxouterskipthunks($ctx);
            }
            my $stash := nqp::create(PseudoStash);
            nqp::bindattr($stash, Map, '$!storage', nqp::ctxlexpad($ctx));
            nqp::bindattr($stash, PseudoStash, '$!ctx', $ctx);
            nqp::bindattr_i($stash, PseudoStash, '$!mode', PRECISE_SCOPE);
            nqp::setwho(
                Metamodel::ModuleHOW.new_type(:name('CORE')),
                $stash);
        },
        'CALLER' => sub ($cur) {
            my Mu $ctx := nqp::ctxcallerskipthunks(
                nqp::getattr(nqp::decont($cur), PseudoStash, '$!ctx'));
            my $stash := nqp::create(PseudoStash);
            nqp::bindattr($stash, Map, '$!storage', nqp::ctxlexpad($ctx));
            nqp::bindattr($stash, PseudoStash, '$!ctx', $ctx);
            nqp::bindattr_i($stash, PseudoStash, '$!mode', PRECISE_SCOPE +| REQUIRE_DYNAMIC);
            nqp::setwho(
                Metamodel::ModuleHOW.new_type(:name('CALLER')),
                $stash);
        },
        'OUTER' => sub ($cur) {
            my Mu $ctx := nqp::ctxouterskipthunks(
                nqp::getattr(nqp::decont($cur), PseudoStash, '$!ctx'));
            my $stash := nqp::create(PseudoStash);
            nqp::bindattr($stash, Map, '$!storage', nqp::ctxlexpad($ctx));
            nqp::bindattr($stash, PseudoStash, '$!ctx', $ctx);
            nqp::bindattr_i($stash, PseudoStash, '$!mode', PRECISE_SCOPE);
            nqp::setwho(
                Metamodel::ModuleHOW.new_type(:name('OUTER')),
                $stash);
        },
        'LEXICAL' => sub ($cur) {
            my $stash := nqp::clone($cur);
            nqp::bindattr_i($stash, PseudoStash, '$!mode', STATIC_CHAIN);
            nqp::setwho(
                Metamodel::ModuleHOW.new_type(:name('LEXICAL')),
                $stash);
        },
        'OUTERS' => sub ($cur) {
            my Mu $ctx := nqp::ctxouterskipthunks(
                nqp::getattr(nqp::decont($cur), PseudoStash, '$!ctx'));
            my $stash := nqp::create(PseudoStash);
            nqp::bindattr($stash, Map, '$!storage', nqp::ctxlexpad($ctx));
            nqp::bindattr($stash, PseudoStash, '$!ctx', $ctx);
            nqp::bindattr_i($stash, PseudoStash, '$!mode', STATIC_CHAIN);
            nqp::setwho(
                Metamodel::ModuleHOW.new_type(:name('OUTERS')),
                $stash);
        },
        'DYNAMIC' => sub ($cur) {
            my $stash := nqp::clone($cur);
            nqp::bindattr_i($stash, PseudoStash, '$!mode', DYNAMIC_CHAIN);
            nqp::setwho(
                Metamodel::ModuleHOW.new_type(:name('DYNAMIC')),
                $stash);
        },
        'CALLERS' => sub ($cur) {
            my Mu $ctx := nqp::ctxcallerskipthunks(
                nqp::getattr(nqp::decont($cur), PseudoStash, '$!ctx'));
            my $stash := nqp::create(PseudoStash);
            nqp::bindattr($stash, Map, '$!storage', nqp::ctxlexpad($ctx));
            nqp::bindattr($stash, PseudoStash, '$!ctx', $ctx);
            nqp::bindattr_i($stash, PseudoStash, '$!mode', DYNAMIC_CHAIN +| REQUIRE_DYNAMIC);
            nqp::setwho(
                Metamodel::ModuleHOW.new_type(:name('CALLERS')),
                $stash);
        },
        'UNIT' => sub ($cur) {
            my Mu $ctx := nqp::getattr(nqp::decont($cur), PseudoStash, '$!ctx');
            until nqp::existskey(nqp::ctxlexpad($ctx), '!UNIT_MARKER') {
                $ctx := nqp::ctxouterskipthunks($ctx);
            }
            my $stash := nqp::create(PseudoStash);
            nqp::bindattr($stash, Map, '$!storage',nqp::ctxlexpad($ctx));
            nqp::bindattr($stash, PseudoStash, '$!ctx', $ctx);
            nqp::bindattr_i($stash, PseudoStash, '$!mode', PRECISE_SCOPE);
            nqp::setwho(
                Metamodel::ModuleHOW.new_type(:name('UNIT')),
                $stash);
        },
        'SETTING' => sub ($cur) {
            # Same as UNIT, but go a little further out (two steps, for
            # internals reasons).
            my Mu $ctx := nqp::getattr(nqp::decont($cur), PseudoStash, '$!ctx');
            until nqp::existskey(nqp::ctxlexpad($ctx), '!UNIT_MARKER') {
                $ctx := nqp::ctxouterskipthunks($ctx);
            }
            $ctx := nqp::ctxouter(nqp::ctxouter($ctx));
            my $stash := nqp::create(PseudoStash);
            nqp::bindattr($stash, Map, '$!storage', nqp::ctxlexpad($ctx));
            nqp::bindattr($stash, PseudoStash, '$!ctx', $ctx);
            nqp::bindattr_i($stash, PseudoStash, '$!mode', STATIC_CHAIN);
            nqp::setwho(
                Metamodel::ModuleHOW.new_type(:name('SETTING')),
                $stash);
        },
        'CLIENT' => sub ($cur) {
            my $pkg := nqp::getlexrel(
                nqp::getattr(nqp::decont($cur), PseudoStash, '$!ctx'),
                '$?PACKAGE');
            die "GLOBAL can have no client package" if $pkg.^name eq "GLOBAL";
            my Mu $ctx := nqp::ctxcallerskipthunks(
                nqp::getattr(nqp::decont($cur), PseudoStash, '$!ctx'));
            while nqp::getlexrel($ctx, '$?PACKAGE') === $pkg {
                $ctx := nqp::ctxcallerskipthunks($ctx);
                die "No client package found" unless $ctx;
            }
            my $stash := nqp::create(PseudoStash);
            nqp::bindattr($stash, Map, '$!storage', nqp::ctxlexpad($ctx));
            nqp::bindattr($stash, PseudoStash, '$!ctx', $ctx);
            nqp::bindattr_i($stash, PseudoStash, '$!mode', PRECISE_SCOPE +| REQUIRE_DYNAMIC);
            nqp::setwho(
                Metamodel::ModuleHOW.new_type(:name('CLIENT')),
                $stash);
        },
        'OUR' => sub ($cur) {
            nqp::getlexrel(
                nqp::getattr(nqp::decont($cur), PseudoStash, '$!ctx'),
                '$?PACKAGE')
        };

    multi method AT-KEY(PseudoStash:D: Str() $key) is raw {
        my Mu $nkey := nqp::unbox_s($key);
        if %pseudoers.EXISTS-KEY($key) {
            %pseudoers.AT-KEY($key)(self)
        }
        elsif nqp::bitand_i($!mode, PRECISE_SCOPE) {
            my Mu $store := nqp::getattr(self, Map, '$!storage');
            my Mu $res := nqp::existskey($store, $nkey) ??
                            nqp::atkey($store, $nkey) !!
                            Nil;
            if !($res =:= Nil) && nqp::bitand_i($!mode, REQUIRE_DYNAMIC) {
                if try !$res.VAR.dynamic {
                    X::Caller::NotDynamic.new(
                        symbol => $key,
                    ).throw;
                }
            }
            $res;
        }
        elsif nqp::bitand_i($!mode, nqp::bitor_i(DYNAMIC_CHAIN, PICK_CHAIN_BY_NAME)) && $key.substr-eq("*",1) {
            my $found := nqp::getlexreldyn(
                nqp::getattr(self, PseudoStash, '$!ctx'),
                $nkey);
            nqp::isnull($found) ?? Nil !! $found
        }
        else { # STATIC_CHAIN
            my $found := nqp::getlexrel(
                nqp::getattr(self, PseudoStash, '$!ctx'),
                $nkey);
            nqp::isnull($found) ?? Nil !! $found
        }
    }

    method BIND-KEY(Str() $key, \value) is raw {
        if %pseudoers.EXISTS-KEY($key) {
            X::Bind.new(target => "pseudo-package $key").throw;
        }
        elsif nqp::bitand_i($!mode, PRECISE_SCOPE) {
            my Mu $store := nqp::getattr(self, Map, '$!storage');
            nqp::bindkey($store, nqp::unbox_s($key), value)
        }
        elsif nqp::bitand_i($!mode, nqp::bitor_i(DYNAMIC_CHAIN, PICK_CHAIN_BY_NAME)) && $key.substr-eq("*",1) {
            die "Binding to dynamic variables not yet implemented";
        }
        else { # STATIC_CHAIN
            die "This case of binding is not yet implemented";
        }
    }
    # for some reason we get a ambiguous dispatch error by making this a multi
    method EXISTS-KEY(PseudoStash:D: Str() $key) {
        if %pseudoers.EXISTS-KEY($key) {
            True
        }
        elsif nqp::bitand_i($!mode, PRECISE_SCOPE) {
            nqp::p6bool(nqp::existskey(
                nqp::getattr(self, Map, '$!storage'),
                nqp::unbox_s($key)))
        }
        elsif nqp::bitand_i($!mode, nqp::bitor_i(DYNAMIC_CHAIN, PICK_CHAIN_BY_NAME)) && $key.substr-eq("*",1) {
            nqp::isnull(
                nqp::getlexreldyn(
                    nqp::getattr(self, PseudoStash, '$!ctx'),
                    nqp::unbox_s($key)))
                ?? False !! True
        }
        else { # STATIC_CHAIN
            nqp::isnull(
                nqp::getlexrel(
                    nqp::getattr(self, PseudoStash, '$!ctx'),
                    nqp::unbox_s($key)))
                ?? False !! True
        }
    }
}

# vim: ft=perl6 expandtab sw=4
