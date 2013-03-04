my class X::Bind { ... }

my class PseudoStash is EnumMap {
    has Mu $!ctx;
    has int $!mode;
    
    # Lookup modes.
    my int constant PICK_CHAIN_BY_NAME = 0;
    my int constant STATIC_CHAIN       = 1;
    my int constant DYNAMIC_CHAIN      = 2;
    my int constant PRECISE_SCOPE      = 3;

    method new() {
        my $obj := nqp::create(self);
        my $ctx := nqp::ctxcaller(nqp::ctx());
        nqp::bindattr($obj, PseudoStash, '$!ctx', $ctx);
        nqp::bindattr($obj, EnumMap, '$!storage', nqp::ctxlexpad($ctx));
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
            my Mu $ctx := nqp::getattr(nqp::p6decont($cur), PseudoStash, '$!ctx');
            until nqp::existskey(nqp::ctxlexpad($ctx), '!CORE_MARKER') {
                $ctx := nqp::ctxouter($ctx);
            }
            my $stash := nqp::create(PseudoStash);
            nqp::bindattr($stash, EnumMap, '$!storage', nqp::ctxlexpad($ctx));
            nqp::bindattr($stash, PseudoStash, '$!ctx', $ctx);
            nqp::bindattr_i($stash, PseudoStash, '$!mode', PRECISE_SCOPE);
            nqp::setwho(
                Metamodel::ModuleHOW.new_type(:name('CORE')),
                $stash);
        },
        'CALLER' => sub ($cur) {
            my Mu $ctx := nqp::ctxcaller(
                nqp::getattr(nqp::p6decont($cur), PseudoStash, '$!ctx'));
            my $stash := nqp::create(PseudoStash);
            nqp::bindattr($stash, EnumMap, '$!storage', nqp::ctxlexpad($ctx));
            nqp::bindattr($stash, PseudoStash, '$!ctx', $ctx);
            nqp::bindattr_i($stash, PseudoStash, '$!mode', PRECISE_SCOPE);
            nqp::setwho(
                Metamodel::ModuleHOW.new_type(:name('CALLER')),
                $stash);
        },
        'OUTER' => sub ($cur) {
            my Mu $ctx := nqp::ctxouter(
                nqp::getattr(nqp::p6decont($cur), PseudoStash, '$!ctx'));
            my $stash := nqp::create(PseudoStash);
            nqp::bindattr($stash, EnumMap, '$!storage', nqp::ctxlexpad($ctx));
            nqp::bindattr($stash, PseudoStash, '$!ctx', $ctx);
            nqp::bindattr_i($stash, PseudoStash, '$!mode', PRECISE_SCOPE);
            nqp::setwho(
                Metamodel::ModuleHOW.new_type(:name('OUTER')),
                $stash);
        },
        'DYNAMIC' => sub ($cur) {
            my $stash := nqp::clone($cur);
            nqp::bindattr_i($stash, PseudoStash, '$!mode', DYNAMIC_CHAIN);
            nqp::setwho(
                Metamodel::ModuleHOW.new_type(:name('DYNAMIC')),
                $stash);
        },
        'UNIT' => sub ($cur) {
            my Mu $ctx := nqp::getattr(nqp::p6decont($cur), PseudoStash, '$!ctx');
            until nqp::existskey(nqp::ctxlexpad($ctx), '!UNIT_MARKER') {
                $ctx := nqp::ctxouter($ctx);
            }
            my $stash := nqp::create(PseudoStash);
            nqp::bindattr($stash, EnumMap, '$!storage',nqp::ctxlexpad($ctx));
            nqp::bindattr($stash, PseudoStash, '$!ctx', $ctx);
            nqp::bindattr_i($stash, PseudoStash, '$!mode', PRECISE_SCOPE);
            nqp::setwho(
                Metamodel::ModuleHOW.new_type(:name('UNIT')),
                $stash);
        },
        'SETTING' => sub ($cur) {
            # Same as UNIT, but go a little further out (two steps, for
            # internals reasons).
            my Mu $ctx := nqp::getattr(nqp::p6decont($cur), PseudoStash, '$!ctx');
            until nqp::existskey(nqp::ctxlexpad($ctx), '!UNIT_MARKER') {
                $ctx := nqp::ctxouter($ctx);
            }
            $ctx := nqp::ctxouter(nqp::ctxouter($ctx));
            my $stash := nqp::create(PseudoStash);
            nqp::bindattr($stash, EnumMap, '$!storage', nqp::ctxlexpad($ctx));
            nqp::bindattr($stash, PseudoStash, '$!ctx', $ctx);
            nqp::bindattr_i($stash, PseudoStash, '$!mode', PRECISE_SCOPE);
            nqp::setwho(
                Metamodel::ModuleHOW.new_type(:name('UNIT')),
                $stash);
        },
        'OUR' => sub ($cur) {
            pir::find_lex_relative__PPs(
                nqp::getattr(nqp::p6decont($cur), PseudoStash, '$!ctx'),
                '$?PACKAGE')
        };
    
    method at_key($key is copy) is rw {
        $key = $key.Str;
        if %pseudoers.exists($key) {
            %pseudoers{$key}(self)
        }
        elsif $!mode == PRECISE_SCOPE {
            my Mu $store := nqp::getattr(self, EnumMap, '$!storage');
            nqp::existskey($store, nqp::unbox_s($key)) ??
                nqp::atkey($store, nqp::unbox_s($key)) !!
                Any
        }
        elsif $!mode == DYNAMIC_CHAIN || $!mode == PICK_CHAIN_BY_NAME && substr($key, 1, 1) eq '*' {
            my $found := pir::find_dynamic_lex_relative__PPs(
                nqp::getattr(self, PseudoStash, '$!ctx'),
                nqp::unbox_s($key));
            nqp::isnull($found) ?? Any !! $found
        }
        else {
            my $found := pir::find_lex_relative__PPs(
                nqp::getattr(self, PseudoStash, '$!ctx'),
                nqp::unbox_s($key));
            nqp::isnull($found) ?? Any !! $found
        }
    }
    
    method bind_key($key is copy, \value) {
        $key = $key.Str;
        if %pseudoers.exists($key) {
            X::Bind.new(target => "pseudo-package $key").throw;
        }
        elsif $!mode == PRECISE_SCOPE {
            my Mu $store := nqp::getattr(self, EnumMap, '$!storage');
            nqp::bindkey($store, nqp::unbox_s($key), value)
        }
        elsif $!mode == DYNAMIC_CHAIN || $!mode == PICK_CHAIN_BY_NAME && substr($key, 1, 1) eq '*' {
            die "Binding to dynamic variables not yet implemented";
        }
        else {
            die "This case of binding is not yet implemented";
        }
    }
    
    method exists($key is copy) {
        $key = $key.Str;
        if %pseudoers.exists($key) {
            True
        }
        elsif $!mode == PRECISE_SCOPE {
            nqp::p6bool(nqp::existskey(
                nqp::getattr(self, EnumMap, '$!storage'),
                nqp::unbox_s($key)))
        }
        elsif $!mode == DYNAMIC_CHAIN || $!mode == PICK_CHAIN_BY_NAME && substr($key, 1, 1) eq '*' {
            nqp::isnull(
                pir::find_dynamic_lex_relative__PPs(
                    nqp::getattr(self, PseudoStash, '$!ctx'),
                    nqp::unbox_s($key)))
                ?? False !! True
        }
        else {
            nqp::isnull(
                pir::find_lex_relative__PPs(
                    nqp::getattr(self, PseudoStash, '$!ctx'),
                    nqp::unbox_s($key)))
                ?? False !! True
        }
    }
}
