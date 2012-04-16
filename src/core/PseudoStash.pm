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
        my $ctx := pir::getattribute__PPs(
            nqp::atkey(pir::getinterp__P(), 'context'),
            'caller_ctx');
        nqp::bindattr($obj, PseudoStash, '$!ctx', $ctx);
        nqp::bindattr($obj, EnumMap, '$!storage',
            pir::getattribute__PPs($ctx, 'lex_pad'));
        $obj
    }
    
    my %pseudoers =
        'MY' => sub ($cur) {
            my $stash := pir::repr_clone__PP($cur);
            nqp::bindattr_i($stash, PseudoStash, '$!mode', PRECISE_SCOPE);
            pir::set_who__0PP(
                Metamodel::ModuleHOW.new(:name('MY')),
                $stash);
        },
        'CORE' => sub ($cur) {
            my Mu $ctx := nqp::getattr(nqp::p6decont($cur), PseudoStash, '$!ctx');
            until nqp::existskey(pir::getattribute__PPs($ctx, 'lex_pad'), '!CORE_MARKER') {
                $ctx := pir::getattribute__PPs($ctx, 'outer_ctx');
            }
            my $stash := nqp::create(PseudoStash);
            nqp::bindattr($stash, EnumMap, '$!storage',
                pir::getattribute__PPs($ctx, 'lex_pad'));
            nqp::bindattr($stash, PseudoStash, '$!ctx', $ctx);
            nqp::bindattr_i($stash, PseudoStash, '$!mode', PRECISE_SCOPE);
            pir::set_who__0PP(
                Metamodel::ModuleHOW.new(:name('CORE')),
                $stash);
        },
        'CALLER' => sub ($cur) {
            my Mu $ctx := pir::getattribute__PPs(
                nqp::getattr(nqp::p6decont($cur), PseudoStash, '$!ctx'),
                'caller_ctx');
            my $stash := nqp::create(PseudoStash);
            nqp::bindattr($stash, EnumMap, '$!storage',
                pir::getattribute__PPs($ctx, 'lex_pad'));
            nqp::bindattr($stash, PseudoStash, '$!ctx', $ctx);
            nqp::bindattr_i($stash, PseudoStash, '$!mode', PRECISE_SCOPE);
            pir::set_who__0PP(
                Metamodel::ModuleHOW.new(:name('CALLER')),
                $stash);
        },
        'OUTER' => sub ($cur) {
            my Mu $ctx := pir::getattribute__PPs(
                nqp::getattr(nqp::p6decont($cur), PseudoStash, '$!ctx'),
                'outer_ctx');
            my $stash := nqp::create(PseudoStash);
            nqp::bindattr($stash, EnumMap, '$!storage',
                pir::getattribute__PPs($ctx, 'lex_pad'));
            nqp::bindattr($stash, PseudoStash, '$!ctx', $ctx);
            nqp::bindattr_i($stash, PseudoStash, '$!mode', PRECISE_SCOPE);
            pir::set_who__0PP(
                Metamodel::ModuleHOW.new(:name('OUTER')),
                $stash);
        },
        'DYNAMIC' => sub ($cur) {
            my $stash := pir::repr_clone__PP($cur);
            nqp::bindattr_i($stash, PseudoStash, '$!mode', DYNAMIC_CHAIN);
            pir::set_who__0PP(
                Metamodel::ModuleHOW.new(:name('DYNAMIC')),
                $stash);
        },
        'UNIT' => sub ($cur) {
            my Mu $ctx := nqp::getattr(nqp::p6decont($cur), PseudoStash, '$!ctx');
            until nqp::existskey(pir::getattribute__PPs($ctx, 'lex_pad'), '!UNIT_MARKER') {
                $ctx := pir::getattribute__PPs($ctx, 'outer_ctx');
            }
            my $stash := nqp::create(PseudoStash);
            nqp::bindattr($stash, EnumMap, '$!storage',
                pir::getattribute__PPs($ctx, 'lex_pad'));
            nqp::bindattr($stash, PseudoStash, '$!ctx', $ctx);
            nqp::bindattr_i($stash, PseudoStash, '$!mode', PRECISE_SCOPE);
            pir::set_who__0PP(
                Metamodel::ModuleHOW.new(:name('UNIT')),
                $stash);
        },
        'SETTING' => sub ($cur) {
            # Same as UNIT, but go a little further out (two steps, for
            # internals reasons).
            my Mu $ctx := nqp::getattr(nqp::p6decont($cur), PseudoStash, '$!ctx');
            until nqp::existskey(pir::getattribute__PPs($ctx, 'lex_pad'), '!UNIT_MARKER') {
                $ctx := pir::getattribute__PPs($ctx, 'outer_ctx');
            }
            $ctx := pir::getattribute__PPs(pir::getattribute__PPs($ctx, 'outer_ctx'), 'outer_ctx');
            my $stash := nqp::create(PseudoStash);
            nqp::bindattr($stash, EnumMap, '$!storage',
                pir::getattribute__PPs($ctx, 'lex_pad'));
            nqp::bindattr($stash, PseudoStash, '$!ctx', $ctx);
            nqp::bindattr_i($stash, PseudoStash, '$!mode', PRECISE_SCOPE);
            pir::set_who__0PP(
                Metamodel::ModuleHOW.new(:name('UNIT')),
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
    
    method bind_key($key is copy, \$value) {
        $key = $key.Str;
        if %pseudoers.exists($key) {
            die "Cannot bind to pseudo-package $key";
        }
        elsif $!mode == PRECISE_SCOPE {
            my Mu $store := nqp::getattr(self, EnumMap, '$!storage');
            nqp::bindkey($store, nqp::unbox_s($key), $value)
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
            nqp::existskey(
                nqp::getattr(self, EnumMap, '$!storage'),
                nqp::unbox_s($key))
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
