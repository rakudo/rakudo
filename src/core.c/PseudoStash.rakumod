my class X::Bind { ... }
my class X::Caller::NotDynamic { ... }

my class PseudoStash is Map {
    has Mu $!ctx;
    has int $!mode;

    # Lookup modes.
    my int constant PICK_CHAIN_BY_NAME  = 0;
    my int constant STATIC_CHAIN        = 1;
    my int constant DYNAMIC_CHAIN       = 2;
    my int constant PRECISE_SCOPE       = 4;
    my int constant REQUIRE_DYNAMIC     = 8;
    my int constant BEGIN_TIME_FALLBACK = 16;

    # Code the compiler runs at BEGIN time lives in a frame whose outer is
    # the setting, so a static-chain walk from it reaches setting symbols
    # but not what the unit being compiled declares. While a begin-time
    # effect runs the compiler leaves its resolver and that compilation's
    # token in $*BEGIN-TIME-LOOKUP, and the frames it compiles dynamically
    # for the unit see the token, so only the unit's own code is answered
    # for. Consulted by stashes carrying BEGIN_TIME_FALLBACK and by
    # INDIRECT_NAME_LOOKUP, in both cases once every other source of the
    # name has missed.
    method BEGIN-TIME-DECLARATION(str $key) is implementation-detail {
        my $state := nqp::getlexdyn('$*BEGIN-TIME-LOOKUP');
        nqp::isnull($state)
          ?? Nil
          !! nqp::not_i(nqp::isnull(
               my $marker := nqp::getlexrel(
                 nqp::getattr(self, PseudoStash, '$!ctx'),
                 '!BEGIN_TIME_MARKER')))
               && nqp::eqaddr($marker, nqp::atpos($state, 1))
            ?? (nqp::atpos($state, 0).resolve-lexical-constant-in-scopes($key) || Nil)
            !! Nil
    }

    method new() {
        my $obj := nqp::create(self);
        my $ctx := nqp::ctxcaller(nqp::ctx());
        nqp::bindattr($obj, PseudoStash, '$!ctx', $ctx);
        nqp::bindattr($obj, Map, '$!storage', nqp::ctxlexpad($ctx));
        $obj
    }

    method new-from-ctx(Mu $ctx is raw, :$mode = STATIC_CHAIN) {
        my $obj := nqp::create(self);
        my Mu $dctx := nqp::decont($ctx);
        nqp::bindattr($obj, PseudoStash, '$!ctx', $dctx);
        nqp::bindattr($obj, Map, '$!storage', nqp::ctxlexpad($ctx));
        nqp::bindattr_i($obj, PseudoStash, '$!mode', nqp::decont($mode));
        $obj
    }

    sub ok-to-include(Mu \value) {
        nqp::not_i(nqp::istype(value,Code) && value.is-implementation-detail)
    }

    method keys(:$implementation-detail --> Seq:D) {
        $implementation-detail
          ?? (nextsame)
          !! Seq.new(self.iterator).map: { .key if ok-to-include(.value) }
    }

    method values(:$implementation-detail --> Seq:D) {
        $implementation-detail
          ?? (nextsame)
          !! callsame.grep: &ok-to-include
    }

    method kv(:$implementation-detail --> Seq:D) {
        $implementation-detail
          ?? (nextsame)
          !! Seq.new(self.iterator).map: {
                 (.key,.value).Slip if ok-to-include(.value)
             }
    }

    method pairs(:$implementation-detail --> Seq:D) {
        $implementation-detail
          ?? (nextsame)
          !! Seq.new(self.iterator).map: { $_ if ok-to-include(.value) }
    }

    method sort(:$implementation-detail --> Seq:D) {
        $implementation-detail
          ?? (nextsame)
          !! self.pairs.sort
    }

    multi method elems(PseudoStash:D: :$implementation-detail) {
        $implementation-detail
          ?? (nextsame)
          !! self.values.elems
    }

    multi method WHICH(PseudoStash:D: --> ObjAt:D) { self.Mu::WHICH }

    my $pseudoers := nqp::hash(
        'MY', -> $cur {
            my $stash := nqp::clone($cur);
            nqp::bindattr_i($stash, PseudoStash, '$!mode', PRECISE_SCOPE);
            nqp::setwho(
                Metamodel::ModuleHOW.new_type(:name('MY')),
                $stash);
        },
        'CORE', -> $cur {
            # In 6.c and 6.d implementations of rakudo CORE was always pointing at the outermost setting.
            # XXX If EVAL get :unit option we'd need to check for intermidiate CORE.setting. But for now this code
            # should be ok.
            my Mu $ctx := nqp::getattr(nqp::decont($cur), PseudoStash, '$!ctx');
            my $found-ctx := nqp::null();
            until nqp::isnull($ctx) {
                my $pad := nqp::ctxlexpad($ctx);
                if nqp::existskey($pad, 'CORE-SETTING-REV') {
                    $found-ctx := $ctx;
                }
                $ctx := nqp::ctxouterskipthunks($ctx);
            }
            nqp::if(
              nqp::isnull($found-ctx),
              Nil,
              nqp::stmts(
                (my $stash := nqp::create(PseudoStash)),
                nqp::bindattr($stash, Map, '$!storage', nqp::ctxlexpad($found-ctx)),
                nqp::bindattr($stash, PseudoStash, '$!ctx', $found-ctx),
                nqp::bindattr_i($stash, PseudoStash, '$!mode', PRECISE_SCOPE),
                nqp::setwho(
                  Metamodel::ModuleHOW.new_type(:name('CORE')),
                    $stash)))
        },
        'CALLER', -> $cur {
            nqp::if(
              nqp::isnull(
                my Mu $ctx := nqp::ctxcallerskipthunks(
                  nqp::getattr(nqp::decont($cur), PseudoStash, '$!ctx'))),
              Nil,
              nqp::stmts(
                (my $stash := nqp::create(PseudoStash)),
                nqp::bindattr($stash, Map, '$!storage', nqp::ctxlexpad($ctx)),
                nqp::bindattr($stash, PseudoStash, '$!ctx', $ctx),
                nqp::bindattr_i($stash, PseudoStash, '$!mode', PRECISE_SCOPE +| REQUIRE_DYNAMIC),
                nqp::setwho(
                    Metamodel::ModuleHOW.new_type(:name('CALLER')),
                    $stash)))
        },
        'OUTER', -> $cur {
            my Mu $ctx := nqp::ctxouterskipthunks(
              nqp::getattr(nqp::decont($cur),PseudoStash,'$!ctx'));

            if nqp::isnull($ctx) {
                Nil
            }
            else {
                my $stash := nqp::create(PseudoStash);
                nqp::bindattr($stash, Map, '$!storage', nqp::ctxlexpad($ctx));
                nqp::bindattr($stash, PseudoStash, '$!ctx', $ctx);
                nqp::bindattr_i($stash, PseudoStash, '$!mode', PRECISE_SCOPE);
                nqp::setwho(
                    Metamodel::ModuleHOW.new_type(:name('OUTER')),
                    $stash)
            }
        },
        'LEXICAL', -> $cur {
            my $stash := nqp::clone($cur);
            nqp::bindattr_i($stash, PseudoStash, '$!mode',
              STATIC_CHAIN +| BEGIN_TIME_FALLBACK);
            nqp::setwho(
                Metamodel::ModuleHOW.new_type(:name('LEXICAL')),
                $stash);
        },
        'OUTERS', -> $cur {
            my Mu $ctx := nqp::ctxouterskipthunks(
                nqp::getattr(nqp::decont($cur), PseudoStash, '$!ctx'));

            if nqp::isnull($ctx) {
                Nil
            }
            else {
                my $stash := nqp::create(PseudoStash);
                nqp::bindattr($stash, Map, '$!storage', nqp::ctxlexpad($ctx));
                nqp::bindattr($stash, PseudoStash, '$!ctx', $ctx);
                nqp::bindattr_i($stash, PseudoStash, '$!mode',
                  STATIC_CHAIN +| BEGIN_TIME_FALLBACK);
                nqp::setwho(
                    Metamodel::ModuleHOW.new_type(:name('OUTERS')),
                    $stash)
            }
        },
        'DYNAMIC', -> $cur {
            my $stash := nqp::clone($cur);
            # A name without the * twigil walks the static chain here, so
            # it has the same begin-time gap a LEXICAL:: lookup has.
            nqp::bindattr_i($stash, PseudoStash, '$!mode',
              DYNAMIC_CHAIN +| BEGIN_TIME_FALLBACK);
            nqp::setwho(
                Metamodel::ModuleHOW.new_type(:name('DYNAMIC')),
                $stash);
        },
        'CALLERS', -> $cur {
            nqp::if(
              nqp::isnull(
                my Mu $ctx := nqp::ctxcallerskipthunks(
                  nqp::getattr(nqp::decont($cur), PseudoStash, '$!ctx'))),
              Nil,
              nqp::stmts(
                (my $stash := nqp::create(PseudoStash)),
                nqp::bindattr($stash, Map, '$!storage', nqp::ctxlexpad($ctx)),
                nqp::bindattr($stash, PseudoStash, '$!ctx', $ctx),
                nqp::bindattr_i($stash, PseudoStash, '$!mode', DYNAMIC_CHAIN +| REQUIRE_DYNAMIC),
                nqp::setwho(
                  Metamodel::ModuleHOW.new_type(:name('CALLERS')),
                  $stash)))
        },
        'UNIT', -> $cur {
            my Mu $ctx := nqp::getattr(nqp::decont($cur), PseudoStash, '$!ctx');
            until nqp::isnull($ctx) || nqp::existskey(nqp::ctxlexpad($ctx), '!UNIT_MARKER') {
                $ctx := nqp::ctxouterskipthunks($ctx);
            }
            nqp::if(
              nqp::isnull($ctx),
              Nil,
              nqp::stmts(
                (my $stash := nqp::create(PseudoStash)),
                nqp::bindattr($stash, Map, '$!storage',nqp::ctxlexpad($ctx)),
                nqp::bindattr($stash, PseudoStash, '$!ctx', $ctx),
                nqp::bindattr_i($stash, PseudoStash, '$!mode', PRECISE_SCOPE),
                nqp::setwho(
                  Metamodel::ModuleHOW.new_type(:name('UNIT')),
                  $stash)))
        },
        'SETTING', -> $cur {
            # Same as UNIT, but go a little further out (two steps, for
            # internals reasons).
            my Mu $ctx := nqp::getattr(nqp::decont($cur), PseudoStash, '$!ctx');
            until nqp::isnull($ctx) || nqp::existskey(nqp::ctxlexpad($ctx), '!UNIT_MARKER') {
                $ctx := nqp::ctxouterskipthunks($ctx);
            }
            my $is-rakuast := nqp::isconcrete($ctx) && nqp::existskey(nqp::ctxlexpad($ctx), '!RAKUAST_MARKER');
            nqp::if(
              nqp::isnull($ctx)
                  || nqp::isnull($ctx := nqp::ctxouter($ctx))
                  || nqp::isnull(nqp::if($is-rakuast, $ctx, ($ctx := nqp::ctxouter($ctx)))),
              Nil,
              nqp::stmts(
                (my $stash := nqp::create(PseudoStash)),
                nqp::bindattr($stash, Map, '$!storage', nqp::ctxlexpad($ctx)),
                nqp::bindattr($stash, PseudoStash, '$!ctx', $ctx),
                nqp::bindattr_i($stash, PseudoStash, '$!mode', STATIC_CHAIN),
                nqp::setwho(
                  Metamodel::ModuleHOW.new_type(:name('SETTING')),
                  $stash)))
        },
        'CLIENT', -> $cur {
            my $pkg := nqp::getlexrel(
                nqp::getattr(nqp::decont($cur), PseudoStash, '$!ctx'),
                '$?PACKAGE');
            my Mu $ctx := nqp::ctxcallerskipthunks(
                nqp::getattr(nqp::decont($cur), PseudoStash, '$!ctx'));
            while nqp::eqaddr(nqp::getlexrel($ctx, '$?PACKAGE'), $pkg) {
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
        'OUR', -> $cur {
            nqp::getlexrel(
                nqp::getattr(nqp::decont($cur), PseudoStash, '$!ctx'),
                '$?PACKAGE')
        }
    );

    multi method AT-KEY(PseudoStash:D: Str() $key) is raw {
        nqp::if(
          nqp::existskey($pseudoers,$key),
          nqp::atkey($pseudoers,$key)(self),
          nqp::stmts(
            (my $is-star := nqp::iseq_i(nqp::ord(nqp::unbox_s($key),1),42)),  # has * twigil
            nqp::if(
              nqp::bitand_i($!mode,PRECISE_SCOPE),
              nqp::stmts(
                (my Mu $res := nqp::if(
                  nqp::existskey(
                    nqp::getattr(self,Map,'$!storage'),nqp::unbox_s($key)),
                  nqp::atkey(
                    nqp::getattr(self,Map,'$!storage'),nqp::unbox_s($key)),
                  Nil )),
                nqp::if(
                  (nqp::not_i(nqp::eqaddr($res,Nil))
                    && nqp::bitand_i($!mode,REQUIRE_DYNAMIC)),
                  nqp::unless(
                    ($is-star || try $res.VAR.dynamic),
                    X::Caller::NotDynamic.new(symbol => $key).throw)),
                $res ),
              nqp::if(
                nqp::bitand_i($!mode,nqp::bitor_i(DYNAMIC_CHAIN,PICK_CHAIN_BY_NAME)) && $is-star,
                nqp::ifnull(
                  nqp::getlexreldyn(
                    nqp::getattr(self,PseudoStash,'$!ctx'),nqp::unbox_s($key)),
                  Nil ),
                nqp::ifnull(                                    # STATIC_CHAIN
                  nqp::getlexrel(
                    nqp::getattr(self,PseudoStash,'$!ctx'),nqp::unbox_s($key)),
                  nqp::if(
                    nqp::bitand_i($!mode,BEGIN_TIME_FALLBACK)
                      && nqp::isconcrete(my $declaration :=
                           self.BEGIN-TIME-DECLARATION(nqp::unbox_s($key))),
                    $declaration.compile-time-value,
                    Nil ))))))
    }
    multi method ASSIGN-KEY(PseudoStash:D: Str() $key, Mu \value) is raw {
        self.AT-KEY($key) = value
    }

    method BIND-KEY(Str() $key, \value) is raw {
        nqp::if(
          nqp::existskey($pseudoers,$key),
          X::Bind.new(target => "pseudo-package $key").throw,
          nqp::if(
            nqp::bitand_i($!mode,PRECISE_SCOPE),
            nqp::bindkey(
              nqp::getattr(self,Map,'$!storage'),nqp::unbox_s($key),value),
            nqp::if(
              (nqp::bitand_i($!mode,nqp::bitor_i(DYNAMIC_CHAIN,PICK_CHAIN_BY_NAME))
                && nqp::iseq_i(nqp::ord(nqp::unbox_s($key),1),42)),  # "*"
              (die "Binding to dynamic variables not yet implemented"),
              (die "This case of binding is not yet implemented"))))
    }

    # for some reason we get an ambiguous dispatch error by making this a multi
    method EXISTS-KEY(PseudoStash:D: Str() $key) {
        nqp::unless(
          nqp::existskey($pseudoers,$key),
          nqp::hllbool(
            nqp::if(
              nqp::bitand_i($!mode,PRECISE_SCOPE),
              nqp::existskey(
                nqp::getattr(self,Map,'$!storage'),nqp::unbox_s($key)),
              nqp::if(
                nqp::bitand_i(
                  $!mode,nqp::bitor_i(DYNAMIC_CHAIN,PICK_CHAIN_BY_NAME)
                ) && nqp::iseq_i(nqp::ord(nqp::unbox_s($key),1),42),  # "*"
                nqp::not_i(
                  nqp::isnull(
                    nqp::getlexreldyn(
                      nqp::getattr(self, PseudoStash, '$!ctx'),
                      nqp::unbox_s($key)))),
                nqp::if(              # STATIC_CHAIN
                  nqp::isnull(
                    nqp::getlexrel(
                      nqp::getattr(self, PseudoStash, '$!ctx'),
                      nqp::unbox_s($key))),
                  nqp::bitand_i($!mode,BEGIN_TIME_FALLBACK)
                    && nqp::isconcrete(
                         self.BEGIN-TIME-DECLARATION(nqp::unbox_s($key))),
                  1)))))
    }
}

# vim: expandtab shiftwidth=4
