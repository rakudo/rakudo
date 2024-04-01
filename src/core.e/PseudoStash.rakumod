my class PseudoStash is CORE::v6c::PseudoStash {
    has $!package;      # Parent package, for which we serve as .WHO
    has $!walker;

    # Lookup modes. Must be kept in sync with CORE::v6c::PseudoStash mode constants.
    my int constant PICK_CHAIN_BY_NAME = 0;
    my int constant STATIC_CHAIN       = 1;
    my int constant DYNAMIC_CHAIN      = 2;
    my int constant PRECISE_SCOPE      = 4;
    my int constant REQUIRE_DYNAMIC    = 8;

    # A convenience shortcut
    my constant PseudoStash6c = CORE::v6c::PseudoStash;

    multi method new(Mu :$ctx is raw, :$mode = STATIC_CHAIN) {
        my $stash := nqp::create(self);
        my Mu $dctx := nqp::decont($ctx);
        $dctx := nqp::ctxcaller(nqp::ctx()) unless nqp::defined($dctx);
        nqp::bindattr($stash, PseudoStash6c, '$!ctx', nqp::decont($dctx));
        nqp::bindattr_i($stash, PseudoStash6c, '$!mode', nqp::decont($mode));
        # $!storage maps symbol names into symbol information. The information can be one of:
        # - BOOTContext or BOOTHash (nqp::ctxlexpad, nqp::hash) for symbol tables obtained either from a context or
        #   from a Stash
        # - a CtxDynThunk instance for a dynamic symbol sourced from GLOBAL or PROCESS namespaces
        # - an Exception instance for symbols which exists but are not accessible via this PseudoStash due to its mode
        # All values in $!storage hash are choosen to minimize memory footprint of the structure. All of them either
        # taken from where they already exists anyway (like lexpads), or are typeobjects (kind of the previous case),
        # or allocated once ever, like GLOBAL/PROCESS thunking objects.
        nqp::p6bindattrinvres($stash, Map, '$!storage', nqp::hash())
    }

    multi method new(Mu $ctx is raw, Mu :$package!, :$mode = STATIC_CHAIN) {
        my $stash := nqp::create(self);
        my Mu $dctx := nqp::decont($ctx);
        $dctx := nqp::ctxcaller(nqp::ctx()) unless nqp::defined($dctx);
        nqp::bindattr($stash, PseudoStash6c, '$!ctx', nqp::decont($dctx));
        nqp::bindattr_i($stash, PseudoStash6c, '$!mode', nqp::decont($mode));
        nqp::bindattr($stash, PseudoStash, '$!package', nqp::decont($package));
        # See the other method candidate
        nqp::p6bindattrinvres($stash, Map, '$!storage', nqp::hash())
    }

    my Int $id = 0;
    method NEW-PACKAGE(:$name = "<anon|{cas($id, {$_ + 1})}>", *%initargs ) is raw is implementation-detail
    {
        my $stash := self.new(|%initargs);
        nqp::setwho(
            nqp::bindattr($stash, PseudoStash, '$!package', Metamodel::ModuleHOW.new_type(:$name)),
            $stash)
    }

    my $pseudoers := nqp::hash(
        'MY', sub ($cur) {
            PseudoStash.NEW-PACKAGE(
                :ctx(nqp::getattr(nqp::decont($cur), PseudoStash6c, '$!ctx')),
                :mode(PRECISE_SCOPE),
                :name<MY>
            )
        },
        'CORE', sub ($cur) {
            my Mu $ctx := nqp::getattr(nqp::decont($cur), PseudoStash6c, '$!ctx');
            until nqp::isnull($ctx) || nqp::existskey(nqp::ctxlexpad($ctx), 'CORE-SETTING-REV') {
                $ctx := nqp::ctxouterskipthunks($ctx);
            }
            nqp::if(
              nqp::isnull($ctx),
              Nil,
              PseudoStash.NEW-PACKAGE( :$ctx, :mode(STATIC_CHAIN), :name<CORE> ))
        },
        'CALLER', sub ($cur) {
            nqp::if(
              nqp::isnull(
                my Mu $ctx := nqp::ctxcallerskipthunks(
                  nqp::getattr(nqp::decont($cur), PseudoStash6c, '$!ctx'))),
              Nil,
              PseudoStash.NEW-PACKAGE( :$ctx, :mode(PRECISE_SCOPE), :name<CALLER> ))
        },
        'OUTER', sub ($cur) is raw {
            my Mu $ctx := nqp::ctxouterskipthunks(
                            nqp::getattr(nqp::decont($cur), PseudoStash6c, '$!ctx'));

            nqp::isnull($ctx)
                ?? Nil
                !! PseudoStash.NEW-PACKAGE( :$ctx, :mode(PRECISE_SCOPE), :name<OUTER> )
        },
        'LEXICAL', sub ($cur) {
            PseudoStash.NEW-PACKAGE(
                :ctx(nqp::getattr(nqp::decont($cur), PseudoStash6c, '$!ctx')),
                :mode(STATIC_CHAIN +| DYNAMIC_CHAIN),
                :name<LEXICAL>
            )
        },
        'OUTERS', sub ($cur) {
            my Mu $ctx :=
                nqp::ctxouterskipthunks(
                    nqp::getattr(nqp::decont($cur), PseudoStash6c, '$!ctx'));

            nqp::isnull($ctx)
                ?? Nil
                !! PseudoStash.NEW-PACKAGE( :$ctx, :mode(STATIC_CHAIN), :name<OUTERS> )
        },
        'DYNAMIC', sub ($cur) {
            PseudoStash.NEW-PACKAGE(
                :ctx(nqp::getattr(nqp::decont($cur), PseudoStash6c, '$!ctx')),
                :mode(DYNAMIC_CHAIN +| REQUIRE_DYNAMIC),
                :name<DYNAMIC>
            )
        },
        'CALLERS', sub ($cur) {
            my Mu $ctx :=
                nqp::ctxcallerskipthunks(
                    nqp::getattr(nqp::decont($cur), PseudoStash6c, '$!ctx'));

            nqp::isnull($ctx)
                ?? Nil
                !! PseudoStash.NEW-PACKAGE( :$ctx, :mode(DYNAMIC_CHAIN +| REQUIRE_DYNAMIC), :name<CALLERS> )
        },
        'UNIT', sub ($cur) {
            my Mu $ctx := nqp::getattr(nqp::decont($cur), PseudoStash6c, '$!ctx');
            until nqp::isnull($ctx) || nqp::existskey(nqp::ctxlexpad($ctx), '!UNIT_MARKER') {
                $ctx := nqp::ctxouterskipthunks($ctx);
            }

            nqp::isnull($ctx)
                ?? Nil
                !! PseudoStash.NEW-PACKAGE( :$ctx, :mode(STATIC_CHAIN), :name<UNIT> )
        },
        'SETTING', sub ($cur) {
            # Same as UNIT, but go a little further out (two steps, for
            # internals reasons).
            my Mu $ctx := nqp::getattr(nqp::decont($cur), PseudoStash6c, '$!ctx');
            until nqp::isnull($ctx)
                    || (nqp::existskey(nqp::ctxlexpad($ctx), '!UNIT_MARKER')
                        && nqp::not_i(nqp::existskey(nqp::ctxlexpad($ctx), '!EVAL_MARKER'))) {
                $ctx := nqp::ctxouterskipthunks($ctx);
            }
            # EVAL adds two extra contexts to EVAL'ed code.
            my $outers = ($ctx && nqp::existskey(nqp::ctxlexpad($ctx), '!EVAL_MARKER')) ?? 4 !! 2;
            $outers-- if nqp::existskey(nqp::ctxlexpad($ctx), '!RAKUAST_MARKER');
            nqp::until(
                (nqp::isnull($ctx) || !$outers),
                nqp::stmts(
                    ($ctx := nqp::ctxouter($ctx)),
                    ($outers--))
            );

            nqp::isnull($ctx)
                ?? Nil
                !! PseudoStash.NEW-PACKAGE( :$ctx, :mode(STATIC_CHAIN), :name<SETTING> )
        },
        'CLIENT', sub ($cur) {
            my $pkg := nqp::getlexrel(
                nqp::getattr(nqp::decont($cur), PseudoStash6c, '$!ctx'),
                '$?PACKAGE');
            my Mu $ctx := nqp::ctxcallerskipthunks(
                nqp::getattr(nqp::decont($cur), PseudoStash6c, '$!ctx'));
            while nqp::eqaddr(nqp::getlexrel($ctx, '$?PACKAGE'), $pkg) {
                $ctx := nqp::ctxcallerskipthunks($ctx);
                die "No client package found" unless $ctx;
            }

            PseudoStash.NEW-PACKAGE( :$ctx, :mode(PRECISE_SCOPE), :name<CLIENT> )
        },
        'OUR', sub ($cur) {
            nqp::getlexrel(
                nqp::getattr(nqp::decont($cur), PseudoStash6c, '$!ctx'),
                '$?PACKAGE')
        }
    );

    method !find-rev-core($key) {
        my $p6rev = nqp::substr($key, 2, 1);
        my $rev = nqp::getcomp('Raku').lvs.internal-from-p6: nqp::substr($key, 2, 1);
        my $ctx := nqp::getattr(self, PseudoStash6c, '$!ctx');
        my $found := nqp::null();
        my $stash;
        nqp::while(
            $ctx && !$found,
            nqp::stmts(
                (my $lexpad := nqp::ctxlexpad($ctx)),
                nqp::if(
                    nqp::existskey($lexpad, 'CORE-SETTING-REV')
                    && $rev == nqp::atkey($lexpad, 'CORE-SETTING-REV'),
                    ($found := $ctx),
                    ($ctx := nqp::ctxouterskipthunks($ctx)))));
        nqp::if(
            nqp::isnull($found),
            X::NoCoreRevision.new(lang-rev => $p6rev).Failure,
            PseudoStash.NEW-PACKAGE(
                :ctx($found),
                :mode(PRECISE_SCOPE),
                :name("CORE::$key")))
    }

    # Interface between GLOBAL and PROCESS and dynamic chain pseudos. Implements mapping of twigilled symbol names
    # into twigilless for atkey/bindkey
    my class CtxDynThunk {
        has Mu $!stash;

        method !SET-SELF(\package) {
            $!stash := package.WHO;
            self
        }
        method new(\package) {
            nqp::create(self)!SET-SELF(package)
        }
        method atkey(\sym) is raw {
            # nqp::atkey($!storage, nqp::replace(sym, 1, 1, ''))
            $!stash.AT-KEY: nqp::replace(sym, 1, 1, '')
        }
        method bindkey(\sym, \val) {
            # nqp::bindkey($!storage, nqp::replace(sym, 1, 1, ''), val)
            $!stash.BIND-KEY: nqp::replace(sym, 1, 1, ''), val
        }
    }

    # Walks over contexts, respects combined chains (DYNAMIC_CHAIN +| STATIC_CHAIN). In the combined case the inital
    # context would be tried for each mode.
    my class CtxWalker {
        has Mu $!start-ctx;     # Stash context – this is where we start from.
        has Mu $!ctx;           # Current context.
        has Mu $!fallbacks;     # List of possible fallbacks to iterate over
        has Mu $!stash-mode;
        has Mu $!modes;
        has $!cur-mode;
        has $!pick-fallback;
        has $.exhausted;

        my CtxDynThunk $GLOBAL-thunk;
        my CtxDynThunk $PROCESS-thunk;

        class Return {
            has Mu $.lexpad is built(:bind);
            has int $.mode is built(:bind);
            has Mu $.sym-info is built(:bind);
        }

        method !SET-SELF(CtxWalker:D: PseudoStash:D \pseudo) {
            $!start-ctx := nqp::getattr(pseudo, PseudoStash6c, '$!ctx');
            $!ctx := nqp::null();
            $!stash-mode := nqp::getattr_i(pseudo, PseudoStash6c, '$!mode');
            $!modes := nqp::list_i();
            nqp::push_i($!modes, $_)
                for (PRECISE_SCOPE, STATIC_CHAIN, DYNAMIC_CHAIN).grep({ nqp::bitand_i($_, $!stash-mode) });
            $!fallbacks := nqp::list();
            $!pick-fallback := 0;
            $!exhausted := 0;
            self
        }

        method new(PseudoStash:D \pseudo) { nqp::create(self)!SET-SELF(pseudo) }

        method !next-mode() is raw {
            nqp::if(
                nqp::elems($!modes),
                nqp::stmts(
                    ($!ctx := $!start-ctx),
                    ($!cur-mode := nqp::shift_i($!modes)),
                    nqp::if(
                        nqp::iseq_i($!cur-mode, STATIC_CHAIN),
                        nqp::unless(
                            nqp::isnull(my $OUR-PKG := nqp::getlexrel($!start-ctx, '$?PACKAGE')),
                            nqp::push($!fallbacks, $OUR-PKG.WHO)),
                        nqp::if(
                            nqp::iseq_i($!cur-mode, DYNAMIC_CHAIN),
                            nqp::stmts(
                                nqp::unless(
                                    nqp::isnull(my \promise = nqp::getlexreldyn($!start-ctx, '$*PROMISE')),
                                    nqp::push($!fallbacks, promise)),
                                nqp::unless(
                                    nqp::defined($GLOBAL-thunk),
                                    nqp::cas($GLOBAL-thunk, CtxDynThunk, CtxDynThunk.new(nqp::getcurhllsym('GLOBAL')))),
                                nqp::push($!fallbacks, nqp::decont($GLOBAL-thunk)),
                                nqp::unless(
                                    nqp::defined($PROCESS-thunk),
                                    nqp::cas($PROCESS-thunk, CtxDynThunk, CtxDynThunk.new(nqp::getcurhllsym('PROCESS')))),
                                nqp::push($!fallbacks, nqp::decont($PROCESS-thunk))),
                            nqp::unless(
                                nqp::iseq_i($!cur-mode, PRECISE_SCOPE),
                                nqp::die("Unknown pseudo-stash mode " ~ $!cur-mode))))),
                nqp::stmts(
                    ($!ctx := nqp::null()),
                    ($!exhausted := 1)));
        }

        method next-ctx() is raw {
            self!next-mode if nqp::isnull($!ctx);
            return Nil if $!exhausted;

            my Mu $lexpad;
            my Mu $sym-info;
            nqp::stmts(
                ($lexpad :=
                    nqp::if(
                        nqp::istype($!ctx, Map), # For a Stash fallback: OUR:: for static chain
                        ($sym-info := nqp::getattr($!ctx, Map, '$!storage')),
                        nqp::if(
                            nqp::istype($!ctx, CtxDynThunk), # For dynamic chain mapping into GLOBAL/PROCESS
                            nqp::getattr(nqp::getattr(($sym-info := $!ctx), CtxDynThunk, '$!stash'), Map, '$!storage'),
                            # At this point the only possible value is BOOTContext
                            ($sym-info := nqp::ctxlexpad($!ctx))))),
                nqp::if(
                    nqp::bitand_i($!stash-mode, PRECISE_SCOPE),
                    ($!ctx := nqp::null()),
                    nqp::repeat_while(
                        (nqp::isnull($!ctx) && $!pick-fallback), # Switch to fallback if a context chain is exhausted
                        nqp::if(
                            $!pick-fallback,
                            nqp::if(
                                nqp::elems($!fallbacks),
                                nqp::stmts(
                                    ($!ctx := nqp::shift($!fallbacks)),
                                    nqp::if(
                                        nqp::istype($!ctx, Promise),
                                        nqp::stmts(
                                            ($!ctx := nqp::getattr($!ctx, Promise, '$!dynamic_context')),
                                            ($!pick-fallback := 0)))),
                                nqp::stmts(
                                    ($!ctx := nqp::null()),
                                    ($!pick-fallback := 0))),
                            nqp::stmts(
                                nqp::if(
                                    nqp::iseq_i($!cur-mode, DYNAMIC_CHAIN),
                                    ($!ctx := nqp::ctxcallerskipthunks($!ctx))),
                                nqp::if(
                                    nqp::iseq_i($!cur-mode, STATIC_CHAIN),
                                    ($!ctx := nqp::ctxouterskipthunks($!ctx))),
                                nqp::ifnull( # Ran through a chain till the end
                                    $!ctx,
                                    ($!pick-fallback := 1)))))),
                Return.new(:$lexpad, :$sym-info, :mode($!cur-mode)))
        }
    }

    method REIFY-STORAGE(Str :$until-symbol) is implementation-detail {
        my Mu $storage := nqp::getattr(self, Map, '$!storage');
        return 1 if $until-symbol && nqp::existskey($storage, $until-symbol);

        nqp::unless(nqp::isconcrete($!walker), ($!walker := CtxWalker.new(self)));
        return 0 if $!walker.exhausted;

        my Mu $mode := nqp::getattr(self, PseudoStash6c, '$!mode');
        my Mu $found := 0;
        nqp::while(
            ((my $ctx-info := $!walker.next-ctx) && !$found),
            nqp::stmts(
                (my Mu $iter := nqp::iterator(nqp::getattr($ctx-info, CtxWalker::Return, '$!lexpad'))),
                nqp::while(
                    $iter,
                    nqp::stmts(
                        (my Mu $sym := nqp::iterkey_s(nqp::shift($iter))),
                        nqp::if(
                            nqp::istype($ctx-info.sym-info, CtxDynThunk),
                            ($sym := nqp::replace($sym, 1, 0, '*'))),
                        nqp::if(
                            nqp::defined($until-symbol),
                            nqp::unless($found, ($found := nqp::iseq_s($sym, $until-symbol)))),
                        nqp::unless(
                            nqp::existskey($storage, $sym), # Skip if already encountered
                            nqp::stmts(
                                (my Mu $val := nqp::iterval($iter)),
                                (my Mu $is-dynamic := nqp::iseq_i(nqp::ord($sym, 1), 42)), # has * twigil
                                (my Mu $sym-info :=
                                    nqp::if(
                                        (nqp::bitand_i($mode, REQUIRE_DYNAMIC)
                                         || nqp::iseq_i($ctx-info.mode, DYNAMIC_CHAIN)),
                                        nqp::if(
                                            ($is-dynamic || nqp::if(nqp::iscont($val), $val.VAR.dynamic)),
                                            nqp::getattr($ctx-info, CtxWalker::Return, '$!sym-info'),
                                            X::Symbol::NotDynamic),
                                        nqp::if(
                                            (!$is-dynamic || nqp::bitand_i($mode, PRECISE_SCOPE)),
                                            nqp::getattr($ctx-info, CtxWalker::Return, '$!sym-info'),
                                            X::Symbol::NotLexical))),
                                nqp::bindkey($storage, $sym, $sym-info)))))));
        $found
    }

    my sub SYM-VALUE(Mu \sym-info, \sym-key) is raw {
        nqp::if(
            nqp::istype(sym-info, CtxDynThunk),
            (sym-info.atkey(sym-key)),
            nqp::if(
                nqp::istype(sym-info, Exception),
                sym-info,
                nqp::atkey(sym-info, sym-key)))
    }

    multi method AT-KEY(PseudoStash:D: Str() $key) is raw {
        my $name := $!package.^name;
        my Mu $val := nqp::if(
            (nqp::iseq_s($name, 'CORE')
                && nqp::iseq_i(nqp::chars($key), 3)
                && nqp::iseq_i(nqp::index($key, 'v6'), 0)),
            self!find-rev-core($key),
            nqp::null());
        nqp::if(
            nqp::isnull($val),
            nqp::if(
                nqp::existskey($pseudoers,$key),
                ($val := nqp::atkey($pseudoers, $key)(self)),
                nqp::if(
                    (self.REIFY-STORAGE(until-symbol => $key)),
                    nqp::stmts(
                        (my Mu $sym-info := nqp::atkey(nqp::getattr(self, Map, '$!storage'), $key)),
                        ($val := SYM-VALUE($sym-info, $key))))));
        nqp::if(
            nqp::isnull($val),
            Failure.new(X::NoSuchSymbol.new(symbol => $!package.^name ~ '::<' ~ $key ~ '>')),
            nqp::if(
                nqp::istype($val, Exception),
                Failure.new($val.new(package => $!package.^name, symbol => $key)),
                $val))
    }

    method BIND-KEY(PseudoStash:D: Str() $key, Mu \value) is raw {
        nqp::if(
            nqp::existskey($pseudoers,$key),
            X::Bind.new(target => "pseudo-package $key").throw,
            nqp::if(
                (self.REIFY-STORAGE(until-symbol => $key)),
                nqp::stmts(
                    (my Mu $storage := nqp::getattr(self, Map, '$!storage')),
                    (my Mu $sym-info := nqp::atkey($storage, $key)),
                    nqp::if(
                        nqp::istype($sym-info, CtxDynThunk),
                        ($sym-info.bindkey($key, value)),
                        nqp::if(
                            nqp::istype($sym-info, Exception),
                            ($sym-info.new(symbol => $key, package => $!package.^name).throw),
                            nqp::bindkey($sym-info, $key, value)))),
                (X::NoSuchSymbol.new(symbol => $!package.^name ~ '::<' ~ $key ~ '>').throw)))
    }

    method EXISTS-KEY(PseudoStash:D: Str:D() $key) {
        nqp::hllbool(
            nqp::unless(
                nqp::existskey($pseudoers, $key),
                nqp::if(
                    (self.REIFY-STORAGE(until-symbol => $key)),
                    nqp::not_i(nqp::istype(nqp::atkey(nqp::getattr(self, Map, '$!storage'), $key), Exception)))))
    }

    my role CtxSymIterator does Rakudo::Iterator::Mappy {
        has $!implementation-detail;

        method !set-implementation-detail(:$!implementation-detail) { }

        method new(PseudoStash:D \stash, $implementation-detail = 0) {
            # Fill the storage with all available symbols for iteration.
            stash.REIFY-STORAGE;
            my $iter = self.Rakudo::Iterator::Mappy::new(stash);
            # The upstream new may return Rakudo::Iterator::Empty
            $iter!set-implementation-detail(:$implementation-detail) if $iter ~~ ::?ROLE;
            $iter
        }

        method !accept-item(Mu \iter) {
            my $sym-info := nqp::iterval(iter);
            return 0 if nqp::istype($sym-info, Exception);
            my $val := SYM-VALUE($sym-info, nqp::iterkey_s(iter));
            nqp::unless(
                $!implementation-detail,
                nqp::not_i(
                    nqp::if(
                        nqp::istype(nqp::decont($val), Code),
                        $val.is-implementation-detail)))
        }

        method skip-one {
            my $iter = nqp::getattr(self, Rakudo::Iterator::Mappy, '$!iter');
            my $done = 0;
            nqp::while(
                ($iter && !$done),
                nqp::stmts(
                    nqp::shift($iter),
                    ($done = self!accept-item($iter))));
            $done
        }
    }

    my class CtxSymIterator::Pairs does CtxSymIterator {
        method pull-one {
            my Mu $item := IterationEnd;
            nqp::while(
                ($!iter && nqp::eqaddr($item, IterationEnd)),
                nqp::stmts(
                    nqp::shift($!iter),
                    nqp::if(
                        self!accept-item($!iter),
                        ($item := Pair.new(
                                    nqp::iterkey_s($!iter),
                                    SYM-VALUE(nqp::iterval($!iter), nqp::iterkey_s($!iter)))))));
            $item
        }
        method push-all(\target --> IterationEnd) {
            nqp::while(
                $!iter,
                nqp::stmts(
                    nqp::shift($!iter),
                    nqp::if(
                        self!accept-item($!iter),
                        (target.push(
                            Pair.new(
                                nqp::iterkey_s($!iter),
                                SYM-VALUE(nqp::iterval($!iter), nqp::iterkey_s($!iter))))))))
        }
    }

    my class CtxSymIterator::Keys does CtxSymIterator {
        method new(|c) { self.CtxSymIterator::new(|c) }
        method pull-one() {
            my Mu $item := IterationEnd;
            nqp::while(
                ($!iter && nqp::eqaddr($item, IterationEnd)),
                nqp::stmts(
                    nqp::shift($!iter),
                    nqp::if(
                        (self!accept-item($!iter)),
                        ($item := nqp::iterkey_s($!iter)))));
            $item
        }
        method push-all(\target --> IterationEnd) {
            nqp::while(
                $!iter,
                nqp::stmts(
                    nqp::shift($!iter),
                    nqp::if(
                        (self!accept-item($!iter)),
                        (target.push(nqp::iterkey_s($!iter))))))
        }
    }

    my class CtxSymIterator::Values does CtxSymIterator {
        method new(|c) { self.CtxSymIterator::new(|c) }
        method pull-one {
            my Mu $item := IterationEnd;
            nqp::while(
                ($!iter && nqp::eqaddr($item, IterationEnd)),
                nqp::stmts(
                    nqp::shift($!iter),
                    nqp::if(
                        self!accept-item($!iter),
                        ($item := SYM-VALUE(nqp::iterval($!iter), nqp::iterkey_s($!iter))))));
            $item
        }
        method push-all(\target --> IterationEnd) {
            nqp::while(
                $!iter,
                nqp::stmts(
                    nqp::shift($!iter),
                    nqp::if(
                        self!accept-item($!iter),
                        (target.push(SYM-VALUE(nqp::iterval($!iter), nqp::iterkey_s($!iter)))))))
        }
    }

    my class CtxSymIterator::KV does CtxSymIterator {
        has $!on;

        method pull-one {
            my Mu $item = IterationEnd;
            nqp::while(
                ($!iter && nqp::eqaddr($item, IterationEnd)),
                nqp::if(
                    $!on,
                    nqp::stmts(
                        ($!on := 0),
                        ($item := SYM-VALUE(nqp::iterval($!iter), nqp::iterkey_s($!iter)))),
                    nqp::stmts(
                        nqp::shift($!iter),
                        nqp::if(
                            self!accept-item($!iter),
                            nqp::stmts(
                                ($!on := 1),
                                ($item := nqp::iterkey_s($!iter)))))));
            $item
        }
        method push-all(\target --> IterationEnd) {
            nqp::while(
                $!iter,
                nqp::stmts(
                    nqp::shift($!iter),
                    nqp::if(
                        self!accept-item($!iter),
                        nqp::stmts(
                            target.push(nqp::iterkey_s($!iter)),
                            target.push(SYM-VALUE(nqp::iterval($!iter), nqp::iterkey_s($!iter)))))))
        }
    }

    # The default iterator includes the implementation detail symbols.
    multi method iterator(::?CLASS:D: --> Iterator:D) { CtxSymIterator::Pairs.new(self, 1) }

    multi method keys(::?CLASS:D: :$implementation-detail --> Seq:D) {
        Seq.new(CtxSymIterator::Keys.new(self, $implementation-detail))
    }

    multi method values(::?CLASS:D: :$implementation-detail --> Seq:D) {
        Seq.new(CtxSymIterator::Values.new(self, $implementation-detail))
    }

    multi method kv(::?CLASS:D: :$implementation-detail --> Seq:D) {
        Seq.new(CtxSymIterator::KV.new(self, $implementation-detail))
    }

    multi method pairs(::?CLASS:D: :$implementation-detail --> Seq:D) {
        Seq.new(CtxSymIterator::Pairs.new(self, $implementation-detail))
    }

    method sort(::?CLASS:D: :$implementation-detail --> Seq:D) {
        Seq.new(CtxSymIterator::Pairs.new(self, $implementation-detail)).sort
    }

    multi method elems(::?CLASS:D: :$implementation-detail) {
        Seq.new(CtxSymIterator::Values.new(self, $implementation-detail)).elems
    }
}

# vim: expandtab shiftwidth=4
