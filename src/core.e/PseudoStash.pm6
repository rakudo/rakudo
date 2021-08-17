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

    method new(Mu :$ctx is raw, :$mode = STATIC_CHAIN) {
        my $stash := nqp::create(self);
        $ctx := nqp::decont($ctx);
        $ctx := nqp::ctxcaller(nqp::ctx()) unless nqp::defined($ctx);
        nqp::bindattr($stash, PseudoStash6c, '$!ctx', nqp::decont($ctx));
        nqp::bindattr_i($stash, PseudoStash6c, '$!mode', nqp::decont($mode));
        nqp::p6bindattrinvres($stash, Map, '$!storage', nqp::hash())
    }

    my atomicint $id = 0;
    method NEW-PACKAGE(:$name = "<anon|{++⚛$id}>", *%initargs) is raw is implementation-detail {
        my $stash := self.new(|%initargs);
        nqp::setwho(
            nqp::bindattr($stash, PseudoStash, '$!package', Metamodel::ModuleHOW.new_type(:$name)),
            $stash
        )
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
              PseudoStash.NEW-PACKAGE( :$ctx, :mode(STATIC_CHAIN), :name<CORE> )
            )
        },
        'CALLER', sub ($cur) {
            nqp::if(
              nqp::isnull(
                my Mu $ctx := nqp::ctxcallerskipthunks(
                  nqp::getattr(nqp::decont($cur), PseudoStash6c, '$!ctx'))),
              Nil,
              PseudoStash.NEW-PACKAGE( :$ctx, :mode(PRECISE_SCOPE), :name<CALLER> )
            )
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
                        && !nqp::existskey(nqp::ctxlexpad($ctx), '!EVAL_MARKER')) {
                $ctx := nqp::ctxouterskipthunks($ctx);
            }
            # EVAL adds two extra contexts to EVAL'ed code.
            my $outers = ($ctx && nqp::existskey(nqp::ctxlexpad($ctx), '!EVAL_MARKER')) ?? 4 !! 2;
            $outers-- if nqp::existskey(nqp::ctxlexpad($ctx), '!RAKUAST_MARKER');
            nqp::until(
                (nqp::isnull($ctx) || !$outers),
                nqp::stmts(
                    ($ctx := nqp::ctxouter($ctx)),
                    ($outers--)
                )
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

    # Walks over contexts, respects combined chains (DYNAMIC_CHAIN +| STATIC_CHAIN). In the combined case the inital
    # context would be tried for each mode.
    my class CtxWalker {
        has Mu $!start-ctx;     # Stash context – this is where we start from.
        has Mu $!ctx;           # Current context.
        has int $!stash-mode;
        has $!modes;

        class Return {
            has $.ctx is built(:bind);
            has int $.mode is built(:bind);
        }

        method !SET-SELF(CtxWalker:D: PseudoStash:D \pseudo) {
            nqp::bindattr(self, CtxWalker, '$!start-ctx', nqp::getattr(pseudo, PseudoStash6c, '$!ctx'));
            nqp::bindattr(self, CtxWalker, '$!ctx', nqp::getattr(pseudo, PseudoStash6c, '$!ctx'));
            nqp::bindattr_i(self, CtxWalker, '$!stash-mode',
                            (nqp::getattr(pseudo, PseudoStash6c, '$!mode') || STATIC_CHAIN) # We default to STATIC_CHAIN
            );
            $!modes := nqp::list_i(STATIC_CHAIN, DYNAMIC_CHAIN);
            self
        }

        method new(PseudoStash:D \pseudo) { nqp::create(self)!SET-SELF(pseudo) }

        method exhausted() { nqp::isnull($!ctx) }

        method next-ctx() is raw {
            return Nil if nqp::isnull($!ctx);
            my int $ret-mode;
            nqp::stmts(
                (my Mu $ret-ctx := $!ctx),
                # Don't iterate over precise scope or when all modes has been tried.
                nqp::if(
                    (nqp::bitand_i($!stash-mode,PRECISE_SCOPE) || (nqp::elems($!modes) == 0)),
                    ($!ctx := nqp::null()),
                    nqp::repeat_while(
                        (nqp::isnull($!ctx) && nqp::elems($!modes)),
                        nqp::if(    # Skip a mode unless the stash has it set
                            nqp::bitand_i($!stash-mode,nqp::atpos_i($!modes,0)),
                            nqp::stmts(
                                ($ret-mode = nqp::atpos_i($!modes, 0)),
                                # If $!ctx is not set at this point then mode switch has took place. Start over.
                                # The inital context would be returned next time again paired with the new mode.
                                nqp::unless(
                                    $!ctx,
                                    nqp::bindattr(self, CtxWalker, '$!ctx', $!start-ctx),
                                    nqp::stmts(
                                        nqp::if(
                                            nqp::iseq_i($ret-mode, DYNAMIC_CHAIN),
                                            ($!ctx := nqp::ctxcallerskipthunks($!ctx)),
                                        ),
                                        nqp::if(
                                            nqp::iseq_i($ret-mode, STATIC_CHAIN),
                                            ($!ctx := nqp::ctxouterskipthunks($!ctx)),
                                        ),
                                    )
                                ),
                                nqp::unless(    # If it's the last context then switch to the next mode.
                                    $!ctx,
                                    nqp::shift_i($!modes),
                                )
                            ),
                            nqp::shift_i($!modes)
                        )
                    )
                ),
                Return.new(ctx => $ret-ctx, mode => $ret-mode)
            )
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
                # TODO? Technically it is possible for the walker to take $!storage from GLOBAL's and PROCESS' Stash and
                # return it in $ctx-info.ctx for dynamic chains. This way we can even better simulate the behavior of
                # &DYNAMIC. The only problem is the * twigil which symbols in these two packages don't have. This
                # overcomplicates further symbol lookup because a user would expected DYNAMIC::<$*IN> to work, but
                # AT-KEY, EXISTS-KEY, and so on, will be in a big trouble trying to determine if the twigil is to be
                # removed for lookups.
                # Other way around would be not to remove the twigil. But this would cause problems when, say, $*IN is
                # defined in a caller context where it shadows off PROCESS::<$IN>. But our dynamic chain would then have
                # both.
                (my Mu $lexpad := nqp::ctxlexpad($ctx-info.ctx)),
                (my Mu $iter := nqp::iterator($lexpad)),
                nqp::while(
                    $iter,
                    nqp::stmts(
                        (my Mu $sym := nqp::iterkey_s(nqp::shift($iter))),
                        nqp::if(
                            nqp::defined($until-symbol),
                            nqp::unless($found, ($found := nqp::iseq_s($sym, $until-symbol)))),
                        nqp::unless(
                            nqp::existskey($storage, $sym), # Skip if already encountered
                            nqp::stmts(
                                (my Mu $val := nqp::iterval($iter)),
                                (my Mu $is-dynamic := nqp::iseq_i(nqp::ord($sym, 1), 42)), # has * twigil
                                (my Mu $storage-val :=
                                    nqp::if(
                                        (nqp::bitand_i($mode, REQUIRE_DYNAMIC)
                                         || nqp::iseq_i($ctx-info.mode, DYNAMIC_CHAIN)),
                                        nqp::if(
                                            ($is-dynamic || nqp::if(nqp::can($val.VAR, "dynamic"), $val.VAR.dynamic)),
                                            $lexpad,
                                            X::Symbol::NotDynamic
                                        ),
                                        nqp::if(
                                            (!$is-dynamic || nqp::bitand_i($mode, PRECISE_SCOPE)),
                                            $lexpad,
                                            X::Symbol::NotLexical
                                        )
                                    )
                                ),
                                nqp::bindkey($storage, $sym, $storage-val)))))));
        $found
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
                        (my Mu $lexpad := nqp::atkey(nqp::getattr(self, Map, '$!storage'), $key)),
                        ($val := nqp::if(
                                    nqp::istype($lexpad, Exception),
                                    $lexpad,
                                    nqp::atkey($lexpad, $key)))))
            )
        );
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
                    (my Mu $lexpad := nqp::atkey($storage, $key)),
                    nqp::if(
                        nqp::istype($lexpad, Exception),
                        ($lexpad.new(symbol => $key, package => $!package.^name).throw),
                        nqp::bindkey($lexpad, $key, value)
                    )
                ),
                (X::NoSuchSymbol.new(symbol => $!package.^name ~ '::<' ~ $key ~ '>').throw)
            )
        )
    }

    method EXISTS-KEY(PseudoStash:D: Str:D() $key) {
        nqp::hllbool(
            nqp::unless(
                nqp::existskey($pseudoers, $key),
                nqp::if(
                    (self.REIFY-STORAGE(until-symbol => $key)),
                    nqp::not_i(nqp::istype(nqp::getattr(self, Map, '$!storage'), Exception)))))
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
            my $lexpad := nqp::iterval(iter);
            return 0 if nqp::istype($lexpad, Exception);
            my $val := nqp::atkey($lexpad, nqp::iterkey_s(iter));
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
                                    nqp::atkey(nqp::iterval($!iter), nqp::iterkey_s($!iter)))))));
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
                                nqp::atkey(nqp::iterval($!iter), nqp::iterkey_s($!iter))))))))
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
                        ($item := nqp::atkey(nqp::iterval($!iter), nqp::iterkey_s($!iter))))));
            $item
        }
        method push-all(\target --> IterationEnd) {
            nqp::while(
                $!iter,
                nqp::stmts(
                    nqp::shift($!iter),
                    nqp::if(
                        self!accept-item($!iter),
                        (target.push(nqp::atkey(nqp::iterval($!iter), nqp::iterkey_s($!iter)))))))
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
                        ($item := nqp::atkey(nqp::iterval($!iter), nqp::iterkey_s($!iter)))),
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
                            target.push(nqp::atkey(nqp::iterval($!iter), nqp::iterkey_s($!iter)))))))
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
