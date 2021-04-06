# my class X::Bind { ... }
# my class X::Caller::NotDynamic { ... }
# my class X::NoSuchSymbol { ... }

my class PseudoStash is Map {
    has Mu $!ctx;
    has int $!mode;
    has $!package;      # Parent package, for which we serve as .WHO

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

    multi method WHICH(PseudoStash:D: --> ObjAt:D) { self.Mu::WHICH }

    my $pseudoers := nqp::hash(
        'MY', sub ($cur) {
            my $stash := nqp::clone($cur);
            nqp::bindattr_i($stash, PseudoStash, '$!mode', PRECISE_SCOPE);
            $stash.pseudo-package('MY');
        },
        'CORE', sub ($cur) {
            my Mu $ctx := nqp::getattr(nqp::decont($cur), PseudoStash, '$!ctx');
            until nqp::isnull($ctx) || nqp::existskey(nqp::ctxlexpad($ctx), 'CORE-SETTING-REV') {
                $ctx := nqp::ctxouterskipthunks($ctx);
            }
            nqp::if(
              nqp::isnull($ctx),
              Nil,
              nqp::stmts(
                (my $stash := nqp::create(PseudoStash)),
                nqp::bindattr($stash, Map, '$!storage', nqp::ctxlexpad($ctx)),
                nqp::bindattr($stash, PseudoStash, '$!ctx', $ctx),
                nqp::bindattr_i($stash, PseudoStash, '$!mode', STATIC_CHAIN),
                $stash.pseudo-package('CORE'),
              )
            )
        },
        'CALLER', sub ($cur) {
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
                $stash.pseudo-package('CALLER')
              )
            )
        },
        'OUTER', sub ($cur) is raw {
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
                $stash.pseudo-package('OUTER')
            }
        },
        'LEXICAL', sub ($cur) {
            my $stash := nqp::clone($cur);
            nqp::bindattr_i($stash, PseudoStash, '$!mode', STATIC_CHAIN +| DYNAMIC_CHAIN);
            $stash.pseudo-package('LEXICAL')
        },
        'OUTERS', sub ($cur) {
            my Mu $ctx := nqp::ctxouterskipthunks(
                nqp::getattr(nqp::decont($cur), PseudoStash, '$!ctx'));

            if nqp::isnull($ctx) {
                Nil
            }
            else {
                my $stash := nqp::create(PseudoStash);
                nqp::bindattr($stash, Map, '$!storage', nqp::ctxlexpad($ctx));
                nqp::bindattr($stash, PseudoStash, '$!ctx', $ctx);
                nqp::bindattr_i($stash, PseudoStash, '$!mode', STATIC_CHAIN);
                $stash.pseudo-package('OUTERS')
            }
        },
        'DYNAMIC', sub ($cur) {
            my $stash := nqp::clone($cur);
            nqp::bindattr_i($stash, PseudoStash, '$!mode', DYNAMIC_CHAIN +| REQUIRE_DYNAMIC);
            $stash.pseudo-package('DYNAMIC');
        },
        'CALLERS', sub ($cur) {
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
                $stash.pseudo-package('CALLERS')
              )
            )
        },
        'UNIT', sub ($cur) {
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
                nqp::bindattr_i($stash, PseudoStash, '$!mode', STATIC_CHAIN),
                $stash.pseudo-package('UNIT')
              )
            )
        },
        'SETTING', sub ($cur) {
            # Same as UNIT, but go a little further out (two steps, for
            # internals reasons).
            my Mu $ctx := nqp::getattr(nqp::decont($cur), PseudoStash, '$!ctx');
            until nqp::isnull($ctx)
                    || (nqp::existskey(nqp::ctxlexpad($ctx), '!UNIT_MARKER')
                        && !nqp::existskey(nqp::ctxlexpad($ctx), '!EVAL_MARKER')) {
                $ctx := nqp::ctxouterskipthunks($ctx);
            }
            # EVAL adds two extra contexts to EVAL'ed code.
            my $outers = ($ctx && nqp::existskey(nqp::ctxlexpad($ctx), '!EVAL_MARKER')) ?? 4 !! 2;
            nqp::until(
                (nqp::isnull($ctx) || !$outers),
                nqp::stmts(
                    ($ctx := nqp::ctxouter($ctx)),
                    ($outers--)
                )
            );
            nqp::if(
              nqp::isnull($ctx),
              Nil,
              nqp::stmts(
                (my $stash := nqp::create(PseudoStash)),
                nqp::bindattr($stash, Map, '$!storage', nqp::ctxlexpad($ctx)),
                nqp::bindattr($stash, PseudoStash, '$!ctx', $ctx),
                nqp::bindattr_i($stash, PseudoStash, '$!mode', STATIC_CHAIN),
                $stash.pseudo-package('SETTING')
              )
            )
        },
        'CLIENT', sub ($cur) {
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
            $stash.pseudo-package('CLIENT');
        },
        'OUR', sub ($cur) {
            nqp::getlexrel(
                nqp::getattr(nqp::decont($cur), PseudoStash, '$!ctx'),
                '$?PACKAGE')
        }
    );


    method !find-rev-core($key) {
        my $rev = nqp::substr($key, 2, 1);
        my $ctx := $!ctx;
        my $found := nqp::null();
        my $stash;
        nqp::while(
            $ctx && !$found,
            nqp::stmts(
                (my $lexpad := nqp::ctxlexpad($ctx)),
                nqp::if(
                    nqp::existskey($lexpad, 'CORE-SETTING-REV')
                    && nqp::iseq_s($rev, nqp::atkey($lexpad, 'CORE-SETTING-REV')),
                    ($found := $ctx),
                    ($ctx := nqp::ctxouterskipthunks($ctx))
                ),
            )
        );
        nqp::if(
            nqp::isnull($found),
            Failure.new(X::NoCoreRevision.new(lang-rev => $rev)),
            nqp::stmts(
                ($stash := nqp::create(PseudoStash)),
                nqp::bindattr($stash, Map, '$!storage', nqp::ctxlexpad($found)),
                nqp::bindattr($stash, PseudoStash, '$!ctx', $found),
                nqp::bindattr_i($stash, PseudoStash, '$!mode', PRECISE_SCOPE),
                $stash.pseudo-package('CORE::' ~ $key)
            )
        )
    }

    multi method AT-KEY(PseudoStash:D: Str() $key) is raw {
        my $name := $!package.^name;
        my Mu $val := nqp::if(
            (nqp::iseq_s($name, 'CORE')
                && nqp::iseq_i(nqp::chars($key), 3)
                && nqp::iseq_i(nqp::index($key, 'v6'), 0)),
            self!find-rev-core($key),
            nqp::null()
        );
        nqp::if(
          nqp::isnull($val) && nqp::existskey($pseudoers,$key),
          ($val := nqp::atkey($pseudoers,$key)(self)),
          nqp::stmts(
            nqp::if(          # PRECISE_SCOPE is exclusive
              nqp::bitand_i($!mode,PRECISE_SCOPE),
              nqp::if(
                nqp::existskey(
                  nqp::getattr(self,Map,'$!storage'),nqp::unbox_s($key)),
                ($val := nqp::atkey(
                  nqp::getattr(self,Map,'$!storage'),nqp::unbox_s($key)))
              ),
              nqp::stmts(         # DYNAMIC_CHAIN can be combined with STATIC_CHAIN
                nqp::if(          # DYNAMIC_CHAIN
                    (nqp::isnull($val)
                        && nqp::bitand_i(
                          $!mode,nqp::bitor_i(DYNAMIC_CHAIN,PICK_CHAIN_BY_NAME)
                          ) && nqp::iseq_i(nqp::ord(nqp::unbox_s($key),1),42)),  # "*"
                    ($val := nqp::ifnull(
                      nqp::getlexreldyn(
                        nqp::getattr(self,PseudoStash,'$!ctx'),nqp::unbox_s($key)),
                      nqp::null()
                    ))
                ),
                nqp::if(            # STATIC_CHAIN is the default
                    nqp::isnull($val),
                    ($val := nqp::ifnull(
                        nqp::getlexrel(
                          nqp::getattr(self,PseudoStash,'$!ctx'),nqp::unbox_s($key)),
                        nqp::null()
                    ))
                )
              )
            ),
            nqp::if(
              (nqp::not_i(nqp::isnull($val))
                && nqp::bitand_i($!mode,REQUIRE_DYNAMIC)),
              nqp::if(
                (try nqp::not_i($val.VAR.dynamic)),
                ($val := Failure.new(X::Caller::NotDynamic.new(symbol => $key)))
              )
            )
          )
        );
        nqp::isnull($val)
            ?? Failure.new(X::NoSuchSymbol.new(symbol => $!package.^name ~ '::<' ~ $key ~ '>'))
            !! $val
    }

    multi method ASSIGN-KEY(PseudoStash:D: Str() $key, Mu \value) is raw {
        self.AT-KEY($key) = value
    }

    # Walks over contexts, respects combined chains (DYNAMIC_CHAIN +| STATIC_CHAIN). It latter case the inital context
    # would be repeated for each mode.
    my class CtxWalker {
        has Mu $!start-ctx;     # Stash context – this is where we start from.
        has Mu $!ctx;           # Current context.
        has int $!stash-mode;
        has $!modes;

        method !SET-SELF(CtxWalker:D: PseudoStash:D \pseudo) {
            nqp::bindattr(self, CtxWalker, '$!start-ctx', nqp::getattr(pseudo, PseudoStash, '$!ctx'));
            nqp::bindattr(self, CtxWalker, '$!ctx', nqp::getattr(pseudo, PseudoStash, '$!ctx'));
            nqp::bindattr_i(self, CtxWalker, '$!stash-mode',
                            (nqp::getattr(pseudo, PseudoStash, '$!mode') || STATIC_CHAIN) # We default to STATIC_CHAIN
            );
            $!modes := nqp::list_i(PRECISE_SCOPE, DYNAMIC_CHAIN, STATIC_CHAIN);
            self
        }

        method new(PseudoStash:D \pseudo) { nqp::create(self)!SET-SELF(pseudo) }

        method exhausted() { nqp::isnull($!ctx) }

        method next-ctx() {
            return [] if nqp::isnull($!ctx);
            nqp::stmts(
                (my Mu $ret-ctx := $!ctx),
                (my $ret-mode := nqp::atpos_i($!modes,0)),
                # Don't iterate over precise scope or when all modes has been tried.
                nqp::if(
                    (nqp::bitand_i($!stash-mode,PRECISE_SCOPE) || (nqp::elems($!modes) == 0)),
                    ($!ctx := nqp::null()),
                    nqp::repeat_while(
                        (nqp::isnull($!ctx) && nqp::elems($!modes)),
                        nqp::if(    # Skip a mode unless the stash has it set
                            nqp::bitand_i($!stash-mode,nqp::atpos_i($!modes,0)),
                            nqp::stmts(
                                # If $!ctx is not set at this point then mode switch has took place. Start over.
                                # The inital context would be returned next time again paired with the new mode.
                                nqp::unless(
                                    $!ctx,
                                    nqp::bindattr(self, CtxWalker, '$!ctx', $!start-ctx),
                                    nqp::stmts(
                                        nqp::if(
                                            nqp::iseq_i(nqp::atpos_i($!modes,0),DYNAMIC_CHAIN),
                                            ($!ctx := nqp::ctxcallerskipthunks($!ctx)),
                                        ),
                                        nqp::if(
                                            nqp::iseq_i(nqp::atpos_i($!modes,0),STATIC_CHAIN),
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
                # XXX nqp::list() would be faster, perhaps. But `is raw` is ignored for methods converting BOOTArray
                # into List.
                [$ret-ctx, $ret-mode]
            )
        }
    }

    # Finds the context in which $key is defined. Throws if not found.
    # Returns nqp::list(found-ctx, mode-flag) – same as CtxWalker
    method lookup-ctx(Str $key) {
        my @target;
        my $ctx-walker := CtxWalker.new(self);
        nqp::stmts(
            nqp::while(
                ((my @ctx-info = $ctx-walker.next-ctx) && !@target),
                nqp::stmts(
                    (my $ctx := nqp::decont(@ctx-info[0])),
                    nqp::if(
                        nqp::existskey($ctx,nqp::unbox_s($key)),
                        nqp::if(    # Skip if non-dynamic symbol is found in a DYNAMIC_CHAIN
                            ((@ctx-info[1] != DYNAMIC_CHAIN)
                              || nqp::atkey($ctx,nqp::unbox_s($key)).VAR.dynamic),
                            (@target = @ctx-info)
                        )
                    )
                )
            ),
            nqp::unless(
                @target,
                X::NoSuchSymbol.new(symbol => $!package.^name ~ '::<' ~ $key ~ '>').throw
            )
        );
        $ctx := nqp::decont(@target[0]);
        @target
    }

    method BIND-KEY(PseudoStash:D: Str() $key, Mu \value) is raw {
        nqp::if(
          nqp::existskey($pseudoers,$key),
          X::Bind.new(target => "pseudo-package $key").throw,
          nqp::if(
            nqp::bitand_i($!mode,PRECISE_SCOPE),
            nqp::bindkey(
                nqp::getattr(self,Map,'$!storage'),nqp::unbox_s($key),value
            ),
            nqp::bindkey(
                nqp::ctxlexpad(nqp::decont(self.lookup-ctx($key)[0])),
                nqp::unbox_s($key),
                value
            ),
          )
        )
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
                nqp::not_i(           # STATIC_CHAIN
                  nqp::isnull(
                    nqp::getlexrel(
                      nqp::getattr(self, PseudoStash, '$!ctx'),
                      nqp::unbox_s($key))))
              )
            )
          )
        )
    }

    # Iterate over context
    my role CtxSymIterator does Iterator {
        has PseudoStash $!stash;
        has $!stash-mode;
        has Mu $!ctx;
        has $!ctx-mode;
        has $!ctx-walker;
        has $!iter;
        has $!seen; # this also serves as "the first run" indicator.

        method !SET-SELF(PseudoStash:D \pseudo) {
            $!stash := pseudo;
            $!ctx-walker := CtxWalker.new(pseudo); # Don't waste memory, create for chained modes only
            $!stash-mode := nqp::getattr(pseudo, PseudoStash, '$!mode'); # Cache for faster access
            self
        }

        method new(PseudoStash:D \pseudo) { nqp::create(self)!SET-SELF(pseudo) }

        # Switch to the next parent context if necessary
        method maybe-next-context() {
                nqp::unless(
                    $!iter,
                    nqp::if(
                        $!ctx-walker.exhausted,
                        nqp::stmts(
                            ($!ctx := nqp::null()),
                        ),
                        nqp::stmts(
                            (my @ctx-info = $!ctx-walker.next-ctx),
                            ($!ctx := nqp::decont(@ctx-info[0])),
                            ($!ctx-mode = @ctx-info[1]),
                            ($!iter := nqp::iterator(nqp::ctxlexpad($!ctx)))
                        )
                    )
                )
        }

        # Like pull-one but doesn't return actual value. Skips non-dynamics in dynamic chains.
        method next-one() {
            my $got-one := 0;
            my $sym;
            nqp::while( # Repeat until got a candidate or no more contexts to iterate left
                (!nqp::defined($!seen) || ($!ctx && !$got-one)),
                nqp::stmts(
                    nqp::unless(nqp::defined($!seen), $!seen := nqp::hash()),
                    self.maybe-next-context,
                    nqp::if(
                        $!iter,
                        nqp::stmts(
                            nqp::shift($!iter),
                            # We have candidate if the chain is not dynamic; or if container under the symbol is
                            # dynamic.
                            ($sym := nqp::iterkey_s($!iter)),
                            # The symbol has to be dynamic if pseudo-package is marked as requiring dynamics or if
                            # we'recurrently iterating over the dynamic chain.
                            ($got-one := !nqp::atkey($!seen,$sym) && (
                                            ! (
                                                nqp::bitand_i($!stash-mode, REQUIRE_DYNAMIC)
                                                || $!ctx-mode == DYNAMIC_CHAIN
                                            )
                                            || (try { nqp::iterval($!iter).VAR.dynamic })
                                        ))
                        )
                    )
                )
            );
            nqp::bindkey($!seen,$sym,1) if $got-one;
            $got-one
        }
    }

    my class CtxSymIterator::Pairs does CtxSymIterator {
        method pull-one() is raw {
            nqp::if(
                self.next-one,
                Pair.new(nqp::iterkey_s($!iter), nqp::iterval($!iter)),
                IterationEnd
            )
        }
    }

    my class CtxSymIterator::Keys does CtxSymIterator {
        method pull-one() is raw {
            nqp::if(
                self.next-one,
                nqp::iterkey_s($!iter),
                IterationEnd
            )
        }
    }

    my class CtxSymIterator::Values does CtxSymIterator {
        method pull-one() is raw {
            nqp::if(
                self.next-one,
                nqp::iterval($!iter),
                IterationEnd
            )
        }
    }

    multi method iterator(PseudoStash:D: --> Iterator:D) { CtxSymIterator::Pairs.new(self) }

    multi method pairs(PseudoStash:D: --> Seq:D) {
        Seq.new(self.iterator)
    }

    multi method keys(PseudoStash:D: --> Seq:D) {
        Seq.new(CtxSymIterator::Keys.new(self))
    }

    multi method values(PseudoStash:D: --> Seq:D) {
        Seq.new(CtxSymIterator::Values.new(self))
    }

    method pseudo-package(PseudoStash:D: Str:D $name) is raw {
        nqp::setwho(
            ($!package := Metamodel::ModuleHOW.new_type(:$name)),
            self
        )
    }
}

# vim: expandtab shiftwidth=4
