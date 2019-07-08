my class X::Bind { ... }
my class X::Caller::NotDynamic { ... }
my class X::NoSuchSymbol { ... }

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
            nqp::bindattr_i($stash, PseudoStash, '$!mode', STATIC_CHAIN +| DYNAMIC_CHAIN);
            $stash.pseudo-package('MY');
        },
        'CORE', sub ($cur) {
            my Mu $ctx := nqp::getattr(nqp::decont($cur), PseudoStash, '$!ctx');
            until nqp::isnull($ctx) || nqp::existskey(nqp::ctxlexpad($ctx), '!CORE_MARKER') {
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
                $stash.pseudo-package('CORE')
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
            nqp::bindattr_i($stash, PseudoStash, '$!mode', STATIC_CHAIN);
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
            nqp::bindattr_i($stash, PseudoStash, '$!mode', DYNAMIC_CHAIN);
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
                nqp::bindattr_i($stash, PseudoStash, '$!mode', PRECISE_SCOPE),
                $stash.pseudo-package('UNIT')
              )
            )
        },
        'SETTING', sub ($cur) {
            # Same as UNIT, but go a little further out (two steps, for
            # internals reasons).
            my Mu $ctx := nqp::getattr(nqp::decont($cur), PseudoStash, '$!ctx');
            until nqp::isnull($ctx) || nqp::existskey(nqp::ctxlexpad($ctx), '!UNIT_MARKER') {
                $ctx := nqp::ctxouterskipthunks($ctx);
            }
            nqp::if(
              nqp::isnull($ctx) || nqp::isnull($ctx := nqp::ctxouter(nqp::ctxouter($ctx))),
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
            $stash.pseudo-package('CLIENT');
        },
        'OUR', sub ($cur) {
            nqp::getlexrel(
                nqp::getattr(nqp::decont($cur), PseudoStash, '$!ctx'),
                '$?PACKAGE')
        }
    );

    multi method AT-KEY(PseudoStash:D: Str() $key) is raw {
        note("AT-KEY($key) on ", $!package.^name) if %*ENV<RAKUDO_DEBUG>;
        my Mu $val := nqp::null();
        nqp::if(
          nqp::existskey($pseudoers,$key),
          ($val := nqp::atkey($pseudoers,$key)(self)),
          nqp::stmts(
            nqp::if(          # PRECISE_SCOPE
              nqp::bitand_i($!mode,PRECISE_SCOPE),
              nqp::stmts(
                nqp::if(
                  nqp::existskey(
                    nqp::getattr(self,Map,'$!storage'),nqp::unbox_s($key)),
                  ($val := nqp::atkey(
                    nqp::getattr(self,Map,'$!storage'),nqp::unbox_s($key)))
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
            ),
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
        );
        note("AT-KEY RETURNS: ", (nqp::isnull($val) ?? "Failure" !! $val.^name), " // mode: ", $!mode.fmt('%x')) if %*ENV<RAKUDO_DEBUG>;
        nqp::isnull($val)
            ?? Failure.new(X::NoSuchSymbol.new(symbol => $!package.^name ~ '::<' ~ $key ~ '>'))
            !! $val
    }

    multi method ASSIGN-KEY(PseudoStash:D: Str() $key, Mu \value) is raw {
        self.AT-KEY($key) = value
    }

    # Finds the context in which $key is defined. Throws if not found.
    method lookup-ctx(Mu \ctx, Str $key) is raw {
        my Mu $ctx := ctx;
        my Mu $target := nqp::null();
        nqp::stmts(
          nqp::while(
              ($ctx && nqp::isnull($target)),
              nqp::if(
                  nqp::existskey($ctx,nqp::unbox_s($key)),
                  ($target := $ctx),
                  ($ctx := self.parent-ctx($ctx)),
              )
          ),
          nqp::ifnull(
              $target,
              X::NoSuchSymbol.new(symbol => $!package.^name ~ '::<' ~ $key ~ '>').throw
          )
        )
    }

    method BIND-KEY(PseudoStash:D: Str() $key, Mu \value) is raw {
        nqp::if(
          nqp::existskey($pseudoers,$key),
          X::Bind.new(target => "pseudo-package $key").throw,
          nqp::if(
            nqp::bitand_i($!mode,PRECISE_SCOPE),
            nqp::bindkey(
              nqp::getattr(self,Map,'$!storage'),nqp::unbox_s($key),value),
            nqp::if(
              nqp::bitand_i(
                $!mode,nqp::bitor_i(DYNAMIC_CHAIN,PICK_CHAIN_BY_NAME)
              ) && nqp::iseq_i(nqp::ord(nqp::unbox_s($key),1),42),  # "*"; TODO: Must validate by .VAR.dynamic
              nqp::stmts(
                (my Mu $target-ctx := self.lookup-ctx($!ctx,$key)),
                nqp::bindkey($target-ctx,nqp::unbox_s($key),value),
              ),
              nqp::bindkey(self.lookup-ctx($!ctx,$key),nqp::unbox_s($key),value),   # STATIC_CHAIN
            )
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

    method parent-ctx(PseudoStash:D: Mu \ctx) is raw {
        nqp::if(
            nqp::bitand_i($!mode,PRECISE_SCOPE),
            nqp::null(),
            nqp::if(
                nqp::bitand_i($!mode,nqp::bitor_i(DYNAMIC_CHAIN,PICK_CHAIN_BY_NAME)),
                nqp::ctxcallerskipthunks(ctx),
                nqp::ctxouterskipthunks(ctx)      # STATIC_CHAIN
            )
        )
    }

    # Iterate over context, return symbol => value Pairs.
    my role CtxIterator does Iterator {
        has PseudoStash $!stash;
        has Mu $!ctx;
        has $!iter;
        has $!seen;

        method !SET-SELF(PseudoStash:D \pseudo) {
            $!stash := pseudo;
            # When dealing with precise scope all needed symbols are already in $!storage. For chains we'd need the
            # context.
            # NOTE: Actually, the storage is our context at all times. Isn't it? * vrurg
            $!ctx := nqp::if(
                nqp::bitand_i(nqp::getattr(pseudo, PseudoStash, '$!mode'),PRECISE_SCOPE),
                nqp::getattr(pseudo, Map, '$!storage'),
                nqp::getattr(pseudo, PseudoStash, '$!ctx')
            );
            $!iter := nqp::iterator(nqp::ctxlexpad($!ctx));
            $!seen := nqp::hash();
            self
        }

        method new(PseudoStash:D \pseudo) { nqp::create(self)!SET-SELF(pseudo) }

        # Switch to the next parent context if necessary
        method maybe-next-context() {
            return unless $!ctx;
            nqp::unless(
                $!iter,
                nqp::if(
                    nqp::bitand_i(nqp::getattr($!stash, PseudoStash, '$!mode'), PRECISE_SCOPE),
                    # Reset current context manually for precise scope. Otherwise parent-ctx() would do it for us.
                    nqp::stmts(
                        # (note(" -> resetting context for PRECISE_SCOPE") if %*ENV<RAKUDO_DEBUG>),
                        ($!ctx := nqp::null()),
                    ),
                    nqp::stmts(
                        # (note(" -> iterating over parents") if %*ENV<RAKUDO_DEBUG>),
                        nqp::repeat_while(
                            ($!ctx && !nqp::elems($!ctx)), # Until context with symbols is found; or no parents left.
                            ($!ctx := $!stash.parent-ctx($!ctx)),
                        ),
                        nqp::if(
                            $!ctx,
                            # If we have a parent context then iterate over it.
                            ($!iter := nqp::iterator(nqp::ctxlexpad($!ctx))),
                        )
                    )
                )
            )
        }

        # Like pull-one but doesn't return actual value. Skips non-dynamics in dynamic chains.
        method next-one() is raw {
            note("next-one on ", nqp::getattr($!stash,PseudoStash,'$!package').^name) if %*ENV<RAKUDO_DEBUG>;
            my $got-one := 0;
            my $sym;
            nqp::while( # Repeat until got a candidate or no more contexts to iterate left
                ($!ctx && !$got-one),
                nqp::stmts(
                    (note(" -> maybe next context?") if %*ENV<RAKUDO_DEBUG>),
                    self.maybe-next-context,
                    (note(" -> has iter?") if %*ENV<RAKUDO_DEBUG>),
                    nqp::if(
                        $!iter,
                        nqp::stmts(
                            (note("   -> shift iter") if %*ENV<RAKUDO_DEBUG>),
                            nqp::shift($!iter),
                            # We have candidate if the chain is not dynamic; or if container under the symbol is
                            # dynamic.
                            ($sym := nqp::iterkey_s($!iter)),
                            (note("    -> $sym is the current symbol") if %*ENV<RAKUDO_DEBUG>),
                            ($got-one := !nqp::atkey($!seen,$sym) && (
                                            !nqp::bitand_i(nqp::getattr($!stash,PseudoStash,'$!mode'),REQUIRE_DYNAMIC) ||
                                            (try { nqp::iterval($!iter).VAR.dynamic })
                                        )),
                            (note("    -> accepted? ", $got-one ?? "YES" !! "NO") if %*ENV<RAKUDO_DEBUG>)
                        )
                    )
                )
            );
            nqp::bindkey($!seen,$sym,1) if $got-one;
            $got-one
        }
    }

    my class CtxIterator::Pairs does CtxIterator {
        method pull-one() is raw {
            nqp::if(
                self.next-one,
                Pair.new(nqp::iterkey_s($!iter), nqp::iterval($!iter)),
                IterationEnd
            )
        }
    }

    my class CtxIterator::Keys does CtxIterator {
        method pull-one() is raw {
            nqp::if(
                self.next-one,
                nqp::iterkey_s($!iter),
                IterationEnd
            )
        }
    }

    my class CtxIterator::Values does CtxIterator {
        method pull-one() is raw {
            nqp::if(
                self.next-one,
                nqp::stmts(
                    (note("VALUE FOR ", nqp::iterkey_s($!iter)) if %*ENV<RAKUDO_DEBUG>),
                    nqp::iterval($!iter),
                ),
                IterationEnd
            )
        }
    }

    multi method iterator(PseudoStash:D: --> Iterator:D) { CtxIterator::Pairs.new(self) }

    multi method pairs(PseudoStash:D: --> Seq:D) {
        Seq.new(self.iterator)
    }

    multi method keys(PseudoStash:D: --> Seq:D) {
        Seq.new(CtxIterator::Keys.new(self))
    }

    multi method values(PseudoStash:D: --> Seq:D) {
        Seq.new(CtxIterator::Values.new(self))
    }

    method pseudo-package(PseudoStash:D: Str:D $name) is raw {
        nqp::setwho(
            ($!package := Metamodel::ModuleHOW.new_type(:$name)),
            nqp::decont(self)
        )
    }
}

# vim: ft=perl6 expandtab sw=4
