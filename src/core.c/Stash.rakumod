my class Stash { # declared in BOOTSTRAP
    # class Stash is Hash
    #     has str $!longname;
    #     has $!lock;

    multi method new(Stash: --> Stash:D) {
        nqp::p6bindattrinvres(nqp::create(self), Stash, '$!lock', Lock.new)
    }

    method clone(Stash:D:) is raw {
        my $cloned := callsame();
        nqp::bindattr_s(
            $cloned, Stash, '$!longname',
            nqp::getattr_s(self, Stash, '$!longname'));
        nqp::bindattr($cloned, Stash, '$!lock', Lock.new);
        $cloned
    }

    multi method AT-KEY(Stash:D: Str:D $key) is raw {
        my \storage := nqp::getattr(self,Map,'$!storage');
        nqp::existskey(storage,$key)
          ?? nqp::atkey(storage,$key)
          !! nqp::p6scalarfromdesc(
               ContainerDescriptor::BindHashKey.new(Mu, self, $key)
             )
    }
    multi method AT-KEY(Stash:D: Str() $key, :$global_fallback!) is raw {
        my \storage := nqp::getattr(self,Map,'$!storage');
        nqp::if(
          nqp::existskey(storage,$key),
          nqp::atkey(storage,$key),
          nqp::if(
            $global_fallback,
            nqp::if(
              nqp::existskey(GLOBAL.WHO,$key),
              nqp::atkey(GLOBAL.WHO,$key),
              "Could not find symbol '$key' in '{self}'".Failure
            ),
            nqp::p6scalarfromdesc(
              ContainerDescriptor::BindHashKey.new(Mu, self, $key)
            )
          )
        )
    }

    method VIVIFY-KEY(Stash:D: $key) is raw is implementation-detail {
        self.BIND-KEY($key, (my str $sigil = nqp::substr($key,0,1)) eq '$'
          ?? (my $)
          !! $sigil eq '&'
            ?? (my &)
            !! $sigil eq '@'
              ?? []
              !! {}  # assume %
         ) unless self.EXISTS-KEY($key);
         self.AT-KEY($key)
    }

    # New proto is introduced here in order to cut off Hash candidates completely. There are few reasons to do so:
    # 1. Hash is not thread-safe whereas Stash claims to be
    # 2. Stash candidates are fully overriding their counterparts from Hash
    # 3. Minor: could result in some minor improvement in multi-dispatch lookups due to lesser number of candidates
    proto method ASSIGN-KEY(Stash:D: $, Mu $) {*}
    multi method ASSIGN-KEY(Stash:D: Str:D $key, Mu \assignval) is raw {
        my $storage := nqp::getattr(self,Map,'$!storage');
        my \existing-key := nqp::atkey($storage, $key);
        if nqp::isnull(existing-key) {
            $!lock.protect: {
                my \scalar := nqp::bindkey(
                    ($storage := nqp::clone($storage)),
                    $key,
                    nqp::p6assign(
                        nqp::p6bindattrinvres(
                            nqp::create(Scalar),
                            Scalar,
                            '$!descriptor',
                            nqp::getattr(self, Hash, '$!descriptor')),
                        assignval)
                );
#?if moar
                nqp::atomicbindattr(self, Map, '$!storage', $storage);
#?endif
#?if !moar
                # XXX Here and below: nqp::atomicbindattr works on JVM but still breaks building CORE.d. Guess, it is
                # not serializing properly, but a quick fix, similar to MoarVM's
                # https://github.com/MoarVM/MoarVM/commit/a9fcd5a74e8c530b4baa8fdc348b82a71bc0824d, where this problem
                # was observed too, didn't fix the situation. So, let's stick to the unsafe path for now.
                nqp::bindattr(self, Map, '$!storage', $storage);
#?endif
                scalar
            };
        }
        else {
            nqp::p6assign(existing-key, assignval);
        }
    }
    multi method ASSIGN-KEY(Stash:D: \key, Mu \assignval) is raw {
        nextwith(key.Str, assignval)
    }

    # See the comment for ASSIGN-KEY on proto.
    proto method BIND-KEY(|) {*}
    multi method BIND-KEY(Stash:D: Str:D $key, Mu \bindval) is raw {
        $!lock.protect: {
            my $storage := nqp::clone(nqp::getattr(self,Map,'$!storage'));
            nqp::bindkey($storage, $key, bindval);
#?if moar
                nqp::atomicbindattr(self, Map, '$!storage', $storage);
#?endif
#?if !moar
                nqp::bindattr(self, Map, '$!storage', $storage);
#?endif
        }
        bindval
    }
    multi method BIND-KEY(Stash:D: \key, Mu \bindval) is raw {
        nextwith(key.Str, bindval)
    }

    method package_at_key(Stash:D: str $key) {
        my $storage := nqp::getattr(self,Map,'$!storage');
        nqp::ifnull(
          nqp::atkey($storage,$key),
          $!lock.protect({
            my $pkg := Metamodel::PackageHOW.new_type(:name("{$!longname}::$key"));
            $pkg.^compose;
            $storage := nqp::clone($storage);
            nqp::bindkey($storage,$key,$pkg);
#?if moar
                nqp::atomicbindattr(self, Map, '$!storage', $storage);
#?endif
#?if !moar
                nqp::bindattr(self, Map, '$!storage', $storage);
#?endif
            $pkg
          })
        )
    }

    multi method gist(Stash:D:) {
        self.Str
    }

    multi method Str(Stash:D:) {
        nqp::isnull_s($!longname) ?? '<anon>' !! $!longname
    }

    method merge-symbols(Stash:D: Mu \globalish) { # NQP gives a Hash, not a Stash
        if nqp::defined(globalish) {
            $!lock.protect: {
                # For thread safety, ModuleLoader's merge_globals is calling this method when its target is a Stash.
                # Therefore we call it on cloned symbol hash. This prevents recursion and works slightly faster.
                nqp::gethllsym('Raku','ModuleLoader').merge_globals(
                    (my $storage := nqp::clone(my $old-storage := nqp::getattr(self,Map,'$!storage'))),
                    globalish
                );
#?if moar
                nqp::atomicbindattr(self, Map, '$!storage', $storage);
#?endif
#?if !moar
                nqp::bindattr(self, Map, '$!storage', $storage);
#?endif
            }
        }
    }
}

# vim: expandtab shiftwidth=4
