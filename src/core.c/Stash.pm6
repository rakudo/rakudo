my class Stash { # declared in BOOTSTRAP
    # class Stash is Hash
    #     has str $!longname;
    #     has $!lock;

    multi method new(Stash: --> Map:D) {
        nqp::p6bindattrinvres(nqp::create(self), Stash, '$!lock', Lock.new)
    }

    method clone(Stash:D:) is raw {
        nqp::p6bindattrinvres(
            nqp::p6bindattrinvres(
                callsame(),
                Stash, '$!longname', nqp::getattr(self, Stash, '$!longname')),
            Stash, '$!lock', Lock.new)
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

    proto method ASSIGN-KEY(Stash:D: $, $) {*}
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
                nqp::atomicbindattr(self, Map, '$!storage', $storage);
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

    proto method BIND-KEY(|) {*}
    multi method BIND-KEY(Stash:D: Str:D $key, Mu \bindval) is raw {
        $!lock.protect: {
            my $storage := nqp::clone(nqp::getattr(self,Map,'$!storage'));
            nqp::bindkey($storage, $key, bindval);
            nqp::atomicbindattr(self, Map, '$!storage', $storage);
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
            nqp::atomicbindattr(self, Map, '$!storage', $storage);
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
                # XXX We cannot use atomic bind for now because it breaks MoarVM serialization.
                # nqp::atomicbindattr(self, Map, '$!storage', $storage);
                nqp::bindattr(self, Map, '$!storage', $storage);
            }
        }
    }
}

# vim: expandtab shiftwidth=4
