my class Stash { # declared in BOOTSTRAP
    # class Stash is Hash
    #     has str $!longname;
    #     has $!lock;

    multi method new(Stash: --> Map:D) {
        nqp::p6bindattrinvres(nqp::create(self), Stash, '$!lock', Lock.new)
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
                nqp::bindattr(self, Map, '$!storage', $storage);
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

    method package_at_key(Stash:D: str $key) {
        my \storage := nqp::getattr(self,Map,'$!storage');
        nqp::ifnull(
          nqp::atkey(storage,$key),
          nqp::stmts(
            (my $pkg := Metamodel::PackageHOW.new_type(:name("{$!longname}::$key"))),
            $pkg.^compose,
            nqp::bindkey(storage,$key,$pkg)
          )
        )
    }

    multi method gist(Stash:D:) {
        self.Str
    }

    multi method Str(Stash:D:) {
        nqp::isnull_s($!longname) ?? '<anon>' !! $!longname
    }

    method merge-symbols(Stash:D: Hash $globalish) { # NQP gives a Hash, not a Stash
        nqp::gethllsym('Raku','ModuleLoader').merge_globals(self,$globalish)
          if $globalish.defined;
    }
}

# vim: expandtab shiftwidth=4
