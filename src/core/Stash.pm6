my class Stash { # declared in BOOTSTRAP
    # class Stash is Hash
    #     has str $!longname;

    multi method AT-KEY(Stash:D: Str:D $key) is raw {
        nqp::if(
          nqp::getattr(self,Map,'$!storage')
            && nqp::existskey(nqp::getattr(self,Map,'$!storage'),$key),
          nqp::atkey(nqp::getattr(self,Map,'$!storage'),$key),
          nqp::p6scalarfromdesc(ContainerDescriptor::BindHashPos.new(Mu, self, $key))
        )
    }
    multi method AT-KEY(Stash:D: Str() $key, :$global_fallback!) is raw {
        nqp::if(
          nqp::getattr(self,Map,'$!storage')
            && nqp::existskey(nqp::getattr(self,Map,'$!storage'),$key),
          nqp::atkey(nqp::getattr(self,Map,'$!storage'),$key),
          nqp::if(
            $global_fallback,
            nqp::if(
              nqp::existskey(GLOBAL.WHO,$key),
              nqp::atkey(GLOBAL.WHO,$key),
              Failure.new("Could not find symbol '$key'")
            ),
            nqp::p6scalarfromdesc(ContainerDescriptor::BindHashPos.new(Mu, self, $key))
          )
        )
    }

    method package_at_key(Stash:D: str $key) {
        my Mu $storage := nqp::defined(nqp::getattr(self, Map, '$!storage')) ??
            nqp::getattr(self, Map, '$!storage') !!
            nqp::bindattr(self, Map, '$!storage', nqp::hash());
        if nqp::existskey($storage, nqp::unbox_s($key)) {
            nqp::atkey($storage, $key)
        }
        else {
            my $pkg := Metamodel::PackageHOW.new_type(:name($key));
            $pkg.^compose;
            nqp::bindkey($storage, $key, $pkg)
        }
    }

    multi method gist(Stash:D:) {
        self.Str
    }

    multi method Str(Stash:D:) {
        nqp::isnull_s($!longname) ?? '<anon>' !! $!longname
    }

    method merge-symbols(Stash:D: Hash $globalish) { # NQP gives a Hash, not a Stash
        nqp::gethllsym('perl6','ModuleLoader').merge_globals(self,$globalish)
          if $globalish.defined;
    }
}

# vim: ft=perl6 expandtab sw=4
