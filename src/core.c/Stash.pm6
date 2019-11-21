my class Stash { # declared in BOOTSTRAP
    # class Stash is Hash
    #     has str $!longname;

    multi method AT-KEY(Stash:D: Str:D $key) is raw {
        my \storage := nqp::getattr(self,Map,'$!storage');
        nqp::if(
          nqp::existskey(storage,$key),
          nqp::atkey(storage,$key),
          nqp::p6scalarfromdesc(
            ContainerDescriptor::BindHashPos.new(Mu, self, $key)
          )
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
              Failure.new("Could not find symbol '$key'")
            ),
            nqp::p6scalarfromdesc(
              ContainerDescriptor::BindHashPos.new(Mu, self, $key)
            )
          )
        )
    }

    method package_at_key(Stash:D: str $key) {
        my \storage := nqp::getattr(self,Map,'$!storage');
        nqp::ifnull(
          nqp::atkey(storage,$key),
          nqp::stmts(
            (my $pkg := Metamodel::PackageHOW.new_type(:name($key))),
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
        nqp::gethllsym('perl6','ModuleLoader').merge_globals(self,$globalish)
          if $globalish.defined;
    }

    # SNAPSHOT methods are only to be used in conjunction with serialization/deserialization.
    method !BUILD-SNAPSHOT(Stash:D: Stash:D \stash, $schandle, :$level = 0) is raw {
        my $pfx := "  " x $level;
        note($pfx, "BUILDING SNAPSHOT OF ", $schandle) if %*ENV<RAKUDO_DEBUG>;
        my \storage := nqp::getattr(stash, Map, '$!storage');
        my \iter := nqp::iterator(storage);
        my $table := nqp::list();
        note($pfx, "iter: ", nqp::istrue(iter)) if %*ENV<RAKUDO_DEBUG>;
        while iter {
            nqp::shift(iter);
            my \sym := nqp::iterkey_s(iter);
            my \val := nqp::iterval(iter);
            note($pfx, "CONSIDERING ", sym) if %*ENV<RAKUDO_DEBUG>;
            # Item is a list of 2 or 3 elements: symbol name, value, and subtable when the value is a package type.
            my $subtable;
            if !nqp::isconcrete(val) && nqp::istype(val.HOW, Metamodel::PackageHOW) {
                $subtable := self!BUILD-SNAPSHOT(val.WHO, $schandle, level => ($level + 1));
            }
                note($pfx, " -> adding as a ", ($subtable && $subtable.elems ?? "subtable" !! "leaf")) if %*ENV<RAKUDO_DEBUG>;
                nqp::push(
                    $table,
                    nqp::if(
                        ($subtable && $subtable.elems),
                        nqp::list(sym, val, $subtable),
                        nqp::list(sym, val),
                    )
                );
        }
        $table
    }

    method TAKE-SNAPSHOT(Stash:D: $schandle) {
        note("TAKING SNAPSHOT OF ", $!longname, " -- ", $schandle) if %*ENV<RAKUDO_DEBUG>;
        nqp::bindattr(self, Stash, '$!snapshot', self!BUILD-SNAPSHOT(self, $schandle));
    }

    method !MERGE-SNAPSHOT(Stash:D: \stash, @table, :$level = 0) {
        my $pfx = "  " x $level;
        my \storage := nqp::getattr(stash, Map, '$!storage');
        for @table -> @item {
            my \sym := @item[0];
            my \val := @item[1];
            note($pfx, ": ", sym) if %*ENV<RAKUDO_DEBUG>;
            unless nqp::existskey(storage, sym) && nqp::atkey(storage, sym) =:= val {
                note($pfx, "WILL STORE ", sym) if %*ENV<RAKUDO_DEBUG>;
                nqp::bindkey(storage, sym, val);
            }
            if @item > 2 {
                note($pfx, "SUBTABLE OF ", sym) if %*ENV<RAKUDO_DEBUG>;
                self!MERGE-SNAPSHOT(val.WHO, @item[2], level => ($level + 1));
            }
        }
    }

    method RESTORE-SNAPSHOT(Stash:D:) {
        note("RESTORING SNAPSHOT") if %*ENV<RAKUDO_DEBUG>;
        self!MERGE-SNAPSHOT(self, nqp::getattr(self, Stash, '$!snapshot'));
        # Don't keep the old records when we don't need them anymore.
        nqp::bindattr(self, Stash, '$!snapshot', Nil);
    }
}

# vim: ft=perl6 expandtab sw=4
