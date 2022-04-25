role CompUnit::Repository::Locally {
    has IO::Path   $.prefix    is built(:bind);
    has Str        $.abspath   is built(False);
    has ValueObjAt $.WHICH     is built(False);
    has Str        $.path-spec is built(False);

    my $instances := nqp::hash;  # cache with instances, keyed on WHICH
    my $lock      := Lock.new;   # serializing access to instances hash

    # handle a new object that wasn't cached before
    method !SET-SELF(Str:D $abspath, str $WHICH) {
        $!abspath   := $abspath;
        $!WHICH     := ValueObjAt.new($WHICH);
        $!path-spec := self.short-id ~ '#' ~ $abspath;

        nqp::bindkey($instances,$WHICH,self)
    }

    # CompUnit::Repository::Locally objects are special in that there
    # can only be one for each combination of class and directory that
    # they consider their work space.  So any parameters passed apart
    # from the "prefix" parameter, will be *ignored* any subsequent
    # attempt at creating an object of that type on that prefix.
    method new(CompUnit::Repository::Locally: Any:D :$prefix) {
        my $abspath = nqp::istype($prefix,IO::Path)
          ?? $prefix.absolute
          !! $*SPEC.rel2abs($prefix.Str);
        my str $WHICH = self.^name ~ '|' ~ $abspath;

        $lock.protect: {
            nqp::ifnull(
              nqp::atkey($instances,$WHICH),
              self.bless(
                :prefix(
                  nqp::istype($prefix,IO::Path) ?? $prefix !! $abspath.IO
                ), |%_
              )!SET-SELF($abspath, $WHICH)
            )
        }
    }

    multi method WHICH(CompUnit::Repository::Locally:D: --> ValueObjAt:D) {
        $!WHICH
    }
    multi method Str(CompUnit::Repository::Locally:D: --> Str:D) {
        $!abspath
    }
    multi method gist(CompUnit::Repository::Locally:D: --> Str:D) {
        $!path-spec
    }
    multi method raku(CompUnit::Repository::Locally:D: --> Str:D) {
        $?CLASS.^name ~ '.new(prefix => ' ~ $!abspath.raku ~ ')';
    }
    method source-file(Str:D $name --> IO::Path:D) {
        $!prefix.add($name)
    }

    method id(--> Str:D) {
        nqp::sha1(self.next-repo
          ?? $!path-spec ~ ',' ~ self.next-repo.id
          !! $!path-spec
        )
    }

    # stubs
    method short-id(CompUnit::Repository::Locally:D: --> Str:D) { ... }
}

# vim: expandtab shiftwidth=4
