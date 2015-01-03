role CompUnitRepo::Locally {
    has Lock    $!lock;
    has IO::Dir $.IO;
    has Str     $.WHICH;

    my %instances;

    method new(CompUnitRepo::Locally: $dir as Str) {
        my $IO := CHANGE-DIRECTORY($dir,$*CWD.Str,&FILETEST-R);
        return Nil unless $IO;

        %instances{$IO.abspath} //=
          self.bless(:$IO,:lock(Lock.new),:WHICH(self.^name ~ '|' ~ $IO.abspath));
    }

    multi method Str(CompUnitRepo::Locally:D:) { $!IO.abspath }
    multi method gist(CompUnitRepo::Locally:D:) {
        "{self.short-id}:$!IO.abspath()";
    }
    multi method perl(CompUnitRepo::Locally:D:) {
        $?CLASS.^name ~ ".new('$!IO.abspath()')";
    }

    method path(CompUnitRepo::Locally:D:) {
        DEPRECATED('IO', |<2014.11 2015.11>);
        $!IO;
    }

    multi method WHICH(CompUnitRepo::Locally:D:) { $!WHICH }

    method path-spec(CompUnitRepo::Locally:D:) {
        self.short-id ~ ':' ~ $!IO.abspath;
    }

    # stubs
    method install(CompUnitRepo::Locally:D: $source, $from? )             {...}
    method files(CompUnitRepo::Locally:D: $file, :$name, :$auth, :$ver)   {...}
    method candidates(CompUnitRepo::Locally:D: $name,:$file,:$auth,:$ver) {...}
    method short-id(CompUnitRepo::Locally:D:)                             {...}
}
