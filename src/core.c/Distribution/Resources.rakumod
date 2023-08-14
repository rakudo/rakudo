role CompUnit::Repository { ... }

class Distribution::Resources does Associative {
    has Str $.dist-id   is built(False);
    has Str $.repo      is built(False);
    has Str $.repo-name is built(False);

    proto method BUILD(|) {*}

    multi method BUILD(:$!dist-id, CompUnit::Repository :$repo --> Nil) {
        unless $repo.can('name') and $!repo-name = $repo.name and $!repo-name ne '' {
            $!repo = $repo.path-spec;
            $!repo-name = Str;
        }
    }
    multi method BUILD(:$!dist-id, :$repo, Str :$!repo-name --> Nil) { }
    multi method BUILD(:$!dist-id, Str :$!repo, :$repo-name --> Nil) { }

    # Alternate instantiator called from Actions.nqp during compilation
    # of %?RESOURCES
    method from-precomp(Distribution::Resources:U:) is implementation-detail {
        if %*ENV<RAKUDO_PRECOMP_DIST> -> $dist {
            my %data := Rakudo::Internals::JSON.from-json: $dist;
            self.new:
              :repo(%data<repo>),
              :repo-name(%data<repo-name>),
              :dist-id(%data<dist-id>);
        }
        else {
            Nil
        }
    }

    multi method AT-KEY(Distribution::Resources:D: $key) {
        Distribution::Resource.new(:$.repo, :$.repo-name, :$.dist-id, :$key)
    }

    multi method Str(Distribution::Resources:D:) {
        Rakudo::Internals::JSON.to-json: {:$!repo, :$!repo-name, :$!dist-id}
    }

    # More sensible error messages if %?RESOURCES are trying to be changed
    multi method ASSIGN-KEY(Distribution::Resources:D: $key, Mu) {
        die "Cannot assign to key '$key' in an immutable " ~ self.^name;
    }
    multi method BIND-KEY(Distribution::Resources:D: $key, Mu) {
        die "Cannot bind to key '$key' in an immutable " ~ self.^name;
    }
    multi method DELETE-KEY(Distribution::Resources:D: $key) {
        die "Cannot remove key '$key' in an immutable " ~ self.^name;
    }
}

# vim: expandtab shiftwidth=4
