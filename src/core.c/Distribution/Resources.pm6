role CompUnit::Repository { ... }

class Distribution::Resources does Associative {
    has Str $.dist-id;
    has Str $.repo;
    has Str $.repo-name;

    proto method BUILD(|) {*}

    multi method BUILD(:$!dist-id, CompUnit::Repository :$repo --> Nil) {
        unless $repo.can('name') and $!repo-name = $repo.name and $!repo-name ne '' {
            $!repo = $repo.path-spec;
            $!repo-name = Str;
        }
    }

    multi method BUILD(:$!dist-id, :$repo, Str :$!repo-name --> Nil) { }
    multi method BUILD(:$!dist-id, Str :$!repo, :$repo-name --> Nil) { }

    method from-precomp() {
        if %*ENV<RAKUDO_PRECOMP_DIST> -> \dist {
            my %data := Rakudo::Internals::JSON.from-json: dist;
            self.new(:repo(%data<repo>), :repo-name(%data<repo-name>), :dist-id(%data<dist-id>))
        }
        else {
            Nil
        }
    }

    method AT-KEY($key) {
        Distribution::Resource.new(:$.repo, :$.repo-name, :$.dist-id, :$key)
    }

    method Str() {
        Rakudo::Internals::JSON.to-json: {:$.repo, :$.repo-name, :$.dist-id}
    }
}

# vim: expandtab shiftwidth=4
