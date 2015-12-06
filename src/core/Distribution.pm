class Distribution {
    has $.name;
    has $.auth;
    has $.author;
    has $.authority;
    has $.api;
    has $.ver;
    has $.version;
    has $.description;
    has @.depends;
    has %.provides;
    has %.files;
    has $.source-url;
    method auth { $!auth // $!author // $!authority }
    method ver  { $!ver // $!version }
    method hash {
        {
            :$!name,
            :$.auth,
            :$.ver,
            :$!description,
            :@!depends,
            :%!provides,
            :%!files,
            :$!source-url,
        }
    }
    method Str() {
        return "{$.name}:ver<{$.ver  // ''}>:auth<{$.auth // ''}>:api<{$.api // ''}>";
    }
    method id() {
        return nqp::sha1(self.Str);
    }
}

# during CURLI migration period
class CompUnitRepo::Distribution is Distribution {
    method Hash { self.hash }
}

role CompUnit::Repository { ... }
class CompUnitRepo { ... }
class Distribution::Resources does Associative {
    has Str $.dist-id;
    has Str $.repo;

    proto method BUILD(|) { * }

    multi method BUILD(:$!dist-id, CompUnit::Repository :$repo) {
        $!repo = $repo.path-spec;
    }

    multi method BUILD(:$!dist-id, Str :$!repo) {
    }

    method from-precomp() {
        return unless %*PRECOMP-DIST;
        self.new(:repo($*PRECOMP-DIST<repo>), :dist-id($*PRECOMP-DIST<dist-id>))
    }

    method AT-KEY($key) {
        CompUnitRepo.new($.repo).resource($.dist-id, $key)
    }

    method Str() {
        to-json {repo => $.repo.Str, dist-id => $.dist-id};
    }
}

# vim: ft=perl6 expandtab sw=4
