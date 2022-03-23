# A distribution passed to `CURI.install()` will get encapsulated in this
# class, which normalizes the meta6 data and adds identifiers/content-id
class CompUnit::Repository::Distribution does Distribution {
    has Distribution $!dist handles <content prefix>;
    has $!meta;
    has $.repo;
    has $.dist-id;
    has $.repo-name;

    submethod TWEAK(|) {
        my $meta = $!dist.meta.hash;
        $meta<ver>  //= $meta<version>;
        $meta<auth> //= $meta<authority> // $meta<author>;
        $!meta = $meta;

        $!repo-name //= $!repo.name if nqp::can($!repo,"name") && $!repo.name;
        $!repo = $!repo.path-spec if $!repo.defined && $!repo !~~ Str;
    }

    submethod BUILD(:$!dist, :$!repo, :$!dist-id, :$!repo-name --> Nil) { }

    method new(Distribution $dist, *%_) {
        self.bless(:$dist, |%_)
    }

    method meta { $!meta }

    method Str() {
        return "{$.meta<name>}"
        ~ ":ver<{$.meta<ver>   // ''}>"
        ~ ":auth<{$.meta<auth> // ''}>"
        ~ ":api<{$.meta<api>   // ''}>";
    }

    method id() {
        return nqp::sha1(self.Str);
    }

    method from-precomp(CompUnit::Repository::Distribution:U: --> CompUnit::Repository::Distribution) {
        if %*ENV<RAKUDO_PRECOMP_DIST> -> \dist {
            my %data := Rakudo::Internals::JSON.from-json: dist;
            my $repo := %data<repo-name>
                ?? CompUnit::RepositoryRegistry.repository-for-name(%data<repo-name>)
                !! CompUnit::RepositoryRegistry.repository-for-spec(%data<repo>);
            my $dist := $repo.distribution(%data<dist-id>);
            self.new($dist, :repo(%data<repo>), :repo-name(%data<repo-name>), :dist-id(%data<dist-id>));
        }
        else {
            Nil
        }
    }

    method serialize() {
        Rakudo::Internals::JSON.to-json: {:$.repo, :$.repo-name, :$.dist-id}
    }

    method raku {
        self.^name ~ ".new({$!dist.raku}, repo => {$!repo.raku}, repo-name => {$!repo-name.raku})";
    }
}

# vim: expandtab shiftwidth=4
