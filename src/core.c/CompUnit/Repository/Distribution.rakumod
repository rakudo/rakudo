# A distribution passed to `CURI.install()` will get encapsulated in this
# class, which normalizes the meta6 data and adds identifiers/content-id
class CompUnit::Repository::Distribution does Distribution {
    has Distribution $.dist      is built(:bind) handles <content prefix>;
    has              $.repo      is built(:bind);
    has              $.dist-id   is built(:bind);
    has              $.repo-name is built(:bind);
    has              %.meta      is built(False);

    method TWEAK(--> Nil) {
        my %meta := $!dist.meta.hash;
        %meta<ver>  //= %meta<version> // '';
        %meta<auth> //= %meta<authority> // %meta<author> // '';
        %meta<api>  //= '';
        %!meta := %meta;

        $!repo-name := $!repo.name
          if nqp::not_i(nqp::isconcrete($!repo-name))
          && nqp::can($!repo,"name");
        $!repo := $!repo.path-spec
          if nqp::isconcrete($!repo)
          && nqp::not_i(nqp::istype($!repo,Str));
    }

    method new(Distribution:D $dist
    --> CompUnit::Repository::Distribution:D) {
        self.bless(:$dist, |%_)
    }

    method id(--> Str:D) { nqp::sha1(self.Str) }
    method meta(CompUnit::Repository::Distribution:D:) { %!meta.item }

    # Alternate instantiator called from Actions.nqp during compilation
    # of $?DISTRIBUTION
    method from-precomp(CompUnit::Repository::Distribution:U:
    --> CompUnit::Repository::Distribution:D) is implementation-detail {
        if %*ENV<RAKUDO_PRECOMP_DIST> -> $json {
            my %data := Rakudo::Internals::JSON.from-json: $json;
            my $name := %data<repo-name>;
            my $spec := %data<repo>;  # XXX badly named field?
            my $id   := %data<dist-id>;

            my $repo := $name
              ?? CompUnit::RepositoryRegistry.repository-for-name($name)
              !! CompUnit::RepositoryRegistry.repository-for-spec($spec);
            self.bless:
              :dist($repo.distribution($id)),
              :repo($spec),
              :repo-name($name),
              :dist-id($id);
        }
        else {
            Nil
        }
    }

    method serialize(--> Str:D) is implementation-detail {
        Rakudo::Internals::JSON.to-json: {:$.repo, :$.repo-name, :$.dist-id}
    }

    multi method Str(CompUnit::Repository::Distribution:D:--> Str:D) {
        "%!meta<name>:ver<%!meta<ver>>:auth<%!meta<auth>>:api<%!meta<api>>"
    }
    multi method raku(CompUnit::Repository::Distribution:D:--> Str:D) {
        self.^name ~ ".new($!dist.raku(), repo => $!repo.raku(), repo-name => $!repo-name.raku())"
    }
}

# vim: expandtab shiftwidth=4
