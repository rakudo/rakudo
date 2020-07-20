# API to obtain the data of any addressable content
role Distribution {
    # `meta` provides an API to the meta data in META6 spec (s22)
    #   -   A Distribution may be represented internally by some other
    #       spec (such as using the file system itself for prereqs), as
    #       long as it can also be represented as the META6 hash format
    method meta(--> Hash) { ... }

    # `content($content-id)` provides an API to the data itself
    #   -   Use `.meta` to determine the $address of a specific $content-id
    #   -   IO::Handle is meant to be a data stream that may or may not be available; for now
    #       it would return an IO::Handle and have `.open(:bin).slurp` called on it. So if
    #       a socket wants to handle this role currently it would have to wrap `open` or `.slurp`
    #       to handle any protocol negotiation as well as probably saving the data to a tmpfile and
    #       return an IO::Handle to that

    method content($content-id --> IO::Handle) { ... }
}

role Distribution::Locally does Distribution {
    has IO::Path $.prefix;
    method content($address) {
        my $path   = IO::Path.new($.meta<files>{$address} // $address, :CWD($!prefix.absolute // $*CWD.absolute));
        my $handle = IO::Handle.new(:$path);
        $handle // $handle.throw;
    }
}

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

class Distribution::Hash does Distribution::Locally {
    has $!meta;
    submethod BUILD(:$!meta, :$!prefix --> Nil) { }
    method new($hash, :$prefix) { self.bless(:meta($hash), :$prefix) }
    method meta { $!meta }
    method raku {
        self.^name ~ ".new({$!meta.raku}, prefix => {$!prefix.raku})";
    }
}

class Distribution::Path does Distribution::Locally {
    has $!meta;
    has $!meta-file;
    submethod BUILD(:$!meta, :$!prefix, :$!meta-file --> Nil) { }
    method new(IO::Path $prefix, IO::Path :$meta-file = IO::Path) {
        my $meta-path = $meta-file // $prefix.add('META6.json');
        die "No meta file located at {$meta-path.path}" unless $meta-path.e;
        my $meta = Rakudo::Internals::JSON.from-json($meta-path.slurp);

        # generate `files` (special directories) directly from the file system
        my %bins = Rakudo::Internals.DIR-RECURSE($prefix.add('bin').absolute).map(*.IO).map: -> $real-path {
            my $name-path = $real-path.is-relative
                ?? $real-path
                !! $real-path.relative($prefix);
            $name-path.subst(:g, '\\', '/') => $name-path.subst(:g, '\\', '/')
        }

        my $resources-dir = $prefix.add('resources');
        my %resources = $meta<resources>.grep(*.?chars).map(*.IO).map: -> $path {
            my $real-path = $path ~~ m/^libraries\/(.*)/
                ?? $resources-dir.add('libraries').add( $*VM.platform-library-name($0.Str.IO) )
                !! $resources-dir.add($path);
            my $name-path = $path.is-relative
                ?? "resources/{$path}"
                !! "resources/{$path.relative($prefix)}";
            $name-path.subst(:g, '\\', '/') => $real-path.relative($prefix).subst(:g, '\\', '/')
        }

        $meta<files> = Hash.new(%bins, %resources);

        self.bless(:$meta, :$prefix, :$meta-file);
    }
    method meta { $!meta }
    method raku {
       self.^name ~ ".new({$!prefix.raku}, meta-file => {$!meta-file.raku})";
    }
}

role CompUnit::Repository { ... }

class Distribution::Resource {
    has $.repo;
    has $.repo-name;
    has $.dist-id;
    has $.key;

    method IO() {
        my $repo := self.repo-name
            ?? CompUnit::RepositoryRegistry.repository-for-name(self.repo-name)
            !! CompUnit::RepositoryRegistry.repository-for-spec(self.repo);
        $repo.resource(self.dist-id, "resources/$.key")
    }

    method platform-library-name() {
        my $library = self.IO;
        ($library ~~ /\.<.alpha>+$/ or $library ~~ /\.so(\.<.digit>+)+$/) #Already a full name?
            ??  $library
            !!  $*VM.platform-library-name($library)
    }

    # delegate appropriate IO::Path methods to the resource IO::Path object
    multi method Str(::?CLASS:D: |c) {
        self.IO.Str(|c)
    }
    multi method gist(::?CLASS:D: |c) {
        self.IO.gist(|c)
    }
    multi method raku(::?CLASS:D: |c) {
        self.IO.raku(|c)
    }
    method absolute(|c) {
        self.IO.absolute(|c)
    }
    method is-absolute(|c) {
        self.IO.is-absolute(|c)
    }
    method relative(|c) {
        self.IO.relative(|c)
    }
    method is-relative(|c) {
        self.IO.is-relative(|c)
    }
    method parts(|c) {
        self.IO.parts(|c)
    }
    method volume(|c) {
        self.IO.volume(|c)
    }
    method dirname(|c) {
        self.IO.dirname(|c)
    }
    method basename(|c) {
        self.IO.basename(|c)
    }
    method extension(|c) {
        self.IO.extension(|c)
    }
    method open(|c) {
        self.IO.open(|c)
    }
    method resolve(|c) {
        self.IO.resolve(|c)
    }
    method slurp(|c) {
        self.IO.slurp(|c)
    }
    method lines(|c) {
        self.IO.lines(|c)
    }
    method comb(|c) {
        self.IO.comb(|c)
    }
    method split(|c) {
        self.IO.split(|c)
    }
    method words(|c) {
        self.IO.words(|c)
    }
    method copy(|c) {
        self.IO.copy(|c)
    }
}

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
