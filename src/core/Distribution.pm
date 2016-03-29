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
    #       it would return an IO::Handle and have `.open.slurp-rest(:bin)` called on it. So if
    #       a socket wants to handle this role currently it would have to wrap `open` or `.slurp-rest`
    #       to handle any protocol negotiation as well as probably saving the data to a tmpfile and
    #       return an IO::Handle to that
    method content($content-id --> IO::Handle) { ... }
}

role Distribution::Locally {
    has IO::Path $.prefix;
    method content($address) {
        my $handle = IO::Handle.new: path => IO::Path.new($address, :CWD($!prefix // $*CWD));
        $handle // $handle.throw;
    }
}

# A distribution passed to `CURI.install()` will get encapsulated in this
# class, which normalizes the meta6 data and adds identifiers/content-id
class CompUnit::Repository::Distribution {
    has Distribution $!dist handles 'content';
    has $!meta;
    submethod BUILD(:$!meta, :$!dist) { }
    method new(Distribution $dist) {
        my $meta = $dist.meta.hash;
        $meta<ver>  //= $meta<version>;
        $meta<auth> //= $meta<authority> // $meta<author>;
        self.bless(:$dist, :$meta);
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
}

class Distribution::Hash does Distribution does Distribution::Locally {
    has $!meta;
    submethod BUILD(:$!meta, :$!prefix) { }
    method new($hash, :$prefix) { self.bless(:meta($hash), :$prefix) }
    method meta { $!meta }
}

class Distribution::Path does Distribution does Distribution::Locally {
    has $!meta;
    submethod BUILD(:$!meta, :$!prefix) { }
    method new(IO::Path $prefix, IO::Path :$file is copy) {
        my $path := ?$file ?? $file !! $prefix.child('META6.json');
        die "No meta file located at {$path.abspath}" unless $path.e;
        my $meta = from-json(slurp($path));

        my sub ls-files($prefix, $subdir) {
            my @stack = dir($prefix.child($subdir));
            my @files = eager gather while ( @stack ) {
                my IO::Path $current = @stack.pop;
                my Str      $relpath = $current.relative($prefix);
                take $relpath and next if $current.f;
                next if $current.basename eq '.precomp';
                @stack.append( |dir($current.absolute) ) if $current.d;
            }
        }

        # generate `files` (special directories) directly from the file system
        my @bins      = ls-files($prefix, 'bin');
        my @resources = ls-files($prefix, 'resources');
        my @files     = grep *.defined, unique(|$meta<files>, |@bins, |@resources);
        $meta<files>  = @files;

        self.bless(:$meta, :$prefix);
    }
    method meta { $!meta }
}

role CompUnit::Repository { ... }
class Distribution::Resources does Associative {
    has Str $.dist-id;
    has Str $.repo;

    proto method BUILD(|) { * }

    multi method BUILD(:$!dist-id, CompUnit::Repository :$repo --> Nil) {
        $!repo = $repo.path-spec;
    }

    multi method BUILD(:$!dist-id, Str :$!repo --> Nil) { }

    method from-precomp() {
        if %*ENV<RAKUDO_PRECOMP_DIST> -> \dist {
            my %data := from-json dist;
            self.new(:repo(%data<repo>), :dist-id(%data<dist-id>))
        }
        else {
            Nil
        }
    }

    method AT-KEY($key) {
        CompUnit::RepositoryRegistry.repository-for-spec($.repo).resource($.dist-id, "resources/$key")
    }

    method Str() {
        to-json {repo => $.repo.Str, dist-id => $.dist-id};
    }
}

# vim: ft=perl6 expandtab sw=4
