# API to obtain the data of any addressable content
role Distribution { ... }

role Distribution {
    # `meta` provides an API to the meta data in META6 spec (s22)
    #   -   A Distribution may be represented internally by some other
    #       spec (such as using the file system itself for prereqs), as
    #       long as it can also be represented as the META6 hash format
    method meta(--> Hash:D) {
        # Cannot just use ... here as that would break legacy code
        my $class-name = ::?CLASS.^name;

        die $class-name eq 'Distribution'
            ?? 'Legacy Distribution object used in code expecting an object consuming the Distribution role'
            !! "Method 'meta' must be implemented by $class-name because it is required by role Distribution"
    }

    # `content($content-id)` provides an API to the data itself
    #   -   Use `.meta` to determine the $address of a specific $content-id
    #   -   IO::Handle is meant to be a data stream that may or may not be available; for now
    #       it would return an IO::Handle and have `.open.slurp-rest(:bin)` called on it. So if
    #       a socket wants to handle this role currently it would have to wrap `open` or `.slurp-rest`
    #       to handle any protocol negotiation as well as probably saving the data to a tmpfile and
    #       return an IO::Handle to that
    method content($content-id --> IO::Handle:D) {
        # Cannot just use ... here as that would break legacy code
        my $class-name = ::?CLASS.^name;

        die $class-name eq 'Distribution'
            ?? 'Legacy Distribution object used in code expecting an object consuming the Distribution role'
            !! "Method 'content' must be implemented by $class-name because it is required by role Distribution"
    }

    # Backwards compatibility shim
    submethod new(*%_) {
        ::?CLASS.^name eq 'Distribution'
            ?? class :: {
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
                method meta(--> Hash:D) {
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
                    return "{$.meta<name>}"
                    ~ ":ver<{$.meta<ver>   // ''}>"
                    ~ ":auth<{$.meta<auth> // ''}>"
                    ~ ":api<{$.meta<api>   // ''}>";

                }
                method content($content-id --> IO::Handle:D) { }
            }.new(|%_)
            !! self.bless(|%_)
    }
}

role Distribution::Locally does Distribution {
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
    submethod BUILD(:$!meta, :$!dist --> Nil) { }
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

class Distribution::Hash does Distribution::Locally {
    has $!meta;
    submethod BUILD(:$!meta, :$!prefix --> Nil) { }
    method new($hash, :$prefix) { self.bless(:meta($hash), :$prefix) }
    method meta { $!meta }
}

class Distribution::Path does Distribution::Locally {
    has $!meta;
    submethod BUILD(:$!meta, :$!prefix --> Nil) { }
    method new(IO::Path $prefix, IO::Path :$meta-file is copy) {
        $meta-file //= $prefix.add('META6.json');
        die "No meta file located at {$meta-file.path}" unless $meta-file.e;
        my $meta = Rakudo::Internals::JSON.from-json($meta-file.slurp);

        # generate `files` (special directories) directly from the file system
        my %bins = Rakudo::Internals.DIR-RECURSE($prefix.add('bin').absolute).map(*.IO).map: -> $real-path {
            my $name-path = $real-path.is-relative
                ?? $real-path
                !! $real-path.relative($prefix);
            $name-path => $real-path.absolute
        }

        my $resources-dir = $prefix.add('resources');
        my %resources = $meta<resources>.grep(*.?chars).map(*.IO).map: -> $path {
            my $real-path = $path ~~ m/^libraries\/(.*)/
                ?? $resources-dir.add('libraries').add( $*VM.platform-library-name($0.Str.IO) )
                !! $resources-dir.add($path);
            my $name-path = $path.is-relative
                ?? "resources/{$path}"
                !! "resources/{$path.relative($prefix)}";
            $name-path => $real-path.absolute;
        }

        $meta<files> = |%bins, |%resources;

        self.bless(:$meta, :$prefix);
    }
    method meta { $!meta }
}

role CompUnit::Repository { ... }

class Distribution::Resource {
    has $.repo;
    has $.repo-name;
    has $.dist-id;
    has $.key;
    has $.IO handles <
        Str gist perl
        absolute is-absolute relative is-relative
        parts volume dirname basename extension
        open resolve slurp lines comb split words copy
    >;

    method BUILD(:$!repo, :$!repo-name, :$!dist-id, :$!key) {
        my $self = self;
        $!IO = Proxy.new(
            FETCH => method () {
                my $repo := $self.repo-name
                    ?? CompUnit::RepositoryRegistry.repository-for-name($self.repo-name)
                    !! CompUnit::RepositoryRegistry.repository-for-spec($self.repo);
                $repo.resource($self.dist-id, "resources/$self.key()")
            },
            STORE => method ($new) { die "Attribute IO is read-only" },
        );
    }

    method platform-library-name() {
        my $library = self.IO;
        ($library ~~ /\.<.alpha>+$/ or $library ~~ /\.so(\.<.digit>+)+$/) #Already a full name?
            ??  $library
            !!  $*VM.platform-library-name($library)
    }
}

class Distribution::Resources does Associative {
    has Str $.dist-id;
    has Str $.repo;
    has Str $.repo-name;

    proto method BUILD(|) { * }

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

# vim: ft=perl6 expandtab sw=4
