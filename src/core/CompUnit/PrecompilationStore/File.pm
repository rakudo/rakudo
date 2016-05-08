class CompUnit::PrecompilationStore::File does CompUnit::PrecompilationStore {
    my class CompUnit::PrecompilationUnit::File does CompUnit::PrecompilationUnit {
        has IO::Path $.path;
        has IO::Handle $.file;
        has CompUnit::PrecompilationDependency @!dependencies;
        has $!initialized = False;

        my class CompUnit::PrecompilationDependency::File does CompUnit::PrecompilationDependency {
            has CompUnit::PrecompilationId $.id;
            has Str $.src;
            has CompUnit::DependencySpecification $.spec;

            method deserialize(Str $str) {
                use MONKEY-SEE-NO-EVAL;
                my ($id, $src, $spec) = $str.split("\0", 3);
                self.new(:$id, :$src, :spec(EVAL $spec));
            }

            method serialize(--> Str) {
                "$.id\0$.src\0{$.spec.perl}"
            }
        }

        method open() {
            $!file = $!path.open(:r);
        }

        method modified(--> Instant) {
            $!path.modified
        }

        method !read-dependencies() {
            return if $!initialized;
            self.open(:r) unless $!file;

            my $dependency = $!file.get();
            while $dependency {
                @!dependencies.push: CompUnit::PrecompilationDependency::File.deserialize($dependency);
                $dependency = $!file.get();
            }
            $!initialized = True;
        }

        method dependencies(--> Array[CompUnit::PrecompilationDependency]) {
            self!read-dependencies;
            @!dependencies
        }

        method bytecode(--> Buf) {
            self!read-dependencies;
            $!file.slurp-rest(:bin)
        }
    }

    has IO::Path $.prefix is required;
    has IO::Handle $!lock;
    has int $!lock-count = 0;

    submethod BUILD(IO::Path :$!prefix --> Nil) {
        $!prefix.mkdir unless $!prefix.e;
    }

    method !dir(CompUnit::PrecompilationId $compiler-id,
                CompUnit::PrecompilationId $precomp-id)
    {
        self.prefix
            .child($compiler-id.IO)
            .child($precomp-id.substr(0, 2).IO)
    }

    method path(CompUnit::PrecompilationId $compiler-id,
                 CompUnit::PrecompilationId $precomp-id,
                 Str :$extension = '')
    {
        self!dir($compiler-id, $precomp-id).child(($precomp-id ~ $extension).IO)
    }

    method !lock() {
        return if $*W && $*W.is_precompilation_mode();
        $!lock //= $.prefix.child('.lock').open(:create, :rw);
        $!lock.lock(2) if $!lock-count == 0;
        $!lock-count++;
    }

    method unlock() {
        return if $*W && $*W.is_precompilation_mode();
        die "unlock when we're not locked!" if $!lock-count == 0;
        $!lock-count-- if $!lock-count > 0;
        $!lock && $!lock-count == 0 ?? $!lock.unlock !! True;
    }

    method load(CompUnit::PrecompilationId $compiler-id,
                CompUnit::PrecompilationId $precomp-id,
                Str :$extension = '')
    {
        my $path = self.path($compiler-id, $precomp-id);
        if $path ~~ :e {
            CompUnit::PrecompilationUnit::File.new(:$path);
        }
        else {
            CompUnit::PrecompilationUnit::File
        }
    }

    method destination(CompUnit::PrecompilationId $compiler-id,
                       CompUnit::PrecompilationId $precomp-id,
                       Str :$extension = '')
        returns IO::Path
    {
        self!lock();
        my $compiler-dir = self.prefix.child($compiler-id.IO);
        $compiler-dir.mkdir unless $compiler-dir.e;
        my $dest = self!dir($compiler-id, $precomp-id);
        $dest.mkdir unless $dest.e;
        $dest.child(($precomp-id ~ $extension).IO)
    }

    method store(CompUnit::PrecompilationId $compiler-id,
                 CompUnit::PrecompilationId $precomp-id,
                 IO::Path:D $path,
                 Str :$extension = '')
    {
        $path.copy(self.destination($compiler-id, $precomp-id, $extension));
        self.unlock;
    }

    method delete(
        CompUnit::PrecompilationId $compiler-id,
        CompUnit::PrecompilationId $precomp-id,
        Str :$extension = '')
    {
        self.path($compiler-id, $precomp-id, :$extension).unlink;
    }

    method delete-by-compiler(CompUnit::PrecompilationId $compiler-id)
    {
         my $compiler-dir = self.prefix.child($compiler-id.IO);
         for $compiler-dir.dir -> $subdir {
             $subdir.dir>>.unlink;
             $subdir.rmdir;
         }
         $compiler-dir.rmdir;
    }
}

# vim: ft=perl6 expandtab sw=4
