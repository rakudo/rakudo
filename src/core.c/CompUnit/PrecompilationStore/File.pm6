class CompUnit::PrecompilationStore::File does CompUnit::PrecompilationStore {
    my class CompUnit::PrecompilationUnit::File does CompUnit::PrecompilationUnit {
        has CompUnit::PrecompilationId $.id;
        has IO::Path $.path;
        has IO::Handle $!file;
        has CompUnit::PrecompilationDependency @!dependencies;
        has $!initialized = False;
        has $.checksum;
        has $.source-checksum;
        has $!bytecode;
        has $!store;
        has Lock $!update-lock = Lock.new;

        submethod BUILD(
            CompUnit::PrecompilationId :$!id,
            IO::Path :$!path,
            :$!source-checksum,
            :@!dependencies,
            :$!bytecode,
            :$!store,
            --> Nil
        ) {
            if $!bytecode {
                $!initialized = True;
                $!checksum = nqp::sha1($!bytecode.decode('iso-8859-1'));
            }
        }

        method !open() {
            $!file = $!path.open(:r);
        }

        method modified(--> Instant:D) {
            $!path.modified
        }

        method !read-dependencies() {
            $!update-lock.protect: {
                return if $!initialized;
                self!open(:r) unless $!file;

                $!checksum        = $!file.get;
                $!source-checksum = $!file.get;
                my $dependency    = $!file.get;
                while $dependency {
                    @!dependencies.push: CompUnit::PrecompilationDependency::File.deserialize($dependency);
                    $dependency = $!file.get;
                }
                $!initialized = True;
            }
        }

        method dependencies(--> Array[CompUnit::PrecompilationDependency]) {
            self!read-dependencies;
            @!dependencies
        }

        method bytecode(--> Buf:D) {
            $!update-lock.protect: {
                self!read-dependencies;
                $!bytecode //= $!file.slurp-rest(:bin,:close)
            }
        }

        method bytecode-handle(--> IO::Handle:D) {
            self!read-dependencies;
            $!file
        }

        method source-checksum() is rw {
            self!read-dependencies;
            $!source-checksum
        }

        method checksum() is rw {
            self!read-dependencies;
            $!checksum
        }

        method Str(--> Str:D) {
            self.path.Str
        }

        method close(--> Nil) {
            $!update-lock.protect: {
                $!file.close if $!file;
                $!file = Nil;
            }
        }

        method save-to(IO::Path $precomp-file) {
            my $handle = $precomp-file.open(:w);
            $handle.print($!checksum ~ "\n");
            $handle.print($!source-checksum ~ "\n");
            $handle.print($_.serialize ~ "\n") for @!dependencies;
            $handle.print("\n");
            $handle.write($!bytecode);
            $handle.close;
            $!path = $precomp-file;
        }

        method is-up-to-date(CompUnit::PrecompilationDependency $dependency, Bool :$check-source --> Bool) {
            my $result = self.CompUnit::PrecompilationUnit::is-up-to-date($dependency, :$check-source);
            $!store.remove-from-cache($.id) unless $result;
            $result
        }
    }

    has IO::Path $.prefix is required;
    has IO::Handle $!lock;
    has int $!lock-count = 0;
    has %!loaded;
    has %!compiler-cache;
    has %!dir-cache;
    has Lock $!update-lock = Lock.new;

    submethod BUILD(IO::Path :$!prefix --> Nil) {
    }

    method new-unit(|c) {
        CompUnit::PrecompilationUnit::File.new(|c, :store(self))
    }

    method !dir(CompUnit::PrecompilationId $compiler-id,
                CompUnit::PrecompilationId $precomp-id)
    {
        $!update-lock.protect: {
            %!dir-cache{$compiler-id ~ $precomp-id} //=
                (%!compiler-cache{$compiler-id} //= self.prefix.add($compiler-id))
                    .add($precomp-id.substr(0, 2))
        }
    }

    method path(CompUnit::PrecompilationId $compiler-id,
                 CompUnit::PrecompilationId $precomp-id,
                 Str :$extension = '')
    {
        self!dir($compiler-id, $precomp-id).add($precomp-id ~ $extension)
    }

    method !lock(--> Nil) {
        return if $*W && $*W.is_precompilation_mode();
        $!update-lock.lock;
        $!lock //= $.prefix.add('.lock').open(:create, :rw);
        $!lock.lock if $!lock-count++ == 0;
    }

    method unlock() {
        return if $*W && $*W.is_precompilation_mode();
        {
            LEAVE $!update-lock.unlock;
            die "unlock when we're not locked!" if $!lock-count == 0;
            $!lock-count-- if $!lock-count > 0;
            if $!lock && $!lock-count == 0 {
                $!lock.unlock;
                $!lock.close;
                $!lock = Nil;
            }
            True
        }
    }

    method load-unit(CompUnit::PrecompilationId $compiler-id,
                CompUnit::PrecompilationId $precomp-id)
    {
        $!update-lock.protect: {
            %!loaded{$precomp-id} //= do {
                my $path = self.path($compiler-id, $precomp-id);
                $path ~~ :e
                    ?? CompUnit::PrecompilationUnit::File.new(:id($precomp-id), :$path, :store(self))
                    !! Nil
            }
        }
    }

    method load-repo-id(CompUnit::PrecompilationId $compiler-id,
                CompUnit::PrecompilationId $precomp-id)
    {
        my $path = self.path($compiler-id, $precomp-id, :extension<.repo-id>);
        if $path ~~ :e {
            $path.slurp
        }
        else {
            Nil
        }
    }

    method remove-from-cache(CompUnit::PrecompilationId $precomp-id) {
        $!update-lock.protect: { %!loaded{$precomp-id}:delete };
    }

    method destination(CompUnit::PrecompilationId $compiler-id,
                       CompUnit::PrecompilationId $precomp-id,
                       Str :$extension = ''
                       --> IO::Path:D)
    {
        unless $!prefix.e {
            $!prefix.mkdir or return;
        }
        return unless $!prefix.w;
        self!lock();
        self!file($compiler-id, $precomp-id, :$extension);
    }

    method !file(CompUnit::PrecompilationId $compiler-id,
                 CompUnit::PrecompilationId $precomp-id,
                 Str :$extension = ''
                 --> IO::Path:D)
    {
        my $compiler-dir = self.prefix.add($compiler-id);
        $compiler-dir.mkdir unless $compiler-dir.e;
        my $dest = self!dir($compiler-id, $precomp-id);
        $dest.mkdir unless $dest.e;
        $dest.add($precomp-id ~ $extension)
    }

    method store-file(CompUnit::PrecompilationId $compiler-id,
                 CompUnit::PrecompilationId $precomp-id,
                 IO::Path:D $path,
                 :$extension = '')
    {
        $path.rename(self!file($compiler-id, $precomp-id, :$extension));
    }

    method store-unit(CompUnit::PrecompilationId $compiler-id,
                 CompUnit::PrecompilationId $precomp-id,
                 CompUnit::PrecompilationUnit $unit)
    {
        my $precomp-file = self!file($compiler-id, $precomp-id, :extension<.tmp>);
        $unit.save-to($precomp-file);
        $precomp-file.rename(self!file($compiler-id, $precomp-id));
        self.remove-from-cache($precomp-id);
    }

    method store-repo-id(CompUnit::PrecompilationId $compiler-id,
                 CompUnit::PrecompilationId $precomp-id,
                 :$repo-id!)
    {
        try self!file($compiler-id, $precomp-id, :extension<.repo-id>).spurt($repo-id);
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
         my $compiler-dir = self.prefix.add($compiler-id);
         for $compiler-dir.dir -> $subdir {
             $subdir.dir>>.unlink;
             $subdir.rmdir;
         }
         $compiler-dir.rmdir;
    }
}

# vim: ft=perl6 expandtab sw=4
