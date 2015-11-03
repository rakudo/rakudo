class CompUnit::PrecompilationStore::File does CompUnit::PrecompilationStore {
    has Str $.prefix is required;

    method !dir(CompUnit::PrecompilationId $compiler-id,
                CompUnit::PrecompilationId $precomp-id)
    {
        self.prefix.IO
            .child($compiler-id.IO)
            .child($precomp-id.substr(0, 2).IO)
    }

    method !path(CompUnit::PrecompilationId $compiler-id,
                 CompUnit::PrecompilationId $precomp-id)
    {
        self!dir.child($precomp-id.IO)
    }

    method load(CompUnit::PrecompilationId $compiler-id,
                CompUnit::PrecompilationId $precomp-id)
    {
        self!path.slurp
    }

    method store(CompUnit::PrecompilationId $compiler-id,
                 CompUnit::PrecompilationId $precomp-id,
                 Str:D $path)
    {
        my $dest = self!dir($compiler-id, $precomp-id);
        $dest.mkdir;
        $path.IO.copy($dest.child($precomp-id.IO));
    }

    method delete(CompUnit::PrecompilationId $compiler-id, CompUnit::PrecompilationId $precomp-id)
    {
        self!path.unlink;
    }

    method delete-by-compiler(CompUnit::PrecompilationId $compiler-id)
    {
         my $compiler-dir = self.prefix.IO.child($compiler-id.IO);
         for $compiler-dir.dir -> $subdir {
             $subdir.dir>>.unlink;
             $subdir.rmdir;
         }
         $compiler-dir.rmdir;
    }
}

# vim: ft=perl6 expandtab sw=4
