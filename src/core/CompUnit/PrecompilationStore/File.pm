class CompUnit::PrecompilationStore::File does CompUnit::PrecompilationStore {
    has IO::Path $.prefix is required;

    method !dir(CompUnit::PrecompilationId $compiler-id,
                CompUnit::PrecompilationId $precomp-id)
    {
        self.prefix
            .child($compiler-id.IO)
            .child($precomp-id.substr(0, 2).IO)
    }

    method !path(CompUnit::PrecompilationId $compiler-id,
                 CompUnit::PrecompilationId $precomp-id)
    {
        self!dir($compiler-id, $precomp-id).child($precomp-id.IO)
    }

    method load(CompUnit::PrecompilationId $compiler-id,
                CompUnit::PrecompilationId $precomp-id)
    {
        my $path = self!path($compiler-id, $precomp-id);
        $path ~~ :e ?? $path !! Str
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
        self!path($compiler-id, $precomp-id).unlink;
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
