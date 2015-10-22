class CompUnit::PrecompilationStore::File does CompUnit::PrecompilationStore {
    has Str $.prefix is required;

    method load(CompUnit::PrecompilationId $compiler-id,
                CompUnit::PrecompilationId $precomp-id)
    {
    }

    method store(CompUnit::PrecompilationId $compiler-id,
                 CompUnit::PrecompilationId $precomp-id,
                 Str:D $path)
    {
        my $dest = self.prefix.IO
            .child($compiler-id.IO)
            .child($precomp-id.substr(0, 2).IO);
        $dest.mkdir;
        $path.IO.copy($dest.child($precomp-id.IO));
    }

    method delete(CompUnit::PrecompilationId $compiler-id, CompUnit::PrecompilationId $precomp-id)
    {
    }

    method delete-by-compiler(CompUnit::PrecompilationId $compiler-id)
    {
    }
}

# vim: ft=perl6 expandtab sw=4
