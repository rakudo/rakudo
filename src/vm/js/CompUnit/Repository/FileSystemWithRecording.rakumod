my class CompUnit::Repository::FileSystemWithRecording is CompUnit::Repository::FileSystem {
    my class CompUnit::PrecompilationStore::FileSystemWithRecording is CompUnit::PrecompilationStore::File {
        method load-unit(CompUnit::PrecompilationId $compiler-id,
                    CompUnit::PrecompilationId $precomp-id)
        {
            my $unit = self.CompUnit::PrecompilationStore::FileSystem::load-unit($compiler-id, $precomp-id);
            if $unit {
                say('LOAD-UNIT ID:', $precomp-id.id, ' DEPS:', $unit.dependencies.map(*.id.id).join(','), ' PATH:', $unit.path.Str);
            }
            $unit;
        }
    }

    has $!precomp-store;
    method precomp-store(--> CompUnit::PrecompilationStore:D) {
        $!precomp-store //= CompUnit::PrecompilationStore::FileSystemWithRecording.new(
            :prefix(self.prefix.add('.precomp')),
        )
    }

}
