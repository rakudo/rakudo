role CompUnit::PrecompilationRepository {
    method load(CompUnit::PrecompilationId $id) returns CompUnit {
        CompUnit
    }
}

BEGIN CompUnit::PrecompilationRepository::<None> := CompUnit::PrecompilationRepository.new;

class CompUnit::PrecompilationRepository::Default does CompUnit::PrecompilationRepository {
    has CompUnit::PrecompilationStore $.store;

    method load(CompUnit::PrecompilationId $id) returns CompUnit {
        my $path = self.store.load($*PERL.compiler.id, $id);
        CompUnit
    }
}

# vim: ft=perl6 expandtab sw=4
