role CompUnit::PrecompilationRepository {
    has CompUnit::PrecompilationStore $.store;
}

BEGIN CompUnit::PrecompilationRepository::<None> := CompUnit::PrecompilationRepository;

# vim: ft=perl6 expandtab sw=4
