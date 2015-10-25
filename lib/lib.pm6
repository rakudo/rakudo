module lib { };
my sub EXPORT(*@a) {
    my CompUnit::Repository $next-repo = $*REPO;
    $next-repo := CompUnitRepo.new($_, :$next-repo) for PARSE-INCLUDE-SPECS(@a.join(','));
    PROCESS::<$REPO> := $next-repo;
    return ().hash;
}
