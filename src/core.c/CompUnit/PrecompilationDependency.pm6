role CompUnit::PrecompilationDependency {
    method id(--> CompUnit::PrecompilationId:D) { ... }
    method src(--> Str:D) { ... }
    method spec(--> CompUnit::DependencySpecification:D) { ... }
    method checksum(--> Str:D) { ... }
    method Str() {
        "$.id $.src $.spec"
    }
    method serialize(--> Str:D) { ... }
    method deserialize(Str, --> CompUnit::PrecompilationDependency:D) { ... }
}

# vim: expandtab shiftwidth=4
