role CompUnit::PrecompilationStore {
    # Prepare a new implementation specific PrecompilationUnit for storage
    method new-unit(| --> CompUnit::PrecompilationUnit:D)
    { ... }

    # Load the precompilation identified by the pairing of the specified
    # compiler and precompilation ID.
    method load-unit(CompUnit::PrecompilationId $compiler-id,
                CompUnit::PrecompilationId $precomp-id)
    { ... }

    # Return the repository id for which the specified precomp file's
    # dependencies have been validated
    method load-repo-id(CompUnit::PrecompilationId $compiler-id,
                CompUnit::PrecompilationId $precomp-id)
    { ... }

    # Store the file at the specified path in the precompilation store,
    # under the given compiler ID and precompilation ID.
    method store-file(CompUnit::PrecompilationId $compiler-id,
                 CompUnit::PrecompilationId $precomp-id,
                 IO::Path:D $path,
                 :$extension = '')
    { ... }

    # Store the given precompilation unit in the precompilation store
    # under the given compiler ID and precompilation ID.
    method store-unit(CompUnit::PrecompilationId $compiler-id,
                 CompUnit::PrecompilationId $precomp-id,
                 CompUnit::PrecompilationUnit $unit)
    { ... }

    # Store the given repo-id for a precompilation under the given
    # compiler ID and precompilation ID.
    method store-repo-id(CompUnit::PrecompilationId $compiler-id,
                 CompUnit::PrecompilationId $precomp-id,
                 :$repo-id!)
    { ... }

    # Delete an individual precompilation.
    method delete(CompUnit::PrecompilationId $compiler-id,
                  CompUnit::PrecompilationId $precomp-id)
    { ... }

    # Delete all precompilations for a particular compiler.
    method delete-by-compiler(CompUnit::PrecompilationId $compiler-id)
    { ... }
}

# vim: expandtab shiftwidth=4
