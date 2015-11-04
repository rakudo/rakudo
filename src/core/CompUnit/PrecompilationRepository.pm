role CompUnit::PrecompilationRepository {
    method load(CompUnit::PrecompilationId $id) returns CompUnit {
        CompUnit
    }
}

BEGIN CompUnit::PrecompilationRepository::<None> := CompUnit::PrecompilationRepository.new;

class CompUnit::PrecompilationRepository::Default does CompUnit::PrecompilationRepository {
    has CompUnit::PrecompilationStore $.store;

    method load(CompUnit::PrecompilationId $id) returns CompUnit::Handle {
        my $path = self.store.load($*PERL.compiler.id, $id);
        if $path {
            my $preserve_global := nqp::ifnull(nqp::gethllsym('perl6', 'GLOBAL'), Mu);
            my $handle := CompUnit::Loader.load-precompilation-file($path);
            nqp::bindhllsym('perl6', 'GLOBAL', $preserve_global);
            CATCH {
                default {
                    nqp::bindhllsym('perl6', 'GLOBAL', $preserve_global);
                    .throw;
                }
            }
            $handle
        }
        else {
            CompUnit::Handle
        }
    }
}

# vim: ft=perl6 expandtab sw=4
