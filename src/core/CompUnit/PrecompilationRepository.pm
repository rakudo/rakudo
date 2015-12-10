{
    my $i;
    role CompUnit::PrecompilationRepository {
        has $!i = $i++;

        method load(CompUnit::PrecompilationId $id) returns CompUnit {
            CompUnit
        }

        method may-precomp() {
            $i < 3 # number of next repo after None and the first Default
        }
    }
}

BEGIN CompUnit::PrecompilationRepository::<None> := CompUnit::PrecompilationRepository.new;

class CompUnit { ... }
class CompUnit::PrecompilationRepository::Default does CompUnit::PrecompilationRepository {
    has CompUnit::PrecompilationStore $.store;

    method !check-dependencies(IO::Path $path, Instant $since) {
        my @dependencies = ($path ~ '.deps').IO.lines;
        my $compiler-id = $*PERL.compiler.id;
        for ($path ~ '.deps').IO.lines -> $dependency {
            my ($id, $src) = $dependency.words;
            my $modified = self.store.path($compiler-id, $id).modified;
            return False if $modified >= $since or not $src.IO.e or $modified <= $src.IO.modified;
        }
        True
    }

    method load(CompUnit::PrecompilationId $id, Instant :$since) returns CompUnit::Handle {
        my $path = self.store.load($*PERL.compiler.id, $id);
        if $path {
            my $modified = $path.modified;
            if not $since or $modified > $since and self!check-dependencies($path, $modified) {
                my $preserve_global := nqp::ifnull(nqp::gethllsym('perl6', 'GLOBAL'), Mu);
                RAKUDO_MODULE_DEBUG("Loading precompiled $path") if $*RAKUDO_MODULE_DEBUG;
                my $handle := CompUnit::Loader.load-precompilation-file($path);
                nqp::bindhllsym('perl6', 'GLOBAL', $preserve_global);
                CATCH {
                    default {
                        nqp::bindhllsym('perl6', 'GLOBAL', $preserve_global);
                        .throw;
                    }
                }
                LEAVE {
                    self.store.unlock;
                }
                $handle
            }
            else {
                self.store.unlock;
                CompUnit::Handle
            }
        }
        else {
            self.store.unlock;
            CompUnit::Handle
        }
    }

    method precompile(IO::Path:D $path, CompUnit::PrecompilationId $id, Bool :$force = False) {
        my $io = self.store.destination($*PERL.compiler.id, $id);
        if not $force and $io.e and $io.s {
            self.store.unlock;
            return True;
        }

        my Mu $opts := nqp::atkey(%*COMPILING, '%?OPTIONS');
        my $lle = !nqp::isnull($opts) && !nqp::isnull(nqp::atkey($opts, 'll-exception'))
          ?? '--ll-exception'
          !! Empty;
        %*ENV<RAKUDO_PRECOMP_WITH> = $*REPO.repo-chain>>.path-spec.join(',');
        %*ENV<RAKUDO_PRECOMP_LOADING> = to-json @*MODULES // [];
        my $current_dist = %*ENV<RAKUDO_PRECOMP_DIST>;
        %*ENV<RAKUDO_PRECOMP_DIST> = $*RESOURCES ?? $*RESOURCES.Str !! '{}';

        my $proc = run($*EXECUTABLE, $lle, "--target={$*VM.precomp-target}", "--output=$io", $path, :out);
        %*ENV<RAKUDO_PRECOMP_WITH>:delete;
        %*ENV<RAKUDO_PRECOMP_LOADING>:delete;
        %*ENV<RAKUDO_PRECOMP_DIST> = $current_dist;

        my @result = $proc.out.lines;
        if not $proc.out.close or $proc.status {  # something wrong
            self.store.unlock;
            push @result, "Return status { $proc.status }\n";
            RAKUDO_MODULE_DEBUG("Precomping $path failed: {@result}") if $*RAKUDO_MODULE_DEBUG;
            fail @result if @result;
        }
        else {
            my @dependencies;
            my $compiler-id = $*PERL.compiler.id;
            for @result -> $dependency {
                my ($dependency-id, $dependency-src) = $dependency.words;
                my $path = self.store.path($compiler-id, $dependency-id);
                if $path.e {
                    push @dependencies, $dependency;
                    spurt(($path ~ '.rev-deps').IO, "$id\n", :append);
                }
            }
            spurt(($io ~ '.deps').IO, @dependencies.map(* ~ "\n").join(''));
            self.store.unlock;
            True
        }
    }
}

# vim: ft=perl6 expandtab sw=4
