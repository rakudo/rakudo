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
        my $any_modification_times = self.store.path(
            $*PERL.compiler.id,
            any (($path ~ '.deps').IO.lines)
        ).modified;
        not $any_modification_times >= $since
    }

    method load(CompUnit::PrecompilationId $id, Instant :$since) returns CompUnit::Handle {
        my $path = self.store.load($*PERL.compiler.id, $id);
        if $path {
            my $modified = $path.modified;
            if not $since or $modified > $since and self!check-dependencies($path, $modified) {
                my $preserve_global := nqp::ifnull(nqp::gethllsym('perl6', 'GLOBAL'), Mu);
                my $handle := CompUnit::Loader.load-precompilation-file($path);
                self.store.unlock;
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
                self.store.unlock;
                CompUnit::Handle
            }
        }
        else {
            CompUnit::Handle
        }
    }

    method precompile(IO::Path:D $path, CompUnit::PrecompilationId $id) {
        my $io = self.store.destination($*PERL.compiler.id, $id);

        my Mu $opts := nqp::atkey(%*COMPILING, '%?OPTIONS');
        my $lle = !nqp::isnull($opts) && !nqp::isnull(nqp::atkey($opts, 'll-exception'))
          ?? ' --ll-exception'
          !! '';
        %*ENV<RAKUDO_PRECOMP_WITH> = $*REPO.repo-chain>>.path-spec.join(',');

RAKUDO_MODULE_DEBUG("Precomping with %*ENV<RAKUDO_PRECOMP_WITH>")
  if $*RAKUDO_MODULE_DEBUG;

        my $cmd = "$*EXECUTABLE$lle --target={$*VM.precomp-target} --output=$io $path";
        my $proc = shell("$cmd 2>&1", :out);
        %*ENV<RAKUDO_PRECOMP_WITH>:delete;

        my @result = $proc.out.lines;
        if not $proc.out.close or $proc.status {  # something wrong
            self.store.unlock;
            push @result, "Return status $proc.status\n";
            fail @result if @result;
        }
        else {
            spurt(($io ~ '.deps').IO, @result.map(* ~ "\n").join(''));
            self.store.unlock;
            True
        }
    }
}

# vim: ft=perl6 expandtab sw=4
