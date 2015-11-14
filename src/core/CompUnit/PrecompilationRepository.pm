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

    method precompile(CompUnit:D $compunit, CompUnit::PrecompilationId $id) {
        my $io = self.store.destination($*PERL.compiler.id, $id);
        my $path = $compunit.path;
        die "Cannot pre-compile over a newer existing file: $io"
          if $io.e && $io.modified > $path.modified;

        my Mu $opts := nqp::atkey(%*COMPILING, '%?OPTIONS');
        my $lle = !nqp::isnull($opts) && !nqp::isnull(nqp::atkey($opts, 'll-exception'))
          ?? ' --ll-exception'
          !! '';
        %*ENV<RAKUDO_PRECOMP_WITH> = CREATE-INCLUDE-SPECS(@*INC);

RAKUDO_MODULE_DEBUG("Precomping with %*ENV<RAKUDO_PRECOMP_WITH>")
  if $*RAKUDO_MODULE_DEBUG;

        my $cmd = "$*EXECUTABLE$lle --target={$*VM.precomp-target} --output=$io $path";
        my $proc = shell("$cmd 2>&1", :out, :!chomp);
        %*ENV<RAKUDO_PRECOMP_WITH>:delete;

        my $result = '';
        $result ~= $_ for $proc.out.lines;
        $proc.out.close;
        if $proc.status -> $status {  # something wrong
            $result ~= "Return status $status\n";
            fail $result if $result;
        }
        note $result if $result;
        True
    }
}

# vim: ft=perl6 expandtab sw=4
