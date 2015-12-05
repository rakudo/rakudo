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
    use nqp;

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
          ?? True
          !! False;
        my @*PRECOMP-WITH = $*REPO.repo-chain>>.path-spec;
        my @*PRECOMP-LOADING = @*MODULES;

RAKUDO_MODULE_DEBUG("Precomping with @*PRECOMP-WITH.join(',')")
  if $*RAKUDO_MODULE_DEBUG;

        my @dependencies;
        my $*ADD-DEPENDENCY = -> $id, $src { @dependencies.push: [$id, $src] };

        my $compiler := nqp::getcomp('perl6');
        $compiler.command_eval: $path, :ll-exception($lle), :target($*VM.precomp-target), :output($io),
            :encoding('utf8'), :transcode('ascii iso-8859-1');

        my $compiler-id = $*PERL.compiler.id;
        for @dependencies -> ($dependency-id, $dependency-src) {
            my $path = self.store.path($compiler-id, $dependency-id);
            if $path.e {
                spurt(($path ~ '.rev-deps').IO, "$id\n", :append);
            }
        }
        my $depfile = ($io ~ '.deps').IO;
        for @dependencies -> ($dependency-id, $dependency-src) {
            $depfile.say: "$dependency-id $dependency-src";
        }
        self.store.unlock;
        True
    }
}

# vim: ft=perl6 expandtab sw=4
