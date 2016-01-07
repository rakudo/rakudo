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
    has %!loaded;

    method !load-handle-for-path(IO::Path $path) {
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

    method !load-dependencies(IO::Path $path, Instant $since) {
        my $compiler-id = $*PERL.compiler.id;
        for ($path ~ '.deps').IO.lines -> $dependency {
            my ($id, $src) = $dependency.words;
            my $file = self.store.path($compiler-id, $id);
            my $modified = $file.modified;
            RAKUDO_MODULE_DEBUG("$file mtime: $modified since: $since src: {$src.IO.modified}") if $*RAKUDO_MODULE_DEBUG;
            return False if $modified > $since or not $src.IO.e or $modified <= $src.IO.modified;
            %!loaded{$id} //= self!load-handle-for-path(self.store.load($compiler-id, $id));

            # report back id and source location of dependency to dependant
            say "$id $src" if $*W and $*W.is_precompilation_mode;
        }
        True
    }

    method load(CompUnit::PrecompilationId $id, Instant :$since) returns CompUnit::Handle {
        my $path = self.store.load($*PERL.compiler.id, $id);
        if $path {
            my $modified = $path.modified;
            if (not $since or $modified > $since) and self!load-dependencies($path, $modified) {
                %!loaded{$id} //= self!load-handle-for-path($path)
            }
            else {
                RAKUDO_MODULE_DEBUG("Removing precompiled $path mtime: $modified since: $since") if $*RAKUDO_MODULE_DEBUG;
                self.store.delete($*PERL.compiler.id, $id); # remove outdated file so we precompile again
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
        my $compiler-id = $*PERL.compiler.id;
        my $io = self.store.destination($compiler-id, $id);
        if not $force and $io.e and $io.s {
            RAKUDO_MODULE_DEBUG("$path already precompiled into $io") if $*RAKUDO_MODULE_DEBUG;
            self.store.unlock;
            return True;
        }

        my $rev-deps = ($io ~ '.rev-deps').IO;
        if $rev-deps.e {
            for $rev-deps.lines {
                RAKUDO_MODULE_DEBUG("removing outdated rev-dep $_") if $*RAKUDO_MODULE_DEBUG;
                self.store.delete($compiler-id, $_);
            }
        }

        my Mu $opts := nqp::atkey(%*COMPILING, '%?OPTIONS');
        my $lle = !nqp::isnull($opts) && !nqp::isnull(nqp::atkey($opts, 'll-exception'))
          ?? '--ll-exception'
          !! Empty;
        %*ENV<RAKUDO_PRECOMP_WITH> = $*REPO.repo-chain.map(*.path-spec).join(',');
        %*ENV<RAKUDO_PRECOMP_LOADING> = to-json @*MODULES // [];
        my $current_dist = %*ENV<RAKUDO_PRECOMP_DIST>;
        %*ENV<RAKUDO_PRECOMP_DIST> = $*RESOURCES ?? $*RESOURCES.Str !! '{}';

        RAKUDO_MODULE_DEBUG("Precompiling $path into $io") if $*RAKUDO_MODULE_DEBUG;
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
            RAKUDO_MODULE_DEBUG("Precompiled $path into $io") if $*RAKUDO_MODULE_DEBUG;
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
