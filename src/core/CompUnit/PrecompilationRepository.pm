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

    my $lle;
    my $profile;

    method try-load(CompUnit::PrecompilationId $id, IO::Path $source) returns CompUnit::Handle {
        my $handle = (
            self.may-precomp and (
                self.load($id, :since($source.modified)) # already precompiled?
                or self.precompile($source, $id) and self.load($id) # if not do it now
            )
        );
        my $precompiled = ?$handle;

        if $*W and $*W.is_precompilation_mode {
            if $precompiled {
                say "$id $source";
            }
            else {
                nqp::exit(0);
            }
        }

        $handle ?? $handle !! Nil
    }

    method !load-handle-for-path(IO::Path $path) {
        my $preserve_global := nqp::ifnull(nqp::gethllsym('perl6', 'GLOBAL'), Mu);
        if $*RAKUDO_MODULE_DEBUG -> $RMD { $RMD("Loading precompiled\n$path") }
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
        my $RMD = $*RAKUDO_MODULE_DEBUG;
        for ($path ~ '.deps').IO.lines -> $dependency {
            Rakudo::Internals.KEY_SPACE_VALUE($dependency,my $id,my $src);
            my $file = self.store.path($compiler-id, $id);
            my $modified = $file.modified;
            $RMD("$file\nmtime: $modified\nsince: $since\n  src: {$src.IO.modified}")
              if $RMD;

            return False if $modified > $since;
            my $srcIO = $src.IO;
            return False if not $srcIO.e or $modified <= $srcIO.modified;

            %!loaded{$id} //= self!load-handle-for-path(self.store.load($compiler-id, $id));

            # report back id and source location of dependency to dependant
            say "$id $src" if $*W and $*W.is_precompilation_mode;
        }
        True
    }

    method load(CompUnit::PrecompilationId $id, Instant :$since) returns CompUnit::Handle {
        my $compiler-id = $*PERL.compiler.id;
        my $path = self.store.load($compiler-id, $id);
        if $path {
            my $modified = $path.modified;
            if (not $since or $modified > $since) and self!load-dependencies($path, $modified) {
                %!loaded{$id} //= self!load-handle-for-path($path)
            }
            else {
                if $*RAKUDO_MODULE_DEBUG -> $RMD {
                    $RMD("Removing precompiled $path\nmtime: $modified\nsince: $since")
                }
                # remove outdated file so we precompile again
                self.store.delete($compiler-id, $id);
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
        my $RMD = $*RAKUDO_MODULE_DEBUG;
        if not $force and $io.e and $io.s {
            $RMD("$path\nalready precompiled into\n$io") if $RMD;
            self.store.unlock;
            return True;
        }

        my $rev-deps = ($io ~ '.rev-deps').IO;
        if $rev-deps.e {
            for $rev-deps.lines {
                $RMD("removing outdated rev-dep $_") if $RMD;
                self.store.delete($compiler-id, $_);
            }
        }

        $lle     //= Rakudo::Internals.LL-EXCEPTION;
        $profile //= Rakudo::Internals.PROFILE;
        my %ENV := %*ENV;
        %ENV<RAKUDO_PRECOMP_WITH> = $*REPO.repo-chain.map(*.path-spec).join(',');
        %ENV<RAKUDO_PRECOMP_LOADING> = to-json @*MODULES // [];
        my $current_dist = %ENV<RAKUDO_PRECOMP_DIST>;
        %ENV<RAKUDO_PRECOMP_DIST> = $*RESOURCES ?? $*RESOURCES.Str !! '{}';

        $RMD("Precompiling $path into $io") if $RMD;
        my $perl6 = $*EXECUTABLE.subst('perl6-debug', 'perl6'); # debugger would try to precompile it's UI
        my $proc = run(
          $perl6,
          $lle,
          $profile,
          "--target=" ~ Rakudo::Internals.PRECOMP-TARGET,
          "--output=$io",
          $path,
          :out,
          :err,
        );
        %ENV.DELETE-KEY(<RAKUDO_PRECOMP_WITH>);
        %ENV.DELETE-KEY(<RAKUDO_PRECOMP_LOADING>);
        %ENV<RAKUDO_PRECOMP_DIST> = $current_dist;

        my @result = $proc.out.lines.unique;
        if not $proc.out.close or $proc.status {  # something wrong
            self.store.unlock;
            $RMD("Precomping $path failed: $proc.status()") if $RMD;
            Rakudo::Internals.VERBATIM-EXCEPTION(1);
            die $proc.err.slurp-rest;
        }

        if $proc.err.slurp-rest -> $warnings {
            $*ERR.print($warnings);
        }
        $RMD("Precompiled $path into $io") if $RMD;
        my str $dependencies = '';
        for @result -> $dependency {
            unless $dependency ~~ /^<[A..Z0..9]> ** 40 \s .+/ {
                say $dependency;
                next
            }
            Rakudo::Internals.KEY_SPACE_VALUE(
              $dependency,my $dependency-id,my $dependency-src);
            my $path = self.store.path($compiler-id, $dependency-id);
            if $path.e {
                $dependencies ~= "$dependency\n";
                spurt($path ~ '.rev-deps', "$id\n", :append);
            }
        }
        spurt($io ~ '.deps', $dependencies);
        self.store.unlock;
        True
    }
}

# vim: ft=perl6 expandtab sw=4
