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

    method try-load(
        CompUnit::PrecompilationId $id,
        IO::Path $source,
        :$source-name,
        CompUnit::DependencySpecification :$spec,
        CompUnit::PrecompilationStore :@precomp-stores = Array[CompUnit::PrecompilationStore].new($.store),
    ) returns CompUnit::Handle {
        my $RMD = $*RAKUDO_MODULE_DEBUG;
        $RMD("try-load $id: $source-name") if $RMD;

        # Even if we may no longer precompile, we should use already loaded files
        return %!loaded{$id} if %!loaded{$id}:exists;

        my $handle = (
            self.may-precomp and (
                self.load($id, :since($source.modified), :@precomp-stores) # already precompiled?
                or self.precompile($source, $id, :$source-name) and self.load($id, :@precomp-stores) # if not do it now
            )
        );
        my $precompiled = ?$handle;

        if $*W and $*W.is_precompilation_mode {
            if $precompiled {
                say "$id\0$source\0{$spec.perl}";
            }
            else {
                nqp::exit(0);
            }
        }

        $handle ?? $handle !! Nil
    }

    method !load-handle-for-path(IO::Handle $precomp-file, $store) {
        my $preserve_global := nqp::ifnull(nqp::gethllsym('perl6', 'GLOBAL'), Mu);
        if $*RAKUDO_MODULE_DEBUG -> $RMD { $RMD("Loading precompiled\n$precomp-file") }
        my $handle := CompUnit::Loader.load-precompilation($precomp-file.slurp-rest(:bin));
        nqp::bindhllsym('perl6', 'GLOBAL', $preserve_global);
        CATCH {
            default {
                nqp::bindhllsym('perl6', 'GLOBAL', $preserve_global);
                .throw;
            }
        }
        LEAVE {
            $store.unlock;
        }
        $handle
    }

    method !deserialize-spec($spec) {
        use MONKEY-SEE-NO-EVAL;
        EVAL $spec
    }

    method !load-dependencies(IO::Handle:D $precomp-file, Instant $since, @precomp-stores) {
        my $compiler-id = $*PERL.compiler.id;
        my $RMD = $*RAKUDO_MODULE_DEBUG;
        my $resolve = False;
        my $repo = $*REPO;
        my $dependency = $precomp-file.get();
        if $dependency ne $repo.id {
            $RMD("Repo changed: $dependency ne {$repo.id}. Need to re-check dependencies.") if $RMD;
            $resolve = True;
        }
        $dependency = $precomp-file.get();
        while $dependency {
            $RMD("dependency: $dependency") if $RMD;
            my ($id, $src, $spec) = $dependency.split("\0", 3);
            unless %!loaded{$id}:exists {
                if $resolve {
                    my $comp-unit = $repo.resolve(self!deserialize-spec($spec));
                    $RMD("Old id: $id, new id: {$comp-unit.repo-id}") if $RMD;
                    return False unless $comp-unit and $comp-unit.repo-id eq $id;
                }
                my $file;
                my $store = @precomp-stores.first({ $file = $_.path($compiler-id, $id); $file.e });
                $RMD("Could not find $spec") if $RMD and not $store;
                return False unless $store;
                my $modified = $file.modified;
                $RMD("$file\nspec: $spec\nmtime: $modified\nsince: $since\n  src: {$src.IO.modified}")
                  if $RMD;

                return False if $modified > $since;
                my $srcIO = $src.IO;
                return False if not $srcIO.e or $modified <= $srcIO.modified;

                my $dependency-precomp = $store.load($compiler-id, $id).open(:r);
                while $dependency-precomp.get { }
                %!loaded{$id} = self!load-handle-for-path($dependency-precomp, $store);
            }

            # report back id and source location of dependency to dependant
            say "$id\0$src\0$spec" if $*W and $*W.is_precompilation_mode;
            $dependency = $precomp-file.get();
        }
        True
    }

    method load(
        CompUnit::PrecompilationId $id,
        Instant :$since,
        CompUnit::PrecompilationStore :@precomp-stores = Array[CompUnit::PrecompilationStore].new($.store),
    ) returns CompUnit::Handle {
        return %!loaded{$id} if %!loaded{$id}:exists;
        my $RMD = $*RAKUDO_MODULE_DEBUG;
        my $compiler-id = $*PERL.compiler.id;
        for @precomp-stores -> $store {
            $RMD("Trying to load $id from $store.prefix()") if $RMD;
            my $path = $store.load($compiler-id, $id);
            if $path {
                my $modified = $path.modified;
                my $precomp-file = $path.open(:r);
                if (not $since or $modified > $since)
                    and self!load-dependencies($precomp-file, $modified, @precomp-stores)
                {
                    return %!loaded{$id} = self!load-handle-for-path($precomp-file, $store)
                }
                else {
                    if $*RAKUDO_MODULE_DEBUG -> $RMD {
                        $RMD("Removing precompiled $path\nmtime: $modified\nsince: $since")
                    }
                    # remove outdated file so we precompile again
                    $store.delete($compiler-id, $id);
                    $store.unlock;
                    return CompUnit::Handle
                }
            }
            else {
                $store.unlock;
            }
        }
        CompUnit::Handle
    }

    method precompile(IO::Path:D $path, CompUnit::PrecompilationId $id, Bool :$force = False, :$source-name) {
        my $compiler-id = $*PERL.compiler.id;
        my $io = self.store.destination($compiler-id, $id);
        my $RMD = $*RAKUDO_MODULE_DEBUG;
        if not $force and $io.e and $io.s {
            $RMD("$source-name\nalready precompiled into\n$io") if $RMD;
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
        %ENV<RAKUDO_PRECOMP_LOADING> = Rakudo::Internals::JSON.to-json: @*MODULES // [];
        my $current_dist = %ENV<RAKUDO_PRECOMP_DIST>;
        %ENV<RAKUDO_PRECOMP_DIST> = $*RESOURCES ?? $*RESOURCES.Str !! '{}';

        $RMD("Precompiling $path into $io.tmp") if $RMD;
        my $perl6 = $*EXECUTABLE.subst('perl6-debug', 'perl6'); # debugger would try to precompile it's UI
        my $proc = run(
          $perl6,
          $lle,
          $profile,
          "--target=" ~ Rakudo::Internals.PRECOMP-TARGET,
          "--output=$io.tmp",
          "--source-name=$source-name",
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
        $RMD("Precompiled $path into $io.tmp") if $RMD;
        my str $dependencies = '';
        for @result -> $dependency {
            unless $dependency ~~ /^<[A..Z0..9]> ** 40 \0 .+/ {
                say $dependency;
                next
            }
            my ($dependency-id, $dependency-src, $dependency-spec) = $dependency.split("\0", 3);
            $RMD("id: $dependency-id, src: $dependency-src, spec: $dependency-spec") if $RMD;
            my $path = self.store.path($compiler-id, $dependency-id);
            if $path.e {
                spurt($path ~ '.rev-deps', "$id\n", :append);
            }
            $dependencies ~= "$dependency\n";
        }
        $RMD("Writing dependencies and byte code to $io") if $RMD;
        my $precomp-file = $io.open(:w);
        $precomp-file.print($*REPO.id ~ "\n" ~ $dependencies ~ "\n");
        $RMD("$io.tmp exists: " ~ ("$io.tmp".IO.e)) if $RMD;
        $precomp-file.write("$io.tmp".IO.slurp(:bin));
        $precomp-file.close;
        self.store.unlock;
        True
    }
}

# vim: ft=perl6 expandtab sw=4
