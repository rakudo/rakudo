{
    role CompUnit::PrecompilationRepository {
        method try-load(
            CompUnit::PrecompilationDependency::File $dependency,
            IO::Path :$source,
            CompUnit::PrecompilationStore :@precomp-stores,
            --> CompUnit::Handle:D) {
            Nil
        }

        method load(CompUnit::PrecompilationId $id --> Nil) { }

        method may-precomp(--> Bool:D) {
            True # would be a good place to check an environment variable
        }
    }
}

BEGIN CompUnit::PrecompilationRepository::<None> := CompUnit::PrecompilationRepository.new;

class CompUnit { ... }
class CompUnit::PrecompilationRepository::Default does CompUnit::PrecompilationRepository {
    has CompUnit::PrecompilationStore $.store;
    my %loaded;
    my $loaded-lock = Lock.new;
    my $first-repo-id;

    my $lle;
    my $profile;
    my $optimize;

    method try-load(
        CompUnit::PrecompilationDependency::File $dependency,
        IO::Path :$source = $dependency.src.IO,
        CompUnit::PrecompilationStore :@precomp-stores = Array[CompUnit::PrecompilationStore].new($.store),
     --> CompUnit::Handle:D) {
        my $RMD = $*RAKUDO_MODULE_DEBUG;
        my $id = $dependency.id;
        $RMD("try-load $id: $source") if $RMD;

        # Even if we may no longer precompile, we should use already loaded files
        $loaded-lock.protect: {
            return %loaded{$id} if %loaded{$id}:exists;
        }

        my ($handle, $checksum) = (
            self.may-precomp and (
                my $loaded = self.load($id, :source($source), :checksum($dependency.checksum), :@precomp-stores) # already precompiled?
                or self.precompile($source, $id, :source-name($dependency.source-name), :force($loaded ~~ Failure), :@precomp-stores)
                    and self.load($id, :@precomp-stores) # if not do it now
            )
        );

        if $*W and $*W.record_precompilation_dependencies {
            if $handle {
                $dependency.checksum = $checksum;
                say $dependency.serialize;
            }
            else {
                nqp::exit(0);
            }
        }

        $handle ?? $handle !! Nil
    }

    method !load-handle-for-path(CompUnit::PrecompilationUnit $unit) {
        my $preserve_global := nqp::ifnull(nqp::gethllsym('perl6', 'GLOBAL'), Mu);
        if $*RAKUDO_MODULE_DEBUG -> $RMD { $RMD("Loading precompiled\n$unit") }
#?if !jvm
        my $handle := CompUnit::Loader.load-precompilation-file($unit.bytecode-handle);
        $unit.close;
#?endif
#?if jvm
        my $handle := CompUnit::Loader.load-precompilation($unit.bytecode);
#?endif
        nqp::bindhllsym('perl6', 'GLOBAL', $preserve_global);
        CATCH {
            default {
                nqp::bindhllsym('perl6', 'GLOBAL', $preserve_global);
                .throw;
            }
        }
        $handle
    }

    method !load-file(
        CompUnit::PrecompilationStore @precomp-stores,
        CompUnit::PrecompilationId $id,
        :$repo-id,
        :$refresh,
    ) {
        my $compiler-id = CompUnit::PrecompilationId.new-without-check($*PERL.compiler.id);
        my $RMD = $*RAKUDO_MODULE_DEBUG;
        for @precomp-stores -> $store {
            $RMD("Trying to load {$id ~ ($repo-id ?? '.repo-id' !! '')} from $store.prefix()") if $RMD;
            $store.remove-from-cache($id) if $refresh;
            my $file = $repo-id
                ?? $store.load-repo-id($compiler-id, $id)
                !! $store.load-unit($compiler-id, $id);
            return $file if $file;
        }
        Nil
    }

    method !load-dependencies(CompUnit::PrecompilationUnit:D $precomp-unit, @precomp-stores) {
        my $compiler-id = CompUnit::PrecompilationId.new-without-check($*PERL.compiler.id);
        my $RMD = $*RAKUDO_MODULE_DEBUG;
        my $resolve = False;
        my $repo = $*REPO;
        $first-repo-id //= $repo.id;
        my $repo-id = self!load-file(@precomp-stores, $precomp-unit.id, :repo-id);
        if $repo-id ne $repo.id {
            $RMD("Repo changed: $repo-id ne {$repo.id}. Need to re-check dependencies.") if $RMD;
            $resolve = True;
        }
        if $repo-id ne $first-repo-id {
            $RMD("Repo chain changed: $repo-id ne {$first-repo-id}. Need to re-check dependencies.") if $RMD;
            $resolve = True;
        }
        $resolve = False unless %*ENV<RAKUDO_RERESOLVE_DEPENDENCIES> // 1;
        my @dependencies;
        for $precomp-unit.dependencies -> $dependency {
            $RMD("dependency: $dependency") if $RMD;

            if $resolve {
                my $comp-unit = $repo.resolve($dependency.spec);
                $RMD("Old id: $dependency.id(), new id: {$comp-unit.repo-id}") if $RMD;
                return False unless $comp-unit and $comp-unit.repo-id eq $dependency.id;
            }

            my $dependency-precomp = @precomp-stores
                .map({ $_.load-unit($compiler-id, $dependency.id) })
                .first(*.defined)
                or do {
                    $RMD("Could not find $dependency.spec()") if $RMD;
                    return False;
                }
            unless $dependency-precomp.is-up-to-date($dependency, :check-source($resolve)) {
                $dependency-precomp.close;
                return False;
            }

            @dependencies.push: $dependency-precomp;
        }

        $loaded-lock.protect: {
            for @dependencies -> $dependency-precomp {
                unless %loaded{$dependency-precomp.id}:exists {
                    %loaded{$dependency-precomp.id} = self!load-handle-for-path($dependency-precomp);
                }
            }
        }

        # report back id and source location of dependency to dependant
        if $*W and $*W.record_precompilation_dependencies {
            for $precomp-unit.dependencies -> $dependency {
                say $dependency.serialize;
            }
        }

        if $resolve {
            self.store.store-repo-id($compiler-id, $precomp-unit.id, :repo-id($repo.id));
        }
        True
    }

    proto method load(|) {*}

    multi method load(
        Str $id,
        Instant :$since,
        IO::Path :$source,
        CompUnit::PrecompilationStore :@precomp-stores = Array[CompUnit::PrecompilationStore].new($.store),
    ) {
        self.load(CompUnit::PrecompilationId.new($id), :$since, :@precomp-stores)
    }

    multi method load(
        CompUnit::PrecompilationId $id,
        IO::Path :$source,
        Str :$checksum is copy,
        Instant :$since,
        CompUnit::PrecompilationStore :@precomp-stores = Array[CompUnit::PrecompilationStore].new($.store),
    ) {
        $loaded-lock.protect: {
            return %loaded{$id} if %loaded{$id}:exists;
        }
        my $RMD = $*RAKUDO_MODULE_DEBUG;
        my $compiler-id = CompUnit::PrecompilationId.new-without-check($*PERL.compiler.id);
        my $unit = self!load-file(@precomp-stores, $id);
        if $unit {
            if (not $since or $unit.modified > $since)
                and (not $source or ($checksum //= nqp::sha1($source.slurp(:enc<iso-8859-1>))) eq $unit.source-checksum)
                and self!load-dependencies($unit, @precomp-stores)
            {
                my \loaded = self!load-handle-for-path($unit);
                $loaded-lock.protect: { %loaded{$id} = loaded };
                return (loaded, $unit.checksum);
            }
            else {
                $RMD("Outdated precompiled {$unit}{$source ?? " for $source" !! ''}\n"
                     ~ "    mtime: {$unit.modified}{$since ?? ", since: $since" !! ''}\n"
                     ~ "    checksum: {$unit.source-checksum}, expected: $checksum") if $RMD;
                $unit.close;
                fail "Outdated precompiled $unit";
            }
        }
        Nil
    }

    proto method precompile(|) {*}

    multi method precompile(
        IO::Path:D $path,
        Str $id,
        Bool :$force = False,
        :$source-name = $path.Str
    ) {
        self.precompile($path, CompUnit::PrecompilationId.new($id), :$force, :$source-name)
    }

    multi method precompile(
        IO::Path:D $path,
        CompUnit::PrecompilationId $id,
        Bool :$force = False,
        :$source-name = $path.Str,
        :$precomp-stores,
    ) {
        my $compiler-id = CompUnit::PrecompilationId.new-without-check($*PERL.compiler.id);
        my $io = self.store.destination($compiler-id, $id);
        return False unless $io;
        my $RMD = $*RAKUDO_MODULE_DEBUG;
        if $force
            ?? (
                $precomp-stores
                and my $unit = self!load-file($precomp-stores, $id, :refresh)
                and do {
                    LEAVE $unit.close;
                    nqp::sha1($path.slurp(:enc<iso-8859-1>)) eq $unit.source-checksum
                    and self!load-dependencies($unit, $precomp-stores)
                }
            )
            !! ($io.e and $io.s)
        {
            $RMD("$source-name\nalready precompiled into\n{$io}{$force ?? ' by another process' !! ''}") if $RMD;
            with %*COMPILING<%?OPTIONS><stagestats> {
                note "\n    load    $path.relative()";
                $*ERR.flush;
            }
            self.store.unlock;
            return True;
        }
        my $source-checksum = nqp::sha1($path.slurp(:enc<iso-8859-1>));
        my $bc = "$io.bc".IO;

        $lle     //= Rakudo::Internals.LL-EXCEPTION;
        $profile //= Rakudo::Internals.PROFILE;
        $optimize //= Rakudo::Internals.OPTIMIZE;
        my %env = %*ENV; # Local copy for us to tweak
        %env<RAKUDO_PRECOMP_WITH> = $*REPO.repo-chain.map(*.path-spec).join(',');

        my $rakudo_precomp_loading = %env<RAKUDO_PRECOMP_LOADING>;
        my $modules = $rakudo_precomp_loading ?? Rakudo::Internals::JSON.from-json: $rakudo_precomp_loading !! [];
        die "Circular module loading detected trying to precompile $path" if $modules.Set{$path.Str}:exists;
        %env<RAKUDO_PRECOMP_LOADING> = Rakudo::Internals::JSON.to-json: [|$modules, $path.Str];
        %env<RAKUDO_PRECOMP_DIST> = $*DISTRIBUTION ?? $*DISTRIBUTION.serialize !! '{}';

        $RMD("Precompiling $path into $bc ($lle $profile $optimize)") if $RMD;
        my $perl6 = $*EXECUTABLE.absolute
            .subst('perl6-debug', 'perl6') # debugger would try to precompile it's UI
            .subst('perl6-gdb', 'perl6')
            .subst('perl6-jdb-server', 'perl6-j') ;
        if %env<RAKUDO_PRECOMP_NESTED_JDB> {
            $perl6.subst-mutate('perl6-j', 'perl6-jdb-server');
            note "starting jdb on port " ~ ++%env<RAKUDO_JDB_PORT>;
        }
        my $out = '';
        my $err = '';
        my $status;
        #note "running PrecompRepo.precompile";
        #note "is stagestats set? { +%*COMPILING<%?OPTIONS><stagestats> } { ~%*COMPILING<%?OPTIONS><stagestats> }";
        with %*COMPILING<%?OPTIONS><stagestats> {
            note "\n    precomp $path.relative()";
            $*ERR.flush;
        }
        react {
            my $proc = Proc::Async.new(
                $perl6,
                $lle,
                $profile,
                $optimize,
                "--target=" ~ Rakudo::Internals.PRECOMP-TARGET,
                "--output=$bc",
                "--source-name=$source-name",
                |("--stagestats" with %*COMPILING<%?OPTIONS><stagestats>),
                $path
            );

            whenever $proc.stdout {
                $out ~= $_
            }
            unless $RMD {
                whenever $proc.stderr {
                    $err ~= $_
                }
            }
            with %*COMPILING<%?OPTIONS><stagestats> {
                whenever $proc.stderr.lines {
                    note("    $_");
                    $*ERR.flush;
                }
            }
            whenever $proc.start(ENV => %env) {
                $status = .exitcode
            }
        }

        my @result is List = $out.lines.unique;
        if $status {  # something wrong
            self.store.unlock;
            $RMD("Precompiling $path failed: $status") if $RMD;
            Rakudo::Internals.VERBATIM-EXCEPTION(1);
            die $RMD ?? @result !! $err;
        }

        if not $RMD and not %*COMPILING<%?OPTIONS><stagestats>:exists and $err -> $warnings {
            $*ERR.print($warnings);
        }
        unless $bc.e {
            $RMD("$path aborted precompilation without failure") if $RMD;
            self.store.unlock;
            return False;
        }
        $RMD("Precompiled $path into $bc") if $RMD;
        my str $dependencies = '';
        my CompUnit::PrecompilationDependency::File @dependencies;
        my %dependencies;
        for @result -> $dependency-str {
            unless $dependency-str ~~ /^<[A..Z0..9]> ** 40 \0 .+/ {
                say $dependency-str;
                next
            }
            my $dependency = CompUnit::PrecompilationDependency::File.deserialize($dependency-str);
            next if %dependencies{$dependency.Str}++; # already got that one
            $RMD($dependency.Str()) if $RMD;
            @dependencies.push: $dependency;
        }
        $RMD("Writing dependencies and byte code to $io.tmp for source checksum: $source-checksum") if $RMD;
        self.store.store-unit(
            $compiler-id,
            $id,
            self.store.new-unit(:$id, :@dependencies, :$source-checksum, :bytecode($bc.slurp(:bin))),
        );
        $bc.unlink;
        self.store.store-repo-id($compiler-id, $id, :repo-id($*REPO.id));
        self.store.unlock;
        True
    }
}

# vim: ft=perl6 expandtab sw=4
