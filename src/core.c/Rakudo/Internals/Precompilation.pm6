my class CompUnit::PrecompilationDependency::File { ... }

my class Rakudo::Internals::Precompilation {
    # easy access to compile options
    my Mu $compiling-options :=
      nqp::ifnull(nqp::atkey(%*COMPILING,'%?OPTIONS'),nqp::hash);

    # running with --ll-exception
    my $LL-EXCEPTION := nqp::existskey($compiling-options, 'll-exception')
      ?? '--ll-exception'
      !! Empty;
    method LL-EXCEPTION() { $LL-EXCEPTION }

    # running with --profile
    my $PROFILE := nqp::existskey($compiling-options, 'profile')
      ?? '--profile'
      !! Empty;
    method PROFILE() { $PROFILE }

    # running with --optimize=X
    my $OPTIMIZE := nqp::existskey($compiling-options, 'optimize')
      ?? '--optimize=' ~ nqp::atkey($compiling-options, 'optimize')
      !! Empty;
    method OPTIMIZE() { $OPTIMIZE }

    # running with --stagestats
    my $STAGESTATS := nqp::existskey($compiling-options, 'stagestats')
      ?? '--stagestats'
      !! Empty;
    method STAGESTATS() { $STAGESTATS }

    # whatever specified with -I
    my $INCLUDE := nqp::existskey($compiling-options,'I')
      ?? do {
             my $I := nqp::atkey($compiling-options,'I');
             nqp::islist($I) ?? $I !! nqp::list($I)
         }
      !! nqp::list;
    method INCLUDE() { $INCLUDE }

#?if moar
    method PRECOMP-EXT(--> "moarvm") { }
    method PRECOMP-TARGET(--> "mbc") { }
#?endif
#?if jvm
    method PRECOMP-EXT(   --> "jar") { }
    method PRECOMP-TARGET(--> "jar") { }
#?endif
#?if js
    method PRECOMP-EXT(   --> "js") { }
    method PRECOMP-TARGET(--> "js") { }
#?endif

    method compile-file-externally(:$path, :$output, :$source-name, :$lle) {
        my $RMD := $*RAKUDO_MODULE_DEBUG;

        # Local copy for us to tweak
        my $env := nqp::clone(nqp::getattr(%*ENV,Map,'$!storage'));
        nqp::bindkey($env,'RAKUDO_PRECOMP_WITH',
          $*REPO.repo-chain.map(*.path-spec).join(',')
        );

        my $rpl := nqp::atkey($env,'RAKUDO_PRECOMP_LOADING');
        if $rpl {
            nqp::bindkey($env,'RAKUDO_PRECOMP_LOADING',
              $rpl.chop
                ~ ','
                ~ Rakudo::Internals::JSON.to-json($path.Str)
                ~ ']');
        }
        else {
            nqp::bindkey($env,'RAKUDO_PRECOMP_LOADING',
              '[' ~ Rakudo::Internals::JSON.to-json($path.Str) ~ ']');
        }

        my $distribution := $*DISTRIBUTION;
        nqp::bindkey($env,'RAKUDO_PRECOMP_DIST',
          $distribution ?? $distribution.serialize !! '{}');
        my $raku := $*EXECUTABLE.absolute
            .subst('perl6-debug', 'perl6') # debugger would try to precompile it's UI
            .subst('perl6-gdb', 'perl6')
            .subst('perl6-jdb-server', 'perl6-j') ;
#?if jvm
        if nqp::atkey($env,'RAKUDO_PRECOMP_NESTED_JDB') {
            $raku.subst-mutate('perl6-j', 'perl6-jdb-server');
            note "starting jdb on port "
              ~ nqp::bindkey($env,'RAKUDO_JDB_PORT',
                  nqp::ifnull(nqp::atkey($env,'RAKUDO_JDB_PORT'),0) + 1
                );
        }
#?endif

        if $STAGESTATS {
            note "\n    precomp $path.relative()";
            $*ERR.flush;
        }

        my $out := nqp::list_s;
        my $err := nqp::list_s;
        my $status;
            nqp::hllize($env).note;
            note join " ", (
                $raku,
                $lle ?? '--ll-exception' !! Empty,
                $PROFILE,
                $OPTIMIZE,
                "--target={self.PRECOMP-TARGET}",
                $STAGESTATS,
                "--output=$output",
                "--source-name=$source-name",
                $path
            );
        react {
            my $proc = Proc::Async.new(
                $raku,
                $lle ?? '--ll-exception' !! Empty,
                $PROFILE,
                $OPTIMIZE,
                "--target={self.PRECOMP-TARGET}",
                $STAGESTATS,
                "--output=$output",
                "--source-name=$source-name",
                $path
            );

            whenever $proc.stdout {
                nqp::push_s($out,$_);
            }
            unless $RMD {
                whenever $proc.stderr {
                    nqp::push_s($err,$_);
                }
            }
            if $STAGESTATS {
                whenever $proc.stderr.lines {
                    note("    $_");
                    $*ERR.flush;
                }
            }
            whenever $proc.start(ENV => nqp::hllize($env)) {
                $status = .exitcode
            }
        }
 
        if $status {  # something wrong
            $RMD("Precompiling $path failed: $status")
              if $RMD;

            Rakudo::Internals.VERBATIM-EXCEPTION(1);
            die $RMD
              ?? nqp::join('',$out).lines.unique.List
              !! nqp::join('',$err);
        }
        if nqp::elems($err) && not($RMD || $STAGESTATS) {
            $*ERR.print(nqp::join('',$err));
        }

        unless Rakudo::Internals.FILETEST-ES($output.absolute) {
            X::Pragma::CannotPrecomp.new.throw;
        }

        $RMD("Precompiled $path into $output")
          if $RMD;

        my $dependencies := nqp::create(IterationBuffer);
        my $seen := nqp::hash;

        for nqp::join('',$out).lines.unique -> str $outstr {
            if nqp::atpos(nqp::radix_I(16,$outstr,0,0,Int),2) == 40
              && nqp::eqat($outstr,"\0",40)
              && nqp::chars($outstr) > 41 {
                my $dependency :=
                  CompUnit::PrecompilationDependency::File.deserialize($outstr);
                if $dependency {
                    my str $dependency-str = $dependency.Str;
                    unless nqp::existskey($seen,$dependency-str) {
                        $RMD($dependency-str)
                          if $RMD;

                        nqp::bindkey($seen,$dependency-str,1);
                        nqp::push($dependencies,$dependency);
                    }
                }
            }

            # huh?  malformed dependency?
            else {
                say $outstr;
            }
        }

        # HLLize dependencies
        my @dependencies;
        nqp::bindattr(@dependencies,List,'$!reified',$dependencies);

        @dependencies
    }

    method compile-file(:$path, :$output, :$source-name) {
        #X::AlreadyPrecompiling.new(:$source-name).throw if $*W and $*W.is_precompilation_mode and $*CURI-INSTALLING;
        my Mu $opts := nqp::atkey(%*COMPILING, '%?OPTIONS');
        my $lle = !nqp::isnull($opts) && !nqp::isnull(nqp::atkey($opts, 'll-exception'))
          ?? True
          !! False;

        my $current_compiler := nqp::getcomp('Raku');
        my $compiler := $current_compiler.WHAT.new;
        $compiler.parsegrammar($current_compiler.parsegrammar);
        $compiler.parseactions($current_compiler.parseactions);
        $compiler.addstage('syntaxcheck', :before<ast>);
        $compiler.addstage('optimize', :after<ast>);

        my $end_phasers := nqp::gethllsym('Raku', '@END_PHASERS');
        nqp::bindhllsym('Raku', '@END_PHASERS', my $new_end_phasers := nqp::list);

        nqp::bindattr($compiler, HLL::Compiler, '$!user_progname', $path);
        my $source = $path.IO.slurp;
        my $dependencies;
        {
            my $preserve_global := nqp::ifnull(nqp::gethllsym('Raku','GLOBAL'),Mu);
            my $global := Metamodel::PackageHOW.new_type(:name<GLOBAL>);
            nqp::bindhllsym('Raku', 'GLOBAL', $global);
            my $?FILES := $source-name;
            my $*CTXSAVE;
            my $*W;
            my %*COMPILING;
            my %adverbs =
                :ll-exception($lle),
                :precomp(1),
                :target(self.PRECOMP-TARGET),
                :$output,
                :encoding('utf8'),
                :$source-name,
                :transcode('ascii iso-8859-1');
            %*COMPILING<%?OPTIONS> := %adverbs.FLATTENABLE_HASH;
            %*COMPILING<dependencies> := Array.new;
            my $*LINEPOSCACHE;
            my $result := $source;
            my $sc;
            my $package-symbols := nqp::list;
            my $*PACKAGE-SYMBOLS := $package-symbols;
            my $allowed_sc_deps := nqp::list;
            my $comp := nqp::getcomp('Raku');
            nqp::push($allowed_sc_deps, nqp::getobjsc($comp.parseactions));
            nqp::push($allowed_sc_deps, nqp::getobjsc($comp.parsegrammar));
            nqp::push($allowed_sc_deps, nqp::getobjsc($comp.WHAT));
            my $*ALLOWED_SC_DEPS := $allowed_sc_deps;

            nqp::force_gc; # Make sure no SCs from aborted compilations linger around

            for $compiler.stages -> $stage {
                $result := $compiler.execute_stage(
                    $stage,
                    $result,
                    nqp::getattr(%adverbs, Map, '$!storage'),
                );
                if $result.isa(QAST::CompUnit) {
                    $sc := $result.sc;
                    nqp::bindattr($sc, $sc.WHAT, 'allowed_sc_deps', $allowed_sc_deps);
                }
                last if $stage eq self.PRECOMP-TARGET;
            }

            while nqp::elems($package-symbols) {
                my $package := nqp::shift($package-symbols);
                my $name := nqp::shift($package-symbols);
                #note "Deleting $name from {$package.Str} { nqp::objectid($package) }";
                nqp::scwbdisable();
                $package.DELETE-KEY(nqp::box_s($name, Str));
                nqp::scwbenable();
            }

            LEAVE {
                nqp::bindhllsym('Raku', 'GLOBAL', $preserve_global);
            }

            nqp::scdisclaim($sc);

            $dependencies = %*COMPILING<dependencies>;
            $dependencies = $dependencies.clone
                if $dependencies;
            CATCH {
                my $sc := nqp::popcompsc();
                nqp::scdisclaim($sc);
                nqp::force_gc; # Try to get rid of the aborted compliation's SC
                when X::Pragma::CannotPrecomp {
                    .throw;
                }
                when X::AlreadyPrecompiling {
                    .throw;
                }
                when X::CircularModuleLoading {
                    .throw;
                }
                default {
                    note("In-process precompilation failed due to $_ in $_.backtrace.full(), trying again in an external process");
                    $dependencies = self.compile-file-externally(:$path, :$output, :$source-name, :$lle);
                    #.throw;
                }
            }
        }

        nqp::splice($end_phasers, $new_end_phasers, nqp::elems($end_phasers), 0);
        LEAVE {
            nqp::bindhllsym('Raku', '@END_PHASERS', $end_phasers);
        }
        $dependencies ?? $dependencies.list !! Empty;
    }
}
