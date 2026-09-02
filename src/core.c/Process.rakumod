augment class Rakudo::Internals {
    my int $last-dst = -1;  # never matches initially
    my int $TZ-was-set-explicitly;
    my int $TZ-offset;

    method GET-LOCAL-TIMEZONE-OFFSET() {
        if $TZ-was-set-explicitly {
            $TZ-offset
        }

        # not set explicitly
        else {
            my int $utc = nqp::div_i(nqp::time,1000000000);
            my $lt     := nqp::decodelocaltime($utc);

            # first time, or possible DST change
            if nqp::isne_i(nqp::atpos_i($lt,8),$last-dst) {
                $last-dst = nqp::atpos_i($lt,8);

                # algorithm from Claus Tøndering
                my int $a = (14 - nqp::atpos_i($lt,4)) div 12;
                my int $y = nqp::atpos_i($lt,5) + 4800 - $a;
                my int $m = nqp::atpos_i($lt,4) + 12 * $a - 3;
                my int $jd = nqp::atpos_i($lt,3) + (153 * $m + 2) div 5
                  + 365 * $y + $y div 4 - $y div 100 + $y div 400 - 32045;
                $TZ-offset = (
                  ($jd - 2440588) * 86400
                    + nqp::atpos_i($lt,2) * 3600
                    + nqp::atpos_i($lt,1) * 60
                    + nqp::atpos_i($lt,0)
                ) - $utc
            }

            # cannot have been a DST change
            else {
                $TZ-offset
            }
        }
    }

    my Lock $fetch-lock := Lock.new;
    method FETCH-USER-GROUP(Str:D $what) is raw {
        $fetch-lock.protect: {
            my $stash := nqp::who(PROCESS);
            unless $stash.EXISTS-KEY($what) {
                if self.IS-WIN {
                    if $what eq '$USER' {
                        $stash.BIND-KEY('$USER',try qx/whoami/.chomp);
                    }
                    # $what eq '$GROUP'
                    elsif (try qx|whoami /groups /FO csv /nh|) -> $groups {
                        $stash.BIND-KEY(
                          '$GROUP', $groups.split('","',2).head.substr(1)
                        );
                    }
                    # alas
                    else {
                        $stash.BIND-KEY('$GROUP',Nil);
                    }
                }
                elsif (try qx/LC_MESSAGES=POSIX id/) -> $id {
                    if $id ~~ m/^
                      [ uid "=" $<uid>=(\d+) ]
                      [ "(" $<user>=(<-[ ) ]>+) ")" ]
                      \s+
                      [ gid "=" $<gid>=(\d+) ]
                      [ "(" $<group>=(<-[ ) ]>+) ")" ]
                    / {
                        $stash.BIND-KEY('$USER', IntStr.new(+$<uid>,~$<user> ));
                        $stash.BIND-KEY('$GROUP',IntStr.new(+$<gid>,~$<group>));
                    }

                    # alas, no support yet
                    else {
                        $stash.BIND-KEY('$USER',  Nil);
                        $stash.BIND-KEY('$GROUP', Nil);
                    }
                }
            }
            $stash.AT-KEY($what)
        }
    }

    my $initializers := nqp::hash(
      '$*ARGFILES', my sub ARGFILES() is raw {
          PROCESS::<$ARGFILES> := do if @*ARGS -> @ARGS {
              if %*SUB-MAIN-OPTS<dash-as-STDIN> {
                  $_ = $*IN if $_ eq '-' for @ARGS;
              }
              IO::ArgFiles.new(@ARGS)
          }
          else {
              $*IN
          }
      },

      '@*ARGS', my sub ARGS() is raw {
          my @ARGS;
          my Mu $argiter := nqp::getcurhllsym('$!ARGITER');
          @ARGS.push(nqp::p6box_s(nqp::shift($argiter))) while $argiter;
          PROCESS::<@ARGS> := @ARGS
      },

      '$*COLLATION', my sub COLLATION() is raw {
          PROCESS::<$COLLATION> := Collation.new
      },

      '$*DEFAULT-READ-ELEMS', my sub DEFAULT-READ-ELEMS() is raw {
          PROCESS::<$DEFAULT_READ_ELEMS> :=
            %*ENV<RAKUDO_DEFAULT_READ_ELEMS> // 65536;
      },

      '$*DISTRO', my sub DISTRO() is raw {
#?if jvm
          my $properties := VM.new.properties;
          my $name       := $properties<os.name>;
          my $version    := $properties<os.version>;
          my $path-sep   := $properties<path.separator>;
#?endif
#?if !jvm
          my $config   := VM.new.config;
          my $name     := $config<osname>;
          my $version  := $config<osvers>;
          my $path-sep := $name eq 'MSWin32' ?? ';' !! ':';
#?endif
          my Str $release := "unknown";
          my Str $auth    := "unknown";
          my Str $desc    := "unknown";

          # helper sub to convert key:value lines into a hash
          sub kv2Map(Str:D $text, str $delimiter --> Map:D) {
              my $hash := nqp::hash;
              for $text.lines -> str $line {
                  my $parts := nqp::split($delimiter,$line);
                  if nqp::elems($parts) > 1 {
                      nqp::bindkey(
                        $hash,
                        nqp::shift($parts),
                        nqp::hllize(
                          nqp::elems($parts) == 2
                            ?? nqp::shift($parts)
                            !! nqp::join($delimiter,$parts)
                        ).trim
                      );
                  }
              }

              nqp::p6bindattrinvres(nqp::create(Map),Map,'$!storage',$hash)
          }

          # darwin specific info
          if $name eq 'darwin' {
              my $lookup :=
                kv2Map(shell("sw_vers", :out, :err).out.slurp(:close),':');
              $name    := $_ with $lookup<ProductName>;
              $version := $_ with $lookup<ProductVersion>;
              $release := $_ with $lookup<BuildVersion>;
              $auth    := 'Apple Inc.'; # presumably

#?if !js
              my constant $names = nqp::hash(
#?endif
#?if js
              my $names := nqp::hash(
#?endif
                '10.0',  'Cheetah',
                '10.1',  'Puma',
                '10.2',  'Jaguar',
                '10.3',  'Panther',
                '10.4',  'Tiger',
                '10.5',  'Leopard',
                '10.6',  'Snow Leopard',
                '10.7',  'Lion',
                '10.8',  'Mountain Lion',
                '10.9',  'Mavericks',
                '10.10', 'Yosemite',
                '10.11', 'El Capitan',
              );

              # Obtain the description from the HTML version of the license
              # that is shown to users when the system is set up.  Versions up
              # to El Capitan where labeled OSX with unknown variations to the
              # license agreement, so we try those first from a static list.
              $desc := nqp::ifnull(
                nqp::atkey($names,$version),
                (Q|/System/Library/CoreServices/Setup Assistant.app/Contents/Resources/en.lproj/OSXSoftwareLicense.html|.IO.slurp.match(
                  /"SOFTWARE LICENSE AGREEMENT FOR macOS " <( <-[<]>+/
                ) // "<unknown>").Str
              );
          }
          elsif Rakudo::Internals.FILETEST-E('/etc/os-release') {
              my $lookup := kv2Map('/etc/os-release'.IO.slurp.subst(:g,'"'),'=');
              $name    := $_ with $lookup<ID>;
              $auth    := $_ with $lookup<HOME_URL>;
              $version := $_ with $lookup<VERSION>;
              $release := $_ with $lookup<VERSION_ID>;
              $desc    := $_ with $lookup<PRETTY_NAME>;
          }
          elsif $name eq 'linux' {
              my $lookup :=
                kv2Map(shell(<lsb_release -a>, :out, :err).out.slurp(:close),":");
              $auth    := $_ with $lookup<<"DISTRIBUTOR ID">>;
              $desc    := $_ with $lookup<DESCRIPTION>;
              $release := $_ with $lookup<RELEASE>;
          }

          $version := $version.Version;  # make sure it is a Version
          PROCESS::<$*DISTRO> := Distro.new(
            :$name, :$version, :$release, :$auth, :$path-sep, :$desc
          );
      },

      '%*ENV', my sub ENV() is raw {
          my %env;
          my $iter := nqp::iterator(nqp::getenvhash);
          nqp::while(
            $iter,
            %env.ASSIGN-KEY(
              nqp::iterkey_s(nqp::shift($iter)),
              val(nqp::iterval($iter))
            )
          );
          PROCESS::<%ENV> := %env
      },

      '$*EXECUTABLE', my sub EXECUTABLE() is raw {
          PROCESS::<$EXECUTABLE> := IO::Path.new(:CWD(INIT nqp::cwd()),
            nqp::execname()
#?if jvm
            || $*VM.properties<perl6.prefix> ~ '/bin/perl6-j'
#?endif
#?if moar
            || ($*VM.config<prefix> ~ '/bin/'
              ~ ($*VM.config<osname> eq 'MSWin32' ?? 'perl6-m.exe' !! 'perl6-m'))
#?endif
#?if js
            // ($*VM.config<prefix> ~ '/bin/'
              ~ ($*VM.config<osname> eq 'MSWin32' ?? 'perl6-js.bat' !! 'perl6-js'))
#?endif
        )
      },

      '$*EXECUTABLE-NAME', my sub EXECUTABLE-NAME() is raw {
          PROCESS::<$EXECUTABLE-NAME> := $*EXECUTABLE.basename
      },

      '&*EXIT', my sub EXIT() is raw {
          PROCESS::<&EXIT> := my sub exit($status) {
              state $exit = $status;  # first call to exit sets value

              $*EXIT = $exit;
              nqp::getcurhllsym('&THE_END')()
                ?? $exit
                !! nqp::exit(nqp::unbox_i($exit.Int))
          }
      },

      '$*GROUP', my sub GROUP() is raw {
          Rakudo::Internals.FETCH-USER-GROUP('$GROUP')
      },

      '$*HOME', my sub HOME() is raw {
          my $HOME is default(Nil);

          if %*ENV<HOME> -> $home {
              $HOME = $home;
          }
          elsif Rakudo::Internals.IS-WIN {
              my $env := %*ENV;
              $env<HOMEDRIVE> && $env<HOMEPATH> && ($HOME
                = nqp::concat($env<HOMEDRIVE>, $env<HOMEPATH>));
          }

          PROCESS::<$HOME> := $HOME
            ?? ($HOME = IO::Path.new($HOME))
            !! $HOME # keep Nil default
      },

      '$*INIT-INSTANT', my sub INIT-INSTANT() is raw {
          PROCESS::<$INIT-INSTANT> := nqp::p6bindattrinvres(
            nqp::create(Instant),
            Instant,
            '$!tai',
            (Rakudo::Internals.tai-from-posix(
              Rakudo::Internals.INITTIME,0) * 1000000000
            ).Int
          )
      },

      '$*KERNEL', my sub KERNEL() is raw {
          PROCESS::<KERNEL> := Kernel.new
      },

      # XXX TODO: https://github.com/rakudo/rakudo/issues/2433
      # my $perl := BEGIN Perl.new;
      '$*PERL', my sub PERL() is raw {
          PROCESS::<$PERL> := Raku.new
      },

      '$*PROGRAM', my sub PROGRAM() is raw {
          PROCESS::<$PROGRAM> :=
            IO::Path.new(:CWD(INIT nqp::cwd()), $*PROGRAM-NAME)
      },

      '$*PROGRAM-NAME', my sub PROGRAM-NAME() is raw {
          PROCESS::<PROGRAM-NAME> :=
            my $ = nqp::getcomp('Raku').user-progname;
      },

      '$*RAKU', my sub RAKU() is raw {
          PROCESS::<$RAKU> := Raku.new
      },

      '$*RAKUDO_MODULE_DEBUG', my sub RAKUDO_MODULE_DEBUG() is raw {
          PROCESS::<$RAKUDO_MODULE_DEBUG> := ?%*ENV<RAKUDO_MODULE_DEBUG>
            ?? -> *@str --> Nil {
                   state $level = %*ENV<RAKUDO_MODULE_DEBUG>++;
                   state $root  = $*CWD.Str;
                   my $indent = (($level - 1) * 4) + 1;
                   note sprintf "%2d%sRMD: %s",
                     $level,
                     " " x $indent,
                     @str>>.indent(7 + $indent)
                       .join("\n")
                       .substr(7 + $indent)
                       .subst($root, '.');
               }
            !! ?%*ENV<RAKUDO_PRECOMPILATION_PROGRESS>
              ?? -> $note --> Nil {
                     state $level = %*ENV<RAKUDO_PRECOMPILATION_PROGRESS>++ - 1;
                     state $module;
                     my $message := $note.trim-leading;
                     if $message.starts-with("Late loading '") {
                         $module = $message.substr(14, *-1);
                     }
                     elsif $message.starts-with("Precompiling ") {
                         note " " x $level ~ "Precompiling $module";
                     }
                 }
              !! False
      },

          '$*REPO', my sub REPO() is raw {
              my $repo := PROCESS::<$REPO> :=
                CompUnit::RepositoryRegistry.setup-repositories;

              my $world := $*W;
              my $cu    := $*CU;
              $world.suspend_recording_precompilation_dependencies if $world;
              $cu.suspend-recording-precompilation-dependencies    if $cu;
              CompUnit::RepositoryRegistry.resolve-unknown-repos($repo);
              $cu.resume-recording-precompilation-dependencies     if $cu;
              $world.resume_recording_precompilation_dependencies  if $world;

              # Cannot be $repo, as CU:RepositoryRegistry changes $*REPO
              PROCESS::<$REPO>
          },

          '$*SCHEDULER', my sub SCHEDULER() is raw {
#?if !js
              PROCESS::<$SCHEDULER> := ThreadPoolScheduler.new
#?endif

#?if js
              PROCESS::<$SCHEDULER> := JavaScriptScheduler.new
#?endif
          },

          '$*THREAD', my sub THREAD() is raw {
              my $init_thread := nqp::create(Thread);
              nqp::bindattr(
                $init_thread,Thread,'$!vm_thread',Rakudo::Internals.INITTHREAD
              );
              nqp::bindattr($init_thread,Thread,'$!app_lifetime',False);
              nqp::bindattr($init_thread,Thread,'$!name','Initial thread');
              PROCESS::<$THREAD> := $init_thread
          },

          '$*TMPDIR', my sub TMPDIR() is raw {
              PROCESS::<$TMPDIR> := my IO $ = $*SPEC.tmpdir
          },

          '$*TOLERANCE', my sub TOLERANCE() is raw {
              PROCESS::<$TOLERANCE> := my $ = 1e-15
          },

          '$*TZ', my sub TZ is raw {
              PROCESS::<$TZ> := Proxy.new(
                FETCH => -> $ {
                    Rakudo::Internals.GET-LOCAL-TIMEZONE-OFFSET
                },
                STORE => -> $, int $offset {
                    $TZ-was-set-explicitly = 1;
                    $TZ-offset             = $offset;
                }
              )
          },

          '$*USER', my sub USER() is raw {
              Rakudo::Internals.FETCH-USER-GROUP('$USER')
          },

          '$*VM', my sub VM() is raw { PROCESS::<$VM> := VM.new }
    );

    method REGISTER-DYNAMIC(
      str $name,
      &code,
      str $version = '6.c',
      :$override
    --> Nil) {
#my $id := nqp::p6box_i(nqp::threadid(nqp::currentthread));
#nqp::say("$id: Registering $name");
        my str $with = nqp::concat($version,nqp::concat("\0",$name));

        nqp::if(
          $override,
          nqp::stmts(
            nqp::bindkey($initializers,$with,&code),
            nqp::bindkey($initializers,$name,&code)
          ),
          nqp::stmts(
            nqp::if(
              nqp::existskey($initializers,$with),
              (die "Already have initializer for '$name' ('$version')"),
              nqp::bindkey($initializers,$with,&code)
            ),
            nqp::unless(  # first come, first kept
              nqp::existskey($initializers,$name),
              nqp::bindkey($initializers,$name,&code)
            )
          )
        )
    }

    my $dynamics-not-found := nqp::hash;
    sub dynamic-not-found(str $key, str $name) {
#nqp::say("failed: $name");
        nqp::ifnull(
          nqp::atkey($dynamics-not-found,$key),
          nqp::bindkey($dynamics-not-found,$key,
            X::Dynamic::NotFound.new(:$name).Failure
          )
        )
    }

    my $DYNAMIC-INITIALIZATION-LOCK := nqp::create(Lock);
    method INITIALIZE-DYNAMIC(str $name, @deprecation?) is raw {
        my str $key = nqp::replace($name,1,1,'');
        my $process = nqp::who(PROCESS);

        $DYNAMIC-INITIALIZATION-LOCK.protect: {
#my $id := nqp::p6box_i(nqp::threadid(nqp::currentthread));
#nqp::say("$id: Initializing $name");

            nqp::if(
              $process.EXISTS-KEY($key),  # beat another thread us to it?
              $process.AT-KEY($key),      # yes, so use that
              nqp::if(                    # attempt to fetch init code
                nqp::isnull(
                  (my $code := nqp::ifnull(
                    nqp::atkey(
                      $initializers,
                      nqp::concat(
                        nqp::getcomp('Raku').language_version,
                        nqp::concat("\0",$name)
                      )
                    ),
                    nqp::atkey($initializers,$name)
                  ))
                ),
                dynamic-not-found($key, $name),   # alas, no init code
                nqp::stmts(                       # haz init code
                  nqp::if(                        # deprecations?
                    @deprecation,
                    Rakudo::Deprecations.DEPRECATED(
                      @deprecation[1],
                      '6.' ~ @deprecation[0],
                      :what($name),
                      :file(@deprecation[2]),
                      :line(@deprecation[3])
                    )
                  ),
                  $code()                         # run the code and bind
                )
              )
            )
        }
    }
}

# vim: expandtab shiftwidth=4
