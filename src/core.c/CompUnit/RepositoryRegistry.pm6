class CompUnit::Repository::FileSystem   { ... }
class CompUnit::Repository::Installation { ... }
class CompUnit::Repository::AbsolutePath { ... }
class CompUnit::Repository::Unknown      { ... }
class CompUnit::Repository::NQP { ... }
class CompUnit::Repository::Perl5 { ... }

#?if jvm
class CompUnit::Repository::JavaRuntime { ... }
class CompUnit::Repository::Java { ... }
#?endif

#?if js
class CompUnit::Repository::FileSystemWithRecording { ... }
class CompUnit::Repository::NodeJs { ... }
#?endif

class CompUnit::RepositoryRegistry {
    my $lock     = Lock.new;
    my %include-spec2cur;

    proto method repository-for-spec(|) { * }
    multi method repository-for-spec(Str $spec, CompUnit::Repository :$next-repo) {
        self.repository-for-spec(CompUnit::Repository::Spec.from-string($spec), :$next-repo)
    }
    multi method repository-for-spec(CompUnit::Repository::Spec $spec, CompUnit::Repository :$next-repo) {
        my $short-id := $spec.short-id;
        my %options  := $spec.options;
        my $path     := $spec.path;

        my $class := short-id2class($short-id);
        return CompUnit::Repository::Unknown.new(:path-spec($spec), :short-name($short-id))
            if so $class && nqp::istype($class, Failure) or !nqp::istype($class, CompUnit::Repository);

        my $abspath = nqp::can($class,"absolutify")
          ?? $class.absolutify($path)
          !! $path;
        my $id      = "$short-id#$abspath";
        %options<next-repo> = $next-repo if $next-repo;
        $lock.protect( {
            %include-spec2cur{$id}:exists
              ?? %include-spec2cur{$id}
              !! (%include-spec2cur{$id} := $class.new(:prefix($abspath), |%options));
        } );
    }

    method !register-repository($id, CompUnit::Repository $repo) {
        $lock.protect( {
            %include-spec2cur{$id}:exists
              ?? %include-spec2cur{$id}
              !! (%include-spec2cur{$id} := $repo);
        } );
    }

    my $custom-lib := nqp::hash();
    method setup-repositories() {
        my $raw-specs;
        # only look up environment once
        my $ENV := nqp::getattr(%*ENV,Map,'$!storage');
        my $sep := $*SPEC.dir-sep;

        # starting up for creating precomp
        my $precomp-specs = nqp::existskey($ENV,'RAKUDO_PRECOMP_WITH')
            ?? nqp::atkey($ENV,'RAKUDO_PRECOMP_WITH')
            !! False;
        if $precomp-specs {
            # assume well formed strings
            $raw-specs := nqp::split(',', $precomp-specs);
        }

        # normal start up
        else {
            $raw-specs := nqp::list();
            for Rakudo::Internals.INCLUDE -> $specs {
               nqp::push($raw-specs,$_)
                 for parse-include-specS($specs);
            }

            if nqp::existskey($ENV,'RAKUDOLIB') {
                nqp::push($raw-specs,$_)
                  for parse-include-specS(nqp::atkey($ENV,'RAKUDOLIB'));
            }
            if nqp::existskey($ENV,'PERL6LIB') {
                nqp::push($raw-specs,$_)
                  for parse-include-specS(nqp::atkey($ENV,'PERL6LIB'));
            }
        }

        my str $prefix = nqp::existskey($ENV,'RAKUDO_PREFIX')
          ?? nqp::atkey($ENV,'RAKUDO_PREFIX')
          !! nqp::getcurhllsym('$RAKUDO_HOME');

        # XXX Various issues with this stuff on JVM , TEMPORARY
        my str $home;
        my str $home-spec;
        try {
            if nqp::existskey($ENV,'HOME')
              ?? nqp::atkey($ENV,'HOME')
              !! nqp::concat(
                   (nqp::existskey($ENV,'HOMEDRIVE')
                     ?? nqp::atkey($ENV,'HOMEDRIVE') !! ''),
                   (nqp::existskey($ENV,'HOMEPATH')
                     ?? nqp::atkey($ENV,'HOMEPATH') !! '')
                 ) -> $home-path {
                $home = "{$home-path}{$sep}.perl6";
                $home-spec = "inst#$home";
            }
        }

        # set up custom libs
        my str $site   = "inst#{$prefix}{$sep}site";
        my str $vendor = "inst#{$prefix}{$sep}vendor";
        my str $core   = "inst#{$prefix}{$sep}core";

        # your basic repo chain
        my CompUnit::Repository $next-repo :=
            $precomp-specs
            ?? CompUnit::Repository
            !! CompUnit::Repository::AbsolutePath.new(
                :next-repo( CompUnit::Repository::NQP.new(
                    :next-repo(CompUnit::Repository::Perl5.new(
#?if jvm
                        :next-repo(CompUnit::Repository::JavaRuntime.new)
#?endif
                    ))
                )
            )
        );

        # create reverted, unique list of path-specs
        my $iter   := nqp::iterator($raw-specs);
        my $unique := nqp::hash();
        my $specs  := nqp::list();
        while $iter {
            my $repo-spec := nqp::shift($iter);
            my str $path-spec = $repo-spec.Str;
            unless nqp::existskey($unique,$path-spec) {
                nqp::bindkey($unique,$path-spec,1);
                nqp::unshift($specs,$repo-spec);
            }
        }

        unless $precomp-specs {
            nqp::bindkey($custom-lib, 'core', $next-repo := self!register-repository(
                $core,
                CompUnit::Repository::Installation.new(:prefix("$prefix/core"), :$next-repo)
            )) unless nqp::existskey($unique, $core);
            nqp::bindkey($custom-lib, 'vendor', $next-repo := self!register-repository(
                $vendor,
                CompUnit::Repository::Installation.new(:prefix("$prefix/vendor"), :$next-repo)
            )) unless nqp::existskey($unique, $vendor);
            nqp::bindkey($custom-lib, 'site', $next-repo := self!register-repository(
                $site,
                CompUnit::Repository::Installation.new(:prefix("$prefix/site"), :$next-repo)
            )) unless nqp::existskey($unique, $site);
            nqp::bindkey($custom-lib, 'home', $next-repo := self!register-repository(
                $home-spec,
                CompUnit::Repository::Installation.new(:prefix($home), :$next-repo)
            )) if $home and not nqp::existskey($unique, $home-spec);
        }

        # convert repo-specs to repos
        my $repos := nqp::hash();
        $iter := nqp::iterator($specs);
        while $iter {
            my $spec = nqp::shift($iter);
            $next-repo := self.use-repository(
              self.repository-for-spec($spec), :current($next-repo));
            nqp::bindkey($repos,$spec.Str,$next-repo);
        }

        # register manually set custom-lib repos
        unless nqp::existskey($custom-lib, 'core') {
            my $repo := nqp::atkey($repos, $core);
            if nqp::isnull($repo) {
                nqp::deletekey($custom-lib, 'core');
            }
            else {
                nqp::bindkey($custom-lib, 'core', $repo);
            }
        }
        unless nqp::existskey($custom-lib, 'vendor') {
            my $repo := nqp::atkey($repos, $vendor);
            if nqp::isnull($repo) {
                nqp::deletekey($custom-lib, 'vendor');
            }
            else {
                nqp::bindkey($custom-lib, 'vendor', $repo);
            }
        }
        unless nqp::existskey($custom-lib, 'site') {
            my $repo := nqp::atkey($repos, $site);
            if nqp::isnull($repo) {
                nqp::deletekey($custom-lib, 'site');
            }
            else {
                nqp::bindkey($custom-lib, 'site', $repo);
            }
        }
        unless nqp::existskey($custom-lib, 'home') {
            my $repo := nqp::atkey($repos, $home-spec);
            if nqp::isnull($repo) {
                nqp::deletekey($custom-lib, 'home');
            }
            else {
                nqp::bindkey($custom-lib, 'home', $repo);
            }
        }

        $next-repo
    }

    method !remove-from-chain(CompUnit::Repository $repo, CompUnit::Repository :$current = $*REPO) {
        my $item = $current;
        while $item {
            if $item.next-repo === $repo {
                $item.next-repo = $repo.next-repo;
                last;
            }
            $item = $item.next-repo;
        }
    }

    method use-repository(CompUnit::Repository $repo, CompUnit::Repository :$current = $*REPO) {
        return $repo if $current === $repo;
        self!remove-from-chain($repo, :$current);
        $repo.next-repo = $current;
        PROCESS::<$REPO> := $repo;
    }

    method repository-for-name(Str:D \name) {
        $*REPO; # initialize if not yet done
        my str $name = nqp::unbox_s(name);
        nqp::existskey($custom-lib,$name)
          ?? nqp::atkey($custom-lib,$name)
          !! Nil
    }

    method register-name($name, CompUnit::Repository $repo) {
        nqp::bindkey($custom-lib, $name, $repo);
    }

    method name-for-repository(CompUnit::Repository $repo) {
        $*REPO; # initialize if not yet done
        my $iter := nqp::iterator($custom-lib);
        while $iter {
            my \pair = nqp::shift($iter);
            return nqp::iterkey_s(pair) if nqp::iterval(pair).prefix eq $repo.prefix;
        }
        Nil
    }

    method file-for-spec(Str $spec) {
        my @parts is List = $spec.split('#', 2);
        if @parts.elems == 2 {
            my $repo = self.repository-for-name(@parts[0]);
            return $repo.source-file(@parts[1]) if $repo.can('source-file');
        }
        Nil
    }

    method run-script($script, :$name, :$auth, :$ver, :$api) {
        shift @*ARGS if $name;
        shift @*ARGS if $auth;
        shift @*ARGS if $ver;

        my @installations = $*REPO.repo-chain.grep(CompUnit::Repository::Installation);
        my @metas = @installations.map({ .files("bin/$script", :$name, :$auth, :$ver).head }).grep(*.defined);
        unless +@metas {
            @metas = flat @installations.map({ .files("bin/$script").Slip }).grep(*.defined);
            if +@metas {
                note "===SORRY!===\n"
                    ~ "No candidate found for '$script' that match your criteria.\n"
                    ~ "Did you perhaps mean one of these?";
                my %caps = :name(['Distribution', 12]), :auth(['Author(ity)', 11]), :ver(['Version', 7]);
                for @metas -> $meta {
                    for %caps.kv -> $caption, @opts {
                        @opts[1] = max @opts[1], ($meta{$caption} // '').Str.chars
                    }
                }
                note '  ' ~ %caps.values.map({ sprintf('%-*s', .[1], .[0]) }).join(' | ');
                for @metas -> $meta {
                    note '  ' ~ %caps.kv.map( -> $k, $v { sprintf('%-*s', $v.[1], $meta{$k} // '') } ).join(' | ')
                }
            }
            else {
                note "===SORRY!===\nNo candidate found for '$script'.\n";
            }
            exit 1;
        }

        my $meta = @metas.sort(*.<ver>).reverse.head;
        my $bin  = $meta<source>;
        require "$bin";
    }

    method head() { # mostly usefull for access from NQP
        $*REPO
    }

    method resolve-unknown-repos($repo is copy) {
        # Cannot just use GLOBAL.WHO here as that gives a BOOTHash
        my $global := nqp::list("GLOBAL");
        my $prev-repo;
        while defined $repo {
            if nqp::istype($repo, CompUnit::Repository::Unknown) {
                my $next-repo := $repo.next-repo;

                my $head := PROCESS<$REPO>;
                PROCESS::<$REPO> := $next-repo;
                my $comp_unit = $next-repo.need(
                    CompUnit::DependencySpecification.new(:short-name($repo.short-name))
                );
                PROCESS::<$REPO> := $head;

                $*W.find_symbol($global).WHO.merge-symbols($comp_unit.handle.globalish-package);
                $repo = self.repository-for-spec($repo.path-spec, :$next-repo);
                if defined $prev-repo {
                    $prev-repo.next-repo = $repo;
                }
                else {
                    PROCESS::<$REPO> := nqp::decont($repo);
                }
            }
            $prev-repo = $repo;
            $repo = $repo.next-repo;
        }
    }

    # Handles any object repossession conflicts that occurred during module load,
    # or complains about any that cannot be resolved.
    method resolve_repossession_conflicts(@conflicts) {
        for @conflicts -> $orig is raw, $current is raw {
            # If it's a Stash in conflict, we make sure any original entries get
            # appropriately copied.
            if $orig.HOW.name($orig) eq 'Stash' {
                $current.merge-symbols($orig);
            }
            # We could complain about anything else, and may in the future; for
            # now, we let it pass by with "latest wins" semantics.
        }
    }

#?if moar
    my constant $short-id2class = nqp::hash(
#?endif
#?if !moar
    my $short-id2class := nqp::hash(
#?endif
      'file',   CompUnit::Repository::FileSystem,
      'inst',   CompUnit::Repository::Installation,
      'ap',     CompUnit::Repository::AbsolutePath,
      'nqp',    CompUnit::Repository::NQP,
      'perl5',  CompUnit::Repository::Perl5,
#?if js
      'nodejs', CompUnit::Repository::NodeJs,
#?endif
#?if jvm
      'javart', CompUnit::Repository::JavaRuntime,
      'java',   CompUnit::Repository::Java,
#?endif
#?if js
      'filerecording', CompUnit::Repository::FileSystemWithRecording,
#?endif
    );
    my $sid-lock := Lock.new;

    sub short-id2class(Str:D $short-id) is rw {
        Proxy.new(
          FETCH => {
              $sid-lock.protect( {
                  nqp::ifnull(
                    nqp::atkey($short-id2class,$short-id),
                    nqp::if(
                      nqp::istype((my \type := ::($short-id)),Failure),
                      (type.defined || Any),  # no unhandled Failure warnings
                      nqp::if(
                        nqp::can(type,"short-id")
                          && (my str $id = type.short-id),
                        nqp::ifnull(
                          nqp::atkey($short-id2class,$id),
                          nqp::bindkey($short-id2class,$id,type)
                        ),
                        (die "Class '{type.^name}' is not a 'CompUnit::Repository'")
                      )
                    )
                  )
              } );
          },
          STORE => -> $, $class {
              nqp::istype((my \type = ::($class)),Failure)
                ?? X::AdHoc.new( payload => "Must load class '$class' first" ).throw
                !! $sid-lock.protect: {
                       nqp::bindkey($short-id2class,$short-id,type)
                   }
          },
        );
    }

    sub parse-include-specS(Str:D $specs) {
        my @found;
        my $default-short-id = 'file';

        if $*RAKUDO_MODULE_DEBUG -> $RMD { $RMD("Parsing specs: $specs") }

        # for all possible specs
        my $spec-list := nqp::split(',', $specs);
        my $iter      := nqp::iterator($spec-list);
        while $iter {
            my $spec := nqp::shift($iter);
            if CompUnit::Repository::Spec.from-string($spec.trim, :$default-short-id) -> $repo-spec {
                @found.push: $repo-spec;
                $default-short-id = $repo-spec.short-id;
            }
            elsif $spec {
                die "Don't know how to handle $spec";
            }
        }
        @found;
    }
}

# vim: ft=perl6 expandtab sw=4
