class CompUnit::Repository::FileSystem   { ... }
class CompUnit::Repository::Installation { ... }
class CompUnit::Repository::AbsolutePath { ... }
class CompUnit::Repository::Unknown      { ... }
class CompUnit::Repository::NQP { ... }
class CompUnit::Repository::Perl5 { ... }

#?if jvm
class CompUnit::Repository::JavaRuntime { ... }
#?endif

class CompUnit::RepositoryRegistry {
    my $lock     = Lock.new;

    method repository-for-spec(Str $spec, CompUnit::Repository :$next-repo) {
        state %include-spec2cur;
        state $lock = Lock.new;

        my ($short-id,%options,$path) := parse-include-spec($spec);
        my $class := short-id2class($short-id);
        return CompUnit::Repository::Unknown.new(:path-spec($spec), :short-name($short-id))
            if so $class && nqp::istype($class, Failure) or $class === Any;

        my $abspath = $class.?absolutify($path) // $path;
        my $id      = "$short-id#$abspath";
        %options<next-repo> = $next-repo if $next-repo;
        $lock.protect( {
            %include-spec2cur{$id}:exists
              ?? %include-spec2cur{$id}
              !! (%include-spec2cur{$id} := $class.new(:prefix($abspath), |%options));
        } );
    }

    my $custom-lib := nqp::hash();
    method setup-repositories() {
        my $raw-specs;
        # only look up environment once
        my $ENV := nqp::getattr(%*ENV,Map,'$!storage');

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
               nqp::push($raw-specs,nqp::unbox_s($_))
                 for parse-include-specS($specs);
            }

            if nqp::existskey($ENV,'RAKUDOLIB') {
                nqp::push($raw-specs,nqp::unbox_s($_))
                  for parse-include-specS(nqp::atkey($ENV,'RAKUDOLIB'));
            }
            if nqp::existskey($ENV,'PERL6LIB') {
                nqp::push($raw-specs,nqp::unbox_s($_))
                  for parse-include-specS(nqp::atkey($ENV,'PERL6LIB'));
            }
        }

        my $prefix := nqp::existskey($ENV,'RAKUDO_PREFIX')
          ?? nqp::atkey($ENV,'RAKUDO_PREFIX')
          !! nqp::concat(
               nqp::atkey(nqp::getcomp('perl6').config,'prefix'),
               '/share/perl6'
             );

        # XXX Various issues with this stuff on JVM , TEMPORARY
        my Mu $compiler := nqp::getcurhllsym('$COMPILER_CONFIG');
        try {
            if nqp::existskey($ENV,'HOME')
              ?? nqp::atkey($ENV,'HOME')
              !! nqp::concat(
                   (nqp::existskey($ENV,'HOMEDRIVE')
                     ?? nqp::atkey($ENV,'HOMEDRIVE') !! ''),
                   (nqp::existskey($ENV,'HOMEPATH')
                     ?? nqp::atkey($ENV,'HOMEPATH') !! '')
                 ) -> $home {
                my str $path = "inst#$home/.perl6";
                nqp::bindkey($custom-lib,'home',$path);
                nqp::push($raw-specs, $path) unless $precomp-specs;
            }
        }

        # set up custom libs
        my str $site = "inst#$prefix/site";
        nqp::bindkey($custom-lib,'site',$site);
        nqp::push($raw-specs, $site) unless $precomp-specs;

        my str $vendor = "inst#$prefix/vendor";
        nqp::bindkey($custom-lib,'vendor',$vendor);
        nqp::push($raw-specs, $vendor) unless $precomp-specs;

        my str $perl = "inst#$prefix";
        nqp::bindkey($custom-lib,'perl',$perl);
        nqp::push($raw-specs, $perl) unless $precomp-specs;

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

        my %repos;
        my $SPEC := $*SPEC;
        sub normalize(\spec){
            my $parts := nqp::split('#', spec);
            my $path := nqp::elems($parts) - 1;
            nqp::bindpos($parts, $path, nqp::unbox_s($SPEC.canonpath(nqp::atpos($parts, $path))));
            nqp::join('#', $parts)
        };

        # create reverted, unique list of path-specs
        my $iter   := nqp::iterator($raw-specs);
        my $unique := nqp::hash();
        my $specs  := nqp::list();
        while $iter {
            my str $path-spec = normalize(nqp::shift($iter));
            unless nqp::existskey($unique,$path-spec) {
                nqp::bindkey($unique,$path-spec,1);
                nqp::unshift($specs,$path-spec);
            }
        }

        # convert path-specs to repos
        $iter := nqp::iterator($specs);
        my $repos := nqp::hash();
        while $iter {
            my str $spec = nqp::shift($iter);
            $next-repo := self.use-repository(
              self.repository-for-spec($spec), :current($next-repo));
            nqp::bindkey($repos,$spec,$next-repo);
        }

        # convert custom-lib path-specs to repos
        $iter := nqp::iterator($custom-lib);
        while $iter {
            my \pair = nqp::shift($iter);
            my $repo := nqp::atkey($repos, normalize(nqp::iterval(pair)));
            if nqp::isnull($repo) {
                nqp::deletekey($custom-lib, nqp::iterkey_s(pair));
            }
            else {
                nqp::bindkey($custom-lib, nqp::iterkey_s(pair), $repo);
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
        my @parts = $spec.split('#', 2);
        if @parts.elems == 2 {
            my $repo = self.repository-for-name(@parts[0]);
            return $repo.source-file(@parts[1]) if $repo.can('source-file');
        }
        Nil
    }

    method head() { # mostly usefull for access from NQP
        $*REPO
    }

    method resolve-unknown-repos(@repos) {
        for @repos.pairs {
            if nqp::istype($_.value, CompUnit::Repository::Unknown) {
                my $i = $_.key;
                my $next-repo := @repos[$i + 1];

                my $head := PROCESS<$REPO>;
                PROCESS::<$REPO> := $next-repo;
                my $comp_unit = $next-repo.need(
                    CompUnit::DependencySpecification.new(:short-name($_.value.short-name))
                );
                PROCESS::<$REPO> := $head;

                my $global := nqp::list(); # Cannot just use GLOBAL.WHO here as that gives a BOOTHash
                nqp::push($global, "GLOBAL");
                $*W.find_symbol($global).WHO.merge-symbols($comp_unit.handle.globalish-package);
                my $new-repo = self.repository-for-spec($_.value.path-spec, :$next-repo);
                if $i > 0 {
                    @repos[$i - 1].next-repo = $new-repo if $i > 0;
                }
                else {
                    PROCESS::<$REPO> := $new-repo;
                }
            }
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

    sub short-id2class(Str:D $short-id) {
        state %short-id2class;
        state $lock = Lock.new;

        Proxy.new(
          FETCH => {
              $lock.protect( {
                  if %short-id2class.EXISTS-KEY($short-id) {
                      %short-id2class.AT-KEY($short-id);
                  }
                  else {
                      my $type = try ::($short-id);
                      if $type !=== Any {
                          if $type.?short-id -> $id {
                              if %short-id2class.EXISTS-KEY($id) {
                                  %short-id2class.AT-KEY($id);
                              }
                              else {
                                  %short-id2class.BIND-KEY($id, $type);
                              }
                          }
                          else {
                              die "Class '$type.^name()' is not a CompUnit::Repository";
                          }
                      }
                      else {
                          Any
                      }
                  }
              } );
          },
          STORE => -> $, $class {
              my $type = ::($class);
              die "Must load class '$class' first" if nqp::istype($type,Failure);
              $lock.protect( { %short-id2class{$short-id} := $type } );
          },
        );
    }

# prime the short-id -> class lookup
    short-id2class('file')  = 'CompUnit::Repository::FileSystem';
    short-id2class('inst')  = 'CompUnit::Repository::Installation';
    short-id2class('ap')    = 'CompUnit::Repository::AbsolutePath';
    short-id2class('nqp')   = 'CompUnit::Repository::NQP';
    short-id2class('perl5') = 'CompUnit::Repository::Perl5';
#?if jvm
    short-id2class('javart')  = 'CompUnit::Repository::JavaRuntime';
    short-id2class('java')  = 'CompUnit::Repository::Java';
#?endif

    sub parse-include-spec(Str:D $spec, Str:D $default-short-id = 'file') {
        my %options;

        # something we understand
        if $spec ~~ /^
          [
            $<type>=[ <.ident>+ % '::' ]
            [ '#' $<n>=\w+
              <[ < ( [ { ]> $<v>=<[\w-]>+ <[ > ) \] } ]>
              { %options{$<n>} = ~$<v> }
            ]*
            '#'
          ]?
          $<path>=.*
        $/ {
            ( $<type> ?? ~$<type> !! $default-short-id, %options, ~$<path> );
        }
    }

    sub parse-include-specS(Str:D $specs) {
        my @found;
        my $default-short-id = 'file';

        if $*RAKUDO_MODULE_DEBUG -> $RMD { $RMD("Parsing specs: $specs") }

        # for all possible specs
        for $specs.split(',') -> $spec {
            if parse-include-spec($spec.trim, $default-short-id) -> $triplet {
                @found.push: join "#",
                  $triplet[0],
                  $triplet[1].map({ .key ~ "<" ~ .value ~ ">" }),
                  $triplet[2];
                $default-short-id = $triplet[0];
            }
            elsif $spec {
                die "Don't know how to handle $spec";
            }
        }
        @found;
    }
}

# vim: ft=perl6 expandtab sw=4
