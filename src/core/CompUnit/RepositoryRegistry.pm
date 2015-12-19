class CompUnit::Repository::FileSystem   { ... }
class CompUnit::Repository::Installation { ... }
class CompUnit::Repository::AbsolutePath { ... }
class CompUnit::Repository::NQP { ... }
class CompUnit::Repository::Perl5 { ... }

class CompUnit::RepositoryRegistry {
    my $lock     = Lock.new;

    method repository-for-spec(Str $spec, CompUnit::Repository :$next-repo) {
        state %include-spec2cur;
        state $lock = Lock.new;

        my ($short-id,%options,$path) := parse-include-spec($spec);
        my $class = short-id2class($short-id);
        die "No class loaded for short-id '$short-id': $spec -> $path"
          if $class === Any;

        my $abspath = $class.?absolutify($path) // $path;
        my $id      = "$short-id#$abspath";
        %options<next-repo> = $next-repo if $next-repo;
        $lock.protect( {
            %include-spec2cur{$id}:exists
              ?? %include-spec2cur{$id}
              !! (%include-spec2cur{$id} := $class.new(:prefix($abspath), |%options));
        } );
    }

    method files($file, :$name, :$auth, :$ver) {
        for @*INC -> $spec {
            if self.repository-for-spec($spec) -> $cur {
                if $cur.files($file, :$name,:$auth,:$ver).list -> @candi {
                    return @candi;
                }
            }
        }
        ();
    }

    my %custom-lib;
    method setup-repositories() {
        my @INC;
        my %ENV := %*ENV; # only look up environment once

        # starting up for creating precomp
        if %ENV<RAKUDO_PRECOMP_WITH> -> \specs {
            @INC = specs.split(','); # assume well formed strings
        }

        # normal start up
        else {
            my $I := nqp::atkey(nqp::atkey(%*COMPILING, '%?OPTIONS'), 'I');
            if nqp::defined($I) {
                if nqp::islist($I) {
                    my Mu $iter := nqp::iterator($I);
                    while $iter {
                        @INC.append: parse-include-specS(nqp::shift($iter));
                    }
               }
                else {
                    @INC.append: parse-include-specS(nqp::p6box_s($I));
                }
            }

            if %ENV<RAKUDOLIB> -> $rakudolib {
                @INC.append: parse-include-specS($rakudolib);
            }
            if %ENV<PERL6LIB> -> $perl6lib {
                @INC.append: parse-include-specS($perl6lib);
            }

#?if jvm
            for nqp::hllize(nqp::jvmclasspaths()) -> $path {
                @INC.append: parse-include-specS($path);
            }
#?endif

            my $prefix := nqp::p6box_s(
              nqp::concat(nqp::atkey(nqp::backendconfig,'prefix'),'/share/perl6')
            );

            my $abspath := "$prefix/share/libraries.json";
            if IO::Path.new-from-absolute-path($abspath).e {
#            my $config = from-json( slurp $abspath );
#
#            for $config.list -> @group {
#                for @group>>.kv -> $class, $props {
#                    for $props.list -> $prop {
#                        if nqp::istype($prop,Associative) {
#                            for $prop.value.flat -> $path {
#                                @INC.push: parse-include-specS($path);
#                                %custom-lib{$prop.key} = $path;
#                            }
#                        }
#                        else {
#                            for $prop.flat -> $path {
#                                @INC.push: parse-include-specS($path);
#                            }
#                        }
#                    }
#                }
#            }
            }
            # There is no config file, so pick sane defaults.
            else {
                # XXX Various issues with this stuff on JVM
                my Mu $compiler := nqp::getcurhllsym('$COMPILER_CONFIG');  # TEMPORARY
                try {
                    if %ENV<HOME>
                      // (%ENV<HOMEDRIVE> // '') ~ (%ENV<HOMEPATH> // '') -> $home {
                        my $ver := nqp::p6box_s(nqp::atkey($compiler, 'version'));
                        my $path := "$home/.perl6/$ver";
                        @INC.append: (%custom-lib<home> = "inst#$path");
                    }
                }
                @INC.append:
                  (%custom-lib<site>   = "inst#$prefix/site"),
                  (%custom-lib<vendor> = "inst#$prefix/vendor"),
                  (%custom-lib<perl>   = "inst#$prefix");
            }
        }

        my CompUnit::Repository $next-repo := CompUnit::Repository::AbsolutePath.new(
            :next-repo(
                CompUnit::Repository::NQP.new(
                    :next-repo(CompUnit::Repository::Perl5.new)
                )
            )
        );
        my %repos;
        my $SPEC := $*SPEC;
        my &canon = -> $repo {
            my @parts = $repo.split('#');
            join '#', @parts[0], $SPEC.canonpath(@parts[1]);
        };
        %repos{$_} = $next-repo := self.use-repository(
                self.repository-for-spec($_),
                :current($next-repo),
            ) for @INC>>.&canon.unique.reverse;

        $_ = %repos{$_.&canon} for %custom-lib.values;

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

    method repository-for-name(Str:D $name) {
        $*REPO; # initialize if not yet done
        %custom-lib{$name}
    }

    method load_module($module_name, %opts, \GLOBALish is raw, :$line, :$file) {
        RAKUDO_MODULE_DEBUG("going to load $module_name: %opts.perl()") if $*RAKUDO_MODULE_DEBUG;
        $lock.protect( {
            my @MODULES = nqp::clone(@*MODULES // ());

            {
                my @*MODULES := @MODULES;
                if +@*MODULES == 0 and %*ENV<RAKUDO_PRECOMP_LOADING> -> $loading {
                    @*MODULES := from-json $loading;
                }
                for @*MODULES.list -> $m {
                    if $m eq $module_name {
                        nqp::die("Circular module loading detected involving module '$module_name'");
                    }
                }
                @*MODULES.push: $module_name;

                my $compunit := (
                    $file
                    ?? $*REPO.load($file.IO)
                    !! $*REPO.need(
                        CompUnit::DependencySpecification.new(
                            :short-name($module_name),
                            :from(%opts<from> // 'Perl6'),
                            :auth-matcher(%opts<auth> // True),
                            :version-matcher(%opts<ver> // True),
                        ),
                    )
                );
                GLOBALish.WHO.merge-symbols($compunit.handle.globalish-package.WHO)
                    if GLOBALish !=== Any;
                $compunit
            }
        } )
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
                              die "Have '$id' already registered for %short-id2class{$id}.^name()"
                                if %short-id2class.EXISTS-KEY($id);
                              %short-id2class.BIND-KEY($id,$type);
                          }
                          else {
                              die "Class '$type.^name()' is not a CompUnit::Repository";
                          }
                      }
                      else {
                          die "No CompUnit::Repository known by '$short-id'";
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

    RAKUDO_MODULE_DEBUG("Parsing specs: $specs")
      if $*RAKUDO_MODULE_DEBUG;

        # for all possible specs
        for $specs.split(/ \s* ',' \s* /) -> $spec {
            if parse-include-spec($spec, $default-short-id) -> $triplet {
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

sub RAKUDO_MODULE_DEBUG(*@str) { note "SET RMD: @str[]" }

# vim: ft=perl6 expandtab sw=4
