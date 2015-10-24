role  CompUnitRepo::Locally             { ... }
class CompUnitRepo::Local::File         { ... }
class CompUnitRepo::Local::Installation { ... }

my class Perl5ModuleLoaderStub {
    method load_module($module_name, %opts, *@GLOBALish, :$line, :$file) {
        {
            CompUnitRepo.load_module('Inline::Perl5', {}, @GLOBALish, :$line, :$file);
            CATCH {
                $*W.find_symbol(nqp::list('X','NYI','Available')).new(
                    :available('Inline::Perl5'), :feature('Perl 5')).throw;
            }
        }

        # Inline::Perl5 has overwritten this module loader at this point
        return CompUnitRepo.load_module($module_name, %opts, @GLOBALish, :$line, :$file);
    }
}

class CompUnitRepo {
    my $lock     = Lock.new;
    my %modules_loaded;

    my %language_module_loaders;
    %language_module_loaders<Perl5> := Perl5ModuleLoaderStub;
    # We're using Perl6::ModuleLoader instead of NQP's here,
    # so it can special-cases NQP wrt GLOBALish correctly.
    %language_module_loaders<NQP>   := nqp::gethllsym('perl6', 'ModuleLoader');

    method register_language_module_loader($lang, $loader, :$force) {
        nqp::die("Language loader already registered for $lang")
            if ! $force && nqp::existskey(%language_module_loaders, $lang);
        %language_module_loaders{$lang} := $loader;
    }

    method new(Str $spec, CompUnit::Repository :$next-repo) {
        INCLUDE-SPEC2CUR($spec, :$next-repo)
    }

    method files($file, :$name, :$auth, :$ver) {
        for @*INC -> $spec {
            if INCLUDE-SPEC2CUR($spec) -> $cur {
                if $cur.files($file, :$name,:$auth,:$ver).list -> @candi {
                    return @candi;
                }
            }
        }
        ();
    }

    method candidates($name, :$file, :$auth, :$ver) {
        for @*INC -> $spec {

RAKUDO_MODULE_DEBUG("Looking in $spec for $name")
  if $*RAKUDO_MODULE_DEBUG;

            if INCLUDE-SPEC2CUR($spec) -> $cur {
                if $cur.candidates($name, :$file,:$auth,:$ver).list -> @candi {
                    return @candi;
                }
            }
        }
        ();
    }

    method load_module($module_name, %opts, \GLOBALish is raw, :$line, :$file) {
        RAKUDO_MODULE_DEBUG("going to load $module_name: %opts.perl()") if $*RAKUDO_MODULE_DEBUG;
        $lock.protect( {
        if %opts<from> {
            # See if we need to load it from elsewhere.
            if %language_module_loaders{%opts<from>}:exists {
                return %language_module_loaders{%opts<from>}.load_module($module_name,
                    %opts, GLOBALish, :$line, :$file);
            }
            else {
                nqp::die("Do not know how to load code from " ~ %opts<from>);
            }
        }
        elsif self.candidates($module_name, :$file, :auth(%opts<auth>), :ver(%opts<ver>)) -> ($candi) {
            $candi.load(GLOBALish, :$line)
        }
        elsif $file {
            nqp::die("Could not find file '$file' for module $module_name");
        }
        else {
            my $from-hint := "";
            if nqp::substr(~$module_name, nqp::rindex(~$module_name, ":") + 1) eq "from" {
                $from-hint := "\nUse a single colon to include a module from another language.";
            }
            nqp::die("Could not find $module_name in any of:\n  " ~
                join("\n  ", @*INC) ~ $from-hint);
        }
    } ) }

    method ctxsave() {
        $*MAIN_CTX := nqp::ctxcaller(nqp::ctx());
        $*CTXSAVE := 0;
    }

    # Handles any object repossession conflicts that occurred during module load,
    # or complains about any that cannot be resolved.
    method resolve_repossession_conflicts(@conflicts) {
        for @conflicts -> $orig is raw, $current is raw {
            # If it's a Stash in conflict, we make sure any original entries get
            # appropriately copied.
            if $orig.HOW.name($orig) eq 'Stash' {
                for $orig.FLATTENABLE_HASH() {
                    if !nqp::existskey($current, $_.key) || nqp::eqat($_.key, '&', 0) {
                        $current{$_.key} := $_.value;
                    }
                }
            }
            # We could complain about anything else, and may in the future; for
            # now, we let it pass by with "latest wins" semantics.
        }
    }
}

sub SHORT-ID2CLASS(Str:D $short-id) {
    state %SHORT-ID2CLASS;
    state $lock = Lock.new;

    Proxy.new(
      FETCH => {
          $lock.protect( {
              if %SHORT-ID2CLASS.EXISTS-KEY($short-id) {
                  %SHORT-ID2CLASS.AT-KEY($short-id);
              }
              else {
                  my $type = try ::($short-id);
                  if $type !=== Any {
                      if $type.?short-id -> $id {
                          die "Have '$id' already registered for %SHORT-ID2CLASS{$id}.^name()"
                            if %SHORT-ID2CLASS.EXISTS-KEY($id);
                          %SHORT-ID2CLASS.BIND-KEY($id,$type);
                      }
                      else {
                          die "Class '$type.^name()' is not a CompUnitRepo";
                      }
                  }
                  else {
                      die "No CompUnitRepo known by '$short-id'";
                  }
              }
          } );
      },
      STORE => -> $, $class {
          my $type = ::($class);
          die "Must load class '$class' first" if nqp::istype($type,Failure);
          $lock.protect( { %SHORT-ID2CLASS{$short-id} := $type } );
      },
    );
}

# prime the short-id -> class lookup
SHORT-ID2CLASS('file') = 'CompUnitRepo::Local::File';
SHORT-ID2CLASS('inst') = 'CompUnitRepo::Local::Installation';

sub INCLUDE-SPEC2CUR(Str:D $spec, CompUnit::Repository :$next-repo) {
    state %INCLUDE-SPEC2CUR;
    state $lock = Lock.new;

    my ($short-id,%options,$path) := PARSE-INCLUDE-SPEC($spec);
    my $class = SHORT-ID2CLASS($short-id);
    die "No class loaded for short-id '$short-id': $spec -> $path"
      if $class === Any;

    my $abspath = $class.?absolutify($path) // $path;
    my $id      = "$short-id#$abspath";
    %options<next-repo> = $next-repo if $next-repo;
    $lock.protect( {
        %INCLUDE-SPEC2CUR{$id}:exists
          ?? %INCLUDE-SPEC2CUR{$id}
          !! (%INCLUDE-SPEC2CUR{$id} := $class.new($abspath,|%options));
    } );
}

sub PARSE-INCLUDE-SPEC(Str:D $spec, Str:D $default-short-id = 'file') {
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
      $<path>=.+
    $/ {
        ( $<type> ?? ~$<type> !! $default-short-id, %options, ~$<path> );
    }
}

sub PARSE-INCLUDE-SPECS(Str:D $specs) {
    my @found;
    my $default-short-id = 'file';

RAKUDO_MODULE_DEBUG("Parsing specs: $specs")
  if $*RAKUDO_MODULE_DEBUG;

    # for all possible specs
    for $specs.split(/ \s* ',' \s* /) -> $spec {
        if PARSE-INCLUDE-SPEC($spec, $default-short-id) -> $triplet {
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

sub CREATE-INCLUDE-SPECS(*@INC) { @INC.join(',') }

sub RAKUDO_MODULE_DEBUG(*@str) { note "SET RMD: @str[]" }

# vim: ft=perl6 expandtab sw=4
