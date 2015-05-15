role  CompUnitRepo::Locally             { ... }
class CompUnitRepo::Local::File         { ... }
class CompUnitRepo::Local::Installation { ... }

class CompUnitRepo {
    my Mu $p6ml := nqp::gethllsym('perl6', 'ModuleLoader');
    my $lock     = Lock.new;

    method new(Str $spec) { INCLUDE-SPEC2CUR($spec) }

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
  if $?RAKUDO_MODULE_DEBUG;

            if INCLUDE-SPEC2CUR($spec) -> $cur {
                if $cur.candidates($name, :$file,:$auth,:$ver).list -> @candi {
                    return @candi;
                }
            }
        }
        ();
    }

    method p6ml { $p6ml }

    method load_module($module_name, %opts, *@GLOBALish is rw, :$line, :$file, :%chosen) {
        $lock.protect( {
        my $candi = self.candidates($module_name, :auth(%opts<auth>), :ver(%opts<ver>))[0];
        if $candi {
            %chosen<pm>   :=
              $candi<provides>{$module_name}<pm><file> //
              $candi<provides>{$module_name}<pm6><file>;
            %chosen<pm>   := ~%chosen<pm> if %chosen<pm>.DEFINITE;
            if $candi<provides>{$module_name}{$*VM.precomp-ext}<file> -> $load {
                %chosen<load> := $load;
            }
            %chosen<key>  := %chosen<pm> // %chosen<load>;
        }
        $p6ml.load_module($module_name, %opts, |@GLOBALish, :$line, :$file, :%chosen);
    } ) }

    method ctxsave() { $p6ml.ctxsave() }
    method absolute_path($path) { $p6ml.absolute_path($path) }
    method load_setting($setting_name) { $p6ml.load_setting($setting_name) }
    method resolve_repossession_conflicts(@conflicts) {
        $p6ml.resolve_repossession_conflicts( nqp::findmethod(@conflicts, 'FLATTENABLE_LIST')(@conflicts) )
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

sub INCLUDE-SPEC2CUR(Str:D $spec) {
    state %INCLUDE-SPEC2CUR;
    state $lock = Lock.new;

    my ($short-id,%options,$path) := PARSE-INCLUDE-SPEC($spec);
    my $class = SHORT-ID2CLASS($short-id);
    die "No class loaded for short-id '$short-id': $spec -> $path" 
      if $class === Any;

    my $abspath = $class.?absolutify($path) // $path;
    my $id      = "$short-id:$abspath";
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
  if $?RAKUDO_MODULE_DEBUG;

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

sub RAKUDO_MODULE_DEBUG(*@str) { note "MODULE_DEBUG: @str[]" }

# vim: ft=perl6 expy andtab sw=4
