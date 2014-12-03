class CompUnitRepo::Local::File         { ... }
class CompUnitRepo::Local::Installation { ... }

class CompUnitRepo {
    my Mu $p6ml := nqp::gethllsym('perl6', 'ModuleLoader');
    my $lock := Lock.new;

    method files($file, :$name, :$auth, :$ver) {
        for @*INC {
            if nqp::istype($_,Str) ?? CompUnitRepo::Local::File.new($_) !! $_ -> $cur {
                if $cur.files($name, :$file,:$auth,:$ver).list -> @candi {
                    return @candi;
                }
            }
        }
        ();
    }

    method candidates($name, :$file, :$auth, :$ver) {
        my $list = List.new;
        for @*INC {
            if nqp::istype($_,Str) ?? CompUnitRepo::Local::File.new($_) !! $_ -> $cur {
                if $cur.candidates($name, :$file,:$auth,:$ver).list -> @candi {
                    $list.push(@candi);
                }
            }
        }
        $list;
    }

    method p6ml { $p6ml }

    method load_module($module_name, %opts, *@GLOBALish is rw, :$line, :$file) {
        $lock.protect( {
        my $candi = self.candidates($module_name, :auth(%opts<auth>), :ver(%opts<ver>))[0];
        my %chosen;
        if $candi {
            %chosen<pm>   :=
              $candi<provides>{$module_name}<pm><file> //
              $candi<provides>{$module_name}<pm6><file>;
            %chosen<pm>   := ~%chosen<pm> if %chosen<pm>.DEFINITE;
            %chosen<load> :=
              $candi<provides>{$module_name}{$*VM.precomp-ext}<file>;
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

    # prime the short-id -> type lookup
    my %id2class = (
      file => CompUnitRepo::Local::File,
      inst => CompUnitRepo::Local::Installation,
    );

    method parse-spec ($specs) {
        my @found;
        my $class = %id2class<file>;
        
        # for all possible specs
        for $specs.split(/ \s* ',' \s* /) -> $spec {
            my %options;

            # something we understand
            if $spec ~~ /^
              [ 
                $<type>=[ <.ident>+ % '::' ]
                [ ':' $<n>=\w+
                  <[ < ( [ { ]> $<v>=<[\w-]>+ <[ > ) \] } ]>
                  { %options{$<n>} = ~$<v> }
                ]*
                ':'
              ]?
              $<path>=.+
            $/ {

                # a type (short-id or class name) was specified
                if $<type> -> $this {
                    for %id2class{ $class = ~$this }:v -> $type {
                        $class = $type;
                    }
                }

                # still don't have a type object
                if nqp::istype($class,Str) {
                    my $type = ::($class);

                    # alas, no a known class
                    if nqp::istype($type,Failure) {

                        # it's a short-id
                        if %id2class.exists_key($class) {
                            $class = %id2class{$class}
                        }

                        # give up
                        else {
                            die $class ~~ m/\:/
                              ?? "Must load class '$class' first"
                              !! "Unknown short-id '$class'";
                        }
                    }

                    # successfully converted string to type
                    else {
                        $class = $type;
                        %id2class{$class.short-id} //= $class;
                    }
                }

                # keep this one
                @found.push: $($class, ~$<path>, %options);
            }

            # huh?
            elsif $spec ~~ m/\w+/ {
                die "Don't know what to do with '$spec'";
            }
        }
        @found;
    }
}
