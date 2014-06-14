class CompUnitRepo::Local::File { ... }

class CompUnitRepo {
    my Mu $p6ml := nqp::gethllsym('perl6', 'ModuleLoader');
    my $lock := Lock.new;

    method files($file, :$name, :$auth, :$ver) {
        for @*INC -> $group {
            my @candi;
            for $group.list {
                my $cur = $_ ~~ Str ?? CompUnitRepo::Local::File.new($_) !! $_;
                @candi := (@candi, $cur.files($file, :$name, :$auth, :$ver)).flat
            }
            return @candi.sort: { ($^b<ver> // Version.new('0')) cmp ($^a<ver> // Version.new('0')) } if +@candi
        }
    }

    method candidates($name, :$file, :$auth, :$ver) {
        for @*INC -> $group {
            my @candi;
            for $group.list {
                my $cur = $_ ~~ Str ?? CompUnitRepo::Local::File.new($_) !! $_;
                @candi := (@candi, $cur.candidates($name, :$file, :$auth, :$ver)).flat
            }
            return @candi.sort: { ($^b<ver> // Version.new('0')) cmp ($^a<ver> // Version.new('0')) } if +@candi
        }
    }

    method p6ml { $p6ml }

    method load_module($module_name, %opts, *@GLOBALish is rw, :$line, :$file) {
        $lock.protect( {
        my $candi = self.candidates($module_name, :auth(%opts<auth>), :ver(%opts<ver>))[0];
        my %chosen;
        if $candi {
            %chosen<pm>   :=
              $candi<provides>{$module_name}<pm><file>;
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
}
