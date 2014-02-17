class CompUnitRepo {
    my Mu $p6ml := nqp::gethllsym('perl6', 'ModuleLoader');
    my %repos;

    method files($file, :$name, :$auth, :$ver) {
        for %repos.sort -> $prio {
            my @candi;
            for @($prio.value) {
                @candi := (@candi, .files($file, :$name, :$auth, :$ver)).flat
            }
            @candi.sort: { ($^b<ver> // Version.new('0')) cmp ($^a<ver> // Version.new('0')) };
            return @candi if +@candi
        }
    }

    method candidates($name, :$file, :$auth, :$ver) {
        for %repos.sort -> $prio {
            my @candi;
            for @($prio.value) {
                @candi := (@candi, .candidates($name, :$file, :$auth, :$ver)).flat
            }
            @candi.sort: { ($^b<ver> // Version.new('0')) cmp ($^a<ver> // Version.new('0')) };
            return @candi if +@candi
        }
    }

    method add_repo($repo, :$name, :$prio = 0) {
        %repos{$prio}.push: $repo;
        %*CUSTOM_LIB{$name} := $repo if $name
    }

    method p6ml { $p6ml }

    method load_module($module_name, %opts, *@GLOBALish is rw, :$line, :$file) {
        my $candi = self.candidates($module_name, :auth(%opts<auth>), :ver(%opts<ver>))[0];
        my %chosen;
        if $candi {
            %chosen<pm>   := $candi<provides>{$module_name}<pm><file>;
#?if parrot
            %chosen<load> := $candi<provides>{$module_name}<pir><file>;
#?endif
#?if jvm
            %chosen<load> := $candi<provides>{$module_name}<jar><file>;
#?endif
#?if moar
            %chosen<load> := $candi<provides>{$module_name}<moarvm><file>;
#?endif
            %chosen<key>  := %chosen<pm> // %chosen<load>;
        }
        $p6ml.load_module($module_name, %opts, |@GLOBALish, :$line, :$file, :%chosen);
    }

    method ctxsave() { $p6ml.ctxsave() }
    method absolute_path($path) { $p6ml.absolute_path($path) }
    method load_setting($setting_name) { $p6ml.load_setting($setting_name) }
    method resolve_repossession_conflicts(@conflicts) {
        $p6ml.resolve_repossession_conflicts( nqp::findmethod(@conflicts, 'FLATTENABLE_LIST')(@conflicts) )
    }
}
