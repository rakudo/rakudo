role Perl6::ModuleLoaderVMConfig {
    method vm_search_paths() {
        my @search_paths;
        @search_paths.push(nqp::gethllsym('default', 'SysConfig').rakudo-home() ~ '/lib');
        # XXX CHEAT: Goes away when we implement :from<nqp>.
        @search_paths.push(nqp::gethllsym('default', 'SysConfig').nqp-home() ~ '/lib');
        @search_paths
    }

    # Finds a setting to load.
    method find_setting($setting_name) {
        my $path := "$setting_name.setting.moarvm";
        my @prefixes := self.search_path();
        for @prefixes -> $prefix {
            $prefix := nqp::gethllsym('Raku', 'ModuleLoader').absolute_path(~$prefix);
            if nqp::stat("$prefix/$path", 0) {
                $path := "$prefix/$path";
                last;
            }
        }
        $path
    }

    method file-extension() {
        '.moarvm'
    }
}

# vim: expandtab sw=4
