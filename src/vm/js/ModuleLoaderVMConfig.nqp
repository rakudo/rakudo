role Perl6::ModuleLoaderVMConfig {
    method vm_search_paths() {
        my @search_paths;
        @search_paths
    }

    # Finds a setting to load.
    method find_setting($setting_name) {
        $setting_name ~ '.setting.js';
    }

    method file-extension() {
        '.js'
    }
}

# vim: expandtab sw=4
