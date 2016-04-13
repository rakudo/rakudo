role Perl6::ModuleLoaderVMConfig {
    method vm_search_paths() {
        my @search_paths;
        @search_paths
    }
    
    # Finds a setting to load.
    method find_setting($setting_name) {
        nqp::say("TODO - finding setting $setting_name"); 
    }

    method file-extension() {
        '.js'
    }
}
