role Perl6::ModuleLoaderVMConfig {
    method vm_search_paths() {
        my @search_paths;
        @search_paths
    }
    
    # Finds a setting to load.
    method find_setting($setting_name) {
        say("TODO - finding setting"); 
    }

    method file-extension() {
        '.js'
    }
}
