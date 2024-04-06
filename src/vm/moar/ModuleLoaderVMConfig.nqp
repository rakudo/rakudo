role Perl6::ModuleLoaderVMConfig {
    my @search_paths;

    method vm_search_paths() { @search_paths }
    method file-extension()  { '.moarvm'     }

    # Finds a setting to load.
    method find_setting($setting_name) {
        my str $path := "$setting_name.setting.moarvm";
        my @prefixes := self.search_path;

        if (my int $m := nqp::elems(@prefixes)) {
            my $absolutifier := nqp::gethllsym('Raku', 'ModuleLoader');

            my int $i;
            while $i < $m {
                my str $abspath :=
                  $absolutifier.absolute_path(nqp::atpos(@prefixes, $i)),
                  ~ "/" ~ $path;
                nqp::stat($abspath, 0)
                  ?? (return $abspath)
                  !! ++$i;
            }
        }

        # None of the prefixes matched, so do the relative path
        $path
    }

}

# vim: expandtab sw=4
