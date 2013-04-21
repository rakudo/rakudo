role Perl6::ModuleLoaderVMConfig {
    method vm_search_paths() {
        # XXX TODO
        []
    }
    
    # Locates files we could potentially load for this module.
    method locate_candidates($module_name, @prefixes, :$file?) {
        # If its name contains a slash or dot treat is as a path rather than a package name.
        my @candidates;
        if nqp::defined($file) {
            if nqp::stat($file, 0) {
                my %cand;
                %cand<key> := $file;
                my $dot := nqp::rindex($file, '.');
                my $ext := $dot >= 0 ?? nqp::substr($file, $dot, nqp::chars($file) - $dot) !! '';
                if $ext eq 'class' {
                    %cand<load> := $file;
                }
                else {
                    %cand<pm> := $file;
                }
                @candidates.push(%cand);
            }
        }
        else {
            # Assemble various files we'd look for.
            my $base_path  := nqp::join('/', nqp::split('::', $module_name));
            my $class_path := $base_path ~ '.class';
            my $pm_path    := $base_path ~ '.pm';
            my $pm6_path   := $base_path ~ '.pm6';
            
            # Go through the prefixes and build a candidate list.
            for @prefixes -> $prefix {
                $prefix := ~$prefix;
                my $have_pm    := nqp::stat("$prefix/$pm_path", 0);
                my $have_pm6   := nqp::stat("$prefix/$pm6_path", 0);
                my $have_class := nqp::stat("$prefix/$class_path", 0);
                if $have_pm6 {
                    # if there are both .pm and .pm6 we assume that
                    # the former is a Perl 5 module and use the latter
                    $have_pm := 1;
                    $pm_path := $pm6_path;
                }
                if $have_pm {
                    my %cand;
                    %cand<key> := "$prefix/$pm_path";
                    %cand<pm>  := "$prefix/$pm_path";
                    if $have_class && nqp::stat("$prefix/$class_path", 7)
                                    >= nqp::stat("$prefix/$pm_path", 7) {
                        %cand<load> := "$prefix/$class_path";
                    }
                    @candidates.push(%cand);
                }
                elsif $have_class {
                    my %cand;
                    %cand<key>  := "$prefix/$class_path";
                    %cand<load> := "$prefix/$class_path";
                    @candidates.push(%cand);
                }
            }
        }
        @candidates
    }
    
    # Finds a setting to load.
    method find_setting($setting_name) {
        my $path := "$setting_name.setting.class";
        my @prefixes := self.search_path();
        for @prefixes -> $prefix {
            $prefix := ~$prefix;
            if nqp::stat("$prefix/$path", 0) {
                $path := "$prefix/$path";
                last;
            }
        }
        $path
    }
}
