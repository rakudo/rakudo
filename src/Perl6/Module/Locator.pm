# This class handles the mapping of a module name to a file on disk.
# For now it does it purely by looking through all of the candidates;
# in the future, it should be updated to do caching.
class Perl6::Module::Locator;

method find_candidates($lookfor, @inc) {
    # convert $lookfor A::B::C into $localpath A/B/ and $file C
    my @dirs      := pir::split__PSS('::', $lookfor);
    my $file      := @dirs.pop();
    my $localpath := +@dirs ?? pir::join__SSP('/', @dirs) ~ '/' !! '';
    # pir::say("\nlookfor: $lookfor\nlocalpath: $localpath\nfile: $file");

    # within each @inc path look for "$localpath/$file.pm"
    my @candidates;
    for @inc {
        my $path := "$_/$localpath";
        # pir::say("  path: $path");
        my $check_path := pir::substr__SSII($path, 0, pir::length__IS($path) - 1);
        if pir::stat__ISI($check_path, 0) && pir::stat__ISI($check_path, 2) {
            my @dir := pir::new__PS('OS').readdir($path);
            my $candidate := "";
            for @dir {
                # pir::say("    readdir: $_");
                if pir::substr__SSII($_, 0, pir::length__IS($file) + 1) eq $file ~ '.' {
                    if pir::substr__SSII($_, pir::length__IS($_) - 4, 4) eq '.pm6' ||
                       !$candidate && pir::substr__SSII($_, pir::length__IS($_) - 3, 3) eq '.pm' {
                        $candidate := "$path$_";
                    }
                }
            }
            if $candidate {
                @candidates.push($candidate);
                # pir::say("      found: $candidate");
            }
        }
    }
    return @candidates;
}

method get_module_info($filename) {
    # Set filename and defaults.
    my %h;
    %h<file>    := $filename;
    %h<version> := -1;
    %h<auth>    := "";
    
    # Read in file and parse it.
    my $fh     := pir::open__PSS($filename, 'r');
    $fh.encoding('utf8');
    my $source := $fh.readall();
    $fh.close();
    my $actions := Perl6::Module::VersionDetectionActions.new();
    try {
        Perl6::Grammar.parse($source, :actions($actions));
    };
    %h<ver>  := $actions.ver();
    %h<auth> := $actions.auth();
    return %h;
}

method version_compatible($module_ver, $want_ver) {
    $module_ver == $want_ver
}

method find_module_no_conditions($lookfor, @inc) {
    my @candidates := self.find_candidates($lookfor, @inc);
    my $best;
    for @candidates {
        my $candinfo := self.get_module_info($_);
        if !$best || $candinfo<ver> > $best<ver> {
            $best := $candinfo;
        }
    }
    return $best ?? $best<file> !! '';
}

method find_module($lookfor, @inc, $ver, $auth?) {
    my @candidates := self.find_candidates($lookfor, @inc);
    my @candinfo;
    for @candidates {
        my $candinfo := self.get_module_info($_);
        if !$auth || $candinfo<auth> eq $auth {
            if self.version_compatible($candinfo<ver>, $ver) {
                @candinfo.push($candinfo);
            }
        }
    }
    return +@candinfo ?? @candinfo[0]<file> !! '';
}
