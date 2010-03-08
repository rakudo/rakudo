class Perl6::Module::Locator;

method find_candidates($lookfor, @inc) {
    my @dirs      := pir::split__PSS('::', $lookfor);
    my $file      := @dirs.pop();
    my $localpath := pir::join__PSP('/', @dirs);
 
    my @candidates;
    for @inc {
        my $path := "$_/$localpath";
        if pir::stat__ISI($path, 2) {
            if pir::stat__ISI("$path/$file.pm", 0) {
                @candidates.push("$path/$file.pm");
            }
            my @dir := pir::new__PS('OS').readdir($path);
            for @dir {
                my $match := $_ ~~ /^(<[\w\-\_]>)+\.\d+\.pm$/;
                if $match && $match[0] eq $file {
                    @candidates.push("$path/$_");
                }
            }
        }
    }
    return @candidates;
}

method get_module_info($filename) {
    my %h;
    %h<file>    := $filename;
    # XXX Actually parse here.
    %h<version> := -1;
    %h<auth>    := "";
    return %h;
}

method version_compatible($module_ver, $want_ver) {
    $module_ver == $want_ver
}

method find_module_no_conditions($lookfor, @inc) {
    my @candidates := self.find_candidates($lookfor, @inc);
    my $best;
    for @candidates {
        my $candinfo := get_module_info($_);
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
        my $candinfo := get_module_info($_);
        if !$auth || $candinfo<auth> eq $auth {
            if version_compatible($candinfo<ver>, $ver) {
                @candinfo.push($candinfo);
            }
        }
    }
    return +@candinfo ?? @candinfo[0]<file> !! '';
}
