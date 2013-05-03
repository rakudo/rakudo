my $DEBUG := +nqp::atkey(nqp::getenvhash(), 'RAKUDO_MODULE_DEBUG');
sub DEBUG(*@strs) {
    my $err := nqp::getstderr();
    $err.print("MODULE_DEBUG: ");
    for @strs { $err.print($_) };
    $err.print("\n");
    1;
}

class Perl6::ModuleLoader {
    my %modules_loaded;
    my %settings_loaded;
    
    method ctxsave() {
        $*MAIN_CTX := nqp::ctxcaller(nqp::ctx());
        $*CTXSAVE := 0;
    }
    
    method search_path() {
        # See if we have an @*INC set up, and if so just use that.
        my $PROCESS := nqp::gethllsym('perl6', 'PROCESS');
        if !nqp::isnull($PROCESS) && nqp::existskey($PROCESS.WHO, '@INC') {
            my $INC := ($PROCESS.WHO)<@INC>;
            if nqp::defined($INC) {
                my @INC := $INC.FLATTENABLE_LIST();
                if +@INC {
                    return @INC;
                }
            }
        }
        
        # Too early to have @*INC; probably no setting yet loaded to provide
        # the PROCESS initialization.
        my @search_paths;
        @search_paths.push('.');
        @search_paths.push('blib');
        my %conf := pir::getinterp__P()[pir::const::IGLOBALS_CONFIG_HASH];
        @search_paths.push(%conf<libdir> ~ %conf<versiondir> ~
            '/languages/perl6/lib');
        # XXX CHEAT: Goes away when we implement :from<nqp>.
        @search_paths.push(%conf<libdir> ~ %conf<versiondir> ~
            '/languages/nqp/lib');
        @search_paths
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
                if $ext eq 'pbc' || $ext eq 'pir' {
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
            my $base_path := nqp::join('/', nqp::split('::', $module_name));
            my $pbc_path  := $base_path ~ '.pbc';
            my $pir_path  := $base_path ~ '.pir';
            my $pm_path   := $base_path ~ '.pm';
            my $pm6_path  := $base_path ~ '.pm6';
            
            # Go through the prefixes and build a candidate list.
            for @prefixes -> $prefix {
                $prefix := ~$prefix;
                my $have_pm  := nqp::stat("$prefix/$pm_path", 0);
                my $have_pm6 := nqp::stat("$prefix/$pm6_path", 0);
                my $have_pir := nqp::stat("$prefix/$pir_path", 0);
                my $have_pbc := nqp::stat("$prefix/$pbc_path", 0);
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
                    if $have_pir && nqp::stat("$prefix/$pir_path", 7)
                                 >= nqp::stat("$prefix/$pm_path", 7) {
                        %cand<load> := "$prefix/$pir_path";
                    }
                    elsif $have_pbc && nqp::stat("$prefix/$pbc_path", 7)
                                    >= nqp::stat("$prefix/$pm_path", 7) {
                        %cand<load> := "$prefix/$pbc_path";
                    }
                    @candidates.push(%cand);
                }
                elsif $have_pir {
                    my %cand;
                    %cand<key>  := "$prefix/$pir_path";
                    %cand<load> := "$prefix/$pir_path";
                    @candidates.push(%cand);
                }
                elsif $have_pbc {
                    my %cand;
                    %cand<key>  := "$prefix/$pbc_path";
                    %cand<load> := "$prefix/$pbc_path";
                    @candidates.push(%cand);
                }
            }
        }
        @candidates
    }
    
    method load_module($module_name, *@GLOBALish, :$line, :$file?) {
        # Locate all the things that we potentially could load. Choose
        # the first one for now (XXX need to filter by version and auth).
        my @prefixes   := self.search_path();
        my @candidates := self.locate_candidates($module_name, @prefixes, :$file);
        if +@candidates == 0 {
            if nqp::defined($file) {
                nqp::die("Could not find file '$file' for module $module_name");
            }
            else {
                nqp::die("Could not find $module_name in any of: " ~
                    nqp::join(', ', @prefixes));
            }
        }
        my %chosen := @candidates[0];
        
        my @MODULES := nqp::clone(@*MODULES);
        for @MODULES -> $m {
            if $m<module> eq $module_name {
                nqp::die("Circular module loading detected involving module '$module_name'");
            }
        }

        # If we didn't already do so, load the module and capture
        # its mainline. Otherwise, we already loaded it so go on
        # with what we already have.
        my $module_ctx;
        if nqp::defined(%modules_loaded{%chosen<key>}) {
            $module_ctx := %modules_loaded{%chosen<key>};
        }
        else {
            my @*MODULES := @MODULES;
            if +@*MODULES  == 0 {
                my %prev        := nqp::hash();
                %prev<line>     := $line;
                %prev<filename> := nqp::getlexdyn('$?FILES');
                @*MODULES[0]    := %prev;
            }
            else {
                @*MODULES[-1]<line> := $line;
            }
            my %trace := nqp::hash();
            %trace<module>   := $module_name;
            %trace<filename> := %chosen<pm>;
            my $preserve_global := nqp::ifnull(nqp::gethllsym('perl6', 'GLOBAL'), NQPMu);
            nqp::push(@*MODULES, %trace);
            if %chosen<load> {
                %trace<precompiled> := %chosen<load>;
                DEBUG("loading ", %chosen<load>) if $DEBUG;
                my %*COMPILING := {};
                my $*CTXSAVE := self;
                my $*MAIN_CTX;
                nqp::loadbytecode(%chosen<load>);
                %modules_loaded{%chosen<key>} := $module_ctx := $*MAIN_CTX;
                DEBUG("done loading ", %chosen<load>) if $DEBUG;
            }
            else {
                # If we're doing module pre-compilation, we should only
                # allow the modules we load to be pre-compiled also.
                my $precomp := 0;
                try $precomp := $*W.is_precompilation_mode();
                if $precomp {
                    nqp::die(
                        "When pre-compiling a module, its dependencies must be pre-compiled first.\n" ~
                        "Please pre-compile " ~ %chosen<pm>);
                }
                
                # Read source file.
                DEBUG("loading ", %chosen<pm>) if $DEBUG;
                my $fh := nqp::open(%chosen<pm>, 'r');
                $fh.encoding('utf8');
                my $source := $fh.readall();
                $fh.close();
                
                # Get the compiler and compile the code, then run it
                # (which runs the mainline and captures UNIT).
                my $?FILES   := %chosen<pm>;
                my $eval     := nqp::getcomp('perl6').compile($source);
                my $*CTXSAVE := self;
                my $*MAIN_CTX;
                $eval();
                %modules_loaded{%chosen<key>} := $module_ctx := $*MAIN_CTX;
                DEBUG("done loading ", %chosen<pm>) if $DEBUG;

            }
            nqp::bindhllsym('perl6', 'GLOBAL', $preserve_global);
            CATCH {
                nqp::bindhllsym('perl6', 'GLOBAL', $preserve_global);
                nqp::rethrow($_);
            }
        }

        # Provided we have a mainline and need to do global merging...
        if nqp::defined($module_ctx) {
            # Merge any globals.
            if +@GLOBALish {
                my $UNIT := nqp::ctxlexpad($module_ctx);
                unless nqp::isnull($UNIT<GLOBALish>) {
                    merge_globals(@GLOBALish[0], $UNIT<GLOBALish>);
                }
            }
        }

        return $module_ctx;
    }
    
    # This is a first cut of the globals merger. For another approach,
    # see sorear++'s work in Niecza. That one is likely more "pure"
    # than this, but that would seem to involve copying too, and the
    # details of exactly what that entails are a bit hazy to me at the
    # moment. We'll see how far this takes us.
    my $stub_how := 'Perl6::Metamodel::PackageHOW';
    sub merge_globals($target, $source) {
        # Start off merging top-level symbols. Easy when there's no
        # overlap. Otherwise, we need to recurse.
        my %known_symbols;
        for $target.WHO {
            %known_symbols{$_.key} := 1;
        }
        for $source.WHO {
            my $sym := $_.key;
            if !%known_symbols{$sym} {
                ($target.WHO){$sym} := $_.value;
            }
            elsif ($target.WHO){$sym} =:= $_.value {
                # No problemo; a symbol can't conflict with itself.
            }
            else {
                my $source_mo := $_.value.HOW;
                my $source_is_stub := $source_mo.HOW.name($source_mo) eq $stub_how;
                my $target_mo := ($target.WHO){$sym}.HOW;
                my $target_is_stub := $target_mo.HOW.name($target_mo) eq $stub_how;
                if $source_is_stub && $target_is_stub {
                    # Both stubs. We can safely merge the symbols from
                    # the source into the target that's importing them.
                    merge_globals(($target.WHO){$sym}, $_.value);
                }
                elsif $source_is_stub {
                    # The target has a real package, but the source is a
                    # stub. Also fine to merge source symbols into target.
                    merge_globals(($target.WHO){$sym}, $_.value);
                }
                elsif $target_is_stub {
                    # The tricky case: here the interesting package is the
                    # one in the module. So we merge the other way around
                    # and install that as the result.
                    merge_globals($_.value, ($target.WHO){$sym});
                    ($target.WHO){$sym} := $_.value;
                }
                else {
                    nqp::die("Merging GLOBAL symbols failed: duplicate definition of symbol $sym");
                }
            }
        }
    }
    
    method load_setting($setting_name) {
        my $setting;
        
        if $setting_name ne 'NULL' {
            # Unless we already did so, locate and load the setting.
            unless nqp::defined(%settings_loaded{$setting_name}) {
                # Find it.
                my $path := "$setting_name.setting.pbc";
                my @prefixes := self.search_path();
                for @prefixes -> $prefix {
                    $prefix := ~$prefix;
                    if nqp::stat("$prefix/$path", 0) {
                        $path := "$prefix/$path";
                        last;
                    }
                }
                
                # Load it.
                my $*CTXSAVE := self;
                my $*MAIN_CTX;
                my $preserve_global := nqp::ifnull(nqp::gethllsym('perl6', 'GLOBAL'), NQPMu);
                nqp::scwbdisable();
                nqp::loadbytecode($path);
                nqp::scwbenable();
                nqp::bindhllsym('perl6', 'GLOBAL', $preserve_global);
                unless nqp::defined($*MAIN_CTX) {
                    nqp::die("Unable to load setting $setting_name; maybe it is missing a YOU_ARE_HERE?");
                }
                %settings_loaded{$setting_name} := $*MAIN_CTX;
            }
            
            $setting := %settings_loaded{$setting_name};
        }
        
        return $setting;
    }
    
    # Handles any object repossession conflicts that occurred during module load,
    # or complains about any that cannot be resolved.
    method resolve_repossession_conflicts(@conflicts) {
        for @conflicts -> $orig, $current {
            # If it's a Stash in conflict, we make sure any original entries get
            # appropriately copied.
            if $orig.HOW.name($orig) eq 'Stash' {
                for $orig {
                    unless nqp::existskey($current, $_.key) {
                        $current{$_.key} := $_.value;
                    }
                }
            }
            # We could complain about anything else, and may in the future; for
            # now, we let it pass by with "latest wins" semantics.
        }
    }
}

# We stash this in the perl6 HLL namespace, just so it's easy to
# locate. Note this makes it invisible inside Perl 6 itself.
nqp::bindhllsym('perl6', 'ModuleLoader', Perl6::ModuleLoader);
