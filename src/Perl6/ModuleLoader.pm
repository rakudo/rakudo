class Perl6::ModuleLoader {
    my %modules_loaded;
    my %settings_loaded;
    
    method ctxsave() {
        $*MAIN_CTX := Q:PIR {
            getinterp $P0
            %r = $P0['context';1]
        };
        $*CTXSAVE := 0;
    }
    
    method search_path() {
        # See if we have an @*INC set up, and if so just use that.
        my $hll_ns := pir::get_root_global__PS('perl6');
        if pir::exists($hll_ns, 'PROCESS') && pir::exists($hll_ns<PROCESS>.WHO, '@INC') {
            my $INC := ($hll_ns<PROCESS>.WHO)<@INC>;
            if pir::defined($INC) {
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
    method locate_candidates($module_name, @prefixes) {
        # Assemble various files we'd look for.
        my $base_path := pir::join('/', pir::split('::', $module_name));
        my $pbc_path  := $base_path ~ '.pbc';
        my $pir_path  := $base_path ~ '.pir';
        my $pm_path   := $base_path ~ '.pm';
        my $pm6_path  := $base_path ~ '.pm6';
        
        # Go through the prefixes and build a candidate list.
        my @candidates;
        for @prefixes -> $prefix {
            $prefix := ~$prefix;
            my $have_pm  := pir::stat__isi("$prefix/$pm_path", 0);
            my $have_pm6 := pir::stat__isi("$prefix/$pm6_path", 0);
            my $have_pir := pir::stat__isi("$prefix/$pir_path", 0);
            my $have_pbc := pir::stat__isi("$prefix/$pbc_path", 0);
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
                if $have_pir && pir::stat__ISI("$prefix/$pir_path", 7)
                             >= pir::stat__ISI("$prefix/$pm_path", 7) {
                    %cand<load> := "$prefix/$pir_path";
                }
                elsif $have_pbc && pir::stat__ISI("$prefix/$pbc_path", 7)
                                >= pir::stat__ISI("$prefix/$pm_path", 7) {
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
        @candidates
    }
    
    method load_module($module_name, *@GLOBALish) {
        # Locate all the things that we potentially could load. Choose
        # the first one for now (XXX need to filter by version and auth).
        my @prefixes   := self.search_path();
        my @candidates := self.locate_candidates($module_name, @prefixes);
        if +@candidates == 0 {
            pir::die("Could not find $module_name in any of: " ~
                pir::join(', ', @prefixes));
        }
        my %chosen := @candidates[0];
        
        # If we didn't already do so, load the module and capture
        # its mainline. Otherwise, we already loaded it so go on
        # with what we already have.
        my $module_ctx;
        if pir::defined(%modules_loaded{%chosen<key>}) {
            $module_ctx := %modules_loaded{%chosen<key>};
        }
        else {
            my $preserve_global := pir::get_hll_global__Ps('GLOBAL');
            if %chosen<load> {
                my %*COMPILING := {};
                my $*CTXSAVE := self;
                my $*MAIN_CTX;
                pir::load_bytecode(%chosen<load>);
                %modules_loaded{%chosen<key>} := $module_ctx := $*MAIN_CTX;
            }
            else {
                # Read source file.
                my $fh := pir::open__PSs(%chosen<pm>, 'r');
                $fh.encoding('utf8');
                my $source := $fh.readall();
                $fh.close();
                
                # Get the compiler and compile the code, then run it
                # (which runs the mainline and captures UNIT).
                my $?FILES   := %chosen<pm>;
                my $eval     := pir::compreg__Ps('perl6').compile($source);
                my $*CTXSAVE := self;
                my $*MAIN_CTX;
                $eval();
                %modules_loaded{%chosen<key>} := $module_ctx := $*MAIN_CTX;
            }
            pir::set_hll_global__vsP('GLOBAL', $preserve_global);
        }

        # Provided we have a mainline and need to do global merging...
        if pir::defined($module_ctx) {
            # Merge any globals.
            if +@GLOBALish {
                my $UNIT := pir::getattribute__PPs($module_ctx, 'lex_pad');
                unless pir::isnull($UNIT<GLOBALish>) {
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
                    pir::die("Merging GLOBAL symbols failed: duplicate definition of symbol $sym");
                }
            }
        }
    }
    
    method load_setting($setting_name) {
        my $setting;
        
        if $setting_name ne 'NULL' {
            # Unless we already did so, locate and load the setting.
            unless pir::defined(%settings_loaded{$setting_name}) {
                # Find it.
                my $path := "$setting_name.setting.pbc";
                my @prefixes := self.search_path();
                for @prefixes -> $prefix {
                    $prefix := ~$prefix;
                    if pir::stat__isi("$prefix/$path", 0) {
                        $path := "$prefix/$path";
                        last;
                    }
                }
                
                # Load it.
                my $*CTXSAVE := self;
                my $*MAIN_CTX;
                my $preserve_global := pir::get_hll_global__Ps('GLOBAL');
                pir::load_bytecode($path);
                pir::set_hll_global__vsP('GLOBAL', $preserve_global);
                unless pir::defined($*MAIN_CTX) {
                    pir::die("Unable to load setting $setting_name; maybe it is missing a YOU_ARE_HERE?");
                }
                %settings_loaded{$setting_name} := $*MAIN_CTX;
            }
            
            $setting := %settings_loaded{$setting_name};
        }
        
        return $setting;
    }
}

# We stash this in the perl6 HLL namespace, just so it's easy to
# locate. Note this makes it invisible inside Perl 6 itself.
pir::set_root_global__vPsP(['perl6'], 'ModuleLoader', Perl6::ModuleLoader);
