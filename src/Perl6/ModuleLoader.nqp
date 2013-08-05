my $DEBUG := +nqp::ifnull(nqp::atkey(nqp::getenvhash(), 'RAKUDO_MODULE_DEBUG'), 0);
sub DEBUG(*@strs) {
    my $err := nqp::getstderr();
    nqp::printfh($err, "MODULE_DEBUG: ");
    for @strs { nqp::printfh($err, $_) };
    nqp::printfh($err, "\n");
    1;
}

class Perl6::ModuleLoader does Perl6::ModuleLoaderVMConfig {
    my %modules_loaded;
    my %settings_loaded;
    my %language_module_loaders;
    
    method register_language_module_loader($lang, $loader) {
        nqp::die("Language loader already registered for $lang")
            if nqp::existskey(%language_module_loaders, $lang);
        %language_module_loaders{$lang} := $loader;
    }
    
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
        for self.vm_search_paths() {    
            @search_paths.push($_);
        }
        @search_paths
    }
    
    method load_module($module_name, %opts, *@GLOBALish, :$line, :$file?) {
        # See if we need to load it from elsewhere.
        if nqp::existskey(%opts, 'from') {
            if nqp::existskey(%language_module_loaders, %opts<from>) {
                return %language_module_loaders{%opts<from>}.load_module($module_name,
                    %opts, |@GLOBALish, :$line, :$file);
            }
            else {
                nqp::die("Do not know how to load code from " ~ %opts<from>);
            }
        }
        
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
                    join(', ', @prefixes));
            }
        }
        my %chosen := @candidates[0];
        
        my @MODULES := nqp::clone(@*MODULES // []);
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
#?if parrot
                my $fh := nqp::open(%chosen<pm>, 'r');
                $fh.encoding('utf8');
                my $source := $fh.readall();
                $fh.close();
#?endif
#?if jvm
                my $fh := nqp::open(%chosen<pm>, 'r');
                nqp::setencoding($fh, 'utf8');
                my $source := nqp::readallfh($fh);
                nqp::closefh($fh);
#?endif
                
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
            my $UNIT := nqp::ctxlexpad($module_ctx);
            if +@GLOBALish {
                unless nqp::isnull($UNIT<GLOBALish>) {
                    merge_globals(@GLOBALish[0], $UNIT<GLOBALish>);
                }
            }
            return $UNIT;
        }
        else {
            return {};
        }
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
        for stash_hash($target) {
            %known_symbols{$_.key} := 1;
        }
        for stash_hash($source) {
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
                my $path := self.find_setting($setting_name);
                
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
                for $orig.FLATTENABLE_HASH() {
                    unless nqp::existskey($current, $_.key) {
                        $current{$_.key} := $_.value;
                    }
                }
            }
            # We could complain about anything else, and may in the future; for
            # now, we let it pass by with "latest wins" semantics.
        }
    }
    
    sub stash_hash($pkg) {
        my $hash := $pkg.WHO;
        unless nqp::ishash($hash) {
            $hash := $hash.FLATTENABLE_HASH();
        }
        $hash
    }
}

# We stash this in the perl6 HLL namespace, just so it's easy to
# locate. Note this makes it invisible inside Perl 6 itself.
nqp::bindhllsym('perl6', 'ModuleLoader', Perl6::ModuleLoader);
