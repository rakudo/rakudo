my $DEBUG := +nqp::ifnull(nqp::atkey(nqp::getenvhash(), 'RAKUDO_MODULE_DEBUG'), 0);
sub DEBUG(*@strs) {
    my $err := nqp::getstderr();
    nqp::printfh($err, "P6M RMD: ");
    for @strs { nqp::printfh($err, $_) };
    nqp::printfh($err, "\n");
    1;
}

class Perl6::ModuleLoader does Perl6::ModuleLoaderVMConfig {
    my %modules_loaded;
    my %settings_loaded;
    my $absolute_path_func;

    my %language_module_loaders := nqp::hash(
        'NQP', nqp::gethllsym('nqp', 'ModuleLoader'),
    );

    method register_language_module_loader($lang, $loader, :$force) {
        nqp::die("Language loader already registered for $lang")
            if ! $force && nqp::existskey(%language_module_loaders, $lang);
        %language_module_loaders{$lang} := $loader;
    }

    method register_absolute_path_func($func) {
        $absolute_path_func := $func;
    }

    method absolute_path($path) {
        $absolute_path_func ?? $absolute_path_func($path) !! $path;
    }

    method ctxsave() {
        $*MAIN_CTX := nqp::ctxcaller(nqp::ctx());
        $*CTXSAVE := 0;
    }

    method search_path() {
        # See if we have an @*INC set up, and if so just use that.
        my $PROCESS := nqp::gethllsym('perl6', 'PROCESS');
        if !nqp::isnull($PROCESS) {
            if !nqp::existskey($PROCESS.WHO, '@INC') {
                my &DYNAMIC :=
                  nqp::ctxlexpad(%settings_loaded{'CORE'})<&DYNAMIC>;
                if nqp::isnull(&DYNAMIC) {
                    DEBUG('Could not initialize @*INC') if $DEBUG;
                }
                else {
                    DEBUG('Initializing @*INC') if $DEBUG;
                    &DYNAMIC('@*INC');
                }
            }
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
        DEBUG('Setting up default paths: . blib') if $DEBUG;
        my @search_paths;
        @search_paths.push('.');
        @search_paths.push('blib');
        for self.vm_search_paths() {
            @search_paths.push($_);
        }
        @search_paths
    }

    method load_module($module_name, %opts, *@GLOBALish, :$line, :$file, :%chosen) {
        DEBUG("going to load $module_name") if $DEBUG;
        if $module_name eq 'Perl6::BOOTSTRAP' {
            my $preserve_global := nqp::ifnull(nqp::gethllsym('perl6', 'GLOBAL'), NQPMu);
            my %*COMPILING := {};
            my $*CTXSAVE := self;
            my $*MAIN_CTX;
            my $file := 'Perl6/BOOTSTRAP' ~ self.file-extension;
            $file := nqp::stat("blib/$file", 0)
                ?? "blib/$file"
                !! nqp::backendconfig<prefix> ~ '/share/nqp/lib/' ~ $file;
            nqp::loadbytecode($file);
            %modules_loaded{$file} := my $module_ctx := $*MAIN_CTX;
            nqp::bindhllsym('perl6', 'GLOBAL', $preserve_global);
            my $UNIT := nqp::ctxlexpad($module_ctx);
            if +@GLOBALish {
                unless nqp::isnull($UNIT<GLOBALish>) {
                    self.merge_globals(@GLOBALish[0].WHO, $UNIT<GLOBALish>.WHO);
                }
            }
            return $UNIT;
        }

        if nqp::existskey(%language_module_loaders, %opts<from> // 'NQP') {
            # We expect that custom module loaders will accept a Stash, only
            # NQP expects a hash and therefor needs special handling.
            my $CompUnitHandle := $*W.find_symbol(["CompUnit", "Handle"]);
            if %opts<from> eq 'NQP' {
                if +@GLOBALish {
                    my $target := nqp::knowhow().new_type(:name('GLOBALish'));
                    nqp::setwho($target, @GLOBALish[0].WHO.FLATTENABLE_HASH());
                    return $CompUnitHandle.new(
                        %language_module_loaders<NQP>.load_module($module_name, $target)
                    );
                }
                else {
                    return $CompUnitHandle.new(
                        %language_module_loaders<NQP>.load_module($module_name)
                    );
                }
            }
            return %language_module_loaders{%opts<from>}.load_module($module_name,
                %opts, |@GLOBALish, :$line, :$file);
        }
        else {
            nqp::die("Do not know how to load code from " ~ %opts<from>);
        }
    }

    # This is a first cut of the globals merger. For another approach,
    # see sorear++'s work in Niecza. That one is likely more "pure"
    # than this, but that would seem to involve copying too, and the
    # details of exactly what that entails are a bit hazy to me at the
    # moment. We'll see how far this takes us.
    my $stub_how := 'Perl6::Metamodel::PackageHOW';
    my $nqp_stub_how := 'KnowHOW';
    method merge_globals($target, $source) {
        # Start off merging top-level symbols. Easy when there's no
        # overlap. Otherwise, we need to recurse.
        my %known_symbols;
        for stash_hash($target) {
            %known_symbols{$_.key} := 1;
        }
        for stash_hash($source) {
            my $sym := $_.key;
            if !%known_symbols{$sym} {
                ($target){$sym} := $_.value;
            }
            elsif ($target){$sym} =:= $_.value {
                # No problemo; a symbol can't conflict with itself.
            }
            else {
                my $source_mo := $_.value.HOW;
                my $source_is_stub := $source_mo.HOW.name($source_mo) eq $stub_how
                                   || $source_mo.HOW.name($source_mo) eq $nqp_stub_how;
                my $target_mo := ($target){$sym}.HOW;
                my $target_is_stub := $target_mo.HOW.name($target_mo) eq $stub_how
                                   || $source_mo.HOW.name($source_mo) eq $nqp_stub_how;
                if $source_is_stub && $target_is_stub {
                    # Both stubs. We can safely merge the symbols from
                    # the source into the target that's importing them.
                    self.merge_globals(($target){$sym}.WHO, $_.value.WHO);
                }
                elsif $source_is_stub {
                    # The target has a real package, but the source is a
                    # stub. Also fine to merge source symbols into target.
                    self.merge_globals(($target){$sym}.WHO, $_.value.WHO);
                }
                elsif $target_is_stub {
                    # The tricky case: here the interesting package is the
                    # one in the module. So we merge the other way around
                    # and install that as the result.
                    self.merge_globals($_.value.WHO, ($target){$sym}.WHO);
                    ($target){$sym} := $_.value;
                }
                else {
                    nqp::die("P6M Merging GLOBAL symbols failed: duplicate definition of symbol $sym");
                }
            }
        }
    }

    method load_setting($setting_name) {
        my $setting;

        if $setting_name ne 'NULL' {
            # Unless we already did so, locate and load the setting.
            unless nqp::defined(%settings_loaded{$setting_name}) {
                DEBUG("Loading settings $setting_name") if $DEBUG;
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
                DEBUG("Settings $setting_name loaded") if $DEBUG;
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
                    if !nqp::existskey($current, $_.key) || nqp::eqat($_.key, '&', 0) {
                        $current{$_.key} := $_.value;
                    }
                }
            }
            # We could complain about anything else, and may in the future; for
            # now, we let it pass by with "latest wins" semantics.
        }
    }

    sub stash_hash($pkg) {
        my $hash := $pkg;
        unless nqp::ishash($hash) {
            $hash := $hash.FLATTENABLE_HASH();
        }
        $hash
    }
}

# We stash this in the perl6 HLL namespace, just so it's easy to
# locate. Note this makes it invisible inside Perl 6 itself.
nqp::bindhllsym('perl6', 'ModuleLoader', Perl6::ModuleLoader);
