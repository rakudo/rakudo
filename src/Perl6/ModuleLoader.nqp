my $rakudo-module-debug := nqp::atkey(nqp::getenvhash(), 'RAKUDO_MODULE_DEBUG');
my $DEBUG := nqp::stmts((my $debug-radix := nqp::radix(10, $rakudo-module-debug, 0, 0)),($debug-radix[2] != -1))
?? ?$debug-radix[0] !! ?nqp::chars($rakudo-module-debug);
sub DEBUG(*@strs) {
    my $err := stderr();
    $err.print(" " ~ $rakudo-module-debug ~ nqp::x(" ", ($rakudo-module-debug - 1) * 4) ~ " RMD: ");
    for @strs { $err.print($_) };
    $err.print("\n");
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
        nqp::existskey(%language_module_loaders, $lang) && !$force
          ?? nqp::die("Language loader already registered for $lang")
          !! nqp::bindkey(%language_module_loaders, $lang, $loader);
    }

    method register_absolute_path_func($func) {
        $absolute_path_func := $func;
    }

    method absolute_path($path) {
        $absolute_path_func ?? $absolute_path_func($path) !! $path;
    }

    method ctxsave() {
        $*MAIN_CTX := nqp::ctxcaller(nqp::ctx);
        $*CTXSAVE := 0;
    }

    method search_path() { self.vm_search_paths }

    method load_module($module_name, %opts, *@GLOBALish, :$line, :$file, :%chosen) {
        DEBUG("going to load $module_name") if $DEBUG;

        if nqp::eqat($module_name, 'Perl6::BOOTSTRAP::v6', 0) {
            my $preserve_global := nqp::gethllsym('Raku', 'GLOBAL');
            my %*COMPILING := {};
            my $*CTXSAVE := self;
            my $*MAIN_CTX;
            my str $file := nqp::join('/', nqp::split('::', $module_name)) ~ self.file-extension;

            my @prefixes := self.search_path;
            if (my int $m := nqp::elems(@prefixes)) {
                my int $i;
                while $i < $m {
                    my str $prefix := nqp::atpos(@prefixes, $i);
                    if nqp::stat("$prefix/$file", 0) {
                        $file := "$prefix/$file";
                        $i := $m;  # last without control exception
                    }
                    else {
                        ++$i;
                    }
                }
            }
            my $ctx := nqp::atkey(%modules_loaded, $file);

            # Not loaded yet, need to load now
            if nqp::isnull($ctx) {
                nqp::loadbytecode($file);
                nqp::bindkey(
                  %modules_loaded, $file, my $module_ctx := $*MAIN_CTX
                );
                nqp::bindhllsym('Raku', 'GLOBAL', $preserve_global);

                my $UNIT := nqp::ctxlexpad($module_ctx);
                self.merge_globals(
                  nqp::atpos(@GLOBALish, 0).WHO,
                  nqp::atkey($UNIT, 'GLOBALish').WHO
                ) if nqp::elems(@GLOBALish)
                  && nqp::not_i(nqp::isnull(nqp::atkey($UNIT, 'GLOBALish')));

                $UNIT
            }
            
            # Already loaded
            else {
                nqp::ctxlexpad($ctx)
            }
        }

        # Not a bootstrap file
        else {
            my str $from := nqp::ifnull(nqp::atkey(%opts, 'from'), 'NQP');
            my $loader   := nqp::atkey(%language_module_loaders, $from);

            if nqp::isnull($loader) {
                nqp::die("Do not know how to load code from $from");
            }

            # A loader is available
            else {
                # We expect that custom module loaders will accept a Stash, only
                # NQP expects a hash and therefor needs special handling.
                if $from eq 'NQP' {
                    if nqp::elems(@GLOBALish) {
                        my $target := nqp::knowhow.new_type(:name('GLOBALish'));
                        nqp::setwho(
                          $target,
                          nqp::atpos(@GLOBALish, 0).WHO.FLATTENABLE_HASH
                        );
                        $loader.load_module($module_name, $target);
                    }
                    else {
                        $loader.load_module($module_name);
                    }
                }
                else {
                    $loader.load_module(
                      $module_name, %opts, |@GLOBALish, :$line, :$file
                    )
                }
            }
        }
    }

    # This is a first cut of the globals merger. For another approach,
    # see sorear++'s work in Niecza. That one is likely more "pure"
    # than this, but that would seem to involve copying too, and the
    # details of exactly what that entails are a bit hazy to me at the
    # moment. We'll see how far this takes us.
    my str $raku_stub_how_name := 'Perl6::Metamodel::PackageHOW';
    my str $nqp_stub_how_name  := 'KnowHOW';
    sub is_HOW_stub($target) {
         my $targetHOW := $target.HOW;
         my str $name  := $targetHOW.HOW.name($targetHOW);
         $name eq $raku_stub_how_name || $name eq $nqp_stub_how_name
    }
    method merge_globals($target, $source) {
        my $metamodel-configuration := nqp::gethllsym('Raku', 'METAMODEL_CONFIGURATION');
        if !nqp::isnull($metamodel-configuration) && nqp::istype($target, $metamodel-configuration.stash_type()) {
            # merge-symbols will loop back on this method again, but would lock-protect itself first.
            $target.merge-symbols($source);
        }
        elsif stash_hash($source) -> %source {
            # Start off merging top-level symbols. Easy when there's no
            # overlap. Otherwise, we need to recurse.
            my %known_symbols;
            my $iter := nqp::iterator(stash_hash($target));
            nqp::while(
              $iter,
              nqp::bindkey(%known_symbols,nqp::iterkey_s(nqp::shift($iter)),1)
            );
            for sorted_keys(%source) -> $sym {
                my $value := %source{$sym};
                if nqp::not_i(%known_symbols{$sym}) {
                    $target{$sym} := $value;
                }
                elsif nqp::decont(my $target_sym := $target{$sym}) =:=
                  nqp::decont($value) { # Stash entries are containerized
                    # No problemo; a symbol can't conflict with itself.
                }
                elsif is_HOW_stub($value) {
                    # Since the source is a stub, it doesn't matter whether
                    # the target is also a stub or not.  In either case,
                    # it is fine to merge source symbols into target.
                    self.merge_globals($target_sym.WHO, $value.WHO);
                }
                elsif is_HOW_stub($target_sym) {
                    # The tricky case: here the interesting package is the
                    # one in the module. So we merge the other way around
                    # and install that as the result.
                    self.merge_globals($value.WHO, $target_sym.WHO);
                    $target{$sym} := $value;
                }
                elsif nqp::eqat($sym, '&', 0) {
                    # "Latest wins" semantics for functions
                    $target{$sym} := $value;
                }
                else {
                    # Potentially do other conflict resolution in the future
                    nqp::die("Merging GLOBAL symbols failed: duplicate definition of symbol $sym");
                }
            }
        }
    }

    method merge_globals_lexically($world, $target, $source) {
        my %source := stash_hash($source);

        if (my int $m := nqp::elems(%source)) {

            # Set up known symbols for this target
            my %known_symbols;
            my $iter := nqp::iterator(stash_hash($target.symtable));
            nqp::while(
              $iter,
              nqp::bindkey(
                %known_symbols,
                nqp::iterkey_s(nqp::shift($iter)),
                nqp::atkey(nqp::iterval($iter), 'value')
              )
            );

            # Start off merging top-level symbols. Easy when there's no
            # overlap. Otherwise, we need to recurse.
            my @keys := sorted_keys(%source);
            my int $i;
            while $i < $m {
                my str $symbol := nqp::atpos_s(@keys, $i);
                my     $value  := nqp::atkey(%source, $symbol);
                my int $outer;

                unless nqp::existskey(%known_symbols, $symbol) {
                    try {
                        nqp::bindkey(
                          %known_symbols,
                          $symbol,
                          $world.find_single_symbol($symbol)
                        );
                        $outer := 1;
                    }
                }

                # We have a potential collision
                if nqp::existskey(%known_symbols, $symbol) {
                    my $existing := nqp::atkey(%known_symbols, $symbol);

                    # No problemo; a symbol can't conflict with itself.
                    if nqp::eqaddr(
                         nqp::decont($existing),
                         nqp::decont($value)
                       ) {
                    }

                    # Since the source is a stub, it doesn't matter whether
                    # the target is also a stub or not.  In either case,
                    # it is fine to merge source symbols into target.
                    elsif is_HOW_stub($value) {
                        self.merge_globals($existing.WHO, $value.WHO);
                    }

                    # The tricky case: here the interesting package is the
                    # one in the module. So we merge the other way around
                    # and install that as the result.
                    elsif is_HOW_stub($existing) {
                        self.merge_globals($value.WHO, $existing.WHO);
                        $target.symbol($symbol, :scope<lexical>, :$value);
                    }

                    # ok to overwrite non-stub symbols of outer lexical scopes
                    # or "latest wins" semantics for functions
                    elsif $outer || nqp::eqat($symbol, '&', 0) {
                        $target.symbol($symbol, :scope<lexical>, :$value);
                    }

                    # Alas
                    else {
                        nqp::die("Merging GLOBAL symbols failed: duplicate definition of symbol $symbol");
                    }
                }

                # Not known yet, add it
                else {
                    $target.symbol($symbol, :scope<lexical>, :$value);
                    nqp::push(
                      nqp::atpos($target, 0),
                      QAST::Var.new(:name($symbol),
                        :scope<lexical>, :decl<static>, :$value
                     )
                    );
                    $world.add_object_if_no_sc($value);
                }

                ++$i;
            }
        }
    }

    # Transforms NULL.<release> into CORE.<previous-release>,
    # CORE.<release> into CORE.<previous-release>
    method previous_setting_name ($setting_name, :$base = 'CORE') {
        nqp::ifnull(
          nqp::atkey(
            nqp::atkey(
              nqp::gethllsym('default', 'SysConfig').rakudo-build-config,
              'prev-setting-name'
            ),
            $setting_name
          ),
          nqp::die("Don't know setting $setting_name")
        )
    }

    method transform_setting_name ($setting_name) {
        self.previous_setting_name($setting_name, base => 'NULL');
    }

    my $setting-lock := NQPLock.new;
    method load_setting(str $setting_name) {
        my $setting;

        unless $setting_name eq 'NULL.c' {
            CATCH {
                nqp::unlock($setting-lock);
                nqp::can($_, 'rethrow') ?? $_.rethrow !! nqp::rethrow($_);
            }
            nqp::lock($setting-lock);

            DEBUG("Requested for settings $setting_name") if $DEBUG;
            $setting_name := self.transform_setting_name($setting_name);

            # First, pre-load previous setting.
            my str $prev_setting_name :=
              self.previous_setting_name($setting_name);
            my $prev_setting;

            # Don't do this for .c for which $setting_name doesn't change
            $prev_setting := self.load_setting($prev_setting_name)
              unless $prev_setting_name eq $setting_name;

            # This setting not loaded yet
            $setting := nqp::atkey(%settings_loaded, $setting_name);
            if nqp::isnull($setting) {
                DEBUG("Loading settings $setting_name") if $DEBUG;
                # Find it.
                my str $path := self.find_setting($setting_name);

                # Load it.
                my $*CTXSAVE := self;
                my $*MAIN_CTX;
                my $preserve_global := nqp::gethllsym('Raku','GLOBAL');

                DEBUG("Loading bytecode from $path") if $DEBUG;
                nqp::scwbdisable;
                nqp::loadbytecode($path);
                nqp::scwbenable;

                nqp::bindhllsym('Raku', 'GLOBAL', $preserve_global);
                $setting := $*MAIN_CTX;

                nqp::die(
                  "Unable to load setting $setting_name; maybe it is missing a YOU_ARE_HERE?"
                ) unless nqp::defined($setting);

                nqp::forceouterctx(nqp::ctxcode($setting), $prev_setting)
                  if nqp::defined($prev_setting);
                nqp::bindkey(%settings_loaded, $setting_name, $setting);

                DEBUG("Settings $setting_name loaded") if $DEBUG;
            }

            # Already loaded
            elsif $DEBUG {
                DEBUG("Settings $setting_name already loaded");
            }

            nqp::unlock($setting-lock);
        }

        $setting
    }

    # Handles any object repossession conflicts that occurred during module
    # load, or complains about any that cannot be resolved.
    method resolve_repossession_conflicts(@conflicts) {
        my int $m := nqp::elems(@conflicts);
        my int $i;
        while $i < $m {
            my $original := nqp::atpos(@conflicts, $i);

            # If it's a Stash in conflict, we make sure any original entries
            # get appropriately copied.
            if $original.HOW.name($original) eq 'Stash' {  # XXX typecheck??
                my %current := nqp::atpos(@conflicts, $i + 1);

                for $original.FLATTENABLE_HASH {
                    my str $key := $_.key;

                    nqp::bindkey(%current, $key, $_.value)
                      if nqp::eqat($key, '&', 0);
                      || nqp::not_i(nqp::existskey(%current, $key))
                }
            }
            # We could complain about anything else, and may in the future; for
            # now, we let it pass by with "latest wins" semantics.

            $i := $i + 2;
        }
    }

    sub stash_hash($pkg) {
        nqp::ishash($pkg) ?? $pkg !! $pkg.FLATTENABLE_HASH
    }
}

# We stash this in the raku HLL namespace, just so it's easy to
# locate. Note this makes it invisible inside Raku itself.
nqp::bindhllsym('Raku', 'ModuleLoader', Perl6::ModuleLoader);

# vim: expandtab sw=4
