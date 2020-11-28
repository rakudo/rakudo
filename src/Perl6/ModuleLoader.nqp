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
        self.vm_search_paths()
    }

    method load_module($module_name, %opts, *@GLOBALish, :$line, :$file, :%chosen) {
        DEBUG("going to load $module_name") if $DEBUG;
        if nqp::eqat($module_name, 'Perl6::BOOTSTRAP::v6', 0) {
            my $preserve_global := nqp::ifnull(nqp::gethllsym('Raku', 'GLOBAL'), NQPMu);
            my %*COMPILING := {};
            my $*CTXSAVE := self;
            my $*MAIN_CTX;
            my $file := nqp::join('/', nqp::split('::', $module_name)) ~ self.file-extension;

            my @prefixes := self.search_path();
            for @prefixes -> $prefix {
                if nqp::stat("$prefix/$file", 0) {
                    $file := "$prefix/$file";
                    last;
                }
            }

            if nqp::existskey(%modules_loaded, $file) {
                return nqp::ctxlexpad(%modules_loaded{$file});
            }

            nqp::loadbytecode($file);
            %modules_loaded{$file} := my $module_ctx := $*MAIN_CTX;
            nqp::bindhllsym('Raku', 'GLOBAL', $preserve_global);
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
            if %opts<from> eq 'NQP' {
                if +@GLOBALish {
                    my $target := nqp::knowhow().new_type(:name('GLOBALish'));
                    nqp::setwho($target, @GLOBALish[0].WHO.FLATTENABLE_HASH());
                    return %language_module_loaders<NQP>.load_module($module_name, $target);
                }
                else {
                    return %language_module_loaders<NQP>.load_module($module_name);
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
    sub is_stub($how) {
        $how.HOW.name($how) eq $stub_how || $how.HOW.name($how) eq $nqp_stub_how
    }
    sub is_from_dependency($sc-desc) {
        if $*W && $sc-desc eq nqp::scgetdesc($*W.sc) {
            note("repossessing into same sc!") if $DEBUG;
            return 1;
        }
        if %*COMPILING && %*COMPILING<dependencies> {
            my @dependencies := %*COMPILING<dependencies>.FLATTENABLE_LIST;
            for @dependencies {
                note("repossessing a dependency! $sc-desc") if $DEBUG && $_.source-name eq $sc-desc;
                return 1 if $_.source-name eq $sc-desc;
            }
        }
        return 0;
    }
    method allowed_sc_dep($value) {
        my $allowed_sc_deps := $*ALLOWED_SC_DEPS;
        if nqp::defined($allowed_sc_deps) && $*W.is_precompilation_mode {
            my $allowed := 0;
            my $sc := nqp::getobjsc($value);
            if nqp::defined($sc) {
                for $allowed_sc_deps {
                    if $sc =:= $_ {
                        return 1;
                    }
                }
                note("    allowed: $allowed " ~ nqp::scgetdesc($sc)) if $DEBUG;
                return 0 unless $allowed; # transitive dependencies are an issue that is_from_dependency just handles better
            }
        }
        return 1;
    }

    method create_stub_package($globalish, $package_how, $name, $longname) {
        note("Creating stub package $longname") if $DEBUG;
        my $pkg := $package_how.new_type(:name($longname));
        $pkg.HOW.compose($pkg);
        nqp::bindkey(nqp::ishash($globalish) ?? $globalish !! nqp::who($globalish), $name, $pkg);
        $pkg
    }

    method register_package_symbols($package_how, @symbols, $globalish) {
        if nqp::defined(@symbols) {
            $globalish := $globalish.FLATTENABLE_HASH if nqp::can($globalish, 'FLATTENABLE_HASH');
            @symbols := @symbols.FLATTENABLE_LIST unless nqp::islist(@symbols);
            for @symbols {
                self.install_package_symbol($_[0], $_[1], $_[2], $_[3], $package_how, $globalish);
            }
        }
    }
    method debug_helper($who) {
        nqp::sin_n(1.0);
    }

    method reinstall_package_symbol($root, $package_name, $name, $obj, $package_how) {
        note("Perl6::ModuleLoader.reinstall_package_symbol(" ~ $root.HOW.name($root) ~ ", $package_name, $name, " ~ $obj.HOW.name($obj) ~ " (" ~ $obj.HOW.HOW.name($obj.HOW) ~ ")) ") if $DEBUG;
        my @name_parts := nqp::split('::', $package_name);
        my $longname := nqp::shift(@name_parts); # already got root
        my $cur_package := $root;
        for @name_parts {
            $longname := $longname ~ '::' ~ $_;
            my $who := nqp::who($cur_package);
            my $repossess := nqp::isnull(nqp::getobjsc($who)) || self.allowed_sc_dep($who);
            #my $repossess := nqp::isnull(nqp::getobjsc($who));
            nqp::scwbdisable() unless $repossess; # avoid repossession of existing stashes
            $cur_package := nqp::ishash($who)
                ?? nqp::ifnull(nqp::atkey($who, $_), self.create_stub_package($cur_package, $package_how, $_, $longname))
                !! $who.package_at_key($_);
            nqp::scwbenable() unless $repossess;
        }

        my $who := nqp::who($cur_package);
        #note($cur_package.HOW.name($cur_package));
        #nqp::hllizefor($who, 'Raku').keys.note;
        #nqp::hllizefor(stash_hash($who), 'Raku').keys.note;
        if nqp::existskey(stash_hash($who), $name) {
            my $existing := stash_hash($who){$name};
            nqp::hllizefor($existing, 'Raku').note;
            if nqp::decont($existing) =:= $obj {
                note("     $name " ~ $existing.HOW.name($existing) ~ " already there") if $DEBUG;
                return;
            }
            note("    already exists! " ~ $existing.HOW.name($existing) ~ ' ' ~ $existing.HOW.HOW.name($existing.HOW)) if $DEBUG;
            if is_stub($obj.HOW) {
                note("    not installing stub over existing symbol") if $DEBUG;
                return;
            }
            if is_stub($existing.HOW) {
                note("    salvaging items from existing symbol") if $DEBUG;
                my $existing_who := nqp::who($existing);
                for $existing_who.keys.FLATTENABLE_LIST {
                    $who.BIND-KEY($_, $existing_who.AT-KEY($_)) unless $who.EXISTS-KEY($_);
                }
            }
        }

        #my $repossess := nqp::isnull(nqp::getobjsc($who)) || is_from_dependency(nqp::scgetdesc(nqp::getobjsc($who)));
        my $repossess := nqp::isnull(nqp::getobjsc($who)) || self.allowed_sc_dep($who);
        #my $repossess := nqp::isnull(nqp::getobjsc($who));
        nqp::scwbdisable unless $repossess; # avoid repossession of existing stashes

        if nqp::ishash($who) {
            note("   installing $name (" ~ $obj.HOW.HOW.name($obj.HOW) ~ ") into BOOTHash " ~ $cur_package.HOW.HOW.name($cur_package.HOW) ~ ' ' ~ (nqp::getobjsc($cur_package) ?? nqp::scgetdesc(nqp::getobjsc($cur_package)) !! '')) if $DEBUG;
            $who{$name} := $obj;
        }
        else {
            note("   installing $name (" ~ $obj.HOW.HOW.name($obj.HOW) ~ ") into " ~ $cur_package.HOW.name($cur_package) ~ ' who ' ~ $who.Str ~ ' ' ~ $cur_package.HOW.HOW.name($cur_package.HOW) ~ ' ' ~ (nqp::getobjsc($cur_package) ?? nqp::scgetdesc(nqp::getobjsc($cur_package)) !! '')) if $DEBUG;
            $who.BIND-KEY($name, $obj);
        }

        nqp::scwbenable unless $repossess;
    }

    method install_package_symbol($package_name, $name, $obj, $globalish, $package_how, $target) {
        note("Perl6::ModuleLoader.install_package_symbol($package_name, $name, " ~ $obj.HOW.name($obj) ~ " (" ~ $obj.HOW.HOW.name($obj.HOW) ~ ")) " ~ (nqp::isnull(nqp::getlexdyn('$*TARGET-GLOBAL')) ?? ' globally ' !! 'directly')) if $DEBUG;

        note('    ' ~ $target.HOW.name($target)) if $DEBUG;

        if $package_name eq 'GLOBAL' {
            if nqp::ishash($target) {
                if nqp::existskey($target, $name) {
                    my $existing := $target{$name};
                    if nqp::decont($existing) =:= $obj {
                        note("     already there") if $DEBUG;
                        return;
                    }
                }
                else {
                    $target{$name} := $obj;
                    note("    added pseudo lexical to hash") if $DEBUG;
                }
            }
            else {
                if nqp::existskey($target.symtable, $name) {
                    my $existing := $target.symtable{$name}<value>;
                    if nqp::decont($existing) =:= $obj {
                        note("     already there") if $DEBUG;
                        return;
                    }
                    if is_stub($obj.HOW) {
                        note("    not installing stub over existing symbol") if $DEBUG;
                        return;
                    }
                    if is_stub($existing.HOW) {
                        note("    salvaging items from existing symbol " ~  $existing.HOW.name($existing) ~ " " ~ $existing.HOW.HOW.name($existing.HOW)) if $DEBUG;
                        my $existing_who := nqp::who($existing);
                        my $who := nqp::who($obj);
                        for $existing_who.keys.FLATTENABLE_LIST {
                            $who.BIND-KEY($_, $existing_who.AT-KEY($_)) unless $who.EXISTS-KEY($_);
                        }
                        $target.symbol($name, :scope('lexical'), :value($obj));
                        for $target[0].list {
                            if nqp::istype($_, QAST::Var) && $_.name eq $name {
                                note("setting QAST::Var $name") if $DEBUG;
                                $_.value($obj);
                            }
                        }
                    }
                }
                else {
                    my $exists := 0;
                    my $existing;
                    try {
                        $existing := $*W.find_single_symbol($name);
                        unless $existing =:= NQPMu {
                            $exists := 1;
                            note("    outer") if $DEBUG;
                        }
                    }
                    if $exists {
                        note("    already exists in outer scope") if $DEBUG;
                    }
                    else {
                        $target.symbol($name, :scope('lexical'), :value($obj));
                        $target[0].push(QAST::Var.new(
                            :scope('lexical'), :name($name), :decl('static'), :value($obj)
                        ));
                        $*W.add_object_if_no_sc($obj);
                        note("    created lexical $name") if $DEBUG;
                    }
                }
            }
        }
        else {
            my @name_parts := nqp::split('::', $package_name);
            unless nqp::elems(@name_parts) {
                return;
            }
            my $first := nqp::shift(@name_parts);

            my $cur_package := nqp::ishash($target) ?? $target{$first} !! $target.symtable{$first};
            #note($cur_package.HOW.name($cur_package));
            my $outer := 0;
            if nqp::isnull($cur_package) || $cur_package =:= NQPMu {
                try {
                    #note("find_single_symbol");
                    $cur_package := $*W.find_single_symbol($first);
                    #note($cur_package.HOW.name($cur_package));
                    unless $cur_package =:= NQPMu {
                        #nqp::hllizefor($cur_package, "Raku").note;
                        $outer := 1;
                        note("    found outer symbol $first containing " ~ nqp::hllizefor(nqp::who($cur_package), 'Raku').keys.gist) if $DEBUG;
                    }
                }
            }
            else {
                note("found an existing symbol $first at " ~ nqp::objectid($cur_package<value>) ~ " containing " ~ nqp::objectid(nqp::who($cur_package<value>)) ~ ' ' ~ nqp::hllizefor(nqp::who($cur_package<value>), 'Raku').keys.gist) if $DEBUG;
                if is_stub($cur_package<value>.HOW) { # replace existing stub with a new one so we avoid needless dependencies
                    my $pkg := $package_how.new_type(:name($first));
                    $pkg.HOW.compose($pkg);
                    my $who := nqp::who($pkg);
                    my $existing_who := nqp::who($cur_package<value>);
                    for $existing_who.keys.FLATTENABLE_LIST {
                        $who.BIND-KEY($_, $existing_who.AT-KEY($_)) unless $who.EXISTS-KEY($_);
                    }
                    $cur_package<value> := $pkg;
                }
                $cur_package := $cur_package<value>;
            }

            if nqp::isnull($cur_package) || $cur_package =:= NQPMu {
                $cur_package := nqp::ifnull(
                    nqp::atkey(nqp::ishash($target) ?? $target !! nqp::who($target), $first),
                    self.create_stub_package($target, $package_how, $first, $first)
                );
                note("created: " ~ $cur_package.HOW.name($cur_package)) if $DEBUG;

                unless nqp::ishash($target) {
                    $target.symbol($first, :scope('lexical'), :value($cur_package));
                    $target[0].push(QAST::Var.new(
                        :scope('lexical'), :name($first), :decl('static'), :value($cur_package)
                    ));
                    $*W.add_object_if_no_sc($cur_package);
                }
            }

            my $root := $cur_package;
            my $longname := $cur_package.HOW.name($cur_package);

            for @name_parts {
                $longname := $longname ~ '::' ~ $_;
                my $who := nqp::who($cur_package);
                my $repossess := nqp::isnull(nqp::getobjsc($who)) || is_from_dependency(nqp::scgetdesc(nqp::getobjsc($who)));
                #$repossess := 0;
                nqp::scwbdisable() unless $repossess; # avoid repossession of existing stashes
                $cur_package := nqp::ishash($who)
                    ?? nqp::ifnull(nqp::atkey($who, $_), self.create_stub_package($cur_package, $package_how, $_, $longname))
                    !! $who.package_at_key($_);
                nqp::scwbenable() unless $repossess;
            }
            my $who := nqp::who($cur_package);
            my $repossess := nqp::isnull(nqp::getobjsc($who)) || is_from_dependency(nqp::scgetdesc(nqp::getobjsc($who)));
            #$repossess := 0;
            note("who is a " ~ $who.HOW.name($who));
            my $install := 1;
            if nqp::existskey(stash_hash($who), $name) {
                my $existing := stash_hash($who){$name};
                if nqp::decont($existing) =:= $obj || nqp::decont($existing) =:= nqp::decont($obj) {
                    note("     already there") if $DEBUG;
                    $install := 0;
                }
                elsif is_stub($obj.HOW) {
                    note("    not installing stub over existing symbol") if $DEBUG;
                    return;
                }
                elsif is_stub($existing.HOW) {
                    note("    salvaging items from existing symbol") if $DEBUG;
                    my $existing_who := nqp::who($existing);
                    for $existing_who.keys.FLATTENABLE_LIST {
                        $who.BIND-KEY($_, $existing_who.AT-KEY($_)) unless $who.EXISTS-KEY($_);
                    }
                }
                elsif $existing =:= $obj {
                    $install := 0;
                    note("not decontend") if $DEBUG;
                }
                elsif $existing =:= nqp::decont($obj) {
                    $install := 0;
                    note("deconted obj") if $DEBUG;
                }
                elsif nqp::eqat($name, '&', 0) {
                    note("    sub already exists - latest wins: " ~ $existing.HOW.name($existing) ~ ' ' ~ $existing.HOW.HOW.name($existing.HOW)) if $DEBUG;
                }
                else  {
                    note("    $name already exists in $package_name! " ~ $existing.HOW.name($existing) ~ ' ' ~ $existing.HOW.HOW.name($existing.HOW));
                }
            }

            if nqp::ishash($who) {
                note("   installing $name (" ~ $obj.HOW.HOW.name($obj.HOW) ~ ") into BOOTHash " ~ $cur_package.HOW.HOW.name($cur_package.HOW) ~ ' ' ~ (nqp::getobjsc($cur_package) ?? nqp::scgetdesc(nqp::getobjsc($cur_package)) !! '')) if $DEBUG;
                nqp::scwbdisable unless $repossess; # avoid repossession of existing stashes
                $who{$name} := $obj if $install;
                nqp::scwbenable unless $repossess;
                my $world := $*W;
                if $world {
                    $world.add_object_if_no_sc($root);
                    $world.add_object_if_no_sc($obj);
                    $world.add_fixup_task(:deserialize_ast(
                        QAST::Stmts.new(
#                            QAST::Op.new(
#                                :op<callmethod>,
#                                :name<note>,
#                                QAST::Op.new(
#                                    :op<hllize>,
#                                    QAST::SVal.new(:value(
#                                        "$name (" ~ $obj.HOW.HOW.name($obj.HOW) ~ ") -> " ~ $cur_package.HOW.name($cur_package)
#                                    ))
#                                )
#                            ),
                            QAST::Op.new(
                                :op<callmethod>,
                                :name<reinstall_package_symbol>,
                                QAST::Op.new(:op<gethllsym>, QAST::SVal.new(:value<Raku>), QAST::SVal.new(:value<ModuleLoader>)),
                                QAST::WVal.new(:value($root)),
                                QAST::SVal.new(:value($package_name)),
                                QAST::SVal.new(:value($name)),
                                QAST::WVal.new(:value($obj)),
                                QAST::WVal.new(:value($package_how)),
                            ),
                        )
                    ));
                }
            }
            else {
                note("   installing $name (" ~ $obj.HOW.HOW.name($obj.HOW) ~ ") into " ~ $who.Str ~ ' ' ~ $cur_package.HOW.HOW.name($cur_package.HOW) ~ ' ' ~ (nqp::getobjsc($cur_package) ?? nqp::scgetdesc(nqp::getobjsc($cur_package)) !! '')) if $DEBUG;

                nqp::scwbdisable unless $repossess; # avoid repossession of existing stashes
                $who.BIND-KEY($name, $obj) if $install;
                nqp::scwbenable unless $repossess;
                my $world := $*W;
                if $world {
                    if $name eq '&SSLv2_client_method' {
                        self.debug_helper($obj);
                    }
                    note("    who SC: " ~ nqp::scgetdesc(nqp::getobjsc($who))) if $DEBUG && nqp::getobjsc($who);
                    note("    obj SC: " ~ nqp::scgetdesc(nqp::getobjsc($obj))) if $DEBUG && nqp::getobjsc($obj);
                    $world.add_object_if_no_sc($root);
                    $world.add_object_if_no_sc($obj);
                    $world.add_fixup_task(:deserialize_ast(
                        QAST::Stmts.new(
#                            QAST::Op.new(
#                                :op<callmethod>,
#                                :name<note>,
#                                QAST::Op.new(
#                                    :op<hllize>,
#                                    QAST::SVal.new(:value(
#                                        "$name (" ~ $obj.HOW.HOW.name($obj.HOW) ~ ") -> Stash " ~ $cur_package.HOW.name($cur_package)
#                                    ))
#                                )
#                            ),
                            QAST::Op.new(
                                :op<callmethod>,
                                :name<reinstall_package_symbol>,
                                QAST::Op.new(:op<gethllsym>, QAST::SVal.new(:value<Raku>), QAST::SVal.new(:value<ModuleLoader>)),
                                QAST::WVal.new(:value($root)),
                                QAST::SVal.new(:value($package_name)),
                                QAST::SVal.new(:value($name)),
                                QAST::WVal.new(:value($obj)),
                                QAST::WVal.new(:value($package_how)),
                            ),
                        )
                    ));
                }
            }
            nqp::scwbdisable;
            if nqp::defined($*PACKAGE-SYMBOLS) {
                note("    Registering $name in " ~ nqp::objectid($who)) if $DEBUG;
                nqp::push($*PACKAGE-SYMBOLS, $who);
                nqp::push($*PACKAGE-SYMBOLS, $name);
            }
            nqp::scwbenable;
        }
    }

    method merge_globals($target, $source) {
        # Start off merging top-level symbols. Easy when there's no
        # overlap. Otherwise, we need to recurse.
        my %known_symbols;
        for stash_hash($target) {
            %known_symbols{$_.key} := 1;
        }
        my %source := stash_hash($source);
        note("merge_globals "
            ~ (nqp::can($source, 'Str') ?? $source.Str !! '') ~ " " ~ nqp::objectid($source) ~ " " ~ nqp::objectid(%source) ~ " -> "
            ~ (nqp::can($target, 'Str') ?? $target.Str !! '') ~ ' ' ~ nqp::objectid($target)) if $DEBUG;
        #nqp::hllizefor(%known_symbols, "Raku").keys.Str.note if $DEBUG;
        #nqp::hllizefor(%source, "Raku").keys.Str.note if $DEBUG;
        for sorted_keys(%source) -> $sym {
            next if $sym eq 'packages-to-register';
            note("  $sym -> " ~ (nqp::can($target, 'Str') ?? $target.Str !! '') ~ ' ' ~ nqp::objectid($target) ~ ' decont ' ~ nqp::objectid(nqp::decont($target))) if $DEBUG;
            #dump_scs();
            my $value := %source{$sym};

            my $allowed_sc_deps := $*ALLOWED_SC_DEPS;
            if nqp::defined($allowed_sc_deps) && $*W.is_precompilation_mode {
                my $allowed := 0;
                my $sc := nqp::getobjsc($value);
                if nqp::defined($sc) {
                for $allowed_sc_deps {
                    if $sc =:= $_ {
                        $allowed := 1;
                        last;
                    }
                }
                #note("    allowed: $allowed " ~ nqp::scgetdesc($sc));
                last unless $allowed || is_from_dependency(nqp::scgetdesc($sc)); # transitive dependencies are an issue that is_from_dependency just handles better
                }
            }

            if !%known_symbols{$sym} {
                #note("    merged");
                if nqp::defined($*PACKAGE-SYMBOLS) {
                    #note("    Registering $sym in " ~ nqp::objectid($target));
                    nqp::push($*PACKAGE-SYMBOLS, $target);
                    nqp::push($*PACKAGE-SYMBOLS, $sym);
                }
                ($target){$sym} := $value;
            }
            elsif nqp::decont(($target){$sym}) =:= nqp::decont($value) { # Stash entries are containerized
                #note("    already there");
                # No problemo; a symbol can't conflict with itself.
            }
            else {
                my $source_is_stub := is_stub($value.HOW);
                my $target_is_stub := is_stub(($target){$sym}.HOW);
                #note("    source is stub " ~ $source.name) if $source_is_stub;
                #note("    target is stub " ~ $target.name) if $target_is_stub;
                my $existing-sc := nqp::getobjsc(($target){$sym}.WHO);
                my $existing-sc-desc := nqp::isnull($existing-sc) ?? "<null>" !! nqp::scgetdesc($existing-sc);
                my $repossess := is_from_dependency($existing-sc-desc);
                if $source_is_stub && $target_is_stub {
                    # Both stubs. We can safely merge the symbols from
                    # the source into the target that's importing them.
                    my $value-sc := nqp::getobjsc($value.WHO);
                    #note("existing sc: " ~ (nqp::isnull($existing-sc) ?? "<null>" !! nqp::scgetdesc($existing-sc)) ~ ", value sc: " ~ (nqp::isnull($value-sc) ?? "<null>" !! nqp::scgetdesc($value-sc)));
                    #my $repossess := $existing-sc-desc eq '/home/nine/rakudo/lib/NativeCall.rakumod (NativeCall)';
                    #my $repossess := $*W && nqp::scgetdesc($*W.sc) eq '/home/nine/rakudo/lib/NativeCall.rakumod (NativeCall)';
                    nqp::scwbdisable() unless $repossess;
                    #nqp::neverrepossess(($target){$sym}.WHO);
                    #note("repossessing!") if $repossess;
                    self.merge_globals(($target){$sym}.WHO, $value.WHO);
                    nqp::scwbenable() unless $repossess;
                }
                elsif $source_is_stub {
                    # The target has a real package, but the source is a
                    # stub. Also fine to merge source symbols into target.
                    nqp::scwbdisable() unless $repossess;
                    self.merge_globals(($target){$sym}.WHO, $value.WHO);
                    nqp::scwbenable() unless $repossess;
                }
                elsif $target_is_stub {
                    # The tricky case: here the interesting package is the
                    # one in the module. So we merge the other way around
                    # and install that as the result.
                    nqp::scwbdisable() unless $repossess;
                    self.merge_globals($value.WHO, ($target){$sym}.WHO);
                    nqp::scwbenable() unless $repossess;
                    ($target){$sym} := $value;
                }
                elsif nqp::eqat($sym, '&', 0) {
                    # "Latest wins" semantics for functions
                    #note("    is a function");
                    #note("$sym in merge_globals");
                    ($target){$sym} := $value;
                }
                else {
                    nqp::die("P6M Merging GLOBAL symbols failed: duplicate definition of symbol $sym");
                    #note("P6M Merging GLOBAL symbols failed: duplicate definition of symbol $sym");
                    #self.merge_globals(($target){$sym}.WHO, $value.WHO);
                }
            }
        }
    }
    sub dump_scs() {
        my @scs;
        {
            while 1 {
                my $sc := nqp::popcompsc;
                nqp::unshift(@scs, $sc);
                #note("    * " ~ nqp::scgetdesc($sc));
            }
            CATCH { }
        }
        for @scs {
            nqp::pushcompsc($_);
        }
    }
    method merge_globals_lexically($world, $target, $source) {
        # Start off merging top-level symbols. Easy when there's no
        # overlap. Otherwise, we need to recurse.
        my %known_symbols;
        for $target.symtable {
            %known_symbols{$_.key} := $_.value<value>;
        }
        my %source := stash_hash($source);
        note("merge_globals_lexically "
            ~ (nqp::can($source, 'Str') ?? $source.Str !! '') ~ " " ~ nqp::objectid($source) ~ " " ~ nqp::objectid(%source) ~ " -> "
            ~ (nqp::can($target, 'Str') ?? $target.Str !! '') ~ ' ' ~ nqp::objectid($target)) if $DEBUG;
        #nqp::hllizefor(%known_symbols, "Raku").keys.Str.note if $DEBUG;
        #nqp::hllizefor(%source, "Raku").keys.Str.note if $DEBUG;
        for sorted_keys(%source) -> $sym {
            note("  $sym -> " ~ (nqp::ishash($target) ?? nqp::objectid($target) !! $target.name ~ ' ' ~ nqp::objectid($target))) if $DEBUG;
            #dump_scs();
            my $value := %source{$sym};
            my $outer := 0;
            if !nqp::existskey(%known_symbols, $sym) {
                try {
                    %known_symbols{$sym} := $world.find_single_symbol($sym);
                    $outer := 1;
                    note("    outer") if $DEBUG;
                }
            }
            if !nqp::existskey(%known_symbols, $sym) {
                $target.symbol($sym, :scope('lexical'), :value($value));
                $target[0].push(QAST::Var.new(
                    :scope('lexical'), :name($sym), :decl('static'), :value($value)
                ));
                $world.add_object_if_no_sc($value);
                note("    created lexical") if $DEBUG;
            }
            elsif nqp::decont(%known_symbols{$sym}) =:= nqp::decont($value) { # Stash entries are containerized
                note("    already there") if $DEBUG;
                # No problemo; a symbol can't conflict with itself.
            }
            else {
                my $existing := %known_symbols{$sym};
                my $source_is_stub := is_stub($value.HOW);
                my $target_is_stub := is_stub($existing.HOW);
                my $existing-sc := nqp::getobjsc($existing.WHO);
                my $existing-sc-desc := nqp::isnull($existing-sc) ?? "<null>" !! nqp::scgetdesc($existing-sc);
                my $repossess := is_from_dependency($existing-sc-desc);
                note("    source is stub " ~ $source.name) if $DEBUG && $source_is_stub;
                note("    target is stub " ~ $target.name) if $DEBUG && $target_is_stub;
                if $source_is_stub && $target_is_stub {
                    # Both stubs. We can safely merge the symbols from
                    # the source into the target that's importing them.
                    my $value-sc := nqp::getobjsc($value.WHO);
                    #note("existing sc: " ~ (nqp::isnull($existing-sc) ?? "<null>" !! nqp::scgetdesc($existing-sc)) ~ ", value sc: " ~ (nqp::isnull($value-sc) ?? "<null>" !! nqp::scgetdesc($value-sc)));
                    #my $repossess := $*W && nqp::scgetdesc($*W.sc) eq '/home/nine/rakudo/lib/NativeCall.rakumod (NativeCall)';
                    note("repossessing!") if $DEBUG && $repossess;
                    nqp::scwbdisable() unless $repossess;
                    #nqp::neverrepossess($existing.WHO);
                    self.merge_globals($existing.WHO, $value.WHO);
                    nqp::scwbenable() unless $repossess;
                }
                elsif $source_is_stub {
                    # The target has a real package, but the source is a
                    # stub. Also fine to merge source symbols into target.
                    nqp::scwbdisable() unless $repossess;
                    note("    merge source stub into target") if $DEBUG;
                    self.merge_globals($existing.WHO, $value.WHO);
                    nqp::scwbenable() unless $repossess;
                }
                elsif $target_is_stub {
                    # The tricky case: here the interesting package is the
                    # one in the module. So we merge the other way around
                    # and install that as the result.
                    nqp::scwbdisable() unless $repossess;
                    note("    merge target into source!") if $DEBUG;
                    self.merge_globals($value.WHO, $existing.WHO);
                    nqp::scwbenable() unless $repossess;
                    $target.symbol($sym, :scope('lexical'), :value($value));
                }
                elsif nqp::eqat($sym, '&', 0) {
                    #note("$sym in merge_globals_lexically");
                    # "Latest wins" semantics for functions
                    $target.symbol($sym, :scope('lexical'), :value($value));
                }
                elsif $outer {
                    # It's ok to overwrite non-stub symbols of outer lexical scopes
                    $target.symbol($sym, :scope('lexical'), :value($value));
                }
                else {
                    nqp::die("P6M Merging GLOBAL symbols failed: duplicate definition of symbol $sym");
                    #note("P6M Merging GLOBAL symbols failed: duplicate definition of symbol $sym");
                }
            }
        }
    }

    # Transforms NULL.<release> into CORE.<previous-release>, CORE.<release> into CORE.<previous-release>
    method previous_setting_name ($setting_name, :$base = 'CORE') {
        nqp::gethllsym('default', 'SysConfig').rakudo-build-config()<prev-setting-name>{$setting_name}
            // nqp::die("Don't know setting $setting_name")
    }

    method transform_setting_name ($setting_name) {
        return self.previous_setting_name($setting_name, base => 'NULL');
    }

    method load_setting($setting_name) {
        my $setting;

        if $setting_name ne 'NULL.c' {
            DEBUG("Requested for settings $setting_name") if $DEBUG;
            $setting_name := self.transform_setting_name($setting_name);

            # First, pre-load previous setting.
            my $prev_setting_name := self.previous_setting_name($setting_name);
            my $prev_setting;
            # Don't do this for .c for which $setting_name doesn't change
            unless nqp::iseq_s($prev_setting_name, $setting_name) {
                $prev_setting := self.load_setting($prev_setting_name);
            }

            # Unless we already did so, locate and load the setting.
            unless nqp::defined(%settings_loaded{$setting_name}) {
                DEBUG("Loading settings $setting_name") if $DEBUG;
                # Find it.
                my $path := self.find_setting($setting_name);

                # Load it.
                my $*CTXSAVE := self;
                my $*MAIN_CTX;
                my $preserve_global := nqp::ifnull(nqp::gethllsym('Raku', 'GLOBAL'), NQPMu);
                nqp::scwbdisable();
                DEBUG("Loading bytecode from $path") if $DEBUG;
                nqp::loadbytecode($path);
                nqp::scwbenable();
                nqp::bindhllsym('Raku', 'GLOBAL', $preserve_global);
                unless nqp::defined($*MAIN_CTX) {
                    nqp::die("Unable to load setting $setting_name; maybe it is missing a YOU_ARE_HERE?");
                }
                #note("MAIN_CTX: " ~ nqp::scgetdesc(nqp::getobjsc(nqp::ctxcode($*MAIN_CTX))));
                nqp::forceouterctx(nqp::ctxcode($*MAIN_CTX), $prev_setting) if nqp::defined($prev_setting);
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

# We stash this in the raku HLL namespace, just so it's easy to
# locate. Note this makes it invisible inside Raku itself.
nqp::bindhllsym('Raku', 'ModuleLoader', Perl6::ModuleLoader);

# vim: expandtab sw=4
