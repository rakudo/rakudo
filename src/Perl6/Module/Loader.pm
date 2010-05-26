# This class contains various bits of logic for implemneting need
# and import.
class Perl6::Module::Loader;

our %LOADED;

method need($name, %name_adverbs?) {
    # From another HLL?
    if %name_adverbs<from> {
        return self.need_foreign($name, %name_adverbs);
    }

    # Otherwise, use the Perl 6 module locator.
    my @inc     := pir::get_hll_global__PS('@INC');
    my $pm_file := %name_adverbs<ver> || %name_adverbs<auth> ??
        Perl6::Module::Locator.find_module($name, @inc, %name_adverbs<ver>, %name_adverbs<auth>) !!
        Perl6::Module::Locator.find_module_no_conditions($name, @inc);
    if $pm_file eq '' {
        pir::die("Unable to find module '$name'" ~
            (%name_adverbs<ver> ?? " with version '" ~ %name_adverbs<ver> ~ "'" !! "") ~
            (%name_adverbs<ver> && %name_adverbs<auth> ?? ' and' !! '') ~
            (%name_adverbs<auth> ?? " with authority '" ~ %name_adverbs<auth> ~ "'" !! "") ~
            " in the @*INC directories.\n"
            ~ "(@*INC contains:\n  "
            ~ pir::join("\n  ", @*INC)
            ~ ")"
        );
    }

    # Need not load file if we already did so.
    unless %LOADED{$pm_file} {
        # Is there a pre-compiled PIR version?
        my $pir_file := pir::substr__SSII($pm_file, 0, pir::length__IS($pm_file) - 2) ~ 'pir';
        my $loaded_pir := 0;
        if pir::stat__ISI($pir_file, 0) {
            # XXX We really should check if it's newer than the .pm file
            # to avoid loading out of date versions.
            pir::load_bytecode__vS($pir_file);
            $loaded_pir := 1;
        }

        # If we couldn't load a cached PIR version, read in and compile.
        # XXX We can try to write the compiled PIR to disk so the next
        # time around it's fast.
        unless $loaded_pir {
            my $fh     := pir::open__PSS($pm_file, 'r');
            my $source := $fh.readall();
            $fh.close();
            my $eval := Perl6::Compiler.compile($source);
            $eval();
        }

        # Mark loaded.
        %LOADED{$pm_file} := 1;
    }

    1;
}

method need_foreign($name, %name_adverbs) {
    # If it's a foreign module, we delegate most of the work to the
    # other language.  However, we still need to install a namespace
    # alias in order for import and qualified references to work.

    my $lang := %name_adverbs<from>;
    my $orig_name := $name; # TODO: Implement :from<lang not.coloned.Name>

    pir::load_language__vs($lang);

    my $lsm := pir::compreg__ps($lang);
    my $mod := $lsm.load_module($orig_name);
    my $ns  := $lsm.get_namespace($mod);

    # Perl6's two phase import mechanism complicates things slightly
    # We need to remember the $lsm to we can get at the exports *later*
    my $exports_closure := pir::newclosure__PP(sub() {
        # TODO: Import flags
        my %raw_exports := $lsm.get_exports($mod);
        my %exports;

        my %subs := %raw_exports<sub> // pir::root_new__PP(<parrot Hash>);
        for %subs {
            %exports{'&' ~ $_.key} := %subs{$_.key};
        }
        # TODO: Non-sub exports

        %exports;
    });
    pir::setprop($ns, '!rakudo-export-closure', $exports_closure);

    # XXX This alias wants to be lexical, but for now we put it into the
    # namespace.
    my @nsparts := pir::split__PSS('::', $name);
    my $lastpart := @nsparts.pop;
    pir::set_hll_global__vPSP(@nsparts, '&' ~ $lastpart, sub() { $ns });
    pir::set_hll_global__vPSP(@nsparts,       $lastpart, $ns);

    return 1;
}

method get_imports($name) {
    # Look up default export namespace.
    my @nsparts := pir::split__PSS('::', $name);

    # If it's an alias to a foreign namespace, we attached an export closure
    # to it at need time.
    my $ns := pir::get_hll_namespace__PP(@nsparts);
    unless pir::isnull($ns) {
        my $closure := pir::getprop('!rakudo-export-closure', $ns);
        unless pir::isnull($closure) {
            return $closure();
        }
    }

    # Otherwise, just go looking in EXPORT::DEFAULT namespace.
    # XXX Here is where we need to support custom tags.
    @nsparts.push('EXPORT');
    @nsparts.push('DEFAULT');
    return pir::get_hll_namespace__PP(@nsparts);
}

method stub_lexical_imports($name, $block_ast) {
    my %imports := self.get_imports($name);
    unless pir::isnull__IP(%imports) {
        for %imports {
            $block_ast[0].push(PAST::Var.new(
                :name($_.key), :scope('lexical'), :isdecl(1),
                :viviself(PAST::Op.new( :pirop('null P')) )
            ));
            $block_ast.symbol($_.key, :scope('lexical'));
        }
    }
}

method import($name) {
    # XXX For now, target is always lexpad of the caller. But we may
    # have a variety of import descriptors...needs more work later.
    my $targetns := Q:PIR {
        %r = getinterp
        %r = %r['lexpad';1]
    };

    # Get imports.
    my %imports := self.get_imports($name);
    unless pir::isnull__IP(%imports) {
        for %imports {
            $targetns{$_.key} := $_.value;
        }
    }

    1;
}
