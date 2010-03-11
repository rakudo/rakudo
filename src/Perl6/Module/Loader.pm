# This class contains various bits of logic for implemneting need
# and import.
class Perl6::Module::Loader;

our %LOADED;

method need($name, %name_adverbs?) {
    # Use locator to find the module.
    my @inc     := pir::get_hll_global__PS('@INC');
    my $pm_file := %name_adverbs<ver> || %name_adverbs<auth> ??
        Perl6::Module::Locator.find_module($name, @inc, %name_adverbs<ver>, %name_adverbs<auth>) !!
        Perl6::Module::Locator.find_module_no_conditions($name, @inc);
    if $pm_file eq '' {
        pir::die("Unable to find module '$name'" ~
            (%name_adverbs<ver> ?? " with version '" ~ %name_adverbs<ver> ~ "'" !! "") ~
            (%name_adverbs<ver> && %name_adverbs<auth> ?? ' and' !! '') ~
            (%name_adverbs<auth> ?? " with authority '" ~ %name_adverbs<auth> ~ "'" !! "") ~
            ".");
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

method get_imports($name) {
    # Look up default export namespace.
    # XXX Here is where we need to support custom tags.
    my @nsparts := pir::split__PSS('::', $name);
    @nsparts.push('EXPORT');
    @nsparts.push('DEFAULT');
    return pir::get_hll_namespace__PP(@nsparts);
}

method stub_lexical_imports($name, $block_ast) {
    my %imports := self.get_imports($name);
    unless pir::isnull__IP(%imports) {
        for %imports {
            $block_ast[0].push(PAST::Var.new(
                :name(~$_), :scope('lexical'), :isdecl(1),
                :viviself(PAST::Op.new( :pirop('null P')) )
            ));
            $block_ast.symbol(~$_, :scope('lexical'));
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
