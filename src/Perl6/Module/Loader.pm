# This class contains various bits of logic for implemneting need
# and import.
class Perl6::Module::Loader;

method need($name, %name_adverbs) {
    # Use locator to find the module.
    my @inc     := pir::get_hll_global__PS('@INC');
    my $pm_file := %name_adverbs<ver> || %name_adverbs<auth> ??
        Perl6::Module::Locator.find_module($name, @inc, %name_adverbs<ver>, %name_adverbs<auth>) !!
        Perl6::Module::Locator.find_module_no_conditions($name, @inc);
    if $pm_file eq '' {
        # XXX Awesomeize - include version and auth if specified.
        pir::die("Unable to find module '$name'.");
    }

    # XXX For now, we just use the pre-compiled PIR file. (Yes, epic hack.)
    my $pir_file := pir::substr__SSII($pm_file, 0, pir::length__IS($pm_file) - 2) ~ 'pir';
    unless pir::stat__ISI($pir_file, 0) {
        pir::die("Sorry, for now you must manually compile .pm modules to .pir (missing for $name).");
    }
    pir::load_bytecode__vS($pir_file);
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
}

method import($name) {
    # XXX For now, target is always namespace of the caller. In the
    # future we need to be much more smart and handle lexical imports.
    my $targetns := Q:PIR {
        %r = getinterp
        %r = %r['namespace';1]
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
