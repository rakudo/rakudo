class Perl6::Compiler::Module is Perl6::Compiler::Package;

# Modules don't support methods.
method methods() {
    pir::die('You can not add a method to a module; use a class or role');
}

# Accessor for attributes hash.
method attributes() {
    pir::die('You can not add an attribute to a module; use a class or role');
}

# This method drives the code generation and fixes up the block.
method finish($block) {
    if $!scope eq 'our' {
        $block.blocktype('immediate');
        $block.namespace(Perl6::Grammar::parse_name(~$!name));
    }
    else {
        pir::die("Can't handle scope declarator " ~ $!scope ~ " on modules yet");
    }
    return $block;
}
