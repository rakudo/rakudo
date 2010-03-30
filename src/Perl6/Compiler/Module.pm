class Perl6::Compiler::Module is Perl6::Compiler::Package;

has %!dummy;

# Modules don't support methods; just give back some dummy table.
method methods() {
    if $*SCOPE eq '' || $*SCOPE eq 'has' {
        pir::printerr("Useless declaration of has-scoped " ~ $*METHODTYPE ~
            " in a module; add our or my to install it in the lexpad or namespace\n");
    }
    %!dummy
}

# Accessor for attributes hash.
method attributes() {
    pir::die('You can not add an attribute to a module; use a class or role');
}

# This method drives the code generation and fixes up the block.
method finish($block) {
    if $!scope eq 'our' || $!scope eq '' {
        $block.blocktype('immediate');
        $block.namespace(Perl6::Grammar::parse_name(~$!name));
    }
    else {
        pir::die("Can't handle scope declarator " ~ $!scope ~ " on modules yet");
    }
    return $block;
}
