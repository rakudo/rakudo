# Bind the HOWs into the EXPORTHOW package under the package declarator
# names.

{
    # Don't expose this name in CORE's namespace
    class Perl6::Metamodel::v6e::GrammarHOW
        is Perl6::Metamodel::ClassHOW
        does Perl6::Metamodel::DefaultParent
    {
    }

    # Set 6.e Grammar as the default for grammars
    Perl6::Metamodel::v6e::GrammarHOW.set_default_parent_type(Grammar);
    EXPORTHOW.WHO<grammar> := Perl6::Metamodel::v6e::GrammarHOW;
}

# vim: expandtab shiftwidth=4
