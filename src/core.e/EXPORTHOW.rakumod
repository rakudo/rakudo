# Bind the HOWs into the EXPORTHOW package under the package declarator
# names.

{
    # Don't expose this name in CORE's namespace
    class Metamodel::v6e::GrammarHOW
        is Metamodel::ClassHOW
        does Metamodel::DefaultParent
    {
    }

    # Set 6.e Grammar as the default for grammars
    Metamodel::v6e::GrammarHOW.set_default_parent_type(Grammar);
    EXPORTHOW.WHO<grammar> := Metamodel::v6e::GrammarHOW;
}

# vim: expandtab shiftwidth=4
