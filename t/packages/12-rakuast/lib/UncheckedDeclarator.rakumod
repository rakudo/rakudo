# A module whose EXPORTHOW supersedes the grammar declarator in the
# unchecked form with a meta-object it keeps to itself.
my class UncheckedGrammarHOW is Metamodel::GrammarHOW { }
my module EXPORTHOW {
    constant grammar = UncheckedGrammarHOW;
}
