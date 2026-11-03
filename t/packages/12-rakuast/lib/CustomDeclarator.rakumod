# A module whose EXPORTHOW adds a class declarator, for the deparse and
# the .raku of a package declared with it.
class CustomDeclaratorHOW is Metamodel::ClassHOW is export { }
my package EXPORTHOW {
    package DECLARE {
        constant custom-class = CustomDeclaratorHOW;
    }
}
