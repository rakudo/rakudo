# A module whose EXPORTHOW supersedes the subset declarator with a
# meta-object it keeps to itself.
my class SupersededSubsetHOW is Metamodel::SubsetHOW { }
my package EXPORTHOW {
    package SUPERSEDE {
        constant subset = SupersededSubsetHOW;
    }
}
