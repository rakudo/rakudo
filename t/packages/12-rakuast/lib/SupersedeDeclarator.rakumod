# A module whose EXPORTHOW supersedes the class declarator and its
# attribute type with types it keeps to itself.
my class SupersededClassHOW is Metamodel::ClassHOW { }
my class SupersededAttribute is Attribute { }
my package EXPORTHOW {
    package SUPERSEDE {
        constant class = SupersededClassHOW;
        constant class-attr = SupersededAttribute;
    }
}
