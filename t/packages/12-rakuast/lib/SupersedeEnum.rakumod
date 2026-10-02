# A module whose EXPORTHOW supersedes the enum declarator with a
# meta-object it keeps to itself.
my class SupersededEnumHOW is Metamodel::EnumHOW { }
my package EXPORTHOW {
    package SUPERSEDE {
        constant enum = SupersededEnumHOW;
    }
}
