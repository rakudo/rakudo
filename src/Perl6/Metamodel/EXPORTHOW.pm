# Bind the HOWs into the EXPORTHOW package under the package declarator
# names.
my module EXPORTHOW {
    ($?PACKAGE.WHO)<package>      := Perl6::Metamodel::PackageHOW;
    ($?PACKAGE.WHO)<module>       := Perl6::Metamodel::ModuleHOW;
    ($?PACKAGE.WHO)<class>        := Perl6::Metamodel::ClassHOW;
}
