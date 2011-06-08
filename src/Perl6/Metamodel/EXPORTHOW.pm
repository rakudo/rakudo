# Bind the HOWs into the EXPORTHOW package under the package declarator
# names.
my module EXPORTHOW {
    ($?PACKAGE.WHO)<package>      := Perl6::Metamodel::PackageHOW;
    ($?PACKAGE.WHO)<module>       := Perl6::Metamodel::ModuleHOW;
    ($?PACKAGE.WHO)<generic>      := Perl6::Metamodel::GenericHOW;
    ($?PACKAGE.WHO)<class>        := Perl6::Metamodel::ClassHOW;
    ($?PACKAGE.WHO)<class-attr>   := Attribute;
    ($?PACKAGE.WHO)<role>         := Perl6::Metamodel::ParametricRoleHOW;
    ($?PACKAGE.WHO)<role-attr>    := Attribute;
    ($?PACKAGE.WHO)<native>       := Perl6::Metamodel::NativeHOW;
}
