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
    ($?PACKAGE.WHO)<role-group>   := Perl6::Metamodel::ParametricRoleGroupHOW;
    ($?PACKAGE.WHO)<grammar>      := Perl6::Metamodel::GrammarHOW;
    ($?PACKAGE.WHO)<grammar-attr> := Attribute;
    ($?PACKAGE.WHO)<native>       := Perl6::Metamodel::NativeHOW;
    ($?PACKAGE.WHO)<subset>       := Perl6::Metamodel::SubsetHOW;
    ($?PACKAGE.WHO)<enum>         := Perl6::Metamodel::EnumHOW;
    ($?PACKAGE.WHO)<coercion>     := Perl6::Metamodel::CoercionHOW;
    ($?PACKAGE.WHO)<definite>     := Perl6::Metamodel::DefiniteHOW;
}

# vim: expandtab sw=4
