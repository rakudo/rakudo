# Bind the HOWs into the EXPORTHOW package under the package declarator
# names.
my module EXPORTHOW {
    my %who := $?PACKAGE.WHO;
    %who<package>      := Perl6::Metamodel::PackageHOW;
    %who<module>       := Perl6::Metamodel::ModuleHOW;
    %who<generic>      := Perl6::Metamodel::GenericHOW;
    %who<class>        := Perl6::Metamodel::ClassHOW;
    %who<class-attr>   := Attribute;
    %who<role>         := Perl6::Metamodel::ParametricRoleHOW;
    %who<role-attr>    := Attribute;
    %who<role-group>   := Perl6::Metamodel::ParametricRoleGroupHOW;
    %who<grammar>      := Perl6::Metamodel::GrammarHOW;
    %who<grammar-attr> := Attribute;
    %who<native>       := Perl6::Metamodel::NativeHOW;
    %who<subset>       := Perl6::Metamodel::SubsetHOW;
    %who<enum>         := Perl6::Metamodel::EnumHOW;
    %who<coercion>     := Perl6::Metamodel::CoercionHOW;
    %who<definite>     := Perl6::Metamodel::DefiniteHOW;
};

# vim: expandtab sw=4
