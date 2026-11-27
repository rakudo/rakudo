# Bind the HOWs into the EXPORTHOW package under the package declarator
# names.
my module EXPORTHOW {
    my %who := $?PACKAGE.WHO;
    %who<package>      := Metamodel::PackageHOW;
    %who<module>       := Metamodel::ModuleHOW;
    %who<generic>      := Metamodel::GenericHOW;
    %who<class>        := Metamodel::ClassHOW;
    %who<class-attr>   := Attribute;
    %who<role>         := Metamodel::ParametricRoleHOW;
    %who<role-attr>    := Attribute;
    %who<role-group>   := Metamodel::ParametricRoleGroupHOW;
    %who<grammar>      := Metamodel::GrammarHOW;
    %who<grammar-attr> := Attribute;
    %who<native>       := Metamodel::NativeHOW;
    %who<subset>       := Metamodel::SubsetHOW;
    %who<enum>         := Metamodel::EnumHOW;
    %who<coercion>     := Metamodel::CoercionHOW;
    %who<definite>     := Metamodel::DefiniteHOW;
};

# vim: expandtab sw=4
