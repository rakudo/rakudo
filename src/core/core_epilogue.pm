# Re-parent meta-objects so they appear to be under Any.
BEGIN {
    Perl6::Metamodel::ClassHOW.HOW.reparent(Perl6::Metamodel::ClassHOW, Any);
    Perl6::Metamodel::ConcreteRoleHOW.HOW.reparent(Perl6::Metamodel::ConcreteRoleHOW, Any);
    Perl6::Metamodel::CurriedRoleHOW.HOW.reparent(Perl6::Metamodel::CurriedRoleHOW, Any);
    Perl6::Metamodel::EnumHOW.HOW.reparent(Perl6::Metamodel::EnumHOW, Any);
    Perl6::Metamodel::GenericHOW.HOW.reparent(Perl6::Metamodel::GenericHOW, Any);
    Perl6::Metamodel::ModuleHOW.HOW.reparent(Perl6::Metamodel::ModuleHOW, Any);
    Perl6::Metamodel::NativeHOW.HOW.reparent(Perl6::Metamodel::NativeHOW, Any);
    Perl6::Metamodel::PackageHOW.HOW.reparent(Perl6::Metamodel::PackageHOW, Any);
    Perl6::Metamodel::ParametricRoleGroupHOW.HOW.reparent(Perl6::Metamodel::ParametricRoleGroupHOW, Any);
    Perl6::Metamodel::ParametricRoleHOW.HOW.reparent(Perl6::Metamodel::ParametricRoleHOW, Any);
    Perl6::Metamodel::SubsetHOW.HOW.reparent(Perl6::Metamodel::SubsetHOW, Any);
    Perl6::Metamodel::GrammarHOW.HOW.compose(Perl6::Metamodel::GrammarHOW);
}

{YOU_ARE_HERE}
