# Bind the HOWs into the EXPORTHOW package under the package declarator
# names.
my module EXPORTHOW {
    nqp::bindkey($?PACKAGE.WHO, 'package', Perl6::Metamodel::PackageHOW);
    nqp::bindkey($?PACKAGE.WHO, 'module', Perl6::Metamodel::ModuleHOW);
    nqp::bindkey($?PACKAGE.WHO, 'generic', Perl6::Metamodel::GenericHOW);
    nqp::bindkey($?PACKAGE.WHO, 'class', Perl6::Metamodel::ClassHOW);
    nqp::bindkey($?PACKAGE.WHO, 'class-attr', Attribute);
    nqp::bindkey($?PACKAGE.WHO, 'role', Perl6::Metamodel::ParametricRoleHOW);
    nqp::bindkey($?PACKAGE.WHO, 'role-attr', Attribute);
    nqp::bindkey($?PACKAGE.WHO, 'role-group', Perl6::Metamodel::ParametricRoleGroupHOW);
    nqp::bindkey($?PACKAGE.WHO, 'grammar', Perl6::Metamodel::GrammarHOW);
    nqp::bindkey($?PACKAGE.WHO, 'grammar-attr', Attribute);
    nqp::bindkey($?PACKAGE.WHO, 'native', Perl6::Metamodel::NativeHOW);
    nqp::bindkey($?PACKAGE.WHO, 'subset', Perl6::Metamodel::SubsetHOW);
    nqp::bindkey($?PACKAGE.WHO, 'enum', Perl6::Metamodel::EnumHOW);
}
