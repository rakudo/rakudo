# Bind the HOWs into the EXPORTHOW package under the package declarator
# names.
my module EXPORTHOW {
    nqp::bindkey($?PACKAGE.WHO, 'package', Metamodel::PackageHOW);
    nqp::bindkey($?PACKAGE.WHO, 'module', Metamodel::ModuleHOW);
    nqp::bindkey($?PACKAGE.WHO, 'generic', Metamodel::GenericHOW);
    nqp::bindkey($?PACKAGE.WHO, 'class', Metamodel::ClassHOW);
    nqp::bindkey($?PACKAGE.WHO, 'class-attr', Attribute);
    nqp::bindkey($?PACKAGE.WHO, 'role', Metamodel::ParametricRoleHOW);
    nqp::bindkey($?PACKAGE.WHO, 'role-attr', Attribute);
    nqp::bindkey($?PACKAGE.WHO, 'role-group', Metamodel::ParametricRoleGroupHOW);
    nqp::bindkey($?PACKAGE.WHO, 'grammar', Metamodel::GrammarHOW);
    nqp::bindkey($?PACKAGE.WHO, 'grammar-attr', Attribute);
    nqp::bindkey($?PACKAGE.WHO, 'native', Metamodel::NativeHOW);
    nqp::bindkey($?PACKAGE.WHO, 'subset', Metamodel::SubsetHOW);
    nqp::bindkey($?PACKAGE.WHO, 'enum', Metamodel::EnumHOW);
    nqp::bindkey($?PACKAGE.WHO, 'coercion', Metamodel::CoercionHOW);
    nqp::bindkey($?PACKAGE.WHO, 'definite', Metamodel::DefiniteHOW);
}

# vim: expandtab shiftwidth=4
