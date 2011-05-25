# Bind the HOWs into the EXPORTHOW package under the package declarator
# names.
# XXX Workarounds: no binding yet, no string literals yet!
my module EXPORTHOW {
    pir::set__vQsPP($?PACKAGE.WHO, Q:PIR{ %r = box 'package' }, Perl6::Metamodel::PackageHOW);
    pir::set__vQsPP($?PACKAGE.WHO, Q:PIR{ %r = box 'module' }, Perl6::Metamodel::ModuleHOW);
    pir::set__vQsPP($?PACKAGE.WHO, Q:PIR{ %r = box 'class' }, Perl6::Metamodel::ClassHOW);
    pir::set__vQsPP($?PACKAGE.WHO, Q:PIR{ %r = box 'class-attr' }, Attribute);
    pir::set__vQsPP($?PACKAGE.WHO, Q:PIR{ %r = box 'native' }, Perl6::Metamodel::NativeHOW);
}
