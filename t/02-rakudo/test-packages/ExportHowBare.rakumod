# Exports its own `class` HOW through a bare (non-`my`) EXPORTHOW package, the
# way InterceptAllMethods does. A bare `package EXPORTHOW` must be lexical, not
# `our`: an `our` one leaks into GLOBAL and, when this module is used, clashes
# with the setting's own EXPORTHOW. The HOW adds a marker method at compose time
# so a consumer can observe that its `class` declarations went through here.
class MetamodelX::ExportHowBare is Metamodel::ClassHOW {
    method compose(Mu \type) {
        self.add_method(type, 'composed-by-bare-exporthow', sub ($) { True });
        self.Metamodel::ClassHOW::compose(type)
    }
}
package EXPORTHOW {
    constant class = MetamodelX::ExportHowBare;
}
