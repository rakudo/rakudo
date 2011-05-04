# Bind the HOWs into the EXPORTHOW package under the package declarator
# names.
my module EXPORTHOW {
    ($?PACKAGE.WHO)<package>      := PackageHOW;
    ($?PACKAGE.WHO)<module>       := ModuleHOW;
    ($?PACKAGE.WHO)<class>        := ClassHOW;
}
