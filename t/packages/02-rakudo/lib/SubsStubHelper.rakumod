unit module SubsStubHelper;

# Exports a stub routine (body is just `...`) and a real routine, so a using
# program can try to override each with a declaration of the same name.
sub stub-export() is export { ... }
sub real-export() is export { "real-export" }
