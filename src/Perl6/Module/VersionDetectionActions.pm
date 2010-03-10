# This is an extremely simple actions class for the Perl 6 grammar.
# It looks until it finds the first package_def in the file, then
# extracts the version details and aborts the parse. It is used for
# finding the effective version and authority for a module.
class Perl6::Module::VersionDetectionActions;

has $!ver;
has $!auth;

method ver() {
    $!ver // -1
}

method auth() {
    $!auth // ""
}

method package_def($/, $key?) {
    if $<def_module_name>[0]<longname><colonpair> {
        for $<def_module_name>[0]<longname><colonpair> {
            if $_<identifier> eq 'ver' {
                $!ver := ~$_<circumfix><quote_EXPR><quote_delimited><quote_atom>[0];
            }
            if $_<identifier> eq 'auth' {
                $!auth := $_<circumfix><quote_EXPR><quote_delimited><quote_atom>[0];
            }
        }
    }
    pir::die('PACKAGE_INFO_FOUND');
}
