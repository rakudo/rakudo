# Load all of the available deparsing localizations
#
# This allows .DEPARSE($lang) to just work, e.g. .DEPARSE("FR")

use RakuAST::Deparse::L10N::DE;
use RakuAST::Deparse::L10N::EN;
use RakuAST::Deparse::L10N::FR;
use RakuAST::Deparse::L10N::IT;
use RakuAST::Deparse::L10N::NL;

# Make available localization introspectable
sub localizations() is export {
    <DE EN FR IT NL>
}

# vim: expandtab shiftwidth=4
