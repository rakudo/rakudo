# Load all of the available deparsing localizations
#
# This allows .DEPARSE($lang) to just work, e.g. .DEPARSE("FR")

# Available localizations introspectable by RakuAST::Deparse::L10N::.keys
use RakuAST::Deparse::L10N::DE;
use RakuAST::Deparse::L10N::EN;
use RakuAST::Deparse::L10N::FR;
use RakuAST::Deparse::L10N::HU;
use RakuAST::Deparse::L10N::IT;
use RakuAST::Deparse::L10N::NL;
use RakuAST::Deparse::L10N::PT;

# vim: expandtab shiftwidth=4
