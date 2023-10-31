# Load all of the available localization slang roles
#
# This allows .DEPARSE($lang) to just work, e.g. .DEPARSE("FR")

# Available localization roles introspectable with LION::.keys
use L10N::DE 'dontslang';
use L10N::EN 'dontslang';
use L10N::FR 'dontslang';
use L10N::IT 'dontslang';
use L10N::NL 'dontslang';
use L10N::PT 'dontslang';

my sub EXPORT() {
    BEGIN Map.new: ("L10N" => L10N)
}

# vim: expandtab shiftwidth=4
