#!/usr/bin/env raku

# This script reads the translation files in tools/templates/L10N and
# converts each file found ("xxx") in there (except "CORE") to a source file
# in lib/RakuAST/Deparse/L10N/xxx.rakumod to provide deparsing for that
# localization, and a source file in lib/L10N/xxx.rakumod to provide
# activation of that localization as a slang.

# Get the role generation logic
use RakuAST::L10N;

# For all available localizations
for localization-files() -> $io {
    update-module($io);
}

# vim: expandtab sw=4
