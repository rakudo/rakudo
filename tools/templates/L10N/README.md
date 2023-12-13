# Introduction
The `tools/templates/L10N` directory contains the translations for the
localizations that the Raku Programming Language supports "out of the
box".

Each file consists of lines, which are either a comment if they start
with a "#" (and are thus ignored), or they consist of *two* words (as
in: two strings, separated by whitespace)..

The first word consists of a prefix, followed by a hyphen, and then the
name of the Raku feature to be translated.

The second word should be the translation in the given localization.

So, e.g. in an Italian localization:

    block-if    se
    core-say    dillo

would indicate that "se" should be used instead of "if" when it is used
before a block.  And that the core feature "say" would need be expressed
as "dillo" in an Italian localization.  So that one could say:

    se 42 {
        dillo "ciao!";
    }

as the Italian localization of:

    if 42 {
        say "ciao!";
    }

# Defined Prefixes
The following prefixes are currently defined:

## adverb-pc
Core adverbs on postcircumfixes (:exists, :delete, etc.)

## adverb-q
Quote language adverbs ("to","ww","v", etc.)

## adverb-rx
Core adverbs on regexes (:i, :m, :ignorecase, etc.)

## block
Syntax involving a block ("if","elsif","else","start", etc.)

## core
Sub/Method names that are part of the Raku core ("say", etc.)

## constraint
Related to (ad-hoc) constraints ("where", etc.))

## infix
Infix operators with alphanumeric chars ("eq","ne","cmp", etc.)

## meta
Meta-operator prefixes ('R','X','Z')

## modifier
Statement modifier syntax ("if","with", etc.)

## multi
Types of multi syntax ("multi","only","proto")

## named
Named arguments to methods/subs (:k,:p,:absolute, etc.)

## package
Package declarators ("class","module","grammar, etc.)

## pragma
Compile-time pragmas ("lib","precompilation", etc.)

## prefix
Prefix operators with alphanumeric chars ("so","not")

## phaser
Types of phasers ("BEGIN","END","CHECK", etc.)

## quote-lang
Quote language markers ("q","Q","qq", etc.)

## routine
Types of named code blocks ("sub","method", etc.)

## scope
Types of scope ("my","our","state", etc.)

## stmt-prefix
Statement prefixes ("do","eager","lazy","quietly", etc.)

## system
Methods called by the system (TWEAK,BUILD,ACCEPTS etc.)

## term
Terms ("time","now","self",etc.)

## trait-is
Names of "is" traits ("default","ro","raw","DEPRECATED", etc.)

## traitmod
Types of trait_mods ("is","does","returns", etc.)

## typer
Type constructors ("enum","subset")

## use
Use related ("use","no","require", etc.)

# Additional notes
Please note that the second word currently *must* adhere to Raku identifier
rules, so that they must start with a \w character (which includes the
underscore), and may have a hyphen as long as it is not the last character
if the word, and as long as there are no two consecutive hyphens.

The "CORE" file contains the "null" translation, as they would occur in the
core Raku Language.

The "EN" file contains the English translation.  This is currently identical
to the "CORE" translation, but *could* start to differ in the future from
"CORE".

# Adding a new localization
From a translator point of view, the only thing that needs to be added is
a translation file in this directory.  The name of the file should be an
[ISO 639-1](https://en.wikipedia.org/wiki/ISO_639-1) supported language code,
at least for the foreseeable future.

No checks are made on the actual validity.  The given code will become part
of the module names for activating the slang (`use L10N::IT` to activate the
Italian localization) and the deparsing (`use RakuAST::Deparse::L10N::IT`).

The easiest way to start a new localization, is to copy one of the existing
localization files (such as `EN`, or any other localization file that a
translator may feel is closer to the new localization)), and then start
changing the second word on each line.

If the translated word is the same, then leave that line in, as it indicates
that the translator agreed with the unchanged word.  If the translator does
not know yet how to translate the word, then mark the line as a comment by
prefixing a `#`.  This is functionally the same as identity, but is a marker
for potential future translation.

After committing the localization file in this directory, someone with commit
access to the Rakudo repository will need to run the:

    tools/build/makeL10N.raku

script: this will create the actual module files in `lib/L10N` and
`lib/RakuAST/Deparse/L10N`.

# The Future
Please note that as the Raku Programming Language evolves, further
elements may be added, so any translations will probably need to be
updated by then as well.

Any additional items to translate will need to be added to each supported
localization as a comment line, indicating "not yet translated" status.
