use Test;

plan 21;

# Under :i a string interpolated into a regex is compared after case
# folding, and folding can change how many graphemes the target and the
# pattern have. The match has to take from the target the graphemes the
# comparison consumed, not the pattern's own length, or it ends at the
# wrong position. The array cases where the element folds to more
# graphemes than the target use the sequential form: the NFA that picks
# candidates for `@a` encodes case per codepoint and cannot see SS in ß,
# exactly as it cannot for the literal `[ SS | x ]`.

is 'ß' ~~ /:i [||@(['SS'])]/, 'ß',
    'an interpolated array element folding to more graphemes matches a shorter target';
is ('ß' ~~ /:i [||@(['SS'])]/).to, 1,
    'the match ends where the target ends';

is 'SS' ~~ /:i @(['ß'])/, 'SS',
    'an interpolated array element folding to fewer graphemes takes the whole target';

is 'ß' ~~ /:i $('SS')/, 'ß',
    'an interpolated string folding to more graphemes matches a shorter target';
is 'SS' ~~ /:i $('ß')/, 'SS',
    'an interpolated string folding to fewer graphemes takes the whole target';

is 'STRASSE' ~~ /:i $('straße') $/, 'STRASSE',
    'a fold in the middle of an interpolated string is measured against the target';

is 'aß' ~~ /:i a $('SS') $/, 'aß',
    'a match after an earlier atom is measured from its own start';
is 'aSS' ~~ /:i a $('ß') $/, 'aSS',
    'a match after an earlier atom takes the graphemes from its own start';

is ('ßSSß' ~~ m:i:g/$('SS')/).join('|'), 'ß|SS|ß',
    'each match under :g advances by the graphemes it took';
is ('SSSS' ~~ m:i:g/$('ß')/).join('|'), 'SS|SS',
    'each match of a pattern folding to fewer graphemes advances past the graphemes it took';

is 'ß' ~~ /:i:m $('SS')/, 'ß',
    'a pattern folding to more graphemes matches under ignorecase and ignoremark';
is 'SS' ~~ /:i:m $('ß')/, 'SS',
    'a pattern folding to fewer graphemes takes the whole target under ignorecase and ignoremark';

is 'ﬃ' ~~ /:i $('FFI')/, 'ﬃ',
    'a ligature folding to three graphemes matches a one grapheme target';
is 'FFI' ~~ /:i $('ﬃ')/, 'FFI',
    'a ligature pattern takes all three target graphemes';
is ('ßß' ~~ /:i $('SSSS')/).to, 2,
    'two adjacent folding graphemes are both taken';

my $list = ['SS'];
is 'ß' ~~ /:i [||$list]/, 'ß',
    'an array held in a scalar folds to match a shorter target';

is 'SSSS' ~~ /:i $('ß')+ $/, 'SSSS',
    'a quantified pattern takes the target two graphemes at a time';

is 'ß' ~~ /:i @(< SS ß >)/, 'ß',
    'an unmatchable folding element does not block a later element';

is 'abc' ~~ /:i @(< AB >) c/, 'abc',
    'an interpolated array element without a fold expansion takes its own length';
is 'SŚ' ~~ /:m $('SS')/, 'SŚ',
    'ignoremark alone takes the pattern length';
is 'Straße' ~~ /:i [||@(< STRASSE >)]/, 'Straße',
    'a target folding to more graphemes than the interpolated element';

# vim: expandtab shiftwidth=4
