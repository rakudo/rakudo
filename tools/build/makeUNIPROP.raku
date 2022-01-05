#!/usr/bin/env raku

# This script generates the Unicode canonical alternates/shortnames ->
# canonical full names hash and the mappings of Unicode properties to
# Str, Bool, and other types.in /src/core/Cool.pm
#
# There is also a testing subroutine below that can also test the mapping of
# canonical alternates/shortnumes to full names as well as a subroutine to
# allow you test the speed of property lookups.
use v6;
use Test;
# Below is the text taken from PropertyAliases-9.0.0.txt from the Unicode site's zip file
my $property-aliases-string = Q:to/🐧/;
# PropertyAliases-10.0.0.txt
# Date: 2017-02-14, 04:26:16 GMT
# © 2017 Unicode®, Inc.
# Unicode and the Unicode Logo are registered trademarks of Unicode, Inc. in the U.S. and other countries.
# For terms of use, see http://www.unicode.org/terms_of_use.html
#
# Unicode Character Database
#   For documentation, see http://www.unicode.org/reports/tr44/
#
# This file contains aliases for properties used in the UCD.
# These names can be used for XML formats of UCD data, for regular-expression
# property tests, and other programmatic textual descriptions of Unicode data.
#
# The names may be translated in appropriate environments, and additional
# aliases may be useful.
#
# FORMAT
#
# Each line has two or more fields, separated by semicolons.
#
# First Field: The first field is an abbreviated name for the property.
#
# Second Field: The second field is a long name
#
# The above are the preferred aliases. Other aliases may be listed in additional fields.
#
# Loose matching should be applied to all property names and property values, with
# the exception of String Property values. With loose matching of property names and
# values, the case distinctions, whitespace, and '_' are ignored. For Numeric Property
# values, numeric equivalencies are applied: thus "01.00" is equivalent to "1".
#
# NOTE: Property value names are NOT unique across properties. For example:
#
#   AL means Arabic Letter for the Bidi_Class property, and
#   AL means Above_Left for the Combining_Class property, and
#   AL means Alphabetic for the Line_Break property.
#
# In addition, some property names may be the same as some property value names.
# For example:
#
#   sc means the Script property, and
#   Sc means the General_Category property value Currency_Symbol (Sc)
#
# The combination of property value and property name is, however, unique.
#
# For more information, see UTS #18: Unicode Regular Expressions
# ================================================


# ================================================
# Numeric Properties
# ================================================
cjkAccountingNumeric     ; kAccountingNumeric
cjkOtherNumeric          ; kOtherNumeric
cjkPrimaryNumeric        ; kPrimaryNumeric
nv                       ; Numeric_Value

# ================================================
# String Properties
# ================================================
cf                       ; Case_Folding
cjkCompatibilityVariant  ; kCompatibilityVariant
dm                       ; Decomposition_Mapping
FC_NFKC                  ; FC_NFKC_Closure
lc                       ; Lowercase_Mapping
NFKC_CF                  ; NFKC_Casefold
scf                      ; Simple_Case_Folding         ; sfc
slc                      ; Simple_Lowercase_Mapping
stc                      ; Simple_Titlecase_Mapping
suc                      ; Simple_Uppercase_Mapping
tc                       ; Titlecase_Mapping
uc                       ; Uppercase_Mapping

# ================================================
# Miscellaneous Properties
# ================================================
bmg                      ; Bidi_Mirroring_Glyph
bpb                      ; Bidi_Paired_Bracket
cjkIICore                ; kIICore
cjkIRG_GSource           ; kIRG_GSource
cjkIRG_HSource           ; kIRG_HSource
cjkIRG_JSource           ; kIRG_JSource
cjkIRG_KPSource          ; kIRG_KPSource
cjkIRG_KSource           ; kIRG_KSource
cjkIRG_MSource           ; kIRG_MSource
cjkIRG_TSource           ; kIRG_TSource
cjkIRG_USource           ; kIRG_USource
cjkIRG_VSource           ; kIRG_VSource
cjkRSUnicode             ; kRSUnicode                  ; Unicode_Radical_Stroke; URS
isc                      ; ISO_Comment
JSN                      ; Jamo_Short_Name
na                       ; Name
na1                      ; Unicode_1_Name
Name_Alias               ; Name_Alias
scx                      ; Script_Extensions

# ================================================
# Catalog Properties
# ================================================
age                      ; Age
blk                      ; Block
sc                       ; Script

# ================================================
# Enumerated Properties
# ================================================
bc                       ; Bidi_Class
bpt                      ; Bidi_Paired_Bracket_Type
ccc                      ; Canonical_Combining_Class
dt                       ; Decomposition_Type
ea                       ; East_Asian_Width
gc                       ; General_Category
GCB                      ; Grapheme_Cluster_Break
hst                      ; Hangul_Syllable_Type
InPC                     ; Indic_Positional_Category
InSC                     ; Indic_Syllabic_Category
jg                       ; Joining_Group
jt                       ; Joining_Type
lb                       ; Line_Break
NFC_QC                   ; NFC_Quick_Check
NFD_QC                   ; NFD_Quick_Check
NFKC_QC                  ; NFKC_Quick_Check
NFKD_QC                  ; NFKD_Quick_Check
nt                       ; Numeric_Type
SB                       ; Sentence_Break
vo                       ; Vertical_Orientation
WB                       ; Word_Break

# ================================================
# Binary Properties
# ================================================
AHex                     ; ASCII_Hex_Digit
Alpha                    ; Alphabetic
Bidi_C                   ; Bidi_Control
Bidi_M                   ; Bidi_Mirrored
Cased                    ; Cased
CE                       ; Composition_Exclusion
CI                       ; Case_Ignorable
Comp_Ex                  ; Full_Composition_Exclusion
CWCF                     ; Changes_When_Casefolded
CWCM                     ; Changes_When_Casemapped
CWKCF                    ; Changes_When_NFKC_Casefolded
CWL                      ; Changes_When_Lowercased
CWT                      ; Changes_When_Titlecased
CWU                      ; Changes_When_Uppercased
Dash                     ; Dash
Dep                      ; Deprecated
DI                       ; Default_Ignorable_Code_Point
Dia                      ; Diacritic
Ext                      ; Extender
Gr_Base                  ; Grapheme_Base
Gr_Ext                   ; Grapheme_Extend
Gr_Link                  ; Grapheme_Link
Hex                      ; Hex_Digit
Hyphen                   ; Hyphen
IDC                      ; ID_Continue
Ideo                     ; Ideographic
IDS                      ; ID_Start
IDSB                     ; IDS_Binary_Operator
IDST                     ; IDS_Trinary_Operator
Join_C                   ; Join_Control
LOE                      ; Logical_Order_Exception
Lower                    ; Lowercase
Math                     ; Math
NChar                    ; Noncharacter_Code_Point
OAlpha                   ; Other_Alphabetic
ODI                      ; Other_Default_Ignorable_Code_Point
OGr_Ext                  ; Other_Grapheme_Extend
OIDC                     ; Other_ID_Continue
OIDS                     ; Other_ID_Start
OLower                   ; Other_Lowercase
OMath                    ; Other_Math
OUpper                   ; Other_Uppercase
Pat_Syn                  ; Pattern_Syntax
Pat_WS                   ; Pattern_White_Space
PCM                      ; Prepended_Concatenation_Mark
QMark                    ; Quotation_Mark
Radical                  ; Radical
RI                       ; Regional_Indicator
SD                       ; Soft_Dotted
STerm                    ; Sentence_Terminal
Term                     ; Terminal_Punctuation
UIdeo                    ; Unified_Ideograph
Upper                    ; Uppercase
VS                       ; Variation_Selector
WSpace                   ; White_Space                 ; space
XIDC                     ; XID_Continue
XIDS                     ; XID_Start
XO_NFC                   ; Expands_On_NFC
XO_NFD                   ; Expands_On_NFD
XO_NFKC                  ; Expands_On_NFKC
XO_NFKD                  ; Expands_On_NFKD

# ================================================
# Total:    120

# EOF
🐧
my Str:D $base-indent = ' ' x 4;
# The code below processes the unicode property string.
sub get-uni-props is export {
    my %hash;
    my $type;
    for $property-aliases-string.lines -> $line {
        my $uniprop-type;
        if $line ~~ /'# '(.*)' Properties'/ {
            $type = ~$0;
        }
        next if $line.starts-with: '#';
        my @names = $line.split(';');
        next if @names.elems < 2;
        my @names-alt;
        push @names-alt, @names.shift.trim;
        my $long-name = @names.shift.trim;
        for @names {
            push @names-alt, .trim;
        }
        next if $long-name ~~ / ^ \s* $ /;
        $uniprop-type = 'B' if ~$type eq 'Binary';
        $uniprop-type = 'S' if ~$type eq any('Enumerated', 'String', 'Catalog');
        given $long-name {
            when 'Uppercase_Mapping' { $uniprop-type = 'uc' }
            when 'Lowercase_Mapping' { $uniprop-type = 'lc' }
            when 'Titlecase_Mapping' { $uniprop-type = 'tc' }
            when 'Name'              { $uniprop-type = 'na' }
            when 'Numeric_Value'     { $uniprop-type = 'nv' }
            when 'ISO_Comment'       { $uniprop-type = 'S'  }
            when 'Bidi_Mirroring_Glyph' { $uniprop-type = 'bmg' }
        }
        for @names-alt {
            my $trimmed = .trim;
            #next if .defined.not;
            #%hash{.trim}<type> = $uniprop-type // $type;
            push %hash{$long-name}<names>, $trimmed;
        }
        %hash{$long-name}<type> = $uniprop-type // $type;
    }
    %hash;
}
# This sub just keeps us under 80 chars width when printing
sub print-line (Str:D $str, Bool :$flush?) {
    my Str:D $indent      = ' ' x 2;
    my Int:D $max-width   = 80 - $indent.chars - $base-indent.chars;
    state Str $full-string = '';
    if $full-string.chars + $str.chars > $max-width or $flush {
        say $base-indent ~ $indent ~ $full-string;
        $full-string = '';
    }
    $full-string ~= $str;
}
# Running this prints the code to stdout
sub create-Str-code {
    my @allowed-types = 'uc', 'lc', 'tc', 'na', 'B', 'S', 'nv', 'bmg';
    my %hash = get-uni-props;
    say $base-indent, '## The code below was generated by tools/build/makeUNIPROP.p6';
    say $base-indent, 'my constant $name2pref = nqp::hash(';
    # These Emoji properties are not in the UCD, but are officially a spec of the Unicode Org
    # These may or may not currently have short names. When/if they do, we should add these to the hash
    # higher up, or generate from some Unicode provided file. At the time of Emoji 4.0 there
    # are no alias names in the files.
    for <Emoji Emoji_Presentation Emoji_Modifier Emoji_Modifier_Base> {
        %hash{$_}<type> = 'B';
    }
    for %hash.keys.sort -> $key {
        if %hash{$key}<names> {
            for %hash{$key}<names>.list -> $shortname {
                %hash{$shortname}<type> = %hash{$key}<type>;
            }
        }
    }
    for %hash.keys.sort -> $key {
        my $value = %hash{$key};
        if $value<type> eq any( @allowed-types) {
            die "key '$key' shouldn't contain any spaces" if $key.contains(' ');
            print-line qq['$key','{$value<type>}',];
        }
    }
    print-line('', :flush );
    say $base-indent, ');';

    my str @prefs;
    use nqp;
    @prefs[.key] = .value.head.value<type> // ""
      for %hash.categorize: { nqp::unipropcode(.key) };
    @prefs[0] = "";  # ambiguous

    say $base-indent, 'my constant $prop2pref = nqp::list_s', @prefs.raku.substr(14), ';';
    say $base-indent, '## End generated code';
}
# This is a test to make sure that all shortnames/alternates return the same
# result as canonical full names This will hopefully not be needed anymore
# when MoarVM issue #448 is resolved...
sub test-Unicode {
    my %hash = get-uni-props;
    for (0..0x1FFFF) -> $codepoint {
        for %hash.kv -> $key, $value {
            for $value<names>.list -> $alternate {
                is $codepoint.uniprop($key), $codepoint.uniprop($alternate),
                    sprintf "[U+%x]: %s is %s", $codepoint, $key, $alternate;
            }
        }
    }
    done-testing;
}
# Can be used to time the lookups by uniprop
sub time-it {
    my $t1 = now;
    for ^10 {
        for (0..0x1FFFF) {
            uniprop $_;
        }
    }
    my $t2 = now;
    say $t2 - $t1;
}
sub MAIN (Bool:D :$test = False) {
    if $test {
        test-Unicode;
    }
    else {
        create-Str-code;
    }
}

# vim: expandtab sw=4
