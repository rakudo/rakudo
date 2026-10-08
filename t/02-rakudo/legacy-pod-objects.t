use Test;
use nqp;

plan 9;

# $=pod holds the same Pod objects whichever frontend compiled this file

=begin pod
X<an item|Define an item>
X<items|defining, a term>
X<a place|Same; Place>
X<terms|a, b; c, d>
X<a thing|entry;>
D<term|synonym>
D<term|first; second>
=end pod

my @codes = $=pod[0].contents[0].contents.grep(Pod::FormattingCode);

is-deeply @codes[0].meta, ["Define an item"],
    'an index entry of a single level is a string';
is-deeply @codes[1].meta, [["defining", " a term"],],
    'commas separate the levels of an index entry';
is-deeply @codes[2].meta, [["Same"], [" Place"]],
    'semicolons separate index entries';
is-deeply @codes[3].meta, [["a", " b"], [" c", " d"]],
    'each index entry has its own levels';
is-deeply @codes[4].meta, ["entry"],
    'a trailing semicolon adds no index entry';
is-deeply @codes[5].meta, ["synonym"],
    'a definition with a single synonym';
is-deeply @codes[6].meta, ["first", " second"],
    'semicolons separate the synonyms of a definition';

=begin pod
X<a word|V<b;c>>
D<a term|V<b;c>>
=end pod

@codes = $=pod[1].contents[0].contents.grep(Pod::FormattingCode);

todo 'the legacy pod grammar reads markup in meta as text', 2
    unless nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';
is-deeply @codes[0].meta, ["b;c"],
    'a semicolon inside V<> does not separate index entries';
is-deeply @codes[1].meta, ["b;c"],
    'a semicolon inside V<> does not separate synonyms';

