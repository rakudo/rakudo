use lib <t/packages/Test-Helpers>;
use Test::Helpers::QAST;
use Test;
use QAST:from<NQP>;
use nqp;
plan 26;

# A `before` assertion whose argument is a single simple check compiles
# to the corresponding zerowidth atom instead of invoking the argument's
# thunk, a method call plus a nested cursor, at every position tried. A
# regex that begins with an anchor to the start of the string drops
# its scan loop. The legacy optimizer's version of the assertion rewrite needs
# the argument held as an inline block, a shape the legacy Rakudo
# frontend rarely produces, so the shape assertions here are this
# frontend's.

sub find-regex-node (Mu $qast, Str:D :$rxtype!, Str :$subtype, Str :$name --> Bool:D) {
    if nqp::istype($qast, QAST::Regex)
        && $qast.rxtype eq $rxtype
        && (!$subtype.defined || $qast.subtype eq $subtype)
        && (!$name.defined || $qast.name eq $name) {
        return True;
    }
    if nqp::istype($qast, QAST::Node) {
        for $qast.list {
            find-regex-node($_, :$rxtype, :$subtype, :$name) and return True;
        }
    }
    False
}

if nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    qast-is 'my $r = rx/ <?before abc> b /', :full, -> \v {
        find-regex-node(v, :rxtype<literal>, :subtype<zerowidth>)
        and not find-regex-node(v, :rxtype<subrule>, :name<before>)
    }, 'a before assertion of a literal compiles to a zerowidth literal';

    qast-is 'my $r = rx/ <?before \d> b /', :full, -> \v {
        find-regex-node(v, :rxtype<cclass>, :subtype<zerowidth>)
        and not find-regex-node(v, :rxtype<subrule>, :name<before>)
    }, 'a before assertion of a character class compiles to a zerowidth class';

    qast-is 'my $r = rx/ <!before <[abc]>> b /', :full, -> \v {
        find-regex-node(v, :rxtype<enumcharlist>, :subtype<zerowidth>)
        and not find-regex-node(v, :rxtype<subrule>, :name<before>)
    }, 'a negated before assertion of a character list compiles to a zerowidth list';

    # The class op demands a character even when negated, so the reduced
    # form pairs it with an anchor to the end of the string, where the
    # thunk's failure makes the negated assertion hold.
    qast-is 'my $r = rx/ <!before \d> b /', :full, -> \v {
        find-regex-node(v, :rxtype<altseq>)
        and find-regex-node(v, :rxtype<cclass>, :subtype<zerowidth>)
        and not find-regex-node(v, :rxtype<subrule>, :name<before>)
    }, 'a negated before assertion of a character class pairs its class with an anchor';

    qast-is 'my $r = rx/ <?before a+b> b /', :full, -> \v {
        find-regex-node(v, :rxtype<subrule>, :name<before>)
    }, 'a before assertion of a quantified argument keeps its thunk';

    qast-is 'my $r = rx/ :i <?before abc> b /', :full, -> \v {
        find-regex-node(v, :rxtype<subrule>, :name<before>)
    }, 'a before assertion under the ignorecase modifier keeps its thunk';

    qast-is 'my $r = rx/ a <?after a> /', :full, -> \v {
        find-regex-node(v, :rxtype<subrule>, :name<after>)
    }, 'an after assertion keeps its thunk';
}
else {
    skip 'the assertion shapes are specific to the RakuAST frontend', 7;
}

qast-is 'my $r = rx/^ foo /', :full, -> \v {
    not find-regex-node(v, :rxtype<scan>)
}, 'a regex anchored to the beginning of the string drops its scan';

qast-is 'my $r = rx/ foo /', :full, -> \v {
    find-regex-node(v, :rxtype<scan>)
}, 'an unanchored regex keeps its scan';

# Behavior stays identical.

{
    my $m = "abc" ~~ / <?before b> /;
    is "$m.from()|$m.to()", '1|1', 'a reduced before assertion matches zero width at the right position';
}

is ("aab" ~~ / a* <?before b> /), 'aa',
    'backtracking settles where the reduced before assertion holds';

is ("ab" ~~ / . <!before \d> /), 'a',
    'a reduced negated before assertion holds where its class does not match';

{
    my token t1 { <?before a> \w+ };
    my token t2 { <?before b> \w+ };
    my regex both { <t1> || <t2> };
    is ("banana" ~~ /<both>/)<both><t2>, 'banana',
        'longest token matching still sees through a before assertion';
}

{
    my $s = "abc";
    $s ~~ s/ <?before b> /X/;
    is $s, 'aXbc', 'a substitution at a reduced before assertion inserts at its position';
}

{
    my $n = 0;
    "aaab" ~~ / [ <?before \w> . { $n++ } ]+ /;
    is $n, 4, 'a quantified group with a reduced before assertion iterates per position';
}

is ("HELLO world" ~~ / :i <?before hello> \w+ /), 'HELLO',
    'a before assertion under the ignorecase modifier still matches through its thunk';

is ("9z" ~~ / <?before <[0..9]>> . /), '9',
    'a before assertion of a character range matches';

# The end of the string is where a negated assertion and a negated
# argument differ: every simple argument demands a character, so its
# thunk fails there, and the assertion's negation decides the outcome.

is ("a" ~~ / a <!before \d> /), 'a',
    'a negated before assertion of a character class holds at the end of the string';

is ("x" ~~ / x <!before <[abc]>> /), 'x',
    'a reduced negated before assertion of a character list holds at the end of the string';

is ("x" ~~ / x <!before <-[abc]>> /), 'x',
    'a negated before assertion of a negated character list holds at the end of the string';

is ("x" ~~ / x <!before <[0..9]>> /), 'x',
    'a negated before assertion of a character range holds at the end of the string';

nok ("x9" ~~ / x <!before <[0..9]>> /).defined,
    'a negated before assertion of a character range fails on a range character';

nok ("a" ~~ / a <?before <-[b]>> /).defined,
    'a before assertion of a negated character list fails at the end of the string';

nok ("a" ~~ / a <?before \D> /).defined,
    'a before assertion of a negated character class fails at the end of the string';

is ("x" ~~ / x <!before yz> /), 'x',
    'a negated before assertion of a literal holds at the end of the string';

is ("xay" ~~ / x <?after x> a /), 'xa',
    'an after assertion matches through its thunk';

# vim: expandtab shiftwidth=4
