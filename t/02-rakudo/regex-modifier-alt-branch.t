use lib <t/packages/Test-Helpers>;
use Test;
use Test::Helpers;

plan 5;

# An internal regex modifier (`:i`, `:m`, `:r`, `:s`) alone as a top-level
# branch of an alternation or conjunction applies to the surrounding
# construct and compiles to an empty branch.

is-run q:to/CODE/,
    grammar G {
        token irregular {
            :i
            | 'en-GB-oed'
            | 'i-ami'
        }
    }
    say G.parse('EN-GB-OED', :rule<irregular>) // 'no';
    say G.parse('i-ami',     :rule<irregular>) // 'no';
    say G.parse('zzz',       :rule<irregular>) // 'no';
    CODE
    :out("｢EN-GB-OED｣\n｢i-ami｣\nno\n"),
    ':i as the first branch of `|` alternation matches with the modifier applied';

is-run q:to/CODE/,
    grammar G {
        token middle { 'a' | :i | 'B' }
    }
    say G.parse('a', :rule<middle>) // 'no';
    say G.parse('b', :rule<middle>) // 'no';
    CODE
    :out("｢a｣\n｢b｣\n"),
    ':i in a non-first branch position applies to subsequent branches';

is-run q:to/CODE/,
    grammar G {
        token nego { :!i | 'foo' | 'bar' }
    }
    say G.parse('foo', :rule<nego>) // 'no';
    say G.parse('FOO', :rule<nego>) // 'no';
    CODE
    :out("｢foo｣\nno\n"),
    ':!i as the first branch of `|` alternation keeps case-sensitive matching';

is-run q:to/CODE/,
    grammar G {
        token seqalt {
            :i
            || 'foo'
            || 'bar'
        }
    }
    G.parse('FOO', :rule<seqalt>);
    say 'compiled';
    CODE
    :out("compiled\n"),
    ':i as the first branch of `||` alternation compiles';

is-run q:to/CODE/,
    grammar G {
        token conjAtom { :i & <[ a..z A..Z ]> & 'A' }
    }
    G.parse('A', :rule<conjAtom>);
    say 'compiled';
    CODE
    :out("compiled\n"),
    ':i as the first branch of `&` conjunction compiles';

# vim: expandtab shiftwidth=4
