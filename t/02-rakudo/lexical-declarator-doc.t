use lib <t/packages/Test-Helpers>;
use Test;
use Test::Helpers;

plan 8;

# A trailing declarator doc (`#=`) attaches to the declaration it follows,
# including a lexical variable declaration. A clean run (no warning on
# stderr) shows the doc attached to its declaration instead of being
# reported as a missing declarand.
is-run q:to/CODE/,
        my $lock;      #= the lock
        my %registry;  #= the registry
        sub helper { }
        print "ok";
        CODE
    :out("ok"), :err(""),
    'a trailing declarator doc on a lexical does not warn about a missing declarand';

is-run q:to/CODE/,
        our $global;  #= the global
        sub tick { state $n;  #= the counter
        }
        print "ok";
        CODE
    :out("ok"), :err(""),
    'trailing declarator docs on our and state declarations do not warn either';

# A lexical only takes trailing doc. A leading `#|` before it still falls
# through to the next documentable declarand (here the anon sub), rather than
# being consumed by the lexical, where it would be hidden from $=pod.
is-run q:to/CODE/,
        #| documented
        my $f = anon sub bar { };
        print $=pod.elems;
        CODE
    :out("1"), :err(""),
    'leading declarator doc before a lexical falls through to the next declarand';

# A lexical has no runtime meta-object to carry documentation, so its doc
# must not be surfaced through the meta-object of its *type*.
is-run q:to/CODE/,
        my Int $x;  #= int doc
        sub helper { }
        print Int.WHY.defined ?? "leaked" !! "clean";
        print $=pod.elems;
        CODE
    :out("clean0"), :err(""),
    'a trailing declarator doc on a typed lexical does not become the WHY of its type';

is-run q:to/CODE/,
        #| class doc
        class C {}
        my C $c;  #= var doc
        print C.WHY.leading;
        CODE
    :out("class doc"), :err(""),
    'a trailing declarator doc on a typed lexical does not replace the class doc';

# The lexical's doc does not appear in $=pod, so it must not reserve a
# position there either.
is-run q:to/CODE/,
        my $x;  #= var doc
        #| sub doc
        sub f() {}
        print $=pod.elems;
        print "|";
        print $=pod[0].defined;
        CODE
    :out("1|True"), :err(""),
    'a documented lexical does not leave a hole in $=pod';

# A HAS attribute is documentable just like a has attribute.
is-run q:to/CODE/,
        class Point is repr("CStruct") { has num64 $.x }
        class Line is repr("CStruct") {
            HAS Point $!start;  #= start point
        }
        print Line.^attributes[0].WHY;
        CODE
    :out("start point"), :err(""),
    'a trailing declarator doc attaches to a HAS attribute without warning';

# The doc of a lexical stays on its RakuAST declaration node, where the
# $=rakudoc collection picks it up.
if %*ENV<RAKUDO_RAKUAST> {
    is-run q:to/CODE/,
            my $lock;  #= the lock
            sub helper { }
            print $=rakudoc.elems;
            print "|";
            print $=rakudoc[0].WHY.trailing;
            CODE
        :out("1|the lock"), :err(""),
        'the declarator doc of a lexical is available through $=rakudoc';
}
else {
    skip 'the $=rakudoc variable requires the RakuAST frontend';
}

# vim: expandtab shiftwidth=4
