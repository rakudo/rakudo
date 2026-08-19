use lib <t/packages/Test-Helpers>;
use Test;
use Test::Helpers;

plan 3;

# Legacy never reports a missing declarand. RakuAST does, but only for a
# leading declarator doc that reaches no declarand at all (that case lives in
# t/05-messages/10-warnings.t). These check the cases RakuAST used to warn
# about where legacy stays silent.

# A trailing #= that cannot attach to the preceding declarand (it sits on its
# own line, past the previous routine) used to become a "missing declarand"
# worry once the following routine was seen. It should be dropped silently.
is-run q:to/CODE/,
        class C {
            method a { 1 }

            #= documents the wrong way round
            method b { 2 }
        }
        print "ran";
        CODE
    'a trailing #= that reaches no declarand does not warn',
    :out<ran>, :err(''), :exitcode(0);

# A leading #| before a lexical, with nothing later to document, grounds on
# the lexical rather than being reported as a missing declarand.
is-run q:to/CODE/,
        #| documents the counter
        my $counter = 0;
        print "ran";
        CODE
    'a leading #| before a lexical with no other target does not warn',
    :out<ran>, :err(''), :exitcode(0);

# The lexical must not swallow the doc when a documentable target follows: a
# leading #| before a lexical whose initializer is a sub still documents the
# sub.
is EVAL(q:to/CODE/), 'inner',
        #| inner
        my $code = sub { 1 };
        $code.WHY.contents.head
        CODE
    'a leading #| falls through a lexical to a sub in its initializer';
