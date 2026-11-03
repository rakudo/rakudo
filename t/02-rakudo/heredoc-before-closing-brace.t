use MONKEY-SEE-NO-EVAL;
use Test;
use nqp;

plan 12;

# A statement prefix, phaser, constant or use right before the closing brace
# of a block leaves that brace to the block, which takes the heredoc body. A
# BEGIN, constant or use there runs before the body is parsed, so it dies. A
# block takes the body when a semicolon follows its brace at the end of the
# line too.

is (try EVAL q:to/CODE/), "outer-word\n", 'try before the closing brace of a sub takes the heredoc body';
    my $word = "outer-word";
    sub prefixed() { try qq:to/END/ }
        $word
        END
    prefixed()
    CODE

is (try EVAL q:to/CODE/), "a\nb\n", 'two heredocs before the closing brace of a sub both take their bodies';
    sub two() { try q:to/A/ ~ q:to/B/ }
        a
        A
        b
        B
    two()
    CODE

is (try EVAL q:to/CODE/), "bare\n", 'do before the closing brace of a bare block takes the heredoc body';
    { do q:to/END/ }
        bare
        END
    CODE

is (try EVAL q:to/CODE/), "checked\n", 'a CHECK phaser before the closing brace of a block takes the heredoc body';
    my $y;
    { CHECK $y = q:to/END/ }
        checked
        END
    $y
    CODE

is (try EVAL q:to/CODE/), "inited\n", 'an INIT phaser before the closing brace of a block takes the heredoc body';
    my $y;
    { INIT $y = q:to/END/ }
        inited
        END
    $y
    CODE

is (try EVAL q:to/CODE/), "entered\n", 'an ENTER phaser before the closing brace of a block takes the heredoc body';
    my $y;
    { ENTER $y = q:to/END/ }
        entered
        END
    $y
    CODE

throws-like q:to/CODE/, X::Comp::BeginTime, message => /'Premature heredoc consumption'/, 'a BEGIN before a closing brace reports premature heredoc consumption';
    my $early;
    { BEGIN $early = q:to/END/ }
        body
        END
    CODE

throws-like q:to/CODE/, X::Comp::BeginTime, message => /'Premature heredoc consumption'/, 'a constant before the closing brace of a class reports premature heredoc consumption';
    class Early { constant EARLY = q:to/END/ }
        body
        END
    CODE

throws-like q:to/CODE/, X::Comp, line => 2, 'a use before a closing brace fails at its line';
    my $pad;
    { use lib q:to/END/ }
        /nonexistent
        END
    CODE

is (try EVAL q:to/CODE/), "body\n", 'a BEGIN block whose brace and a semicolon end its line takes the heredoc body';
    my $got;
    BEGIN { $got = q:to/END/ };
        body
        END
    $got
    CODE

if nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    is (try EVAL q:to/CODE/), "tagged\n", 'a trait of a sub whose brace and a semicolon end its line reads the heredoc body';
        multi trait_mod:<is>(Routine $r, :$heredoc-tagged!) {
            $r does role { method tag() { $heredoc-tagged } }
        }
        sub tagged() is heredoc-tagged(q:to/END/) { 1 };
            tagged
            END
        &tagged.tag
        CODE
}
else {
    skip 'the legacy frontend applies the traits of a sub before it parses the block';
}

is (try EVAL q:to/CODE/), "body\n 2", 'a block in an array whose brace and a semicolon end the line keeps the next element';
    my @a = [ { q:to/END/ };
        body
        END
      2 ];
    @a[0]() ~ " " ~ @a[1]
    CODE
