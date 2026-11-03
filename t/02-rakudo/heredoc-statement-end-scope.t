use MONKEY-SEE-NO-EVAL;
use Test;
use lib <t/packages/02-rakudo/lib>;

plan 12;

# The body of a heredoc in a statement prefix, a phaser, a constant or a use
# that ends its line with a semicolon belongs to the scope of that statement.

is (try EVAL q:to/CODE/), "xbc\n", 'try with a heredoc resolves lexicals of the enclosing scope';
my $foo = "abc";
my $bar = "x";
try qq:to/MESSAGE/;
$foo.subst("a", $bar)
MESSAGE
CODE

is (try EVAL q:to/CODE/), "do-body\n", 'do with a heredoc resolves a lexical of the enclosing scope';
my $do-word = "do-body";
do qq:to/END/;
$do-word
END
CODE

is (try EVAL q:to/CODE/), "quiet-body\n", 'quietly with a heredoc resolves a lexical of the enclosing scope';
my $quiet-word = "quiet-body";
quietly qq:to/END/;
$quiet-word
END
CODE

is (try EVAL q:to/CODE/), "once-body\n", 'once with a heredoc resolves a lexical of the enclosing scope';
my $once-word = "once-body";
once qq:to/END/;
$once-word
END
CODE

is-deeply (try EVAL q:to/CODE/), ["gather-body\n"], 'gather with a heredoc resolves a lexical of the enclosing scope';
my $gather-word = "gather-body";
my @taken = gather take qq:to/END/;
$gather-word
END
@taken
CODE

is (try EVAL q:to/CODE/), "start-body\n", 'start with a heredoc resolves a lexical of the enclosing scope';
my $start-word = "start-body";
my $promise = start qq:to/END/;
$start-word
END
await $promise
CODE

is (try EVAL q:to/CODE/), "hi!\n", 'BEGIN with a heredoc resolves a lexical of the enclosing scope';
my $greeting;
BEGIN $greeting = "hi";
my $composed;
BEGIN $composed = qq:to/END/;
$greeting!
END
$composed
CODE

is (try EVAL q:to/CODE/), "hello raku\n", 'a constant with a heredoc resolves a lexical of the enclosing scope';
constant $name = "raku";
constant GREETING = qq:to/END/;
hello $name
END
GREETING
CODE

is-deeply (try EVAL q:to/CODE/), ["inner\n"], 'use with a heredoc argument resolves the sub constant, not a shadowed outer one';
constant $use-word = "outer";
sub with-use() {
    constant $use-word = "inner";
    use HeredocUseArgs qq:to/END/;
    $use-word
    END
    heredoc-use-args()
}
with-use()
CODE

is (try EVAL q:to/CODE/), "inner\n", 'a heredoc statement in a sub resolves the sub lexical, not a shadowed outer one';
my $shadowed = "outer";
sub inside() {
    my $shadowed = "inner";
    my $result = try qq:to/END/;
    $shadowed
    END
    $result
}
inside()
CODE

is (try EVAL q:to/CODE/), "block-body\n", 'a heredoc statement in a bare block resolves the block lexical';
{
    my $block-word = "block-body";
    try qq:to/END/;
    $block-word
    END
}
CODE

# A heredoc whose line ends in the closing brace of a block has its body
# outside that block, so the lexicals of that block are out of reach.
throws-like q:to/CODE/, X::Undeclared, 'a heredoc body after a closing brace does not see the lexicals of that block';
sub closed() { my $closed-word = "closed"; qq:to/END/ }
$closed-word
END
CODE
