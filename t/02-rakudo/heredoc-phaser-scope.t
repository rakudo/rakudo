use Test;

plan 7;

# A BEGIN-time assignment whose value is a heredoc writes the enclosing lexical,
# and that write is visible once the mainline runs.
my $written;
BEGIN $written = q:to/END/;
line one
line two
END
is $written, "line one\nline two\n", 'BEGIN assignment of a heredoc reaches the outer lexical';

my $prefixed;
BEGIN $prefixed = 'p-' ~ q:to/END/;
body
END
is $prefixed, "p-body\n", 'BEGIN assignment of a heredoc within an expression reaches the outer lexical';

# A CHECK phaser writing a heredoc behaves the same as BEGIN.
my $checked;
CHECK $checked = q:to/END/;
checked
END
is $checked, "checked\n", 'CHECK assignment of a heredoc reaches the outer lexical';

# The mainline may consume a lexical a BEGIN populated: a gather reading the
# heredoc-holding variable sees what the BEGIN stored, even though the gather
# appears before the BEGIN in the source.
my $data;
my %counts = gather for $data.lines -> $line {
    take $line => $line.chars;
}
BEGIN $data = q:to/END/;
aa
bbb
END
is %counts<aa>, 2, 'a gather over a BEGIN-set heredoc lexical sees the value';
is %counts<bbb>, 3, 'the gather processed every line of the heredoc';

# A later phaser must resolve enclosing lexicals too, not just the first.
my $first;
my $second;
BEGIN $first = q:to/END/;
one
END
BEGIN $second = "have-" ~ $first.chomp;
is $second, 'have-one', 'a phaser after a heredoc phaser still resolves the outer lexical';

# A read of an enclosing lexical from a heredoc BEGIN resolves it as well. The
# lexical is set at BEGIN time, so the read observes that value.
my $greeting;
BEGIN $greeting = 'hi';
my $composed;
BEGIN $composed = $greeting ~ q:to/END/;
!
END
is $composed, "hi!\n", 'a heredoc BEGIN reading a BEGIN-set outer lexical observes its value';
