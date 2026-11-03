use Test;

plan 3;

# A heredoc as an attribute default: its body is parsed after the attribute
# declaration line, so the default must pick up the body, not the terminator.
class WithString {
    has $.style = q:to/CSS/;
      #deckgl { height: 95vh; width: 95vw; }
      #deckwrapper { border: 1px solid black; }
      CSS
}

my $style = WithString.new.style;
is $style.lines.elems, 2, 'a heredoc attribute default holds its whole body';
ok $style.contains('height: 95vh'), 'the body content is present, not the terminator';
isnt $style.trim, 'CSS', 'the default is not the heredoc terminator word';
