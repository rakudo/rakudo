use Test;

plan 8;

# A "\r\n" is a single CR LF grapheme. On its own, \v matches it; inside a
# character class, \v matches only the single vertical-space codepoints, so the
# grapheme is not a member.

my $crlf = "\r\n";

nok $crlf ~~ / ^ <[\v]>  $ /, 'the CR LF grapheme is not a member of <[\v]>';
nok $crlf ~~ / ^ <[\v x]> $ /, 'nor when \v is merged with a literal in a class';
ok  $crlf ~~ / ^ <-[\v]> $ /, 'so <-[\v]> matches the CR LF grapheme';
ok  $crlf ~~ / ^ <[\V]>  $ /, 'and <[\V]> matches it';
ok  $crlf ~~ / ^ \v      $ /, 'while a standalone \v still matches it';

# The individual vertical-space codepoints remain members of the class.
ok "\n"    ~~ / ^ <[\v]> $ /, '<[\v]> matches a line feed';
ok "\x0d"  ~~ / ^ <[\v]> $ /, '<[\v]> matches a carriage return';
nok "x"    ~~ / ^ <[\v]> $ /, '<[\v]> does not match a non-space character';
