use v6-alpha;

say '1..13';

# Double-quoted string
say "ok 1";

# Single-quoted
say 'ok 2';

# Invoke method on literal
'ok 3'.say;

# Invoke method on list
('ok', ' ', 4).say;
say 'ok', ' ', 5;

# Invoke method on number
print 'ok ';
6.say;

print 'ok ';
say 7;

# Invoke method on scalar variables
my $test8 = 'ok 8';
$test8.say;

my $test9 = 'ok 9';
say $test9;

# Verify return code of say
say 'ok ', 11*say 'ok 10';

# Direct call on result of expression
print 'ok ';
(2**4-4).say;

# Indirect call on result of expression
print 'ok ';
say 2**4-3;
