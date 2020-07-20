use v6;

say '1..12';

# Double-quoted string
say "ok 1";

# Single-quoted
say 'ok 2';

# Invoke method on literal
'ok 3'.say;

say 'ok', ' ', 4;

# Invoke method on number
print 'ok ';
5.say;

print 'ok ';
say 6;

# Invoke method on scalar variables
my $test7 = 'ok 7';
$test7.say;

my $test8 = 'ok 8';
say $test8;

# Verify return code of say
say 'ok ', 10*say 'ok 9';

# Direct call on result of expression
print 'ok ';
(2**4-5).say;

# Indirect call on result of expression
print 'ok ';
say 2**4-4;

# vim: expandtab shiftwidth=4
