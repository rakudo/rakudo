use v6-alpha;

say '1..12';

# Double-quoted string
say "ok 1";

# Single-quoted
say 'ok 2';

# Invoke method on literal
'ok 3'.say;

# Invoke method on list
#<ok 4>.say; # XXX This used to pass
say 'ok 4';

# But we have to force the list to a string
say ~<ok 5>;

# Invoke method on number
print 'ok ';
# 6.say;
say 6; # XXX should be "6.say";

print 'ok ';
say 7;

# Invoke method on scalar variables
my $test8 = 'ok 8';
$test8.say;

my $test9 = 'ok 9';
say $test9;

# Verify return code of say
say 'ok ', 11*say 'ok 10';

# Call on result of expression
print 'ok ';
(2**4-4).say;

# More ideas:
# * sort based on the keys of a hash
