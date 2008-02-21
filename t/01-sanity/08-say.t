use v6-alpha;

say '1..10';

# Double-quoted string
say "ok 1";

# Single-quoted
say 'ok 2';

# Invoke method on literal
'ok 3'.say;

# Invoke method on list
<ok 4>.say;

# But we have to force the list to a string
say ~<ok 5>;

# Invoke method on number
print 'ok ';
# 6.say;
say 6; # XXX should be "6.say";

print 'ok ';
say 7;

# Invoke method on variable
my $test8 = 'ok 8';
$test8.say;

# Verify return code of say
say 'ok ', 10*say 'ok 9';

# More ideas:
# * sort based on the keys of a hash
