# Rakudo "nom" Branch Low Hanging Fruit

Want to help? Want something that (hopefully) doesn't require deep
guts skillz? Take a task from this list. Delete it from here when
you commit; maybe announce on #perl6 that you're working on it too.

Note - do not just copy stuff from the master setting! Often things
need to be done differently in the "nom" branch, to take advantage of
new possible performance.


## More Smart-matching
See S03-smartmatch tests, especially array-array.t, hash-hash.t and
so forth. The regex ones are probably also good candidates now.

## Nil assignment
my Int $b = 3;  $b = Nil;   
