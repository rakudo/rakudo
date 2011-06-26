# Rakudo "nom" Branch Low Hanging Fruit

Want to help? Want something that (hopefully) doesn't require deep
guts skillz? Take a task from this list. Delete it from here when
you commit; maybe announce on #perl6 that you're working on it too.

Note - do not just copy stuff from the master setting! Often things
need to be done differently in the "nom" branch, to take advantage of
new possible performance.

## Basic IO
the stat calls should be pretty straight-forward to port from master

## Nil assignment
my Int $b = 3;  $b = Nil;   
