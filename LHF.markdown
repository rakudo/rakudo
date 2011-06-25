# Rakudo "nom" Branch Low Hanging Fruit

Want to help? Want something that (hopefully) doesn't require deep
guts skillz? Take a task from this list. Delete it from here when
you commit; maybe announce on #perl6 that you're working on it too.

Note - do not just copy stuff from the master setting! Often things
need to be done differently in the "nom" branch, to take advantage of
new possible performance.

## Basic IO
While the iterator-y bits can't be done yet, adding back open and some
of the IO class should be do-able.

## Built-ins
Str.chop

## Nil assignment
my Int $b = 3;  $b = Nil;   
