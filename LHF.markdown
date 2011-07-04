# Rakudo "nom" Branch Low Hanging Fruit

Want to help? Want something that (hopefully) doesn't require deep
guts skillz? Take a task from this list. Delete it from here when
you commit; maybe announce on #perl6 that you're working on it too.

Note - do not just copy stuff from the master setting! Often things
need to be done differently in the "nom" branch, to take advantage of
new possible performance.

## Basic IO
the stat calls should be pretty straight-forward to port from master

also: IO::ArgFiles, $*ARGFILES, get() and lines()

## Nil assignment
my Int $b = 3;  $b = Nil;   

## Built-in functions
Often builtins are only added as methods, even when the corresponding function
should exist. Go through src/core/ and write functions that re-dispatch to the
methods where necessary

## Str.ords

Implement Str.ords and the sub form (returns a list of codepoints)
