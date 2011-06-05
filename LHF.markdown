# Rakudo "nom" Branch Low Hanging Fruit

Want to help? Want something that (hopefully) doesn't require deep
guts skillz? Take a task from this list. Delete it from here when
you commit; maybe announce on #perl6 that you're working on it too.

Note - do not just copy stuff from the master setting! Often things
need to be done differently in the "nom" branch, to take advantage of
new possible performance.

## Basic string methods in Cool.pm
See examples already there. Should be easy to add .uc, .lc, .bytes...

## More basic math ops
See t/00-parrot/02-op-math.t and operators.pm. Add enough to make that
test file pass (just Int multi variants of the ops).
