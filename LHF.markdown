# Rakudo "nom" Branch Low Hanging Fruit

Want to help? Want something that (hopefully) doesn't require deep
guts skillz? Take a task from this list. Delete it from here when
you commit; maybe announce on #perl6 that you're working on it too.

Note - do not just copy stuff from the master setting! Often things
need to be done differently in the "nom" branch, to take advantage of
new possible performance.

## Basic string methods in Cool.pm
See examples already there. Should be easy to add .bytes, .ord, ...

## Int.Num, various Num multi candidates
Implement coercion from Int to Num by writing Num method. Probably
like pir::perl6_box_num__PN(pir::repr_unbox_int__IP(self)) in the
body. The write the Num variants for +, -, *, /, **, and numeric
comparsion operators. You can use the coercion from Int to test them
as we don't have Num literals yet.
