use v6;

use Test;

plan 9;

ok !$Test::todo_next_test, '$Test::todo_next_test set to False intially';

if ($?COMPILER and $?COMPILER eq 'Pugs') {
    todo :pugs;  # never unTODO this.
    my $saved_val = $Test::todo_next_test;
    ok 0, "this test should be TODO'd";
    ok $saved_val, 'todo() sets $Test::todo_next_test to True';
    ok !$Test::todo_next_test, 'todo() only affects the next one test';

    todo :pugs('9999' ~ $?VERSION);  # never unTODO this.
    $saved_val = $Test::todo_next_test;
    ok 0, "this test should be TODO'd";
    ok $saved_val, 'todo() sets $Test::todo_next_test to True';
    ok !$Test::todo_next_test, 'todo() only affects the next one test';

    todo :pugs('-10.' ~ $?VERSION);  # never unTODO this.
    $saved_val = $Test::todo_next_test;
    ok 1, "this test should not be TODO'd";
    #warn ">>> $saved_val\n";
    ok !$saved_val, "todo() didn't set \$Test::todo_next_test to True";
} else {
    skip 9, 'no general tests';
}
