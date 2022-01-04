# Rakudo Use Of Roast

Rakudo is validating its implementation of Raku by running tests from the Raku
roast test suite. The process is called 'spectesting' and most of the time is
initiated by `make spectest` command invoked in Rakudo root directory.

The Raku roast suite repository is checked out into `t/spec/` under Rakudo build
directory. This is where more information about the roast itself is to be found.

## Test Modes

Spectesting could be done in one of three modes:

1. Normal. Done by `spectest` target of Makefile.
2. Quick. Done by `quicktest` target.
3. Stress. Done by `stresstest` target.

More details could be found in Rakudo Makefile itself.

## Tools For Testing

### `tools/update-passing-test-data.pl`

This is a verification tool to control what tests are not included into the
roast `spectest.data` file and their status. It tries to run all test files
which are not included into `spectest.data` and reports back the status of each
run.

Useful for finding tests worth including into regular spectesting. Sometimes it
might make sense including failing tests too if they're properly fudged. See
`t/spec/README.md` for more detailed description of `fudge` preprocessor.

### `tools/autounfudge.pl`

Helps in managing fudging directives in the roast test files. More details can
be found in the script file itself or with `perldoc tools/autounfudge.pl`.

