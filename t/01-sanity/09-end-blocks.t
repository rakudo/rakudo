use v6;

say "1..2";

#
# $was_in_second_end_block is a package variable, not a lexical one, per S04:
#
# Some closures produce C<Code> objects at compile time that cannot be
# cloned, because they're not attached to any runtime code that can
# actually clone them.  C<BEGIN>, C<CHECK>, C<INIT>, and C<END> blocks
# fall into this category...  It's only safe to refer to package
# variables and file-scoped lexicals from such a routine.
#

our $was_in_second_end_block = 0;

END {
    if $was_in_second_end_block {
        say "ok 2";
    } else {
        say "not ok 2";
    }
}

END {
    $was_in_second_end_block = 1;
    say "ok 1";
}
