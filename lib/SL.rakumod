# The sole purpose of this module is to make it easier to show
# a Spesh Log report for a given piece of Raku code to execute,
# be it with -e or by calling a script.
#
# Typical use is specifying it with -M on the command line, e.g.:
#
#  $ raku -MSL -e 'my @a = ^10; @a[3] = 42 for ^100000'
#
# This module is only installed on the MoarVM backend.  Trying to
# load this module on any other backend may or may not work.

use MoarVM::SL;

if SL() -> $SL {
    LEAVE $SL.exit;
    say "\n$SL.report()";
}

# vim: expandtab shiftwidth=4
