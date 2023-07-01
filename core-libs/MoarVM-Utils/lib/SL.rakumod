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
    say "\n$SL.report(%*ENV<SL_REPORT> // 5)";
    $SL.exit;
}

# vim: expandtab shiftwidth=4
