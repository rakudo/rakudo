use v6;

use lib <core-libs/Test core-libs/NativeCall>;
use nqp;
use NQPHLL:from<NQP>;
use NativeCall;
use Test;

plan 1;
is 42, 42, 'symbol reexported by NativeCall used to cause trouble';

# vim: expandtab shiftwidth=4
