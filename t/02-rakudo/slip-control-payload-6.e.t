use v6.e.PREVIEW;
use Test;

# From 6.e a loop control statement may carry a payload, which the iterator
# unpacks through the same start-slip and slip-all that an ordinary block
# result goes through.  This needs its own file because the language
# version has to be set before anything else.

plan 5;

is-deeply (1..4).map({ next slip($_,-$_) if $_ %% 2; $_ }).List, (1,2,-2,3,4,-4),
  'next with a Slip payload contributes each of its elements';
is-deeply (1..4).map({ next Empty if $_ %% 2; $_ }).List, (1,3),
  'next with an Empty payload contributes no element';
is-deeply (1..3).map({ next Slip if $_ == 2; $_ }).List, (1, Slip, 3),
  'next with the Slip type object contributes it as a value';
is-deeply (1..4).map({ last slip(8,9) if $_ == 3; $_ }).List, (1,2,8,9),
  'last with a Slip payload contributes each of its elements';
is-deeply (1..100).map({ next slip($_,-$_) if $_ %% 2; $_ }).head(4).List, (1,2,-2,3),
  'next with a Slip payload flattens when pulled one value at a time';

# vim: expandtab shiftwidth=4
