use v6.e.PREVIEW;
use Test;

# From 6.e a loop control statement may carry a payload, which the iterator
# unpacks through the same start-slip and slip-all that an ordinary block
# result goes through.  This needs its own file because the language
# version has to be set before anything else.

plan 12;

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

is-deeply (for ^7 -> $a, $b?, $c? { NEXT { }; next Empty if $a == 3; $a }).List, (0,6),
  'next with an Empty payload in a block taking several values with a phaser still runs the block for the final values';
is-deeply (for ^7 -> $a, $b?, $c? { NEXT { }; next [].Slip if $a == 3; $a }).List, (0,6),
  'next with an empty Slip payload in a block taking several values with a phaser still runs the block for the final values';
is-deeply (for ^9 -> $a, $b, $c { NEXT { }; next slip(7,8) if $a == 3; $a }).List, (0,7,8,6),
  'next with a Slip payload in a block taking several values with a phaser contributes each of its elements';
is-deeply (for ^9 -> $a, $b, $c { NEXT { }; next Slip if $a == 3; $a }).List, (0,Slip,6),
  'next with the Slip type object in a block taking several values with a phaser contributes it as a value';
is-deeply (for ^9 -> $a, $b, $c { NEXT { }; last slip(7,8) if $a == 3; $a }).List, (0,7,8),
  'last with a Slip payload in a block taking several values with a phaser contributes each of its elements';
is-deeply (for ^9 -> $a, $b, $c { NEXT { }; last 42 if $a == 3; $a }).List, (0,42),
  'last with a payload in a block taking several values with a phaser contributes it';
is-deeply (for ^9 -> $a, $b, $c { NEXT { }; last Empty if $a == 3; $a }).List, (0,),
  'last with an Empty payload in a block taking several values with a phaser contributes no element';

# vim: expandtab shiftwidth=4
