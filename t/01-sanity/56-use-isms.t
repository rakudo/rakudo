use v6;

use lib <lib>;
use Test;

plan 2;

my $ran;
lives-ok { EVAL('use isms <Perl5>; sub abs() { $ran = True }; abs') },
  'Can we run p5ish code without it complaining';
ok $ran, 'Did the code actually run';
