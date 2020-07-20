use v6;

use lib <lib>;
use Test;

plan 8;

for '', ' <Perl5>' -> $ism {
    my $ran;
    lives-ok { EVAL("
      use isms$ism;
      sub abs() \{ \$ran = True };
      abs
    ") }, 'Can we run p5ish code without it complaining';
    ok $ran, "Did the code with 'use isms$ism' actually run";
}

for '', ' <C++>' -> $ism {
    my $ran;
    lives-ok { EVAL("
      use isms$ism;
      sub new(\\a,|c) \{ a.new(|c) }
      \$ran = new Str, :value<foo>;
    ") },
      'Can we run C++ish code without it complaining';
    is $ran, 'foo', "Did the code with 'use isms$ism' actually run";
}

# vim: expandtab shiftwidth=4
