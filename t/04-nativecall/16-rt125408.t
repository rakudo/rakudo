use v6;

use lib <lib>;
use NativeCall;
use Test;

plan 1;

class Foo {
   method foo(Mu:U $type, Int $a ) {
      my @buff := CArray[$type].new;
      my $ctype = $type ~~ Num ?? Num !! Int;
      @buff[$_] = $ctype(0) for ^(10 * $a);
   }
}

lives-ok {
   for ^10 {
      for (int16, int32,num64, num32 ) -> $type {
         my $foo = Foo.new;
         $foo.foo($type, 10.rand.Int);
      }
   }
}, "RT#125408 stayed fixed";


