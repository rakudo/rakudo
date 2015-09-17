use Test;

print "1..0 # Skip: Release pending\n";
exit 0;

plan 1;

use NativeCall;

class Foo is repr("CStruct") {
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


