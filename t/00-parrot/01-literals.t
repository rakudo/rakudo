#!./parrot

# check literals

use v6;

say '1..23';


print "ok ";
print 1;
print "\n";

print 'ok ';
say 2;

print "ok 3\n";
say 'ok 4';
say "ok 5";

say 'ok ', 0x6;
say 'ok ', 0b111;
say 'ok ', 0o10;
say 'ok ', 0d9;
say 'ok ', +"0x0a";
say 'ok ', '0b1010' + 1;
say 'ok ', '0o6' * '0b10';
say 'ok ', +'0d13';

say 'ok ', 0_0_1_4;
say 'ok ', 0x0000_000f;
say 'ok ', 0d16;
say 'ok ', 0b0001_0001;

say 'ok ', "\x31\x38";
say 'ok ', "1\x39";
say 'ok ', "\d50\o60";
say "ok 21 \x023 \o163k\d105\x70";

say 'ok ', +"\x[32]2";
say 'ok ', +"2\d[51]";
#say 'ok ', +"\o[62,065]";


# TODO a lot more
