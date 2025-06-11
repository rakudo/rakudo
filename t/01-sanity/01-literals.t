# check literals
say '1..24';

# testing if printing a literal works
print "ok ";
print 1;
print "\n";

# testing say with literal
print 'ok ';
say 2;

# testing to see if mulitple says can be used in a row
print "ok 3\n";
say 'ok 4';
say "ok 5";

# testing different types of literals i.e hex, bin, oct
say 'ok ', 0x6;
say 'ok ', 0b111;
say 'ok ', 0o10;
say 'ok ', 0d9;
say 'ok ', +"0x0a";
say 'ok ', '0b1010' + 1;
say 'ok ', '0o6' * '0b10';
say 'ok ', +'0d13';

# testing literals with underscores
say 'ok ', 0_0_1_4;
say 'ok ', 0x0000_000f;
say 'ok ', 0d16;
say 'ok ', 0b0001_0001;

# testing different escape characters in with different literals
say 'ok ', "\x31\x38";
say 'ok ', "1\x39";
say 'ok ', "\x32\o60";
say "\x023 test multiple escapes in string using diag output: \x31\x32\o63";
say "ok 21";

# testings literals with brackets
say 'ok ', +"\x[32]2";
say 'ok ', +"2\x[33]";
say 'ok ', +"\o[62,064]";


## TODO a lot more

# vim: expandtab shiftwidth=4
