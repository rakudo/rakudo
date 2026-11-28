use Test;

plan 1;

ok $*RAKU.compiler.id ~~ /^ <[0..9 A..F]> ** 40 $/,
  'RakuAST frontend compiler id is a 40 digit hex digest';

# vim: expandtab shiftwidth=4
