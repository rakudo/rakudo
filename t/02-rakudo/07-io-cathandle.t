use v6;
use Test;

# Tests that NYI methods of IO::CatHandle throw

my @meths = <flush  print  printf  print-nl  put  say  write>;
plan +@meths;

throws-like { IO::CatHandle.new."$_"() }, X::NYI, $_ for @meths;
