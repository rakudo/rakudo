use v6;
use Test;

# Tests that NYI methods of IO::CatHandle throw

my @meths = <flush  nl-out  print  printf  print-nl  put  say  write>;
plan 2 + @meths;

throws-like { IO::CatHandle.new."$_"() }, X::NYI, $_ for @meths;

throws-like { IO::CatHandle.new.slurp-rest }, X::Obsolete,
    :old<slurp-rest>, :replacement<slurp>, :when('with IO::CatHandle'),
    '.slurp-rest';

is IO::CatHandle.new.Str, '<closed IO::CatHandle>', '.Str on closed handle';
