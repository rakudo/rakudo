use v6;
use Test;

# Tests that NYI methods of IO::CatHandle throw

my @meths = <
    flush  out-buffer  print  printf  print-nl  put  say  write
>;  # nl-out
plan 2 + @meths;

throws-like { IO::CatHandle.new."$_"() }, X::NYI, $_ for @meths;

throws-like { IO::CatHandle.new.slurp-rest }, X::Obsolete,
    :old<slurp-rest>, :replacement<slurp>, :when('with IO::CatHandle'),
    '.slurp-rest';

is IO::CatHandle.new.Str, '<closed IO::CatHandle>', '.Str on closed handle';
