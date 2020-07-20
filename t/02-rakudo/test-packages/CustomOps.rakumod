unit module CustomOps;

# These custom operators are for testing functionality of cmp-ok
# in t/02-rakudo/03-cmp-ok.t

sub infix:«<=!»    is export { $^a < $^b }
sub infix:<«>      is export { $^a < $^b }
sub infix:['<=»']  is export { $^a < $^b }
sub infix:['<«']   is export { $^a < $^b }
sub infix:['>»']   is export { $^a < $^b }
sub infix:['<«>»'] is export { $^a < $^b }

# vim: expandtab shiftwidth=4
