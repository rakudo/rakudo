unit module RegistryUnit;

# A registry held in a unit lexical, filled by the modules that use it as
# they load.

my @in-order;

sub register(Str:D $name) is export { @in-order.push($name) }
sub registered() is export { @in-order }
