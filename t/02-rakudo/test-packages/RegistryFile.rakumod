# The same registry held in the mainline of a file without a package
# declaration.

my @in-order;

sub register-in-file(Str:D $name) is export { @in-order.push($name) }
sub registered-in-file() is export { @in-order }
