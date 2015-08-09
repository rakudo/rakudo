# Configuration for hyper/race, controlling how we parallelize. Not a class
# end users can expect to work with unless they're doing truly special
# things.
my class HyperConfiguration {
    has Bool $.race;
    has int $.batch;
    has Int $.degree;
}

# vim: ft=perl6 expandtab sw=4
