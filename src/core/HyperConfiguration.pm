# Configuration for hyper/race, controlling how we parallelize (number of
# items at a time, and number of threads).
my class HyperConfiguration {
    has int $.batch;
    has Int $.degree;
}

# vim: ft=perl6 expandtab sw=4
