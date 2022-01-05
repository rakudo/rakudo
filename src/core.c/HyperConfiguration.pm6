my class X::Invalid::Value { ... }

# Configuration for hyper/race, controlling how we parallelize (number of
# items at a time, and number of threads).
my class HyperConfiguration {
    has int $.batch;
    has Int $.degree;

    submethod TWEAK(:$method) {
        X::Invalid::Value.new(:$method,:name<batch>,:value($!batch)).throw
          if $!batch <= 0;

        X::Invalid::Value.new(:$method,:name<degree>,:value($!degree)).throw
          if $!degree <= 0;
    }
}

# vim: expandtab shiftwidth=4
