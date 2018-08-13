my class X::Invalid::Value { ... }

# Configuration for hyper/race, controlling how we parallelize (number of
# items at a time, and number of threads).
my class HyperConfiguration {
    has int $.batch;
    has Int $.degree;

    submethod TWEAK(:$method) {
        nqp::if(
          $!batch <= 0,
          X::Invalid::Value.new(:$method,:name<batch>,:value($!batch)).throw,
          nqp::if(
            $!degree <= 0,
            X::Invalid::Value.new(:$method,:name<degree>,:value($!degree)).throw,
            Nil
          )
        )
    }
}

# vim: ft=perl6 expandtab sw=4
