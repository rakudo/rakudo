use nqp;

package EXPORT::cached {
    multi sub trait_mod:<is>(Routine $r, :$cached!) {
        my %cache;
        nqp::bindattr_i($r, Routine, '$!onlystar', 0 )
          if $r.onlystar; # disable optimization
        $r.wrap(-> |c {
            my $key := c.gist;
            %cache.EXISTS-KEY($key)
              ?? %cache{$key}
              !! (%cache{$key} := callsame);
        });
    }

    multi sub trait_mod:<is>(Method $m, :$cached!) {
        X::NYI.new(:feature("'is cached' on methods")).throw;
    }

    OUR::{'&trait_mod:<is>'} := &trait_mod:<is>;
}
