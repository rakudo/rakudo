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

package EXPORT::macros {
    OUR::<EXPERIMENTAL-MACROS> := True;
}

package EXPORT::smallnatives {
    our native int1 is repr('P6int') is Int is nativesize( 1) { }
    our native int2 is repr('P6int') is Int is nativesize( 2) { }
    our native int4 is repr('P6int') is Int is nativesize( 4) { }
    our native uint1 is repr('P6int') is Int is nativesize( 1) is unsigned { }
    our native   bit is repr('P6int') is Int is nativesize( 1) is unsigned { }
    our native uint2 is repr('P6int') is Int is nativesize( 2) is unsigned { }
    our native uint4 is repr('P6int') is Int is nativesize( 4) is unsigned { }
}

package EXPORT::pack {
    OUR::<EXPERIMENTAL-PACK> := True;
}
package EXPORT::collation {
    OUR::<EXPERIMENTAL-COLLATION> := True;
}
