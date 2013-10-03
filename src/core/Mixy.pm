my role Mixy does Baggy  {  # should really be QuantHash, but that's for later

    method default(--> Real) { 0 }
    method total(--> Real) { [+] self.values }

    multi method gist(Mixy:D $ : --> Str) {
        my $name := self.^name;
        ( $name eq 'Mix' ?? 'mix' !! "$name.new" )
        ~ '('
#        ~ %!elems.values.map( { "{.key.gist}({.value})" } ).join(', ')
        ~ self.pairs.map( {
              .value == 1 ?? .key.gist !! "{.key.gist}({.value})"
          } ).join(', ')
        ~ ')';
    }

    method pick ($count = 1) {
        fail ".pick is not supported on a {.self.^name}";
    }

    method roll ($count = 1) {
        my $total  = self.total;
        my $rolls  = $count ~~ Num ?? $total min $count !! $count;
#        my @pairs := %!elems.values;
        my @pairs := self.pairs;

        map {
            my $rand = $total.rand;
            my $seen = 0;
            my $roll;
            for @pairs -> $pair {
                next if ( $seen += $pair.value ) <= $rand;

                $roll = $pair.key;
                last;
            }
            $roll;
        }, 1 .. $rolls;
    }
}
