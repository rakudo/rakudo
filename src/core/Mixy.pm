my role Mixy does Baggy  {
    method default(--> Real) { 0 }
    method total(--> Real) { [+] self.values }

    method new-fp(*@pairs --> Mixy) {
        my %e;
        for @pairs {
            when Pair {
                (%e{$_.key.WHICH} //= ($_.key => 0)).value += $_.value;
            }
            default {
                (%e{$_.WHICH} //= ($_ => 0)).value++;
            }
        }
        for %e -> $p {
            %e.delete_key($p.key) if $p.value.value == 0;
        }
        self.bless(:elems(%e));
    }

    multi method gist(Mixy:D $ : --> Str) {
        my $name := self.^name;
        ( $name eq 'Mix' ?? 'mix' !! "$name.new" )
        ~ '('
#        ~ %!elems.values.map( {
        ~ self.pairs.map( {
              .value == 1 ?? .key.gist !! "{.key.gist}({.value})"
          } ).join(', ')
        ~ ')';
    }

    method grab ($count = 1) {
        fail ".grab is not supported on a {.self.^name}";
    }

    method pick ($count = 1) {
        fail ".pick is not supported on a {.self.^name}";
    }

    method roll ($count = 1) {
        my $total  = [+] self.values.grep: * > 0;
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
