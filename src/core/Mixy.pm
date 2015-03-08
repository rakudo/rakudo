my role Mixy does Baggy  {
    method default(--> Real) { 0 }
    method total(--> Real) { [+] self.values }

    method new-from-pairs(*@pairs --> Mixy) {
        my %e;
        for @pairs {
            when Pair {
                (%e.AT-KEY($_.key.WHICH) //= ($_.key => 0)).value += $_.value;
            }
            default {
                (%e.AT-KEY($_.WHICH) //= ($_ => 0)).value++;
            }
        }
        for %e -> $p {
            %e.DELETE-KEY($p.key) if $p.value.value == 0;
        }
        nqp::create(self).BUILD(:elems(%e));
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

    multi method grab(Mixy:D: $count?) {
        fail ".grab is not supported on a {self.^name}";
    }

    multi method pick(Mixy:D: $count?) {
        fail ".pick is not supported on a {self.^name}";
    }

    multi method roll($count = 1) {
        my @pairs := self.pairs.grep: *.value > 0;
        my $total := [+] @pairs.map: *.value;
        my $rolls = nqp::istype($count,Num)
          ?? $total min $count !! nqp::istype($count,Whatever) ?? Inf !! $count;

        sub roll-one ($ignore?){
            my $rand = $total.rand;
            my $seen = 0;
            for @pairs -> $pair {
                return $pair.key if ( $seen += $pair.value ) > $rand;
            }
        }
        return roll-one if $rolls == 1;

        map &roll-one, 1 .. $rolls;
    }
}

# vim: ft=perl6 expandtab sw=4
