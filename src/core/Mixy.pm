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
        fail ".grab is not supported on a {self.^name}";
    }

    method pick ($count = 1) {
        fail ".pick is not supported on a {self.^name}";
    }

    method roll ($count = 1) {
        my $total = [+] self.values.grep: * > 0;
        my $rolls = $count ~~ Num
          ?? $total min $count !! $count ~~ Whatever ?? $Inf !! $count;
#        my @pairs := %!elems.values;
        my @pairs := self.pairs;

        sub roll-one ($ignore?){
            my $rand = $total.rand;
            my $seen = 0;
            my $roll;
            for @pairs -> $pair {
                return $pair.key if ( $seen += $pair.value ) > $rand;
            }
        }
        return roll-one if $rolls == 1;

        map &roll-one, 1 .. $rolls;
    }
}

# vim: ft=perl6 expandtab sw=4
