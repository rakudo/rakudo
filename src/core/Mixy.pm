my role Mixy does Baggy  {

    method PAIR(\key) { Pair.new(key, my Real $ = 0 ) }
    method SANITY(%elems --> Nil) {
        for %elems -> $p {
            %elems.DELETE-KEY($p.key) if $p.value.value == 0;
        }
    }

    method default(--> Real) { 0 }
    method total(--> Real) { [+] self.values }

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
        my @pairs = self.pairs.grep: *.value > 0;
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

        map &roll-one, 1 .. $rolls;
    }
}

# vim: ft=perl6 expandtab sw=4
