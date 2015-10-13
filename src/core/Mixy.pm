my role Mixy does Baggy  {

    method PAIR(\key,\value) { Pair.new(key, my Real $ = value ) }
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

    multi method kxxv(Mixy:D:) {
        fail ".kxxv is not supported on a {self.^name}";
    }

    multi method grab(Mixy:D: $count?) {
        fail ".grab is not supported on a {self.^name}";
    }

    multi method pick(Mixy:D: $count?) {
        fail ".pick is not supported on a {self.^name}";
    }
}

# vim: ft=perl6 expandtab sw=4
