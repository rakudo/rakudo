use Test;

plan 1;

# https://github.com/rakudo/rakudo/issues/1488
{ 
    my class Foo { 
        method Stringy { 
            die "missed optimization" 
        } 
    }; 
    my $f := Foo.new; 
    is-deeply $f cmp ($ = $f), Same, 'eqaddr optimization for cmp exists' 
}
