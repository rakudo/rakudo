use Test;

plan 2;

# A $.meth(...) call parses into a wrapper node that delegates its
# compilation to an inner self.meth(...).item chain. A for statement
# modifier thunks the wrapper itself, so a lexical used as an argument
# crosses a frame boundary that the wrapper must report, or the lexical
# gets lowered to a frame-local the thunk cannot reach.

my class Collector {
    has @.seen;
    method add($value, $extra) { @!seen.push($value + $extra) }
    method create() {
        my $base = 10;
        $.add($base, $_) for 1, 2;
        $base
    }
}

my $c = Collector.new;
is $c.create, 10,
    'the method returns its lexical after the modifier loop';
is-deeply $c.seen, [11, 12],
    'each iteration passed the lexical to the self-call';
