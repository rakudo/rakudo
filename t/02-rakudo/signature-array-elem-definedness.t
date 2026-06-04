use Test;

plan 2;

# `SomeType:D @arr` as a parameter declares the element type as the
# bare base type for container parameterisation and enforces `:D`
# separately, matching the long-standing legacy behaviour. A default
# whose container is `Array[SomeType]` should bind, because role
# parameterisation is invariant and `Array[Str]` does not `do`
# `Positional[Str:D]`.

my Str @chars = 'a', 'b';

sub with-default(Str:D :@alpha = @chars) { @alpha }
is with-default().elems, 2,
    'Str:D :@alpha = Array[Str] default binds';

is &with-default.signature.params[0].type.^name, 'Positional[Str]',
    'Str:D @alpha parameter stores Positional[Str], not Positional[Str:D]';

# vim: expandtab shiftwidth=4
