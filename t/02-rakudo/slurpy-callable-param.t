use Test;

plan 4;

# A `*` slurpy on a `&`-sigil parameter is accepted and, like a `*$`, has no
# slurpy effect: it binds a single Callable, exactly as `&code` would.

sub with-star($t, *&code) { code($t) }
is with-star(21, * * 2), 42, 'a `*&` parameter binds and calls a single Callable';

my $param = &with-star.signature.params[1];
nok $param.slurpy, 'a `*&` parameter is not actually slurpy';
is $param.sigil, '&', 'a `*&` parameter keeps its callable sigil';

dies-ok { with-star(1, {;}, {;}) },
    'a `*&` parameter still takes a single argument, not many';
