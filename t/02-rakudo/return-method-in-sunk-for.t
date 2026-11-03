use Test;

# The .return method returns from the routine it is lexically in, the same as
# the `return` control. Its signature check must look at that routine too, not
# at an internal routine such as sink-all that a sunk loop interposes on the
# dynamic call stack.

plan 5;

sub sunk-for { for <a b c> { .return if $_ eq 'b' }; 'fell-through' }
is sunk-for(), 'b', '.return in a sunk for loop returns from the routine';

sub sunk-map { <a b c>.map({ .return if $_ eq 'b' }).sink; 'fell-through' }
is sunk-map(), 'b', '.return in a sunk .map(...).sink returns from the routine';

sub typed(--> Int) { 5.return }
is typed(), 5, '.return of a value matching the return type works';

throws-like { sub f(--> 42) { 27.return }; f }, X::AdHoc, message => /'return value 42'/,
    '.return still honors a literal return constraint';

throws-like { sub f(--> Int) { for <a b c> { .return if $_ eq 'b' } }; f },
    X::TypeCheck::Return,
    'a type-constrained .return in a sunk for reports the routine type-check error';

# vim: expandtab shiftwidth=4
