use v6.e.PREVIEW;
use Test;

# The %s directive of the 6.e sprintf with type objects

plan 5;

is (quietly sprintf('%s', Str)), '', '%s of a Str type object';
is (quietly sprintf('%s', Int)), '', '%s of an Int type object';
is (quietly sprintf('%5s', Str)), '     ',
    'a type object still honors the width';
is (quietly sprintf('%.3s', Str)), '',
    'a type object still honors the precision';

my $warning = '';
{
    CONTROL { when CX::Warn { $warning = .message; .resume } }
    my $ = sprintf('%s', Str);
}
like $warning, /'uninitialized value'/, '%s of a type object warns';

# vim: expandtab shiftwidth=4
