use Test;

# A lone constant string argument reads as a str literal in the call
# analysis, so passing one to a routine that can only take a native int
# or num is refused at compile time. An assignment of an unfitting
# numeric literal to a native variable names the variable and its native
# type in the refusal.

plan 12;

sub compile-refuses($code, $expected, $desc) {
    my $error = '';
    {
        EVAL $code;
        CATCH { default { $error = .gist.Str } }
    }
    ok $error.contains($expected), $desc;
}

sub compiles($code, $desc) {
    my $error = '';
    {
        EVAL $code;
        CATCH { default { $error = .gist.Str } }
    }
    is $error, '', $desc;
}

compile-refuses 'sub f(int $x) { }; f("x")',
    'will never work',
    'a string argument to an int parameter is refused at compile time';

compile-refuses 'sub f(num $x) { }; f("x")',
    'will never work',
    'a string argument to a num parameter is refused at compile time';

compile-refuses 'sub f(Int $x) { }; f("x")',
    'will never work',
    'a string argument to an Int parameter is refused at compile time';

{
    sub f(str $x) { $x }
    is f("x"), 'x', 'a string argument to a str parameter still binds';
}

compiles 'sub f(int $x) { }; my $v = "2"; my &c = { f("$v") };',
    'an interpolated string argument is not refused at compile time';

compiles 'sub f(int $x) { }; my &c = { f(<x>) };',
    'a word-quoted argument with a val processor is not refused at compile time';

compile-refuses 'multi f(int $x) { }; multi f(num $x) { }; f("x")',
    'will never work',
    'a string argument to all-native multi candidates is refused at compile time';

{
    multi f(int $x) { 'native' }
    multi f(Str $x) { 'string' }
    is f("x"), 'string', 'a string argument to a mixed multi still picks the Str candidate';
}

compile-refuses 'my int $i; $i = 1.5',
    'variable ($i)',
    'assigning a Rat literal to a native int variable names the variable';

compile-refuses 'my int $i; $i = 1.5',
    'type int',
    'assigning a Rat literal to a native int variable names the native type';

compile-refuses 'my int $i; $i = 1.5',
    'type Real',
    'assigning a Rat literal to a native int variable suggests the Real type';

compile-refuses 'my int $i = 1.5',
    'native variable ($i)',
    'a Rat literal initializer on a native int declaration names the native variable';

# vim: expandtab shiftwidth=4
