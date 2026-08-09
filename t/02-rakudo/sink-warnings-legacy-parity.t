use Test;

plan 23;

sub stderr-of(Str $code) {
    run($*EXECUTABLE.absolute, '-e', $code, :err).err.slurp(:close)
}

sub useless-lines(Str $err) {
    $err.lines.map(*.trim).grep(*.starts-with('Useless use of '))
}

sub useless-of(Str $err, Str $subject) {
    useless-lines($err).any.starts-with("Useless use of $subject ")
}

# `1, 2, 3;` at statement level produces a worry for each constant
# operand. Legacy stops there; RakuAST used to add a fourth worry
# whose subject was the bare `,` operator. Both compilers must keep
# warning about the operands themselves.

my $comma-err = stderr-of 'sub f { 1, 2, 3; 42 }; f()';
nok useless-of($comma-err, ','),
    '`,` is not a useless-use subject for `1, 2, 3;`';
ok  useless-of($comma-err, 'constant integer 2'),
    'the constant integer operand of `1, 2, 3;` is still a useless-use subject';

# The yada-yada stubs `...`, `???` and `!!!` compile to `&fail`,
# `&warn` and `&die` and have visible side effects when reached, so a
# stubbed method body like `method foo(--> Nil) { ... }` is not a
# useless expression in sink context. None of the three stubs should
# show up as a useless-use subject.

for <... ??? !!!> -> $yada {
    my $err = stderr-of qq[role R \{ method foo(--> Nil) \{ $yada \} \}];
    nok useless-of($err, $yada),
        "`$yada` is not a useless-use subject for a stubbed method body";
}

# `rand` is an impure call, `*` is the Whatever singleton, and `**`
# is the HyperWhatever singleton. None of them are useless values in
# sink context: `rand` has side effects and the two singletons carry
# meaning even when their result is discarded. Legacy stays silent on
# all three at statement level.

for 'rand', '*', '**' -> $term {
    my $err = stderr-of "sub f \{ $term; 42 \}; f()";
    nok useless-of($err, $term),
        "`$term` is not a useless-use subject at statement level";
}

# An operator is useless in sink context only when its routine carries the
# `is pure` trait, matching the legacy frontend.

# `temp` and `let` localize a variable and restore it when the scope leaves,
# so they have an effect even though their result is discarded.
nok useless-lines(stderr-of 'my $x = 1; sub f { temp $x; 42 }; f()').elems,
    '`temp` is not a useless-use subject in sink context';

nok useless-lines(stderr-of 'my $x = 1; sub f { let $x; 42 }; f()').elems,
    '`let` is not a useless-use subject in sink context';

nok useless-lines(
        stderr-of 'sub prefix:<sidef>($x) { $x }; my $y = 1; sub f { sidef $y; 42 }; f()'
    ).elems,
    'a user-defined prefix without `is pure` is not a useless-use subject';

nok useless-lines(
        stderr-of 'sub infix:<sidef>($a, $b) { $a }; my $y = 1; sub f { $y sidef $y; 42 }; f()'
    ).elems,
    'a user-defined infix without `is pure` is not a useless-use subject';

ok useless-lines(
        stderr-of 'sub prefix:<puref>($x) is pure { $x }; my $y = 1; sub f { puref $y; 42 }; f()'
    ).elems,
    'a user-defined prefix declared `is pure` is a useless-use subject';

ok useless-lines(
        stderr-of 'sub infix:<puref>($a, $b) is pure { $a }; my $y = 1; sub f { $y puref $y; 42 }; f()'
    ).elems,
    'a user-defined infix declared `is pure` is a useless-use subject';

# `{*}` performs the proto dispatch to its candidates, so it has an effect
# even in a non-tail (sunk) position in a proto body. Legacy never warns.

nok useless-of(
        stderr-of('class C { proto method m($x) { {*}; 42 }; multi method m(Int $x) { $x } }; C.m(1)'),
        '{*}'
    ),
    '`{*}` is not a useless-use subject in a non-tail proto body';

# An assignment carried by a zip, cross or reverse meta operator still
# assigns when its result is discarded, so `Z=`, `X=` and `R=` are not
# useless in sink context. Their operands are consumed by the meta
# operator rather than being discarded, so they are not subjects either.
# Legacy stays silent on all three.

nok useless-lines(stderr-of 'my @a = 1,2; my @b = 3,4; sub f { @a Z= @b; 42 }; f()').elems,
    '`Z=` produces no useless-use subjects in sink context';

nok useless-lines(stderr-of 'my @a = 1,2; sub f { @a X= 9; 42 }; f()').elems,
    '`X=` produces no useless-use subjects in sink context';

nok useless-lines(stderr-of 'my $x = 1; my $y = 2; sub f { $x R= $y; 42 }; f()').elems,
    '`R=` produces no useless-use subjects in sink context';

# A meta operator wrapping a pure operator stays a useless use, and only
# the operator itself is the subject.

my $zip-err = stderr-of 'my @a = 1,2; my @b = 3,4; sub f { @a Z+ @b; 42 }; f()';
ok useless-of($zip-err, 'Z+'),
    '`Z+` of a pure operator is still a useless-use subject in sink context';
nok useless-of($zip-err, '@a'),
    'the operands of a sunk `Z+` are not useless-use subjects';

# An assignment carried by a meta operator must still run when sunk,
# including iterating the lazy sequence a zip or cross produces.

{
    my @a = 1,2,3;
    my @b = 4,5,6;
    @a Z= @b;
    is-deeply @a, [4,5,6], 'sunk `Z=` assigned zipwise';

    my @c = 1,2;
    @c X= 9;
    is-deeply @c, [9,9], 'sunk `X=` assigned each element';

    my $x = 0;
    my $y = 1;
    $x R= $y;
    is $y, 0, 'sunk `R=` assigned to its right operand';
}

# vim: expandtab shiftwidth=4
