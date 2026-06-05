use Test;

plan 5;

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

# vim: expandtab shiftwidth=4
