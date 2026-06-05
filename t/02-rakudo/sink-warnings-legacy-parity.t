use Test;

plan 8;

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

# vim: expandtab shiftwidth=4
