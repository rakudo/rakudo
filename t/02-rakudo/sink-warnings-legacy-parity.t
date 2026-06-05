use Test;

plan 2;

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

# vim: expandtab shiftwidth=4
