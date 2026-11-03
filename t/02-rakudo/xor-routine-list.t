use Test;

plan 6;

is-deeply &infix:<^^>(0, "", 0e0), 0e0, '&infix:<^^> returns the last of three false operands';
is-deeply &infix:<xor>(0, "", 0e0), 0e0, '&infix:<xor> returns the last of three false operands';
is &infix:<^^>(-> { 5 }, 0, 0), 5, '&infix:<^^> calls a Block first of three operands';
is &infix:<^^>(-> { 0 }, -> { 7 }, 0), 7, '&infix:<^^> calls each Block of three operands';
{
    sub e { Empty }
    my $x = 5;
    is ($x R^^ e() R^^ e()), 5, 'R^^ calls the thunk that Empty operands leave in front';
}
{
    sub e { Empty }
    my $x = 5;
    my $y = 0;
    is ($x Rxor $y Rxor e()), 5, 'Rxor finds its one true operand beside an Empty one';
}
