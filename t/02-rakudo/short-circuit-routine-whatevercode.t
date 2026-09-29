use Test;

plan 18;

is &infix:<&&>(1, *.succ)(41), 42, '&infix:<&&> returns a WhateverCode second operand';
is &infix:<||>(0, *.succ)(41), 42, '&infix:<||> returns a WhateverCode second operand';
is &infix:<^^>(0, *.succ)(41), 42, '&infix:<^^> returns a WhateverCode second operand';
is &infix:<^^>(0, *.succ, 0)(41), 42, '&infix:<^^> returns a WhateverCode among three operands';
is &infix:<//>(Any, *.succ)(41), 42, '&infix:<//> returns a WhateverCode second operand';
is &infix:<and>(1, *.succ)(41), 42, '&infix:<and> returns a WhateverCode second operand';
is &infix:<or>(0, *.succ)(41), 42, '&infix:<or> returns a WhateverCode second operand';
is &infix:<xor>(0, *.succ)(41), 42, '&infix:<xor> returns a WhateverCode second operand';
is &infix:<&&>(1, 1, *.succ)(41), 42, '&infix:<&&> returns a WhateverCode among three operands';
is &infix:<||>(0, 0, *.succ)(41), 42, '&infix:<||> returns a WhateverCode among three operands';
is &infix:<//>(Any, Any, *.succ)(41), 42, '&infix:<//> returns a WhateverCode among three operands';
is &infix:<&&>(1, -> { 5 }), 5, '&infix:<&&> calls a Block second operand';

{
    my @values = 1, *.succ;
    is ([&&] @values)(41), 42, '[&&] of a list returns a WhateverCode element';
}
{
    my @values = 0, *.succ;
    is ([||] @values)(41), 42, '[||] of a list returns a WhateverCode element';
}
{
    my @values = Any, *.succ;
    is ([//] @values)(41), 42, '[//] of a list returns a WhateverCode element';
}
{
    my @values = 0, *.succ;
    is ([^^] @values)(41), 42, '[^^] of a list returns a WhateverCode element';
}
{
    my @results = (0, 0) Z^^ (*.succ, *.pred);
    is @results[0](41), 42, 'Z^^ returns the first WhateverCode element';
    is @results[1](41), 40, 'Z^^ returns the second WhateverCode element';
}
