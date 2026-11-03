my $begin-closure;

role RoleMethodDroppedCode {
    method dead-if()       { if False { try 1 }; 'if' }
    method dead-nested()   { if False { -> { try 2 } }; 'nested' }
    method dead-else()     { if True { try 42 } else { try 4 } }
    method dead-unless()   { unless True { try 5 }; 'unless' }
    method dead-modifier() { (try 6) if False; 'modifier' }
    method dead-and()      { False and try 7; 'and' }
    method dead-sub()      { if False { sub inner() { 8 } }; 'sub' }
    method dead-begin()    { if False { BEGIN $begin-closure = -> { 'begin' } }; 'dead-begin' }
    method begin-closure() { $begin-closure() }
}
