use lib <t/packages/Test-Helpers>;
use Test;
use Test::Helpers;

plan 2;

# A parameter's `#=` declarator doc is shown inline in its routine's rendered
# signature. It must not also appear as a standalone block (a parameter is not
# a class), so the doc text shows exactly once under --doc.

is-run '#| the sub
sub thing(Str $foo #= the foo doc
) { }',
    'a parameter declarator doc renders once under --doc',
    :compiler-args['-Ilib', '--doc'],
    :out({ .comb('the foo doc').elems == 1 });

is-run 'class C {
    #| a method
    method m(Int $n #= the n param
    ) { }
}',
    'a method parameter declarator doc renders once under --doc',
    :compiler-args['-Ilib', '--doc'],
    :out({ .comb('the n param').elems == 1 });

# vim: expandtab shiftwidth=4
