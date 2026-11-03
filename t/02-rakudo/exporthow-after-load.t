use lib <t/packages/02-rakudo/lib>;
use Test;

plan 2;

# A unit that declares a my scoped EXPORTHOW leaks an EXPORTHOW entry into
# its GLOBALish. Loading such a unit imports its GLOBALish, and installing
# that entry as a lexical hands any later my scoped EXPORTHOW declaration in
# the loading unit to the declaration merge machinery, which mutates the
# setting's declarator table and loses the declaration. The consumer of the
# loading unit then fails to parse the declarator at all.

{
    use ExporthowDeclarer;
    my declarer-class A { }
    ok A.HOW ~~ Metamodel::ClassHOW,
        'a declarator from a directly used module declares a class';
}

{
    use ExporthowAfterLoad;
    my after-load-class B { }
    ok B.HOW ~~ Metamodel::ClassHOW,
        'a declarator declared after loading another module that carries an EXPORTHOW survives';
}

# vim: expandtab shiftwidth=4
