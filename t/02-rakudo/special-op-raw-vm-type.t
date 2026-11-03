use Test;
use nqp;

plan 12;

# .WHAT and friends compile to primitive ops on the value itself. A raw
# VM array or hash must yield its own VM type object, the same one
# nqp::what returns, rather than being mapped to List or Hash.

{
    my $list := nqp::list();
    ok nqp::eqaddr($list.WHAT, nqp::what($list)),
        '.WHAT on a raw VM array is the VM array type object';
    is $list.WHAT.^name, 'BOOTArray',
        '.WHAT on a raw VM array names the BOOTArray type';
    ok nqp::eqaddr(nqp::what(nqp::create($list.WHAT)), $list.WHAT),
        'nqp::create of a raw VM array .WHAT yields a VM array';
    ok nqp::eqaddr($list.VAR, $list),
        '.VAR on a bound raw VM array is the array itself';
}

{
    my $hash := nqp::hash();
    ok nqp::eqaddr($hash.WHAT, nqp::what($hash)),
        '.WHAT on a raw VM hash is the VM hash type object';
    is $hash.WHAT.^name, 'BOOTHash',
        '.WHAT on a raw VM hash names the BOOTHash type';
    ok nqp::eqaddr(nqp::what(nqp::create($hash.WHAT)), $hash.WHAT),
        'nqp::create of a raw VM hash .WHAT yields a VM hash';
}

ok nqp::eqaddr(nqp::list().WHAT, nqp::what(nqp::list())),
    '.WHAT directly on an nqp::list() expression is the VM array type object';

{
    $_ := nqp::list();
    ok nqp::eqaddr(.WHAT, nqp::what($_)),
        '.WHAT as a topic call on a raw VM array is the VM array type object';
    ok nqp::eqaddr(.VAR, $_),
        '.VAR as a topic call on a bound raw VM array is the array itself';
}

# A real method call may return a foreign value, which must still be
# mapped into Raku land.
{
    class Foreign {
        method list() { nqp::list(1, 2) }
        method hash() { nqp::hash('a', 1) }
    }
    is Foreign.list.^name, 'List',
        'a method call returning a raw VM array is still mapped to List';
    is Foreign.hash.^name, 'Hash',
        'a method call returning a raw VM hash is still mapped to Hash';
}

# vim: expandtab shiftwidth=4
