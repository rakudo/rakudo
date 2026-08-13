use lib <t/packages/Test-Helpers>;
use Test::Helpers::QAST;
use Test;
use QAST:from<NQP>;
use nqp;
plan 63;

# An increment, decrement, or compound assignment on a native int or
# num attribute lowers to the raw step op on the attribute's ref, the
# same lowering a native lexical gets. The routine call stays for
# anything the lexical form also declines: an Int literal step, a
# postfix on a narrower width, a boxed attribute, or a redefined
# operator. A role body declines every step, since the optimize pass
# does not reach it. The shapes are this frontend's.

if nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    qast-is 'my class C { has int $!x; method b() { $!x++; Nil } }', :full, -> \v {
        qast-contains-op(v, 'add_i')
        and not qast-contains-op(v, 'sub_i')
        and not qast-contains-call(v, '&postfix:<++>')
    }, 'a sunk postfix increment of an int attribute steps with no reverse';

    qast-is 'my class C { has int $!x; method b() { ++$!x } }', :full, -> \v {
        qast-contains-op(v, 'add_i')
        and not qast-contains-call(v, '&prefix:<++>')
    }, 'a prefix increment of an int attribute steps through the raw op';

    qast-is 'my class C { has int $!x; method b() { $!x--; Nil } }', :full, -> \v {
        qast-contains-op(v, 'sub_i')
        and not qast-contains-op(v, 'add_i')
        and not qast-contains-call(v, '&postfix:<-->')
    }, 'a sunk postfix decrement of an int attribute steps with no reverse';

    qast-is 'my class C { has int $!x; method b() { my $v = $!x++; $v } }', :full, -> \v {
        qast-contains-op(v, 'add_i')
        and qast-contains-op(v, 'sub_i')
        and not qast-contains-call(v, '&postfix:<++>')
    }, 'a used postfix increment of an int attribute reverses the step for the original';

    qast-is 'my class C { has int $!x; method b() { my int $v = 3; $!x += $v } }', :full, -> \v {
        qast-contains-op(v, 'add_i')
        and not qast-contains-call(v, '&METAOP_ASSIGN')
    }, 'a compound add of a native lexical to an int attribute steps through the raw op';

    qast-is 'my class C { has int $!x; has int $!y; method b() { $!x += $!y } }', :full, -> \v {
        qast-contains-op(v, 'add_i')
        and not qast-contains-call(v, '&METAOP_ASSIGN')
    }, 'a compound add of an int attribute to an int attribute steps through the raw op';

    qast-is 'my class C { has int $!x; method b() { my int $v = 3; $!x *= $v } }', :full, -> \v {
        qast-contains-op(v, 'mul_i')
        and not qast-contains-call(v, '&METAOP_ASSIGN')
    }, 'a compound multiply on an int attribute steps through the raw op';

    qast-is 'my class C { has num $!n; method b() { $!n -= 2e0 } }', :full, -> \v {
        qast-contains-op(v, 'sub_n')
        and not qast-contains-call(v, '&METAOP_ASSIGN')
    }, 'a compound subtract of a num literal from a num attribute steps through the raw op';

    qast-is 'my class C { has int $!x; method b() { my int $v = 3; $v += $!x } }', :full, -> \v {
        qast-contains-op(v, 'add_i')
        and not qast-contains-call(v, '&METAOP_ASSIGN')
    }, 'an int attribute serves as the step value for a native lexical';

    qast-is 'my class C { has int $!s; method b() { $!s += 1 } }', :full, -> \v {
        qast-contains-call(v, '&METAOP_ASSIGN')
    }, 'a compound add of an Int literal keeps the metaop';

    qast-is 'my class C { has int8 $!t; method b() { my $v = $!t++; $v } }', :full, -> \v {
        qast-contains-call(v, '&postfix:<++>')
    }, 'a postfix increment of a sized int attribute keeps the routine';

    qast-is 'my class C { has Int $!b = 0; method b() { $!b++ } }', :full, -> \v {
        qast-contains-call(v, '&postfix:<++>')
    }, 'a postfix increment of a boxed attribute keeps the routine';

    qast-is 'my class C { has int $!x; method b() { sub postfix:<++>($a is rw) { $a = 9 }; $!x++ } }', :full, -> \v {
        not qast-contains-op(v, 'add_i')
    }, 'a redefined operator in scope keeps the call';

    qast-is 'my class C { has num $!n; method b() { my $v = $!n++; $v } }', :full, -> \v {
        qast-contains-op(v, 'add_n')
        and qast-contains-op(v, 'sub_n')
        and not qast-contains-call(v, '&postfix:<++>')
    }, 'a used postfix increment of a num attribute reverses the step for the original';

    qast-is 'my class C { has num $!n; method b() { --$!n } }', :full, -> \v {
        qast-contains-op(v, 'sub_n')
        and not qast-contains-call(v, '&prefix:<-->')
    }, 'a prefix decrement of a num attribute steps through the raw op';

    qast-is 'my class C { has num $!n; method b() { my num $v = 1e0; $v += $!n; $v } }', :full, -> \v {
        qast-contains-op(v, 'add_n')
        and not qast-contains-call(v, '&METAOP_ASSIGN')
    }, 'a num attribute serves as the step value for a native num lexical';

    qast-is 'my class C { has int $!x; method b() { my $c = { $!x++ }; $c() } }', :full, -> \v {
        qast-contains-op(v, 'add_i')
        and not qast-contains-call(v, '&postfix:<++>')
    }, 'an attribute step inside a nested block steps through the raw op';

    qast-is 'my class C { has uint $!u; method b() { $!u++ } }', :full, -> \v {
        qast-contains-call(v, '&postfix:<++>')
        and not qast-contains-op(v, 'add_i')
    }, 'a postfix increment of a uint attribute keeps the routine';

    qast-is 'my role R[::T] { has T $!x; method b() { $!x++ } }', :full, -> \v {
        qast-contains-call(v, '&postfix:<++>')
        and not qast-contains-op(v, 'add_i')
    }, 'a step in a role body keeps the routine, as the optimize pass does not reach it';
}
else {
    skip 'the step shapes are specific to the RakuAST frontend', 19;
}

# The lowered forms yield the values the operator calls yield.

{
    my class C {
        has int  $!i = 10;
        has int  $!j = 2;
        has num  $!n = 2e0;
        has int8 $!t = 100;
        has Int  $!b = 5;
        method postfix-used()  { my $v = $!i++; $v }
        method i()             { $!i }
        method prefix()        { ++$!i }
        method postfix-dec()   { $!i--; $!i }
        method prefix-dec()    { --$!i }
        method add(int $v)     { $!i += $v; $!i }
        method sub(int $v)     { $!i -= $v; $!i }
        method mul(int $v)     { $!i *= $v; $!i }
        method add-attr()      { $!i += $!j; $!i }
        method into-lex()      { my int $v = 100; $v += $!j; $v }
        method num-add()       { $!n += 1.5e0; $!n }
        method num-mul(num $w) { $!n *= $w; $!n }
        method t-postfix()     { $!t++; $!t }
        method t-prefix()      { ++$!t }
        method boxed()         { $!b++; $!b }
        method int-literal()   { $!i += 1; $!i }
    }
    my $c = C.new;
    is $c.postfix-used, 10, 'a used postfix increment yields the original value';
    is $c.i, 11, 'the used postfix increment stored the stepped value';
    is $c.prefix, 12, 'a prefix increment yields the stepped value';
    is $c.postfix-dec, 11, 'a sunk postfix decrement stores the stepped value';
    is $c.prefix-dec, 10, 'a prefix decrement yields the stepped value';
    is $c.add(5), 15, 'a compound add of a native lexical stores the sum';
    is $c.sub(3), 12, 'a compound subtract of a native lexical stores the difference';
    is $c.mul(4), 48, 'a compound multiply of a native lexical stores the product';
    is $c.add-attr, 50, 'a compound add of an attribute steps by the attribute value';
    is $c.into-lex, 102, 'an attribute serves as the step value for a lexical';
    is $c.num-add, 3.5e0, 'a compound add of a num literal stores the sum';
    is $c.num-mul(0.5e0), 1.75e0, 'a compound multiply of a num lexical stores the product';
    is $c.t-postfix, 101, 'a sized int postfix increment computes through the routine';
    is $c.t-prefix, 102, 'a sized int prefix increment yields the stepped value';
    is $c.boxed, 6, 'a boxed attribute increment computes through the routine';
    is $c.int-literal, 51, 'a compound add of an Int literal computes through the metaop';
}

{
    my class W {
        has int8 $!t = 127;
        has num32 $!f = 1e0;
        method wrap()   { ++$!t }
        method stored() { ++$!t; $!t }
        method trunc()  { $!f += 0.1e0; $!f }
    }
    is W.new.wrap, 128, 'a sized int prefix increment yields the unstored step result';
    is W.new.stored, -128, 'a sized int prefix increment wraps in storage';
    is W.new.trunc, 1.100000023841858e0, 'a sized num compound add keeps the narrow precision';
}

{
    my role R {
        has int $!x = 3;
        method work() { $!x++; $!x += $!x; $!x }
    }
    my class C does R {}
    is C.new.work, 8, 'attribute steps in a composed role method compute correctly';
}

{
    my class C {
        has int $!x;
        method redef() {
            sub postfix:<++>($a is rw) { $a = 99 }
            $!x++;
            $!x
        }
    }
    is C.new.redef, 99, 'a redefined operator in scope still runs';
}

{
    my class C {
        has int $!x;
        method bump() { $!x++ }
        method v()    { $!x }
    }
    my $a = C.new;
    my $b = C.new;
    $a.bump; $a.bump; $b.bump;
    is $a.v, 2, 'steps on one object write that object';
    is $b.v, 1, 'steps on one object leave the other object alone';
}

{
    my class NI {
        has num $!n = 1.5e0;
        method used-postfix() { my $v = $!n++; $v }
        method n()            { $!n }
        method prefix()       { ++$!n }
        method postfix-dec()  { $!n--; $!n }
        method prefix-dec()   { --$!n }
    }
    my $ni = NI.new;
    is $ni.used-postfix, 1.5e0, 'a used num postfix increment yields the original value';
    is $ni.n, 2.5e0, 'the used num postfix increment stored the stepped value';
    is $ni.prefix, 3.5e0, 'a num prefix increment yields the stepped value';
    is $ni.postfix-dec, 2.5e0, 'a sunk num postfix decrement stores the stepped value';
    is $ni.prefix-dec, 1.5e0, 'a num prefix decrement yields the stepped value';
}

{
    my class NB {
        has int $!x;
        method closure() { my $c = { $!x++ }; $c(); $c(); $!x }
        method loop()    { my int $s = 2; $!x += $s for ^3; $!x }
    }
    is NB.new.closure, 2, 'an attribute step in a closure writes through the captured self';
    is NB.new.loop, 6, 'an attribute compound add in a for body accumulates across iterations';
}

{
    my class U {
        has uint  $!u = 5;
        has uint8 $!w = 255;
        method up()   { $!u++; $!u }
        method wrap() { $!w++; $!w }
    }
    my $u = U.new;
    is $u.up, 6, 'a uint attribute increment computes through the routine';
    is $u.wrap, 0, 'a uint8 attribute increment wraps at the unsigned boundary';
}

{
    my class PubRO {
        has int $.x = 5;
        method step() { my $v = $.x++; "$v $!x" }
    }
    is PubRO.new.step, '5 5', 'a postfix increment through a plain public accessor leaves the attribute alone';
    my class PubRW {
        has int $.x is rw = 5;
        method step() { my $v = $.x++; "$v $!x" }
        method raw()  { $!x++; self.x }
    }
    my $pr = PubRW.new;
    is $pr.step, '5 6', 'a postfix increment through an is rw accessor stores the stepped value';
    is $pr.raw, 7, 'the accessor reads a raw private step of an is rw attribute';
}

{
    my class TW {
        has int $!x = 5;
        submethod TWEAK() { $!x++; $!x *= 2 }
        method v() { $!x }
    }
    is TW.new.v, 12, 'attribute steps in TWEAK see the default and store the result';
}

{
    my class BigN {
        has num $!n = 9007199254740992e0;
        method used() { my $v = $!n++; $v }
        method n()    { $!n }
    }
    my $bn = BigN.new;
    is $bn.used, 9007199254740991e0, 'a used num postfix past the integer precision yields the reverse stepped value';
    is $bn.n, 9007199254740992e0, 'the num postfix past the integer precision stores the rounded step';
}

# A narrower target stores the truncated step while the expression
# yields the full step result.

{
    my class N {
        has int8  $!t = 127;
        has int32 $!w = 2147483647;
        has num32 $!f = 1e0;
        method add-yield() { my int8 $o = 1; $!t += $o }
        method t()         { $!t }
        method pre-yield() { ++$!w }
        method w()         { $!w }
        method num-yield() { $!f += 0.1e0 }
        method f()         { $!f }
    }
    my $n = N.new;
    is $n.add-yield, 128, 'an int8 compound add yields the unstored step result';
    is $n.t, -128, 'the int8 compound add wraps in storage';
    is $n.pre-yield, 2147483648, 'an int32 prefix increment yields the unstored step result';
    is $n.w, -2147483648, 'the int32 prefix increment wraps in storage';
    is $n.num-yield, 1.1e0, 'a num32 compound add yields the unrounded step result';
    is $n.f, 1.100000023841858e0, 'the num32 compound add stores the narrow precision';
}

# vim: expandtab shiftwidth=4
