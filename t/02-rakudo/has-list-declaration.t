use nqp;
use Test;

# has (...) declares a list of attributes. The parenthesized form parses
# as a signature, so per-attribute traits, defaults, and the required
# marker arrive as parameter metadata, and the compiler moves them onto
# the attribute declarations. The legacy frontend drops that metadata
# silently, so those cases are skipped there.

my $rakuast := nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';

plan 18;

my $basic = EVAL 'class GH4522-Basic { has ($.a, @.b, %.c) }; GH4522-Basic.new';
nok $basic.a.defined,
    'a scalar attribute declared in a has list starts undefined';
isa-ok $basic.b, Array,
    'an @-sigil attribute declared in a has list gets an Array container';
isa-ok $basic.c, Hash,
    'a %-sigil attribute declared in a has list gets a Hash container';

throws-like 'class GH4522-Assign { has ($.a) = 1 }',
    X::Comp,
    message => /"Cannot assign to a list of 'has' scoped declarations"/,
    'assigning to a whole has list is a compile time error';

if $rakuast {
    is EVAL('class GH4522-Default { has ($.a = 42, $.b) }; GH4522-Default.new.a'), 42,
        'a default inside a has list is applied to its attribute';

    is EVAL('class GH4522-Chain { has ($.x = 3, $.y = $!x + 1) }; GH4522-Chain.new.y'), 4,
        'a default inside a has list can read an earlier attribute';

    is EVAL('class GH4522-Self { has ($.x = 3, $.y = self.x + 2) }; GH4522-Self.new.y'), 5,
        'a default inside a has list can call methods on self';

    my $required = EVAL 'class GH4522-Bang { has ($.n!, $.rest) }; GH4522-Bang';
    throws-like { $required.new }, X::Attribute::Required,
        'the ! marker inside a has list makes its attribute required';
    is $required.new(:n(1)).n, 1,
        'a ! marked attribute in a has list accepts a supplied value';

    throws-like { EVAL('class GH4522-Req { has ($.a is required) }; GH4522-Req').new },
        X::Attribute::Required,
        'is required inside a has list is enforced at construction';

    my $rw = EVAL 'class GH4522-Rw { has ($.c is rw, $.b) }; GH4522-Rw.new';
    $rw.c = 5;
    is $rw.c, 5,
        'is rw inside a has list produces a writable accessor';

    my $typed = EVAL 'class GH4522-Typed { has (Int $.g = 5, $.rest) }; GH4522-Typed';
    is $typed.new.g, 5,
        'a typed attribute in a has list keeps its default';
    throws-like { $typed.new(g => "x") }, X::TypeCheck::Assignment,
        'a typed attribute in a has list rejects a wrongly typed value';

    throws-like 'class GH4522-Bogus { has ($.a is totally-made-up) }',
        X::Comp::Trait::Unknown,
        message => /'attribute declaration'/,
        'an unknown trait inside a has list is a compile time error';

    is EVAL('role GH4522-Role { has ($.x, $.y = 2) }; (class GH4522-Does does GH4522-Role {}).new.y'), 2,
        'a has list inside a role body declares attributes with defaults';

    is EVAL('class GH4522-Order { has ($.a = 1, $.b!) }; GH4522-Order.new(:b(2)).a'), 1,
        'an attribute with a default may precede a required one in a has list';

    is EVAL('class GH4522-Private { has ($!p = 7, $.q); method p { $!p } }; GH4522-Private.new.p'), 7,
        'a private attribute in a has list keeps its default';

    is EVAL('class GH4522-Sub { submethod b { has ($.s = 3) } }; GH4522-Sub.new.s'), 3,
        'a has list inside a submethod body declares attributes';
}
else {
    skip 'has list attribute traits and defaults are NYI on the legacy frontend', 14;
}

# vim: expandtab shiftwidth=4
