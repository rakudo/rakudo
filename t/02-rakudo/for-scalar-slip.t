use Test;

plan 8;

# A `for` over a source held in a container iterates it as a single item,
# except a Slip, which flattens even from a container. RakuAST used to treat a
# scalar-held Slip as one item.

my $slip = slip(2, 19, 7);

my @got;
@got.push($_) for $slip;
is-deeply @got, [2, 19, 7], 'a statement-modifier for flattens a scalar-held Slip';

my @block;
for $slip { @block.push($_) }
is-deeply @block, [2, 19, 7], 'a block for flattens a scalar-held Slip';

my $sum = 0;
$sum += $_ for $slip;
is $sum, 28, 'the topic is bound to each flattened element';

# An ordinary value in a container is still one item.
my $int = 42;
my @one;
@one.push($_) for $int;
is-deeply @one, [42], 'a scalar-held plain value is one item';

# An itemized list is one item (only a Slip flattens from a container).
my $list = (1, 2, 3);
my @item;
@item.push($_) for $list;
is-deeply @item, [(1, 2, 3),], 'a scalar-held list is one item';

# A bare Slip and a plain list still iterate their elements.
my @bare;
@bare.push($_) for slip(2, 19, 7);
is-deeply @bare, [2, 19, 7], 'a bare Slip iterates its elements';

my @plain;
@plain.push($_) for (1, 2, 3);
is-deeply @plain, [1, 2, 3], 'a plain list iterates its elements';

# An empty Slip in a container iterates nothing.
my $empty = slip();
my $count = 0;
$count++ for $empty;
is $count, 0, 'an empty scalar-held Slip iterates nothing';

# vim: expandtab shiftwidth=4
