use Test;
plan 31;
my $r;

=begin pod
The seven suspects are:

=item  Happy
=item  Dopey
=item  Sleepy
=item  Bashful
=item  Sneezy
=item  Grumpy
=item  Keyser Soze
=end pod

$r = $=POD[0];
is $r.content.elems, 8;
for 1..7 {
    isa_ok $r.content[$_], Pod::Item;
}

is $r.content[1].content[0].content, 'Happy', 'content is happy :)';
is $r.content[2].content[0].content, 'Dopey';
is $r.content[7].content[0].content, 'Keyser Soze';
nok $r.content[4].level.defined, 'no level information';

=begin pod
=item1  Animal
=item2     Vertebrate
=item2     Invertebrate

=item1  Phase
=item2     Solid
=item2     Liquid
=item2     Gas
=item2     Chocolate
=end pod

$r = $=POD[1];
is $r.content.elems, 8;
for 0..7 {
    isa_ok $r.content[$_], Pod::Item;
}

$r.content[0].content[0].content, 'Animal';
$r.content[0].level,   1;
$r.content[2].content[0].content, 'Invertebrate';
$r.content[2].level,   2;
$r.content[3].content[0].content, 'Phase';
$r.content[3].level,   1;
$r.content[4].content[0].content, 'Solid';
$r.content[4].level,   2;

=begin pod
=comment CORRECT...
=begin item1
The choices are:
=end item1
=item2 Liberty
=item2 Death
=item2 Beer
=end pod

$r = $=POD[2];
is $r.content.elems, 5;
for 1..4 {
    isa_ok $r.content[$_], Pod::Item;
}

# XXX Those items are :numbered in S26, but we're waiting with block
# configuration until we're inside Rakudo, otherwise we'll have to
# pretty much implement Pair parsing in gsocmess only to get rid of
# it later.

=begin pod
Let's consider two common proverbs:

=begin item
I<The rain in Spain falls mainly on the plain.>

This is a common myth and an unconscionable slur on the Spanish
people, the majority of whom are extremely attractive.
=end item

=begin item
I<The early bird gets the worm.>

In deciding whether to become an early riser, it is worth
considering whether you would actually enjoy annelids
for breakfast.
=end item

As you can see, folk wisdom is often of dubious value.
=end pod

$r = $=POD[3];
is $r.content.elems, 4;
is $r.content[0].content, "Let's consider two common proverbs:";
ok $r.content[1].content[1].content
   ~~ /:s This is a common .+ are extremely attractive/;
ok $r.content[2].content[1].content
   ~~ /:s In deciding .+ annelids for breakfast/;
is $r.content[3].content, "As you can see, folk wisdom is often of dubious value.";
