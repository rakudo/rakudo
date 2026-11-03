use Test;

# A list that is all Int, or all Str, sorts through a comparison built from
# a single VM op instead of the general infix:<cmp> dispatch.  Every case
# below pins a value or an ordering that specialization has to reproduce,
# not the specialization itself.
#
# Classifying a list only pays for itself on a list long enough to sort for
# a while, so it is only done above a length, and a case built from three
# or four elements would never reach the code it means to test.  The two
# helpers append values whose keys sort after everything being asserted on,
# so a case can be written at the length that reads well and still reach
# the classifying path.  The first element is classified apart from the
# rest, so the cases where position could change the answer are written
# with their disqualifying element in more than one place.

plan 63;

sub enough-int(*@head) { (|@head, |(4000 .. 4020)) }
sub enough-str(*@head) { (|@head, |("zzz00" .. "zzz20")) }

# --- lists that qualify -------------------------------------------------

is-deeply enough-int(5,3,9,1,7).sort.head(5).List, (1,3,5,7,9),
  'a list of Int sorts ascending';
is-deeply enough-str(<pear apple fig cherry>).sort.head(4).List,
  <apple cherry fig pear>.List,
  'a list of Str sorts by codepoint';
is-deeply (my @a = enough-int(5,3,9,1,7)).sort.head(5).List, (1,3,5,7,9),
  'an Array of Int sorts through its element containers';
is-deeply (|(2**70, 5, 2**80, 1), |(2**90 .. 2**90 + 20)).sort.head(4).List,
  (1, 5, 2**70, 2**80),
  'Int elements too large for a machine word keep their order';
is-deeply enough-int(-3, 0, -7, 4).sort.head(4).List, (-7, -3, 0, 4),
  'negative Int elements sort below zero';
is-deeply enough-str("b", "", "a").sort.head(3).List, ("", "a", "b"),
  'an empty Str sorts before every other Str';
is-deeply enough-str("Z", "a", "A").sort.head(3).List, ("A", "Z", "a"),
  'Str sorting is by codepoint, not case-insensitive';
is-deeply (%(<a b c d e f g h i j k l m n o p q r s t>.map({ $_ => 1 })).keys.sort.List),
  <a b c d e f g h i j k l m n o p q r s t>.List,
  'the keys of a hash sort as Str';
is-deeply ("c\na\nb\n" ~ ("zzz00" .. "zzz20").join("\n")).lines.sort.head(3).List,
  ("a","b","c"),
  'the lines of a string sort as Str';

# --- lists long enough to merge at more than a few widths ---------------
# The element merges are hand written copies of one skeleton and the key
# merge shares one loop, so drive both wide enough that a slip in a run
# bound shows up, and check them against a two argument comparator, which
# is a different merge entirely.

my @wide-int = (^400).map({ ($_ * 2654435761) % 997 });
is-deeply @wide-int.sort.List, @wide-int.sort(-> $a, $b { $a cmp $b }).List,
  'four hundred Int sort as a two argument comparator sorts them';
my @wide-str = (^400).map({ "k" ~ ($_ * 2654435761) % 997 });
is-deeply @wide-str.sort.List, @wide-str.sort(-> $a, $b { $a cmp $b }).List,
  'four hundred Str sort as a two argument comparator sorts them';
is-deeply @wide-int.sort(*.Str).List,
          @wide-int.sort(-> $a, $b { $a.Str cmp $b.Str }).List,
  'four hundred Str keys order as a two argument comparator orders them';
is-deeply @wide-str.sort(*.chars).List,
          @wide-str.sort(-> $a, $b { $a.chars cmp $b.chars }).List,
  'four hundred Int keys order as a two argument comparator orders them';

# --- the fast comparison must stay stable -------------------------------
# Equal Int and Str elements are indistinguishable by value, so stability
# of the element merges is only visible through object identity.  Bind the
# elements rather than assigning them: assigning these variables would put
# each value behind its own Scalar, and =:= would then compare containers.

my $int-first := Int.new(1);
my $int-later := Int.new(1);
my $ints := enough-int($int-first, $int-later, Int.new(0)).sort.List;
ok $ints[1] =:= $int-first, 'the earlier of two equal Int elements stays first';
ok $ints[2] =:= $int-later, 'the later of two equal Int elements stays second';

my $str-first := Str.new(value => "m");
my $str-later := Str.new(value => "m");
my $strs := enough-str($str-first, $str-later, Str.new(value => "a")).sort.List;
ok $strs[1] =:= $str-first, 'the earlier of two equal Str elements stays first';
ok $strs[2] =:= $str-later, 'the later of two equal Str elements stays second';

is-deeply enough-int(3,1,3,1,2).sort.head(5).List, (1,1,2,3,3),
  'duplicate Int elements sort without losing any of them';
is-deeply enough-str(<bb aa cc dd>).sort(*.chars).head(4).List, <bb aa cc dd>.List,
  'equal extracted keys keep the input order of their elements';

# --- element types that must NOT take the fast comparison ---------------
# An Allomorph is the case that would give a wrong answer rather than a
# crash: it is both a number and a string, and its infix:<cmp> candidate
# breaks a numeric tie on the string, where nqp::cmp_I calls the pair
# equal.

is-deeply enough-int(IntStr.new(1,"b"), IntStr.new(1,"a"), IntStr.new(1,"c"))
    .sort.head(3).map(*.Str).List,
  ("a","b","c"), 'IntStr elements that tie numerically order by their string';
is-deeply enough-int(IntStr.new(10,"10"), IntStr.new(9,"9"), IntStr.new(100,"100"))
    .sort.head(3).map(*.Int).List,
  (9, 10, 100), 'IntStr among plain Int sort by numeric value, not by string';
is-deeply enough-int(RatStr.new(1/2,"0.5"), RatStr.new(1/4,"0.25"),
                     RatStr.new(1/8,"0.125")).sort.head(3).map(*.Rat).List,
  (0.125, 0.25, 0.5), 'RatStr among plain Int sort by numeric value, not by string';
is-deeply enough-int(IntStr.new(3,"z"), 1, 2).sort.head(3).List,
  (1, 2, IntStr.new(3,"z")),
  'an IntStr among plain Int sorts by its numeric value';

my @proxied;
for ^20 -> $i {
    my $value = (37 * $i) % 20;
    @proxied[$i] := Proxy.new(
      FETCH => method () { $value }, STORE => method ($x) {});
}
is-deeply @proxied.sort.List, (^20).List,
  'a list of Proxy elements sorts by the value each one fetches';

# The comparison the fast merges use would take the first read for the
# type of every later one, so a container that answers with a different
# type the second time has to keep them out.
my @flip;
for ^20 -> $i {
    my $value = (37 * $i) % 20;
    my $reads = 0;
    @flip[$i] := Proxy.new(
      FETCH => method () { $reads++ ?? "s$value" !! $value },
      STORE => method ($x) {});
}
my $flipped;
lives-ok { $flipped = @flip.sort.List },
  'a container answering with a different type on a later read still sorts';
is $flipped.elems, 20, 'and it keeps every element';

is-deeply enough-int(1, 2, 1.5).sort.head(3).List, (1, 1.5, 2),
  'a Rat after the first element still sorts by numeric value';
is-deeply enough-int(1, 2, IntStr.new(5,"b"), IntStr.new(5,"a"))
    .sort.head(4).map(*.Str).List, ("1","2","a","b"),
  'an IntStr later in a list of Int still breaks a numeric tie on its string';

# Padding a case with Int is what refuses it when the head is an Allomorph,
# so one list has to be nothing but Allomorphs for the first element's own
# type test to be the thing that decides.
my @all-allomorph = (^20).map({ IntStr.new($_ div 2, ("b","a")[$_ % 2] ~ $_) });
is-deeply @all-allomorph.sort.map(*.Str).head(6).List,
  ("a1","b0","a3","b2","a5","b4"),
  'a list that is all IntStr breaks each numeric tie on the string';
is-deeply enough-int(1.5, 2, 3).sort.head(3).List, (1.5, 2, 3),
  'a Rat in the first position still sorts by numeric value';
quietly is enough-int(1, 2, Int).sort.head(3).map({ .defined ?? .Str !! .^name }).join(","),
  "Int,1,2",
  'an Int type object after the first element sorts through the general comparison';
quietly is enough-int(Int, 2, 1).sort.head(3).map({ .defined ?? .Str !! .^name }).join(","),
  "Int,1,2",
  'an Int type object in the first position sorts through the general comparison';

my @holed = enough-int(1, 2); @holed[30] = 3;
dies-ok { @holed.sort.List },
  'a list with a hole does not reach a comparison that would accept it';
my @front-hole; @front-hole[1] = 2; @front-hole[$_] = $_ for 3 .. 20;
dies-ok { @front-hole.sort.List },
  'a hole in the first position does not reach a comparison that would accept it';

my class MyInt is Int { }
is enough-int(1, 2, MyInt.new(3)).sort.head(3).join(","), "1,2,3",
  'a subclass of Int sorts by value alongside plain Int';
my class MyStr is Str { }
is enough-str("a", "c", MyStr.new(value => "b")).sort.head(3).join(","), "a,b,c",
  'a subclass of Str sorts alongside plain Str';

is-deeply enough-int(3e0, 1e0, 2e0).sort.head(3).List, (1e0, 2e0, 3e0),
  'Num among plain Int sort by numeric value';
is-deeply (3e0, NaN, 1e0, 2e0).sort.List, (1e0, 2e0, 3e0, NaN),
  'NaN sorts after every ordinary Num';
my @lists = (1,0),(0,1),(1,1);
@lists.push((2,$_)) for ^21;
is-deeply @lists.sort.head(3).List, ($(0,1), $(1,0), $(1,1)),
  'a list of Lists still sorts through the general comparison';

# --- around the length at which classifying starts ----------------------
# One length below the gate, the gate itself, where the run widths divide
# the list exactly, and one above, which is the shortest ragged case.

for 15, 16, 17 -> $n {
    my @i = (^$n).map({ ($_ * 37) % 11 });
    my @s = @i.map({ "k" ~ $_ });
    is-deeply @i.sort.List, @i.sort(-> $a, $b { $a cmp $b }).List,
      "$n Int sort as a two argument comparator sorts them";
    is-deeply @s.sort.List, @s.sort(-> $a, $b { $a cmp $b }).List,
      "$n Str sort as a two argument comparator sorts them";
}
is-deeply (|(1 .. 15), 0.5).sort.head(2).List, (0.5, 1),
  'a disqualifying element last in the shortest classified list is still seen';
is-deeply (^20).map({ (17 * $_) % 20 }).List.sort(*.self, :k).head(5).List,
  (0, 13, 6, 19, 12),
  'the :k form reports indices when the keys took the fast comparison';

# --- sizes below the point where a merge sort starts --------------------

is-deeply ().sort.List, (), 'an empty list sorts to an empty list';
is-deeply (1,).sort.List, (1,), 'a one element list sorts to itself';
is-deeply (2,1).sort.List, (1,2), 'a two element list of Int swaps';
is-deeply (1,2).sort.List, (1,2), 'a sorted two element list is left alone';
is-deeply <b a>.sort.List, <a b>.List, 'a two element list of Str swaps';
is-deeply (5,3,9,1,7).sort.List, (1,3,5,7,9),
  'a list too short to classify sorts through the general comparison';
is-deeply <pear apple fig>.sort.List, <apple fig pear>.List,
  'a short list of Str sorts through the general comparison';

# --- the key extracting form --------------------------------------------

is-deeply enough-str(<bbb a cc dddd>).sort(*.chars).head(4).List,
  <a cc bbb dddd>.List,
  'an Int key extractor orders by the extracted key';
is-deeply enough-int(3,1,2).sort(*.Str).head(3).List, (1,2,3),
  'a Str key extractor orders by the extracted key';
is-deeply enough-str("a","b","c").sort({ $_ eq "c" ?? 3 !! $_ }).head(3).List,
  ("c", "a", "b"),
  'a key that is not a Str after the first orders through the general comparison';
is-deeply enough-int(1,2,3).sort({
    $_ >= 4000 ?? "zzz" !! $_ %% 2 ?? "x" !! 9
}).head(3).List, (1,3,2),
  'keys of mixed types order through the general comparison';

# A key inside a container is refused outright, because the mapper runs
# over the whole list before any comparison and a later call can still
# reach back and change a key that was already classified.
my @src = flat (3,1,2), (10 .. 30);
is-deeply (^24).sort({ @src[$_] }).head(3).List, (1,2,0),
  'a key extractor returning a container orders by the contained value';
my @boxes := Array.new; @boxes[$_] = 0 for ^24;
is-deeply (^24).sort({
    @boxes[$_] = 100 - $_; @boxes[0] = "zz" if $_ == 23; @boxes[$_]
}).head(3).List, (23, 22, 21),
  'a mapper that changes an already extracted key still orders by cmp';

# An undefined key is the shape .sort(*.attr) produces whenever the
# attribute was never assigned.
class Aged { has Int $.age }
my @aged = flat (Aged.new(age => 3), Aged.new, Aged.new(age => 1)),
                (10 .. 30).map({ Aged.new(age => $_) });
quietly is-deeply @aged.sort(*.age).head(3).map({ .age.defined ?? .age !! 0 }).List,
  (0, 1, 3),
  'an undefined Int key orders through the general comparison';
quietly is-deeply enough-str("bb","a","ccc").sort({ .chars == 1 ?? Str !! $_ }).head(3).List,
  ("a","bb","ccc"),
  'an undefined Str key orders through the general comparison';
is-deeply <c a b>.sort(*.self, :k).List, (1,2,0),
  'the :k form reports the indices the sorted elements came from';

# vim: expandtab shiftwidth=4
