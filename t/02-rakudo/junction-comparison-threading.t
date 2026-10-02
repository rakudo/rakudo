use Test;
use nqp;

plan 178;

unless nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    skip-rest 'junction threading in a boolean comparison needs the RakuAST frontend';
    exit;
}

sub warnings-of(&code) {
    my @warnings;
    {
        code();
        CONTROL { when CX::Warn { @warnings.push: .message; .resume } }
    }
    @warnings
}

# A comparison against a junction in boolean position autothreads a
# junction on the other side the same as in any other position.
my $mixed = (True, False, True, False);
my $j     = all(1, 2);
my \sj    = all(1, 2);
sub returns-junction { all(1, 2) }

is (so $mixed.all == True|False), True,
    'an all junction of mixed Bools equals True|False';
is (so $j == 1|2), True,
    'an all junction in a variable threads before the any junction';
is (so sj == 1|2), True,
    'an all junction in a sigilless variable threads before the any junction';
is (so returns-junction() == 1|2), True,
    'an all junction returned from a call threads before the any junction';
is (so 1|2 == $j), True,
    'an all junction on the right threads before the any junction on the left';
is (so all('a', 'b') eq 'a'|'b'), True,
    'an all junction threads first through string equality';
is (so none(1, 2) == 1&3), True,
    'a none junction on the left threads before the all junction';
is (so one(1, 2) == 1|2), False,
    'a one junction on the left threads before the any junction';
is (do if $j == 1|2 { 'taken' } else { 'skipped' }), 'taken',
    'an if condition threads the all junction first';
is (do { my $r = 'skipped'; unless $j == 1|2 { $r = 'taken' }; $r }), 'skipped',
    'an unless condition threads the all junction first';
is (not $j == 1|2), False,
    'a not threads the all junction first';

my constant ANY-JUNCTION = 1|2;
my constant ONE-JUNCTION = one(1, 2);
is (so $j == ANY-JUNCTION), True,
    'an all junction threads before a constant any junction';
is (so ONE-JUNCTION == 1|2), False,
    'a constant one junction threads before the any junction';

my $calls = 0;
sub two { $calls++; 2 }
is (so $j == 1|two()), True,
    'an all junction threads before an any junction with a computed eigenstate';
is (so two()|1 == $j), True,
    'an all junction threads before a computed any junction on its left';

my $junction-calls = 0;
sub counted-junction { $junction-calls++; all(1, 2) }
is (so counted-junction() == 1|2), True,
    'a junction from a call threads first';
is $junction-calls, 1, 'the call producing the junction runs once';

# Plain operands compare against each eigenstate.
my $two   = 2;
my $three = 3;
my int $native = 2;
my Int $typed  = 2;
my str $letter = 'b';
my $num = 2e0;
is (so $two == 1|2), True,    'a value equal to one eigenstate';
is (so $three == 1|2), False, 'a value equal to no eigenstate';
is (so $two == 1&2), False,   'a value not equal to every eigenstate';
is (so $native == 1|2), True, 'a native value equal to one eigenstate';
is (so $typed == 1|2), True,  'a typed variable equal to one eigenstate';
is (so $letter eq 'a'|'b'), True, 'a native string equal to one eigenstate';
is (so $num == 1e0|2e0), True, 'a Num equal to one Num eigenstate';
is (so $two == 1|2.0), True,  'a value equal to a Rat eigenstate';

# Every operand and eigenstate evaluates once, in source order.
my $one = 1;
$calls = 0;
is (so $one == 1|two()), True, 'a comparison matching its first eigenstate';
is $calls, 1, 'the eigenstate after the match still evaluates';
$calls = 0;
is (so $native == 2|two()), True, 'a native comparison matching its first eigenstate';
is $calls, 1, 'the eigenstate after the match still evaluates for a native operand';

my $order = '';
sub first-operand  { $order ~= 'a'; 1 }
sub second-operand { $order ~= 'b'; 2 }
sub third-operand  { $order ~= 'c'; 1 }
is (so first-operand()|2 == third-operand()), True,
    'a comparison with the junction on the left';
is $order, 'ac', 'the junction on the left evaluates before the right operand';
$order = '';
is (so third-operand() == first-operand()|2), True,
    'a comparison with the junction on the right';
is $order, 'ca', 'the left operand evaluates before the junction on the right';
$order = '';
is (so $one == first-operand()|second-operand()), True,
    'a comparison against two computed eigenstates';
is $order, 'ab', 'both computed eigenstates evaluate in source order';
$order = '';
is (so first-operand()|second-operand() == third-operand()), True,
    'two computed eigenstates on the left';
is $order, 'abc', 'the computed eigenstates evaluate before the right operand';

# An eigenstate takes its value when the junction is built.
my $changing = 1;
is (so $changing|3 == $changing++), True,
    'an eigenstate keeps its value when the right operand changes it';
my int $native-changing = 1;
is (so $native-changing|3 == $native-changing++), True,
    'a native eigenstate keeps its value when the right operand changes it';
my $assigned = 1;
is (so $assigned == 1&($assigned = 5)), False,
    'the operand is read after an eigenstate assigns to it';
$assigned = 1;
is (so $assigned == $assigned&($assigned = 5)), True,
    'an eigenstate takes its value once every eigenstate has evaluated';
my $reassigned = 'a';
is (so $reassigned eq 'a'&($reassigned = 'z')), False,
    'a string operand is read after an eigenstate assigns to it';
my @cells = 1, 2;
is (so @cells[0] == 1&(@cells[0] = 5)), False,
    'an array element operand is read after an eigenstate assigns to it';
$assigned = 1;
is (so $assigned|3 == ($assigned = 5)), False,
    'an eigenstate keeps its value when the right operand assigns to it';

# A Slip among the eigenstates flattens as it does in a junction.
my $flag = False;
my $zero = 0;
is (so $zero == 1|(2 if $flag)), False,
    'an empty Slip eigenstate is no eigenstate at all';
is (do if $zero == 1|(2 if $flag) { 'taken' } else { 'skipped' }), 'skipped',
    'an if condition with an empty Slip eigenstate';
my $seven = 7;
is (so $seven == 7&(2 if $flag)), True,
    'an all junction with an empty Slip eigenstate';

# An undefined value warns for every eigenstate compared, naming a variable.
my $undefined;
my @warnings = warnings-of({ so $undefined == 1&2 });
is @warnings.elems, 2, 'an undefined operand warns once per eigenstate';
is @warnings.grep(*.contains('$undefined')).elems, 2,
    'each warning for an undefined operand names the variable';
is warnings-of({ so Any == 1&2 }).elems, 2,
    'an undefined constant operand warns once per eigenstate';
is warnings-of({ so $one == 1|$undefined }).elems, 1,
    'an undefined eigenstate after the match still warns';
is warnings-of({ so $one == 1|Any }).elems, 1,
    'an undefined constant eigenstate after the match still warns';

# A comparison that dies or fails behaves as it does in a junction.
throws-like { so $one == 1|Mu }, X::Multi::NoMatch,
    'an eigenstate no candidate accepts still throws after the match';
my class Dies { method Numeric { die 'boom' } }
throws-like { so $one == 1|Dies.new }, X::AdHoc,
    'an eigenstate whose coercion dies still throws after the match';
my $coercions = 0;
my class Counts { method Numeric { $coercions++; 5 } }
my $counts = Counts.new;
my $counted = so $counts == 5|6;
is $coercions, 2, 'an operand coerces once per eigenstate';
is (try {
    use fatal;
    my $text = 'abc';
    if $two == 1|$text { 'taken' } else { 'skipped' }
}) // 'died', 'skipped', 'a Failure among the eigenstates stays in the junction under fatal';

# A block taking the condition receives the junction.
my @received;
if $one == 1|2 -> $v { @received.push: $v }
if $one == 3|4 { } else -> $v { @received.push: $v }
unless $one == 3|4 -> $v { @received.push: $v }
my $loops = 0;
while $loops == 0|1 -> $v { @received.push: $v; $loops = 5 }
if $one == 1|2 { @received.push: $^v }
is @received.elems, 5, 'every block taking the condition runs';
is @received[0].^name, 'Junction', 'an if block receives the junction';
is @received[1].^name, 'Junction', 'an else block receives the junction';
is @received[2].^name, 'Junction', 'an unless block receives the junction';
is @received[3].^name, 'Junction', 'a while block receives the junction';
is @received[4].^name, 'Junction', 'a block with a placeholder receives the junction';

# A Proxy is fetched as often as the comparison against the built junction.
my $fetches = 0;
my $proxy := Proxy.new(FETCH => { $fetches++; 2 }, STORE => -> $, $ { });
sub fetches-of(&code) { $fetches = 0; code(); $fetches }
is (so $proxy == 1|2), True, 'a Proxy operand compares by its fetched value';
is fetches-of({ so $proxy == 1|2 }), fetches-of({ so $proxy == any(1, 2) }),
    'a Proxy operand is fetched as often as when compared against any()';
is (so $two == 1|$proxy), True, 'a Proxy eigenstate compares by its fetched value';
is fetches-of({ so $two == 1|$proxy }), fetches-of({ so $two == any(1, $proxy) }),
    'a Proxy eigenstate is fetched as often as when the junction is built by any()';
is fetches-of({ so 1|$proxy == $two }), fetches-of({ so any(1, $proxy) == $two }),
    'a Proxy eigenstate on the left is fetched as often as when the junction is built by any()';
is fetches-of({ so $two == 1&$proxy }), fetches-of({ so $two == all(1, $proxy) }),
    'a Proxy eigenstate of an all junction is fetched as often as when the junction is built by all()';
my $counter := Proxy.new(FETCH => { ++$fetches }, STORE => -> $, $ { });
my $four = 4;
$fetches = 0;
my $counter-unfolded = so $four == 0|$counter;
$fetches = 0;
my $counter-built = so $four == any(0, $counter);
is $counter-unfolded, $counter-built,
    'a Proxy eigenstate whose value changes on each fetch compares as when the junction is built by any()';

# A native reference operand compares by the value it refers to.
my class Natives {
    has int $.i = 2;
    has num $.n = 2e0;
    has str $.s = 'b';
    method int-matches { so $!i == 1|2 }
    method int-after-match { calls-of({ so $!i == 2|two() }) }
    method num-after-match { calls-of({ so $!n == 2e0|two-num() }) }
    method str-after-match { calls-of({ so $!s eq 'b'|two-str() }) }
}
sub two-num { $calls++; 2e0 }
sub two-str { $calls++; 'c' }
sub calls-of(&code) { $calls = 0; code(); $calls }
is Natives.new.int-matches, True, 'a native int attribute equal to one eigenstate';
is Natives.new.int-after-match, 1,
    'the eigenstate after the match still evaluates for a native int attribute';
is Natives.new.num-after-match, 1,
    'the eigenstate after the match still evaluates for a native num attribute';
is Natives.new.str-after-match, 1,
    'the eigenstate after the match still evaluates for a native str attribute';
my int @native-ints = 2;
is (so @native-ints[0] == 1|2), True, 'a native int array element equal to one eigenstate';
is calls-of({ so @native-ints[0] == 2|two() }), 1,
    'the eigenstate after the match still evaluates for a native int array element';
my num $native-num = 2e0;
my str $native-str = 'b';
is calls-of({ so $native-num == 2e0|two-num() }), 1,
    'the eigenstate after the match still evaluates for a native num operand';
is calls-of({ so $native-str eq 'b'|two-str() }), 1,
    'the eigenstate after the match still evaluates for a native str operand';
is warnings-of({ so $native eq '2'|$undefined }).elems, 1,
    'an undefined eigenstate after a native int compared as a string still warns';

# Every unfolded operator threads a one junction on the left first.
my $one-of = one(1, 2);
my $one-str = one('a', 'b');
is (so $one-of < 2|3), False, 'a one junction threads before the any junction through <';
is (so $one-of <= 1|2), False, 'a one junction threads before the any junction through <=';
is (so $one-of > 0|1), False, 'a one junction threads before the any junction through >';
is (so $one-of >= 1|2), False, 'a one junction threads before the any junction through >=';
is (so $one-of ≤ 1|2), False, 'a one junction threads before the any junction through ≤';
is (so $one-of ≥ 1|2), False, 'a one junction threads before the any junction through ≥';
is (so $one-str lt 'b'|'c'), False, 'a one junction threads before the any junction through lt';
is (so $one-str le 'a'|'b'), False, 'a one junction threads before the any junction through le';
is (so $one-str gt '`'|'a'), False, 'a one junction threads before the any junction through gt';
is (so $one-str ge 'a'|'b'), False, 'a one junction threads before the any junction through ge';
my $letter-b = 'b';
is calls-of({ so $one < 2|two() }), 1, 'the eigenstate after a < match still evaluates';
is calls-of({ so $one ≤ 1|two() }), 1, 'the eigenstate after a ≤ match still evaluates';
is calls-of({ so $letter-b lt 'c'|two-str() }), 1, 'the eigenstate after an lt match still evaluates';
is warnings-of({ so $one < 2|$undefined }).elems, 1, 'an undefined eigenstate after a < match still warns';
is warnings-of({ so $letter-b ge 'b'|$undefined }).elems, 1, 'an undefined eigenstate after a ge match still warns';

# A plain operand whose coercion fails keeps the Failure inside the junction.
my $text = 'abc';
is (try { so $text == 1|2 }) // 'died', False,
    'a Str operand that does not parse as a number compares as False';
is (try { use fatal; if $text == 1|2 { 'taken' } else { 'skipped' } }) // 'died', 'skipped',
    'a Str operand that does not parse as a number is skipped under fatal';
is (try { so 2i < 2|3 }) // 'died', False,
    'a Complex operand that cannot be ordered compares as False';
is (try { so $native-str == 1|2 }) // 'died', False,
    'a native str that does not parse as a number compares as False';

# A non-empty Slip eigenstate flattens into the junction.
is (so $seven == 7&(7, 7).Slip), True, 'a Slip eigenstate flattens into an all junction';
is (so $seven == 1|(2, 7).Slip), True, 'a Slip eigenstate flattens into an any junction';
is (so $seven == 7&Empty), True, 'an Empty eigenstate is no eigenstate at all';
my @seven-eight = 7, 8;
is (so $seven == 1|@seven-eight.Slip), True, 'a slipped array flattens into the junction';
is warnings-of({ so $seven == 7&($undefined, $undefined).Slip }).elems, 2,
    'each undefined value in a Slip eigenstate warns';

# An elsif condition threads, and an elsif or else block taking the value gets the junction.
is (do if $one == 5|6 { 'first' } elsif $j == 1|2 { 'taken' } else { 'skipped' }), 'taken',
    'an elsif condition threads the all junction first';
is (do if 0 { 'first' } elsif $one == 1|2 -> $v { $v.^name } else { 'skipped' }), 'Junction',
    'an elsif block taking the condition receives the junction';
is (do if $one == 5|6 { 'first' } elsif $one == 7|8 { 'second' } else -> $v { $v.^name }), 'Junction',
    'an else block after an elsif receives the junction';
is (do if $j == 1|2 -> { 'taken' } else { 'skipped' }), 'taken',
    'a pointy block without parameters still lets the condition thread';

# Loop conditions and statement modifiers thread the all junction first.
my $iterations = 0;
while $j == 1|2 { $iterations++; last }
is $iterations, 1, 'a while condition threads the all junction first';
$iterations = 0;
until $j == 1|2 { $iterations++; last }
is $iterations, 0, 'an until condition threads the all junction first';
$iterations = 0;
loop (; $j == 1|2;) { $iterations++; last }
is $iterations, 1, 'a loop condition threads the all junction first';
my $ending = all(1, 2);
$iterations = 0;
repeat { $iterations++; $ending = 0 if $iterations >= 2; last if $iterations > 5 } while $ending == 1|2;
is $iterations, 2, 'a repeat while condition threads the all junction first';
my $modified = 'skipped';
$modified = 'taken' if $j == 1|2;
is $modified, 'taken', 'an if statement modifier threads the all junction first';
$modified = 'skipped';
$modified = 'taken' unless $j == 1|2;
is $modified, 'skipped', 'an unless statement modifier threads the all junction first';
my $stepped = 0;
$calls = 0;
$stepped++ while $stepped == 0|two();
is $stepped, 1, 'a while statement modifier compares against a computed eigenstate';
is $calls, 2, 'a computed eigenstate evaluates on every iteration of a while modifier';

# A string comparison coerces and evaluates every eigenstate.
my $stringifications = 0;
my class Stringifies { method Str { $stringifications++; 'x' } }
my $stringy = Stringifies.new;
my $stringy-matched = so $stringy eq 'x'|'y';
is $stringifications, 2, 'an operand stringifies once per eigenstate';
$stringifications = 0;
my $letter-x = 'x';
my $stringy-eigen = so $letter-x eq 'x'|$stringy;
is $stringifications, 1, 'an eigenstate after the match still stringifies';
is warnings-of({ so $undefined eq 'a'&'b' }).elems, 2, 'an undefined operand warns once per string eigenstate';
my class DiesStr { method Str { die 'boom' } }
throws-like { so $letter-b eq 'b'|DiesStr.new }, X::AdHoc,
    'an eigenstate whose stringification dies still throws after the match';

# A Num chain and Int subclasses evaluate every eigenstate.
is calls-of({ so $num == 2e0|two() }), 1, 'an Int eigenstate after the matching Num eigenstate still evaluates';
is warnings-of({ so $num == 2e0|$undefined }).elems, 1, 'an undefined eigenstate after a Num match still warns';
my $big = 2**70;
is (so $big == 2**70|1), True, 'a big Int equal to a big eigenstate';
is (so $big == 1|2**71), False, 'a big Int equal to no big eigenstate';
my class MyInt is Int { }
my $subclassed = MyInt.new(2);
is warnings-of({ so $subclassed == 2|$undefined }).elems, 1,
    'an Int subclass operand still warns for the eigenstate after the match';
my $allomorph = <2>;
is warnings-of({ so $allomorph eq '2'|$undefined }).elems, 1,
    'an IntStr operand compared as a string still warns for the eigenstate after the match';
is (so $allomorph == <1>|<2>), True, 'IntStr eigenstates compare numerically';

# Three eigenstates all evaluate, and a junction on the left that dies stops the right operand.
is calls-of({ so $three == 3|two()|1 }), 1, 'a computed middle eigenstate evaluates after the first matches';
$calls = 0;
is (so $two == two()|two()|two()), True, 'three computed eigenstates all compare';
is $calls, 3, 'three computed eigenstates all evaluate';
sub dies-operand { $order ~= 'x'; die 'boom' }
$order = '';
try { so first-operand()|dies-operand() == third-operand() }
is $order, 'ax', 'a junction on the left whose eigenstate dies never evaluates the right operand';

# A negated comparison negates the collapsed junction.
is (so $two != 1|2), False, 'a value equal to one eigenstate is not unequal to the any junction';
is (so $three != 1|2), True, 'a value equal to no eigenstate is unequal to the any junction';
is (so $two != 1&2), True, 'a value equal to one eigenstate is unequal to the all junction';
is (so $two ≠ 2&2), False, 'a value equal to every eigenstate is not unequal to the all junction';
is (so $letter-b ne 'a'|'b'), False, 'a string equal to one eigenstate is not unequal to the any junction';
is (so $letter-b ne 'a'|'c'), True, 'a string equal to no eigenstate is unequal to the any junction';
is (so $j != 1|2), False, 'an all junction on the left of != threads before the any junction';
is (so $one-of != 1|2), True, 'a one junction on the left of != threads before the any junction';
is (so $one-str ne 'a'|'b'), True, 'a one junction on the left of ne threads before the any junction';
is calls-of({ so $one != 1|two() }), 1, 'the eigenstate after a != mismatch still evaluates';
is calls-of({ so $letter-b ne 'b'|two-str() }), 1, 'the eigenstate after a ne mismatch still evaluates';
is warnings-of({ so $one != 1|$undefined }).elems, 1, 'an undefined eigenstate after a != mismatch still warns';
is warnings-of({ so $undefined != 1&2 }).elems, 2, 'an undefined operand of != warns once per eigenstate';
is (do if $two != 3|4 -> $v { $v.^name } else { 'skipped' }), 'Bool',
    'a block taking a negated condition receives the Bool it collapses to';
is (do { my $r = 'skipped'; $r = 'taken' if $two != 3|4; $r }), 'taken',
    'an if modifier with a negated comparison';

# Exact core numeric types mix in one comparison.
my $half = 1/2;
is (so $half == 1|2), False, 'a Rat equal to no Int eigenstate';
is (so $half == 0.5|2), True, 'a Rat equal to a Rat eigenstate among Ints';
is (so $two == 1|2.5), False, 'an Int equal to no eigenstate of a Rat and an Int';
is (so $two == 2|2.5), True, 'an Int equal to the Int eigenstate beside a Rat';
is (so $two == 1|2e0), True, 'an Int equal to a Num eigenstate';
is (so $num == 1|2), True, 'a Num equal to an Int eigenstate';
is (so $two < 2.5|3), True, 'an Int below a Rat eigenstate';
is (so $half <= 0.5&1), True, 'a Rat at or below every eigenstate of a Rat and an Int';
is (so $native-num == 1|2), True, 'a native num equal to an Int eigenstate';
is (so $native == 1.5|2.5), False, 'a native int equal to no Rat eigenstate';
is (so 2.5 == 2.5|3), True, 'a Rat constant equal to one eigenstate';
sub two-and-a-half { $calls++; 2.5 }
is calls-of({ so $two == 2|two-and-a-half() }), 1, 'a Rat eigenstate after the match still evaluates';
is calls-of({ so $native-num == 2|two() }), 1, 'an Int eigenstate after the match still evaluates for a native num';
is warnings-of({ so $two == 2.5|$undefined }).elems, 1, 'an undefined eigenstate after a Rat still warns';
is warnings-of({ so $half == 1&2 }).elems, 0, 'a Rat compared against Int eigenstates does not warn';
is (so $two == 1|(1/2)), False, 'a computed Rat eigenstate among Ints';
my $bridges = 0;
my class Bridged is Int { method Bridge { $bridges++; callsame } }
my $bridged = Bridged.new(2);
my $bridged-matched = so $bridged == 2e0|1e0;
is $bridges, 2, 'an Int subclass compared against Num eigenstates bridges once per eigenstate';
is (so $bridged == 2|3), True, 'an Int subclass equal to an Int eigenstate';
my $ratio = 1/3;
is (so $ratio == 1|2|3), False, 'a Rat equal to none of three Int eigenstates';
is (so $two == 2|2e0|2.0), True, 'an Int equal to eigenstates of every numeric kind';

# A string comparison stringifies exact core numbers.
is (so $two eq '1'|'2'), True, 'an Int equal to a Str eigenstate as a string';
is (so $two eq '1'|'3'), False, 'an Int equal to no Str eigenstate as a string';
is (so 2.5 eq '2.5'|'x'), True, 'a Rat constant equal to a Str eigenstate as a string';
is (so $letter-b eq 1|2), False, 'a Str equal to no Int eigenstate as a string';
is (so $native eq '2'|'3'), True, 'a native int equal to a Str eigenstate as a string';
is calls-of({ so $two eq '2'|two-str() }), 1, 'the eigenstate after an Int matched a Str still evaluates';
is warnings-of({ so $two eq '2'|$undefined }).elems, 1, 'an undefined eigenstate after an Int matched a Str still warns';
is (so $two lt '3'|'1'), True, 'an Int below a Str eigenstate as a string';
my $stringifies-int = 0;
my class StrInt is Int { method Str { $stringifies-int++; 'x' } }
my $str-int = StrInt.new(2);
my $str-int-matched = so $str-int eq 'x'|'y';
is $stringifies-int, 2, 'an Int subclass compared against Str eigenstates stringifies once per eigenstate';

# A Bool or an enum compares as an Int against Int eigenstates.
is (so True == 1|2), True, 'a Bool equal to an Int eigenstate';
my enum Shade <Light Dark>;
my $shade = Dark;
is (so $shade == Light|Dark), True, 'an enum value equal to an enum eigenstate';
is (so $shade == 0|1), True, 'an enum value equal to an Int eigenstate';
is (so $shade == 0.5|1), True, 'an enum value equal to an Int eigenstate beside a Rat';
is warnings-of({ so $shade == 1|$undefined }).elems, 1, 'an undefined eigenstate after an enum matched still warns';
