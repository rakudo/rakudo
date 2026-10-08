use Test;
use nqp;

plan 33;

# Declarator docs are set as the .WHY of what they document from BEGIN time
# on. A trait of the declaration sees them, as does any BEGIN time code
# after it.

is EVAL(q:to/CODE/), 'lead',
my $seen;
multi trait_mod:<is>(Routine:D $r, :$peek!) { $seen = $r.WHY.Str }
#| lead
sub f() is peek { }
$seen
CODE
    'a trait of a sub sees its leading doc';

is EVAL(q:to/CODE/), 'lead',
#| lead
sub f() { }
BEGIN &f.WHY.Str
CODE
    'BEGIN time code after a sub sees its leading doc';

is EVAL(q:to/CODE/), 'trail',
sub f() { } #= trail
BEGIN &f.WHY.Str
CODE
    'BEGIN time code after a sub sees its trailing doc';

is EVAL(q:to/CODE/), 'trail',
sub f() { }
#= trail
BEGIN &f.WHY.Str
CODE
    'BEGIN time code after a sub sees its trailing doc on the next line';

is EVAL(q:to/CODE/), 'lead',
my $seen;
multi trait_mod:<is>(Parameter:D $p, :$peek!) { $seen = $p.WHY.Str }
sub f(
  #| lead
  $x is peek
) { }
$seen
CODE
    'a trait of a parameter sees its leading doc';

is EVAL(q:to/CODE/), 'trail',
my $seen;
multi trait_mod:<is>(Parameter:D $p, :$peek!) { $seen = $p.WHY.Str }
sub f(
  $x is peek, #= trail
) { }
$seen
CODE
    'a trait of a parameter sees its trailing doc';

is EVAL(q:to/CODE/), 'lead x|trail y',
my $seen;
multi trait_mod:<is>(Routine:D $r, :$peek!) {
    $seen = $r.signature.params.map(*.WHY.Str).join('|')
}
sub f(
  #| lead x
  $x,
  $y, #= trail y
) is peek { }
$seen
CODE
    'a trait of a sub sees the docs of its parameters';

is EVAL(q:to/CODE/), 'int|str',
my @seen;
multi trait_mod:<is>(Routine:D $r, :$peek!) { @seen.push: $r.WHY.Str }
#| int
multi sub f(Int) is peek { }
#| str
multi sub f(Str) is peek { }
@seen.join('|')
CODE
    'a trait of each multi candidate sees the leading doc of that candidate';

is EVAL(q:to/CODE/), 'lead',
my $seen;
multi trait_mod:<is>(Method:D $m, :$peek!) { $seen = $m.WHY.Str }
my class C {
    #| lead
    method m() is peek { }
}
$seen
CODE
    'a trait of a method sees its leading doc';

is EVAL(q:to/CODE/), 'lead',
my $seen;
multi trait_mod:<is>(Attribute:D $a, :$peek!) { $seen = $a.WHY.Str }
my class C {
    #| lead
    has $.x is peek;
}
$seen
CODE
    'a trait of an attribute sees its leading doc';

is EVAL(q:to/CODE/), "lead\ntrail",
my $seen;
multi trait_mod:<is>(Attribute:D $a, :$keep!) { $seen = $a.WHY }
my class C {
    #| lead
    has $.x is keep; #= trail
}
$seen.Str
CODE
    'the doc a trait of an attribute holds gets the trailing doc parsed after it';

is EVAL(q:to/CODE/), 'trail',
my $seen;
my class C {
    has $.x; #= trail
    BEGIN $seen = C.^attributes.head.WHY.Str;
}
$seen
CODE
    'BEGIN time code after an attribute sees its trailing doc';

ok EVAL(q:to/CODE/),
my $seen;
multi trait_mod:<is>(Routine:D $r, :$keep!) { $seen = $r.WHY }
#| lead
sub f() is keep { }; #= trail
$=pod.head<> =:= $seen<> && &f.WHY<> =:= $seen<>
CODE
    'the doc a trait of a sub holds is the doc of the sub and its $=pod entry';

is EVAL(q:to/CODE/), "lead\ntrail",
#| lead
my class C { } #= trail
BEGIN C.WHY.Str
CODE
    'BEGIN time code after a class sees its leading and trailing doc';

is EVAL(q:to/CODE/), 'lead',
my role R {
    #| lead
    has $.x;
}
my class C does R { }
C.^attributes.head.WHY.Str
CODE
    'a class gets the doc of an attribute of a role it does';

is EVAL(q:to/CODE/), 'lead',
my role R {
    #| lead
    method m() { }
}
my class C does R { }
C.^find_method('m').WHY.Str
CODE
    'a class gets the doc of a method of a role it does';

is EVAL(q:to/CODE/), 'lead',
my role R[::T] {
    #| lead
    has T $.x;
}
my class C does R[Int] { }
C.^attributes.head.WHY.Str
CODE
    'a class gets the doc of an attribute of a parametric role it does';

is EVAL(q:to/CODE/), 'lead',
#| lead
my role R { }
BEGIN R.new;
R.new.WHY.Str
CODE
    'an instance of a role punned at BEGIN time has the doc of the role';

is EVAL(q:to/CODE/), 'lead|none',
#| lead
my subset S of Int where sub ($x) { $x > 0 };
S.WHY.Str ~ '|' ~ (S.^refinement.WHY // 'none')
CODE
    'the doc of a subset moves off its where routine';

is EVAL(q:to/CODE/), 'lead',
#| lead
my subset S of Int where ({ $_ > 0 });
BEGIN S.WHY.Str
CODE
    'BEGIN time code after a subset with a parenthesized where block sees its leading doc';

is EVAL(q:to/CODE/), 'lead',
#| lead
my enum E (do { my sub f() { <a b> }; f() });
BEGIN E.WHY.Str
CODE
    'BEGIN time code after an enum whose term declares a sub sees its leading doc';

todo 'the legacy frontend drops the doc'
  unless nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast';
is EVAL(q:to/CODE/), 'sig',
my subset S of Signature where :($p) #= sig
;
S.WHY.Str
CODE
    'a subset takes a trailing doc after its where clause from a parameter in it';

# The legacy frontend documents a type or a regex after applying its traits.
if nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    is EVAL(q:to/CODE/), 'lead',
    my $seen;
    multi trait_mod:<is>(Mu:U $type, :$peek!) { $seen = ($type.HOW.WHY // '').Str }
    #| lead
    my class C is peek { }
    $seen
    CODE
        'a trait of a class sees its leading doc';

    is EVAL(q:to/CODE/), 'lead',
    my $seen;
    multi trait_mod:<is>(Mu:U $type, :$peek!) { $seen = ($type.HOW.WHY // '').Str }
    #| lead
    my subset S of Int is peek where ({ $_ > 0 });
    $seen
    CODE
        'a trait of a subset with a parenthesized where block sees its leading doc';

    is EVAL(q:to/CODE/), 'lead',
    my $seen;
    multi trait_mod:<is>(Mu:U $type, :$peek!) { $seen = ($type.HOW.WHY // '').Str }
    #| lead
    my enum E is peek (do { my sub f() { <a b> }; f() });
    $seen
    CODE
        'a trait of an enum whose term declares a sub sees its leading doc';

    is EVAL(q:to/CODE/), 'lead',
    my $seen;
    multi trait_mod:<is>(Mu:U $type, :$peek!) { $seen = ($type.HOW.WHY // '').Str }
    #| lead
    my role R is peek { }
    $seen
    CODE
        'a trait of a role sees its leading doc';

    is EVAL(q:to/CODE/), 'lead',
    my $seen;
    multi trait_mod:<is>(Mu:U $type, :$peek!) { $seen = ($type.HOW.WHY // '').Str }
    #| lead
    my enum E is peek <a b>;
    $seen
    CODE
        'a trait of an enum sees its leading doc';

    is EVAL(q:to/CODE/), 'lead',
    my $seen;
    multi trait_mod:<is>(Mu:U $type, :$peek!) { $seen = ($type.HOW.WHY // '').Str }
    #| lead
    my subset S of Int is peek where * > 0;
    $seen
    CODE
        'a trait of a subset sees its leading doc';

    is EVAL(q:to/CODE/), 'lead',
    my $seen;
    multi trait_mod:<is>(Mu:U $type, :$peek!) { $seen = ($type.HOW.WHY // '').Str }
    #| lead
    my subset S of Int is peek where { $_ > 0 };
    $seen
    CODE
        'a trait of a subset with a where block sees its leading doc';

    is EVAL(q:to/CODE/), 'lead',
    my $seen;
    multi trait_mod:<is>(Routine:D $r, :$peek!) { $seen = $r.WHY.Str }
    my grammar G {
        #| lead
        token TOP is peek { a }
    }
    $seen
    CODE
        'a trait of a token sees its leading doc';

    my $seen;
    EVAL(q:to/CODE/);
    #| lead
    unit class DeclaratorDocBeginTimeUnit; #= trail
    BEGIN $seen = (DeclaratorDocBeginTimeUnit.HOW.WHY // '').Str;
    CODE
    is $seen, "lead\ntrail",
        'BEGIN time code in a unit scoped class sees its leading and trailing doc';

    is EVAL(q:to/CODE/), "lead\ntrail",
    my $seen;
    multi trait_mod:<is>(Mu:U $type, :$keep!) { $seen = $type.HOW.WHY }
    #| lead
    my class C is keep { } #= trail
    ($seen // '').Str
    CODE
        'the doc a trait of a class holds gets the trailing doc parsed after it';

    ok EVAL(q:to/CODE/),
    my $seen;
    multi trait_mod:<is>(Mu:U $type, :$keep!) { $seen = $type.HOW.WHY }
    #| lead
    my class C is keep { } #= trail
    $seen<> =:= C.WHY<>
    CODE
        'the doc a trait of a class holds is the doc of the class';
}
else {
    skip 'the legacy frontend documents a type or a regex after applying its traits', 11;
}

# vim: expandtab shiftwidth=4
