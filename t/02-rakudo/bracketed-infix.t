use Test;

plan 26;

is-deeply (try (1..10).grep(* [%%] 3).List), (3, 6, 9),
    'a bracketed infix primes a Whatever operand';

is (try (* [~~] Int)(5)), True,
    'a bracketed smartmatch primes a Whatever left operand';

{
    my $x = 0;
    my $ran;
    $x [&&]= ($ran = 1);
    nok $ran, '`[&&]=` does not evaluate its right side when its left side is false';
}

{
    my $x = 5;
    my $ran;
    $x [//]= ($ran = 1);
    nok $ran, '`[//]=` does not evaluate its right side when its left side is defined';
}

{
    my $x = 1;
    my $ran;
    $x [||]= ($ran = 1);
    nok $ran, '`[||]=` does not evaluate its right side when its left side is true';
}

{
    my $ran;
    is ([[&&]] 1, 0, ($ran = 1)), 0,
        'a reduction over a bracketed `&&` yields the first false value';
    nok $ran,
        'a reduction over a bracketed `&&` does not evaluate the values after the first false one';
}

{
    my $ran;
    is (($ran = 1) R[&&] 0), 0,
        'a reversed bracketed `&&` yields its false right side';
    nok $ran,
        'a reversed bracketed `&&` does not evaluate its left side when its right side is false';
}

{
    my int $n = 9223372036854775807;
    is ($n R[+] 1), ($n R+ 1),
        'a reversed bracketed infix passes a literal beside a native operand as the bare one does';
}

{
    my $x = 4;
    my $ran;
    $x [notandthen]= ($ran = 1);
    nok $ran, '`[notandthen]=` does not evaluate its right side when its left side is defined';
    is $x, 4, '`[notandthen]=` leaves a defined left side as it was';
}

{
    my $x;
    my $ran;
    $x [andthen]= ($ran = 1);
    nok $ran, '`[andthen]=` does not evaluate its right side when its left side is undefined';
}

{
    my $x = 5;
    my $ran;
    $x [orelse]= ($ran = 1);
    nok $ran, '`[orelse]=` does not evaluate its right side when its left side is defined';
}

{
    my $x;
    $x [&&]= 5;
    nok $x.defined, '`[&&]=` leaves an undefined left side undefined';
}

{
    my @a = 1;
    try @a[0] xx= 3;
    is @a[0].elems, 3, '`xx=` assigns the repetition of its left side';
}

{
    my @a = 1;
    try @a[0] [xx]= 3;
    is @a[0].elems, 3, '`[xx]=` assigns the repetition of its left side';
}

{
    my @a = 1;
    try @a[0] [xx=] 3;
    is @a[0].elems, 3, '`[xx=]` assigns the repetition of its left side';
}

{
    my $c = 0;
    is-deeply (3 R[xx] (++$c)).List, (1, 2, 3),
        'a reversed bracketed `xx` evaluates its right side once per repetition';
}

is (try (.succ R[andthen] 42)), 43,
    'a reversed bracketed `andthen` topicalizes its left side';

is-deeply (try EVAL q[((1, 0) X[&&] (2, 3)).List]), (2, 3, 0, 0),
    'a cross over a bracketed `&&` applies it to every pairing';
is-deeply (try EVAL q[((1, 0) Z[&&] (2, 3)).List]), (2, 0),
    'a zip over a bracketed `&&` applies it to each pair';
is-deeply (try EVAL q[((1, 0) XR&& (2, 3)).List]), (1, 1, 0, 0),
    'a cross over a reversed `&&` applies it to every pairing';

throws-like '1 [&&] 2 :foo', X::Syntax::Adverb,
    'a bracketed `&&` rejects an adverb';
throws-like '1 [<] 2 :foo', X::Syntax::Adverb,
    'a bracketed chaining infix rejects an adverb';
throws-like '1 [^^] 0 :foo', X::Syntax::Adverb,
    'a bracketed list infix rejects an adverb';

# vim: expandtab shiftwidth=4
