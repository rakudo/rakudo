use Test;

# RT #129763
throws-like '1++', X::Multi::NoMatch,
    message => /'but require mutable arguments'/,
'point out matching `is rw` candidates when passing non-rw';

subtest 'curly quotes are not called smart quotes' => {
    my @quotes = ｢‘｣, ｢‚｣, ｢’｣, ｢“｣, ｢„｣, ｢”｣;
    plan +@quotes;

    for @quotes -> $q {
        throws-like $q, Exception,
            :message{ not .contains('smart') and .contains('curly') },
        "$q (U+$q.ord.base(16)) quote is called curly, not smart";
    }
}

# RT #130712
throws-like 'sub infix:<$>() return Nil {}',
    X::AdHoc,
    :message{ .contains("'returns'") },
    'typing "return" instead of "returns" gives a fixing hint';

# RT #130630
throws-like ｢'4x'.Rat.nude｣, X::Str::Numeric,
    :message{ not .contains("Metamodel.nqp") },
    '.Rat.nude on non-numeric string does not reference guts in error';

# RT #130509
throws-like ｢…｣, X::StubCode,
    :message{ not .contains('CORE.setting') },
    'stub code does not reference guts when executed';

# RT #130913
subtest 'chr with large codepoints throws useful error' => {
    my @tests = 'chr 2⁶³-1',   '(2⁶³-1).chr', 'chr 2⁶³',
                '2⁶³.chr',     'chr 2¹⁰⁰',    '(2¹⁰⁰).chr';
    plan +@tests;
    for @tests {
        throws-like $^code, Exception,
            :message{ not .contains('negative') and .contains('codepoint') },
        "$code.perl()";
    }
}

# https://irclog.perlgeek.de/perl6/2017-03-14#i_14263417
throws-like ｢m: my @a = for 1..3 <-> { $_ }｣, Exception,
    :message(/«'do for'»/),
    '<-> does not prevent an error suggesting to use `do for`';

# https://irclog.perlgeek.de/perl6-dev/2017-04-13#i_14425133
# RT #79288
{
    my $param = '$bar';
    throws-like { EVAL q[ sub foo(\qq{$param}? is rw) {} ] }, Exception,
        message => "Cannot use 'is rw' on optional parameter '$param'.",
        'making an "is rw" parameter optional dies with adequate error message and mentions the parameter name';

    throws-like { EVAL q[ sub foo(\qq{$param} is rw = 42) {} ] }, Exception,
        message => "Cannot use 'is rw' on optional parameter '$param'.",
        'making an "is rw" parameter optional dies with adequate error message and mentions the parameter name';
}

# RT #113954
{
    is run(:err, $*EXECUTABLE, ['-e', q[multi MAIN(q|foo bar|) {}]]).err.slurp(:close),
       qq|Usage:\n  -e '...' 'foo bar' \n|,
       'a space in a literal param to a MAIN() multi makes the suggestion quoted';

    is run(:err, $*EXECUTABLE, ['-e', q[multi MAIN(q|foo"bar|) {}]]).err.slurp(:close),
       qq|Usage:\n  -e '...' 'foo"bar' \n|,
       'a double qoute in a literal param to a MAIN() multi makes the suggestion quoted';

    is run(:err, $*EXECUTABLE, ['-e', q[multi MAIN(q|foo'bar|) {}]]).err.slurp(:close),
       qq|Usage:\n  -e '...' 'foo'"'"'bar' \n|,
       'a single qoute in a literal param to a MAIN() multi makes the suggestion quoted';
}

# RT #118263
{
    throws-like { EVAL q|role R { method a {...}; method b { say "b" }; method c {...} }; class C is R {}| },
        Exception,
        message => all(/<<'C'>>/, /<<'R'>>/, /<<'a,' \s* 'c'>>/, /<<'does'>>/),
        'The message when trying to pun a role with required methods should have the names of the child, parent, required methods, and suggest "does"';
}

# RT #126124
# adapted from S06-signature/types.t
{
    throws-like { sub f(Mu:D $a) {}; f(Int) },
        Exception,
        message => all(/'Parameter'/, /\W '$a'>>/, /<<'f'>>/, /<<'must be an object instance'>>/, /<<'not a type object'>>/, /<<'Mu'>>/,  /<<'Int'>>/, /\W '.new'>>/),
        'types and names shown in the exception message are correct';
    throws-like { sub f(Mu:U $a) {}; f(123) },
        Exception,
        message => all(/'Parameter'/, /\W '$a'>>/, /<<'f'>>/, /<<'not an object instance'>>/, /<<'must be a type object'>>/, /<<'Mu'>>/,  /<<'Int'>>/, /<<'multi'>>/),
        'types shown in the exception message are correct';
}

# adapted from S32-exceptions/misc.t
for <fail die throw rethrow resumable resume> -> $meth {
    throws-like 'X::NYI.' ~ $meth,
        Exception,
        message => all(/'Invocant'/, /<<$meth>>/, /<<'must be an object instance'>>/, /<<'not a type object'>>/, /<<'Exception'>>/,  /<<'X::NYI'>>/, /\W '.new'>>/),
}

done-testing;
