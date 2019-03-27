use lib <t/packages/>;
use Test;
use Test::Helpers;

plan 48;

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
    is-run ｢multi MAIN(q|foo bar|) {}｣,
       :err(qq|Usage:\n  -e '...' 'foo bar'\n|),
       :exitcode(*),
       'a space in a literal param to a MAIN() multi makes the suggestion quoted';

    if $*DISTRO.is-win {
        skip "Test routine quoting doesn't work right on Windows: RT#132258"
    }
    else {
        is-run ｢multi MAIN(q|foo"bar|) {}｣,
           :err(qq|Usage:\n  -e '...' 'foo"bar'\n|),
           :exitcode(*),
           'a double qoute in a literal param to a MAIN() multi makes the suggestion quoted';
    }

    is-run ｢multi MAIN(q|foo'bar|) {}｣,
       :err(qq|Usage:\n  -e '...' 'foo'"'"'bar'\n|),
       :exitcode(*),
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
for <fail die throw rethrow resume> -> $meth {
    throws-like 'X::NYI.' ~ $meth,
        Exception,
        message => all(/'Invocant'/, /<<$meth>>/, /<<'must be an object instance'>>/, /<<'not a type object'>>/, /<<'Exception'>>/,  /<<'X::NYI'>>/, /\W '.new'>>/),
}

# RT #112396
{
    throws-like q|3 ==> &sin ==> &say|,
        Exception,
        message => /<<'sin()'\W/,
        'sinking to a code object in a feed suggests calling the routine';
}

# RT #122232
{
    throws-like { my class RT122232::B {}; RT122232.new },
        Exception,
        message => /'cannot create' .+ «RT122232»/,
        'trying to instantiate a non-class gives the name in the error';
    throws-like { my subset RT122232 of Int where * > 42; RT122232.new },
        Exception,
        message => /'cannot create' .+ «RT122232»/,
        'trying to instantiate a non-class gives the name in the error';
}

subtest 'non-ASCII digits > 7 in leading-zero-octal warning' => {
    plan 2;

    with run $*EXECUTABLE, '-e', 'say 0୯', :err, :out {
        is   .out.slurp(:close), "9\n", 'STDOUT is right';
        like .err.slurp(:close), /'୯ is not a valid octal number'/,
            'STDERR mentions the end-result is not valid octal';
    }
}

# RT #123085
{
    throws-like { sub foo([$head, $tail]) {}; foo([3, 4], [3]) },
        Exception,
        message => all(/<<'Too many'>>/, /<<'expected 1'>>/, /<<'got 2'>>/),
        'wrong arity in a signature has correct values in error message';
    throws-like { sub foo([$head, $tail], [$foo]) {}; foo([3, 4]) },
        Exception,
        message => all(/<<'Too few'>>/, /<<'expected 2'>>/, /<<'got 1'>>/),
        'wrong arity in a signature has correct values in error message';
    throws-like { sub foo([$head]) {}; foo([3, 4]) },
        Exception,
        message => all(/<<'Too many'>>/, /<<'expected 1'>>/, /<<'got 2'>>/, /<<'sub-signature'>>/),
        'wrong arity in a sub-signature has correct values in error message';
    throws-like { sub foo(@bar ($head, $tail)) {}; foo([3]) },
        Exception,
        message => all(/<<'Too few'>>/, /<<'expected 2'>>/, /<<'got 1'>>/, /<<'sub-signature'>>/, /<<'parameter @bar'>>/),
        'wrong arity in a sub-signature with a named parameter has correct values in error message';
}

# RT #131408
{
    throws-like { sub foo([$head, $tail]) {}; foo([3, 4], [3]) },
        Exception,
        message => /<<'foo'>>/,
        'wrong arity in a signature mentions the name of the sub';
    throws-like { class A { sub foo([$head, $tail]) {} }; A.foo([3, 4], [3]) },
        Exception,
        message => /<<'foo'>>/,
        'wrong arity in a signature mentions the name of the method';
}

{ # https://irclog.perlgeek.de/perl6-dev/2017-05-31#i_14666102
    throws-like '42.length      ', Exception, '.length on non-List Cool',
        :message{ .contains: <chars codes>.all & none <elems graphs> };

    throws-like '[].length      ', Exception, '.length on List',
        :message{ .contains: 'elems' & none <chars codes graphs>     };

    throws-like 'class {}.length', Exception, '.length on non-Cool',
        :message{ .contains: <elems chars codes>.all & none 'graphs' };

    throws-like 'length 42      ', Exception, '&length',
        :message{ .contains: <elems chars codes>.all & none 'graphs' };
}

# RT #131201
throws-like { class { proto method x(|) {*} }.new.x }, X::Multi::NoMatch,
    :message{ .contains: 'only the proto' & none 'none of these signatures' },
    'error points out only only proto is defined';

# RT #131367
throws-like { Blob.split }, X::Multi::NoMatch,
    :message{ .contains: 'only the proto' & none 'none of these signatures' },
    'error points out only only proto is defined (Blob.split)';

# RT #131367
throws-like { Blob.splice }, X::Multi::NoMatch,
    :message{ .contains: 'only the proto' & none 'none of these signatures' },
    'error points out only only proto is defined (Blob.splice)';

# RT #127395, #123078
{
    throws-like q| class RT123078_1 { method foo { self.bar }; method !bar { }; method baz { } }; RT123078_1.new.foo |,
        X::Method::NotFound,
        message => all(/<<"No such method 'bar'" \W/, /<<'RT123078_1'>>/, /\W '!bar'>>/, /<<'baz'>>/),
        'a private method of the same name as the public missing method is suggested';
    throws-like q| class RT123078_2 { method foo { self!bar }; method bar { }; method baz { } } |,
        X::Method::NotFound,
        message => all(/<<"No such private method '!bar'" \W/, /<<'RT123078_2'>>/, /<<'bar'>>/, /<<'baz'>>/),
        'a public method of the same name as the missing private method is suggested';
    throws-like q| <a a b>.uniq |,
        X::Method::NotFound,
        message => all(/<<"No such method 'uniq'" \W/, /<<'unique'>>/),
        'potentially common misspelling gives the right suggestion';
    throws-like q| ‘foo’.starts-wizh(‘f’) |,
        X::Method::NotFound,
        message => all(/<<"No such method 'starts-wizh'" \W/, /<<'starts-with'>>/),
        'longer method names are suggested also';
}

subtest '`IO::Socket::INET.new: :listen` fails with useful error' => {
    plan 3;
    my $res = IO::Socket::INET.new: :listen;
    isa-ok $res, Failure, 'got a Failure';
    ok $res.handled.not, 'Failure is unhandled';
    $res.so;
    like $res.exception.message, /'Invalid port'/, 'error mentions port';
}

throws-like ｢use v5｣, X::Language::Unsupported,
    '`use v5` in code does not try to load non-existent modules';

# RT#127341
is-run 'Duration.new: Inf; Duration.new: "meow"',
    :out{not .contains: '$!tai'}, :err{not .contains: '$!tai'}, :exitcode(*),
    'Duration.new with bad args does not reference guts';

# RT#125902
is-run ｢my Str where 'foo' $test｣, :exitcode(*),
  :err{.contains: ｢forget a variable｣ and not .contains: ｢Did you mean 'Str'｣},
'sane error when missing variables with my and where';

# RT#132285
throws-like ｢Blob[num32].new: 2e0｣,
    Exception,
    :message{ .contains: ｢Can only parameterize｣ & ｢num32｣ and not .contains: ｢got null｣ },
    'sane NYI error for num32 Blob';

# RT#77754
throws-like ｢callframe.callframe(1).my.perl｣, X::NYI,
    'callframe.my throws sane NYI error message';


subtest '.new on native types works (deprecated; will die)' => {
    my @types := int, int8, int16, int32, int64, num, num32, num64, str;
    plan 2 * @types;

    for @types -> \T {
        throws-like { T.new }, Exception,
            :message{ .contains: "Cannot instantiate" }, T.^name ~ ' no args';
        throws-like { T.new: 42 }, Exception,
            :message{ .contains: "Cannot instantiate" }, T.^name ~ ' with args';
    }
}

#### THIS FILE ALREADY LOTS OF TESTS ADD NEW TESTS TO THE NEXT error.t FILE

# vim: ft=perl6 expandtab sw=4
