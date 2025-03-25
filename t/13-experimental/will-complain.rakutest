use experimental :will-complain;

use Test;

plan 8;

subtest "Enum" => {
    plan 2;

    my enum FOO will complain { "need something FOO-ish, got {.raku}" } <foo1 foo2 foo3>;

    lives-ok { my FOO $foo = foo1; }, "doesn't fire when there is no error";

    throws-like
        'my FOO $foo; $foo = 0;',
        X::TypeCheck::Assignment,
        "activated upon failed type check",
        message => /'need something FOO-ish, got 0'/;
}

subtest "Custom class" => {
    plan 2;

    my class Bar will complain { "need something Bar-like, got {.raku}" } { }

    lives-ok { my Bar $bar = Bar.new; }, "doesn't fire when there is no error";

    throws-like
        'my Bar $bar; $bar = 0;',
        X::TypeCheck::Assignment,
        "activated upon failed type check",
        message => /'need something Bar-like, got 0'/;
}

subtest "Parameter" => {
    plan 2;
    my sub foo(Str:D $p will complain { "the first argument must be a string with 'foo'" } where *.contains('foo')) {}

    lives-ok { foo("this is foo-ish") }, "doesn't fire when there is no error";

    throws-like
        { foo("this is bar-like") },
        X::TypeCheck::Binding,
        "activated upon failed type check",
        message => /"the first argument must be a string with 'foo'"/;
}

subtest "Subset" => {
    plan 2;

    my subset IntD of Int:D will complain { "only non-zero positive integers, not {.raku}" } where * > 0;
    my IntD $v = 1;

    lives-ok { $v = 10 }, "doesn't fire when there is no error";

    throws-like
        { $v = -1 },
        X::TypeCheck::Assignment,
        "activated upon failed type check",
        message => /'only non-zero positive integers, not -1'/;
}

subtest "Variable" => {
    plan 2;

    my Str $s will complain { "gimme a string, not {.^name}" };

    lives-ok { $s = "ok" }, "doesn't fire when there is no error";

    throws-like
        { $s = 0 },
        X::TypeCheck::Assignment,
        "activated upon failed type check",
        message => /'gimme a string, not Int'/;
}

subtest "Attribute" => {
    plan 2;

    my class Foo {
        has Int $.a is rw will complain { "you offer me {.raku}, but with all the respect: an integer, please!" };
    }
    my $foo = Foo.new;

    lives-ok { $foo.a = 42 }, "doesn't fire when there is no error";

    throws-like
        { $foo.a = 4.2 },
        X::TypeCheck::Assignment,
        "activated upon failed type check",
        message => /"you offer me 4.2, but with all the respect: an integer, please!"/;
}

subtest "Hash" => {
    plan 2;

    my Str %h will complain { "hash values are to be strings, not {.^name}" };

    lives-ok { %h<ok> = "good" }, "doesn't fire when there is no error";

    throws-like
        { %h<oops> = pi },
        X::TypeCheck::Assignment,
        "activated upon failed type check",
        message => /'hash values are to be strings, not Num'/;
}

subtest "Array" => {
    plan 2;

    my Int @a will complain { "this array is all about integers, not {.^name}" };

    lives-ok { @a[0] = 42 }, "doesn't fire when there is no error";

    throws-like
        { @a[1] = pi },
        X::TypeCheck::Assignment,
        "activated upon failed type check",
        message => /'this array is all about integers, not Num'/;
}

done-testing;
