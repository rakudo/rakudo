use Test;

plan 4;

# A `!`-twigil attributive parameter in a `sub` binds into the attribute of
# the object whose method lexically encloses the sub. RakuAST used to reject
# it outright ("Variable $!x used where no 'self' is available"), because the
# sub is not itself a method; legacy accepts it since `self` is lexically
# available.
{
    my class C {
        has $.x is rw;
        method via-sub {
            my $store = sub ($, $!x) { };
            $store(self, 42);
            $!x;
        }
    }
    is C.new.via-sub, 42,
        'a $!attr parameter in a sub nested in a method binds the attribute';
}

# The Proxy shape from PDF::Content::Text::Box: a method returns a Proxy whose
# FETCH/STORE closures reach the attribute, and STORE takes it as a parameter.
{
    my class TB {
        has $!text;
        method text is rw {
            Proxy.new(
                FETCH => sub ($)      { $!text },
                STORE => sub ($, $!text) { $!text .= uc },
            );
        }
    }
    my $tb = TB.new;
    $tb.text = 'hi';
    is $tb.text, 'HI',
        'a $!attr parameter in a Proxy STORE sub binds and reads back';
}

# Plain attribute access (not as a parameter) in a nested sub already worked;
# guard against regressing it.
{
    my class C {
        has $.x = 7;
        method via-sub { my $get = sub { $!x }; $get() }
    }
    is C.new.via-sub, 7,
        'plain $!attr access in a sub nested in a method still works';
}

# With no enclosing method there is no self, so it is still an error.
throws-like 'sub f($!x) { }', X::Syntax::NoSelf,
    'a $!attr parameter in a sub with no enclosing method is rejected';
