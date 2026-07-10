use Test;

plan 4;

# A three-name alias parameter (`:x(:y(:$z))`) forces the full binder for the
# whole signature. A private attributive parameter in that signature is then
# bound by the binder, which reaches the attribute through the parameter's
# declaring package.
{
    my class C {
        has @.path;
        submethod BUILD(:@!path!, :x(:y(:$z))) { }
        multi method new(@path) { self.bless(:@path) }
    }
    is C.new([1,2,3]).path.join(','), '1,2,3',
        'a private array attributive parameter binds under the full binder';
}

# The same for a scalar attribute.
{
    my class C {
        has $.v;
        submethod BUILD(:$!v!, :x(:y(:$z))) { }
    }
    is C.bless(v => 42).v, 42,
        'a scalar attributive parameter binds under the full binder';
}

# In a role the declaring package is generic, so it must instantiate to the
# class the role is composed into rather than stay the role.
{
    my role R {
        has @.a;
        submethod BUILD(:@!a!, :x(:y(:$z))) { }
    }
    my class D does R {
        multi method new(@a) { self.bless(:@a) }
    }
    is D.new([4,5,6]).a.join(','), '4,5,6',
        'an attributive parameter in a composed role binds to the pun';
}

# A two-name alias keeps the inline bindings path, which must still work.
{
    my class C {
        has @.a;
        submethod BUILD(:@!a!, :x(:$z)) { }
        multi method new(@a) { self.bless(:@a) }
    }
    is C.new([7,8]).a.join(','), '7,8',
        'the inline bindings path is unaffected';
}
