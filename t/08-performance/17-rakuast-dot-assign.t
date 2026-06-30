use lib <t/packages/Test-Helpers>;
use Test::Helpers::QAST;
use Test;
plan 11;

# A dot-assignment inlines to a plain method call whose result is stored back,
# dropping the dispatch:<.=> dispatcher.
{
    my $s = 'Hi'; $s .= lc;
    is $s, 'hi', 'an explicit scalar target stores the result back';
}
{
    my @a = <z a b>; @a .= sort;
    is @a.join, 'abz', 'an explicit array target stores the result back';
}
{
    class Ctor { has $.v = 9 }
    my Ctor $c .= new;
    is $c.v, 9, 'a declaration with a dot-assignment constructs the value';
}

# A bare dot-assignment on the topic stores back through $_.
{
    my $s = 'Hi'; given $s { .=lc }
    is $s, 'hi', 'a bare topic dot-assignment stores the result back';
}

# A test modifier on a topic dot-assignment keeps its laziness: the call runs
# only when the modifier selects it, and the topic is stored back when it does.
{
    class Cwith { has $.v = 9 }
    my $a = Cwith.new(:v(1)); given $a { .=new with $a }
    is $a.v, 9, 'a with modifier runs the call on a defined topic';
}
{
    my $ran = 0;
    my $a = Int; given $a { .=new with $a }
    is $a.defined, False, 'a with modifier skips the call on an undefined topic';
}
{
    my Int $a; given $a { .=new without $a }
    is $a.defined, True, 'a without modifier runs the call on an undefined topic';
}

# A chain of topic dot-assignments composes through andthen and orelse.
{
    my Int $x; given $x { .=new andthen .=new orelse .=new }
    is $x.defined, True, 'a chained topic dot-assignment composes';
}

# A target that is not a plain variable is evaluated once.
{
    my $calls = 0; my @a = ['Hi']; sub idx { $calls++; 0 }
    @a[idx()] .= lc;
    is "{@a[0]} $calls", 'hi 1', 'a complex target is evaluated once';
}

# The inlining drops the dispatcher for both the explicit and the topic form.
# These observe the emitted QAST.
qast-is 'my $s = "Hi"; $s .= lc', :full, -> \v { not qast-contains-callmethod v, 'dispatch:<.=>' },
    'an explicit dot-assignment inlines the dispatcher away';
qast-is 'my $s = "Hi"; given $s { .=lc }', :full, -> \v { not qast-contains-callmethod v, 'dispatch:<.=>' },
    'a topic dot-assignment inlines the dispatcher away';

# vim: expandtab shiftwidth=4
