use Test;

plan 15;

# A doc block belongs to the statement that follows it in the same statement
# list. A doc block preceding a package declaration must not end up inside
# the package body, where it would sit in a stub method's statement list and
# stop the method from registering as a stub with the yada bit.

sub role-code(Str:D $name) {
    qq:to/CODE/;
    =begin pod
    documentation
    =end pod
    my role $name \{
        method dispatch(\$event) \{ ... }
    }
    CODE
}

is EVAL(role-code('SinkA') ~ 'SinkA').^lookup('dispatch').yada, True,
    'a stub method in a role preceded by a doc block keeps its yada bit';

throws-like { EVAL(role-code('SinkB') ~ 'SinkB.new.dispatch("x")') },
    Exception,
    message => /'must be implemented'/,
    'punning such a role reports the unimplemented required method';

throws-like { EVAL(role-code('SinkC') ~ 'class C does SinkC { }') },
    Exception,
    message => /'must be implemented'/,
    'composing such a role without the method fails composition';

is EVAL(q:to/CODE/), 'first,second', 'doc blocks before and after a role reach $=pod in order';
=begin pod
first
=end pod
my role SinkD {
    method dispatch($event) { ... }
}
=begin pod
second
=end pod
$=pod.map({ .contents[0].contents[0].Str }).join(",")
CODE

is EVAL(q:to/CODE/), 'between', 'a doc block between class members attaches inside the class body';
class MembersA {
    has $.a;
    =begin pod
    between
    =end pod
    method m() { 42 }
}
MembersA.new.m;
$=pod[0].contents[0].contents[0].Str
CODE

is EVAL(q:to/CODE/), '42,1', 'a trailing doc block inside a method body reaches $=pod';
class TrailA {
    method m() {
        42;
        =begin pod
        tail
        =end pod
    }
}
TrailA.new.m ~ ',' ~ $=pod.elems
CODE

lives-ok { EVAL "=begin pod\nonly\n=end pod\n" },
    'a compilation unit holding only a doc block compiles';

is EVAL(q:to/CODE/), 1, 'a doc block before a statement-prefixed loop expression reaches $=pod';
=begin pod
prefixed
=end pod
my @r = do for 1..3 { $_ * 2 };
$=pod.elems
CODE

is EVAL(q:to/CODE/), 1, 'a doc block before a simple do prefix expression reaches $=pod';
=begin pod
prefixed
=end pod
my $x = do 42;
$=pod.elems
CODE

is EVAL(q:to/CODE/), 1, 'a leading doc block with a parenthesized config value reaches $=pod';
=begin pod :kind("Type")
configured
=end pod
my $x = 42;
$=pod.elems
CODE

is EVAL(q:to/CODE/), 'A,B', 'a doc block before one with a block config value reaches $=pod';
say "";
=begin pod
A
=end pod
=begin pod :x{a => 1}
B
=end pod
$=pod.map({ .contents[0].contents[0].Str }).join(",")
CODE

is EVAL(q:to/CODE/), 'A,B', 'doc blocks interleave in order across a statement prefix';
=begin pod
A
=end pod
my $x = do 42;
=begin pod
B
=end pod
my $y = 1;
$=pod.map({ .contents[0].contents[0].Str }).join(",")
CODE

is EVAL(q:to/CODE/), '1,1', 'a doc block before a phaser argument inside a sub reaches $=pod';
sub p() {
    ENTER
=begin pod
phased
=end pod
    42;
    1
}
p() ~ ',' ~ $=pod.elems
CODE

is EVAL(q:to/CODE/), 'first,second', 'two leading doc blocks where the second has a parenthesized config reach $=pod in order';
=begin pod
first
=end pod
=begin pod :kind("Type")
second
=end pod
my $x = 42;
$=pod.map({ .contents[0].contents[0].Str }).join(",")
CODE

is EVAL(q:to/CODE/), 1, 'a doc block before a regex with a :my declaration reaches $=pod';
=begin pod
rx
=end pod
my $r = / :my $q = 1; abc /;
$=pod.elems
CODE

# vim: expandtab shiftwidth=4
