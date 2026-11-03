use v6.e.PREVIEW;
use experimental :rakuast;
use Test;

plan 4;

# A call site that has seen many types looks a method up in the flattened
# method table each class caches.  A precompiled module carries the tables
# its classes built while it compiled, and adding a method to a parent
# class afterwards clears only the table of that parent.

# The setting augments RakuAST::Node with .raku after some of those tables
# were built, so a table read back from the setting still names the method
# Mu provides.
{
    # every node class the parse of this source produces, once each
    my $source = Q:to/CODE/;
    my Int $a = 1;
    my @b = 1e0, 1.5, "a", v1, $a, $*b, :c, :!d;
    my @c = @b[0], |@b;
    sub f(Int $x, *@y, :$z --> Str) { $x ?? "x" !! "y" }
    class C { has $.e; method m { $!e }; }
    for @b -> $g { say $g }
    if $a { A: while 1 { last A } }
    my $h = -$a + $a * 2 ** 3;
    $h++;
    "$a" ~~ /a+ <[b]> ^ $/;
    CODE
    my @nodes;
    my %seen;
    sub walk($node) {
        @nodes.push($node) unless %seen{$node.^name}++;
        $node.visit-children(&walk);
    }
    walk($source.AST);

    sub site(Mu $node) { $node.raku }
    site($_) for @nodes;

    my $block = "=begin pod\n\nhi\n\n=end pod\n".AST.statements.head;
    is site($block), $block.raku,
      'a megamorphic call site finds the .raku the setting added to RakuAST::Node';

    my $doc = RakuAST::Doc::Paragraph.new('hi');
    is site($doc), $doc.raku,
      'a megamorphic call site finds the .raku of a doc paragraph';
}

# The same through a module of our own: its compilation builds the table
# of the subclass through a megamorphic site, the module is precompiled,
# and the program that loads it adds a method to the parent.
{
    my $dir = $*TMPDIR.add("megamorphic-augmented-" ~ $*PID);
    sub remove(IO::Path $path) {
        if $path.d {
            remove($_) for $path.dir;
            $path.rmdir;
        }
        else {
            $path.unlink;
        }
    }
    LEAVE remove($dir) if $dir.e;
    $dir.add('lib').mkdir;
    $dir.add('lib/Stale.rakumod').spurt: Q:to/MODULE/;
    unit module Stale;
    class Top is export { method who { 'top' } }
    class Middle is Top is export { }
    class Bottom is Middle is export { }
    my @fillers = BEGIN (^20).map({
        Metamodel::ClassHOW.new_type(name => "Filler$_").^compose
    });
    sub site(Mu $o) { $o.WHICH }
    BEGIN { site($_) for @fillers; site(Bottom) }
    MODULE

    my $program = Q:to/PROGRAM/;
    use Stale;
    use MONKEY-TYPING;
    augment class Middle { method who { 'middle' } }
    my @others = (^20).map({
        my $type = Metamodel::ClassHOW.new_type(name => "Other$_");
        $type.^add_method('who', method { 'other' });
        $type.^compose
    });
    sub site(Mu $o) { $o.who }
    site($_) for @others;
    print site(Bottom.new);
    PROGRAM

    my @command = $*EXECUTABLE, '-I' ~ $dir.add('lib'), '-e', $program;
    my $first = run |@command, :out, :err;
    is $first.out.slurp(:close), 'middle',
      'the program finds the added method with the module compiled in process';
    $first.err.slurp(:close);

    my $second = run |@command, :out, :err;
    is $second.out.slurp(:close), 'middle',
      'the program finds the added method with the module loaded from its precompilation';
    $second.err.slurp(:close);
}

# vim: expandtab shiftwidth=4
