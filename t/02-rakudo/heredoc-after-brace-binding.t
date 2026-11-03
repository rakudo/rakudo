use MONKEY-SEE-NO-EVAL;
use lib <t/packages/Test-Helpers>;
use Test;
use Test::Helpers;
use nqp;

plan 67;

unless nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast' {
    skip-rest 'the legacy frontend binds a heredoc body where it is parsed';
    exit;
}

# A heredoc body is parsed after its line ends but runs where the heredoc
# starts. A name it uses must mean the same declaration in both places, so a
# name shadowed in only one of them is ambiguous. Names the compiler declares
# for the code it runs in, such as self and the topic, bind where it starts.

throws-like q:to/CODE/, X::Syntax::Heredoc::AmbiguousName, symbol => '$a', 'a heredoc body after the brace of a sub cannot use a name the sub shadows';
    my $a = "outer";
    sub f() { my $a = "inner"; qq:to/END/ }
        $a
        END
    CODE

throws-like q:to/CODE/, X::Syntax::Heredoc::AmbiguousName, symbol => '$b', 'a heredoc body after the brace of an if block cannot use a name the block shadows';
    my $b = "outer";
    sub g() { if True { my $b = "inner"; qq:to/END/ } }
        $b
        END
    CODE

throws-like q:to/CODE/, X::Syntax::Heredoc::AmbiguousName, symbol => '$c', 'a block interpolated in a heredoc body cannot use a name the sub shadows';
    my $c = "outer";
    sub h() { my $c = "inner"; qq:to/END/ }
        { $c }
        END
    CODE

throws-like q:to/CODE/, X::Syntax::Heredoc::AmbiguousName, symbol => '$d', 'a heredoc body after the brace of a pointy block cannot use a name the block shadows';
    my $d = "outer";
    my &p = -> { my $d = "inner"; qq:to/END/ };
        $d
        END
    CODE

throws-like q:to/CODE/, X::Syntax::Heredoc::AmbiguousName, symbol => '$e', 'a heredoc that does not end its line cannot use a name its sub shadows';
    my $e = "outer";
    sub mid() { my $e = "inner"; my $x = qq:to/END/; $x }
        $e
        END
    CODE

throws-like q:to/CODE/, X::Syntax::Heredoc::AmbiguousName, symbol => 'K', 'a block in a heredoc body cannot use a class the sub shadows';
    my class K { method n() { "outer" } }
    sub kf() { my class K { method n() { "inner" } }; qq:to/END/ }
        [{ K.n }]
        END
    CODE

throws-like q:to/CODE/, X::Syntax::Heredoc::AmbiguousName, symbol => '$n', 'a heredoc started inside a heredoc body cannot use a name the sub of the outer heredoc shadows';
    my $n = "outer";
    sub nf() { my $n = "inner"; qq:to/A/ }
        [{ qq:to/B/
            $n
            B
        }]
        A
    CODE

is (try EVAL q:to/CODE/), "[inner]\n", 'a heredoc body after the brace of a sub sees that sub as its routine';
    sub outer-routine() { sub inner() { qq:to/END/ }
        [{ &?ROUTINE.name }]
        END
    inner() }
    outer-routine()
    CODE

is (try EVAL q:to/CODE/), "[inner]\n", 'a BEGIN time EVAL in a heredoc body resolves where the heredoc starts';
    use MONKEY-SEE-NO-EVAL;
    constant $e = "outer";
    sub ef() { my constant $e = "inner"; qq:to/END/ }
        [{ BEGIN EVAL q[$e] }]
        END
    ef()
    CODE

throws-like q:to/CODE/, X::Syntax::Heredoc::AmbiguousName, symbol => '$o', 'a heredoc body parsed inside a block opened after the heredoc cannot use a name that block shadows';
    my $o = "outer";
    my $s = qq:to/END/ ~ do { my $o = "inner";
        [$o]
        END
    $o };
    CODE

is (try EVAL q:to/CODE/), "a X\n", 'a heredoc body parsed at the start of a block opened after the heredoc interpolates a lexical';
    my $x = "X";
    my $got = "";
    for qq:to/END/.lines -> $line {
        a $x
        END
        $got ~= "$line\n";
    }
    $got
    CODE

is (try EVAL q:to/CODE/), "[X]\n", 'a heredoc body after the brace of a method reads its named arguments';
    my class Named { method m() { qq:to/END/ } }
        [{ %_<x> }]
        END
    Named.m(:x<X>)
    CODE

is (try EVAL q:to/CODE/), "returned", 'a return in a heredoc body returns from the routine the heredoc is in';
    sub early() { my $x = qq:to/END/; "fell through" }
        [{ return "returned" }]
        END
    early()
    CODE

throws-like q:to/CODE/, X::Placeholder::Mainline, 'a placeholder in a heredoc body after the brace of its block is refused';
    my &placeholder = { qq:to/END/ }
        [$^a]
        END
    CODE

throws-like q:to/CODE/, X::Placeholder::Mainline, 'a placeholder in a heredoc body after the brace of its sub is refused';
    sub placeholder-sub { qq:to/END/ };
        [@_[]]
        END
    CODE

throws-like q:to/CODE/, X::Placeholder::Mainline, 'a placeholder in a heredoc body after its block ended is refused';
    my @late = (1, 2).map({ qq:to/END/ });
        [$^a]
        END
    CODE

is (try EVAL q:to/CODE/), "[bf]\n[bg]\n", 'two heredocs from different subs on one line each bind where they start';
    sub bf() { qq:to/A/ }; sub bg() { qq:to/B/ }
        [{ &?ROUTINE.name }]
        A
        [{ &?ROUTINE.name }]
        B
    bf() ~ bg()
    CODE

throws-like q:to/CODE/, X::Undeclared, 'a heredoc body cannot use a name only a block opened after the heredoc declares';
    my $s = qq:to/END/ ~ do { my $only-later = "L";
        [$only-later]
        END
    1 };
    CODE

is (try EVAL q:to/CODE/), "[3]\n", 'a variable a heredoc body declares without strict belongs to where the heredoc starts';
    no strict; sub fresh() { qq:to/END/ }
        [$fresh1]
        END
    $fresh1 = 3; fresh()
    CODE

is (try EVAL q:to/CODE/), "[7]\n", 'a heredoc body after the brace of a class reads an attribute of it';
    class Closed { has $.v = 7; method m() { qq:to/END/ } }; my $pad;
        [$!v]
        END
    Closed.new.m
    CODE

is-run q:to/CODE/, :out("[Parsed]\n"), :err(''), 'a heredoc body after the brace of a class parses in the package where the heredoc starts';
    class Parsed { method m() { qq:to/END/ } }; my $pad;
        [{ $?PACKAGE.^name }]
        END
    print Parsed.m
    CODE

is (try EVAL q:to/CODE/), "[5]\n", 'a heredoc body follows the pragmas where the heredoc starts';
    my $r = do { no strict; qq:to/END/ }
        [{ $auto = 5; $auto }]
        END
    $r
    CODE

throws-like q:to/CODE/, X::Syntax::Heredoc::AmbiguousName, symbol => 'C', 'a block in a heredoc body cannot use a constant the sub shadows';
    constant C = "outer";
    sub cf() { my constant C = "inner"; qq:to/END/ }
        [{ C }]
        END
    CODE

throws-like q:to/CODE/, X::Syntax::Heredoc::AmbiguousName, symbol => 'which', 'a block in a heredoc body cannot call a sub the sub shadows';
    sub which() { "outer" }
    sub sf() { my sub which() { "inner" }; qq:to/END/ }
        [{ which() }]
        END
    CODE

throws-like q:to/CODE/, X::Syntax::Heredoc::AmbiguousName, symbol => '&which', 'a block in a heredoc body cannot call through the variable of a sub the sub shadows';
    sub which() { "outer" }
    sub sf() { my sub which() { "inner" }; qq:to/END/ }
        [{ &which() }]
        END
    CODE

throws-like q:to/CODE/, X::Syntax::Heredoc::AmbiguousName, symbol => 't', 'a block in a heredoc body cannot use a sigilless term the sub shadows';
    my \t = "outer";
    sub tf() { my \t = "inner"; qq:to/END/ }
        [{ t }]
        END
    CODE

throws-like q:to/CODE/, X::Syntax::Heredoc::AmbiguousName, symbol => 'K', 'a block in a heredoc body cannot declare a variable typed by a class the sub shadows';
    my class K { method n() { "outer" } }
    sub kf() { my class K { method n() { "inner" } }; qq:to/END/ }
        [{ my K $k; 1 }]
        END
    CODE

throws-like q:to/CODE/, X::Syntax::Heredoc::AmbiguousName, symbol => '$n', 'a heredoc body cannot use a native lexical the sub shadows';
    my int $n = 42;
    sub native() { my str $n = "s"; qq:to/END/ }
        $n
        END
    CODE

throws-like q:to/CODE/, X::Syntax::Heredoc::HiddenName, symbol => 'hs', 'a block in a heredoc body after a brace cannot call a sub declared only inside that block';
    sub hidden-call() { my sub hs() { "hs" }; qq:to/END/ }
        [{ hs() }]
        END
    CODE

is (try EVAL q:to/CODE/), "[fx]\n", 'a heredoc body after the brace of an inner block uses a name of the block that is still open';
    sub open() {
        my $x = "fx";
        if True { qq:to/END/ }
            [$x]
            END
    }
    open()
    CODE

is (try EVAL q:to/CODE/), "item a\nitem b\n", 'a heredoc body after the brace of a for loop binds the loop topic';
    my @got;
    for <a b> { @got.push: qq:to/END/ }
        item $_
        END
    @got.join
    CODE

is (try EVAL q:to/CODE/), "changed\n", 'a heredoc body after a brace reads the current value of an outer lexical';
    my $outer-only = "outer";
    sub reads() { qq:to/END/ }
        $outer-only
        END
    $outer-only = "changed";
    reads()
    CODE

throws-like q:to/CODE/, X::Syntax::Heredoc::AmbiguousName, symbol => '$late', 'a heredoc body after a brace cannot use a name its sub declares after the heredoc starts';
    my $late = "outer";
    sub lf() { my $x = qq:to/END/; my $late = "inner"; $x }
        [{ $late }]
        END
    CODE

throws-like q:to/CODE/, X::Syntax::Heredoc::HiddenName, symbol => 'ls', 'a heredoc body after a brace cannot call a sub its sub declares after the heredoc starts';
    sub late-call() { my $s = qq:to/END/; my sub ls() { "ls" }; $s }
        [{ ls() }]
        END
    CODE

throws-like q:to/CODE/, X::Syntax::Heredoc::AmbiguousName, symbol => 'which-late', 'a heredoc body after a brace cannot call a sub its sub shadows after the heredoc starts';
    sub which-late() { "outer" }
    sub wl() { my $s = qq:to/END/; my sub which-late() { "inner" }; $s }
        [{ which-late() }]
        END
    CODE

throws-like q:to/CODE/, X::Syntax::Heredoc::AmbiguousName, symbol => '&which-amp', 'a heredoc body after a brace cannot call through the variable of a sub its sub shadows after the heredoc starts';
    sub which-amp() { "outer" }
    sub wa() { my $s = qq:to/END/; my sub which-amp() { "inner" }; $s }
        [{ &which-amp() }]
        END
    CODE

is (try EVAL q:to/CODE/), "[g]\n", 'a heredoc body calls a sub declared later on its line in the scope it is written in';
    sub fl() { "outer" }
    sub gl() {
        my $s = qq:to/END/; my sub fl() { "g" };
            [{ fl() }]
            END
        $s
    }
    gl()
    CODE

throws-like q:to/CODE/, X::Syntax::Heredoc::AmbiguousName, symbol => '&later-sub', 'a heredoc body cannot call a sub that the block opened after the heredoc declares after the body';
    sub later-sub() { "outer" }
    my $s = qq:to/END/; if True {
        [{ later-sub() }]
        END
        sub later-sub() { "inner" }
    }
    CODE

throws-like q:to/CODE/, X::Syntax::Heredoc::AmbiguousName, symbol => '&later-multi', 'a heredoc body cannot call a sub that a multi in the block opened after the heredoc shadows after the body';
    sub later-multi() { "outer" }
    my $s = qq:to/END/; if True {
        [{ later-multi() }]
        END
        multi later-multi() { "inner" }
    }
    CODE

throws-like q:to/CODE/, X::Syntax::Heredoc::AmbiguousName, symbol => '&uc', 'a heredoc body cannot call a setting sub that the block opened after the heredoc declares after the body';
    my $s = qq:to/END/; if True {
        [{ uc "x" }]
        END
        sub uc(|) { "mine" }
    }
    CODE

throws-like q:to/CODE/, X::Syntax::Heredoc::AmbiguousName, symbol => '$later-var', 'a heredoc body cannot use a name that the block opened after the heredoc declares after the body';
    my $later-var = "outer";
    my $s = qq:to/END/; if True {
        [$later-var]
        END
        my $later-var = "inner";
    }
    CODE

throws-like q:to/CODE/, X::Syntax::Heredoc::HiddenName, symbol => '$only-inside', 'a heredoc body after a brace cannot use a name declared only inside that block';
    sub hidden() { my $only-inside = 1; qq:to/END/ }
        $only-inside
        END
    CODE

is (try EVAL q:to/CODE/), "[3] [3]\n", 'a heredoc body after a brace sees what it declares itself';
    sub own() { qq:to/END/ }
        [$(my $z = 3)] [$z]
        END
    own()
    CODE

throws-like q:to/CODE/, X::Redeclaration, 'a heredoc body redeclaring a name its line declared after the heredoc is a redeclaration';
    my $s = qq:to/END/; my constant C = 1;
        [$(my constant C = 2)]
        END
    CODE

is (try EVAL q:to/CODE/), "[5]\n5", 'a heredoc body declares into the scope where the heredoc starts';
    my $s = qq:to/END/;
        [$(my $w = 5)]
        END
    $s ~ $w
    CODE

throws-like q:to/CODE/, X::Syntax::Heredoc::HiddenName, symbol => 'Hidden', 'a heredoc body after a brace cannot use a class declared only inside that block';
    sub hidden() { my class Hidden { method n() { 1 } }; qq:to/END/ }
        [{ Hidden.n }]
        END
    CODE

is (try EVAL q:to/CODE/), "[Y]", 'a heredoc body in a method block after its brace reads the method named arguments';
    class Nested { method m() { if True { return qq:to/END/.chomp } }
        [%_<x>]
        END
    }
    Nested.m(:x<Y>)
    CODE

is (try EVAL q:to/CODE/), "1\n[1]\n", 'a later heredoc on a line sees what an earlier heredoc body declared';
    my $a = qq:to/A/; my $b = qq:to/B/;
        $(my $x = 1)
        A
        [$x]
        B
    $a ~ $b
    CODE

is (try EVAL q:to/CODE/), "5\n and 5\n", 'a heredoc body sees what a heredoc started inside it declared';
    my $s = qq:to/A/;
        $( qq:to/B/
            $(my $q = 5)
            B
        ) and $q
        A
    $s
    CODE

is (try EVAL q:to/CODE/), "[1] [1\n]\n", 'a heredoc started inside a heredoc body sees what the outer body declared';
    sub declares() { qq:to/A/ }
        [$(my $x = 1)] [{ qq:to/B/
            $x
            B
        }]
        A
    declares()
    CODE

throws-like q:to/CODE/, X::Syntax::Heredoc::HiddenName, 'a heredoc started inside a heredoc body cannot use a name declared only inside the block the outer heredoc closed';
    sub hidden() { my $only-inside = 1; qq:to/A/ }
        [{ qq:to/B/
            $only-inside
            B
        }]
        A
    CODE

throws-like q:to/CODE/, X::Syntax::Heredoc::AmbiguousName, symbol => '$e', 'a heredoc body after the brace of a sub cannot use a constant variable the sub shadows';
    constant $e = "outer";
    sub shadows() { my $e = "inner"; qq:to/END/ }
        [$e]
        END
    CODE

is (try EVAL q:to/CODE/), "[rv] [rv]\n", 'a heredoc body after the brace of a role method reads an attribute and self';
    role HasRv { has $.rv = "rv"; method m() { qq:to/END/ }
        [$!rv] [{ self.rv }]
        END
    }
    class DoesRv does HasRv { }
    DoesRv.new.m
    CODE

is (try EVAL q:to/CODE/), "[v]\n", 'a heredoc body after the brace of a class method called at BEGIN time reads an attribute';
    my $got;
    class Early { has $.v = "v"; method m() { qq:to/END/ } }; my $pad;
        [$!v]
        END
    BEGIN $got = Early.new.m;
    $got
    CODE

is (try EVAL q:to/CODE/), "[Closing] [m]\n", 'a heredoc body after the brace of a class reads the class and routine where the heredoc starts';
    class Closing { method m() { qq:to/END/ } }; my $pad;
        [{ $?CLASS.^name }] [{ &?ROUTINE.name }]
        END
    Closing.m
    CODE

is (try EVAL q:to/CODE/), "[5] [Home::Inner] 5 True", 'a heredoc body after the brace of a class declares into the package where the heredoc starts';
    class Home { method m() { qq:to/END/.chomp } }; my $pad;
        [{ our $o = 5; $o }] [{ class Inner { }; Inner.^name }]
        END
    my $s = Home.m;
    "$s $Home::o {Home::<Inner>:exists}"
    CODE

throws-like q:to/CODE/, X::Syntax::Heredoc::HiddenName, symbol => 'HQ', 'a heredoc body after a brace cannot call into a package declared only inside that block';
    sub hq() { my class HQ { our sub z() { "z" } }; qq:to/END/ }
        [{ HQ::z() }]
        END
    CODE

throws-like q:to/CODE/, X::Syntax::Heredoc::AmbiguousName, symbol => 'HS', 'a heredoc body after a brace cannot call into a package the sub shadows';
    my class HS { our sub z() { "outer" } }
    sub hs() { my class HS { our sub z() { "inner" } }; qq:to/END/ }
        [{ HS::z() }]
        END
    CODE

throws-like q:to/CODE/, X::Syntax::Heredoc::AmbiguousName, symbol => 'HT', 'a heredoc body after a brace cannot use a class inside a package the sub shadows';
    my class HT { our class Inner { method which() { "outer" } } }
    sub ht() { my class HT { our class Inner { method which() { "inner" } } }; qq:to/END/ }
        [{ HT::Inner.which }]
        END
    CODE

throws-like q:to/CODE/, X::Syntax::Heredoc::AmbiguousName, symbol => 'HV', 'a heredoc body after a brace cannot use a variable of a package the sub shadows';
    my class HV { our $pv = "outer" }
    sub hv() { my class HV { our $pv = "inner" }; qq:to/END/ }
        [{ $HV::pv }]
        END
    CODE

is (try EVAL q:to/CODE/), "[n]\n", 'a heredoc body after a brace reaches a class its sub declared with our through the package';
    sub ho() { our class HOur { method n() { "n" } }; qq:to/END/ }
        [{ HOur.n }]
        END
    ho()
    CODE

throws-like q:to/CODE/, X::Syntax::Heredoc::HiddenName, symbol => '$rp', 'a heredoc body after the brace of a role cannot use a parameter of the role';
    role RoleParam[$rp] { method m() { qq:to/END/ } }
        [$rp]
        END
    CODE

throws-like q:to/CODE/, X::Syntax::Heredoc::HiddenName, symbol => '&infix:<±>', 'a heredoc body after a brace cannot use an operator declared only inside that block';
    if True { sub infix:<±>($a, $b) { $a + $b }; my $s = qq:to/END/ }
        [{ 1 ± 2 }]
        END
    CODE

throws-like q:to/CODE/, X::Syntax::Heredoc::AmbiguousName, symbol => '&prefix:<√>', 'a heredoc body after a brace cannot use an operator the block shadows';
    sub prefix:<√>($a) { "outer" }
    if True { sub prefix:<√>($a) { "inner" }; my $s = qq:to/END/ }
        [{ √4 }]
        END
    CODE

throws-like q:to/CODE/, X::Syntax::Heredoc::HiddenName, symbol => '&term:<magic>', 'a heredoc body after a brace cannot use a term declared only inside that block';
    if True { sub term:<magic> { 42 }; my $s = qq:to/END/ }
        [{ magic }]
        END
    CODE

is (try EVAL q:to/CODE/), "[red] [green]\n", 'a heredoc body after a brace reaches an enum the block declared with our through the package';
    sub colors() { enum HColor <red green>; qq:to/END/ }
        [{ red }] [{ HColor::green }]
        END
    colors()
    CODE

is (try EVAL q:to/CODE/), "a <5>\n<%d>", 'a heredoc body after a brace declares in the scope open where it is written';
    my $v = 5;
    my $got = do { if True { qq:to/END/ } };
        a $v.fmt(my $fmt = "<%d>")
        END
    $got ~ $fmt
    CODE
