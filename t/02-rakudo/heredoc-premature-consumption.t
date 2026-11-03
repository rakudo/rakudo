use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;
use nqp;
use lib <t/packages/02-rakudo/lib>;
use HeredocParameterDefault;

plan 46;

my $rakuast := nqp::ifnull(nqp::gethllsym('Raku', 'COMPILER-FRONTEND'), '') eq 'rakuast';

# A heredoc body is parsed once its line ends. Code that needs the heredoc
# before then dies.

throws-like q:to/CODE/, X::Comp::BeginTime, message => /'Premature heredoc consumption'/, 'a BEGIN reading a heredoc before its line ends dies';
    my $early;
    BEGIN { $early = q:to/END/ }; my $after;
        body
        END
    CODE

throws-like q:to/CODE/, X::Comp::BeginTime, message => /'Premature heredoc consumption'/, 'a constant holding a heredoc before its line ends dies';
    constant EARLY = q:to/END/; my $after;
        body
        END
    CODE

throws-like q:to/CODE/, X::Comp, message => /'Premature heredoc consumption'/, line => 3, 'a role parameterized by a heredoc before its line ends dies at the trait';
    role Early[$s] { method s() { $s } }
    class EarlyUser
      does Early[q:to/END/] { }
        body
        END
    CODE

throws-like q:to/CODE/, X::Comp, message => /'Premature heredoc consumption'/, line => 2, 'a role parameterized by a heredoc in a type dies at the heredoc';
    role Typed[$s] { method s() { $s } }
    my Typed[q:to/END/] $typed;
        body
        END
    CODE

throws-like q:to/CODE/, X::Comp, message => /'Premature heredoc consumption'/, line => 2, 'a heredoc after another role argument in a type dies at the heredoc';
    role Pair[$s, $t] { method s() { $s } }
    my Pair[1, q:to/END/] $typed;
        body
        END
    CODE

throws-like q:to/CODE/, X::Comp, message => /'Premature heredoc consumption'/, line => 2, 'a heredoc role argument in a parameter type dies at the heredoc';
    role Typed[$s] { method s() { $s } }
    sub typed(Typed[q:to/END/] $x) { }; my $after;
        body
        END
    CODE

throws-like q:to/CODE/, X::Syntax::Extension::TooComplex, line => 2, 'an operator named by a heredoc before its line ends is too complex';
    my $pad;
    sub infix:[q:to/END/] ($a, $b) { 42 }
        op
        END
    CODE

throws-like q:to/CODE/, X::Syntax::Extension::TooComplex, line => 2, 'a method named by a heredoc before its line ends is too complex';
    my $pad;
    class MethodNamed { method m:sym(q:to/END/) { 42 } }
        name
        END
    CODE

throws-like q:to/CODE/, X::Syntax::Extension::TooComplex, line => 2, 'a token named by a heredoc before its line ends is too complex';
    my $pad;
    grammar TokenNamed { proto token t {*}; token t:sym(q:to/END/) { x } }
        x
        END
    CODE

throws-like q:to/CODE/, Exception, message => /'Premature heredoc consumption'/, 'an enum value from a heredoc before its line ends dies';
    enum EarlyEnum (a => q:to/END/); my $after;
        body
        END
    CODE

throws-like q:to/CODE/, Exception, message => /'Premature heredoc consumption'/, 'a container default from a heredoc before its line ends dies';
    my $early is default(q:to/END/); my $after;
        body
        END
    CODE

throws-like q:to/CODE/, Exception, message => /'Premature heredoc consumption'/, 'a trait argument of a routine from a heredoc before its line ends dies';
    sub early() is DEPRECATED(q:to/END/) { }; my $after;
        body
        END
    CODE

# Code in a closure or a WhateverCode passed as a role argument runs later,
# once the heredoc has its body.

is (try EVAL q:to/CODE/), "block\n", 'a block passed as a role argument reads the heredoc body when it runs';
    role CalledBlock[&c] { method run() { c() } }
    class Blocky does CalledBlock[{ q:to/END/ }] { }
        block
        END
    Blocky.run
    CODE

is (try EVAL q:to/CODE/), "pointy\n", 'a pointy block passed as a role argument reads the heredoc body when it runs';
    role CalledPointy[&c] { method run() { c() } }
    class Pointy does CalledPointy[-> { q:to/END/ }] { }
        pointy
        END
    Pointy.run
    CODE

is (try EVAL q:to/CODE/), "a whatever\n", 'a WhateverCode passed as a role argument reads the heredoc body when it runs';
    role CalledStar[&c] { method run() { c("a ") } }
    class Starry does CalledStar[* ~ q:to/END/] { }
        whatever
        END
    Starry.run
    CODE

# A role body compiles when the role closes, but waits for a heredoc in it.
if $rakuast {
    is (try EVAL q:to/CODE/), "[2]\n", 'a heredoc in a role that closes before its line ends reads its body once composed';
        role Closed { method m() { qq:to/END/ } }; my $pad;
            [{ 1 + 1 }]
            END
        class Composed does Closed { }
        Composed.m
        CODE

    is (try EVAL q:to/CODE/), "param\n", 'a heredoc in a parameterized role that closes before its line ends reads its body once composed';
        role ClosedParam[$x] { method m() { q:to/END/ } }; my $pad;
            param
            END
        class ComposedParam does ClosedParam[1] { }
        ComposedParam.m
        CODE

    is (try EVAL q:to/CODE/), "default\n", 'a heredoc attribute default in a role that closes before its line ends reads its body once composed';
        role ClosedDefault { has $.a = q:to/END/ }; my $pad;
            default
            END
        class ComposedDefault does ClosedDefault { }
        ComposedDefault.new.a
        CODE

    is (try EVAL q:to/CODE/), "[2]\n", 'a heredoc in a role composed before its line ends reads its body when run';
        role ClosedEarly { method m() { qq:to/END/ } }; class ComposedEarly does ClosedEarly { }
            [{ 1 + 1 }]
            END
        ComposedEarly.m
        CODE
}
else {
    skip 'the legacy frontend compiles a role body before its line ends', 4;
}

is (try EVAL q:to/CODE/), "body\n", 'a BEGIN block ending its line reads the heredoc body';
    my $got;
    BEGIN { $got = q:to/END/ }
        body
        END
    $got
    CODE

is (try EVAL q:to/CODE/), "body\n", 'a constant ending its line holds the heredoc body';
    constant GOT = q:to/END/;
        body
        END
    GOT
    CODE

is (try EVAL RakuAST::Heredoc.new(
  :segments[RakuAST::StrLiteral.new("built\n")], :processors['heredoc',], :stop("END\n")
)), "built\n", 'a heredoc node built in code with the heredoc processor holds its value';

# A default with no value at BEGIN time is thunked. One whose value is known
# by the time code is generated binds as a literal, in the parameter's
# meta-object too.

is (try EVAL q:to/CODE/), "positional\n", 'the meta-object of a heredoc parameter default holds its body';
    sub positional($x = q:to/END/) { $x }
        positional
        END
    &positional.signature.params[0].default.()
    CODE

is (try EVAL q:to/CODE/), "sum 2\n", 'a heredoc parameter default with an interpolated block binds its body';
    sub interpolated($x = qq:to/END/) { $x }
        sum {1 + 1}
        END
    interpolated()
    CODE

is (try EVAL q:to/CODE/), "sub body\n", 'a heredoc default of a parameter in a sub signature binds its body';
    sub captured(|c($x = q:to/END/)) { $x }
        sub body
        END
    captured()
    CODE

is (try EVAL q:to/CODE/), -1, 'a folded default of a parameter in a sub signature binds its value';
    sub captured(|c($x = -1)) { $x }
    captured()
    CODE

is (try EVAL q:to/CODE/), "7 -1", 'a folded default in a declarator signature binds its value';
    my ($p, $q = -1) := \(7);
    "$p $q"
    CODE

is (try EVAL q:to/CODE/), "7 decl\n", 'a heredoc default in a declarator signature binds its body';
    my ($p, $q = q:to/END/) := \(7); my $after;
        decl
        END
    "$p $q"
    CODE

is &heredoc-default.signature.params[0].default.(), "from the module\n", 'the meta-object of a precompiled heredoc default holds its body';

is heredoc-default(), "from the module\n", 'a heredoc parameter default binds its body in a precompiled module';

is (try EVAL q:to/CODE/), "positional\n", 'a heredoc positional parameter default binds its body';
    sub positional($x = q:to/END/) { $x }
        positional
        END
    positional()
    CODE

is (try EVAL q:to/CODE/), "typed\n", 'a heredoc default of a typed parameter binds its body';
    sub typed(Str $x = q:to/END/) { $x }
        typed
        END
    typed()
    CODE

is (try EVAL q:to/CODE/), "native\n", 'a heredoc default of a native parameter binds its body';
    sub native(str $x = q:to/END/) { $x }
        native
        END
    native()
    CODE

is (try EVAL q:to/CODE/), -1, 'a folded parameter default binds its value';
    sub folded($x = -1) { $x }
    folded()
    CODE

# Errors from BEGIN time code report the line they come from.

throws-like q:to/CODE/, X::LibNone, line => 2, 'a pragma error reports its line';
    my $pad;
    use lib;
    CODE

throws-like q:to/CODE/, X::Syntax::Extension::TooComplex, line => 2, 'an operator named by an interpolated string is too complex at its line';
    my $x = 1;
    sub infix:["a$x"] (
      $a, $b
    ) { 42 }
    CODE

throws-like q:to/CODE/, X::Comp, line => 3, 'an EVAL error in a pragma argument keeps its own line';
    use MONKEY-SEE-NO-EVAL;
    use lib EVAL(q[

    1 +]);
    CODE

if $rakuast {
    throws-like q:to/CODE/, X::Comp, message => /'Premature heredoc consumption'/, line => 2, 'a heredoc return value whose routine does not end its line dies';
        my $pad;
        my &returned = sub (--> q:to/END/) { }; my $after;
            ret
            END
        CODE

    throws-like q:to/CODE/, X::Comp, message => /'Premature heredoc consumption'/, line => 2, 'a heredoc as a literal parameter dies';
        my $pad;
        multi literal(q:to/END/) { }
            body
            END
        CODE

    throws-like q:to/CODE/, X::Comp, message => /'Premature heredoc consumption'/, line => 2, 'a use argument heredoc before its line ends dies';
        my $pad;
        use lib q:to/END/; my $after;
            /nonexistent
            END
        CODE

    throws-like q:to/CODE/, X::Syntax::Extension::TooComplex, line => 2, 'a lookup of an operator named by an interpolated string is too complex';
        my $x = 1;
        &infix:["a$x"]
        CODE

    throws-like q:to/CODE/, X::Syntax::Extension::TooComplex, line => 2, 'a variable named by an interpolated string is too complex';
        my $x = 1;
        my &infix:["a$x"] = sub ($a, $b) { 1 };
        CODE

    is (try EVAL q:to/CODE/), "ret\n", 'a heredoc return value after --> binds its body when its routine ends the line';
        sub returned(--> q:to/END/) { }
            ret
            END
        returned()
        CODE

    throws-like q:to/CODE/, X::Comp, message => /'Return value after --> may only be a type or a constant'/, line => 2, 'a heredoc return value whose body interpolates is rejected at the heredoc';
        my $x = 1;
        sub interpolates(--> qq:to/END/) {
            v{$x}
            END
        }
        CODE

    throws-like q:to/CODE/, X::Comp, message => /'Premature heredoc consumption'/, line => 2, 'a heredoc in a do block passed as a role argument dies at the heredoc';
        role Typed[$s] { method s() { $s } }
        my Typed[do { q:to/END/ }] $typed;
            body
            END
        CODE

    throws-like 'my $pad; say q:to/END/', X::Comp, message => /'Ending delimiter END not found'/, 'a heredoc on a last line without a newline is missing its body';
}
else {
    skip 'the legacy frontend fails these with other errors', 9;
}
