use v6.e.PREVIEW;
use nqp;
use Test;

plan 31;

# https://github.com/rakudo/rakudo/issues/1235

# A method form finding a non-Scalar container in the caller's $/ leaves
# it alone rather than inspecting it for the isolation marker.
{
    sub with-native-slash($/ is rw) { "ab".subst("a", "x", :g, :ii) }
    my int $native = 1;
    is with-native-slash($native), "xb",
      'a subst with a native reference bound to $/ works';
}

# The 6.e block-scoped $/ is a RakuAST frontend behavior.
if nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast' {
    {
        is "42".subst(/(\d)/, { $0 * 2 }, :g), "84",
          'a zero-arg subst callback reads the captures of its own match';
    }
    {
        $/ := "meow";
        is "42".subst(/(\d)/, { $0 * 2 }, :g), "84",
          'a subst callback works when the surrounding $/ is bound to a plain value';
    }
    {
        is "ab".match(/(\w)/, :g).map({ ~$0 }).join(","), "a,b",
          'a block invoked with a Match reads that Match through $0';
    }
    {
        "abc" ~~ /(b)/;
        for 1 { "xyz" ~~ /(x)/ }
        is ~$0, "b",
          'a match inside a block does not disturb $/ in the enclosing scope';
    }
    {
        my $got = do if "abc" ~~ /(b)/ { ~$0 };
        is $got, "b",
          'a block with no match of its own reads the enclosing $/';
    }
    {
        "abc" ~~ /(b)/;
        is ~$0, "b",
          'the match operator still sets $/ in its own scope';
    }
    {
        my sub f { { my $/; "abc" ~~ /(b)/; ~$0 } }
        is f(), "b",
          'an explicit my $/ in a block replaces the implicit one';
    }
    {
        "abc" ~~ /(b)/;
        is (try ~$0), "b",
          'a try block reads the enclosing $/';
    }
    {
        is (^200).race(:2batch).map({ S/ (.*) /$0/ }).unique.elems, 200,
          'a substitution in a racing block keeps its match state per frame';
    }
    {
        "old" ~~ /(old)/;
        "abc".match(/(b)/);
        is ~$0, "old",
          'the match method does not write into the surrounding $/';
    }
    {
        "old" ~~ /(old)/;
        "abc".match(/(\w)/, :g);
        is ~$0, "old",
          'the match method with :g does not write into the surrounding $/';
    }
    {
        "old" ~~ /(old)/;
        "abc".match(/(\w)/, :x(2));
        is ~$0, "old",
          'the match method with :x does not write into the surrounding $/';
    }
    {
        "old" ~~ /(old)/;
        grammar P { token TOP { \w+ } }
        P.parse("abc");
        is ~$/, "old",
          'the parse method does not write into the surrounding $/';
    }
    {
        "old" ~~ /(old)/;
        grammar SP { token TOP { \w+ } }
        SP.subparse("abc");
        is ~$/, "old",
          'the subparse method does not write into the surrounding $/';
    }
    {
        "old" ~~ /(old)/;
        "42".subst(/(\d)/, "x");
        is ~$0, "old",
          'the subst method does not write into the surrounding $/';
    }
    {
        "old" ~~ /(old)/;
        "a1c".trans(/(\d)/ => { "x" });
        is ~$0, "old",
          'the trans method with a regex pair does not write into the surrounding $/';
    }
    {
        my int $stable = 0;
        for ^500 {
            "abc" ~~ /(b)/;
            "xyz".match(/(x)/);
            $stable++ if ~$0 eq "b";
        }
        is $stable, 500,
          'method isolation of $/ survives repeated operator writes to the container';
    }
    {
        my @r;
        for "ab".match(/(\w)/, :g).list { @r.push(~$0) }
        is @r.join(","), "a,b",
          'a for block whose topic is a Match reads that Match through $0';
    }
    {
        my $/;
        "old" ~~ /(old)/;
        "abc".match(/(b)/);
        is ~$0, "old",
          'an explicit my $/ at mainline is as isolated as the implicit one';
    }
    {
        my sub f { { my $/; "old" ~~ /(old)/; "abc".match(/(b)/); ~$0 } }
        is f(), "old",
          'an explicit my $/ in a block is as isolated as the implicit one';
    }
    {
        my class Match { has $.x }
        "o" ~~ /(o)/;
        my @r;
        for (Match.new(:x(1)),) { @r.push(~$0) }
        is @r.join, "o",
          'a user type named Match as the topic does not become $/';
    }
    {
        grammar Q { token TOP { \w+ } }
        class QA { method TOP($/) { make $/.Str.subst(/a/, "X") } }
        is Q.parse("abc", :actions(QA)).made, "Xbc",
          'subst works inside an action method with a $/ parameter';
    }
    {
        my @m = "ab".match(/./, :g).list;
        @m.map({ make "X" }).eager;
        is @m.map({ .made // "Nil" }).join(","), "X,X",
          'make in a block whose topic is a Match attaches to that Match';
    }
    {
        my @m = "ab".match(/./, :g).list;
        @m.map({ my $probe := $/; make "Y" }).eager;
        is @m.map({ .made // "Nil" }).join(","), "Y,Y",
          'make attaches to the same Match whether or not the block mentions $/';
    }
    {
        "zz" ~~ /(z)/;
        my @m = "ab".match(/./, :g).list;
        @m.map({ make "X" }).eager;
        is @m.map({ .made // "Nil" }).join(",") ~ "|" ~ ($/.made // "Nil"),
          "X,X|Nil",
          'make prefers the Match topic over a Match in the enclosing $/';
    }
    {
        my @m = "ab".match(/./, :g).list;
        my @r;
        for @m { "zz" ~~ /(z)/; make "F"; @r.push($/.made // "Nil") }
        is @r.join(",") ~ "|" ~ @m.map({ .made // "Nil" }).join(","),
          "F,F|Nil,Nil",
          'make prefers a match the block itself established over the topic';
    }
    {
        my Match $/;
        "old" ~~ /(old)/;
        "abc".match(/(b)/);
        is ~$0, "b",
          'a typed my Match $/ keeps accepting method-form writes';
    }
    {
        my $got = do with "abc".match(/(b)/) { ~$0 };
        is $got, "b",
          'a with block whose topic is a Match reads that Match through $0';
    }
    {
        my $got = do given "xy" { when /(y)/ { ~$0 } };
        is $got, "y",
          'a when block reads the match its own regex condition produced';
    }
    {
        with "abc" { /b/.Bool }
        is $/.defined, False,
          'a bare regex match inside a with block does not leak into the enclosing $/';
    }
}
else {
    skip-rest 'block-scoped $/ needs the RakuAST frontend';
}

# vim: expandtab shiftwidth=4
