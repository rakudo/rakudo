use lib <t/packages/Test-Helpers>;
use nqp;
use Test;
use Test::Helpers;

plan 21;

subtest 'Supply.interval with negative value warns' => {
    plan 2;
    CONTROL { when CX::Warn {
        like .message, /'Minimum timer resolution is 1ms'/, 'useful warning';
        .resume;
    }}
    react whenever Supply.interval(-100) {
        pass "intervaled code ran";
        done;
    }
}

if $*DISTRO.is-win {
    # https://github.com/Raku/old-issue-tracker/issues/6591
    skip 'is-run code is too complex to run on Windows';
}
else {
    subtest 'no useless-use warning on return when KEEP/UNDO phasers used' => {
        plan 3;
        is-run ｢
            if  1 { LEAVE 42.uc; Any }; if  1 { LEAVE 42.uc; 42  };
            for 1 { LEAVE 42.uc; Any }; for 1 { LEAVE 42.uc; 42  };
        ｣, :err{ 2|4 == .comb: 'Useless use' },
            'we get warnings with phasers that do not care about return value';

        is-run ｢
            if  1 { KEEP 42.uc; Any }; if  1 { KEEP 42.uc; 42  };
            for 1 { KEEP 42.uc; Any }; for 1 { KEEP 42.uc; 42  };
        ｣, :err(''), 'no warnings with KEEP phaser';

        is-run ｢
            if  1 { UNDO 42.uc; Any }; if  1 { UNDO 42.uc; 42  };
            for 1 { UNDO 42.uc; Any }; for 1 { UNDO 42.uc; 42  };
        ｣, :err(''), 'no warnings with UNDO phaser';
    }
}

if $*DISTRO.is-win {
    # https://github.com/Raku/old-issue-tracker/issues/6591
    skip 'is-run code is too complex to run on Windows';
}
else {
    subtest 'no useless-use warning in andthen/notandthen/orelse/ chains' => {
        plan 2;
        is-run ｢
            1 notandthen 2 notandthen 3  notandthen 4;
            5 andthen    6 andthen    7  andthen    8;
            9 orelse     10 orelse    11 orelse     12;
        ｣, :err{ 3 == .comb: 'Useless use' },
            'we get warnings when last value is useless';

        is-run ｢
            2 notandthen 2 notandthen 2 notandthen 2.uc;
            2 andthen    2 andthen    2 andthen    2.uc;
            2 orelse     2 orelse     2 orelse     2.uc;
        ｣, 'no warnings when last value is useful';
    }
}

# https://github.com/Raku/old-issue-tracker/issues/6244
is-run ｢
    sub prefix:<ᔑ> (Pair $p --> Pair) is tighter(&postcircumfix:<[ ]>) {};
    print postcircumfix:<[ ]>(<foo bar ber>, 1)
｣, :out<bar>, 'no spurious warnings when invoking colonpaired routine';

# https://github.com/Raku/old-issue-tracker/issues/6221
todo 'crashes the JVM', 1 if $*VM.name eq 'jvm';
is-run ｢my $a; $a [R~]= "b"; $a [Z~]= "b"; $a [X~]= "b"｣,
    'metaops + metaassign op do not produce spurious warnings';

# https://github.com/Raku/old-issue-tracker/issues/6253
# https://github.com/Raku/old-issue-tracker/issues/6185
is-run ｢my $ = ^2 .grep: {try 1 after 0}; my $ = {try 5 == 5}()｣,
    'no spurious warnings with `try` thunks in blocks';

is-run ｢my @a; sink @a; my $b := gather { print 'meow' }; sink $b｣,
    :out<meow>, 'no warnings when sinking variables';

todo('Macros NYI') if %*ENV<RAKUDO_RAKUAST>;
is-run ｢use experimental :macros; macro z($) { quasi {} };
    z $; z <x>; print "pass"｣, :compiler-args[<-I lib>], :out<pass>,
    'args to macros do not cause useless use warnings';

# https://github.com/rakudo/rakudo/issues/2554
is-run ｢my @a[Int] = 1,2,3; dd @a｣,
    'ignored shape specification issues a warning',
    :err(/'Ignoring [Int] as shape specification'/);

is-run ｢method m() {}; print "ran"｣,
    'has-scoped method in mainline warns with file and line',
    :out<ran>, :err(/'Useless declaration of a has-scoped method in' .*? 'at -e:1'/);

is-run ｢package Foo { method m() {} }; print "ran"｣,
    'has-scoped method in a package warns with file and line',
    :out<ran>, :err(/'Useless declaration of a has-scoped method in' .*? 'package' .*? 'at -e:1'/);

is-run ｢use fatal; method m() {}; print "ran"｣,
    'use fatal promotes the useless method declaration worry to an error',
    :err(/'Useless declaration of a has-scoped method in'/), :exitcode(1);

if nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast' {
    is-run ｢#| dangling
｣,
        'leading declarator doc without declarand warns with file and line',
        :err(/'Missing declarand for leading declarator doc' .*? 'at -e:1'/);
}
else {
    skip 'legacy frontend does not warn on a dangling leading declarator doc';
}

is-run ｢sub f() is export is export {}; print "ran"｣,
    'duplicate trait warns with file and line',
    :out<ran>, :err(/'Duplicate' .*? 'is export' .*? 'at -e:1'/);

is-run ｢my $.x; print "ran"｣,
    'accessor generation outside a package warns with file and line',
    :out<ran>, :err(/'Useless generation of accessor method in mainline' .*? 'at -e:1'/);

is-run ｢package Foo::Bar { class Foo::Bar {} }; print "ran"｣,
    'class replacing a same-named enclosing package warns with file and line',
    :out<ran>, :err(/'inside an enclosing package of the same name' .*? 'at -e:1'/);

is-run ｢use fatal; package Foo::Bar { class Foo::Bar {} }; print "ran"｣,
    'use fatal promotes the same-named enclosing package worry to an error',
    :err(/'inside an enclosing package of the same name'/), :exitcode(1);

# https://github.com/rakudo/rakudo/issues/6074
is-run ｢print do given 5 { when 5 { 42; 43 } }｣,
    'the last statement of a when block in value position stays wanted',
    :out<43>,
    :err{ .contains('constant integer 42') && !.contains('constant integer 43') };

is-run ｢my $_; class A { }; class B { }; print "ran"｣,
    'a worry followed by package declarations is printed once',
    :out<ran>, :err{ .comb('Potential difficulties').elems == 1 && .comb('Redeclaration').elems == 1 };

is-run ｢my $_; my Int:X $x; class B { }; class C { }; BEGIN note "later"; print "ran"｣,
    'an error known at a package declaration is thrown there with the earlier worry inside it once',
    :out(''), :exitcode(1),
    :err{ !.contains('later')
        && .comb('Invalid type smiley').elems == 1
        && .comb('potential difficulties').elems == 1
        && .comb('Redeclaration').elems == 1 };

is-run ｢my $_; class A { }; foo()｣,
    'a worry before a package declaration survives an error at the end of the unit',
    :exitcode(1),
    :err{ .comb('Redeclaration').elems == 1 && .contains('Undeclared routine') };

# vim: expandtab shiftwidth=4
