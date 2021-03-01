use lib <t/packages/>;
use Test;
use Test::Helpers;

plan 11;

subtest '.lang-rev-before method on Perl6::World' => {
    plan 5;
    is-run ｢use v6.c; BEGIN print ?$*W.lang-rev-before: 'd'｣, 'c is before d', :out<True>;
    is-run ｢use v6.c; BEGIN print ?$*W.lang-rev-before: 'c'｣, 'c is not before d', :out<False>;
    is-run ｢use v6.e.PREVIEW; BEGIN print ?$*W.lang-rev-before: 'e'｣, 'e.PREVIEW is not before e', :out<False>;
    is-run ｢use v6.e.PREVIEW; BEGIN print ?$*W.lang-rev-before: 'd'｣, 'e is not before d', :out<False>;
    throws-like ｢BEGIN $*W.lang-rev-before: <6.d>｣, Exception,
        :self{.exception.message.contains: 'must be 1 char long'},
        'using wrong revision format as argument throws';
}

subtest 'IO::Handle.raku.EVAL roundtrips' => {
    plan 7;

    my $orig = IO::Handle.new: :path("foo".IO), :!chomp, :nl-in[<I ♥ Perl 6>],
        :nl-out<foo>, :encoding<ascii>;

    is-deeply IO::Handle.raku.EVAL, IO::Handle, 'type object';
    given $orig.raku.EVAL -> $evaled {
        is-deeply $evaled, $orig, 'instance';
        is-deeply $evaled."$_"(), $orig."$_"(), $_
            for <path  chomp  nl-in  nl-out  encoding>;
    }
}

# https://github.com/MoarVM/MoarVM/issues/971
if $*DISTRO.is-win {
    # https://github.com/Raku/old-issue-tracker/issues/6591
    skip 'code too complex for Win32';
}
else {
    is-run :compiler-args[
        '--profile', '--profile-filename=' ~ make-temp-path.absolute
    ], ｢
        my %functions = (
            1 => sub (@args) { [-] @args },
            4 => sub (@args) { [+] @args }
        );
        sub foo(@ast) {
            %functions{@ast[0]}(@ast.map: {$_ ~~ Array ?? foo $_ !! $_} );
        }([4, [1, 1, 2]]) for ^250;
        print "success";
    ｣, :out{.contains: 'success'}, :err{.contains: 'Writing profiler output'},
    'profiler does not crash';
}

# https://github.com/Raku/old-issue-tracker/issues/6661
# XXX TODO 6.d REVIEW. Setting traits from multiple multies is undefined
# and this test may need to be moved to rakudo's test suite.
eval-lives-ok ｢
    multi infix:<↑> is assoc<right> is tighter(&infix:<**>) { $^n ** $^m }
    multi infix:<↑↑> ($, 0) is assoc<right> is tighter(&infix:<↑>) { 1 }
    multi infix:<↑↑> is assoc<right> is tighter(&infix:<↑>) { [↑] $^n xx $^m }
｣, 'no crash when defining multiple routines with tightnes';

# https://github.com/rakudo/rakudo/issues/1411
-> Positional:D[Int] \z {
    is-deeply z.List, (1, 2, 3),
    'parametarization of a DefiniteHOW does not complain about complex coercers'
}(Array[Int].new: 1, 2, 3);

# https://github.com/rakudo/rakudo/issues/1315
# https://github.com/rakudo/rakudo/issues/1477
# The non-optimizing custom stuff might not be spec material:
# https://colabti.org/irclogger/irclogger_log/perl6-dev?date=2018-02-07#l44
# and with extra comments on https://github.com/rakudo/rakudo/issues/1477#issuecomment-363644261
subtest 'postfix-to-prefix-inc-dec opt does not rewrite custom ops' => {
    plan 5;
    subtest 'custom classes' => {
        plan 2;
        my class A {}
        sub  prefix:<++>(A) { flunk 'postfix increment' }
        sub postfix:<++>(A) { pass  'postfix increment' }
        sub  prefix:<-->(A) { flunk 'postfix decrement' }
        sub postfix:<-->(A) { pass  'postfix decrement' }
        my $var = A.new;
        $var++;
        $var--;
    }
    subtest 'core types (Int)' => {
        plan 2;
        sub  prefix:<++>(Int) { flunk 'postfix increment' }
        sub postfix:<++>(Int) { pass  'postfix increment' }
        sub  prefix:<-->(Int) { flunk 'postfix decrement' }
        sub postfix:<-->(Int) { pass  'postfix decrement' }
        my $var = 42;
        $var++;
        $var--;
    }
    subtest 'core types (Num)' => {
        plan 2;
        sub  prefix:<++>(Num) { flunk 'postfix increment' }
        sub postfix:<++>(Num) { pass  'postfix increment' }
        sub  prefix:<-->(Num) { flunk 'postfix decrement' }
        sub postfix:<-->(Num) { pass  'postfix decrement' }
        my $var = 42e0;
        $var++;
        $var--;
    }
    subtest 'core types (native int)' => {
        plan 2;
        sub  prefix:<++>(int) { flunk 'postfix increment' }
        sub postfix:<++>(int) { pass  'postfix increment' }
        sub  prefix:<-->(int) { flunk 'postfix decrement' }
        sub postfix:<-->(int) { pass  'postfix decrement' }
        my int $var = 42;
        $var++;
        $var--;
    }
    subtest 'core types (native num)' => {
        plan 2;
        sub  prefix:<++>(num) { flunk 'postfix increment' }
        sub postfix:<++>(num) { pass  'postfix increment' }
        sub  prefix:<-->(num) { flunk 'postfix decrement' }
        sub postfix:<-->(num) { pass  'postfix decrement' }
        my num $var = 42e0;
        $var++;
        $var--;
    }
}

{ # https://github.com/rakudo/rakudo/issues/1481
    my @res;
    multi sub foo($x where /{@res.push: $x}./) {}
    multi sub foo($y where /{@res.push: $y}./) {}
    foo 'a';
    foo 'b';
    is-deeply @res, [<a a b b>],
        'regex blocks update their lexical variables right';
}

group-of 2 => 'collation experiment' => {
    is-run ｢$*COLLATION.set: :primary; print 'pass'｣,
        :out<pass>, '$*COLLATION.set no longer requires experimental pragma';
    is-run ｢
        use experimental :collation;
        $*COLLATION.set: :primary;
        print 'pass'
    ｣, :out<pass>, :compiler-args[<-I lib>], 'we can still use the pragma (to support old code)';
}

subtest 'Distribution::Resource can be stringified', {
    lives-ok { Distribution::Resource.raku }, 'Can use .raku';
    lives-ok { Distribution::Resource.Str  }, 'Can use .Str';
    lives-ok { Distribution::Resource.gist }, 'Can use .gist';
}

class ParameterChild is Parameter {
    has $.foobar
}
is ParameterChild.new(foobar => 'Baz').foobar, 'Baz', 'Subclassing of Parameter works';

is Parameter.new(:name('$a'), :type(Int), :optional).perl, 'Int $a?', 'Parameter takes by-name parameters itself';

# vim: expandtab shiftwidth=4
