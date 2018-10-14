use lib <t/packages/>;
use Test;
use Test::Helpers;

plan 5;

subtest '.lang-ver-before method on Perl6::World' => {
    plan 5;
    ok  ｢use v6.c; BEGIN $*W.lang-ver-before: 'd'｣.EVAL, 'c is before d';
    nok ｢use v6.c; BEGIN $*W.lang-ver-before: 'c'｣.EVAL, 'c is not before d';
    nok ｢use v6.d.PREVIEW; BEGIN $*W.lang-ver-before: 'd'｣.EVAL,
        'd is not before d';
    nok ｢use v6.d.PREVIEW; BEGIN $*W.lang-ver-before: 'c'｣.EVAL,
        'd is not before c';
    throws-like ｢BEGIN $*W.lang-ver-before: <6.d>｣, Exception,
        :self{.exception.message.contains: 'must be 1 char long'},
    'using wrong version format as argument throws';
}

subtest 'IO::Handle.perl.EVAL roundtrips' => {
    plan 7;

    my $orig = IO::Handle.new: :path("foo".IO), :!chomp, :nl-in[<I ♥ Perl 6>],
        :nl-out<foo>, :encoding<ascii>;

    is-deeply IO::Handle.perl.EVAL, IO::Handle, 'type object';
    given $orig.perl.EVAL -> $evaled {
        is-deeply $evaled, $orig, 'instance';
        is-deeply $evaled."$_"(), $orig."$_"(), $_
            for <path  chomp  nl-in  nl-out  encoding>;
    }
}

# https://github.com/MoarVM/MoarVM/issues/971
if $*DISTRO.is-win {
    skip 'code too complex for Win32 due to RT#132258';
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

# RT #132710
# XXX TODO 6.d REVIEW. Setting traits from multiple multies is undefined
# and this test may need to be moved to rakudo's test suite. See RT#132710
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
