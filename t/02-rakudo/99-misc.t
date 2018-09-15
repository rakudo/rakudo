use Test;
plan 2;

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
