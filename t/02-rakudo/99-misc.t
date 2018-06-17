use Test;
plan 1;

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
