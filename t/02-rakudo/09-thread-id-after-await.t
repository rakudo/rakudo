use v6.d;
use Test;
use nqp;

plan 1;

# This test file covers a bug where dynamic lexical caching resulted in a
# continuation seeing the wrong value after being invoked. This caused issues
# with getting the wrong $*THREAD. It was reported in:
#   https://github.com/Raku/old-issue-tracker/issues/6390

my $c = Channel.new;
my $p = Promise.new;

my class Wrapper {
    has Mu $.cont;
}

my $t1 = Thread.start({
    my constant PROMPT = Mu.new;
    my $*FOO = 'original';
    nqp::continuationreset(PROMPT, {
        sub foo() {
            diag $*FOO;
            nqp::continuationcontrol(0, PROMPT, -> \cont {
                $c.send(Wrapper.new(cont => cont));
            });
            $p.keep($*FOO);
        };
        foo();
    });
});

my $t2 = Thread.start({
    my $*FOO = 'correct';
    nqp::continuationinvoke($c.receive.cont, nqp::null)
});

is await($p), 'correct',
    'Correct dynamic seen after invokving continuation on different thread';

# vim: expandtab shiftwidth=4
