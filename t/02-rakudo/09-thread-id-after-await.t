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
    nqp::continuationreset(PROMPT, nqp::getattr({
        sub foo() {
            diag $*FOO;
            nqp::continuationcontrol(0, PROMPT, nqp::getattr(-> \cont {
                $c.send(Wrapper.new(cont => cont));
            }, Code, '$!do'));
            $p.keep($*FOO);
        };
        foo();
    }, Code, '$!do'));
});

my $t2 = Thread.start({
    my $*FOO = 'correct';
    nqp::continuationinvoke($c.receive.cont, nqp::null)
});

is await($p), 'correct',
    'Correct dynamic seen after invokving continuation on different thread';

# vim: expandtab shiftwidth=4
