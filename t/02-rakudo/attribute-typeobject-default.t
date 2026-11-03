use Test;

plan 5;

# An attribute whose default is a type object (e.g. `has T $.x = T`) must be
# initialized at construction. If the build is left not concrete, the metamodel
# does not initialize the attribute at construction, and the slot is vivified
# lazily on the first read. That vivification is not synchronized, so it
# fragments lock-free structures under concurrency, and an untyped attribute
# reads back as Any.

my class U { has $.x is rw = Int; }
is U.new.x.^name, 'Int',
    'untyped attribute whose default is a type object is initialized to that type';

my class N { has N $.next is rw = N; }
is N.new.next.WHICH, N.WHICH,
    'typed attribute whose default is its own type object holds that object';

my class C { has Int $.n is rw = 42; }
is C.new.n, 42, 'a concrete default still works';

my class M {}
my class P { has M $.m is rw = M; }
is P.new.m.^name, 'M', 'a default that is a sibling type object is initialized';

# The concurrency symptom: a lock-free Michael-Scott queue whose nodes default
# `.next` to the type object. Without initialization at construction the chain
# fragments.
my class Node { has $.value; has Node $.next is rw = Node; }
my class Queue {
    has Node $.head is rw;
    has Node $.tail is rw;
    submethod BUILD(--> Nil) { $!head = $!tail = Node.new }
    method enqueue($value) {
        my $node = Node.new(:$value);
        my $tail;
        loop {
            $tail = $!tail;
            my $next = $tail.next;
            if $tail === $!tail {
                if $next.DEFINITE { cas($!tail, $tail, $next) }
                else { last if cas($tail.next, $next, $node) === $next }
            }
        }
        cas($!tail, $tail, $node);
    }
    method depth {
        my $n = $!head.next;
        my $c = 0;
        while $n.DEFINITE { $c++; $n = $n.next }
        $c
    }
}
my $queue = Queue.new;
my constant THREADS = 4;
my constant PER = 10000;
await do for ^THREADS { start { $queue.enqueue($_) for ^PER } }
is $queue.depth, THREADS * PER,
    'concurrent lock-free enqueue does not fragment the chain';

# vim: expandtab shiftwidth=4
