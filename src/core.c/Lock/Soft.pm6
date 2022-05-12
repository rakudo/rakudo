my class X::Lock::Unlock::NoMutex is Exception {
    method message {
        "Attempt to unlock mutex not held yet"
    }
}

my class X::Lock::Unlock::WrongThread is Exception {
    method message {
        "Attempt to unlock mutex by thread not holding it"
    }
}

my class X::Lock::ConditionVariable::Duplicate is Exception {
    method message {
        "Lock already has a condition variable"
    }
}

my class X::Lock::ConditionVariable::NoMutex is Exception {
    method message {
        "Can only wait on a condition variable when holding mutex"
    }
}

my class X::Lock::ConditionVariable::WrongThread is Exception {
    method message {
        "Can only call wait on the thread which holds the mutex"
    }
}

my class Lock::Soft {
    my class ConditionVariable {...}
    trusts ConditionVariable;

    my class Node {
        has $.promise;
        has $.vow;
        has Int $.stack-id;
        has int $.holders;

        method !SET-SELF {
            $!stack-id = +$*STACK-ID;
            $!holders = 1;
            $!vow = ($!promise = Promise.new).vow;
            self
        }

        method new {
            nqp::create(self)!SET-SELF
        }

        method acquire {
            ++$!holders;
        }

        method release {
            die "Too many calls to release: " ~ nqp::abs_i($!holders) if --$!holders < 0;
            $!holders
        }
    }

    my class Threads {
        has $!queue is built(:bind) = nqp::list();

        method push(Node:D $node) is raw {
            my $queue := nqp::clone($!queue);
            nqp::push($queue, $node);
            nqp::p6bindattrinvres(nqp::create(Threads), Threads, '$!queue', $queue)
        }

        method shift is raw {
            my $queue := nqp::clone($!queue);
            nqp::shift($queue);
            nqp::p6bindattrinvres(nqp::create(Threads), Threads, '$!queue', $queue)
        }

        method replace-head(Node:D $node) is raw {
            my $queue := nqp::clone($!queue);
            nqp::bindpos($queue, 0, $node);
            nqp::p6bindattrinvres(nqp::create(Threads), Threads, '$!queue', $queue)
        }

        method head is raw { nqp::atpos($!queue, 0) }

        method elems { nqp::elems($!queue) }
    }

    my class ConditionVariable {
        trusts Lock::Soft;

        my class CondNode {
            has $.node;
            has &.predicate;
            has $.promise;

            method !SET-SELF($!node, &!predicate) {
                $!promise = Promise.new;
                self
            }
            method new($node, &predicate) { nqp::create(self)!SET-SELF($node, &predicate) }
        }

        has Lock::Soft $.lock is built(:bind);
        has Mu $!wait-list;
        has atomicint $!signals;

        method !SET-SELF($!lock) {
            $!wait-list := nqp::list();
            $!signals = 0;
            self
        }

        method new(Lock::Soft:D $lock) {
            X::Lock::ConditionVariable::Duplicate.new.throw with nqp::getattr(nqp::decont($lock), Lock::Soft, '$!cond');
            nqp::create(self)!SET-SELF($lock)
        }

        # Since wait can only be called from within a held mutex there is no need to protect condition variable
        # structure integrity.
        method wait(&predicate?) {
            my $stack-id = +$*STACK-ID;

            X::Lock::ConditionVariable::NoMutex.new.throw unless $!lock!Lock::Soft::thread-elems;

            # We need to do nothing if the predicate is already true.
            return if &predicate andthen .();

            my $owner := $!lock!Lock::Soft::owner;

            X::Lock::ConditionVariable::WrongThread.new.throw unless $owner.stack-id == $stack-id;

            $!signals ⚛= 0 unless nqp::elems($!wait-list);

            my $cnode = CondNode.new($owner, &predicate);
            nqp::push($!wait-list, $cnode);
            # This thread will be awaiting for the condition, release the next one in the queue
            $!lock!Lock::Soft::shift-node(:unlock);
            $*AWAITER.await: $cnode.promise;
        }

        method signal {
            ++⚛$!signals unless ⚛$!signals < 0;
            self!release-waiting if nqp::elems($!wait-list) && $!lock!Lock::Soft::try-acquire-lock;
        }
        method signal_all {
            $!signals ⚛= -1;
            self!release-waiting if nqp::elems($!wait-list) && $!lock!Lock::Soft::try-acquire-lock;
        }

        # Return true if a waiting thread has been released. We always release only one awaiting mutex, event if
        # signall_all has been called. In the latter case we rely upon unlocking of a previously released mutext to
        # release the next one in the waiting list. And so on until there is any releasable remaining.
        method !release-waiting {
            my $waiting = nqp::elems($!wait-list);

            return False unless $waiting && $!signals;

            loop (my $i = 0; $i < $waiting; ++$i) {
                my $cnode = nqp::atpos($!wait-list, $i);

                if !$cnode.predicate || $cnode.predicate.() {
                    $!wait-list := nqp::splice($!wait-list, nqp::list(), $i, 1);
                    $!lock!Lock::Soft::replace-owner($cnode.node);
                    --⚛$!signals if $!signals > 0;
                    # Release the waiting thread
                    $cnode.promise.keep(True);
                    return True;
                }
            }

            False;
        }
    }

    has Threads $!threads .= new;
    has ConditionVariable $!cond is built(:bind);

    method !shift-node(:$unlock) {
        loop {
            my $updated := (my $threads := ⚛$!threads).shift;
            if nqp::eqaddr(nqp::cas($!threads, $threads, $updated), $threads) {
                if $unlock {
                    .vow.keep(True) with $updated.head;
                }
                return $threads.head
            }
        }
    }

    method !replace-owner($node) {
        loop {
            my $updated := (my $threads := ⚛$!threads).replace-head($node);
            if nqp::eqaddr(nqp::cas($!threads, $threads, $updated), $threads) {
                return
            }
        }
    }

    method !try-acquire-lock {
        my $threads := ⚛$!threads;
        return 0 if $threads.elems;
        my $node = Node.new;
        my $updated := $threads.push($node);
        nqp::eqaddr(nqp::cas($!threads, $threads, $updated), $threads)
    }

    method !thread-elems { (⚛$!threads).elems }

    method !owner { (⚛$!threads).head }

    # Note that the meaning of returned promise is different from Lock::Async
    method lock(--> Nil) {
#?if !js
        my $stack-id = +$*STACK-ID; # Reduce dynamic lookups by caching
        my $node;
        my $promise;
        until nqp::defined($promise) {
            my $threads := ⚛$!threads;
            my $owner = $threads.head;
            if nqp::defined($owner) and $owner.stack-id == $stack-id {
                # Recursive lock
                $owner.acquire;
                return
            }
            $node //= Node.new;
            my $updated := $threads.push($node);
            if nqp::eqaddr(nqp::cas($!threads, $threads, $updated), $threads) {
                $promise = $node.promise;
                $node.vow.keep(True) unless $threads.elems;
            }
        }
        # Await for our turn unless we're first on the queue
        $*AWAITER.await: $promise;
#?endif
    }

    method unlock(--> Nil) {
#?if !js
        my $stack-id = +$*STACK-ID;
        my $threads := ⚛$!threads;
        X::Lock::Unlock::NoMutex.new.throw unless $threads.elems;
        my $owner = $threads.head;
        X::Lock::Unlock::WrongThread.new.throw unless $stack-id == $owner.stack-id;
        unless $owner.release {
            # We only pull the owner from the list when no condition is to be fulfilled. In the later case the owner
            # item will be replaced with a waiting thread.
            unless nqp::defined($!cond) && $!cond!ConditionVariable::release-waiting {
                self!shift-node(:unlock);
            }
            return
        }
#?endif
    }

    proto method protect(|) {*}
    multi method protect(::?CLASS:D: &code --> Mu) is raw {
#?if !js
        self.lock;
        LEAVE self.unlock;
#?endif
        code()
    }

    method condition {
#?if !js
        without ⚛$!cond {
            nqp::cas($!cond, ConditionVariable, ConditionVariable.new(self));
        }
        $!cond
#?endif
#?if js
        $!cond //= ConditionVariable.new(self)
#?endif
    }
}

# vim: expandtab shiftwidth=4
