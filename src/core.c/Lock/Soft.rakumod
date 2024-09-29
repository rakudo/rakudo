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
        has $!promise;
        has Int $!stack-id;
        has int $!holders;

        my $KEPT-PROMISE := nqp::null();
        method !SET-SELF($kept is raw) {
            $!stack-id := +$*STACK-ID;
            $!holders = 1;
            $!promise :=
                nqp::if($kept,
                    nqp::ifnull($KEPT-PROMISE, ($KEPT-PROMISE := Promise.kept)),
                    Promise.new);
            self
        }

        method new($kept is raw) {
            nqp::create(self)!SET-SELF($kept)
        }

        method acquire {
            ++$!holders;
        }

        method release {
            die "Too many calls to release: " ~ nqp::abs_i($!holders) if --$!holders < 0;
            $!holders
        }

        method keep-promise { $!promise.keep(True) }
    }

    my class ConditionVariable {
        trusts Lock::Soft;

        my class CondNode {
            has $.node;
            has &.predicate;
            has $.promise;

            method !SET-SELF($!node, &!predicate) {
                $!promise := Promise.new;
                self
            }
            method new($node, &predicate) { nqp::create(self)!SET-SELF($node, &predicate) }
        }

        has Lock::Soft $.lock;
        has Mu $!wait-list;
#?if moar
        has atomicint $!signals;
#?endif
#?if !moar
        has $!signals;
#?endif

        method !SET-SELF($lock is raw) {
            $!lock := $lock;
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
            my $stack-id := +$*STACK-ID;

            my $queue := nqp::getattr($!lock, Lock::Soft, '$!queue');
            X::Lock::ConditionVariable::NoMutex.new.throw unless nqp::elems($queue);

            # We need to do nothing if the predicate is already true.
            return if &predicate andthen .();

            my $owner := nqp::atpos($queue, 0 );

            X::Lock::ConditionVariable::WrongThread.new.throw
                unless nqp::getattr($owner, Node, '$!stack-id') == $stack-id;

#?if moar
            $!signals ⚛= 0 unless nqp::elems($!wait-list);
#?endif
#?if !moar
            cas $!signals, { 0 } unless nqp::elems($!wait-list);
#?endif

            my $cnode := CondNode.new($owner, &predicate);
            nqp::push($!wait-list, $cnode);
            # This thread will be awaiting for the condition, release the next one in the queue
            $!lock!Lock::Soft::shift-node(:unlock);
            $*AWAITER.await: $cnode.promise;
        }

        method signal {
#?if moar
            ++⚛$!signals unless ⚛$!signals < 0;
#?endif
#?if !moar
            cas $!signals, { $_ < 0 ?? $_ !! ++$_ };
#?endif
            self!release-waiting if nqp::elems($!wait-list) && $!lock!Lock::Soft::try-acquire-lock;
        }
        method signal_all {
#?if moar
            $!signals ⚛= -1;
#?endif
#?if !moar
            cas $!signals, { -1 };
#?endif
            self!release-waiting if nqp::elems($!wait-list) && $!lock!Lock::Soft::try-acquire-lock;
        }

        # Return true if a waiting thread has been released. We always release only one awaiting mutex, event if
        # signall_all has been called. In the latter case we rely upon unlocking of a previously released mutext to
        # release the next one in the waiting list. And so on until there is any releasable remaining.
        method !release-waiting {
            my $waiting := nqp::elems($!wait-list);

            return False unless $waiting && $!signals;

            loop (my $i = 0; $i < $waiting; ++$i) {
                my $cnode := nqp::atpos($!wait-list, $i);

                if !$cnode.predicate || $cnode.predicate.() {
                    $!wait-list := nqp::splice($!wait-list, nqp::list(), $i, 1);
                    $!lock!Lock::Soft::replace-owner($cnode.node);
#?if moar
                    --⚛$!signals if $!signals > 0;
#?endif
#?if !moar
                    cas $!signals, { $_ > 0 ?? --$_ !! $_ };
#?endif
                    # Release the waiting thread
                    $cnode.promise.keep(True);
                    return True;
                }
            }

            False;
        }
    }

    has $!queue;
    has ConditionVariable $!cond;

    method !SET-SELF {
        $!queue := nqp::list();
        self
    }

    method new {
        nqp::create(nqp::what(self))!SET-SELF
    }

    method !shift-node(:$unlock) {
        loop {
            my $queue := $!queue;
            nqp::shift(my $updated := nqp::clone($queue));
            if nqp::eqaddr(nqp::casattr(self, ::?CLASS, '$!queue', $queue, $updated), $queue) {
                if $unlock {
                    nqp::if(
                        nqp::elems($updated),
                        nqp::getattr(nqp::atpos($updated,0), Node, '$!promise').keep);
                }
                return nqp::atpos($queue, 0);
            }
        }
    }

    method !replace-owner($node is raw) {
        loop {
            my $queue := $!queue;
            my $updated := nqp::clone($queue);
            nqp::bindpos($updated, 0, $node);
            if nqp::eqaddr(nqp::casattr(self, ::?CLASS, '$!queue', $queue, $updated), $queue) {
                return
            }
        }
    }

    method !try-acquire-lock {
        my $queue := $!queue;
        return 0 if nqp::elems($queue);
        my $node := Node.new;
        my $updated := nqp::clone($queue);
        nqp::push($updated, $node);
        nqp::eqaddr(nqp::casattr(self, ::?CLASS, '$!queue', $queue, $updated), $queue)
    }

    # Note that the meaning of returned promise is different from Lock::Async
    method lock(--> Nil) {
#?if !js
        my $stack-id := +$*STACK-ID; # Reduce dynamic lookups by caching
        my $node-kept := nqp::null();
        my $node-unkept := nqp::null();
        my $promise;
        until nqp::defined($promise) {
            my $queue := $!queue;
            my $elems := nqp::elems($queue);
            my $owner := nqp::atpos($queue, 0);
            if $elems and nqp::getattr($owner, Node, '$!stack-id') == $stack-id {
                # Recursive lock
                $owner.acquire;
                return
            }
            my $updated := nqp::clone($queue);
            nqp::push($updated,
                nqp::if(
                    $elems,
                    nqp::ifnull($node-unkept, ($node-unkept := Node.new(0))),
                    nqp::ifnull($node-kept, ($node-kept := Node.new(1)))));
            if nqp::eqaddr(nqp::casattr(self, Lock::Soft, '$!queue', $queue, $updated), $queue) {
                $promise :=
                    nqp::getattr(nqp::if($elems, $node-unkept, $node-kept), Node, '$!promise');
            }
        }
        # Await for our turn unless we're first on the queue
        $*AWAITER.await: $promise;
#?endif
    }

    method unlock(--> Nil) {
#?if !js
        my $stack-id := +$*STACK-ID;
        my $queue := $!queue;
        X::Lock::Unlock::NoMutex.new.throw unless nqp::elems($queue);
        my $owner := nqp::atpos($queue, 0);
        X::Lock::Unlock::WrongThread.new.throw
            unless nqp::getattr($owner, Node, '$!stack-id') == $stack-id;
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
    }
#?endif
#?if js
        $!cond //= ConditionVariable.new(self)
#?endif
}

# vim: expandtab shiftwidth=4
