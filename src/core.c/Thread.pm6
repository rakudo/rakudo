# Thread represents an OS-level thread. While it could be used directly, it
# is not the preferred way to work in Raku. It's a building block for the
# interesting things.
my class Thread {
    # The VM-level thread handle.
    has Mu $!vm_thread;

    # Is the thread's lifetime bounded by that of the application, such
    # that when it exits, so does the thread?
    has Bool $.app_lifetime;

    # Thread's (user-defined) name.
    has Str $.name;

#?if !jvm
    my atomicint $started;
    my atomicint $aborted;
    my atomicint $completed;
    my atomicint $joined;
    my atomicint $yields;
    my atomicint $highest_id;
#?endif
#?if jvm
    my int $started;
    my int $aborted;
    my int $completed;
    my int $joined;
    my int $yields;
    my int $highest_id;
#?endif

    submethod BUILD(
             :&code!,
      Bool() :$!app_lifetime = False,
      Str()  :$!name         = "<anon>"
      --> Nil
    ) {
        constant THREAD_ERROR = 'Could not create a new Thread: ';
        $!vm_thread := nqp::newthread(
            anon sub THREAD-ENTRY() {
                my $*THREAD = self;
                CONTROL {
                    default {
#?if !jvm
                        ++⚛$aborted;
#?endif
#?if jvm
                        ++$aborted;
#?endif
                        my Mu $vm-ex := nqp::getattr(nqp::decont($_), Exception, '$!ex');
                        nqp::getcomp('Raku').handle-control($vm-ex);
                    }
                }
                code();
#?if !jvm
                ++⚛$completed;
#?endif
#?if jvm
                ++$completed;
#?endif
            },
            $!app_lifetime ?? 1 !! 0);

#?if !jvm
            $highest_id ⚛= nqp::threadid($!vm_thread);
#?endif
#?if jvm
            $highest_id = nqp::threadid($!vm_thread);
#?endif

        CATCH {
            when X::AdHoc {
                .payload.starts-with(THREAD_ERROR)
                  ?? X::Exhausted.new(
                       :what<thread>,
                       :reason(.payload.substr(THREAD_ERROR.chars))
                     ).throw
                  !! .rethrow
            }
        }
    }

    method start(Thread:U: &code, *%adverbs) {
        Thread.new(:&code, |%adverbs).run()
    }

    method run(Thread:D:) {
#?if !jvm
        ++⚛$started;
#?endif
#?if jvm
        ++$started;
#?endif
        nqp::threadrun($!vm_thread);
        self
    }

    method id(Thread:D:) {
        nqp::p6box_i(nqp::threadid($!vm_thread));
    }

    method finish(Thread:D:) {
        nqp::threadjoin($!vm_thread);
#?if !jvm
        ++⚛$joined;
#?endif
#?if jvm
        ++$joined;
#?endif
        self
    }

    method join(Thread:D:) {
        self.finish
    }

    multi method Numeric(Thread:D:) {
        self.id
    }

    multi method Str(Thread:D:) {
        "Thread<$.id>($.name)"
    }
    multi method gist(Thread:D:) {
        "Thread #$.id" ~ ($!name ne '<anon>' ?? " ($!name)" !! '')
    }

    method yield(Thread:U: --> Nil) {
#?if !jvm
        ++⚛$yields;
#?endif
#?if jvm
        ++$yields;
#?endif
        nqp::threadyield();
    }

    method is-initial-thread(--> Bool) {
        nqp::hllbool(
          nqp::iseq_i(
            nqp::threadid(
              nqp::if(nqp::isconcrete(self),$!vm_thread,nqp::currentthread)
            ),
            nqp::threadid(Rakudo::Internals.INITTHREAD)
          )
        )
    }

    method usage(Thread:U:) is raw {
        nqp::list_i($started,$aborted,$completed,$joined,$yields,$highest_id)
    }
}

Rakudo::Internals.REGISTER-DYNAMIC: '$*THREAD', {
    my $init_thread := nqp::create(Thread);
    nqp::bindattr($init_thread, Thread, '$!vm_thread',
      Rakudo::Internals.INITTHREAD);
    nqp::bindattr($init_thread, Thread, '$!app_lifetime', False);
    nqp::bindattr($init_thread, Thread, '$!name', 'Initial thread');
    PROCESS::<$THREAD> := $init_thread;
}

# vim: expandtab shiftwidth=4
