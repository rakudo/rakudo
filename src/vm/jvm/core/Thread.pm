# This file contains early work on concurrency support for Rakudo on the JVM.
# The implementation is clearly VM specific, however the aim is to iterate
# towards a backend-independent API.

# Thread represents an OS-level thread. While it could be used directly, it
# is not the preferred way to work in Perl 6. It's a building block for the
# interesting things.
my class Thread {
    # This thread's underlying JVM thread object.
    has Mu $!jvm_thread;

    # Is the thread's lifetime bounded by that of the application, such
    # that when it exits, so does the thread?
    has Bool $.app_lifetime;

    # Thread's (user-defined) name.
    has Str $.name;

    submethod BUILD(:&code!, :$!app_lifetime as Bool = False, :$!name as Str = "<anon>") {
        my $interop   := nqp::jvmbootinterop();
        my \JVMThread := $interop.typeForName('java.lang.Thread');
        $!jvm_thread  := JVMThread."constructor/new/(Ljava/lang/Runnable;)V"(
            $interop.proxy('java.lang.Runnable', nqp::hash('run',
                {
                    my $*THREAD = self;
                    code();
                })));
        $!jvm_thread.setDaemon(1) if $!app_lifetime;
    }

    method start(&code, *%adverbs) {
        Thread.new(:&code, |%adverbs).jvm_start()
    }

    method jvm_start(Thread:D:) {
        $!jvm_thread.start();
        self
    }

    method id(Thread:D:) {
        $!jvm_thread.getId();
    }

    method finish(Thread:D:) {
        $!jvm_thread.'method/join/()V'();
        self
    }

    multi method Str(Thread:D:) {
        "Thread<$.id>($.name)"
    }

    method yield(Thread:U:) {
        nqp::jvmbootinterop().typeForName('java.lang.Thread').yield();
    }
}

{
    # This code is a little funky to avoid hitting jvmbootinterop at startup
    # even if we never use anything that needs it. This is because it carries
    # some cost and has a very bad interaction with the evalserver.
    my int $not_yet = 1;
    my $init_thread;
    PROCESS::<$THREAD> := Proxy.new(
        FETCH => -> | {
            unless nqp::isconcrete($init_thread) || $not_yet {
                my $interop   := nqp::jvmbootinterop();
                my \JVMThread := $interop.typeForName('java.lang.Thread');
                $init_thread  := nqp::create(Thread);
                nqp::bindattr($init_thread, Thread, '$!jvm_thread', JVMThread.currentThread());
                nqp::bindattr($init_thread, Thread, '$!app_lifetime', False);
                nqp::bindattr($init_thread, Thread, '$!name', 'Initial thread');
            }
            $init_thread
        },
        STORE => -> | {
            X::Assignment::RO.new.throw
        });
    $not_yet = 0;
}
