my class X::Proc::Async::TapBeforeSpawn is Exception {
    has $.handle;
    method message() {
        "To avoid data races, you must tap $!handle before running the process"
    }
}

my class X::Proc::Async::CharsOrBytes is Exception {
    has $.handle;
    method message() {
        "Can only tap one of chars or bytes supply for process $!handle"
    }
}

my class X::Proc::Async::AlreadyStarted is Exception {
    method message() {
        "Process has already been started"
    }
}

my class Proc::Async {
    has $.path;
    has @.args;
    has $.w;
    has Bool $!started;
    has $!stdout_supply;
    has Bool $!stdout_supply_chars;
    has $!stderr_supply;
    has Bool $!stderr_supply_chars;
    has $!process_handle;
    has $!exited_promise;

    my class ProcessCancellation is repr('AsyncTask') { }

    method stdout_chars() {
        X::Proc::Async::TapBeforeSpawn.new(handle => 'stdout').throw
            if $!started;
        X::Proc::Async::CharsOrBytes.new(handle => 'stdout').throw
            if defined $!stderr_supply_chars && !$!stderr_supply_chars;
        $!stdout_supply_chars = True;
        $!stdout_supply //= Supply.new
    }

    method stdout_bytes() {
        X::Proc::Async::TapBeforeSpawn.new(handle => 'stdout').throw
            if $!started;
        X::Proc::Async::CharsOrBytes.new(handle => 'stdout').throw
            if defined $!stderr_supply_chars && $!stderr_supply_chars;
        $!stdout_supply_chars = False;
        $!stdout_supply //= Supply.new
    }

    method stderr_chars() {
        X::Proc::Async::TapBeforeSpawn.new(handle => 'stderr').throw
            if $!started;
        X::Proc::Async::CharsOrBytes.new(handle => 'stderr').throw
            if defined $!stdout_supply_chars && !$!stdout_supply_chars;
        $!stderr_supply_chars = True;
        $!stderr_supply //= Supply.new
    }

    method stderr_bytes() {
        X::Proc::Async::TapBeforeSpawn.new(handle => 'stderr').throw
            if $!started;
        X::Proc::Async::CharsOrBytes.new(handle => 'stderr').throw
            if defined $!stdout_supply_chars && $!stdout_supply_chars;
        $!stderr_supply_chars = False;
        $!stderr_supply //= Supply.new
    }

    method start(:$scheduler = $*SCHEDULER) {
        X::Proc::Async::AlreadyStarted.new.throw
            if $!started;
        $!started = True;

        my Mu $hash-with-containers := nqp::getattr(%*ENV, EnumMap, '$!storage');
        my Mu $hash-without         := nqp::hash();
        my Mu $enviter := nqp::iterator($hash-with-containers);
        my $envelem;
        while $enviter {
            $envelem := nqp::shift($enviter);
            nqp::bindkey($hash-without, nqp::iterkey_s($envelem), nqp::decont(nqp::iterval($envelem)))
        }

        my Mu $args-without := nqp::list(nqp::decont($!path.Str));
        for @!args.eager {
            nqp::push($args-without, nqp::decont(.Str));
        }

        $!exited_promise = Promise.new;
        my $exited_vow   = $!exited_promise.vow;

        my Mu $callbacks := nqp::hash();
        nqp::bindkey($callbacks, 'done', -> Mu \status {
            $!stdout_supply.?done();
            $!stderr_supply.?done();
            $exited_vow.keep(Proc::Status.new(:exit(status)))
        });
        nqp::bindkey($callbacks, 'error', -> Mu \err {
            $exited_vow.break(err);
        });
        if defined $!stdout_supply_chars {
            nqp::bindkey($callbacks,
                $!stdout_supply_chars ?? 'stdout_chars' !! 'stdout_bytes',
                -> Mu \seq, Mu \data, Mu \err {
                    if err {
                        $!stdout_supply.quit(err);
                    }
                    elsif seq >= 0 {
                        $!stdout_supply.more(data);
                    }
                });
        }
        if defined $!stderr_supply_chars {
            nqp::bindkey($callbacks,
                $!stderr_supply_chars ?? 'stderr_chars' !! 'stderr_bytes',
                -> Mu \seq, Mu \data, Mu \err {
                    if err {
                        $!stderr_supply.quit(err);
                    }
                    elsif seq < 0 {
                        $!stderr_supply.done();
                    }
                    else {
                        $!stderr_supply.more(data);
                    }
                });
        }
        nqp::bindkey($callbacks, 'buf_type', buf8.new);
        nqp::bindkey($callbacks, 'write', True) if $.w;

        $!process_handle := nqp::spawnprocasync($scheduler.queue,
            $args-without, $*CWD.Str, $hash-without, $callbacks);

        $!exited_promise
    }

    method print($str as Str, :$scheduler = $*SCHEDULER) {
        my $p = Promise.new;
        my $v = $p.vow;
        nqp::asyncwritestr(
            $!process_handle,
            $scheduler.queue,
            -> Mu \bytes, Mu \err {
                if err {
                    $v.break(err);
                }
                else {
                    $v.keep(bytes);
                }
            },
            nqp::unbox_s($str), ProcessCancellation);
        $p
    }

    method say($str as Str, :$scheduler = $*SCHEDULER) {
        my $p = Promise.new;
        my $v = $p.vow;
        nqp::asyncwritestr(
            $!process_handle,
            $scheduler.queue,
            -> Mu \bytes, Mu \err {
                if err {
                    $v.break(err);
                }
                else {
                    $v.keep(bytes);
                }
            },
            nqp::unbox_s($str ~ "\n"), ProcessCancellation);
        $p
    }

    method write(Buf $b, :$scheduler = $*SCHEDULER) {
        my $p = Promise.new;
        my $v = $p.vow;
        nqp::asyncwritebytes(
            $!process_handle,
            $scheduler.queue,
            -> Mu \bytes, Mu \err {
                if err {
                    $v.break(err);
                }
                else {
                    $v.keep(bytes);
                }
            },
            nqp::decont($b), ProcessCancellation);
        $p
    }

    method close_stdin() {
        nqp::closefh($!process_handle);
        True;
    }

    method kill($signal = 1) {
        nqp::killprocasync($!process_handle, $signal.Int)
    }
}
