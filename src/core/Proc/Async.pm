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
    my class ProcessCancellation is repr('AsyncTask') { }
    my enum  CharsOrBytes ( :Bytes(0), :Chars(1) );

    has $.path;
    has @.args;
    has $.w;
    has Bool $!started;
    has $!stdout_supply;
    has CharsOrBytes $!stdout_type;
    has $!stderr_supply;
    has CharsOrBytes $!stderr_type;
    has $!process_handle;
    has $!exited_promise;

    method !supply(\what,\supply,\type,\value) is hidden_from_backtrace {
        X::Proc::Async::TapBeforeSpawn.new(handle => what).throw
          if $!started;
        X::Proc::Async::CharsOrBytes.new(handle => what).throw
          if supply and type != value;

        type     = value;
        supply //= Supply.new;
    }

    proto method stdout(|) { * }
    multi method stdout(Proc::Async:D:) {
        self!supply('stdout', $!stdout_supply, $!stdout_type, Chars);
    }
    multi method stdout(Proc::Async:D: :$bin!) {
        self!supply('stdout',$!stdout_supply,$!stdout_type,$bin ?? Bytes !! Chars);
    }

    proto method stderr(|) { * }
    multi method stderr(Proc::Async:D:) {
        self!supply('stderr', $!stderr_supply, $!stderr_type, Chars);
    }
    multi method stderr(Proc::Async:D: :$bin!) {
        self!supply('stderr',$!stderr_supply,$!stderr_type,$bin ?? Bytes !! Chars);
    }

    method stdout_chars(Proc::Async:D:) {
        self!supply('stdout', $!stdout_supply, $!stdout_type, Chars);
    }

    method stdout_bytes(Proc::Async:D:) {
        self!supply('stdout', $!stdout_supply, $!stdout_type, Bytes);
    }

    method stderr_chars(Proc::Async:D:) {
        self!supply('stderr', $!stderr_supply, $!stderr_type, Chars);
    }

    method stderr_bytes(Proc::Async:D:) {
        self!supply('stderr', $!stderr_supply, $!stderr_type, Bytes);
    }

    method start(Proc::Async:D: :$scheduler = $*SCHEDULER, :$ENV) {
        X::Proc::Async::AlreadyStarted.new.throw
            if $!started;
        $!started = True;

        my %ENV := $ENV ?? $ENV.hash !! %*ENV;
        my Mu $hash-with-containers := nqp::getattr(%ENV, EnumMap, '$!storage');
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
        if $!stdout_supply {
            nqp::bindkey($callbacks,
                $!stdout_type ?? 'stdout_chars' !! 'stdout_bytes',
                -> Mu \seq, Mu \data, Mu \err {
                    if err {
                        $!stdout_supply.quit(err);
                    }
                    elsif seq >= 0 {
                        $!stdout_supply.more(data);
                    }
                });
        }
        if $!stderr_supply {
            nqp::bindkey($callbacks,
                $!stderr_type ?? 'stderr_chars' !! 'stderr_bytes',
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

    method print(Proc::Async:D: $str as Str, :$scheduler = $*SCHEDULER) {
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

    method say(Proc::Async:D: $str as Str, |c) { self.print( $str ~ "\n", |c ) }

    method write(Proc::Async:D: Buf $b, :$scheduler = $*SCHEDULER) {
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

    method close_stdin(Proc::Async:D:) {
        nqp::closefh($!process_handle);
        True;
    }

    method kill(Proc::Async:D: $signal = 1) {
        nqp::killprocasync($!process_handle, $signal.Int)
    }
}
