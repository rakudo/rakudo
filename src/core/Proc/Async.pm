my class Proc::Async { ... }

my role X::Proc::Async is Exception {
    has Proc::Async $.proc;

}
my class X::Proc::Async::TapBeforeSpawn does X::Proc::Async {
    has $.handle;
    method message() {
        "To avoid data races, you must tap $!handle before running the process"
    }
}

my class X::Proc::Async::CharsOrBytes does X::Proc::Async {
    has $.handle;
    method message() {
        "Can only tap one of chars or bytes supply for $!handle"
    }
}

my class X::Proc::Async::AlreadyStarted does X::Proc::Async {
    method message() {
        "Process has already been started"
    }
}

my class X::Proc::Async::MustBeStarted does X::Proc::Async {
    has $.method;
    method message() {
        "Process must be started first before calling '$!method'"
    }
}

my class X::Proc::Async::OpenForWriting does X::Proc::Async {
    has $.method;
    method message() {
        "Process must be opened for writing with :w to call '$!method'"
    }
}

my class Proc::Async {
    my class ProcessCancellation is repr('AsyncTask') { }
    my enum  CharsOrBytes ( :Bytes(0), :Chars(1) );

    has $.path;
    has @.args;
    has $.w;
    has Bool $.started = False;
    has $!stdout_supply;
    has CharsOrBytes $!stdout_type;
    has $!stderr_supply;
    has CharsOrBytes $!stderr_type;
    has $!process_handle;
    has $!exit_promise;
    has @!promises;

    proto method new(|) { * }
    multi method new($path, *@args, :$w) { self.bless(:$path,:@args,:$w) }

    method !supply(\what,\the-supply,\type,\value) {
        X::Proc::Async::TapBeforeSpawn.new(handle => what, proc => self).throw
          if $!started;
        X::Proc::Async::CharsOrBytes.new(handle => what, proc => self).throw
          if the-supply and type != value;

        type         = value;
        the-supply //= Supplier.new;
    }

    proto method stdout(|) { * }
    multi method stdout(Proc::Async:D:) {
        self!wrap-decoder:
            self!supply('stdout', $!stdout_supply, $!stdout_type, Chars).Supply;
    }
    multi method stdout(Proc::Async:D: :$bin!) {
        $bin
            ?? self!supply('stdout', $!stdout_supply, $!stdout_type, Bytes).Supply
            !! self.stdout()
    }

    proto method stderr(|) { * }
    multi method stderr(Proc::Async:D:) {
        self!wrap-decoder:
            self!supply('stderr', $!stderr_supply, $!stderr_type, Chars).Supply;
    }
    multi method stderr(Proc::Async:D: :$bin!) {
        $bin
            ?? self!supply('stderr', $!stderr_supply, $!stderr_type, Bytes).Supply
            !! self.stderr()
    }

    method !wrap-decoder(Supply:D $bin-supply) {
        supply {
            my $decoder = Rakudo::Internals::VMBackedDecoder.new('utf8');
            whenever $bin-supply {
                $decoder.add-bytes($_);
                my $available = $decoder.consume-available-chars();
                emit $available if $available ne '';
                LAST {
                    with $decoder {
                        emit .consume-all-chars();
                    }
                }
            }
        }
    }

    method !capture(\callbacks,\std,\the-supply) {
        my $promise = Promise.new;
        my $vow = $promise.vow;
        my $ss = Rakudo::Internals::SupplySequencer.new(
            on-data-ready => -> \data { the-supply.emit(data) },
            on-completed  => -> { the-supply.done(); $vow.keep(the-supply) },
            on-error      => -> \err { the-supply.quit(err); $vow.keep((the-supply,err)) });
        nqp::bindkey(callbacks,
            std ~ '_bytes' ,
            -> Mu \seq, Mu \data, Mu \err { $ss.process(seq, data, err) });
        $promise;
    }

    method start(Proc::Async:D: :$scheduler = $*SCHEDULER, :$ENV, :$cwd = $*CWD) {
        X::Proc::Async::AlreadyStarted.new(proc => self).throw if $!started;
        $!started = True;

        my %ENV := $ENV ?? $ENV.hash !! %*ENV;

        $!exit_promise = Promise.new;

        my Mu $callbacks := nqp::hash();
        nqp::bindkey($callbacks, 'done', -> Mu \status {
           $!exit_promise.keep(Proc.new(
               :exitcode(status +> 8), :signal(status +& 0xFF),
               :command[ $!path, |@!args ],
           ))
        });
        nqp::bindkey($callbacks, 'error', -> Mu \err {
            $!exit_promise.break(X::OS.new(os-error => err));
        });

        @!promises.push(
          self!capture($callbacks,'stdout',$!stdout_supply)
        ) if $!stdout_supply;
        @!promises.push(
          self!capture($callbacks,'stderr',$!stderr_supply)
        ) if $!stderr_supply;

        nqp::bindkey($callbacks, 'buf_type', buf8.new);
        nqp::bindkey($callbacks, 'write', True) if $.w;

        $!process_handle := nqp::spawnprocasync($scheduler.queue,
            CLONE-LIST-DECONTAINERIZED($!path,@!args),
            $cwd.Str,
            CLONE-HASH-DECONTAINERIZED(%ENV),
            $callbacks,
        );

        Promise.allof( $!exit_promise, @!promises ).then( {
            for @!promises -> $promise {
                given $promise.result {
                    when Supply { .done }
                    when List   { $_[0].quit( $_[1] ) }
                }
            }
            $!exit_promise.result;
        } );
    }

    method print(Proc::Async:D: Str() $str, :$scheduler = $*SCHEDULER) {
        X::Proc::Async::OpenForWriting.new(:method<print>, proc => self).throw if !$!w;
        X::Proc::Async::MustBeStarted.new(:method<print>, proc => self).throw  if !$!started;

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

    method put(Proc::Async:D: \x, |c) {
        X::Proc::Async::OpenForWriting.new(:method<say>, proc => self).throw if !$!w;
        X::Proc::Async::MustBeStarted.new(:method<say>, proc => self).throw  if !$!started;

        self.print( x.join ~ "\n", |c );
    }

    method say(Proc::Async:D: \x, |c) {
        X::Proc::Async::OpenForWriting.new(:method<say>, proc => self).throw if !$!w;
        X::Proc::Async::MustBeStarted.new(:method<say>, proc => self).throw  if !$!started;

        self.print( x.gist ~ "\n", |c );
    }

    method write(Proc::Async:D: Blob:D $b, :$scheduler = $*SCHEDULER) {
        X::Proc::Async::OpenForWriting.new(:method<write>, proc => self).throw if !$!w;
        X::Proc::Async::MustBeStarted.new(:method<write>, proc => self).throw  if !$!started;

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
    method close-stdin(Proc::Async:D:) {
        X::Proc::Async::OpenForWriting.new(:method<close-stdin>, proc => self).throw
          if !$!w;
        X::Proc::Async::MustBeStarted.new(:method<close-stdin>, proc => self).throw
          if !$!started;

        nqp::closefh($!process_handle);
        True;
    }

    method kill(Proc::Async:D: $signal = "HUP") {
        X::Proc::Async::MustBeStarted.new(:method<kill>, proc => self).throw if !$!started;
        nqp::killprocasync($!process_handle, $*KERNEL.signal($signal));
    }
}
