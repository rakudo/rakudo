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

my class X::Proc::Async::SupplyOrStd does X::Proc::Async {
    method message() {
        "Using .Supply on a Proc::Async implies merging stdout and stderr; .stdout " ~
            "and .stderr cannot therefore be used in combination with it"
    }
}

my class X::Proc::Async::BindOrUse does X::Proc::Async {
    has $.handle;
    has $.use;
    method message() {
        "Cannot both bind $.handle to a handle and also $.use"
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

    has $!ready_promise = Promise.new;
    has $!ready_vow = $!ready_promise.vow;
    has $.path;
    has @.args;
    has $.w;
    has $.enc = 'utf8';
    has $.translate-nl = True;
    has Bool $.started = False;
    has $!stdout_supply;
    has CharsOrBytes $!stdout_type;
    has $!stderr_supply;
    has CharsOrBytes $!stderr_type;
    has $!merge_supply;
    has CharsOrBytes $!merge_type;
    has Int $!stdin-fd;
    has Int $!stdout-fd;
    has Int $!stderr-fd;
    has $!process_handle;
    has $!exit_promise;
    has @!promises;

    proto method new(|) { * }
    multi method new(*@ ($path, *@args), *%_) {
        self.bless(:$path, :@args, |%_)
    }

    method !supply(\what,\the-supply,\type,\value) {
        X::Proc::Async::TapBeforeSpawn.new(handle => what, proc => self).throw
          if $!started;
        X::Proc::Async::CharsOrBytes.new(handle => what, proc => self).throw
          if the-supply and type != value;

        type         = value;
        the-supply //= Supplier::Preserving.new;
    }

    proto method stdout(|) { * }
    multi method stdout(Proc::Async:D: :$bin!) {
        die X::Proc::Async::SupplyOrStd.new if $!merge_supply;
        die X::Proc::Async::BindOrUse.new(:handle<stdout>, :use('get the stdout Supply'))
            if $!stdout-fd;
        $bin
            ?? self!supply('stdout', $!stdout_supply, $!stdout_type, Bytes).Supply
            !! self.stdout(|%_)
    }
    multi method stdout(Proc::Async:D: :$enc, :$translate-nl) {
        die X::Proc::Async::SupplyOrStd.new if $!merge_supply;
        die X::Proc::Async::BindOrUse.new(:handle<stdout>, :use('get the stdout Supply'))
            if $!stdout-fd;
        self!wrap-decoder:
            self!supply('stdout', $!stdout_supply, $!stdout_type, Chars).Supply,
            $enc, :$translate-nl
    }

    proto method stderr(|) { * }
    multi method stderr(Proc::Async:D: :$bin!) {
        die X::Proc::Async::SupplyOrStd.new if $!merge_supply;
        die X::Proc::Async::BindOrUse.new(:handle<stderr>, :use('get the stderr Supply'))
            if $!stderr-fd;
        $bin
            ?? self!supply('stderr', $!stderr_supply, $!stderr_type, Bytes).Supply
            !! self.stderr(|%_)
    }
    multi method stderr(Proc::Async:D: :$enc, :$translate-nl) {
        die X::Proc::Async::SupplyOrStd.new if $!merge_supply;
        die X::Proc::Async::BindOrUse.new(:handle<stderr>, :use('get the stderr Supply'))
            if $!stderr-fd;
        self!wrap-decoder:
            self!supply('stderr', $!stderr_supply, $!stderr_type, Chars).Supply,
            $enc, :$translate-nl
    }

    proto method Supply(|) { * }
    multi method Supply(Proc::Async:D: :$bin!) {
        die X::Proc::Async::SupplyOrStd.new if $!stdout_supply || $!stderr_supply;
        die X::Proc::Async::BindOrUse.new(:handle<stdout>, :use('get the output Supply'))
            if $!stdout-fd;
        die X::Proc::Async::BindOrUse.new(:handle<stderr>, :use('get the output Supply'))
            if $!stderr-fd;
        $bin
            ?? self!supply('merge', $!merge_supply, $!merge_type, Bytes).Supply
            !! self.Supply(|%_)
    }
    multi method Supply(Proc::Async:D: :$enc, :$translate-nl) {
        die X::Proc::Async::SupplyOrStd.new if $!stdout_supply || $!stderr_supply;
        die X::Proc::Async::BindOrUse.new(:handle<stdout>, :use('get the output Supply'))
            if $!stdout-fd;
        die X::Proc::Async::BindOrUse.new(:handle<stderr>, :use('get the output Supply'))
            if $!stderr-fd;
        self!wrap-decoder:
            self!supply('merge', $!merge_supply, $!merge_type, Chars).Supply,
            $enc, :$translate-nl
    }

    proto method bind-stdin($) {*}
    multi method bind-stdin(IO::Handle:D $handle --> Nil) {
        die X::Proc::Async::BindOrUse.new(:handle<stdin>, :use('use :w')) if $!w;
        $!stdin-fd := $handle.native-descriptor;
    }
    multi method bind-stdin(IO::Pipe:D $handle --> Nil) {
        die X::Proc::Async::BindOrUse.new(:handle<stdin>, :use('use :w')) if $!w;
        my $sup := nqp::getattr(nqp::decont($handle), IO::Pipe, '$!bin-supply');
        die "Can only bind an output IO::Pipe to stdin of a process"
            unless $sup.DEFINITE;
        $!w = True;
        $!ready_promise.then({
            $sup().tap: { await self.write($_) },
                done => { self.close-stdin },
                quit => { self.close-stdin };
        });
    }

    method bind-stdout(IO::Handle:D $handle --> Nil) {
        die X::Proc::Async::BindOrUse.new(:handle<stdout>, :use('get the stdout Supply'))
            if $!stdout_supply;
        die X::Proc::Async::BindOrUse.new(:handle<stdout>, :use('get the output Supply'))
            if $!merge_supply;
        $!stdout-fd := $handle.native-descriptor;
    }

    method bind-stderr(IO::Handle:D $handle --> Nil) {
        die X::Proc::Async::BindOrUse.new(:handle<stderr>, :use('get the stderr Supply'))
            if $!stderr_supply;
        die X::Proc::Async::BindOrUse.new(:handle<stderr>, :use('get the output Supply'))
            if $!merge_supply;
        $!stderr-fd := $handle.native-descriptor;
    }

    method ready(--> Promise) {
        $!ready_promise;
    }

    method !wrap-decoder(Supply:D $bin-supply, $enc, :$translate-nl) {
        Rakudo::Internals.BYTE_SUPPLY_DECODER($bin-supply, $enc // $!enc,
            :translate-nl($translate-nl // $!translate-nl))
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

        nqp::bindkey($callbacks, 'ready', {
            $!ready_vow.keep(Nil);
        });

        nqp::bindkey($callbacks, 'error', -> Mu \err {
            my $error = X::OS.new(os-error => err);
            $!exit_promise.break($error);
            $!ready_vow.break($error);
        });

        @!promises.push(
          self!capture($callbacks,'stdout',$!stdout_supply)
        ) if $!stdout_supply;
        @!promises.push(
          self!capture($callbacks,'stderr',$!stderr_supply)
        ) if $!stderr_supply;
        @!promises.push(
          self!capture($callbacks,'merge',$!merge_supply)
        ) if $!merge_supply;

        nqp::bindkey($callbacks, 'buf_type', buf8.new);
        nqp::bindkey($callbacks, 'write', True) if $.w;
        nqp::bindkey($callbacks, 'stdin_fd', $!stdin-fd) if $!stdin-fd.DEFINITE;
        nqp::bindkey($callbacks, 'stdout_fd', $!stdout-fd) if $!stdout-fd.DEFINITE;
        nqp::bindkey($callbacks, 'stderr_fd', $!stderr-fd) if $!stderr-fd.DEFINITE;

        $!process_handle := nqp::spawnprocasync($scheduler.queue,
            CLONE-LIST-DECONTAINERIZED($!path,@!args),
            $cwd.Str,
            CLONE-HASH-DECONTAINERIZED(%ENV),
            $callbacks,
        );
        Promise.allof( $!exit_promise, @!promises ).then({
            $!exit_promise.status == Broken
                ?? $!exit_promise.cause.throw
                !! $!exit_promise.result
        })
    }

    method print(Proc::Async:D: Str() $str, :$scheduler = $*SCHEDULER) {
        X::Proc::Async::OpenForWriting.new(:method<print>, proc => self).throw if !$!w;
        X::Proc::Async::MustBeStarted.new(:method<print>, proc => self).throw  if !$!started;

        self.write($str.encode($!enc, :$!translate-nl))
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

    # Note: some of the duplicated code in methods could be moved to
    # proto, but at the moment (2017-06-02) that makes the call 24% slower
    proto method kill(|) { * }
    multi method kill(Proc::Async:D: Signal:D \signal = SIGHUP) {
        X::Proc::Async::MustBeStarted.new(:method<kill>, proc => self).throw if !$!started;
        nqp::killprocasync($!process_handle, signal.value)
    }
    multi method kill(Proc::Async:D: Int:D \signal) {
        X::Proc::Async::MustBeStarted.new(:method<kill>, proc => self).throw if !$!started;
        nqp::killprocasync($!process_handle, signal)
    }

    # $*KERNEL.signal with Str:D signal isn't thread-safe, as it initializes
    # a hash attribute, so we stick this operation into a lock
    my $kill-lock = Lock.new;
    multi method kill(Proc::Async:D: Str:D \signal) {
        X::Proc::Async::MustBeStarted.new(:method<kill>, proc => self).throw if !$!started;
        nqp::killprocasync($!process_handle,
          $kill-lock.protect: { $*KERNEL.signal: signal })
    }
}
