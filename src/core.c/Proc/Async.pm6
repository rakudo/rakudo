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
    # An asynchronous process output pipe is a Supply that also can provide
    # the native descriptor of the underlying pipe.
    class Pipe is Supply {
        my class PermitOnTap does Tappable {
            has Tappable $.delegate is built(:bind);
            has &.on-tap is built(:bind);
            method tap(|c) {
                &!on-tap();
                $!delegate.tap(|c)
            }
            method live() { self.delegate.live }
            method serial() { self.delegate.serial }
            method sane() { self.delegate.sane }
        }

        has Promise $.native-descriptor is built(:bind);
        has &!on-nd-used is built(:bind);

        method native-descriptor() {
            &!on-nd-used();
            $!native-descriptor
        }

        method new($delegate, $native-descriptor, &on-tap, &on-nd-used) {
            self.bless(
                tappable => PermitOnTap.bless(:$delegate, :&on-tap),
                :$native-descriptor, :&on-nd-used
            )
        }
    }

    my class ProcessCancellation is repr('AsyncTask') { }
    my enum  CharsOrBytes ( :Bytes(0), :Chars(1) );

    has $!ready_promise = Promise.new;
    has $!ready_vow = $!ready_promise.vow;
    has $!handle_available_promise = Promise.new;
    has $!stdout_descriptor_vow;
    has $!stderr_descriptor_vow;
    has $!stdout_descriptor_used = Promise.new;
    has $!stderr_descriptor_used = Promise.new;
    has $.path; # XXX TODO deprecated on 2018-11-04
    has @.args; # XXX TODO deprecated on 2018-11-04
    has @.command is List;
    has $.w;
    has $.enc = 'utf8';
    has $.translate-nl = True;
    has $.arg0;
    has $.win-verbatim-args = False;
    has Bool $.started = False;
    has $!stdout_supply;
    has CharsOrBytes $!stdout_type;
    has $!stderr_supply;
    has CharsOrBytes $!stderr_type;
    has $!merge_supply;
    has CharsOrBytes $!merge_type;
    has $!stdin-fd;
    has $!stdin-fd-close;
    has $!stdout-fd;
    has $!stderr-fd;
    has $!process_handle;
    has $!exit_promise;
    has @!promises;
    has $!encoder;
    has @!close-after-exit;

    proto method new(|) {*}
    multi method new(*@args where .so) {
        # XXX TODO .args and .path deprecated on 2018-11-04 to be
        # replaced by .command https://github.com/rakudo/rakudo/issues/2444
        my @command := @args.List;
        my $path = @args.shift;
        self.bless(:$path, :@args, :@command, |%_)
    }

    submethod TWEAK(--> Nil) {
        $!encoder := Encoding::Registry.find($!enc).encoder(:$!translate-nl);

        $!arg0 //= $!path;
        @!args.unshift: $!arg0;
    }

    method !pipe-cbs(\channel) {
        -> { $!handle_available_promise.then({ nqp::permit($!process_handle, channel, -1) }) },
        -> { (channel == 1 ?? $!stdout_descriptor_used !! $!stderr_descriptor_used).keep(True) }
    }

    method !pipe(\what, \the-supply, \type, \value, \fd-vow, \permit-channel) {
        X::Proc::Async::TapBeforeSpawn.new(handle => what, :proc(self)).throw
          if $!started;
        X::Proc::Async::CharsOrBytes.new(handle => what, :proc(self)).throw
          if the-supply and type != value;

        type         = value;
        the-supply //= Supplier::Preserving.new;

        if nqp::iscont(fd-vow) {
            my $native-descriptor := Promise.new;
            fd-vow = $native-descriptor.vow;
            Pipe.new(the-supply.Supply.Tappable, $native-descriptor, |self!pipe-cbs(permit-channel))
        }
        else {
            the-supply.Supply
        }
    }

    method !wrap-decoder(Supply:D $bin-supply, $enc, \fd-vow, \permit-channel, :$translate-nl) {
        my \sup = Rakudo::Internals.BYTE_SUPPLY_DECODER($bin-supply, $enc // $!enc,
            :translate-nl($translate-nl // $!translate-nl));
        if nqp::iscont(fd-vow) {
            my $native-descriptor := Promise.new;
            fd-vow = $native-descriptor.vow;
            Pipe.new(sup.Supply.Tappable, $native-descriptor, |self!pipe-cbs(permit-channel))
        }
        else {
            sup
        }
    }

    proto method stdout(|) {*}
    multi method stdout(Proc::Async:D: :$bin!) {
        $!merge_supply
          ?? X::Proc::Async::SupplyOrStd.new.throw
          !! $!stdout-fd
            ?? X::Proc::Async::BindOrUse.new(:handle<stdout>, :use('get the stdout Supply')).throw
            !! $bin
              ?? self!pipe('stdout', $!stdout_supply, $!stdout_type, Bytes, $!stdout_descriptor_vow, 1)
              !! self.stdout(|%_)
    }
    multi method stdout(Proc::Async:D: :$enc, :$translate-nl) {
        $!merge_supply
          ?? X::Proc::Async::SupplyOrStd.new.throw
          !! $!stdout-fd
            ?? X::Proc::Async::BindOrUse.new(:handle<stdout>, :use('get the stdout Supply')).throw
            !! self!wrap-decoder:
              self!pipe('stdout',$!stdout_supply,$!stdout_type,Chars,Nil,1),
              $enc, $!stdout_descriptor_vow, 1, :$translate-nl
    }

    proto method stderr(|) {*}
    multi method stderr(Proc::Async:D: :$bin!) {
        $!merge_supply
          ?? X::Proc::Async::SupplyOrStd.new.throw
          !! $!stderr-fd
            ?? X::Proc::Async::BindOrUse.new(:handle<stderr>, :use('get the stderr Supply')).throw
            !! $bin
              ?? self!pipe('stderr', $!stderr_supply, $!stderr_type, Bytes, $!stderr_descriptor_vow, 2)
              !! self.stderr(|%_)
    }
    multi method stderr(Proc::Async:D: :$enc, :$translate-nl) {
        $!merge_supply
          ?? X::Proc::Async::SupplyOrStd.new.throw
          !! $!stderr-fd
            ?? X::Proc::Async::BindOrUse.new(:handle<stderr>, :use('get the stderr Supply')).throw
            !! self!wrap-decoder:
              self!pipe('stderr',$!stderr_supply,$!stderr_type,Chars,Nil,2),
              $enc, $!stderr_descriptor_vow, 2, :$translate-nl
    }

    proto method Supply(|) {*}
    multi method Supply(Proc::Async:D: :$bin!) {
        $!stdout_supply || $!stderr_supply
          ?? X::Proc::Async::SupplyOrStd.new.throw
          !! $!stdout-fd
            ?? X::Proc::Async::BindOrUse.new(:handle<stdout>, :use('get the output Supply')).throw
            !! $!stderr-fd
              ?? X::Proc::Async::BindOrUse.new(:handle<stderr>, :use('get the output Supply')).throw
              !! $bin
                ?? self!pipe('merge',$!merge_supply,$!merge_type,Bytes,Nil,0)
                !! self.Supply(|%_)
    }
    multi method Supply(Proc::Async:D: :$enc, :$translate-nl) {
        $!stdout_supply || $!stderr_supply
          ?? X::Proc::Async::SupplyOrStd.new.throw
          !! $!stdout-fd
            ?? X::Proc::Async::BindOrUse.new(:handle<stdout>, :use('get the output Supply')).throw
            !! $!stderr-fd
              ?? X::Proc::Async::BindOrUse.new(:handle<stderr>, :use('get the output Supply')).throw
              !! self!wrap-decoder:
                self!pipe('merge',$!merge_supply,$!merge_type,Chars,Nil,0),
                $enc, Nil, 0, :$translate-nl
    }

    proto method bind-stdin($) {*}
    multi method bind-stdin(IO::Handle:D $handle --> Nil) {
        X::Proc::Async::BindOrUse.new(:handle<stdin>, :use('use :w')).throw
          if $!w;
        $!stdin-fd := $handle.native-descriptor;
        @!close-after-exit.push($handle)
          if nqp::istype($handle,IO::Pipe);
    }
    multi method bind-stdin(Proc::Async::Pipe:D $pipe --> Nil) {
        if $!w {
            X::Proc::Async::BindOrUse.new(:handle<stdin>, :use('use :w')).throw
        }
        $!stdin-fd := $pipe.native-descriptor;
        $!stdin-fd-close := True;
    }

    method bind-stdout(IO::Handle:D $handle --> Nil) {
        $!stdout_supply
          ?? X::Proc::Async::BindOrUse.new(:handle<stdout>, :use('get the stdout Supply')).throw
          !! $!merge_supply
            ?? X::Proc::Async::BindOrUse.new(:handle<stdout>, :use('get the output Supply')).throw
            !! ($!stdout-fd := $handle.native-descriptor);
    }

    method bind-stderr(IO::Handle:D $handle --> Nil) {
        $!stderr_supply
          ?? X::Proc::Async::BindOrUse.new(:handle<stderr>, :use('get the stderr Supply')).throw
          !! $!merge_supply
            ?? X::Proc::Async::BindOrUse.new(:handle<stderr>, :use('get the output Supply')).throw
            !! ($!stderr-fd := $handle.native-descriptor);
    }

    method ready(--> Promise) {
        $!ready_promise
    }

    method pid(--> Promise) {
        $!ready_promise
    }

    method !capture(\callbacks,\std,\the-supply --> Promise) {
        my $promise := Promise.new;
        my $vow := $promise.vow;
        my $ss := Rakudo::Internals::SupplySequencer.new(
            on-data-ready => -> \data { the-supply.emit(data) },
            on-completed  => -> { the-supply.done(); $vow.keep(the-supply) },
            on-error      => -> \err { the-supply.quit(err); $vow.keep((the-supply,err)) });
        nqp::bindkey(callbacks,
            std ~ '_bytes' ,
            -> Mu \seq, Mu \data, Mu \err { $ss.process(seq, data, err) });
        $promise
    }

    method !win-quote-CommandLineToArgvW(*@args) {
        my @quoted_args;
        for @args -> $arg {
            if !$arg.contains(' ') && !$arg.contains('"') && !$arg.contains('\t') && !$arg.contains('\n') && !$arg.contains('\v') {
                @quoted_args.push: $arg;
            }
            else {
                my $quoted_arg = $arg;
                $quoted_arg ~~ s:g/ ( \\* ) \" /$0$0\\\"/;
                $quoted_arg ~~ s/ ( \\+ ) $ /$0$0/;
                @quoted_args.push: '"' ~ $quoted_arg ~ '"';
            }
        }
        @quoted_args.join: ' '
    }

    method start(Proc::Async:D:
      :$scheduler = $*SCHEDULER, :$ENV, :$cwd = $*CWD
    --> Promise) {
        X::Proc::Async::AlreadyStarted.new(proc => self).throw
          if $!started;

        $!started := True;
        nqp::istype($!stdin-fd,Promise)
          ?? start {
                 await $!stdin-fd.then({ $!stdin-fd := .result });
                 await self!start-internal($scheduler, $ENV, $cwd);
             }
          !! self!start-internal($scheduler, $ENV, $cwd)
    }

    method !start-internal($scheduler, $ENV, $cwd --> Promise) {
        my %ENV := $ENV ?? $ENV.hash !! %*ENV;

#?if jvm
        # The Java process API does not allow disabling Javas
        # sophisticated heuristics of command mangling.
        # NQPs spawnprocasync implementation on JVM thus overwrites
        # arg[0] with the program name and forwards the result to Javas
        # APIs.
        # So we do not quote the arguments and just let Java do its magic.
        my @quoted-args := @!args;
#?endif
#?if !jvm
        my @quoted-args;
        if Rakudo::Internals.IS-WIN {
            @quoted-args.push(
                $!win-verbatim-args
                    ?? @!args.join(' ')
                    !! self!win-quote-CommandLineToArgvW(@!args));
        }
        else {
            @quoted-args := @!args;
        }
#?endif

        $!exit_promise := Promise.new;

        my Mu $callbacks := nqp::hash();
        nqp::bindkey($callbacks, 'done', -> Mu \status {
           $!exit_promise.keep(Proc.new(
               :exitcode(status +> 8), :signal(status +& 0xFF),
               :command( @!command ),
           ))
        });

        nqp::bindkey($callbacks, 'ready', -> Mu \handles = Nil, $pid = 0 {
            if nqp::isconcrete(handles) {
                nqp::atpos_i(handles,0) < 0
                  ?? .break("STDOUT descriptor not available")
                  !! .keep(nqp::atpos_i(handles,0))
                  with $!stdout_descriptor_vow;

                nqp::atpos_i(handles,1) < 0
                  ?? .break("STDERR descriptor not available")
                  !! .keep(nqp::atpos_i(handles,1))
                  with $!stderr_descriptor_vow;
            }
            $!ready_vow.keep($pid);
        });

        nqp::bindkey($callbacks, 'error', -> Mu \err {
            my $error := X::OS.new(os-error => err);
            $!exit_promise.break($error);
            $!ready_vow.break($error);
        });

        @!promises.push(Promise.anyof(
          self!capture($callbacks,'stdout',$!stdout_supply),
          $!stdout_descriptor_used
        )) if $!stdout_supply;

        @!promises.push(Promise.anyof(
          self!capture($callbacks,'stderr',$!stderr_supply),
          $!stderr_descriptor_used
        )) if $!stderr_supply;

        @!promises.push(
          self!capture($callbacks,'merge',$!merge_supply)
        ) if $!merge_supply;

        nqp::bindkey($callbacks, 'buf_type', nqp::create(buf8.^pun));
        nqp::bindkey($callbacks, 'write', True) if $.w;
        nqp::bindkey($callbacks, 'stdin_fd', $!stdin-fd) if $!stdin-fd.DEFINITE;
        nqp::bindkey($callbacks, 'stdin_fd_close', True) if $!stdin-fd-close;
        nqp::bindkey($callbacks, 'stdout_fd', $!stdout-fd) if $!stdout-fd.DEFINITE;
        nqp::bindkey($callbacks, 'stderr_fd', $!stderr-fd) if $!stderr-fd.DEFINITE;

        $!process_handle := nqp::spawnprocasync($scheduler.queue(:hint-affinity),
            $!path.Str,
            CLONE-LIST-DECONTAINERIZED(@quoted-args),
            $cwd.Str,
            CLONE-HASH-DECONTAINERIZED(%ENV),
            $callbacks,
        );
        $!handle_available_promise.keep(True);
        nqp::permit($!process_handle, 0, -1) if $!merge_supply;

        Promise.allof( $!exit_promise, @!promises ).then({
            .close for @!close-after-exit;
            $!exit_promise.status == Broken
                ?? $!exit_promise.cause.throw
                !! $!exit_promise.result
        })
    }

    method print(Proc::Async:D: Str() $str, :$scheduler = $*SCHEDULER) {
        $!w
          ?? $!started
            ?? self.write($!encoder.encode-chars($str))
            !! X::Proc::Async::MustBeStarted.new(:method<print>, :proc(self)).throw
          !! X::Proc::Async::OpenForWriting.new(:method<print>, :proc(self)).throw
    }

    method put(Proc::Async:D: \x, |c) {
        $!w
          ?? $!started
            ?? self.print( x.join ~ "\n", |c )
            !! X::Proc::Async::MustBeStarted.new(:method<say>,:proc(self)).throw
          !! X::Proc::Async::OpenForWriting.new( :method<say>,:proc(self)).throw
    }

    method say(Proc::Async:D: \x, |c) {
        $!w
          ?? $!started
            ?? self.print( x.gist ~ "\n", |c )
            !! X::Proc::Async::MustBeStarted.new(:method<say>,:proc(self)).throw
          !! X::Proc::Async::OpenForWriting.new( :method<say>,:proc(self)).throw
    }

    method write(Proc::Async:D: Blob:D $b, :$scheduler = $*SCHEDULER) {
        if $!w && $!started {
            my $p := Promise.new;
            my $v := $p.vow;
            nqp::asyncwritebytes(
                $!process_handle,
                $scheduler.queue,
                -> Mu \bytes, Mu \err {
                    err
                      ?? $v.break(err)
                      !! $v.keep(bytes)
                },
                nqp::decont($b), ProcessCancellation
            );
            $p
        }
        else {
            $!w
              ?? X::Proc::Async::MustBeStarted.new(:method<write>, :proc(self)).throw
              !! X::Proc::Async::OpenForWriting.new(:method<write>, :proc(self)).throw;
        }
    }

    method close-stdin(Proc::Async:D: --> True) {
        $!w
          ?? $!started
            ?? nqp::closefh($!process_handle)
            !! X::Proc::Async::MustBeStarted.new(:method<close-stdin>, :proc(self)).throw
          !! X::Proc::Async::OpenForWriting.new(:method<close-stdin>, :proc(self)).throw
    }

    # Note: some of the duplicated code in methods could be moved to
    # proto, but at the moment (2017-06-02) that makes the call 24% slower
    proto method kill(|) {*}
    multi method kill(Proc::Async:D: Signal:D \signal = SIGHUP) {
        $!started
          ?? nqp::killprocasync($!process_handle, signal.value)
          !! X::Proc::Async::MustBeStarted.new(:method<kill>, :proc(self)).throw
    }
    multi method kill(Proc::Async:D: Int:D \signal) {
        $!started
          ?? nqp::killprocasync($!process_handle, signal)
          !! X::Proc::Async::MustBeStarted.new(:method<kill>, :proc(self)).throw
    }

    multi method kill(Proc::Async:D: Str:D \signal) {
        $!started
          ?? nqp::killprocasync($!process_handle, $*KERNEL.signal: signal)
          !! X::Proc::Async::MustBeStarted.new(:method<kill>, :proc(self)).throw
    }
}

# vim: expandtab shiftwidth=4
