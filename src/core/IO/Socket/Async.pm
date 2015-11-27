my class IO::Socket::Async {
    my class SocketCancellation is repr('AsyncTask') { }

    has $!VMIO;

    method new() {
        die "Cannot create an asynchronous socket directly; please use " ~
            "IO::Socket::Async.connect or IO::Socket::Async.listen";
    }

    method print(IO::Socket::Async:D: Str() $str, :$scheduler = $*SCHEDULER) {
        my $p = Promise.new;
        my $v = $p.vow;
        nqp::asyncwritestr(
            $!VMIO,
            $scheduler.queue,
            -> Mu \bytes, Mu \err {
                if err {
                    $v.break(err);
                }
                else {
                    $v.keep(bytes);
                }
            },
            nqp::unbox_s($str), SocketCancellation);
        $p
    }

    method write(IO::Socket::Async:D: Blob $b, :$scheduler = $*SCHEDULER) {
        my $p = Promise.new;
        my $v = $p.vow;
        nqp::asyncwritebytes(
            $!VMIO,
            $scheduler.queue,
            -> Mu \bytes, Mu \err {
                if err {
                    $v.break(err);
                }
                else {
                    $v.keep(bytes);
                }
            },
            nqp::decont($b), SocketCancellation);
        $p
    }

    my sub capture(\supply) {
        my $ss = Rakudo::Internals::SupplySequencer.new(
            on-data-ready => -> \data { supply.emit(data) },
            on-completed  => -> { supply.done() },
            on-error      => -> \err { supply.quit(err) });
        -> Mu \seq, Mu \data, Mu \err { $ss.process(seq, data, err) }
    }

    method Supply(IO::Socket::Async:D: :$bin, :$buf = buf8.new, :$scheduler = $*SCHEDULER) {
        my $cancellation;
        Supply.on-demand:
            -> $supply {
                $cancellation := $bin
                    ?? nqp::asyncreadbytes(
                        $!VMIO,
                        $scheduler.queue,
                        capture($supply),
                        nqp::decont($buf),
                        SocketCancellation)
                    !! nqp::asyncreadchars(
                        $!VMIO,
                        $scheduler.queue,
                        capture($supply),
                        SocketCancellation)
          },
          closing => {
              $cancellation && nqp::cancel($cancellation)
          }
    }

    method chars-supply(IO::Socket::Async:D: :$scheduler = $*SCHEDULER) {
        DEPRECATED('Supply');
        self.Supply
    }

    method bytes-supply(IO::Socket::Async:D: :$scheduler = $*SCHEDULER, :$buf = buf8.new) {
        DEPRECATED('Supply(:bin)');
        self.Supply(:bin, :$buf)
    }

    method close(IO::Socket::Async:D: --> True) {
        nqp::closefh($!VMIO);
    }

    method connect(IO::Socket::Async:U: Str() $host, Int() $port,
                   :$scheduler = $*SCHEDULER) {
        my $p = Promise.new;
        my $v = $p.vow;
        nqp::asyncconnect(
            $scheduler.queue,
            -> Mu \socket, Mu \err {
                if err {
                    $v.break(err);
                }
                else {
                    my $client_socket := nqp::create(self);
                    nqp::bindattr($client_socket, IO::Socket::Async, '$!VMIO', socket);
                    $v.keep($client_socket);
                }
            },
            $host, $port, SocketCancellation);
        $p
    }

    method listen(IO::Socket::Async:U: Str() $host, Int() $port,
                  :$scheduler = $*SCHEDULER) {
        my $cancellation;
        Supply.on-demand(-> $s {
            $cancellation := nqp::asynclisten(
                $scheduler.queue,
                -> Mu \socket, Mu \err {
                    if err {
                        $s.quit(err);
                    }
                    else {
                        my $client_socket := nqp::create(self);
                        nqp::bindattr($client_socket, IO::Socket::Async, '$!VMIO', socket);
                        $s.emit($client_socket);
                    }
                },
                $host, $port, SocketCancellation);
        },
        closing => {
            $cancellation && nqp::cancel($cancellation)
        });
    }
}
