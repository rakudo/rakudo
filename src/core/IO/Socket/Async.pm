my class IO::Socket::Async {
    my class SocketCancellation is repr('AsyncTask') { }

    has $!VMIO;

    method new() {
        die "Cannot create an asynchronous socket directly; please use" ~
            "IO::Socket::Async.connect or IO::Socket::Async.listen.";
    }

    method send(IO::Socket::Async:D: $str as Str, :$scheduler = $*SCHEDULER) {
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

    method write(IO::Socket::Async:D: Buf $b, :$scheduler = $*SCHEDULER) {
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

    method chars_supply(IO::Socket::Async:D: :$scheduler = $*SCHEDULER) {
        my $s = Supply.new;
        nqp::asyncreadchars(
            $!VMIO,
            $scheduler.queue,
            -> Mu \seq, Mu \data, Mu \err {
                if err {
                    $s.quit(err);
                }
                elsif seq < 0 {
                    $s.done();
                }
                else {
                    $s.more(data);
                }
            },
            SocketCancellation);
        $s
    }

    method bytes_supply(IO::Socket::Async:D: :$scheduler = $*SCHEDULER, :$buf = buf8.new) {
        my $s = Supply.new;
        nqp::asyncreadbytes(
            $!VMIO,
            $scheduler.queue,
            -> Mu \seq, Mu \data, Mu \err {
                if err {
                    $s.quit(err);
                }
                elsif seq < 0 {
                    $s.done();
                }
                else {
                    $s.more(data);
                }
            },
            nqp::decont($buf),
            SocketCancellation);
        $s
    }

    method close(IO::Socket::Async:D:) {
        nqp::closefh($!VMIO);
    }

    method connect(IO::Socket::Async:U: $host as Str, $port as Int,
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

    method listen(IO::Socket::Async:U: $host as Str, $port as Int,
                  :$scheduler = $*SCHEDULER) {
        my $s = Supply.new;
        nqp::asynclisten(
            $scheduler.queue,
            -> Mu \socket, Mu \err {
                if err {
                    $s.quit(err);
                }
                else {
                    my $client_socket := nqp::create(self);
                    nqp::bindattr($client_socket, IO::Socket::Async, '$!VMIO', socket);
                    $s.more($client_socket);
                }
            },
            $host, $port, SocketCancellation);
        $s
    }
}
