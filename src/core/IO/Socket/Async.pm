my class IO::Socket::Async {
    my class SocketCancellation is repr('AsyncTask') { }

    has $!VMIO;
    has int $!udp;
    has $.enc;

    method new() {
        die "Cannot create an asynchronous socket directly; please use\n" ~
            "IO::Socket::Async.connect, IO::Socket::Async.listen,\n" ~
            "IO::Socket::Async.udp, or IO::Socket::Async.udp-bind";
    }

    method print(IO::Socket::Async:D: Str() $str, :$scheduler = $*SCHEDULER) {
        self.write($str.encode($!enc))
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

    method Supply(IO::Socket::Async:D: :$bin, :$buf = buf8.new, :$enc, :$scheduler = $*SCHEDULER) {
        if $bin {
            my $cancellation;
            Supply.on-demand:
                -> $supply {
                    $cancellation := nqp::asyncreadbytes($!VMIO, $scheduler.queue,
                        capture($supply), nqp::decont($buf), SocketCancellation)
                    },
                    closing => {
                        $cancellation && nqp::cancel($cancellation)
                    }
        }
        else {
            my $bin-supply = self.Supply(:bin);
            if $!udp {
                supply {
                    whenever $bin-supply {
                        emit .decode($enc // $!enc);
                    }
                }
            }
            else {
                Rakudo::Internals.BYTE_SUPPLY_DECODER($bin-supply, $enc // $!enc)
            }
        }
    }

    method close(IO::Socket::Async:D: --> True) {
        nqp::closefh($!VMIO);
    }

    method connect(IO::Socket::Async:U: Str() $host, Int() $port,
                   :$enc = 'utf-8', :$scheduler = $*SCHEDULER) {
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
                    nqp::bindattr($client_socket, IO::Socket::Async, '$!enc', $enc);
                    $v.keep($client_socket);
                }
            },
            $host, $port, SocketCancellation);
        $p
    }

    method listen(IO::Socket::Async:U: Str() $host, Int() $port, Int() $backlog = 128,
                  :$enc = 'utf-8', :$scheduler = $*SCHEDULER) {
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
                        nqp::bindattr($client_socket, IO::Socket::Async, '$!enc', $enc);
                        $s.emit($client_socket);
                    }
                },
                $host, $port, $backlog, SocketCancellation);
        },
        closing => {
            if $cancellation {
                my $p = Promise.new;
                my $v = $p.vow;
                nqp::cancelnotify($cancellation, $scheduler.queue, { $v.keep(True); });
                $p
            }
        });
    }

#?if moar
    method udp(IO::Socket::Async:U: :$broadcast, :$enc = 'utf-8', :$scheduler = $*SCHEDULER) {
        my $p = Promise.new;
        nqp::asyncudp(
            $scheduler.queue,
            -> Mu \socket, Mu \err {
                if err {
                    $p.break(err);
                }
                else {
                    my $client_socket := nqp::create(self);
                    nqp::bindattr($client_socket, IO::Socket::Async, '$!VMIO', socket);
                    nqp::bindattr_i($client_socket, IO::Socket::Async, '$!udp', 1);
                    nqp::bindattr($client_socket, IO::Socket::Async, '$!enc', $enc);
                    $p.keep($client_socket);
                }
            },
            nqp::null_s(), 0, $broadcast ?? 1 !! 0,
            SocketCancellation);
        await $p
    }

    method bind-udp(IO::Socket::Async:U: Str() $host, Int() $port, :$broadcast,
                    :$enc = 'utf-8', :$scheduler = $*SCHEDULER) {
        my $p = Promise.new;
        nqp::asyncudp(
            $scheduler.queue,
            -> Mu \socket, Mu \err {
                if err {
                    $p.break(err);
                }
                else {
                    my $client_socket := nqp::create(self);
                    nqp::bindattr($client_socket, IO::Socket::Async, '$!VMIO', socket);
                    nqp::bindattr_i($client_socket, IO::Socket::Async, '$!udp', 1);
                    nqp::bindattr($client_socket, IO::Socket::Async, '$!enc', $enc);
                    $p.keep($client_socket);
                }
            },
            nqp::unbox_s($host), nqp::unbox_i($port), $broadcast ?? 1 !! 0,
            SocketCancellation);
        await $p
    }

    method print-to(IO::Socket::Async:D: Str() $host, Int() $port, Str() $str, :$scheduler = $*SCHEDULER) {
        self.write-to($host, $port, $str.encode($!enc))
    }

    method write-to(IO::Socket::Async:D: Str() $host, Int() $port, Blob $b, :$scheduler = $*SCHEDULER) {
        my $p = Promise.new;
        my $v = $p.vow;
        nqp::asyncwritebytesto(
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
            nqp::decont($b), SocketCancellation,
            nqp::unbox_s($host), nqp::unbox_i($port));
        $p
    }
#?endif
}
