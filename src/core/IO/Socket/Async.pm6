my class IO::Socket::Async {
    my class SocketCancellation is repr('AsyncTask') { }

    has $!VMIO;
    has int $!udp;
    has $.enc;
    has $!encoder;
    has $!close-promise;
    has $!close-vow;

    has Str $.peer-host;
    has Int $.peer-port;

    has Str $.socket-host;
    has Int $.socket-port;

    has Int $.native-descriptor;

    method new() {
        die "Cannot create an asynchronous socket directly; please use\n" ~
            "IO::Socket::Async.connect, IO::Socket::Async.listen,\n" ~
            "IO::Socket::Async.udp, or IO::Socket::Async.udp-bind";
    }

    method print(IO::Socket::Async:D: Str() $str, :$scheduler = $*SCHEDULER) {
        self.write($!encoder.encode-chars($str))
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

    my class Datagram {
        has $.data;
        has str $.hostname;
        has int $.port;

        method decode(|c) {
            die "Cannot decode a datagram with Str data" if $!data ~~ Str;
            return self.clone(data => $!data.decode(|c));
        }
        method encode(|c) {
            die "Cannot encode a datagram with Blob data" if $!data ~~ Blob;
            return self.clone(data => $!data.encode(|c));
        }
    }

    my class SocketReaderTappable does Tappable {
        has $!VMIO;
        has $!scheduler;
        has $!buf;
        has $!close-promise;
        has $!udp;

        method new(Mu :$VMIO!, :$scheduler!, :$buf!, :$close-promise!, :$udp!) {
            self.CREATE!SET-SELF($VMIO, $scheduler, $buf, $close-promise, $udp)
        }

        method !SET-SELF(Mu $!VMIO, $!scheduler, $!buf, $!close-promise, $!udp) { self }

        method tap(&emit, &done, &quit, &tap) {
            my $buffer := nqp::list();
            my int $buffer-start-seq = 0;
            my int $done-target = -1;
            my int $finished = 0;

            sub emit-events() {
                until nqp::elems($buffer) == 0 || nqp::isnull(nqp::atpos($buffer, 0)) {
                    emit(nqp::shift($buffer));
                    $buffer-start-seq = $buffer-start-seq + 1;
                }
                if $buffer-start-seq == $done-target {
                    done();
                    $finished = 1;
                }
            }

            my $lock = Lock::Async.new;
            my $tap;
            $lock.protect: {
                my $cancellation := nqp::asyncreadbytes(nqp::decont($!VMIO),
                    $!scheduler.queue(:hint-affinity),
                    -> Mu \seq, Mu \data, Mu \err, Mu \hostname = Str, Mu \port = Int {
                        $lock.protect: {
                            unless $finished {
                                if err {
                                    quit(X::AdHoc.new(payload => err));
                                    $finished = 1;
                                }
                                elsif nqp::isconcrete(data) {
                                    my int $insert-pos = seq - $buffer-start-seq;
                                    if $!udp && nqp::isconcrete(hostname) && nqp::isconcrete(port) {
                                        nqp::bindpos($buffer, $insert-pos, Datagram.new(
                                            data => data,
                                            hostname => hostname,
                                            port => port
                                        ));
                                    } else {
                                        nqp::bindpos($buffer, $insert-pos, data);
                                    }
                                    emit-events();
                                }
                                else {
                                    $done-target = seq;
                                    emit-events();
                                }
                            }
                        }
                    },
                    nqp::decont($!buf), SocketCancellation);
                $tap := Tap.new({ nqp::cancel($cancellation) });
                tap($tap);
            }
            $!close-promise.then: {
                $lock.protect: {
                    unless $finished {
                        done();
                        $finished = 1;
                    }
                }
            }

            $tap
        }

        method live(--> False) { }
        method sane(--> True) { }
        method serial(--> True) { }
    }

    multi method Supply(IO::Socket::Async:D: :$bin, :$buf = buf8.new, :$datagram, :$enc, :$scheduler = $*SCHEDULER) {
        if $bin {
            Supply.new: SocketReaderTappable.new:
                :$!VMIO, :$scheduler, :$buf, :$!close-promise, udp => $!udp && $datagram
        }
        else {
            my $bin-supply = self.Supply(:bin, :$datagram);
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
        $!close-vow.keep(True);
    }

    method connect(IO::Socket::Async:U: Str() $host, Int() $port,
                   :$enc = 'utf-8', :$scheduler = $*SCHEDULER) {
        my $p = Promise.new;
        my $v = $p.vow;
        my $encoding = Encoding::Registry.find($enc);
        nqp::asyncconnect(
            $scheduler.queue,
            -> Mu \socket, Mu \err, Mu \peer-host, Mu \peer-port,
               Mu \socket-host, Mu \socket-port, Mu \native-descriptor {
                if err {
                    $v.break(err);
                }
                else {
                    my $client_socket := nqp::create(self);
                    nqp::bindattr($client_socket, IO::Socket::Async, '$!VMIO', socket);
                    nqp::bindattr($client_socket, IO::Socket::Async, '$!enc', $encoding.name);
                    nqp::bindattr($client_socket, IO::Socket::Async, '$!encoder',
                        $encoding.encoder());
                    nqp::bindattr($client_socket, IO::Socket::Async, '$!peer-host', peer-host);
                    nqp::bindattr($client_socket, IO::Socket::Async, '$!peer-port', peer-port);
                    nqp::bindattr($client_socket, IO::Socket::Async, '$!socket-host', socket-host);
                    nqp::bindattr($client_socket, IO::Socket::Async, '$!socket-port', socket-port);
                    nqp::bindattr($client_socket, IO::Socket::Async, '$!native-descriptor', native-descriptor);

                    setup-close($client_socket);
                    $v.keep($client_socket);
                }
            },
            $host, $port, SocketCancellation);
        $p
    }

    class ListenSocket is Tap {
        has Promise $.socket-host;
        has Promise $.socket-port;
        has Promise $.native-descriptor;

        method new(&on-close, Promise :$socket-host, Promise :$socket-port, Promise :$native-descriptor) {
            self.bless(:&on-close, :$socket-host, :$socket-port, :$native-descriptor);
        }
    }

    my class SocketListenerTappable does Tappable {
        has $!host;
        has $!port;
        has $!backlog;
        has $!encoding;
        has $!scheduler;

        method new(:$host!, :$port!, :$backlog!, :$encoding!, :$scheduler!) {
            self.CREATE!SET-SELF($host, $port, $backlog, $encoding, $scheduler)
        }

        method !SET-SELF($!host, $!port, $!backlog, $!encoding, $!scheduler) { self }

        method tap(&emit, &done, &quit, &tap) {
            my $lock := Lock::Async.new;
            my $tap;
            my int $finished = 0;
            my Promise $socket-host       .= new;
            my Promise $socket-port       .= new;
            my Promise $native-descriptor .= new;
            my $host-vow                   = $socket-host.vow;
            my $port-vow                   = $socket-port.vow;
            my $descriptor-vow             = $native-descriptor.vow;
            $lock.protect: {
                my $cancellation := nqp::asynclisten(
                    $!scheduler.queue(:hint-affinity),
                    -> Mu \socket, Mu \err, Mu \peer-host, Mu \peer-port, Mu \peer-native-descriptor,
                       Mu \socket-host, Mu \socket-port, Mu \socket-native-descriptor {
                        $lock.protect: {
                            if $finished {
                                # do nothing
                            }
                            elsif err {
                                my $exc = X::AdHoc.new(payload => err);
                                quit($exc);
                                $host-vow.break($exc)       unless $host-vow.promise;
                                $port-vow.break($exc)       unless $port-vow.promise;
                                $descriptor-vow.break($exc) unless $descriptor-vow.promise;
                                $finished = 1;
                            }
                            elsif socket {
                                my $client_socket := nqp::create(IO::Socket::Async);
                                nqp::bindattr($client_socket, IO::Socket::Async,
                                    '$!VMIO', socket);
                                nqp::bindattr($client_socket, IO::Socket::Async,
                                    '$!enc', $!encoding.name);
                                nqp::bindattr($client_socket, IO::Socket::Async,
                                    '$!encoder', $!encoding.encoder());
                                nqp::bindattr($client_socket, IO::Socket::Async,
                                    '$!peer-host', peer-host);
                                nqp::bindattr($client_socket, IO::Socket::Async,
                                    '$!peer-port', peer-port);
                                nqp::bindattr($client_socket, IO::Socket::Async,
                                    '$!socket-host', socket-host);
                                nqp::bindattr($client_socket, IO::Socket::Async,
                                    '$!socket-port', socket-port);
                                nqp::bindattr($client_socket, IO::Socket::Async,
                                    '$!native-descriptor', peer-native-descriptor);
                                setup-close($client_socket);
                                emit($client_socket);
                            }

                            if socket-host {
                                $host-vow.keep(~socket-host);
                                $port-vow.keep(+socket-port);
                                $descriptor-vow.keep(+socket-native-descriptor);
                            }
                        }
                    },
                    $!host, $!port, $!backlog, SocketCancellation);
                $tap = ListenSocket.new: {
                    my $p = Promise.new;
                    my $v = $p.vow;
                    nqp::cancelnotify($cancellation, $!scheduler.queue, { $v.keep(True); });
                    $p
                }, :$socket-host, :$socket-port, :$native-descriptor;
                tap($tap);
                CATCH {
                    default {
                        tap($tap = ListenSocket.new({ Nil },
                            :$socket-host, :$socket-port, :$native-descriptor)) unless $tap;
                        quit($_);
                    }
                }
            }
            $tap
        }

        method live(--> False) { }
        method sane(--> True) { }
        method serial(--> True) { }
    }

    method listen(IO::Socket::Async:U: Str() $host, Int() $port, Int() $backlog = 128,
                  :$enc = 'utf-8', :$scheduler = $*SCHEDULER) {
        my $encoding = Encoding::Registry.find($enc);
        Supply.new: SocketListenerTappable.new:
            :$host, :$port, :$backlog, :$encoding, :$scheduler
    }

    sub setup-close(\socket --> Nil) {
        my $p := Promise.new;
        nqp::bindattr(socket, IO::Socket::Async, '$!close-promise', $p);
        nqp::bindattr(socket, IO::Socket::Async, '$!close-vow', $p.vow);
    }

#?if moar
    method udp(IO::Socket::Async:U: :$broadcast, :$enc = 'utf-8', :$scheduler = $*SCHEDULER) {
        my $p = Promise.new;
        my $encoding = Encoding::Registry.find($enc);
        nqp::asyncudp(
            $scheduler.queue,
            -> Mu \socket, Mu \err, Mu \native-descriptor {
                if err {
                    $p.break(err);
                }
                else {
                    my $client_socket := nqp::create(self);
                    nqp::bindattr($client_socket, IO::Socket::Async, '$!VMIO', socket);
                    nqp::bindattr_i($client_socket, IO::Socket::Async, '$!udp', 1);
                    nqp::bindattr($client_socket, IO::Socket::Async, '$!enc', $encoding.name);
                    nqp::bindattr($client_socket, IO::Socket::Async, '$!encoder',
                        $encoding.encoder());
                    nqp::bindattr($client_socket, IO::Socket::Async,
                        '$!native-descriptor', native-descriptor);
                    setup-close($client_socket);
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
        my $encoding = Encoding::Registry.find($enc);
        nqp::asyncudp(
            $scheduler.queue(:hint-affinity),
            -> Mu \socket, Mu \err, Mu \native-descriptor {
                if err {
                    $p.break(err);
                }
                else {
                    my $client_socket := nqp::create(self);
                    nqp::bindattr($client_socket, IO::Socket::Async, '$!VMIO', socket);
                    nqp::bindattr_i($client_socket, IO::Socket::Async, '$!udp', 1);
                    nqp::bindattr($client_socket, IO::Socket::Async, '$!enc', $encoding.name);
                    nqp::bindattr($client_socket, IO::Socket::Async, '$!encoder',
                        $encoding.encoder());
                    nqp::bindattr($client_socket, IO::Socket::Async,
                        '$!native-descriptor', native-descriptor);
                    setup-close($client_socket);
                    $p.keep($client_socket);
                }
            },
            nqp::unbox_s($host), nqp::unbox_i($port), $broadcast ?? 1 !! 0,
            SocketCancellation);
        await $p
    }

    method print-to(IO::Socket::Async:D: Str() $host, Int() $port, Str() $str, :$scheduler = $*SCHEDULER) {
        self.write-to($host, $port, $!encoder.encode-chars($str), :$scheduler)
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

# vim: ft=perl6 expandtab sw=4
