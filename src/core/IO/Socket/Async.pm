my class IO::Socket::Async {
    my class SocketCancellation is repr('AsyncTask') { }

    has $!VMIO;

    method new() {
        die "Cannot create an asynchronous socket directly; please use" ~
            "IO::Socket::Async.connect or IO::Socket::Async.listen.";
    }

    method send(IO::Socket::Async:D: Str() $str, :$scheduler = $*SCHEDULER) {
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

        my $lock = Lock.new;
        my int $emitting;
        my int $next_seq;
        my @buffer; # should be Mu, as data can be Mu

        -> Mu \seq, Mu \data, Mu \err {
            if err {
                supply.quit(err);
            }
            elsif seq < 0 {
                supply.done();
            }
            else {
                # cannot simply return out of here, so we need a flag
                my int $in_charge;

                $lock.protect( {
#say "seq = {seq} with {data}   in {$*THREAD}";
                    @buffer[ seq - $next_seq ] := data;
                    $in_charge = $emitting = 1 unless $emitting;
                } );

                if $in_charge {
                    my int $done;
                    while @buffer.exists_pos($done) {
#say "emitting { $next_seq + $done }: {@buffer[$done]}";
                        supply.emit( @buffer[$done] );
                        $done = $done + 1;
                    }

                    $lock.protect( {
                        if $done {
#say "discarding from $next_seq for $done";
                            @buffer.splice(0,$done);
                            $next_seq = $next_seq + $done;
                        }
                        $emitting = 0;
                    } );
                }
            }
        };
    }

    method chars_supply(IO::Socket::Async:D: :$scheduler = $*SCHEDULER) {
        my $cancellation;
        Supply.on_demand( -> $supply {
            $cancellation := nqp::asyncreadchars(
              $!VMIO,
              $scheduler.queue,
              capture($supply),
              SocketCancellation
            );
          },
          closing => {
              $cancellation && nqp::cancel($cancellation)
          },
        );
    }

    method bytes_supply(IO::Socket::Async:D: :$scheduler = $*SCHEDULER, :$buf = buf8.new) {
        my $cancellation;
        Supply.on_demand( -> $supply {
            $cancellation := nqp::asyncreadbytes(
              $!VMIO,
              $scheduler.queue,
              capture($supply),
              nqp::decont($buf),
              SocketCancellation,
            );
          },
          closing => {
              $cancellation && nqp::cancel($cancellation)
          },
        );
    }

    method close(IO::Socket::Async:D:) {
        nqp::closefh($!VMIO);
        True;
    }

    method connect(IO::Socket::Async:U: Str() $host, Int(Any) $port,
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

    method listen(IO::Socket::Async:U: Str() $host, Int(Any) $port,
                  :$scheduler = $*SCHEDULER) {
        my $cancellation;
        Supply.on_demand(-> $s {
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
