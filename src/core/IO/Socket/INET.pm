# XXX: enum does not yet work in core modules, so we use const subs
module PIO {
    our sub PF_LOCAL { 0 }
    our sub PF_UNIX { 1 }
    our sub PF_INET { 2 }
    our sub PF_INET6 { 3 }
    our sub PF_MAX { 4 }
    our sub SOCK_PACKET { 0 }
    our sub SOCK_STREAM { 1 }
    our sub SOCK_DGRAM { 2 }
    our sub SOCK_RAW { 3 }
    our sub SOCK_RDM { 4 }
    our sub SOCK_SEQPACKET { 5 }
    our sub SOCK_MAX { 6 }
    our sub PROTO_TCP { 6 }
    our sub PROTO_UDP { 17 }
}

class IO::Socket::INET does IO::Socket {
    has Str $.host;
    has Int $.port = 80;
    has Str $.localhost;
    has Int $.localport;
    has Int $.listen;
    has $.family = PIO::PF_INET;
    has $.proto = PIO::PROTO_TCP;
    has $.type = PIO::SOCK_STREAM;

    my sub v4-split($uri) {
        return $uri.split(':', 2);
    }

    my sub v6-split($uri) {
        my ($host, $port) = ($uri ~~ /^'[' (.+) ']' \: (\d+)$/)[0,1];
        return $host ?? ($host, $port) !! $uri;
	}

    method new (*%args is copy) {
        fail "Nothing given for new socket to connect or bind to" unless %args<host> || %args<listen>;

        if %args<host>  {
            my ($host, $port) = %args<family> && %args<family> == PIO::PF_INET6() 
                ?? v6-split(%args<host>)
                !! v4-split(%args<host>);
            if $port {
                %args<port> //= $port;
                %args<host> = $host;
            }
        }
        if %args<localhost> {
            my ($peer, $port) = %args<family> && %args<family> == PIO::PF_INET6() 
                ?? v6-split(%args<localhost>)
                !! v4-split(%args<localhost>);
            if $port {
                %args<localport> //= $port;
                %args<localhost> = $peer;
            }
        }

        #TODO: Learn what protocols map to which socket types and then determine which is needed.
        self.bless(*, |%args);
    }


    submethod BUILD {
        #Callsame first to get all the actual class composition+construction done.
        callsame;

        $!PIO = Q:PIR { %r = root_new ['parrot';'Socket'] };
        $!PIO.socket($.family, $.type, $.proto);        
        #Quoting perl5's SIO::INET:
        #If Listen is defined then a listen socket is created, else if the socket type, 
        #which is derived from the protocol, is SOCK_STREAM then connect() is called.
        if $.listen || $.localhost || $.localport {
            my $addr = $!PIO.sockaddr($.localhost || "0.0.0.0", $.localport || 0);
            $!PIO.bind(pir::descalarref__PP($addr));
        }

        if $.listen { 
            $!PIO.listen($.listen);
        }
        elsif $.type == PIO::SOCK_STREAM() {
            my $addr = $!PIO.sockaddr($.host, $.port);
            $!PIO.connect(pir::descalarref__PP($addr));    
        }
    }

    method get() {
        chomp($!PIO.readline);
    }

    method lines() {
        gather { take chomp($!PIO.readline) };
    }

    method accept() {
        return $!PIO.accept();
    }

    method remote_address() {
        return $!PIO.remote_address();
    }

    method local_address() {
        return $!PIO.local_address();
    }

}
