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
    has Str $.peeraddr;
    has Int $.peerport = 80;
    has Str $.localaddr;
    has Int $.localport;
    has Int $.listen;
    has $.family = PIO::PF_INET;
    has $.proto = PIO::PROTO_TCP;
    has $.type = PIO::SOCK_STREAM;

    method new (*%args is copy) {
        %args<peeraddr> //= %args<peerhost>;
        my ($peer, $port) = %args<peeraddr>.split(':', 2);
        if $port {
            %args<peerport> //= $port;
            %args<peeraddr> = $peer;
        }

        if %args<localaddr> || %args<localhost> {
            %args<localaddr> //= %args<localhost>;
            ($peer, $port) = %args<localaddr>.split(':', 2);
            if $port {
                %args<localport> //= $port;
                %args<localaddr> = $peer;
            }
        }

        #TODO: Learn what protocols map to which socket types and then determine which is needed.

        fail "Nothing given for new socket to connect or bind to" unless %args<peeraddr> || %args<listen>;

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
        if $.listen || $.localaddr || $.localport {
            my $addr = $!PIO.sockaddr($.localaddr || "0.0.0.0", $.localport || 0);
            $!PIO.bind(pir::descalarref__PP($addr));
        }

        if $.listen { 
            $!PIO.listen($.listen);
        }
        elsif $.type == PIO::SOCK_STREAM() {
            my $addr = $!PIO.sockaddr($.peeraddr, $.peerport);
            $!PIO.connect(pir::descalarref__PP($addr));    
        }
    }

    method get() {
        $!PIO.readline;
    }

    method lines() {
        gather { take $!PIO.readline };
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
