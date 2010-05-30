class IO::Socket::INET is Cool does IO::Socket {

    method open (Str $hostname, Int $port) {

        my $s = Q:PIR {
            .include "socket.pasm"
            .local pmc sock
            .local pmc address
            .local string hostname
            .local int port
            .local string buf
            .local int ret

            .local pmc self
            self = find_lex 'self'

            $P0 = find_lex "$hostname"
            hostname = $P0

            $P0 = find_lex "$port"
            port = $P0

            # Create the socket handle
            sock = root_new ['parrot';'Socket']
            $P1 = new 'Integer'
            unless sock goto ERR
            sock.'socket'(.PIO_PF_INET, .PIO_SOCK_STREAM, .PIO_PROTO_TCP)

            # Pack a sockaddr_in structure with IP and port
            address = sock.'sockaddr'(hostname, port)
            $P1 = sock.'connect'(address)
            setattribute self, '$!PIO', sock
            goto DONE
        ERR:
            $P1 = -1
			DONE:
            %r = $P1
        };
        unless $s==0 { fail "IO::Socket::INET Couldn't create socket."; }
        return 1;
    }

    method socket(Int $domain, Int $type, Int $protocol) {
        my $PIO := Q:PIR {{ %r = root_new ['parrot';'Socket'] }};
        $PIO.socket($domain, $type, $protocol);
        return IO::Socket::INET.new( :PIO($PIO) );
    }

    method bind($host, $port) {
        $!PIO.bind($!PIO.sockaddr($host, $port));
        return self;
    }

    method listen() {
        $!PIO.listen(1);
        return self;
    }

    method accept() {
        return $!PIO.accept();
    }
}
