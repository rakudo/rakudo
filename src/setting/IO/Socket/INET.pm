class IO::Socket::INET does IO::Socket {

    method open (Str $hostname, Int $port) {

        Q:PIR {
            .include "socket.pasm"
            .local pmc sock
            .local pmc address
            .local string hostname
            .local int port
            .local string buf
            .local int ret

            $P0 = find_lex "$hostname"
            hostname = $P0

            $P0 = find_lex "$port"
            port = $P0

            # Create the socket handle
            sock = root_new ['parrot';'Socket']
            unless sock goto ERR
            sock.'socket'(.PIO_PF_INET, .PIO_SOCK_STREAM, .PIO_PROTO_TCP)

            # Pack a sockaddr_in structure with IP and port
            address = sock.'sockaddr'(hostname, port)
            sock.'connect'(address)
            setattribute self, '$!PIO', sock
        ERR:
            .return (0)
        }
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
