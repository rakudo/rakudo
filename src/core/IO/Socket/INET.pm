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

class IO::Socket::INET is Cool does IO::Socket {

    method open (Str $hostname, Int $port, Int :$protocol = PIO::PROTO_TCP, Int :$family = PIO::PF_INET) {
        my $addr = IO::Socket::INET.getaddrinfo(
                $hostname,
                $port,
                protocol => $protocol,
                family => $family,
                passive => False
        );

        my $s = Q:PIR {
            .local pmc sock

            .local pmc self
            self = find_lex 'self'

            # Create the socket handle
            sock = root_new ['parrot';'Socket']
            setattribute self, '$!PIO', sock
            %r = sock
        };

        my $ret = $s.connect(pir::descalarref__PP($addr));

        unless $ret==0 { fail "IO::Socket::INET Couldn't connect."; }
        return 1;
    }

    method socket(Int $domain, Int $type, Int $protocol) {
        return IO::Socket::INET.new( :PIO(Q:PIR {{
            .local pmc pio
            .local pmc domain
            .local pmc type
            .local pmc protocol
            pio = root_new ['parrot';'Socket']
            domain   = find_lex "$domain"
            type     = find_lex "$type"
            protocol = find_lex "$protocol"
            pio.'socket'(domain, type, protocol)
            %r = pio
        }}) );
    }

    method bind (Str $hostname, Int $port, Int :$protocol = PIO::PROTO_TCP, Int :$family = PIO::PF_INET) {
        my $addr = IO::Socket::INET.getaddrinfo(
                $hostname,
                $port,
                protocol => $protocol,
                family => $family,
                passive => True
        );

        my $s = Q:PIR {
            .local pmc sock

            .local pmc self
            self = find_lex 'self'

            # Create the socket handle
            sock = root_new ['parrot';'Socket']
            setattribute self, '$!PIO', sock
            %r = sock
        };

	my $ret = $s.bind(pir::descalarref__PP($addr));

        return $s;
    }

    method listen() {
        $!PIO.listen(1);
        return self;
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

    method getaddrinfo(Str $hostname, Int $port, Int :$protocol = PIO::PROTO_TCP, Int :$family = 0, Bool :$passive = False) {
        # TODO: convert $port to Str
        my $s = Q:PIR {
            %r = root_new ['parrot';'Socket']
        };

        return $s.getaddrinfo($hostname, $port, $protocol, $family, 0);
    }
}
