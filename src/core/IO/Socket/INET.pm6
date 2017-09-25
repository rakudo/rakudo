my class IO::Socket::INET does IO::Socket {
    my module PIO {
        constant MIN_PORT       = 0;
        constant MAX_PORT       = 65_535; # RFC 793: TCP/UDP port limit
    }

    has Str $.host;
    has Int $.port;
    has Str $.localhost;
    has Int $.localport;
    has Int $.backlog;
    has Bool $.listening;
    has $.family = PF_UNSPEC;
    has $.proto = PROTO_TCP;
    has $.type = SOCK_STREAM;



    my sub split-host-port(:$host is copy, :$port is copy, :$family) {
        if $family === PF_UNIX {
            return ($host, $port);
        }

        if ($host) {
            my ($split-host, $split-port) = (Str, Int);

            if $family !== PF_INET6 {
                ($split-host, $split-port) = v4-split($host);
            }

            if ! $split-port && $family !== PF_INET {
                ($split-host, $split-port) = v6-split($host);
            }

            if $split-port {
                $host = $split-host.Str;
                $port //= $split-port.Int;
            }
        }

        fail "Invalid port $port.gist(). Must be {PIO::MIN_PORT}..{PIO::MAX_PORT}"
            unless $port.defined and PIO::MIN_PORT <= $port <= PIO::MAX_PORT;

        return ($host, $port);
    }

    my sub v4-split($uri) {
        return $uri.split(':', 2);
    }

    my sub v6-split($uri) {
        my ($host, $port) = ($uri ~~ /^'[' (.+) ']' \: (\d+)$/)[0,1];
        return $host ?? ($host, $port) !! $uri;
    }

    method !correct-family(Int:D $family) {
        if ($family == 3) {
            note "You're using the old value (3) for IPv6 / PF_NET6, " ~
                "please use the constant PF_INET6 to specify IPv6";
            note Backtrace.new.Str;

            return PF_INET6;
        }

        return $family;
    }

    # Create new socket that listens on $localhost:$localport
    multi method new(
        Bool:D :$listen!,
        Str    :$localhost is copy,
        Int    :$localport is copy,
        Int    :$family    is copy where {
                $family == PF_INET
             || $family == PF_INET6
             || $family == PF_UNIX
             || $family == PF_UNSPEC
             || $family == 3 # legacy PF_INET6
        } = PF_UNSPEC,
               *%rest,
        --> IO::Socket::INET:D) {


        ($localhost, $localport) = (
            split-host-port :host($localhost), :port($localport), :$family
        orelse fail $_);

        #TODO: Learn what protocols map to which socket types and then determine which is needed.
        self.bless(
            :$localhost,
            :$localport,
            :family(self!correct-family($family)),
            :listening($listen),
            |%rest,
        )!initialize()
    }

    # Open new connection to socket on $host:$port
    multi method new(
        Str:D :$host! is copy,
        Int   :$port is copy,
        Int   :$family where {
               $family == PF_INET
            || $family == PF_INET6
            || $family == PF_UNIX
            || $family == PF_UNSPEC
            || $family == 3 # legacy PF_INET6
        } = PF_UNSPEC,
              *%rest,
        --> IO::Socket::INET:D) {

        ($host, $port) = split-host-port(
            :$host,
            :$port,
            :$family,
        );

        #TODO: Learn what protocols map to which socket types and then determine which is needed.
        self.bless(
            :$host,
            :$port,
            :family(self!correct-family($family)),
            |%rest,
        )!initialize()
    }

    # Fail if no valid parameters are passed
    multi method new() {
        fail "Nothing given for new socket to connect or bind to. "
            ~ "Invalid arguments to .new?";
    }

    # returns the localport combined with address family
    method !get-packed-localport() {
        self!pack-port($!localport || 0);
    }

    # returns the localport combined with address family
    method !get-packed-port() {
        self!pack-port($!port);
    }

    # pack port with address family
    method !pack-port(Int:D $port) {
        return $port + ($.family +< 16)
    }

    method !initialize() {
        if ! $.host && ! $.localhost && $.family == PF_UNIX {
            fail "Socket with family AF_UNIX needs a path to bind to";
        }

        my $PIO := nqp::socket($.listening ?? 10 !! 0);

        #Quoting perl5's SIO::INET:
        #If Listen is defined then a listen socket is created, else if the socket type.
        #which is derived from the protocol, is SOCK_STREAM then connect() is called.
        if $.listening || $.localhost || $.localport {
            nqp::bindsock($PIO, nqp::unbox_s($.localhost || ($.family == PF_INET ?? "0.0.0.0" !! '::0')),
                                 nqp::unbox_i(self!get-packed-localport()), nqp::unbox_i($.backlog || 128));
        }

        if $.listening {
#?if !js
            $!localport = nqp::getport($PIO) if !$!localport;
#?endif
        }
        elsif $.type == SOCK_STREAM {
            nqp::connect($PIO, nqp::unbox_s($.host), nqp::unbox_i(self!get-packed-port()));
        }

        nqp::bindattr(self, $?CLASS, '$!PIO', $PIO);
        self;
    }

    method connect(IO::Socket::INET:U: Str() $host, Int() $port) {
        self.new(:$host, :$port)
    }

    method listen(IO::Socket::INET:U: Str() $localhost, Int() $localport) {
        self.new(:$localhost, :$localport, :listen)
    }

    method accept() {
        ## A solution as proposed by moritz
        my $new_sock := $?CLASS.bless(:$!family, :$!proto, :$!type, :$!nl-in);
        nqp::bindattr($new_sock, $?CLASS, '$!PIO',
            nqp::accept(nqp::getattr(self, $?CLASS, '$!PIO'))
        );
        return $new_sock;
    }
}

# vim: ft=perl6 expandtab sw=4
