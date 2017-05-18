my class IO::Socket::INET does IO::Socket {
    my module PIO {
        constant PF_LOCAL       = 0;
        constant PF_UNIX        = 1;
        constant PF_INET        = 2;
        constant PF_INET6       = 3;
        constant PF_MAX         = 4;
        constant SOCK_PACKET    = 0;
        constant SOCK_STREAM    = 1;
        constant SOCK_DGRAM     = 2;
        constant SOCK_RAW       = 3;
        constant SOCK_RDM       = 4;
        constant SOCK_SEQPACKET = 5;
        constant SOCK_MAX       = 6;
        constant PROTO_TCP      = 6;
        constant PROTO_UDP      = 17;
        constant MIN_PORT       = 0;
        constant MAX_PORT       = 65_535; # RFC 793: TCP/UDP port limit
    }

    has Str $.host;
    has Int $.port;
    has Str $.localhost;
    has Int $.localport;
    has Int $.backlog;
    has Bool $.listening;
    has $.family = PIO::PF_INET;
    has $.proto = PIO::PROTO_TCP;
    has $.type = PIO::SOCK_STREAM;

    my sub split-host-port(:$host is copy, :$port is copy, :$family) {
        if ($host) {
            my ($split-host, $split-port) = $family == PIO::PF_INET6
                ?? v6-split($host)
                !! v4-split($host);

            if $split-port {
                $host = $split-host.Str;
                $port //= $split-port.Int
            }
        }

        fail "Invalid port. Must be { PIO::MIN_PORT } .. { PIO::MAX_PORT }"
            unless PIO::MIN_PORT <= $port <= PIO::MAX_PORT;

        return ($host, $port);
    }

    my sub v4-split($uri) {
        return $uri.split(':', 2);
    }

    my sub v6-split($uri) {
        my ($host, $port) = ($uri ~~ /^'[' (.+) ']' \: (\d+)$/)[0,1];
        return $host ?? ($host, $port) !! $uri;
    }

    # Create new socket that listens on $localhost:$localport
    multi method new(
        Bool:D :$listen!,
        Str    :$localhost is copy,
        Int    :$localport is copy,
        Int    :$family where {
                $family == PIO::PF_INET
             || $family == PIO::PF_INET6
        } = PIO::PF_INET,
               *%rest,
        --> IO::Socket::INET:D) {

        ($localhost, $localport) = split-host-port(
            :host($localhost),
            :port($localport),
            :$family,
        );


        #TODO: Learn what protocols map to which socket types and then determine which is needed.
        self.bless(
            :$localhost,
            :$localport,
            :$family,
            :listening($listen),
            |%rest,
        )!initialize()
    }

    # Open new connection to socket on $host:$port
    multi method new(
        Str:D :$host! is copy,
        Int   :$port is copy,
        Int   :$family where {
               $family == PIO::PF_INET
            || $family == PIO::PF_INET6
        } = PIO::PF_INET,
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
            :$family,
            |%rest,
        )!initialize()
    }

    # Fail if no valid parameters are passed
    multi method new() {
        fail "Nothing given for new socket to connect or bind to";
    }

    method !initialize() {
        my $PIO := nqp::socket($.listening ?? 10 !! 0);
        #Quoting perl5's SIO::INET:
        #If Listen is defined then a listen socket is created, else if the socket type,
        #which is derived from the protocol, is SOCK_STREAM then connect() is called.
        if $.listening || $.localhost || $.localport {
            nqp::bindsock($PIO, nqp::unbox_s($.localhost || "0.0.0.0"),
                                 nqp::unbox_i($.localport || 0), nqp::unbox_i($.backlog || 128));
        }

        if $.listening {
        }
        elsif $.type == PIO::SOCK_STREAM {
            nqp::connect($PIO, nqp::unbox_s($.host), nqp::unbox_i($.port));
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

    method get() {
        my Mu $io := nqp::getattr(self, $?CLASS, '$!PIO');
        nqp::setencoding($io, Rakudo::Internals.NORMALIZE_ENCODING($!encoding));
        Rakudo::Internals.SET_LINE_ENDING_ON_HANDLE($io, $!nl-in);
        my str $line = nqp::readlinechompfh($io);
        if nqp::chars($line) || !nqp::eoffh($io) {
            $line
        }
        else {
            Nil
        }
    }

    method lines() {
        gather while (my $line = self.get()).DEFINITE {
            take $line;
        }
    }

    method accept() {
        ## A solution as proposed by moritz
        my $new_sock := $?CLASS.bless(:$!family, :$!proto, :$!type, :$!nl-in);
#?if jvm
        nqp::getattr($new_sock, $?CLASS, '$!buffer') = buf8.new;
#?endif
        nqp::bindattr($new_sock, $?CLASS, '$!PIO',
            nqp::accept(nqp::getattr(self, $?CLASS, '$!PIO'))
        );
        return $new_sock;
    }
}

# vim: ft=perl6 expandtab sw=4
