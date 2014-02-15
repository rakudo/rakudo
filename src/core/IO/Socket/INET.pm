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
    }

    has Str $.encoding = 'utf8';
    has Str $.host;
    has Int $.port = 80;
    has Str $.localhost;
    has Int $.localport;
    has Bool $.listen;
    has $.family = PIO::PF_INET;
    has $.proto = PIO::PROTO_TCP;
    has $.type = PIO::SOCK_STREAM;
    has Str $.input-line-separator is rw = "\n";
    has Int $.ins = 0;

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

        %args<listen>.=Bool if %args.exists_key('listen');

        #TODO: Learn what protocols map to which socket types and then determine which is needed.
        self.bless(|%args)!initialize()
    }
    
    method !initialize() {
#?if parrot
        my $PIO := Q:PIR { %r = root_new ['parrot';'Socket'] };
        $PIO.socket($.family, $.type, $.proto);        
#?endif
#?if !parrot
        my $PIO := nqp::socket($.listen ?? 10 !! 0);
#?endif
        #Quoting perl5's SIO::INET:
        #If Listen is defined then a listen socket is created, else if the socket type, 
        #which is derived from the protocol, is SOCK_STREAM then connect() is called.
        if $.listen || $.localhost || $.localport {
#?if parrot
            my $addr := $PIO.sockaddr($.localhost || "0.0.0.0", $.localport || 0);
            $PIO.bind($addr);
#?endif
#?if !parrot
            nqp::bindsock($PIO, nqp::unbox_s($.localhost || "0.0.0.0"),
                                 nqp::unbox_i($.localport || 0));
#?endif
        }

        if $.listen {
#?if parrot
            $PIO.listen($.listen);
#?endif
        }
        elsif $.type == PIO::SOCK_STREAM {
#?if parrot
            my $addr := $PIO.sockaddr($.host, $.port);
            $PIO.connect($addr);
#?endif
#?if !parrot
            nqp::connect($PIO, nqp::unbox_s($.host), nqp::unbox_i($.port));
#?endif
        }
        
        nqp::bindattr(self, $?CLASS, '$!PIO', $PIO);
        self;
    }

    method get() {
#?if parrot
        my str $encoding = nqp::unbox_s(NORMALIZE_ENCODING($!encoding));
        my str $sep = pir::trans_encoding__SSI(
            nqp::unbox_s($!input-line-separator),
            pir::find_encoding__IS($encoding));
        my int $sep-len = nqp::chars($sep);
        
        my Mu $PIO := nqp::getattr(self, $?CLASS, '$!PIO');
        $PIO.encoding($encoding);
        
        my str $line = $PIO.readline($sep);
#?endif
#?if jvm
        my str $sep = nqp::unbox_s($!input-line-separator);
        my int $sep-len = nqp::chars($sep);
        
        my Mu $io := nqp::getattr(self, $?CLASS, '$!PIO');
        nqp::setencoding($io, nqp::unbox_s($!encoding));
        nqp::setinputlinesep($io, $sep);
        my Str $line = nqp::p6box_s(nqp::readlinefh($io));
#?endif
#?if moar
        my str $sep = nqp::unbox_s($!input-line-separator);
        my int $sep-len = nqp::chars($sep);
        
        my Mu $io := nqp::getattr(self, $?CLASS, '$!PIO');
        nqp::setencoding($io, nqp::unbox_s($!encoding));
        # XXX NYI
        # nqp::setinputlinesep($io, $sep);
        my Str $line = nqp::p6box_s(nqp::readlinefh($io));
#?endif

        my int $len  = nqp::chars($line);
        
        if $len == 0 { Str }
        else {
            ++$!ins;
            $len >= $sep-len && nqp::substr($line, $len - $sep-len) eq $sep
                ?? nqp::p6box_s(nqp::substr($line, 0, $len - $sep-len))
                !! nqp::p6box_s($line);
        }
#?endif
    }

    method lines() {
        gather while (my $line = self.get()).defined {
            take $line;
        }
    }

    method accept() {
        ## A solution as proposed by moritz
        my $new_sock := $?CLASS.bless(:$!family, :$!proto, :$!type, :$!input-line-separator);
#?if parrot
        nqp::getattr($new_sock, $?CLASS, '$!buffer') = '';
#?endif
#?if jvm
        nqp::getattr($new_sock, $?CLASS, '$!buffer') = buf8.new;
#?endif
        nqp::bindattr($new_sock, $?CLASS, '$!PIO',
#?if parrot
            nqp::getattr(self, $?CLASS, '$!PIO').accept()
#?endif
#?if !parrot
            nqp::accept(nqp::getattr(self, $?CLASS, '$!PIO'))
#?endif
        );
        return $new_sock;
    }

    method remote_address() {
#?if parrot
        return nqp::p6box_s(nqp::getattr(self, $?CLASS, '$!PIO').remote_address());
#?endif
    }

    method local_address() {
#?if parrot
        return nqp::p6box_s(nqp::getattr(self, $?CLASS, '$!PIO').local_address());
#?endif
    }
}
