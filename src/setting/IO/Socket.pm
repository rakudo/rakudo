#!/usr/bin/perl6

class IO::Socket {

	has $!PIO;

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
			sock = new 'Socket'
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

	method recv () {
		fail('Socket not available') unless $!PIO;
		my $received = $!PIO.recv();
		my $len = $received.chars;
		my $buf;
		while $len > 0 {
			$buf = $!PIO.recv();
			$received ~= $buf;
			$len = $buf.chars;
		}
		return $received;
	}

	method send (Str $string) {
		fail("Not connected") unless $!PIO;
		return $!PIO.send($string);
	}

	method close () {
		fail("Not connected!") unless $!PIO;
		return $!PIO.close();
	}

}

