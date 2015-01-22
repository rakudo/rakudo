class IO::Dup does IO does PIO {
    has int $.fileno;

    my @name = <STDIN STDOUT STDERR>;

    method BUILD(:$fileno,|c) {
        $!fileno = $fileno;  # natives cannot be set in signature

        my Mu $PIO;
        if $!fileno == 0 {
            $PIO := nqp::getstdin();
        }
        elsif $!fileno == 1 {
            $PIO := nqp::getstdout();
        }
        elsif $!fileno == 2 {
            $PIO := nqp::getstderr();
        }
        else {
            die "Cannot dup file descriptor #$!fileno yet";
        }

        self!set-PIO-attributes(:$PIO,|c);
    }

    method WHICH(IO::Dup:D:) { "IO::Dup($!fileno)" }
    method Str(IO::Dup:D:)   { @name[$!fileno] // "Dup#$!fileno" }

    method IO(IO::Dup:D:) { self }

    method e(IO::Dup:D:)   { True }
    method d(IO::Dup:D:)   { False }
    method f(IO::Dup:D:)   { False }
    method s(IO::Dup:D:)   { 0 }
    method i(IO::Dup:D:)   { 0 }
    method l(IO::Dup:D:)   { False }
    method r(IO::Dup:D:)   { $!fileno == 0 }
    method w(IO::Dup:D:)   { $!fileno  > 0 }
    method rw(IO::Dup:D:)  { False }
    method x(IO::Dup:D:)   { False }
    method rx(IO::Dup:D:)  { False }
    method wx(IO::Dup:D:)  { False }
    method rwx(IO::Dup:D:) { False }
    method o(IO::Dup:D:)   { False }
    method z(IO::Dup:D:)   { False }
    method modified(IO::Dup:D:) { Instant }
    method accessed(IO::Dup:D:) { Instant }
    method changed(IO::Dup:D:)  { Instant }
}

PROCESS::<$ERR> = IO::Dup.new( :fileno(2) );
PROCESS::<$OUT> = IO::Dup.new( :fileno(1) );
PROCESS::<$IN>  = IO::Dup.new( :fileno(0) );

# vim: ft=perl6 expandtab sw=4
