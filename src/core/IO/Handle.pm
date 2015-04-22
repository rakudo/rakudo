class IO::Handle does IO does PIO does IO::Pathy {

    submethod BUILD(:$abspath,:$path,|c) {
        $!abspath = $abspath // $path.Str;
        self!set-PIO-attributes(|c);
    }

    method open(IO::Handle:D: :$enc,:$p,|c) {
        DEPRECATED('.IO.open or open(...)',|<2014.12 2015.12>);
        DEPRECATED(":encoding($enc)",|<2014.12 2015.12>,:what(":enc($enc)"))
          if $enc;
        DEPRECATED('pipe($path,...)',|<2014.12 2015.12>,:what(':p for pipe'))
          if $p;

        open($!abspath,:$enc,:$p,:nodepr,|c);
    }

    method pipe(IO::Handle:D: :$enc,|c) {
        DEPRECATED('.IO.pipe or pipe(...)',|<2014.12 2015.12>);
        DEPRECATED(":encoding($enc)",|<2014.12 2015.12>,:what(":enc($enc)"))
          if $enc;
        pipe($!abspath,:$enc,:nodepr,|c);
    }

    method slurp(IO::Handle:D: |c) {
        DEPRECATED('$handle.slurp-rest', |<2014.10 2015.09>);
        self.slurp-rest(|c);
    }
    method spurt(IO::Handle:D: \what,:$nodepr) {
        DEPRECATED(".IO.spurt or spurt(...)", |<2014.10 2015.09>)
          unless $nodepr;
        what ~~ Blob
          ?? self.write(what)
          !! self.print(what);
    }

    multi method Str(IO::Handle:D:)  { "open('$.relative')" }
    multi method gist(IO::Handle:D:) { "open('$.relative')" }
    multi method perl(IO::Handle:D:) {
        "open('$.relative',...)";
    }

    method d(PIO:D:) { False }
    method f(PIO:D:) { True }
}

# vim: ft=perl6 expandtab sw=4
