class IO::Handle does IO does PIO {
    has Str $.abspath;

    method BUILD(:$abspath,:$path,|c) {
        $!abspath := $abspath // $path.Str;
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
        DEPRECATED('$handle.slurp-rest', |<2014.10 2015.10>);
        self.slurp-rest(|c);
    }

#    method seek(|c) {
#        fail "You cannot seek on an {self.^name}";
#    }

    method e(PIO:D:)   { True  }
    method d(PIO:D:)   { False }
    method f(PIO:D:)   { FILETEST-F(  $!abspath) }
    method s(PIO:D:)   { FILETEST-S(  $!abspath) }
    method l(PIO:D:)   { FILETEST-L(  $!abspath) }
    method r(PIO:D:)   { FILETEST-R(  $!abspath) }
    method w(PIO:D:)   { FILETEST-W(  $!abspath) }
    method rw(PIO:D:)  { FILETEST-RW( $!abspath) }
    method x(PIO:D:)   { FILETEST-X(  $!abspath) }
    method rwx(PIO:D:) { FILETEST-RWX($!abspath) }
    method z(PIO:D:)   { FILETEST-Z(  $!abspath) }
    method modified(PIO:D:) { FILETEST-MODIFIED($!abspath) }
    method accessed(PIO:D:) { FILETEST-ACCESSED($!abspath) }
    method changed(PIO:D:)  { FILETEST-CHANGED( $!abspath) }

#?if moar
    method watch(IO::Handle:D:) {
        IO::Notification.watch_path($!abspath);
    }
#?endif
}

# vim: ft=perl6 expandtab sw=4
