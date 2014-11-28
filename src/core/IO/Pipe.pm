class IO::Pipe does IO does PIO {
    has Str $.command;

    method BUILD(:$!command,|c) { self!set-PIO-attributes(|c) }

#    method seek(|c) {
#        fail "You cannot seek on an {self.^name}";
#    }
}

# vim: ft=perl6 expandtab sw=4
