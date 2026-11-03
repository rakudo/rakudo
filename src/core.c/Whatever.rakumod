my class X::Cannot::Capture { ... }
my class X::Cannot::New     { ... }

my class Whatever {
    multi method ACCEPTS(Whatever:D: Mu --> True) { }
    multi method raku(Whatever:D: --> '*') { }
    multi method Str(Whatever:D: --> '*') { }
    method Capture() { X::Cannot::Capture.new( :what(self) ).throw }
}

my class HyperWhatever {
    multi method new(HyperWhatever:) { X::Cannot::New.new(class => self).throw }
    multi method ACCEPTS(HyperWhatever:D: $ --> True) { }
    multi method raku(HyperWhatever:D:) { '**' }
    method Capture() { X::Cannot::Capture.new( :what(self) ).throw }
}

sub HYPERWHATEVER (&c) {  # is implementation-detail
    sub (*@_) { map &c, @_ }
}

# vim: expandtab shiftwidth=4
