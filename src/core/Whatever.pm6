my class X::Cannot::Capture { ... }
my class X::Cannot::New     { ... }

my class Whatever {
    multi method ACCEPTS(Whatever:D: Mu --> True) { }
    multi method perl(Whatever:D: --> '*') { }
    multi method Str(Whatever:D: --> '*') { }
    method Capture() { X::Cannot::Capture.new( :what(self) ).throw }
}

my class HyperWhatever {
    multi method new(HyperWhatever:) { X::Cannot::New.new(class => self).throw }
    multi method ACCEPTS(HyperWhatever:D: $ --> True) { }
    multi method perl(HyperWhatever:D:) { '**' }
    method Capture() { X::Cannot::Capture.new( :what(self) ).throw }
}

sub HYPERWHATEVER (&c) { sub (*@_) { map &c, @_ } }

# vim: ft=perl6 expandtab sw=4
