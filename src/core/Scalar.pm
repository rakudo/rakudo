# This is mostly set up in BOOTSTRAP.pm.
my class Scalar {
    method name() {
        my $d := $!descriptor;
        nqp::isnull($d) ?? Str !! $d.name()
    }
}
