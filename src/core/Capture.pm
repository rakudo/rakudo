my class Capture {
    has Mu $!list;
    has Mu $!hash;

    submethod BUILD(:$!list, :$!hash) { }

    method at_key(Capture:D: $key is copy) {
        $key = $key.Str;
        nqp::existskey($!hash, nqp::unbox_s($key))
          ?? nqp::atkey($!hash, nqp::unbox_s($key))
          !! Any
    }

    method at_pos(Capture:D: $pos is copy) {
        $pos = $pos.Int;
        nqp::existspos($!list, nqp::unbox_i($pos))
          ?? nqp::atpos($!list, nqp::unbox_i($pos))
          !! Any
    }

    method hash(Capture:D:) {
        my $enum := nqp::create(EnumMap);
        nqp::bindattr($enum, EnumMap, '$!storage', $!hash);
        $enum;
    }

    method list(Capture:D:) {
        nqp::p6list($!list, List, Mu);
    }
}
