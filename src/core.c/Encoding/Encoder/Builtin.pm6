my class Encoding::Encoder::Builtin does Encoding::Encoder {
    has str $!encoding;
    has Blob $!type;
    has $!replacement;
    has int $!config;

    method new(Str $encoding, Blob:U $type, :$replacement, :$strict) {
        nqp::create(self)!setup($encoding, $type, :$replacement, :$strict)
    }
    method !setup($encoding, $type, :$replacement, :$strict) {
        $!encoding = $encoding;
        $!type := nqp::can($type.HOW, 'pun') ?? $type.^pun !! $type.WHAT;
        $!replacement = $replacement.defined ?? $replacement !! nqp::null_s();
        $!config = $strict ?? 0 !! 1;
#?if jvm
        X::NYI.new(feature => 'encoding with replacement').throw if $replacement.defined;
        X::NYI.new(feature => 'encoding with strict').throw if $strict;
#?endif
        self
    }

    method encode-chars(str $str --> Blob:D) {
#?if !jvm
        nqp::encoderepconf($str,
            $!encoding,
            $!replacement,
            nqp::create($!type),
            $!config)
#?endif
#?if jvm
        nqp::encode($str, $!encoding, nqp::create($!type));

#?endif
    }
}

# vim: expandtab shiftwidth=4
