my class Encoding::Encoder::Builtin does Encoding::Encoder {
    has str $!encoding;
    has Blob $!type;

    method new(Str $encoding, Blob:U $type) {
        nqp::create(self)!setup($encoding, $type)
    }

    method !setup($encoding, $type) {
        $!encoding = $encoding;
        $!type := nqp::can($type.HOW, 'pun') ?? $type.^pun !! $type.WHAT;
        self
    }

    method encode-chars(Str:D $str --> Blob:D) {
        nqp::encode($str, $!encoding, nqp::create($!type))
    }
}

my class Encoding::Encoder::Builtin::Replacement does Encoding::Encoder {
    has str $!encoding;
    has Blob $!type;
    has str $!replacement;

    method new(Str $encoding, Blob:U $type, Str $replacement) {
        nqp::create(self)!setup($encoding, $type, $replacement)
    }

    method !setup($encoding, $type, $replacement) {
        $!encoding = $encoding;
        $!type := nqp::can($type.HOW, 'pun') ?? $type.^pun !! $type.WHAT;
        $!replacement = $replacement;
        self
    }

    method encode-chars(Str:D $str --> Blob:D) {
#?if moar
        nqp::encoderep($str, $!encoding, $!replacement, nqp::create($!type))
#?endif
#?if !moar
        X::NYI.new(feature => 'encoding with replacement').throw
#?endif
    }
}
