class Encoding::Builtin does Encoding {
    has Str $.name;
    has $!alternative-names;

    method new() {
        X::Cannot::New.new(class => self.WHAT).throw
    }

    method SET-SELF(\name, \alternatives) is implementation-detail {
        $!name := name;
        $!alternative-names := alternatives;
        self
    }

    method alternative-names() { $!alternative-names }

    method decoder(:$replacement, :$translate-nl, :$strict --> Encoding::Decoder) {
        my $decoder = $replacement.DEFINITE && $replacement !=== False
            ?? Encoding::Decoder::Builtin.new($!name, :$strict, :$translate-nl, :replacement(self!rep-char($replacement)))
            !! Encoding::Decoder::Builtin.new($!name, :$strict, :$translate-nl);
    }

    my int $is-win = Rakudo::Internals.IS-WIN;
    method encoder(:$replacement, :$translate-nl, :$strict --> Encoding::Encoder) {
        my $encoder = $replacement.DEFINITE && $replacement !=== False
            ?? Encoding::Encoder::Builtin.new($!name, self!buf-type(), :$strict, :replacement(self!rep-char($replacement)))
            !! Encoding::Encoder::Builtin.new($!name, self!buf-type(), :$strict);

        $translate-nl && $is-win
            ?? Encoding::Encoder::TranslateNewlineWrapper.new($encoder)
            !! $encoder
    }

#?if !js
    my constant $enc_type = nqp::hash(
#?endif
#?if js
    my $enc_type := nqp::hash(
#?endif
      'utf8',utf8,'utf16',utf16,'utf32',utf32
    );

    method !buf-type() {
        nqp::ifnull(nqp::atkey($enc_type, $!name), blob8)
    }

    method !rep-char($replacement) {
        nqp::istype($replacement, Bool)
            ?? ($!name.starts-with('utf') ?? "\x[FFFD]" !! "?")
            !! $replacement.Str
    }
}

# vim: expandtab shiftwidth=4
