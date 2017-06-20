class Encoding::Builtin does Encoding {
    has Str $.name;
    has @.alternative-names;

    method new() {
        X::Cannot::New.new(class => self.WHAT).throw
    }

    method alternative-names() { @!alternative-names }

    method decoder(*%options --> Encoding::Decoder) {
        Encoding::Decoder::Builtin.new($!name, |%options)
    }

    my int $is-win = Rakudo::Internals.IS-WIN;
    method encoder(:$replacement, :$translate-nl --> Encoding::Encoder) {
        my $encoder = $replacement
            ?? Encoding::Encoder::Builtin::Replacement.new($!name,
                    self!buf-type(), self!rep-char($replacement))
            !! Encoding::Encoder::Builtin.new($!name, self!buf-type());
        $translate-nl && $is-win
            ?? Encoding::Encoder::TranslateNewlineWrapper.new($encoder)
            !! $encoder
    }

    my $enc_type := nqp::hash('utf8',utf8,'utf16',utf16,'utf32',utf32);
    method !buf-type() {
        nqp::ifnull(nqp::atkey($enc_type, $!name), Blob)
    }

    method !rep-char($replacement) {
        nqp::istype($replacement, Bool)
            ?? ($!name.starts-with('utf') ?? "\x[FFFD]" !! "?")
            !! $replacement.Str
    }
}

Encoding::Registry.register(Encoding::Builtin.bless(
    :name<utf8>, :alternative-names<utf-8>
));
Encoding::Registry.register(Encoding::Builtin.bless(
    :name<utf8-c8>, :alternative-names<utf-8-c8>
));
Encoding::Registry.register(Encoding::Builtin.bless(
    :name<ascii>
));
Encoding::Registry.register(Encoding::Builtin.bless(
    :name<iso-8859-1>, :alternative-names<
        iso_8859-1:1987 iso_8859-1 iso-ir-100 latin1 latin-1 csisolatin1
        l1 ibm819 cp819
    >
));
Encoding::Registry.register(Encoding::Builtin.bless(
    :name<windows-1252>
));
Encoding::Registry.register(Encoding::Builtin.bless(
    :name<utf16>, :alternative-names<utf-16>
));
Encoding::Registry.register(Encoding::Builtin.bless(
    :name<utf32>, :alternative-names<utf-32>
));
