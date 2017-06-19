class Encoding::Builtin does Encoding {
    has Str $.name;
    has @.alternative-names;

    method new() {
        die "Do not create instnaces of this class directly; instead use Encoding::Registry"
    }

    method alternative-names() { @!alternative-names }

    method decoder(*%options --> Encoding::Decoder) {
        Encoding::Decoder::Builtin.new($!name, |%options)
    }

    method encoder(--> Encoding::Encoder) {
        die "NYI"
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
