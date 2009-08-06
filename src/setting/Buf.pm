class Buf does Positional {
    has @!values;

    method new(*@values) {
        self.bless(*, :values(@values));
    }

    method decode($encoding = 'UTF-8') {
        return "";
    }

    method list() {
        return @!values;
    }
}

# vim: ft=perl6
