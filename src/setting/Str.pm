class Str is also {
    our Bool multi method ACCEPTS ($topic) {
        self eq $topic;

    }
    our Str multi method reverse ($str: ) {
        return $str.split('').reverse.join;
    }
}

# vim: ft=perl6
