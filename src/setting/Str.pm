class Str is also {
    our Str multi method reverse ($str: ) {
        return $str.split('').reverse.join;
    }
}

# vim: ft=perl6
