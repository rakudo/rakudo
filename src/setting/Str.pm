class Str is also {
    our Str multi method reverse ($str: ) is export {
        return $str.split('').reverse.join('');
    }
}

# vim: ft=perl6
