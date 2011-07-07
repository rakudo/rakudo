class Pod__Block {
    has @.content;
}

class Pod__Block__Named is Pod__Block {
    has $.name;
}

class Pod__Block__Comment is Pod__Block { }

class Pod__Block__Code is Pod__Block {
    has @.allowed;
}

# vim: ft=perl6
