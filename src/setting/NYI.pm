multi caller(){
    die 'caller() is not yet implemented in Rakudo, sorry';
}
multi context(){
    die 'context() is not yet implemented in Rakudo, sorry';
}
multi runinstead(){
    die 'runinstead() is not yet implemented in Rakudo, sorry';
}

multi cat($) {
    die 'cat() and streams are not yet implemented in Rakudo, sorry';
}

multi infix:«==>» (*@a) {
    die 'Feed operators are not yet implemented in Rakudo, sorry';
}
multi infix:«<==» (*@a) {
    die 'Feed operators are not yet implemented in Rakudo, sorry';
}
multi infix:«==>>» (*@a) {
    die 'Feed operators are not yet implemented in Rakudo, sorry';
}
multi infix:«<<==» (*@a) {
    die 'Feed operators are not yet implemented in Rakudo, sorry';
}

multi infix:<mod>($a, $b) {
    die 'infix:<mod> not yet implemented in Rakudo, sorry';
}

# vim: ft=perl6
