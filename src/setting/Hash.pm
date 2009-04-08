class Hash is also {
    our Hash multi method reverse ( %hash: ) is export {
        my %result;
        (%result).{%hash.values} = %hash.keys;
        %result;
    }
}

multi reverse(%hash) {
    %hash.reverse;
}

# vim: ft=perl6
