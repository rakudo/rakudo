class Hash is also {
    our Hash multi method reverse ( %hash: ) is export {
        my %result;
        (%result).{%hash.values} = %hash.keys;
        %result;
    }
}

# vim: ft=perl6
