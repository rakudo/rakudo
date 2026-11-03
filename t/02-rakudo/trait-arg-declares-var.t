use Test;

plan 5;

# A trait argument on a routine can declare a variable, as in
# `is memoized(my %h)`. The declaration goes into the routine's scope, and
# the trait receives the declaration's container, so a value the trait
# leaves in it is what runtime frames start from.

my %trait-saw;
multi sub trait_mod:<is>(Routine:D \r, :$stash!) {
    %trait-saw{r.name} = $stash;
    $stash<from-trait> = 1 if $stash ~~ Associative;
}

{
    sub f() is stash(my %h) { %h<from-trait> }
    is f(), 1, 'the body starts from the container the trait wrote to';
    is %trait-saw<f><from-trait>, 1, 'the trait received a usable Hash';
}

{
    sub g() is stash(my $x) { }
    ok %trait-saw<g>:exists, 'a scalar declaration in a trait argument works';
}

{
    sub h() is stash(my %h) { %h<n>++; %h<n> }
    is h(), 1, 'first call sees the count it just wrote';
    is h(), 2, 'a container the trait wrote to is shared across calls';
}
