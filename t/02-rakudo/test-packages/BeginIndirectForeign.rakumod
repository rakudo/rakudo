# Code in this unit is foreign to whichever unit runs it at BEGIN time. An
# indirect lookup here resolves against this unit's own scopes, never against
# the unit whose begin-time effect invoked it.
unit module BeginIndirectForeign;

sub foreign-probe() is export {
    (try ::('&loader-secret')).defined ?? 'found' !! 'not found'
}

sub foreign-lexical-probe() is export {
    LEXICAL::.EXISTS-KEY('&loader-secret') ?? 'found' !! 'not found'
}

sub foreign-dynamic-probe() is export {
    (try DYNAMIC::<&loader-secret>).defined ?? 'found' !! 'not found'
}

sub foreign-outers-probe() is export {
    OUTERS::.EXISTS-KEY('&loader-secret') ?? 'found' !! 'not found'
}

our $trait-saw = 'never ran';
multi sub trait_mod:<is>(Routine $r, :$begin-indirect-probing!) is export {
    $trait-saw = (try ::('&loader-secret')).defined ?? 'found' !! 'not found';
}
