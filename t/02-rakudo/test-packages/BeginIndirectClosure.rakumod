# The closure below is made by running this unit's own BEGIN block, so the
# frames it captures were compiled for this compilation. Called during some
# other unit's begin-time effect, its indirect lookup must still resolve
# against this unit's scopes only.
unit module BeginIndirectClosure;

our constant &begin-closure = BEGIN {
    my $made = 'made-at-begin';
    sub () {
        $made eq 'made-at-begin'
          ?? ((try ::('&loader-secret')).defined ?? 'found' !! 'not found')
          !! 'capture broken'
    }
};
