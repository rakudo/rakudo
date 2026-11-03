# This unit's own code runs while whoever loads it is at BEGIN time. It is
# compiled already and resolves against its own scopes, so an indirect lookup
# here must not reach the scopes of the unit being compiled.
unit module BeginIndirectProbe;

our $mainline-saw = (try ::('&consumer-only')).defined ?? 'found' !! 'not found';

our sub export-saw() { $mainline-saw }
