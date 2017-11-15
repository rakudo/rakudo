
# Helper sub: for a line, return commit ID or Nil
sub commit($line) { $line.starts-with('commit ') ?? $line.substr(7,8) !! Nil }

# The commit that did the last VERSION bump
my $version-bump-commit = commit( qx/git log -- VERSION/.lines.head );

# Make a SetHash with all of the commits since the last VERSION bump
my %commits is SetHash = qx/git log/.lines.map( {
    if commit($_) -> \commit {
        last if commit eq $version-bump-commit;
        commit
    }
} );

# Remove the commits that have been documented in ChangeLog since bump
# Note that we assume here that an empty line indicates boundary between
# releases.
%commits{ "docs/ChangeLog".IO.lines.map( {
    $_
      ?? |.comb( / <?after \[> ( <[ 0..9 a..f A..F ]> ** 8 ) <?before \]> / )
      !! last
} ) }:delete;

# Let the world know the result
say "%commits.elems() commits not yet documented in ChangeLog:";
%commits.keys.sort.batch(9).map: *.Str.say;
