
# Helper sub: for a line, return commit ID or Nil
sub commit($line) { $line.starts-with('commit ') ?? $line.substr(7,8) !! Nil }

# The commit that did the last VERSION bump
my $version-bump-commit = commit( qx/git log -- VERSION/.lines.head );

# Make a Set with all of the commits since the last VERSION bump minus
# all the commits that have been documented in ChangeLog since the last
# bump.  Note that we assume that an empty line indicates the boundary
# between releases.
my $missing = qx/git log/.lines.map( {
    if commit($_) -> \commit {
        last if commit eq $version-bump-commit;
        commit
    }
} ) (-) "docs/ChangeLog".IO.lines.map: {
    $_
      ?? |.comb( / <?after \[> ( <[ 0..9 a..f A..F ]> ** 8 ) <?before \]> / )
      !! last
}

# Let the world know the result
say "$missing.elems() commits not yet documented in ChangeLog:";
$missing.keys.sort.batch(9).map: *.Str.say;
