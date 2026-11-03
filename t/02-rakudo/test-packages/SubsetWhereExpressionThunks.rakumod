unit module SubsetWhereExpressionThunks;

subset Verb of Str where any(<GET POST>);
our sub where-param($x where Int|Str) { $x }
our sub default-param($x, $y = $x + 1) { $y }
