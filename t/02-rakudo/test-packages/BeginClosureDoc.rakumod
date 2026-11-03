unit class BeginClosureDoc;

my $s = BEGIN sub ($op) { uc($op) };

my Routine %Ops = BEGIN %(
    'q' => sub ($op) { [$op] },
);

method call-sub($op)   { $s($op) }
method call-table($op) { %Ops{$op}($op) }

#| documented after the BEGIN closures
method documented() { }
