use v6-alpha;


say "1..4";

try { die };
say "ok 1";  # we're still here, so &try worked at least partially.

try { die "foo\n" };
if $! eq "foo\n" { say "ok 2" } else { say "not ok 2" }

try { "this_does_not_die" };
unless $!        { say "ok 3" } else { say "not ok 3" }

try { die "bar\n" };
if $! eq "bar\n" { say "ok 4" } else { say "not ok 4" }
