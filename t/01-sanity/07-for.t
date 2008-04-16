use v6;


say "1..9";

my @array = <a b c>;

my $i = 0;
for @array -> $item {
    if @array[$i] eq $item { say "ok ", $i + 1 } else { say "not ok ", $i + 1 }
    $i++;
}

$i = 0;
for @array {
    if @array[$i] eq $_ { say "ok ", $i + 4 } else { say "not ok ", $i + 4 }
    $i++;
}

$i = 0;
for (0,1,2) {
    if $i eq $_ { say "ok ", $i + 7 } else { say "not ok ", $i + 7 }
    $i++;
}
