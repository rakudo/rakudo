use Test;
plan 1;

my $failed = 0;

sub match ($str, $flip) {
    return False if $flip eq "no";
    return $str ~~ rx/foo/;
}

for 0..104 {
    for "no", "yes" -> $flip {
        if my $match = match("something", $flip) {
            $failed++ unless $match;
        } 
    }
} 

is $failed, 0, '$match should not be true and false (RT#127869)';

