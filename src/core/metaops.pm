
sub METAOP_ASSIGN(\$op) {
    -> Mu \$a, Mu \$b { $a = $op($a, $b) }
}

sub METAOP_REVERSE(\$op) {
    -> Mu \$a, Mu \$b { $op($b, $a) }
}


