multi infix:<+>(Date:D $d, Int:D $x) {
    Date.new-from-daycount($d.daycount + $x)
}
multi infix:<+>(Int:D $x, Date:D $d) {
    Date.new-from-daycount($d.daycount + $x)
}
multi infix:<->(Date:D $d, Int:D $x) {
    Date.new-from-daycount($d.daycount - $x)
}
multi infix:<->(Date:D $a, Date:D $b) {
    $a.daycount - $b.daycount;
}
multi infix:<cmp>(Date:D $a, Date:D $b) {
    $a.daycount cmp $b.daycount
}
multi infix:«<=>»(Date:D $a, Date:D $b) {
    $a.daycount <=> $b.daycount
}
multi infix:<==>(Date:D $a, Date:D $b) {
    $a.daycount == $b.daycount
}
multi infix:<!=>(Date:D $a, Date:D $b) {
    $a.daycount != $b.daycount
}
multi infix:«<=»(Date:D $a, Date:D $b) {
    $a.daycount <= $b.daycount
}
multi infix:«<»(Date:D $a, Date:D $b) {
    $a.daycount < $b.daycount
}
multi infix:«>=»(Date:D $a, Date:D $b) {
    $a.daycount >= $b.daycount
}
multi infix:«>»(Date:D $a, Date:D $b) {
    $a.daycount > $b.daycount
}

