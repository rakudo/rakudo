
sub METAOP_ASSIGN(\$op, \$var is rw, \$value) {
    $var = $op($var, $value);
}
