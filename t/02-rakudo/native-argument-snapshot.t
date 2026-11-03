use Test;

plan 9;

# A native variable argument must be passed as a snapshot of its value
# whenever the callee cannot write back through it. A reference that
# survives into storage reads the variable's current value on every
# later access, as in the Pair a Font::FreeType iterator builds from a
# native char-code attribute it then advances.

{
    my uint32 $c = 32;
    my $rv := (7 => $c);
    $c = $c + 1;
    is $rv.value, 32,
      'a native lexical as a pair value is snapshotted at construction';
}

{
    my class WithNativeAttr {
        has uint32 $!cc = 32;
        method go {
            my $p := (7 => $!cc);
            $!cc = $!cc + 1;
            $p.value
        }
    }
    is WithNativeAttr.new.go, 32,
      'a native attribute as a pair value is snapshotted at construction';
}

{
    my $s;
    sub keep(Mu \v) { $s := v }
    my int $c = 5;
    keep($c);
    $c = 8;
    is $s, 5,
      'a native argument stored by a sub through a raw parameter is a snapshot';
}

{
    my $s;
    sub prefix:<grab>(Mu \v) { $s := v; v }
    my int $c = 5;
    grab $c;
    $c = 8;
    is $s, 5,
      'a native argument stored by a prefix through a raw parameter is a snapshot';
}

{
    my $s;
    sub postfix:<✔>(Mu \v) { $s := v; v }
    my int $c = 5;
    $c✔;
    $c = 8;
    is $s, 5,
      'a native argument stored by a postfix through a raw parameter is a snapshot';
}

{
    sub inc(int $x is rw) { $x++ }
    my int $c = 5;
    inc($c);
    is $c, 6, 'a native argument to an rw parameter still writes back';
}

{
    sub infix:<bump>(int $x is rw, int $y) { $x = $y }
    my int $c = 5;
    $c bump 9;
    is $c, 9,
      'a native argument to an infix with an rw parameter stays writable';
}

{
    sub f(int $x is rw, :$k) { $x++ }
    my int $c = 5;
    f(k => 1, $c);
    is $c, 6,
      'a native argument after a named argument still binds rw to its parameter';
}

{
    sub f($a, int $x is rw) { $x++ }
    my @a = 1,;
    my int $c = 5;
    f(|@a, $c);
    is $c, 6,
      'a native argument after a flattened argument still binds rw to its parameter';
}

# vim: expandtab shiftwidth=4
