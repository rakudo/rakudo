proto sub undefine(Mu, *%) is raw is implementation-detail {
    Rakudo::Deprecations.DEPRECATED:
      'another way: assign a Nil; for Arrays/Hashes, assign Empty or ()',
      Nil, '6.e', :lang-vers, :what<undefine>;
    {*}
}
multi sub undefine(Mu    \x) is raw { x = Nil   }
multi sub undefine(Array \x) is raw { x = Empty }
multi sub undefine(Hash  \x) is raw { x = Empty }

sub infix:<<(<+)>> (|) {
    die "(<+) was removed in v6.d, please use (<=) operator instead
  or compile your code with 'use v6.c'"
}
sub infix:<≼>(|) {
    die "≼ was removed in v6.d, please use ⊆ operator instead
  or compile your code with 'use v6.c'"
}
sub infix:<<(>+)>> (|) {
    die "(>+) was removed in v6.d, please use (>=) operator instead
  or compile your code with 'use v6.c'"
}
sub infix:<≽>(|) {
    die "≽ was removed in v6.d, please use ⊇ operator instead
  or compile your code with 'use v6.c'"
}

# vim: expandtab shiftwidth=4
