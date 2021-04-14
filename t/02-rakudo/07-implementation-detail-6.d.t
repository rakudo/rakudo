use v6.d;
use Test;

plan 4;

sub non-implementation(\base, \matcher) {
    base.pairs
      .grep( { .key ~~ matcher && !.value.is-implementation-detail } )
      .sort( *.key )
      .map( *.value.name )
}

my @UPPER = <EVAL EVALFILE HOW RUN-MAIN VAR WHAT>;

my @lower = ("",<<
  abs acos acosec acosech acosh acotan acotanh all any append
  asec asech asin asinh atan atan2 atanh atomic-add-fetch atomic-assign
  atomic-dec-fetch atomic-fetch atomic-fetch-add atomic-fetch-dec
  atomic-fetch-inc atomic-fetch-sub atomic-inc-fetch atomic-sub-fetch
  await bag cache callframe callsame callwith cas categorize ceiling
  chars chdir chmod chomp chop chr chrs cis classify close comb
  combinations copy cos cosec cosech cosh cotan cotanh deepmap defined
  die dir done duckmap elems emit end exit exp expmod fail fc first
  flat flip floor full-barrier get getc gist goto grep hash index
  indices indir is-prime item join keys kv last lastcall lc leave
  lines link list log log10 log2 lsb make map max min minmax mix
  mkdir move msb next nextcallee nextsame nextwith nodemap none
  not note one open ord ords pair pairs parse-base parse-names
  permutations pick pop prepend print printf proceed produce
  prompt push put rand redo reduce rename repeated repl return
  return-rw reverse rindex rmdir roll roots rotate round roundrobin
  run samecase samemark samewith say sec sech set shell shift sign
  signal sin sinh sleep sleep-timer sleep-until slip slurp so sort
  splice split sprintf spurt sqrt squish srand subbuf-rw substr
  substr-rw succeed sum symlink take take-rw tan tanh tc tclc trim
  trim-leading trim-trailing truncate uc undefine unimatch uniname
  uninames uniparse uniprop uniprop-bool uniprop-int uniprop-str
  uniprops unique unival univals unlink unpolar unshift val values
  warn wordcase words
>>).flat;

my @lower-not-implemented = ();

if $*VM.name eq 'jvm' {
  @lower-not-implemented = <
    atomic-fetch-inc atomic-dec-fetch atomic-fetch-sub atomic-fetch-add
    atomic-add-fetch full-barrier atomic-fetch-dec atomic-inc-fetch
    atomic-sub-fetch
  >;
}

is-deeply
  non-implementation(CORE::, /^ "&" <[A..Z]> /) (^) @UPPER,
  set(),
  "were any global uppercase CORE:: subs added";

is-deeply
  non-implementation(SETTING::, /^ "&" <[A..Z]> /) (^) (),
  set(),
  "were any global uppercase SETTING:: subs added";

is-deeply
  non-implementation(CORE::, /^ "&" <[a..z]> /).grep({ !/ ':' / }) (^) @lower,
  set(@lower-not-implemented),
  "were any global lowercase CORE:: subs added";

is-deeply
  non-implementation(SETTING::, /^ "&" <[a..z]> /).grep({ !/':'/ })
  (^) < await >,
  set(),
  "were any global lowercase SETTING:: subs added";

# vim: expandtab shiftwidth=4
