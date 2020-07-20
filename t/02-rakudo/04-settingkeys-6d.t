use v6.d;
use Test;
plan 2;

# output of "perl6 -e '.say for SETTING::.keys.sort.map: { qq:!c/  Q{$_},/ }'"
my %allowed = (
  Q{Int},
  Q{CORE-SETTING-REV},
  Q{$!},
  Q{$/},
  Q{$=pod},
  Q{$_},
  Q{$¢},
  Q{&REACT},
  Q{&REACT-ONE-WHENEVER},
  Q{&await},
  Q{&infix:<≼>},
  Q{&infix:<≽>},
  Q{&infix:«(<+)»},
  Q{&infix:«(>+)»},
  Q{&undefine},
).map: { $_ => 1 };

my @unknown;
my $known-count;
my @missing;
for %allowed.keys {
    if SETTING::{$_}:exists  {
        $known-count++
    }
    else {
        @missing.push: $_;
    }
}
is %allowed.elems, $known-count, "all allowed symbols found";
diag "Missing symbols: { @missing.sort }" if @missing;
@unknown.push($_) unless %allowed{$_}:exists for SETTING::.keys;
diag "Found {+@unknown} unexpected entries: { @unknown.sort }" if @unknown;
ok @unknown == 0, "No unexpected entries in SETTING::";

# vim: expandtab shiftwidth=4
