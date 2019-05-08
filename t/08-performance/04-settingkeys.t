use Test;
plan 1;

# output of "perl6 -e '.say for SETTING::.keys.sort.map: { qq:!c/  Q{$_},/ }'"
my %allowed = (
  Q{!UNIT_MARKER},
  Q{$!},
  Q{$/},
  Q{$=finish},
  Q{$=pod},
  Q{$?PACKAGE},
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
  Q{::?PACKAGE},
  Q{EXPORT},
  Q{GLOBALish},
).map: { $_ => 1 };

my @unknown;
@unknown.push($_) unless %allowed{$_}:exists for SETTING::.keys;
diag "Found {+@unknown} unexpected entries: { @unknown.sort }" unless
ok @unknown == 0, "No unexpected entries in SETTING::";
