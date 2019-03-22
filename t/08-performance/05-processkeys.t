use Test;
plan 1;

# output of "perl6 -e 'use Test; .say for PROCESS::.keys.sort.map: { qq:!c/  Q{$_},/ }'"
my %allowed = (
  Q{$AWAITER},
  Q{$CWD},
  Q{$ERR},
  Q{$IN},
  Q{$OUT},
  Q{$PERL},
  Q{$PID},
  Q{$RAKUDO_MODULE_DEBUG},
  Q{$REPO},
  Q{$SCHEDULER},
  Q{$SPEC},
  Q{%ENV},
  Q{&chdir},
).map: { $_ => 1 };

my @unknown;
@unknown.push($_) unless %allowed{$_}:exists for PROCESS::.keys;
diag "Found {+@unknown} unexpected entries: { @unknown.sort }" unless
ok @unknown == 0, "No unexpected entries in PROCESS::";
