use Test;
plan 1;

# output of "raku -e 'use Test; .say for PROCESS::.keys.sort.map: { qq:!c/  Q{$_},/ }'"
my $allowed = (
  Q{$AWAITER},
  Q{$CWD},
  Q{$CORE-SETTING-REV},
  Q{$ERR},
  Q{$IN},
  Q{$OUT},
  Q{$PID},
  Q{$RAKU},
  Q{$RAKUDO_MODULE_DEBUG},
  Q{$REPO},
  Q{$SCHEDULER},
  Q{$SPEC},
  Q{%ENV},
  Q{&chdir},
);

my $unknown = PROCESS::.keys (-) $allowed;
diag "Found {+$unknown} unexpected entries: $unknown" unless
ok $unknown == 0, "No unexpected entries in PROCESS::";

# vim: expandtab shiftwidth=4
