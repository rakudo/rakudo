use Test;
plan 2;

# When there is no precompilation for the Test module yet, 
# CU::PrecompilationRepository would auto-vivify $*EXECUTABLE.
# Make sure we have it always, no matter if precomp was needed.
ok ?$*EXECUTABLE, 'control for $*EXECUTABLE';

# output of "raku -e 'use Test; .say for PROCESS::.keys.sort.map: { qq:!c/  Q{$_},/ }'"
my $allowed = (
  Q{$AWAITER},
  Q{$CWD},
  Q{$EXECUTABLE},
  Q{$CORE-SETTING-REV},
  Q{$DISTRO},
  Q{$ERR},
  Q{$EXCEPTION},
  Q{$EXIT},
  Q{$IN},
  Q{$OUT},
  Q{$PID},
  Q{$RAKU},
  Q{$RAKUDO_MODULE_DEBUG},
  Q{$RAT-OVERFLOW},
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
