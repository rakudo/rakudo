# shorthand for loading Telemetry and starting a snapper
# with control-c safety, allowing one to stop the script
# with control-c and still get a report.

use Telemetry <safe-ctrl-c snapper>;

safe-ctrl-c;
snapper( %*ENV<RAKUDO_SNAPPER> // 0.1 );

# vim: expandtab shiftwidth=4
