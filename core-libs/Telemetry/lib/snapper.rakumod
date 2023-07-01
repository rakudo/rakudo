# shorthand for loading Telemetry and starting a snapper
# WITHOOUT control-c safety: pressing control-c will stop
# the process WITHOUT presenting a report.  But this will
# not start a supervisor thread to safely handle the key
# handling, which is cleaner from a benchmarking point of
# view.

use Telemetry <snapper>;

snapper( %*ENV<RAKUDO_SNAPPER> // 0.1 );

# vim: expandtab shiftwidth=4
