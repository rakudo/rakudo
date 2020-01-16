# shorthand for loading Telemetry and starting a snapper

use Telemetry <safe-ctrl-c snapper>;

safe-ctrl-c;
snapper( %*ENV<RAKUDO_SNAPPER> // 0.1 );

# vim: ft=perl6 expandtab sw=4
