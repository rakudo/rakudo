# Monitor a process execution. Usage:
# perl -I<path-to-nqp-config> -I<path-to-Rakudo-tools-lib> monitor-process.pl <options> <command-line>
# See options below.
use v5.10.1;
use strict;
use warnings;
use NQP::Config qw<run_or_die>;
use Getopt::Long;

Getopt::Long::Configure(qw<no_permute>);
my %params = ( output_timeout => 180, timeout => 900,
               verbose => 0, heartbeat => 30, );
Getopt::Long::GetOptions( \%params, 'output_timeout|output-timeout=i', 'timeout=i', 'verbose!',
                          'description|descr|what=s', 'hearbeat=i', 'bypass!' );

if ($params{bypass}) {
    exit system(@ARGV);
}
run_or_die [@ARGV], %params;
