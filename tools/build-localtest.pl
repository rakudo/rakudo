#! perl

=head1 NAME

build-localtest.pl - Create a t/localtest.data file from test output.

=head1 SYNOPSIS

perl tools/build-localtest.pl <test_output >t/localtest.data

=head1 DESCRIPTION

This almost doesn't deserve to be a script.  It simply reads standard
input looking for things of the form "t/spec/*/*.(t|rakudo)", then
prints those to the standard output.  Typically I simply run the
script, then copy-and-paste the summary results of a test run into
the window and capture the results to t/localtest.data .

=cut

while (<>) {
    m!t/spec/(.*?)\.(t|rakudo)! && print "$1.t\n";
}
