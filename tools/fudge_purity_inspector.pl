#! perl
# Copyright (C) 2008, The Perl Foundation.
# $Id$

use strict;
use warnings;


my $input_file = shift @ARGV || 't/spectest_regression.data';
my $impl = 'rakudo';

open my $fh, '<', $input_file
    or die "Can't open '$input_file' for reading: $!";

my @files;

while (<$fh>){
    chomp;
    next if  m/^#/ || m/^\s*$/;
    my ($file, $comment) = split m/\s*#\s*/;
    if ($comment && $comment eq 'pure'){
        push @files => check_file_and_warn($file);
    }
}

if (@files) {
    print "The following files contain fudge directives though they are marked as pure:\n";
    print "$_\n"
        for @files;
}
else { print "all's well\n"; }


sub check_file_and_warn {
    my $filename = shift;
    $filename = "t/spec/$filename";
#    warn "checking file <$filename>\n";
    open my $fh, '<', $filename
        or die "Can't open file '$filename' for reading: $!";
    my $re = qr{^\s*#\?$impl};
    my @lines;
    while (<$fh>) {
        push @lines => $.
            if m/$re/;
    }
    close $fh;

    return @lines ? ("$filename, lines " . join( ', ' => @lines )) : ();
}

