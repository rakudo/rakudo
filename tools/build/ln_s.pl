#!/usr/bin/env perl
# Simulate 'ln -nfs' for AIX where -n means normal mode of operation, not what
# -n/-h means with GNU or BSD versions on ln.

shift @ARGV if $ARGV[0] eq '--';

unless (@ARGV == 2) {
    print STDERR <<ERR;
Usage: $0 <target> <link_name>
ERR
    exit 1;
}

my ($target, $link_name) = @ARGV;

if (-l $link_name || -f $link_name) {
    unlink $link_name or die "Can't unlink '$link_name': $!";
}

symlink($target, $link_name) 
    or die "Can't symlink '$target' to '$link_name': $!";
