#!/usr/bin/env perl5
use File::Find;
use File::Path q<remove_tree>;

my @d;
for my $dir (@ARGV) {
    next unless -d $dir;
    find(
        sub {
            push @d, $File::Find::name if $_ eq ".precomp" and -d;
        },
        $dir
    );
}
remove_tree(@d);
exit;

# vim: expandtab sw=4
