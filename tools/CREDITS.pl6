#!/usr/bin/env perl6
use v6;

# Quick hack to update CREDITS with "git log" type output not perfect since
# doesn't handle duplicates with accents present/missing etc.
# Manual duplication pass recommended

my $io = '../CREDITS'.IO.open;
# XXX assume records start with N:
$io.nl-in="\nN: ";

# old data
my ($header, %h, %email);
for $io.lines -> $l {
    if $l !~~ /^'='/ {
        $l ~~ /(.*?)\n(.*)/;
        my $k = $0;
        my $v = $1;
        $v.=subst("\n=cut\n",'') if $v ~~ /'=cut'/ ; # cheat :/
        %h{$k} = $v;
        # pull out email into another hash as another check
        %h{$k} ~~ /'E: '(.*?)\n/;
        %email{$0}=1 if $0;
    } else {
        $header = $l;
    }
}

# new data
for  qx!git log --pretty='format:%an|%ae'|sort -u!.split("\n") -> $commit {

    my ($name, $email) = $commit.split('|');
    next unless $name and $email;
    next if $name eq 'chromatic'; # user requested this

    # TODO check for existing email here
    %h{$name} = "E: {$email}\n" if all( $name ne "unknown", $email !~~ /'(none)'/, $email !~~ /\.local/, !%h{$name}.defined, !%email{$email}.defined );

}

say $header;

for %h.keys.sort: *.uc -> $k {
    say "N: {$k}\n{%h{$k}}";
}

say "=cut";

# vim: ft=perl6 expandtab sw=4
