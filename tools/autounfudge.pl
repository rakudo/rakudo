#! perl
# Copyright (C) 2008, The Perl Foundation.
# $Id$

=head1 NAME

autounfudge - automatically write patches for unfudging spec tests

=head1 DESCRIPTION

This tool runs the non-pure tests of the C<spectest_regression> make target,
automatically creates files with less fudges, runs them again, and if the
modified tests succeeds, it adds a patch to C<autounfudge.patch> that, when
applied as C<< patch -p0 < autunfudge.patch >>, removes the superflous fudge
directives.

=head1 USAGE

Most common usage: C<perl tools/autounfudge.pl --auto>. For more use cases
pleae run this script without any options or command line parameters.

=head1 WARNINGS

This tool is very platform dependant, and not tested on anthing but linux.

It assumes that all fudge directives are orthogonal, which might not be the
case in real world tests. It is not tested with nested fudges (eg a line
based fudge inside a fudged block).

Never blindly apply the automatically generated patch.

=head1 MISCELANEA

Fudge directives containing the words I<unspecced> or I<unicode> are ignored.
The latter is because Unicode related tests can succeed on platforms with icu
installed, and fail on other platforms.

=cut

use strict;
use warnings;
use Getopt::Long;
use Fatal qw(close);
use File::Temp qw(tempfile tempdir);
use TAP::Harness;
use TAP::Parser::Aggregator;
use Cwd qw(getcwd);
use File::Spec;
use File::Path;

my $impl = 'rakudo';
our $debug = 0;
our $out_filename = 'autounfudge.patch';

if ($^O ne 'linux'){
    warn <<'WARN';
Warning: this tool is only tested on linux so far. Currently it depends on
some linux specific hacks. It requires the `diff' program to be installed.
If you test this on any platform other than linux, pleaes report your results
to parrot-porters@perl.org.
WARN
}

GetOptions  'impl=s'        => \$impl,
            'debug'         => \$debug,
            'specfile=s'    => \my $specfile,
            'auto'          => \my $auto,
            or usage();
my @files;

$specfile = 't/spectest_regression.data' if $auto;

if ($specfile){
    @files = read_specfile($specfile);
}
else {
    @files = @ARGV or usage();
}

if (-e $out_filename){
    unlink $out_filename or warn "Couldn't delete old unfudge.patch";
}
our $tmp_dir = tempdir('RAKUDOXXXXXX', CLEANUP => 1);

for (@files){
    auto_unfudge_file($_);
}

sub auto_unfudge_file {
    my $file_name = shift;
    open my $f, '<:encoding(UTF-8)', $file_name
        or die "Can't open '$file_name' for reading: $!";
    print "Processing file '$file_name'\n";
    my @fudge_lines;
    while (<$f>) {
        push @fudge_lines, $. if m/^\s*#\?$impl/ &&
            !m/unspecced|unicode|utf-?8/i;
    }
    close $f;
    if (@fudge_lines){
        print "Found " . (scalar @fudge_lines) . " fudges...\n" if $debug;
    }
    else {
        print "No fudges found. Nothing to do\n" if $debug;
        return;
    }
    my $fudged = fudge($file_name);
    print "Fudged: $fudged\n" if $debug;
    if (!tests_ok($fudged)){
        print "File '$file_name' doesn't even pass in its current state\n";
        return;
    }
    my @to_unfudge;
    for my $to_unfudge (@fudge_lines){
        $fudged = fudge(unfudge_some($file_name, 0, $to_unfudge));
        if (tests_ok($fudged)){
            print "WOOOOOT: Can remove fudge instruction on line $to_unfudge\n"
                if $debug;
            push @to_unfudge, $to_unfudge,
        }
    }

    if (@to_unfudge){
        my $u = unfudge_some($file_name, 1, @to_unfudge);
        system qq{diff -u "$file_name" "$u" >> "$out_filename"};
        unlink $u;
    }

}

sub fudge {
    my $fn = shift;
    open my $p, '-|', 't/spec/fudge', '--keep-exit-code',  $impl, $fn
        or die "Can't launch fudge: $!";
    my $ret_fn = <$p>;
    chomp $ret_fn;
    1 while <$p>;
    close $p;
    return $ret_fn;
}

sub usage {
    die <<"USAGE"
Usage:
    $0 [options] file+
Valid options:
    --debug             Enable debug output
    --impl impl         Specify a different implementation
    --specfile file     Specification file to read filenames from
    --auto              use t/spectest_regression.data for --specfile
USAGE
}

sub unfudge_some {
    my ($file, $delete, @lines) = @_;
    my ($fh, $tmp_filename) = tempfile(
            'tempXXXXX',
            SUFFIX => '.t',
            DIR => $tmp_dir
    );
    open my $in, '<', $file
        or die "Can't open file '$file' for reading: $!";
    while (<$in>){
        if ($. == $lines[0]){
            print $fh "###$_" unless $delete;
            shift @lines if @lines > 1;
        }
        else {
            print $fh $_;
        }
    }
    close $fh;
    close $in;
    return $tmp_filename;
}

sub tests_ok {
    my $fn = shift;
    $fn =~ s/\s+\z//;
    my $harness = get_harness();
    my $agg = TAP::Parser::Aggregator->new();
    $agg->start();
    $harness->aggregate_tests($agg, $fn);
    $agg->stop();
#    my $agg = $harness->runtests($fn);
    return !$agg->has_errors;
}

sub get_harness {
    return TAP::Harness->new({
            verbosity   => -2,
            exec        => ['../../parrot', '-G', 'perl6.pbc'],
            merge       => 1,
    });
}

sub read_specfile {
    my $fn = shift;
    my @res;
    open (my $f, '<', $fn) or die "Can't open file '$fn' for reading: $!";
    while (<$f>){
        next if m/#/;
        next unless m/\S/;
        s/\s+\z//;
        push @res, "t/spec/$_";
    }
    return @res;
}

END {
    File::Path::rmtree($tmp_dir);
}

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
