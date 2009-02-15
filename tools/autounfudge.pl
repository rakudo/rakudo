#! perl
# Copyright (C) 2008, The Perl Foundation.
# $Id$

=head1 NAME

autounfudge - automatically write patches for unfudging spec tests

=head1 DESCRIPTION

This tool runs the non-pure tests of the C<spectest> make target,
automatically creates files with less 'skip' fudge directives, runs them 
again, and if the
modified tests succeeds, it adds a patch to C<autounfudge.patch> that, when
applied as C<< patch -p0 < autunfudge.patch >>, removes the superflous fudge
directives.

With the C<--untodo> option, C<todo> skip markers are also removed (where
appropriate), with the C<--unskip> option it tries to substitute C<skip>
markers by C<todo> markers.

=head1 USAGE

Most common usage: C<perl tools/autounfudge.pl --auto>. For more options
pleae run this script without any options or command line parameters.

=head1 WARNINGS

This tool assumes that all fudge directives are orthogonal,
which might not be the case in real world tests. So always make sure to
run C<make spectest> before commiting the changes.

Never blindly apply the automatically generated patch.

=head1 MISCELLANEA

Fudge directives containing the words I<unspecced>, I<noauto> or I<unicode>  
are ignored.
The latter is because Unicode related tests can succeed on platforms with icu
installed, and fail on other platforms.

By default some files are skipped (which can be overridden with the
C<--exclude> option) because certain tests loop (at the time of writing
C<t/spec/S04-statement-modifiers/while.t>), others because processing them
simply takes too long; C<t/spec/S05-mass/rx.t> contains more than 250
fudge lines and thus would take about three hours to autoumatically unfudge.

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
use Text::Diff;
use threads;
use threads::shared;
use Thread::Queue;

my $impl = 'rakudo';
our $debug = 0;
our $out_filename = 'autounfudge.patch';
my $exclude = '(?:(?:radix|modifiers/(?:while|until)|rx|assign)\.t)$';
our $threads_num = 1;

GetOptions  'impl=s'        => \$impl,
            'debug'         => \$debug,
            'specfile=s'    => \my $specfile,
            'auto'          => \my $auto,
            'keep-env'      => \my $keep_env,
            'unskip'        => \my $unskip,
            'untodo'        => \my $untodo,
            'section=s'     => \my $section,
            'exclude'       => \$exclude,
            'jobs=i'        => \$threads_num,
            or usage();

delete $ENV{PERL6LIB} unless $keep_env;

my @files;

$specfile = 't/spectest.data' if $auto;

if ($specfile){
    @files = read_specfile($specfile);
}
else {
    @files = @ARGV or usage();
}

if ($section) {
    my $s = ($section =~ m/^\d{1,2}$/)
            ? sprintf('S%02d', $section)
            : $section;
    print "Only of section `$section'\n";
    @files = grep { m{ spec [/\\] \Q$section\E  }x } @files;
}

our $diff_lock :shared = 0;
open our $diff_fh, '>', $out_filename
    or die "Can't open '$out_filename' for writing: $!";
{
    select $diff_fh;
    $| = 1;
    select STDOUT;
}

our $tmp_dir = tempdir('RAKUDOXXXXXX', CLEANUP => 1);

if ($threads_num > 1) {
    my $queue = Thread::Queue->new;
    for (1..$threads_num) {
        threads->create(sub {
                while(my $file_name = $queue->dequeue) {
                    auto_unfudge_file($file_name);
                }
            });
    }

    $queue->enqueue($_) for @files;
    $queue->enqueue(undef) for 1..$threads_num;
    $_->join for threads->list;
}
else {
    for (@files) {
        auto_unfudge_file($_);
    }
}


sub auto_unfudge_file {
    my $file_name = shift;

    return unless defined $file_name;
    open my $f, '<:encoding(UTF-8)', $file_name
        or die "Can't open '$file_name' for reading: $!";
    print "Processing file '$file_name'\n";
    my @fudge_lines;
    while (<$f>) {
        push @fudge_lines, [$. , $_] if m/^\s*#\?$impl/ &&
            !m/unspecced|unicode|utf-?8|noauto/i;
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
        print "trying line $to_unfudge->[0]...\n" if $debug;
        next if $to_unfudge->[1] =~ m/\btodo\b/ && !$untodo;
        $fudged = fudge(unfudge_some($file_name, [$to_unfudge->[0], '']));
        if (tests_ok($fudged)){
            print "WOOOOOT: Can remove fudge instruction on line $to_unfudge->[0]\n"
                if $debug;
            push @to_unfudge, [$to_unfudge->[0], ''],
        } 
        elsif ($unskip && $to_unfudge->[1] =~ s/\bskip\b/todo/) {
            # try to replace 'skip' with 'todo'-markers
            $fudged = fudge(unfudge_some($file_name, $to_unfudge));
            if (tests_ok($fudged)){
                print "s/skip/todo/ successful\n" if $debug;
                push @to_unfudge, $to_unfudge;
            }
        }
        else {
            print "not successful\n"if $debug;
        }
    }

    if (@to_unfudge){
        my $u = unfudge_some($file_name, @to_unfudge);
        lock($diff_lock);
        print $diff_fh diff($file_name, $u);
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
    --auto              use t/spectest.data for --specfile
    --keep-env          Keep PERL6LIB environment variable.
    --exclude regex     Don't run the tests that match regex
    --section number    Run only on tests belonging to section <number>
    --unskip            Try to change 'skip' to 'todo' markers
    --untodo            Try to remove 'todo' markers
USAGE
}

sub unfudge_some {
    my ($file, @lines) = @_;

    my ($fh, $tmp_filename) = tempfile(
            'tempXXXXX',
            SUFFIX => '.t',
            DIR => $tmp_dir
    );
    open my $in, '<', $file
        or die "Can't open file '$file' for reading: $!";
    while (<$in>){
        if ($. == $lines[0][0]){
            print $fh $lines[0][1];
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
    return !$agg->has_errors;
}

sub get_harness {
    my $parrot = -d 'parrot' ? 'parrot/parrot' : '../../parrot';
    return TAP::Harness->new({
            verbosity   => -2,
            exec        => [$parrot, '-G', 'perl6.pbc'],
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
        next if m/$exclude/;
        push @res, "t/spec/$_";
    }
    return @res;
}

END {
    close $diff_fh if $diff_fh;
    File::Path::rmtree($tmp_dir);
}

# Local Variables:
#   mode: cperl
#   cperl-indent-level: 4
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
