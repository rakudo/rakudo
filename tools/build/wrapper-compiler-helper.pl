use 5.10.1;
use strict;
use warnings;

my $command = shift @ARGV;

if ($command eq 'write-exec-size') {
    my $h_file = shift @ARGV;
    my $len = 0;
    if (+@ARGV) {
        my $exe_file = shift @ARGV;
        $len = -s $exe_file;
    }
    open my $fh, ">", $h_file or die "open: $!";
    print $fh "const unsigned long EXEC_LEN = $len;\n" or die "print: $!";
    close $fh or die "close: $!";
}
elsif ($command eq 'validate-sizes') {
    my ($path1, $path2) = @ARGV;
    if (-s $path1 != -s $path2) {
        die "Binary sizes of the Windows runner template stages differ ({$path1.IO.s} vs {$path2.IO.s}). This will break compilation. Aborting.";
    }
}