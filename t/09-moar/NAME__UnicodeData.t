use Test;
use lib 't/09-moar';
use UnipropCheck;
# Please edit UnipropCheck.rakumod to change todo settings!
sub MAIN (Str $folder?, Bool:D :$debug = False) {
    my $*DEBUG = $debug;
    my $name = $*PROGRAM.basename.subst(/".t"$/, "").trans("-" => "/");
    my ($property, $filename, $answer-column) = $name.split('__');
    $filename ~= ".txt";
    my $folder-io = $folder ?? $folder.IO !! IO::Path;
    test-file $folder-io, $filename, $property, :answer-column($answer-column);
    done-testing;
}

# vim: expandtab shiftwidth=4
