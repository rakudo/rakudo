use Perl6::Compiler:from<NQP>;

my $snapshot-idx = 1;
my sub heap-snapshot(
   IO() $io = "heapsnapshot-$($*PID)-$($snapshot-idx++).mvmheap"
--> Str:D) is export {

    my $filename := $io.absolute;
    Perl6::Compiler.profiler-snapshot: :kind<heap>, :$filename;
    $filename
}
