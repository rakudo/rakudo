use lib <t/packages/>;
use Test;
use Test::Helpers;

plan 12;

my $tempdir = make-temp-dir;

my $htmlpath = $tempdir.child("profile.html").absolute;

is-run "", "can launch with profiling requested", compiler-args => ["--profile=$htmlpath"],
    err => / "profiler output" .*? "$htmlpath" /;

ok $htmlpath.IO.f, "profiler html output file exists on disk";
ok (try $htmlpath.IO.s > 128), "profiler html output file isn't tiny";

my $jsonpath = $tempdir.child("profile.json").absolute;

is-run "", "can launch with profiling requested", compiler-args => ["--profile=$jsonpath"],
    err => / "profiler output" .*? "$jsonpath" /;

ok $jsonpath.IO.f, "profiler json output file exists on disk";
ok (try $jsonpath.IO.s) > 128, "profiler json output file isn't tiny";

my $sqlpath = $tempdir.child("profile.sql").absolute;

is-run "", "can launch with profiling requested", compiler-args => ["--profile=$sqlpath"],
    err => / "profiler output" .*? "$sqlpath" /;

ok $sqlpath.IO.f, "profiler sql output file exists on disk";
ok (try $sqlpath.IO.s) > 128, "profiler sql output file isn't tiny";

my $heappath = $tempdir.child("profile.mvmheap").absolute;

is-run "", "can launch with profiling requested", compiler-args => ["--profile=$heappath"],
    err => / "snapshot" .*? "$heappath" /;

ok $heappath.IO.f, "profiler heap output file exists on disk";
ok (try $heappath.IO.s) > 128, "profiler heap output file isn't tiny";
