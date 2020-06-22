use v6;
use Test;

# GH#1202

plan 2;

my Bool $complete = False;

await Promise.anyof(
    start {
        my $out = "";
        # This was a bug with unpredictable output. Hopefully 50 repetiotions is enough to make sure everything works.
        for ^50 -> $rep {
            my $proc = run $*EXECUTABLE.absolute, '-e', q<await (^5).map({start { print qqx{echo $_} } })>, :out;
            $out ~= $proc.out.slurp(:close);
        }
        like $out, /[ <[0..4]> \n ] ** 250/, "shell output";
        $complete = True;
    },
    Promise.in(600), # 10mins timeout
);

ok $complete, "all runs completed";

# vim: expandtab shiftwidth=4
