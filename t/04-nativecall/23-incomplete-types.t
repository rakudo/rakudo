use v6;

use MONKEY-SEE-NO-EVAL;

use lib <lib>;
use NativeCall :TEST;
use Test;

plan 6;

for (<CUnion CStruct CPPStruct>) -> $cls {
    # test the stub case
    my $b = (
        "class Stub-$cls is repr('CStruct') " ~ ｢{ ... }｣,
        "class Oops$cls" ~ ｢ is repr('｣ ~ $cls ~ ｢') { HAS Stub-｣ ~ $cls ~ ｢ $.stubby; }｣;
    ).join("\n");
    throws-like { EVAL $b }, Exception, :message(/inline.*before.*definition/);

    # self-inlining should fail the same way
    my $bad = ("class Oops2$cls is repr(", $cls, ') { HAS Oops2'~$cls~' $.more; }').join(｢'｣);
    throws-like { EVAL $bad }, Exception, :message(/inline.*before.*definition/);
}

# vim: expandtab shiftwidth=4
