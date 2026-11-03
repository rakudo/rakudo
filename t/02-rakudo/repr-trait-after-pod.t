use Test;

plan 6;

# A pod block before a class must not be taken for the `is repr(...)` value,
# and must still reach $=pod even though the trait circumfix is consumed.

my $with-pod = EVAL q:to/CODE/;
    =begin pod
    Some documentation.
    =end pod
    class WithPod is repr('CStruct') { has int64 $.frames; }
    WithPod
CODE
is $with-pod.REPR, 'CStruct',
    'is repr(...) after a pod block sets the representation';

my $multi-pod = EVAL q:to/CODE/;
    =begin pod
    First.
    =end pod
    =begin pod
    Second.
    =end pod
    class MultiPod is repr('CStruct') { has int32 $.rate; }
    MultiPod
CODE
is $multi-pod.REPR, 'CStruct',
    'is repr(...) after several pod blocks still sets the representation';

my $dq = EVAL q:to/CODE/;
    =begin pod
    Doc.
    =end pod
    class Dq is repr("CStruct") { has int64 $.f; }
    Dq
CODE
is $dq.REPR, 'CStruct',
    'is repr(...) with a double-quoted value works after a pod block';

my $no-pod = EVAL 'class NoPod is repr(\'CStruct\') { has int64 $.f; }; NoPod';
is $no-pod.REPR, 'CStruct',
    'is repr(...) without a pod block is unchanged';

my $pod-count = EVAL q:to/CODE/;
    =begin pod
    Kept.
    =end pod
    class Kept is repr('CStruct') { has int64 $.f; }
    $=pod.elems
CODE
is $pod-count, 1, 'the pod block before an is repr(...) class still reaches $=pod';

my $pod-text = EVAL q:to/CODE/;
    =begin pod
    The text.
    =end pod
    class Texted is repr('CStruct') { has int64 $.f; }
    ~$=pod[0].contents[0].contents[0]
CODE
is $pod-text, 'The text.', 'the preserved pod keeps its content';

# vim: expandtab shiftwidth=4
