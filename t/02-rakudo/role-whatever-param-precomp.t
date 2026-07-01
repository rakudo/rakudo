use lib <t/packages/Test-Helpers>;
use Test;
use Test::Helpers;

plan 4;

# A role parameterized by a WhateverCode, punned into a class, must survive
# precompilation. The WhateverCode used to be built by running a throwaway
# BEGIN-time thunk, so the serialized closure carried an outer frame that no
# longer matched its static frame on load:
#   provided outer frame ... does not match expected static frame '<unit>'
# Compiling the argument as a static block keeps the closure serializable.

sub precomp-and-run($name, $source, $call, $expected, $desc) {
    my $tmp       = make-temp-dir;
    my $mod-store = $tmp.add('module-store');
    $mod-store.mkdir;
    $mod-store.add("$name.rakumod").spurt: $source;

    my $proc = run :out, :err,
        $*EXECUTABLE.absolute,
        '-I', $mod-store.absolute,
        '-e', "use $name; print $call";

    my $out = $proc.out.slurp(:close);
    my $err = $proc.err.slurp(:close);

    subtest $desc => {
        plan 3;
        is  $proc.exitcode, 0, 'exits cleanly';
        nok $err.contains('does not match expected static frame'),
            'no outer frame mismatch';
        is  $out, $expected, 'produces the expected result';
    }
}

my $role = q:to/EOF/;
proto sub infix:<precedes>($, $) is export {*}
role Heap[&infix:<precedes> = * cmp * == Less] is export {
    method top($a, $b) { $a precedes $b }
}
class MaxHeap does Heap[* cmp * == More] is export { }
class MinHeap does Heap[* cmp * == Less] is export { }
EOF

precomp-and-run 'HeapMax', $role, 'MaxHeap.new.top(1, 2)', 'False',
    'WhateverCode role argument (max)';
precomp-and-run 'HeapMin', $role, 'MinHeap.new.top(1, 2)', 'True',
    'WhateverCode role argument (min)';
precomp-and-run 'HeapDefault', $role, 'Heap.new.top(1, 2)', 'True',
    'the role default WhateverCode still works';

# A parameterization mixing a compile-time argument with a WhateverCode must
# also survive precompilation.
my $mixed = q:to/EOF/;
role R[::T, &op] is export { method go($x) { $x.&op } }
class C does R[Int, * + 1] is export { }
EOF
precomp-and-run 'MixedArgs', $mixed, 'C.new.go(41)', '42',
    'a compile-time argument alongside a WhateverCode';

# vim: expandtab shiftwidth=4
