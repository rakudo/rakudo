use Test;

plan 4;

# The optimize pass only lowers an operator to a raw op when the name
# resolves to the CORE routine in the scope of its use. A lexical operator
# declaration is visible throughout its block, including before its textual
# position, so a use of the name earlier in the block must dispatch to the
# user routine rather than being lowered.

{
    my int $i = 1;
    my @seen;
    $i++;
    is $i, 99, 'a native int increment honors a postfix:<++> declared later in the block';
    sub postfix:<++>($x is rw) { @seen.push("called"); $x = 99 }
    is-deeply @seen, ["called"], 'the later-declared postfix:<++> was called';
}

{
    my @seen;
    for 3..4 { @seen.push($_) }
    is-deeply @seen, [42], 'a range for loop honors an infix:<..> declared later in the block';
    sub infix:<..>($a, $b) { (42,) }
}

{
    my @seen;
    @seen.push($_) for ^3;
    is-deeply @seen, [7, 8], 'a modifier for loop honors a prefix:<^> declared later in the block';
    sub prefix:<^>($n) { (7, 8) }
}
