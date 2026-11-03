use v6;
use nqp;
use Test;

plan 3;

# An anonymous container declaration must yield a fresh object on every
# call. When the declaration stays a clone-vivified lexical, the MoarVM
# spesh inliner vivifies it once per caller frame instead of once per
# call, so a hot loop starts receiving the same object every iteration.
# Lowering the declaration to a local avoids that shape. These loops run
# long enough for specialization and inlining to have happened.

sub read-ident(Str $source --> Str) { 'id' }

sub parse-group(Str $source, $pos is rw --> Array) {
    my @nodes;
    while $pos < $source.chars {
        my $char = $source.substr($pos, 1);
        if $char eq ':' {
            $pos = $source.chars;
            @nodes.push: { type => 'param', name => read-ident($source) };
        }
        else {
            @nodes.push: { type => 'literal', text => $char };
            $pos++;
        }
    }
    @nodes
}

my $literal-divergence = -1;
for 1..2000 -> $i {
    my $pos = 0;
    my @ast = parse-group('/widgets/:id', $pos);
    my $literals = @ast.grep({ .<type> eq 'literal' }).map({ .<text> }).join;
    unless $literals eq '/widgets/' {
        $literal-divergence = $i;
        last;
    }
}
is $literal-divergence, -1,
    'a hash composer in a hot loop keeps per-iteration literal values';

my $identity-divergence = -1;
for 1..2000 -> $i {
    my $pos = 0;
    my @ast = parse-group('/widgets/:id', $pos);
    my @wheres = @ast.grep({ .<type> eq 'literal' }).map({ .WHERE });
    unless @wheres.unique.elems == @wheres.elems {
        $identity-divergence = $i;
        last;
    }
}
is $identity-divergence, -1,
    'a hash composer in a hot loop allocates a distinct hash per iteration';

sub begin-used-composer(*@elems) { my % = @elems }
BEGIN begin-used-composer('warm', 1);

sub collect-three($i) {
    my @seen;
    for ^3 -> $j {
        @seen.push: begin-used-composer('n', $i + $j);
    }
    @seen
}

my $begin-divergence = -1;
for 1..2000 -> $i {
    my @seen = collect-three($i);
    unless @seen.map({ .WHERE }).unique.elems == 3 {
        $begin-divergence = $i;
        last;
    }
}
# The legacy frontend compiles a BEGIN-invoked sub dynamically with the
# optimizer skipped, so its anonymous hash stays a clone-vivified lexical
# and the spesh inliner serves the same object to every loop iteration.
todo 'BEGIN-compiled routines keep unlowered containers under the legacy frontend'
    unless nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';
is $begin-divergence, -1,
    'a BEGIN-invoked sub returns a fresh anonymous hash per call in a hot loop';
