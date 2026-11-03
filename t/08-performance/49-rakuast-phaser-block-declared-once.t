use lib <t/packages/Test-Helpers>;
use Test::Helpers::QAST;
use Test;
use QAST:from<NQP>;
use nqp;

plan :skip-all('these tests observe the QAST of the RakuAST frontend')
  unless nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast';
plan 1;

# How often the block calling the marker sub appears in the QAST.
my sub marked-block-occurrences(Mu $qast, Str:D $marker --> Int:D) {
    my %ids;
    my sub calls-marker(Mu $node --> Bool:D) {
        return False unless nqp::istype($node, QAST::Node);
        return True if nqp::istype($node, QAST::Op) && $node.op.starts-with('call') && $node.name eq $marker;
        for $node.list {
            return True if !nqp::istype($_, QAST::Block) && calls-marker($_);
        }
        False
    }
    my sub find(Mu $node, %seen) {
        return unless nqp::istype($node, QAST::Node);
        my str $id = ~nqp::objectid($node);
        return if %seen{$id}++;
        %ids{$id} = True if nqp::istype($node, QAST::Block) && calls-marker($node);
        find($_, %seen) for $node.list;
    }
    my sub count(Mu $node, %on-path --> Int:D) {
        return 0 unless nqp::istype($node, QAST::Node);
        my str $id = ~nqp::objectid($node);
        return 0 if %on-path{$id};
        my int $n = %ids{$id} ?? 1 !! 0;
        %on-path{$id} = True;
        $n = $n + count($_, %on-path) for $node.list;
        %on-path{$id} = False;
        $n
    }
    find($qast, {});
    count($qast, {})
}

# This holds whichever block declares the code, and guards what it keeps.
qast-is 'sub quit-marker() { }; supply { whenever Supply.from-list(1) { QUIT { quit-marker() } } }', :full, -> \v {
    marked-block-occurrences(v, '&quit-marker') == 1
}, 'the block of a QUIT phaser is declared once';

# vim: expandtab shiftwidth=4
