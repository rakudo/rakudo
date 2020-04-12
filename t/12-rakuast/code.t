use MONKEY-SEE-NO-EVAL;
use Test;

plan 3;

{
    my $block := EVAL RakuAST::CompUnit.new(
        RakuAST::StatementList.new(
            RakuAST::Statement::Expression.new(
                RakuAST::PointyBlock.new(
                    signature => RakuAST::Signature.new(
                        parameters => ()
                    ),
                    body => RakuAST::Blockoid.new(RakuAST::StatementList.new(
                        RakuAST::Statement::Expression.new(
                            RakuAST::IntLiteral.new(101)
                        )
                    ))
                )
            )
        )
    );
    ok $block.WHAT === Block, 'A point block node evaluates to a Block';
    is $block.signature.params.elems, 0, 'The block takes no parameters';
    is $block(), 101, 'Invoking the block returns the expected value';
}
