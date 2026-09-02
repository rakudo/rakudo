use MONKEY-SEE-NO-EVAL;
use Test;
use nqp;

# CALLER:: inside a BEGIN block names the frame of the compiler that runs
# the block. That frame's chain of outers never reaches a setting, so an
# EVAL given it as context compiles against the setting of the compilation
# in progress instead, and starts in the package of that compilation. The
# BEGIN blocks below run while the string holding them compiles, so the
# tests are skipped as a whole on a frontend that dies at that point.

plan 6;

if nqp::gethllsym('Raku', 'COMPILER-FRONTEND') eq 'rakuast' {
    EVAL q:to/TESTS/;
    my $call;
    BEGIN { $call = EVAL Q[my sub inner() { 42 }; inner()], :context(CALLER::) }
    is $call, 42,
      'BEGIN-time string EVAL with the CALLER:: context of the BEGIN block resolves a call';

    my $from-setting;
    BEGIN { $from-setting = EVAL Q[sqrt(16)], :context(CALLER::) }
    is $from-setting, 4,
      'BEGIN-time string EVAL with the CALLER:: context resolves a setting routine';

    my $unit-package;
    BEGIN { $unit-package = EVAL Q[$?PACKAGE], :context(CALLER::) }
    ok $unit-package === GLOBAL,
      'BEGIN-time string EVAL with the CALLER:: context at file scope starts in GLOBAL';

    my $class-package;
    class PackageFromCallerEval {
        BEGIN { $class-package = EVAL Q[$?PACKAGE], :context(CALLER::) }
    }
    ok $class-package === PackageFromCallerEval,
      'BEGIN-time string EVAL with the CALLER:: context inside a class starts in that class';

    my $ast-package;
    BEGIN { $ast-package = EVAL RakuAST::Var::Compiler::Lookup.new('$?PACKAGE'), :context(CALLER::) }
    ok $ast-package === GLOBAL,
      'BEGIN-time AST EVAL with the CALLER:: context sees GLOBAL rather than a compiler lexical';

    throws-like 'BEGIN { EVAL Q[$nope], :context(CALLER::) }', X::Comp::BeginTime,
      'a compile error in a BEGIN-time string EVAL with the CALLER:: context surfaces';
    TESTS
}
else {
    skip 'the legacy frontend cannot compile against the frame that runs a BEGIN block', 6;
}

# vim: expandtab shiftwidth=4
