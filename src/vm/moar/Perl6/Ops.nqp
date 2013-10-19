# XXX this file stolen from Rakudo's JVM backend; will uncomemnt/adapt it as
# we go about the porting.

# Operand read/write/literal flags.
my $MVM_operand_literal     := 0;
my $MVM_operand_read_reg    := 1;
my $MVM_operand_write_reg   := 2;
my $MVM_operand_read_lex    := 3;
my $MVM_operand_write_lex   := 4;
my $MVM_operand_rw_mask     := 7;

# Register data types.
my $MVM_reg_int64           := 4;
my $MVM_reg_num64           := 6;
my $MVM_reg_str             := 7;
my $MVM_reg_obj             := 8;

# Operand data types.
my $MVM_operand_int64       := nqp::bitshiftl_i($MVM_reg_int64, 3);
my $MVM_operand_num64       := nqp::bitshiftl_i($MVM_reg_num64, 3);
my $MVM_operand_str         := nqp::bitshiftl_i($MVM_reg_str, 3);
my $MVM_operand_obj         := nqp::bitshiftl_i($MVM_reg_obj, 3);

# Register MoarVM extops.
use MASTNodes;
MAST::ExtOpRegistry.register_extop('p6init');
MAST::ExtOpRegistry.register_extop('p6trialbind',
    $MVM_operand_int64 +| $MVM_operand_write_reg,
    $MVM_operand_obj   +| $MVM_operand_read_reg,
    $MVM_operand_obj   +| $MVM_operand_read_reg,
    $MVM_operand_obj   +| $MVM_operand_read_reg);

# Perl 6 opcode specific mappings.
my $ops := nqp::getcomp('qast').operations;
#$ops.map_classlib_hll_op('perl6', 'p6box_i', $TYPE_P6OPS, 'p6box_i', [$RT_INT], $RT_OBJ, :tc);
#$ops.map_classlib_hll_op('perl6', 'p6box_n', $TYPE_P6OPS, 'p6box_n', [$RT_NUM], $RT_OBJ, :tc);
#$ops.map_classlib_hll_op('perl6', 'p6box_s', $TYPE_P6OPS, 'p6box_s', [$RT_STR], $RT_OBJ, :tc);
#$ops.map_classlib_hll_op('perl6', 'p6bigint', $TYPE_P6OPS, 'p6bigint', [$RT_NUM], $RT_OBJ, :tc);
#$ops.map_classlib_hll_op('perl6', 'p6parcel', $TYPE_P6OPS, 'p6parcel', [$RT_OBJ, $RT_OBJ], $RT_OBJ, :tc);
#$ops.map_classlib_hll_op('perl6', 'p6listiter', $TYPE_P6OPS, 'p6listiter', [$RT_OBJ, $RT_OBJ], $RT_OBJ, :tc);
#$ops.map_classlib_hll_op('perl6', 'p6list', $TYPE_P6OPS, 'p6list', [$RT_OBJ, $RT_OBJ, $RT_OBJ], $RT_OBJ, :tc);
#$ops.map_classlib_hll_op('perl6', 'p6listitems', $TYPE_P6OPS, 'p6listitems', [$RT_OBJ], $RT_OBJ, :tc);
#$ops.map_classlib_hll_op('perl6', 'p6recont_ro', $TYPE_P6OPS, 'p6recont_ro', [$RT_OBJ], $RT_OBJ, :tc);
#$ops.map_classlib_hll_op('perl6', 'p6store', $TYPE_P6OPS, 'p6store', [$RT_OBJ, $RT_OBJ], $RT_OBJ, :tc);
#$ops.map_classlib_hll_op('perl6', 'p6var', $TYPE_P6OPS, 'p6var', [$RT_OBJ], $RT_OBJ, :tc);
#$ops.map_classlib_hll_op('perl6', 'p6reprname', $TYPE_P6OPS, 'p6reprname', [$RT_OBJ], $RT_OBJ, :tc);
#$ops.map_classlib_hll_op('perl6', 'p6definite', $TYPE_P6OPS, 'p6definite', [$RT_OBJ], $RT_OBJ, :tc);
#$ops.add_hll_op('perl6', 'p6bindsig', :!inlinable, -> $qastcomp, $op {
#    # XXX
#});
#$ops.map_classlib_hll_op('perl6', 'p6isbindable', $TYPE_P6OPS, 'p6isbindable', [$RT_OBJ, $RT_OBJ], $RT_INT, :tc);
#$ops.map_classlib_hll_op('perl6', 'p6bindcaptosig', $TYPE_P6OPS, 'p6bindcaptosig', [$RT_OBJ, $RT_OBJ], $RT_OBJ, :tc);
$ops.add_hll_moarop_mapping('perl6', 'p6trialbind', 'p6trialbind');
#$ops.map_classlib_hll_op('perl6', 'p6typecheckrv', $TYPE_P6OPS, 'p6typecheckrv', [$RT_OBJ, $RT_OBJ], $RT_OBJ, :tc);
#$ops.map_classlib_hll_op('perl6', 'p6decontrv', $TYPE_P6OPS, 'p6decontrv', [$RT_OBJ, $RT_OBJ], $RT_OBJ, :tc);
#$ops.map_classlib_hll_op('perl6', 'p6capturelex', $TYPE_P6OPS, 'p6capturelex', [$RT_OBJ], $RT_OBJ, :tc, :!inlinable);
#$ops.map_classlib_hll_op('perl6', 'p6bindassert', $TYPE_P6OPS, 'p6bindassert', [$RT_OBJ, $RT_OBJ], $RT_OBJ, :tc);
#$ops.map_classlib_hll_op('perl6', 'p6stateinit', $TYPE_P6OPS, 'p6stateinit', [], $RT_INT, :tc, :!inlinable);
#$ops.map_classlib_hll_op('perl6', 'p6setpre', $TYPE_P6OPS, 'p6setpre', [], $RT_OBJ, :tc);
#$ops.map_classlib_hll_op('perl6', 'p6clearpre', $TYPE_P6OPS, 'p6clearpre', [], $RT_OBJ, :tc);
#$ops.map_classlib_hll_op('perl6', 'p6inpre', $TYPE_P6OPS, 'p6inpre', [], $RT_INT, :tc);
#$ops.map_classlib_hll_op('perl6', 'p6setfirstflag', $TYPE_P6OPS, 'p6setfirstflag', [$RT_OBJ], $RT_OBJ, :tc);
#$ops.map_classlib_hll_op('perl6', 'p6takefirstflag', $TYPE_P6OPS, 'p6takefirstflag', [], $RT_INT, :tc);
#$ops.add_hll_op('perl6', 'p6return', :!inlinable, -> $qastcomp, $op {
#    # XXX
#});
#$ops.map_classlib_hll_op('perl6', 'p6routinereturn', $TYPE_P6OPS, 'p6routinereturn', [$RT_OBJ], $RT_OBJ, :tc, :!inlinable);
#$ops.map_classlib_hll_op('perl6', 'p6getouterctx', $TYPE_P6OPS, 'p6getouterctx', [$RT_OBJ], $RT_OBJ, :tc, :!inlinable);
#$ops.map_classlib_hll_op('perl6', 'p6captureouters', $TYPE_P6OPS, 'p6captureouters', [$RT_OBJ], $RT_OBJ, :tc, :!inlinable);
#$ops.add_hll_op('perl6', 'p6argvmarray', -> $qastcomp, $op {
#    # XXX
#});
#$ops.map_classlib_hll_op('perl6', 'p6bindattrinvres', $TYPE_P6OPS, 'p6bindattrinvres', [$RT_OBJ, $RT_OBJ, $RT_STR, $RT_OBJ], $RT_OBJ, :tc);
#$ops.map_classlib_hll_op('perl6', 'p6finddispatcher', $TYPE_P6OPS, 'p6finddispatcher', [$RT_STR], $RT_OBJ, :tc);
#$ops.map_classlib_hll_op('perl6', 'p6argsfordispatcher', $TYPE_P6OPS, 'p6argsfordispatcher', [$RT_OBJ], $RT_OBJ, :tc);
#$ops.map_classlib_hll_op('perl6', 'p6shiftpush', $TYPE_P6OPS, 'p6shiftpush', [$RT_OBJ, $RT_OBJ, $RT_INT], $RT_OBJ, :tc);
#$ops.map_classlib_hll_op('perl6', 'p6arrfindtypes', $TYPE_P6OPS, 'p6arrfindtypes', [$RT_OBJ, $RT_OBJ, $RT_INT, $RT_INT], $RT_INT, :tc);
#$ops.map_classlib_hll_op('perl6', 'p6decodelocaltime', $TYPE_P6OPS, 'p6decodelocaltime', [$RT_INT], $RT_OBJ, :tc);
#$ops.map_classlib_hll_op('perl6', 'p6setautothreader', $TYPE_P6OPS, 'p6setautothreader', [$RT_OBJ], $RT_OBJ, :tc);
#$ops.map_classlib_hll_op('perl6', 'tclc', $TYPE_P6OPS, 'tclc', [$RT_STR], $RT_STR, :tc);
#$ops.map_classlib_hll_op('perl6', 'p6sort', $TYPE_P6OPS, 'p6sort', [$RT_OBJ, $RT_OBJ], $RT_OBJ, :tc);
#$ops.map_classlib_hll_op('perl6', 'p6staticouter', $TYPE_P6OPS, 'p6staticouter', [$RT_OBJ], $RT_OBJ, :tc);
#my $p6bool := -> $qastcomp, $op {
#    # XXX
#};
#$ops.add_hll_op('perl6', 'p6bool', $p6bool);
#$ops.map_classlib_hll_op('perl6', 'p6scalarfromdesc', $TYPE_P6OPS, 'p6scalarfromdesc', [$RT_OBJ], $RT_OBJ, :tc);
$ops.add_hll_op('perl6', 'p6invokehandler', -> $qastcomp, $op {
    $qastcomp.as_mast(QAST::Op.new( :op('call'), $op[0], $op[1] ));
});
$ops.add_hll_op('perl6', 'p6invokeflat', -> $qastcomp, $op {
    $op[1].flat(1);
    $qastcomp.as_mast(QAST::Op.new( :op('call'), $op[0], $op[1]));
});

# Make some of them also available from NQP land, since we use them in the
# metamodel and bootstrap.
#$ops.add_hll_op('nqp', 'p6bool', $p6bool);
$ops.add_hll_moarop_mapping('nqp', 'p6init', 'p6init');
#$ops.map_classlib_hll_op('nqp', 'p6settypes', $TYPE_P6OPS, 'p6settypes', [$RT_OBJ], $RT_OBJ, :tc);
#$ops.map_classlib_hll_op('nqp', 'p6var', $TYPE_P6OPS, 'p6var', [$RT_OBJ], $RT_OBJ, :tc);
#$ops.map_classlib_hll_op('nqp', 'p6parcel', $TYPE_P6OPS, 'p6parcel', [$RT_OBJ, $RT_OBJ], $RT_OBJ, :tc);
#$ops.map_classlib_hll_op('nqp', 'p6isbindable', $TYPE_P6OPS, 'p6isbindable', [$RT_OBJ, $RT_OBJ], $RT_INT, :tc);
$ops.add_hll_moarop_mapping('nqp', 'p6trialbind', 'p6trialbind');
#$ops.map_classlib_hll_op('nqp', 'p6inpre', $TYPE_P6OPS, 'p6inpre', [], $RT_INT, :tc);

# Override defor to call defined method.
$ops.add_hll_op('perl6', 'defor', -> $qastcomp, $op {
    if +$op.list != 2 {
        nqp::die("Operation 'defor' needs 2 operands");
    }
    my $tmp := $op.unique('defined');
    $qastcomp.as_mast(QAST::Stmts.new(
        QAST::Op.new(
            :op('bind'),
            QAST::Var.new( :name($tmp), :scope('local'), :decl('var') ),
            $op[0]
        ),
        QAST::Op.new(
            :op('if'),
            QAST::Op.new(
                :op('callmethod'), :name('defined'),
                QAST::Var.new( :name($tmp), :scope('local') )
            ),
            QAST::Var.new( :name($tmp), :scope('local') ),
            $op[1]
        )))
});

# Boxing and unboxing configuration.
#$ops.add_hll_box('perl6', $RT_INT, -> $qastcomp {
#    # XXX
#});
#$ops.add_hll_box('perl6', $RT_NUM, -> $qastcomp {
#    # XXX
#});
#$ops.add_hll_box('perl6', $RT_STR, -> $qastcomp {
#    # XXX
#});
