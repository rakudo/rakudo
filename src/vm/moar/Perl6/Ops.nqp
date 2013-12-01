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
MAST::ExtOpRegistry.register_extop('p6settypes',
    $MVM_operand_obj   +| $MVM_operand_read_reg);
MAST::ExtOpRegistry.register_extop('p6box_i',
    $MVM_operand_obj   +| $MVM_operand_write_reg,
    $MVM_operand_int64 +| $MVM_operand_read_reg);
MAST::ExtOpRegistry.register_extop('p6box_n',
    $MVM_operand_obj   +| $MVM_operand_write_reg,
    $MVM_operand_num64 +| $MVM_operand_read_reg);
MAST::ExtOpRegistry.register_extop('p6box_s',
    $MVM_operand_obj   +| $MVM_operand_write_reg,
    $MVM_operand_str   +| $MVM_operand_read_reg);
MAST::ExtOpRegistry.register_extop('p6list',
    $MVM_operand_obj   +| $MVM_operand_write_reg,
    $MVM_operand_obj   +| $MVM_operand_read_reg,
    $MVM_operand_obj   +| $MVM_operand_read_reg,
    $MVM_operand_obj   +| $MVM_operand_read_reg);
MAST::ExtOpRegistry.register_extop('p6listitems',
    $MVM_operand_obj   +| $MVM_operand_write_reg,
    $MVM_operand_obj   +| $MVM_operand_read_reg);
MAST::ExtOpRegistry.register_extop('p6bool',
    $MVM_operand_obj   +| $MVM_operand_write_reg,
    $MVM_operand_int64 +| $MVM_operand_read_reg);
MAST::ExtOpRegistry.register_extop('p6scalarfromdesc',
    $MVM_operand_obj   +| $MVM_operand_write_reg,
    $MVM_operand_obj   +| $MVM_operand_read_reg);
MAST::ExtOpRegistry.register_extop('p6var',
    $MVM_operand_obj   +| $MVM_operand_write_reg,
    $MVM_operand_obj   +| $MVM_operand_read_reg);
MAST::ExtOpRegistry.register_extop('p6parcel',
    $MVM_operand_obj   +| $MVM_operand_write_reg,
    $MVM_operand_obj   +| $MVM_operand_read_reg,
    $MVM_operand_obj   +| $MVM_operand_read_reg);
MAST::ExtOpRegistry.register_extop('p6listiter',
    $MVM_operand_obj   +| $MVM_operand_write_reg,
    $MVM_operand_obj   +| $MVM_operand_read_reg,
    $MVM_operand_obj   +| $MVM_operand_read_reg);
MAST::ExtOpRegistry.register_extop('p6recont_ro',
    $MVM_operand_obj   +| $MVM_operand_write_reg,
    $MVM_operand_obj   +| $MVM_operand_read_reg);
MAST::ExtOpRegistry.register_extop('p6typecheckrv',
    $MVM_operand_obj   +| $MVM_operand_read_reg,
    $MVM_operand_obj   +| $MVM_operand_read_reg);
MAST::ExtOpRegistry.register_extop('p6decontrv',
    $MVM_operand_obj   +| $MVM_operand_write_reg,
    $MVM_operand_obj   +| $MVM_operand_read_reg,
    $MVM_operand_obj   +| $MVM_operand_read_reg);
MAST::ExtOpRegistry.register_extop('p6capturelex',
    $MVM_operand_obj   +| $MVM_operand_write_reg,
    $MVM_operand_obj   +| $MVM_operand_read_reg);
MAST::ExtOpRegistry.register_extop('p6stateinit',
    $MVM_operand_int64 +| $MVM_operand_write_reg);
MAST::ExtOpRegistry.register_extop('p6setfirstflag',
    $MVM_operand_obj   +| $MVM_operand_write_reg,
    $MVM_operand_obj   +| $MVM_operand_read_reg);
MAST::ExtOpRegistry.register_extop('p6routinereturn',
    $MVM_operand_obj   +| $MVM_operand_write_reg,
    $MVM_operand_obj   +| $MVM_operand_read_reg);
MAST::ExtOpRegistry.register_extop('p6captureouters',
    $MVM_operand_obj   +| $MVM_operand_read_reg);
MAST::ExtOpRegistry.register_extop('p6finddispatcher',
    $MVM_operand_obj   +| $MVM_operand_write_reg,
    $MVM_operand_str   +| $MVM_operand_read_reg);
MAST::ExtOpRegistry.register_extop('p6argsfordispatcher',
    $MVM_operand_obj   +| $MVM_operand_write_reg,
    $MVM_operand_obj   +| $MVM_operand_read_reg);
MAST::ExtOpRegistry.register_extop('p6shiftpush',
    $MVM_operand_obj   +| $MVM_operand_write_reg,
    $MVM_operand_obj   +| $MVM_operand_read_reg,
    $MVM_operand_obj   +| $MVM_operand_read_reg,
    $MVM_operand_int64 +| $MVM_operand_read_reg);
MAST::ExtOpRegistry.register_extop('p6arrfindtypes',
    $MVM_operand_int64 +| $MVM_operand_write_reg,
    $MVM_operand_obj   +| $MVM_operand_read_reg,
    $MVM_operand_obj   +| $MVM_operand_read_reg,
    $MVM_operand_int64 +| $MVM_operand_read_reg,
    $MVM_operand_int64 +| $MVM_operand_read_reg);
MAST::ExtOpRegistry.register_extop('p6decodelocaltime',
    $MVM_operand_obj   +| $MVM_operand_write_reg,
    $MVM_operand_int64 +| $MVM_operand_read_reg);
MAST::ExtOpRegistry.register_extop('p6sort',
    $MVM_operand_obj   +| $MVM_operand_write_reg,
    $MVM_operand_obj   +| $MVM_operand_read_reg,
    $MVM_operand_obj   +| $MVM_operand_read_reg);
MAST::ExtOpRegistry.register_extop('p6staticouter',
    $MVM_operand_obj   +| $MVM_operand_write_reg,
    $MVM_operand_obj   +| $MVM_operand_read_reg);
MAST::ExtOpRegistry.register_extop('p6inpre',
    $MVM_operand_int64 +| $MVM_operand_write_reg);

# Perl 6 opcode specific mappings.
my $ops := nqp::getcomp('QAST').operations;
$ops.add_hll_moarop_mapping('perl6', 'p6box_i', 'p6box_i');
$ops.add_hll_moarop_mapping('perl6', 'p6box_n', 'p6box_n');
$ops.add_hll_moarop_mapping('perl6', 'p6box_s', 'p6box_s');
#$ops.map_classlib_hll_op('perl6', 'p6bigint', $TYPE_P6OPS, 'p6bigint', [$RT_NUM], $RT_OBJ, :tc);
$ops.add_hll_moarop_mapping('perl6', 'p6parcel', 'p6parcel');
$ops.add_hll_moarop_mapping('perl6', 'p6listiter', 'p6listiter');
$ops.add_hll_moarop_mapping('perl6', 'p6list', 'p6list');
$ops.add_hll_moarop_mapping('perl6', 'p6listitems', 'p6listitems');
$ops.add_hll_moarop_mapping('perl6', 'p6recont_ro', 'p6recont_ro');
$ops.add_hll_op('perl6', 'p6store', -> $qastcomp, $op {
    my @ops;
    my $cont_res  := $qastcomp.as_mast($op[0], :want($MVM_reg_obj));
    my $value_res := $qastcomp.as_mast($op[1], :want($MVM_reg_obj));
    push_ilist(@ops, $cont_res);
    push_ilist(@ops, $value_res);
    
    my $iscont_reg  := $*REGALLOC.fresh_i();
    my $no_cont_lbl := MAST::Label.new(:name($op.unique('p6store_no_cont_')));
    my $done_lbl    := MAST::Label.new(:name($op.unique('p6store_done_')));
    nqp::push(@ops, MAST::Op.new( :op('iscont'), $iscont_reg, $cont_res.result_reg ));
    nqp::push(@ops, MAST::Op.new( :op('unless'), $iscont_reg, $no_cont_lbl ));
    $*REGALLOC.release_register($iscont_reg, $MVM_reg_int64);
    nqp::push(@ops, MAST::Op.new( :op('decont'), $value_res.result_reg, $value_res.result_reg ));
    nqp::push(@ops, MAST::Op.new( :op('assign'), $cont_res.result_reg, $value_res.result_reg ));
    nqp::push(@ops, MAST::Op.new( :op('goto'), $done_lbl ));
    
    my $meth_reg := $*REGALLOC.fresh_o();
    nqp::push(@ops, $no_cont_lbl);
    nqp::push(@ops, MAST::Op.new( :op('findmeth'), $meth_reg, $cont_res.result_reg,
        MAST::SVal.new( :value('STORE') ) ));
    nqp::push(@ops, MAST::Call.new(
        :target($meth_reg),
        :flags($Arg::obj, $Arg::obj),
        $cont_res.result_reg, $value_res.result_reg
    ));
    nqp::push(@ops, $done_lbl);
    $*REGALLOC.release_register($meth_reg, $MVM_reg_obj);
    
    MAST::InstructionList.new(@ops, $cont_res.result_reg, $MVM_reg_obj)
});
$ops.add_hll_moarop_mapping('perl6', 'p6var', 'p6var');
#$ops.map_classlib_hll_op('perl6', 'p6reprname', $TYPE_P6OPS, 'p6reprname', [$RT_OBJ], $RT_OBJ, :tc);
$ops.add_hll_op('perl6', 'p6definite', -> $qastcomp, $op {
    my @ops;
    my $value_res := $qastcomp.as_mast($op[0], :want($MVM_reg_obj));
    push_ilist(@ops, $value_res);
    nqp::push(@ops, MAST::Op.new( :op('decont'), $value_res.result_reg, $value_res.result_reg ));
    my $tmp_reg := $*REGALLOC.fresh_i();
    nqp::push(@ops, MAST::Op.new( :op('isconcrete'), $tmp_reg, $value_res.result_reg ));
    nqp::push(@ops, MAST::ExtOp.new( :op('p6bool'), :cu($*MAST_COMPUNIT),
        $value_res.result_reg, $tmp_reg ));
    $*REGALLOC.release_register($tmp_reg, $MVM_reg_int64);
    MAST::InstructionList.new(@ops, $value_res.result_reg, $MVM_reg_obj)
});
#$ops.map_classlib_hll_op('perl6', 'p6bindcaptosig', $TYPE_P6OPS, 'p6bindcaptosig', [$RT_OBJ, $RT_OBJ], $RT_OBJ, :tc);
$ops.add_hll_moarop_mapping('perl6', 'p6typecheckrv', 'p6typecheckrv', 0);
$ops.add_hll_moarop_mapping('perl6', 'p6decontrv', 'p6decontrv');
$ops.add_hll_moarop_mapping('perl6', 'p6capturelex', 'p6capturelex');
$ops.add_hll_op('perl6', 'p6bindassert', -> $qastcomp, $op {
    # Compile the bind value and the type.
    my @ops;
    my $value_res := $qastcomp.as_mast($op[0], :want($MVM_reg_obj));
    my $type_res  := $qastcomp.as_mast($op[1], :want($MVM_reg_obj));
    push_ilist(@ops, $value_res);
    push_ilist(@ops, $type_res);
    
    # Emit a type check.
    my $tcr_reg  := $*REGALLOC.fresh_i();
    my $lbl_done := MAST::Label.new(:name($op.unique('bindassert_done_')));
    nqp::push(@ops, MAST::Op.new( :op('istype'), $tcr_reg, $value_res.result_reg, $type_res.result_reg ));
    nqp::push(@ops, MAST::Op.new( :op('if_i'), $tcr_reg, $lbl_done ));
    $*REGALLOC.release_register($tcr_reg, $MVM_reg_int64);
    
    # Error generation.
    proto bind_error($got, $wanted) {
        nqp::die('Type check failed in bind; expected ' ~
            $wanted.HOW.name($wanted) ~ ' but got ' ~
            $got.HOW.name($got));
    }
    my $err_rep := $qastcomp.as_mast(QAST::WVal.new( :value(nqp::getcodeobj(&bind_error)) ));
    push_ilist(@ops, $err_rep);
    nqp::push(@ops, MAST::Call.new(
        :target($err_rep.result_reg),
        :flags($Arg::obj, $Arg::obj),
        $value_res.result_reg, $type_res.result_reg
    ));
    nqp::push(@ops, $lbl_done);
    $*REGALLOC.release_register($err_rep.result_reg, $MVM_reg_obj);

    MAST::InstructionList.new(@ops, $value_res.result_reg, $MVM_reg_obj)
});
$ops.add_hll_moarop_mapping('perl6', 'p6stateinit', 'p6stateinit');
#$ops.map_classlib_hll_op('perl6', 'p6setpre', $TYPE_P6OPS, 'p6setpre', [], $RT_OBJ, :tc);
#$ops.map_classlib_hll_op('perl6', 'p6clearpre', $TYPE_P6OPS, 'p6clearpre', [], $RT_OBJ, :tc);
$ops.add_hll_moarop_mapping('perl6', 'p6setfirstflag', 'p6setfirstflag');
#$ops.map_classlib_hll_op('perl6', 'p6takefirstflag', $TYPE_P6OPS, 'p6takefirstflag', [], $RT_INT, :tc);
$ops.add_hll_op('perl6', 'p6return', :!inlinable, -> $qastcomp, $op {
    # XXX Probably needs more than just this, but it does for now.
    my @ops;
    my $value_res := $qastcomp.as_mast($op[0], :want($MVM_reg_obj));
    push_ilist(@ops, $value_res);
    nqp::push(@ops, MAST::Op.new( :op('return_o'), $value_res.result_reg ));
    MAST::InstructionList.new(@ops, $value_res.result_reg, $MVM_reg_obj)
});
$ops.add_hll_moarop_mapping('perl6', 'p6routinereturn', 'p6routinereturn');
#$ops.map_classlib_hll_op('perl6', 'p6getouterctx', $TYPE_P6OPS, 'p6getouterctx', [$RT_OBJ], $RT_OBJ, :tc, :!inlinable);
$ops.add_hll_moarop_mapping('perl6', 'p6captureouters', 'p6captureouters', 0);
$ops.add_hll_op('perl6', 'p6argvmarray', -> $qastcomp, $op {
    my @ops;
    my $res_reg := $*REGALLOC.fresh_o();
    nqp::push(@ops, MAST::Op.new( :op('param_sp'), $res_reg,
        MAST::IVal.new( :value(0), :size(16) )));
    MAST::InstructionList.new(@ops, $res_reg, $MVM_reg_obj)
});
$ops.add_hll_op('perl6', 'p6bindattrinvres', -> $qastcomp, $op {
    my @ops;
    my $inv_res := $qastcomp.as_mast($op[0], :want($MVM_reg_obj));
    my $ch_res  := $qastcomp.as_mast(QAST::Op.new( :op('decont'), $op[1] ), :want($MVM_reg_obj));
    my $nam_res := $qastcomp.as_mast($op[2], :want($MVM_reg_str));
    my $val_res := $qastcomp.as_mast($op[3], :want($MVM_reg_obj));
    push_ilist(@ops, $inv_res);
    push_ilist(@ops, $ch_res);
    push_ilist(@ops, $nam_res);
    push_ilist(@ops, $val_res);
    nqp::push(@ops, MAST::Op.new( :op('bindattrs_o'), $inv_res.result_reg,
        $ch_res.result_reg, $nam_res.result_reg, $val_res.result_reg));
    $*REGALLOC.release_register($ch_res.result_reg, $MVM_reg_obj);
    $*REGALLOC.release_register($nam_res.result_reg, $MVM_reg_str);
    $*REGALLOC.release_register($val_res.result_reg, $MVM_reg_obj);
    MAST::InstructionList.new(@ops, $inv_res.result_reg, $MVM_reg_obj)
});
$ops.add_hll_moarop_mapping('perl6', 'p6finddispatcher', 'p6finddispatcher');
$ops.add_hll_moarop_mapping('perl6', 'p6argsfordispatcher', 'p6argsfordispatcher');
$ops.add_hll_moarop_mapping('perl6', 'p6shiftpush', 'p6shiftpush');
$ops.add_hll_moarop_mapping('perl6', 'p6arrfindtypes', 'p6arrfindtypes');
$ops.add_hll_moarop_mapping('perl6', 'p6decodelocaltime', 'p6decodelocaltime');
#$ops.map_classlib_hll_op('perl6', 'tclc', $TYPE_P6OPS, 'tclc', [$RT_STR], $RT_STR, :tc);
$ops.add_hll_moarop_mapping('perl6', 'p6sort', 'p6sort');
$ops.add_hll_moarop_mapping('perl6', 'p6staticouter', 'p6staticouter');
my $p6bool := -> $qastcomp, $op {
    # Compile instructions.
    my @ops;
    my $exprres := $qastcomp.as_mast($op[0]);
    push_ilist(@ops, $exprres);

    # Go by result kind.
    my $res_reg   := $*REGALLOC.fresh_o();
    my $cond_kind := $exprres.result_kind;
    if $cond_kind == $MVM_reg_int64 {
        nqp::push(@ops, MAST::ExtOp.new( :op('p6bool'), :cu($*MAST_COMPUNIT),
            $res_reg, $exprres.result_reg ));
    }
    elsif $cond_kind == $MVM_reg_num64 {
        my $tmp_reg := $*REGALLOC.fresh_i();
        my $zero_reg := $*REGALLOC.fresh_n();
        nqp::push(@ops, MAST::Op.new( :op('const_n64'), $zero_reg, MAST::NVal.new( :value(0.0) ) ));
        nqp::push(@ops, MAST::Op.new( :op('eq_n'), $tmp_reg, $exprres.result_reg, $zero_reg ));
        nqp::push(@ops, MAST::ExtOp.new( :op('p6bool'), :cu($*MAST_COMPUNIT),
            $res_reg, $tmp_reg ));
        $*REGALLOC.release_register($zero_reg, $MVM_reg_num64);
        $*REGALLOC.release_register($tmp_reg, $MVM_reg_int64);
    }
    elsif $cond_kind == $MVM_reg_str {
        my $tmp_reg := $*REGALLOC.fresh_i();
        nqp::push(@ops, MAST::Op.new( :op('istrue_s'), $tmp_reg, $exprres.result_reg ));
        nqp::push(@ops, MAST::ExtOp.new( :op('p6bool'), :cu($*MAST_COMPUNIT),
            $res_reg, $tmp_reg ));
        $*REGALLOC.release_register($tmp_reg, $MVM_reg_int64);
    }
    elsif $cond_kind == $MVM_reg_obj {
        my $tmp_reg := $*REGALLOC.fresh_i();
        nqp::push(@ops, MAST::Op.new( :op('decont'), $exprres.result_reg, $exprres.result_reg ));
        nqp::push(@ops, MAST::Op.new( :op('istrue'), $tmp_reg, $exprres.result_reg ));
        nqp::push(@ops, MAST::ExtOp.new( :op('p6bool'), :cu($*MAST_COMPUNIT),
            $res_reg, $tmp_reg ));
        $*REGALLOC.release_register($tmp_reg, $MVM_reg_int64);
    }
    else {
        nqp::die('Unknown register type in p6bool');
    }
    $*REGALLOC.release_register($exprres.result_reg, $exprres.result_kind);
    MAST::InstructionList.new(@ops, $res_reg, $MVM_reg_obj)
};
$ops.add_hll_op('perl6', 'p6bool', $p6bool);
$ops.add_hll_moarop_mapping('perl6', 'p6scalarfromdesc', 'p6scalarfromdesc');
$ops.add_hll_op('perl6', 'p6invokehandler', -> $qastcomp, $op {
    $qastcomp.as_mast(QAST::Op.new( :op('call'), $op[0], $op[1] ));
});
$ops.add_hll_op('perl6', 'p6invokeflat', -> $qastcomp, $op {
    $op[1].flat(1);
    $qastcomp.as_mast(QAST::Op.new( :op('call'), $op[0], $op[1]));
});

# Make some of them also available from NQP land, since we use them in the
# metamodel and bootstrap.
$ops.add_hll_op('nqp', 'p6bool', $p6bool);
$ops.add_hll_moarop_mapping('nqp', 'p6init', 'p6init');
$ops.add_hll_moarop_mapping('nqp', 'p6settypes', 'p6settypes', 0);
$ops.add_hll_moarop_mapping('nqp', 'p6var', 'p6var');
$ops.add_hll_moarop_mapping('nqp', 'p6parcel', 'p6parcel');
$ops.add_hll_moarop_mapping('nqp', 'p6inpre', 'p6inpre');

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
sub boxer($kind, $op) {
    -> $qastcomp, $reg {
        my @ops;
        my $res_reg := $*REGALLOC.fresh_register($MVM_reg_obj);
        nqp::push(@ops, MAST::ExtOp.new( :op($op), :cu($*MAST_COMPUNIT),
            $res_reg, $reg ));
        $*REGALLOC.release_register($reg, $kind);
        MAST::InstructionList.new(@ops, $res_reg, $MVM_reg_obj)
    }
}
$ops.add_hll_box('perl6', $MVM_reg_int64, boxer($MVM_reg_int64, 'p6box_i'));
$ops.add_hll_box('perl6', $MVM_reg_num64, boxer($MVM_reg_num64, 'p6box_n'));
$ops.add_hll_box('perl6', $MVM_reg_str, boxer($MVM_reg_str, 'p6box_s'));

# Signature binding related bits.
our $Binder;
$ops.add_hll_op('perl6', 'p6bindsig', :!inlinable, -> $qastcomp, $op {
    $qastcomp.as_mast(QAST::Op.new(
        :op('callmethod'), :name('bind_sig'),
        QAST::WVal.new( :value($Binder) ),
        QAST::Op.new( :op('savecapture') )
    ));
});
my $is_bindable := -> $qastcomp, $op {
    $qastcomp.as_mast(QAST::Op.new(
        :op('callmethod'), :name('is_bindable'),
        QAST::WVal.new( :value($Binder) ),
        |@($op)
    ));
};
$ops.add_hll_op('nqp', 'p6isbindable', :!inlinable, $is_bindable);
$ops.add_hll_op('perl6', 'p6isbindable', :!inlinable, $is_bindable);
proto sub trial_bind(*@args) {
    $Binder.trial_bind(|@args);
}
my $trial_bind := -> $qastcomp, $op {
    $qastcomp.as_mast(QAST::Op.new(
        :op('call'),
        QAST::WVal.new( :value(nqp::getcodeobj(&trial_bind)) ),
        |@($op)
    ));
};
$ops.add_hll_op('nqp', 'p6trialbind', :!inlinable, $trial_bind);
$ops.add_hll_op('perl6', 'p6trialbind', :!inlinable, $trial_bind);
proto sub set_binder($b) {
    $Binder := $b;
}
$ops.add_hll_op('nqp', 'p6setbinder', -> $qastcomp, $op {
    $qastcomp.as_mast(QAST::Op.new(
        :op('call'),
        QAST::WVal.new( :value(nqp::getcodeobj(&set_binder)) ),
        |@($op)
    ));
});
$ops.add_hll_op('perl6', 'p6setautothreader', :inlinable, -> $qastcomp, $op {
    $qastcomp.as_mast(
        QAST::Op.new( :op('callmethod'), :name('set_autothreader'),
            QAST::WVal.new( :value($Binder) ),
            $op[0]), :want($MVM_reg_obj));
});

sub push_ilist(@dest, $src) {
    nqp::splice(@dest, $src.instructions, +@dest, 0);
}
