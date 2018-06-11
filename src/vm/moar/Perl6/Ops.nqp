# Operand read/write/literal flags.
my int $MVM_operand_literal     := 0;
my int $MVM_operand_read_reg    := 1;
my int $MVM_operand_write_reg   := 2;
my int $MVM_operand_read_lex    := 3;
my int $MVM_operand_write_lex   := 4;
my int $MVM_operand_rw_mask     := 7;

# Register data types.
my int $MVM_reg_void            := 0;
my int $MVM_reg_int64           := 4;
my int $MVM_reg_num64           := 6;
my int $MVM_reg_str             := 7;
my int $MVM_reg_obj             := 8;
my int $MVM_reg_uint64          := 20;

# Operand data types.
my int $MVM_operand_int64       := nqp::bitshiftl_i($MVM_reg_int64, 3);
my int $MVM_operand_num64       := nqp::bitshiftl_i($MVM_reg_num64, 3);
my int $MVM_operand_str         := nqp::bitshiftl_i($MVM_reg_str, 3);
my int $MVM_operand_obj         := nqp::bitshiftl_i($MVM_reg_obj, 3);
my int $MVM_operand_uint64      := nqp::bitshiftl_i($MVM_reg_uint64, 3);

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
MAST::ExtOpRegistry.register_extop('p6box_u',
    $MVM_operand_obj   +| $MVM_operand_write_reg,
    $MVM_operand_uint64 +| $MVM_operand_read_reg);
MAST::ExtOpRegistry.register_extop('p6bool',
    $MVM_operand_obj   +| $MVM_operand_write_reg,
    $MVM_operand_int64 +| $MVM_operand_read_reg);
MAST::ExtOpRegistry.register_extop('p6var',
    $MVM_operand_obj   +| $MVM_operand_write_reg,
    $MVM_operand_obj   +| $MVM_operand_read_reg);
MAST::ExtOpRegistry.register_extop('p6reprname',
    $MVM_operand_obj   +| $MVM_operand_write_reg,
    $MVM_operand_obj   +| $MVM_operand_read_reg);
MAST::ExtOpRegistry.register_extop('p6decontrv',
    $MVM_operand_obj   +| $MVM_operand_write_reg,
    $MVM_operand_obj   +| $MVM_operand_read_reg);
MAST::ExtOpRegistry.register_extop('p6capturelex',
    $MVM_operand_obj   +| $MVM_operand_write_reg,
    $MVM_operand_obj   +| $MVM_operand_read_reg);
MAST::ExtOpRegistry.register_extop('p6capturelexwhere',
    $MVM_operand_obj   +| $MVM_operand_write_reg,
    $MVM_operand_obj   +| $MVM_operand_read_reg);
MAST::ExtOpRegistry.register_extop('p6stateinit',
    $MVM_operand_int64 +| $MVM_operand_write_reg);
MAST::ExtOpRegistry.register_extop('p6setfirstflag',
    $MVM_operand_obj   +| $MVM_operand_write_reg,
    $MVM_operand_obj   +| $MVM_operand_read_reg);
MAST::ExtOpRegistry.register_extop('p6takefirstflag',
    $MVM_operand_int64 +| $MVM_operand_write_reg);
MAST::ExtOpRegistry.register_extop('p6captureouters',
    $MVM_operand_obj   +| $MVM_operand_read_reg,
    $MVM_operand_obj   +| $MVM_operand_read_reg);
MAST::ExtOpRegistry.register_extop('p6getouterctx',
    $MVM_operand_obj   +| $MVM_operand_write_reg,
    $MVM_operand_obj   +| $MVM_operand_read_reg);
MAST::ExtOpRegistry.register_extop('p6finddispatcher',
    $MVM_operand_obj   +| $MVM_operand_write_reg,
    $MVM_operand_str   +| $MVM_operand_read_reg);
MAST::ExtOpRegistry.register_extop('p6argsfordispatcher',
    $MVM_operand_obj   +| $MVM_operand_write_reg,
    $MVM_operand_obj   +| $MVM_operand_read_reg);
MAST::ExtOpRegistry.register_extop('p6decodelocaltime',
    $MVM_operand_obj   +| $MVM_operand_write_reg,
    $MVM_operand_int64 +| $MVM_operand_read_reg);
MAST::ExtOpRegistry.register_extop('p6staticouter',
    $MVM_operand_obj   +| $MVM_operand_write_reg,
    $MVM_operand_obj   +| $MVM_operand_read_reg);
MAST::ExtOpRegistry.register_extop('p6setpre',
    $MVM_operand_obj   +| $MVM_operand_write_reg);
MAST::ExtOpRegistry.register_extop('p6clearpre',
    $MVM_operand_obj   +| $MVM_operand_write_reg);
MAST::ExtOpRegistry.register_extop('p6inpre',
    $MVM_operand_int64 +| $MVM_operand_write_reg);
MAST::ExtOpRegistry.register_extop('p6invokeunder',
    $MVM_operand_obj   +| $MVM_operand_write_reg,
    $MVM_operand_obj   +| $MVM_operand_read_reg,
    $MVM_operand_obj   +| $MVM_operand_read_reg);

# Register a de-sugar from one QAST tree to another.
sub register_op_desugar($name, $desugar, :$inlinable = 1) is export {
    nqp::getcomp('QAST').operations.add_hll_op('perl6', $name, :$inlinable, -> $qastcomp, $op {
        $qastcomp.as_mast($desugar($op));
    });
}

# Perl 6 opcode specific mappings.
my $ops := nqp::getcomp('QAST').operations;
$ops.add_hll_moarop_mapping('perl6', 'p6box_i', 'p6box_i');
$ops.add_hll_moarop_mapping('perl6', 'p6box_n', 'p6box_n');
$ops.add_hll_moarop_mapping('perl6', 'p6box_s', 'p6box_s');
$ops.add_hll_moarop_mapping('perl6', 'p6box_u', 'p6box_u');
$ops.add_hll_op('perl6', 'p6store', -> $qastcomp, $op {
    my @ops;
    my $cont_res  := $qastcomp.as_mast($op[0], :want($MVM_reg_obj));
    my $value_res := $qastcomp.as_mast($op[1], :want($MVM_reg_obj));
    push_ilist(@ops, $cont_res);
    push_ilist(@ops, $value_res);

    my $iscont_reg  := $*REGALLOC.fresh_i();
    my $decont_reg  := $*REGALLOC.fresh_o();
    my $no_cont_lbl := MAST::Label.new();
    my $done_lbl    := MAST::Label.new();
    nqp::push(@ops, MAST::Op.new( :op('iscont'), $iscont_reg, $cont_res.result_reg ));
    nqp::push(@ops, MAST::Op.new( :op('unless_i'), $iscont_reg, $no_cont_lbl ));
    $*REGALLOC.release_register($iscont_reg, $MVM_reg_int64);
    nqp::push(@ops, MAST::Op.new( :op('decont'), $decont_reg, $value_res.result_reg ));
    nqp::push(@ops, MAST::Op.new( :op('assign'), $cont_res.result_reg, $decont_reg ));
    $*REGALLOC.release_register($decont_reg, $MVM_reg_obj);
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
$ops.add_hll_moarop_mapping('perl6', 'p6reprname', 'p6reprname', :decont(0));
$ops.add_hll_op('perl6', 'p6definite', -> $qastcomp, $op {
    my @ops;
    my $value_res := $qastcomp.as_mast($op[0], :want($MVM_reg_obj));
    push_ilist(@ops, $value_res);
    my $tmp_reg := $*REGALLOC.fresh_i();
    my $res_reg := $*REGALLOC.fresh_o();
    nqp::push(@ops, MAST::Op.new( :op('decont'), $res_reg, $value_res.result_reg ));
    nqp::push(@ops, MAST::Op.new( :op('isconcrete'), $tmp_reg, $res_reg ));
    nqp::push(@ops, MAST::ExtOp.new( :op('p6bool'), :cu($qastcomp.mast_compunit),
        $res_reg, $tmp_reg ));
    $*REGALLOC.release_register($value_res.result_reg, $MVM_reg_obj);
    $*REGALLOC.release_register($tmp_reg, $MVM_reg_int64);
    MAST::InstructionList.new(@ops, $res_reg, $MVM_reg_obj)
});
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
    my $dc_reg   := $*REGALLOC.fresh_o();
    my $lbl_done := MAST::Label.new();
    nqp::push(@ops, MAST::Op.new( :op('decont'), $dc_reg, $value_res.result_reg ));
    nqp::push(@ops, MAST::Op.new( :op('istype'), $tcr_reg, $dc_reg, $type_res.result_reg ));
    nqp::push(@ops, MAST::Op.new( :op('if_i'), $tcr_reg, $lbl_done ));
    $*REGALLOC.release_register($dc_reg, $MVM_reg_obj);
    $*REGALLOC.release_register($tcr_reg, $MVM_reg_int64);

    # Error generation.
    proto bind_error($got, $wanted) {
        my %ex := nqp::gethllsym('perl6', 'P6EX');
        if nqp::isnull(%ex) || !nqp::existskey(%ex, 'X::TypeCheck::Binding') {
            nqp::die("Type check failed in binding; expected '" ~
                $wanted.HOW.name($wanted) ~ "' but got '" ~
                $got.HOW.name($got) ~ "'");
        }
        else {
            nqp::atkey(%ex, 'X::TypeCheck::Binding')($got, $wanted)
        }
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
$ops.add_hll_moarop_mapping('perl6', 'p6setpre', 'p6setpre');
$ops.add_hll_moarop_mapping('perl6', 'p6clearpre', 'p6clearpre');
$ops.add_hll_moarop_mapping('perl6', 'p6setfirstflag', 'p6setfirstflag');
$ops.add_hll_moarop_mapping('perl6', 'p6takefirstflag', 'p6takefirstflag');
$ops.add_hll_op('perl6', 'p6return', :!inlinable, -> $qastcomp, $op {
    my @ops;
    my $value_res := $qastcomp.as_mast($op[0], :want($MVM_reg_obj));
    push_ilist(@ops, $value_res);
    my $ex_reg := $*REGALLOC.fresh_o();
    nqp::push(@ops, MAST::Op.new( :op('exception'), $ex_reg ));
    nqp::push(@ops, MAST::Op.new( :op('exreturnafterunwind'), $ex_reg ));
    $*REGALLOC.release_register($ex_reg, $MVM_reg_obj);
    nqp::push(@ops, MAST::Op.new( :op('return_o'), $value_res.result_reg ));
    MAST::InstructionList.new(@ops, $value_res.result_reg, $MVM_reg_obj)
});
$ops.add_hll_moarop_mapping('perl6', 'p6getouterctx', 'p6getouterctx', :decont(0));
$ops.add_hll_moarop_mapping('perl6', 'p6captureouters', 'p6captureouters', 0);
$ops.add_hll_moarop_mapping('nqp', 'p6captureouters2', 'p6captureouters', 0);
$ops.add_hll_op('perl6', 'p6argvmarray', -> $qastcomp, $op {
    my @ops;
    my $res_reg := $*REGALLOC.fresh_o();
    nqp::push(@ops, MAST::Op.new( :op('param_sp'), $res_reg,
        MAST::IVal.new( :value(0), :size(16) )));
    my $i_reg    := $*REGALLOC.fresh_i();
    my $n_reg    := $*REGALLOC.fresh_i();
    my $cmp_reg  := $*REGALLOC.fresh_i();
    my $tmp_reg  := $*REGALLOC.fresh_o();
    my $lbl_next := MAST::Label.new();
    my $lbl_done := MAST::Label.new();
    nqp::push(@ops, MAST::Op.new( :op('elems'), $n_reg, $res_reg ));
    nqp::push(@ops, MAST::Op.new( :op('const_i64'), $i_reg, MAST::IVal.new( :value(0) ) ));
    nqp::push(@ops, $lbl_next);
    nqp::push(@ops, MAST::Op.new( :op('lt_i'), $cmp_reg, $i_reg, $n_reg ));
    nqp::push(@ops, MAST::Op.new( :op('unless_i'), $cmp_reg, $lbl_done ));
    nqp::push(@ops, MAST::Op.new( :op('atpos_o'), $tmp_reg, $res_reg, $i_reg ));
    nqp::push(@ops, MAST::Op.new( :op('hllize'), $tmp_reg, $tmp_reg ));
    nqp::push(@ops, MAST::Op.new( :op('bindpos_o'), $res_reg, $i_reg, $tmp_reg ));
    nqp::push(@ops, MAST::Op.new( :op('const_i64'), $cmp_reg, MAST::IVal.new( :value(1) ) ));
    nqp::push(@ops, MAST::Op.new( :op('add_i'), $i_reg, $i_reg, $cmp_reg ));
    nqp::push(@ops, MAST::Op.new( :op('goto'), $lbl_next ));
    nqp::push(@ops, $lbl_done);
    $*REGALLOC.release_register($i_reg, $MVM_reg_int64);
    $*REGALLOC.release_register($n_reg, $MVM_reg_int64);
    $*REGALLOC.release_register($cmp_reg, $MVM_reg_int64);
    $*REGALLOC.release_register($tmp_reg, $MVM_reg_obj);
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
$ops.add_hll_moarop_mapping('perl6', 'p6decodelocaltime', 'p6decodelocaltime');
$ops.add_hll_moarop_mapping('perl6', 'p6staticouter', 'p6staticouter');
my $p6bool := -> $qastcomp, $op {
    # We never want a container here, so mark as decont context.
    my @ops;
    my $exprres := $qastcomp.as_mast($op[0], :want-decont);
    push_ilist(@ops, $exprres);

    # Go by result kind.
    my $res_reg   := $*REGALLOC.fresh_o();
    my $cond_kind := $exprres.result_kind;
    if $cond_kind == $MVM_reg_int64 {
        nqp::push(@ops, MAST::ExtOp.new( :op('p6bool'), :cu($qastcomp.mast_compunit),
            $res_reg, $exprres.result_reg ));
    }
    elsif $cond_kind == $MVM_reg_num64 {
        my $tmp_reg := $*REGALLOC.fresh_i();
        my $zero_reg := $*REGALLOC.fresh_n();
        nqp::push(@ops, MAST::Op.new( :op('const_n64'), $zero_reg, MAST::NVal.new( :value(0.0) ) ));
        nqp::push(@ops, MAST::Op.new( :op('eq_n'), $tmp_reg, $exprres.result_reg, $zero_reg ));
        nqp::push(@ops, MAST::ExtOp.new( :op('p6bool'), :cu($qastcomp.mast_compunit),
            $res_reg, $tmp_reg ));
        $*REGALLOC.release_register($zero_reg, $MVM_reg_num64);
        $*REGALLOC.release_register($tmp_reg, $MVM_reg_int64);
    }
    elsif $cond_kind == $MVM_reg_str {
        my $tmp_reg := $*REGALLOC.fresh_i();
        nqp::push(@ops, MAST::Op.new( :op('istrue_s'), $tmp_reg, $exprres.result_reg ));
        nqp::push(@ops, MAST::ExtOp.new( :op('p6bool'), :cu($qastcomp.mast_compunit),
            $res_reg, $tmp_reg ));
        $*REGALLOC.release_register($tmp_reg, $MVM_reg_int64);
    }
    elsif $cond_kind == $MVM_reg_obj {
        my $tmp_reg := $*REGALLOC.fresh_i();
        nqp::push(@ops, MAST::Op.new( :op('decont'), $res_reg, $exprres.result_reg ));
        nqp::push(@ops, MAST::Op.new( :op('istrue'), $tmp_reg, $res_reg ));
        nqp::push(@ops, MAST::ExtOp.new( :op('p6bool'), :cu($qastcomp.mast_compunit),
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
$ops.add_hll_op('perl6', 'p6invokehandler', -> $qastcomp, $op {
    $qastcomp.as_mast(QAST::Op.new( :op('call'), $op[0], $op[1] ));
});
$ops.add_hll_op('perl6', 'p6invokeflat', -> $qastcomp, $op {
    $op[1].flat(1);
    $qastcomp.as_mast(QAST::Op.new( :op('call'), $op[0], $op[1]));
});
$ops.add_hll_op('perl6', 'p6sink', -> $qastcomp, $op {
    # Only need to do anything special if it's an object.
    my $sinkee_res := $qastcomp.as_mast($op[0]);
    if $sinkee_res.result_kind == $MVM_reg_obj {
        # Put computation of sinkee first.
        my @ops;
        push_ilist(@ops, $sinkee_res);

        # Check it's concrete try to find the sink method.
        my $sinkee_reg := $sinkee_res.result_reg;
        my $itmp := $*REGALLOC.fresh_i();
        my $meth := $*REGALLOC.fresh_o();
        my $done_lbl := MAST::Label.new();
        nqp::push(@ops, MAST::Op.new( :op('isconcrete'), $itmp, $sinkee_reg ));
        nqp::push(@ops, MAST::Op.new( :op('unless_i'), $itmp, $done_lbl ));
        nqp::push(@ops, MAST::Op.new( :op('tryfindmeth'), $meth, $sinkee_reg,
            MAST::SVal.new( :value('sink') )));
        nqp::push(@ops, MAST::Op.new( :op('isnull'), $itmp, $meth ));
        nqp::push(@ops, MAST::Op.new( :op('if_i'), $itmp, $done_lbl ));
        $*REGALLOC.release_register($itmp, $MVM_reg_int64);

        # Emit sink method call.
        nqp::push(@ops, MAST::Call.new(
            :target($meth), :flags([$Arg::obj]), $sinkee_reg
        ));
        $*REGALLOC.release_register($meth, $MVM_reg_obj);

        # Add end label, and we're done.
        nqp::push(@ops, $done_lbl);
        $*REGALLOC.release_register($sinkee_res.result_reg, $MVM_reg_obj);
        MAST::InstructionList.new(@ops, MAST::VOID, $MVM_reg_void);
    }
    else {
        $sinkee_res
    }
});

# Make some of them also available from NQP land, since we use them in the
# metamodel and bootstrap.
$ops.add_hll_op('nqp', 'p6bool', $p6bool);
$ops.add_hll_moarop_mapping('nqp', 'p6init', 'p6init');
$ops.add_hll_moarop_mapping('nqp', 'p6settypes', 'p6settypes', 0);
$ops.add_hll_moarop_mapping('nqp', 'p6var', 'p6var');
$ops.add_hll_moarop_mapping('nqp', 'p6reprname', 'p6reprname');
$ops.add_hll_moarop_mapping('nqp', 'p6inpre', 'p6inpre');
$ops.add_hll_moarop_mapping('nqp', 'p6capturelexwhere', 'p6capturelexwhere');
$ops.add_hll_moarop_mapping('nqp', 'p6invokeunder', 'p6invokeunder');

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
        nqp::push(@ops, MAST::ExtOp.new( :op($op), :cu($qastcomp.mast_compunit),
            $res_reg, $reg ));
        $*REGALLOC.release_register($reg, $kind);
        MAST::InstructionList.new(@ops, $res_reg, $MVM_reg_obj)
    }
}
$ops.add_hll_box('perl6', $MVM_reg_int64, boxer($MVM_reg_int64, 'p6box_i'));
$ops.add_hll_box('perl6', $MVM_reg_num64, boxer($MVM_reg_num64, 'p6box_n'));
$ops.add_hll_box('perl6', $MVM_reg_str, boxer($MVM_reg_str, 'p6box_s'));
$ops.add_hll_box('perl6', $MVM_reg_uint64, boxer($MVM_reg_uint64, 'p6box_u'));
QAST::MASTOperations.add_hll_unbox('perl6', $MVM_reg_int64, -> $qastcomp, $reg {
    my $il := nqp::list();
    my $res_reg := $*REGALLOC.fresh_register($MVM_reg_int64);
    nqp::push($il, MAST::Op.new( :op('decont_i'), $res_reg, $reg ));
    $*REGALLOC.release_register($reg, $MVM_reg_obj);
    MAST::InstructionList.new($il, $res_reg, $MVM_reg_int64)
});
QAST::MASTOperations.add_hll_unbox('perl6', $MVM_reg_num64, -> $qastcomp, $reg {
    my $il := nqp::list();
    my $res_reg := $*REGALLOC.fresh_register($MVM_reg_num64);
    nqp::push($il, MAST::Op.new( :op('decont_n'), $res_reg, $reg ));
    $*REGALLOC.release_register($reg, $MVM_reg_obj);
    MAST::InstructionList.new($il, $res_reg, $MVM_reg_num64)
});
QAST::MASTOperations.add_hll_unbox('perl6', $MVM_reg_str, -> $qastcomp, $reg {
    my $il := nqp::list();
    my $res_reg := $*REGALLOC.fresh_register($MVM_reg_str);
    nqp::push($il, MAST::Op.new( :op('decont_s'), $res_reg, $reg ));
    $*REGALLOC.release_register($reg, $MVM_reg_obj);
    MAST::InstructionList.new($il, $res_reg, $MVM_reg_str)
});
QAST::MASTOperations.add_hll_unbox('perl6', $MVM_reg_uint64, -> $qastcomp, $reg {
    my $il := nqp::list();
    my $res_reg := $*REGALLOC.fresh_register($MVM_reg_uint64);
    nqp::push($il, MAST::Op.new( :op('decont_u'), $res_reg, $reg ));
    $*REGALLOC.release_register($reg, $MVM_reg_obj);
    MAST::InstructionList.new($il, $res_reg, $MVM_reg_uint64)
});

# Signature binding related bits.
our $Binder;
$ops.add_hll_op('perl6', 'p6bindsig', :!inlinable, -> $qastcomp, $op {
    my @ops;
    my $isnull_result := $*REGALLOC.fresh_i();
    my $dont_return_lbl := MAST::Label.new();
    my $bind_res := $qastcomp.as_mast(
                QAST::Op.new(
                    :op('callmethod'), :name('bind_sig'),
                    QAST::WVal.new( :value($Binder) ),
                    QAST::Op.new( :op('savecapture') ),
                ), :want($MVM_reg_obj)
            );
    push_ilist(@ops, $bind_res);
    nqp::push(@ops, MAST::Op.new( :op('isnull'), $isnull_result, $bind_res.result_reg ));
    nqp::push(@ops, MAST::Op.new( :op('if_i'), $isnull_result, $dont_return_lbl ));
    nqp::push(@ops, MAST::Op.new( :op('return_o'), $bind_res.result_reg ));
    nqp::push(@ops, $dont_return_lbl);

    $*REGALLOC.release_register($bind_res.result_reg, $MVM_reg_obj);
    $*REGALLOC.release_register($isnull_result,       $MVM_reg_int64);
    MAST::InstructionList.new(@ops, MAST::VOID, $MVM_reg_void);
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
my $p6bindcaptosig := -> $qastcomp, $op {
    $qastcomp.as_mast(QAST::Op.new(
        :op('callmethod'), :name('bind_cap_to_sig'),
        QAST::WVal.new( :value($Binder) ),
        |@($op)
    ));
};
$ops.add_hll_op('perl6', 'p6bindcaptosig', :!inlinable, $p6bindcaptosig);
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
proto sub set_binder($b) { $Binder := $b; }
proto sub get_binder()   { $Binder }
$ops.add_hll_op('nqp', 'p6setbinder', -> $qastcomp, $op {
    $qastcomp.as_mast(QAST::Op.new(
        :op('call'),
        QAST::WVal.new( :value(nqp::getcodeobj(&set_binder)) ),
        |@($op)
    ));
});
$ops.add_hll_op('perl6', 'p6typecheckrv', -> $qastcomp, $op {
    if nqp::istype($op[1], QAST::WVal) {
        my $type := nqp::getcodeobj(&get_binder)().get_return_type($op[1].value);
        if nqp::isnull($type) || nqp::objprimspec(nqp::decont($type)) {
            $qastcomp.as_mast($op[0])
        }
        else {
            my $target_type;

            # emit a typecheck for the constraint type, the coercion will be
            # performed later on
            if $type.HOW.archetypes.coercive {
                $target_type := $type.HOW.target_type($type);
                $type := $type.HOW.constraint_type($type);
            }

            # if the type we want to check against is a definite type
            # like Int:D, we can generate much more efficient code by
            # splitting the check up into definedness check + type check
            # against the base type. This saves us from a call into the
            # metamodel for each check.
            my int $emit_definite_check := -1;
            if $type.HOW.archetypes.definite {
                $emit_definite_check := $type.HOW.definite($type);
                $type := $type.HOW.base_type($type);
            }

            my @ops;
            my $value_res   := $qastcomp.as_mast($op[0], :want($MVM_reg_obj));
            my $type_res    := $qastcomp.as_mast(QAST::WVal.new( :value($type) ), :want($MVM_reg_obj));
            my $niltype_res := $qastcomp.as_mast($op[2]);

            my $lbl_done    := MAST::Label.new();
            push_ilist(@ops, $value_res);
            push_ilist(@ops, $type_res);
            my $decont := $*REGALLOC.fresh_o();
            my $istype := $*REGALLOC.fresh_i();
            my $isdefinite;
            my $failure_o := $niltype_res.result_reg;

            unless $emit_definite_check == -1 {
                $isdefinite := $*REGALLOC.fresh_i();
            }

            nqp::push(@ops, MAST::Op.new( :op('decont'), $decont, $value_res.result_reg ));
            nqp::push(@ops, MAST::Op.new( :op('istype'), $istype, $decont, $type_res.result_reg ));

            if $emit_definite_check == -1 {
                nqp::push(@ops, MAST::Op.new( :op('if_i'), $istype, $lbl_done ));
            } else {
                my $lbl_failed_initial_typecheck    := MAST::Label.new();
                nqp::push(@ops, MAST::Op.new( :op('unless_i'), $istype, $lbl_failed_initial_typecheck ));

                nqp::push(@ops, MAST::Op.new( :op('isconcrete'), $isdefinite, $decont ));
                if $emit_definite_check == 0 {
                    nqp::push(@ops, MAST::Op.new( :op('unless_i'), $isdefinite, $lbl_done ));
                } else {
                    nqp::push(@ops, MAST::Op.new( :op('if_i'), $isdefinite, $lbl_done ));
                }
                nqp::push(@ops, $lbl_failed_initial_typecheck);
            }

            push_ilist(@ops, $niltype_res);
            nqp::push(@ops, MAST::Op.new( :op('istype'), $istype, $decont, $failure_o) );
            nqp::push(@ops, MAST::Op.new( :op('if_i'), $istype, $lbl_done ));
            $*REGALLOC.release_register($decont, $MVM_reg_obj);
            $*REGALLOC.release_register($istype, $MVM_reg_int64);
            $*REGALLOC.release_register($failure_o, $MVM_reg_obj);

            unless $emit_definite_check == -1 {
                $*REGALLOC.release_register($isdefinite, $MVM_reg_int64);
            }

            # Error generation.
            proto return_error($got, $wanted) {
                my %ex := nqp::gethllsym('perl6', 'P6EX');
                if nqp::isnull(%ex) || !nqp::existskey(%ex, 'X::TypeCheck::Return') {
                    nqp::die("Type check failed for return value; expected '" ~
                        $wanted.HOW.name($wanted) ~ "' but got '" ~
                        $got.HOW.name($got) ~ "'");
                }
                else {
                    nqp::atkey(%ex, 'X::TypeCheck::Return')($got, $wanted)
                }
            }
            my $err_rep := $qastcomp.as_mast(QAST::WVal.new( :value(nqp::getcodeobj(&return_error)) ));
            push_ilist(@ops, $err_rep);
            nqp::push(@ops, MAST::Call.new(
                :target($err_rep.result_reg),
                :flags($Arg::obj, $Arg::obj),
                $value_res.result_reg, $type_res.result_reg
            ));
            nqp::push(@ops, $lbl_done);
            $*REGALLOC.release_register($err_rep.result_reg, $MVM_reg_obj);

            unless $target_type =:= NQPMu || $target_type =:= $type {
                my $coerce_method := $target_type.HOW.name($target_type);
                my $lbl_no_error := MAST::Label.new();

                my $can := $*REGALLOC.fresh_i();
                nqp::push(@ops,
                    MAST::Op.new(:op('can'), $can, $value_res.result_reg, MAST::SVal.new(:value($coerce_method))));
                nqp::push(@ops,
                    MAST::Op.new(:op('if_i'), $can, $lbl_no_error));
                $*REGALLOC.release_register($can, $MVM_reg_int64);

                # inform the user that the coercion cannot be done
                my $errstr_reg := $*REGALLOC.fresh_s();
                my $dieret_reg := $*REGALLOC.fresh_o();
                my $errstr := "Unable to coerce the return value from "
                    ~ $type.HOW.name($type) ~ " to " ~ $target_type.HOW.name($type)
                    ~ "; no coercion method defined";
                nqp::push(@ops,
                    MAST::Op.new(:op('const_s'), $errstr_reg, MAST::SVal.new(:value($errstr))));
                nqp::push(@ops,
                    MAST::Op.new(:op('die'), $dieret_reg, $errstr_reg));
                $*REGALLOC.release_register($errstr_reg, $MVM_reg_str);
                $*REGALLOC.release_register($dieret_reg, $MVM_reg_obj);

                nqp::push(@ops,
                    $lbl_no_error);

                # perform the type conversion directly into the value_res register
                my $meth := $*REGALLOC.fresh_o();
                nqp::push(@ops,
                    MAST::Op.new(:op('findmeth'), $meth, $value_res.result_reg, MAST::SVal.new(:value($coerce_method))));
                nqp::push(@ops,
                    MAST::Call.new(:target($meth), :result($value_res.result_reg), :flags([$Arg::obj]), $value_res.result_reg));
                $*REGALLOC.release_register($meth, $MVM_reg_obj);
            }

            MAST::InstructionList.new(@ops, $value_res.result_reg, $MVM_reg_obj)
        }
    }
    else {
        nqp::die('p6dtypecheckrv expects a QAST::WVal as its second child');
    }
});
$ops.add_hll_op('perl6', 'p6decontrv', -> $qastcomp, $op {
    my $is_rw;
    if nqp::istype($op[0], QAST::WVal) {
        $is_rw := nqp::istrue($op[0].value.rw);
    }
    else {
        nqp::die('p6decontrv expects a QAST::WVal as its first child');
    }
    if $is_rw {
        $qastcomp.as_mast($op[1])
    }
    else {
        my $type := nqp::getcodeobj(&get_binder)().get_return_type($op[0].value);
        if !nqp::isnull($type) && nqp::objprimspec(nqp::decont($type)) -> int $prim {
            if    $prim == 1 { $qastcomp.as_mast($op[1], :want($MVM_reg_int64)) }
            elsif $prim == 2 { $qastcomp.as_mast($op[1], :want($MVM_reg_num64)) }
            else             { $qastcomp.as_mast($op[1], :want($MVM_reg_str)) }
        }
        else {
            my @ops;
            my $value_res := $qastcomp.as_mast($op[1], :want($MVM_reg_obj), :want-decont);
            push_ilist(@ops, $value_res);
            nqp::push(@ops, MAST::ExtOp.new( :op('p6decontrv'), :cu($qastcomp.mast_compunit),
                $value_res.result_reg, $value_res.result_reg ));
            MAST::InstructionList.new(@ops, $value_res.result_reg, $MVM_reg_obj)
        }
    }
});
$ops.add_hll_op('perl6', 'p6setautothreader', :inlinable, -> $qastcomp, $op {
    $qastcomp.as_mast(
        QAST::Op.new( :op('callmethod'), :name('set_autothreader'),
            QAST::WVal.new( :value($Binder) ),
            $op[0]), :want($MVM_reg_obj));
});
$ops.add_hll_op('perl6', 'p6configposbindfailover', :inlinable, -> $qastcomp, $op {
    $qastcomp.as_mast(
        QAST::Op.new( :op('callmethod'), :name('set_pos_bind_failover'),
            QAST::WVal.new( :value($Binder) ),
            $op[0], $op[1]), :want($MVM_reg_obj));
});

sub push_ilist(@dest, $src) {
    nqp::splice(@dest, $src.instructions, +@dest, 0);
}
