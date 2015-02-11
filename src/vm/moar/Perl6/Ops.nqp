# Operand read/write/literal flags.
my $MVM_operand_literal     := 0;
my $MVM_operand_read_reg    := 1;
my $MVM_operand_write_reg   := 2;
my $MVM_operand_read_lex    := 3;
my $MVM_operand_write_lex   := 4;
my $MVM_operand_rw_mask     := 7;

# Register data types.
my $MVM_reg_void            := 0;
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
MAST::ExtOpRegistry.register_extop('p6reprname',
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
MAST::ExtOpRegistry.register_extop('p6routinereturn',
    $MVM_operand_obj   +| $MVM_operand_write_reg,
    $MVM_operand_obj   +| $MVM_operand_read_reg);
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
    nqp::push(@ops, MAST::Op.new( :op('decont'), $value_res.result_reg, $value_res.result_reg ));
    my $tmp_reg := $*REGALLOC.fresh_i();
    my $res_reg := $*REGALLOC.fresh_o();
    nqp::push(@ops, MAST::Op.new( :op('isconcrete'), $tmp_reg, $value_res.result_reg ));
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
$ops.add_hll_moarop_mapping('perl6', 'p6routinereturn', 'p6routinereturn');
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
$ops.add_hll_moarop_mapping('perl6', 'p6shiftpush', 'p6shiftpush');
$ops.add_hll_moarop_mapping('perl6', 'p6arrfindtypes', 'p6arrfindtypes');
$ops.add_hll_moarop_mapping('perl6', 'p6decodelocaltime', 'p6decodelocaltime');
#$ops.map_classlib_hll_op('perl6', 'tclc', $TYPE_P6OPS, 'tclc', [$RT_STR], $RT_STR, :tc);
proto sub p6sort(@input_data, &comparator) {
    # using the "bottom-up" mergesort implementation as shown in the english
    # wikipedia article

    # for some extra (hopefully measurable) benefit, we plop our indices
    # into native integer lists before going on.

    my @b_list := nqp::list_i();
    my @data := nqp::list_i();
    nqp::setelems(@b_list, +@input_data);
    nqp::setelems(@b_list, 0);
    nqp::setelems(@data, +@input_data);
    nqp::setelems(@data, 0);

    my int $copy_idx;
    for @input_data {
        nqp::bindpos_i(@data, $copy_idx++, $_);
    }

    my int $n := +@data;

    my int $run_w := 1; # the width of each of the runs we are looking at
    while $run_w < $n {
        my int $i;

        while $i < $n {
            my int $left  := $i;
            my int $right := $i + $run_w;
            $right        := $n if $n < $right;

            my int $end   := $i + 2 * $run_w;
            $end          := $n if $n < $end;

            my int $i0    := $left;
            my int $i1    := $right;
            my int $j     := $i0;

            while $j < $end {
                if $i0 < $right && ($i1 >= $end || -1 == comparator(nqp::atpos_i(@data, $i0), nqp::atpos_i(@data, $i1))) {
                    nqp::bindpos_i(@b_list, $j, nqp::atpos_i(@data, $i0));
                    $i0 := $i0 + 1;
                } else {
                    nqp::bindpos_i(@b_list, $j, nqp::atpos_i(@data, $i1));
                    $i1 := $i1 + 1;
                }
                $j := $j + 1;
            }

            $i := $i + 2 * $run_w;
        }

        {
            my $t := @b_list;
            @b_list := @data;
            @data := $t;
        }

        $run_w := $run_w * 2;
    }

    $copy_idx := 0;
    for @data {
        nqp::bindpos(@input_data, $copy_idx++, $_);
    }
}
my $p6sort := -> $qastcomp, $op {
    $qastcomp.as_mast(QAST::Op.new(
        :op('call'),
        QAST::WVal.new( :value(nqp::getcodeobj(&p6sort)) ),
        $op[0],
        $op[1]
    ));
};
$ops.add_hll_op('perl6', 'p6sort', :!inlinable, $p6sort);
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
$ops.add_hll_moarop_mapping('perl6', 'p6scalarfromdesc', 'p6scalarfromdesc');
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

        # Check it's concrete and can sink.
        my $sinkee_reg := $sinkee_res.result_reg;
        my $itmp       := $*REGALLOC.fresh_i();
        my $done_lbl   := MAST::Label.new();
        nqp::push(@ops, MAST::Op.new( :op('isconcrete'), $itmp, $sinkee_reg ));
        nqp::push(@ops, MAST::Op.new( :op('unless_i'), $itmp, $done_lbl ));
        nqp::push(@ops, MAST::Op.new( :op('can'), $itmp, $sinkee_reg,
            MAST::SVal.new( :value('sink') )));
        nqp::push(@ops, MAST::Op.new( :op('unless_i'), $itmp, $done_lbl ));
        $*REGALLOC.release_register($itmp, $MVM_reg_int64);

        # Emit sink method call.
        my $meth := $*REGALLOC.fresh_o();
        $*REGALLOC.release_register($meth, $MVM_reg_obj);
        nqp::push(@ops, MAST::Op.new( :op('findmeth'), $meth, $sinkee_reg,
            MAST::SVal.new( :value('sink') )));
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
$ops.add_hll_moarop_mapping('nqp', 'p6parcel', 'p6parcel');
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
            my @ops;
            my $value_res := $qastcomp.as_mast($op[0], :want($MVM_reg_obj));
            my $type_res  := $qastcomp.as_mast(QAST::WVal.new( :value($type) ), :want($MVM_reg_obj));
            my $lbl_done  := MAST::Label.new();
            push_ilist(@ops, $value_res);
            push_ilist(@ops, $type_res);
            my $decont := $*REGALLOC.fresh_o();
            my $istype := $*REGALLOC.fresh_i();
            nqp::push(@ops, MAST::Op.new( :op('decont'), $decont, $value_res.result_reg ));
            nqp::push(@ops, MAST::Op.new( :op('istype'), $istype, $decont, $type_res.result_reg ));
            nqp::push(@ops, MAST::Op.new( :op('if_i'), $istype, $lbl_done ));
            $*REGALLOC.release_register($decont, $MVM_reg_obj);
            $*REGALLOC.release_register($istype, $MVM_reg_int64);

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
            my $value_res := $qastcomp.as_mast($op[1], :want($MVM_reg_obj));
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

sub push_ilist(@dest, $src) {
    nqp::splice(@dest, $src.instructions, +@dest, 0);
}
