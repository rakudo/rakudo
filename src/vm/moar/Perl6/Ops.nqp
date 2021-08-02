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

# Dispatch op generators.
my %core_op_generators := MAST::Ops.WHO<%generators>;
my &op_dispatch_v := %core_op_generators<dispatch_v>;

# Register MoarVM extops.
use MASTNodes;
MAST::ExtOpRegistry.register_extop('p6init');
MAST::ExtOpRegistry.register_extop('p6settypes',
    $MVM_operand_obj   +| $MVM_operand_read_reg);
MAST::ExtOpRegistry.register_extop('p6reprname',
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
sub register_op_desugar($name, $desugar, :$inlinable = 1, :$compiler = 'Raku') is export {
    nqp::getcomp('QAST').operations.add_hll_op($compiler, $name, :$inlinable, -> $qastcomp, $op {
        $qastcomp.as_mast($desugar($op));
    });
}

# Raku opcode specific mappings.
my $ops := nqp::getcomp('QAST').operations;
my $STORE := QAST::SVal.new( :value('STORE') );
$ops.add_hll_op('Raku', 'p6store', -> $qastcomp, $op {
    my $cont_res  := $qastcomp.as_mast($op[0], :want($MVM_reg_obj));
    my $value_res := $qastcomp.as_mast($op[1], :want($MVM_reg_obj));

    my $iscont_reg  := $*REGALLOC.fresh_i();
    my $decont_reg  := $*REGALLOC.fresh_o();
    my $no_cont_lbl := MAST::Label.new();
    my $done_lbl    := MAST::Label.new();
    MAST::Op.new( :op('iscont'), $iscont_reg, $cont_res.result_reg );
    MAST::Op.new( :op('unless_i'), $iscont_reg, $no_cont_lbl );
    $*REGALLOC.release_register($iscont_reg, $MVM_reg_int64);
    MAST::Op.new( :op('decont'), $decont_reg, $value_res.result_reg );
    MAST::Op.new( :op('assign'), $cont_res.result_reg, $decont_reg );
    $*REGALLOC.release_register($decont_reg, $MVM_reg_obj);
    MAST::Op.new( :op('goto'), $done_lbl );

    $*MAST_FRAME.add-label($no_cont_lbl);
    my $store_mast := $qastcomp.as_mast($STORE, :want($MVM_reg_str));
    my uint $callsite_id := $*MAST_FRAME.callsites.get_callsite_id_from_args(
        [$op[0], $STORE, $op[0], $op[1]],
        [$cont_res, $store_mast, $cont_res, $value_res]);
    op_dispatch_v('lang-meth-call', $callsite_id,
        [$cont_res.result_reg, $store_mast.result_reg, $cont_res.result_reg, $value_res.result_reg]);
    $*MAST_FRAME.add-label($done_lbl);

    MAST::InstructionList.new($cont_res.result_reg, $MVM_reg_obj)
});
$ops.add_hll_op('Raku', 'p6definite', -> $qastcomp, $op {
    my $value_res := $qastcomp.as_mast($op[0], :want($MVM_reg_obj));
    my $tmp_reg := $*REGALLOC.fresh_i();
    my $res_reg := $*REGALLOC.fresh_o();
    MAST::Op.new( :op('decont'), $res_reg, $value_res.result_reg );
    MAST::Op.new( :op('isconcrete'), $tmp_reg, $res_reg );
    MAST::Op.new( :op('hllbool'), $res_reg, $tmp_reg );
    $*REGALLOC.release_register($value_res.result_reg, $MVM_reg_obj);
    $*REGALLOC.release_register($tmp_reg, $MVM_reg_int64);
    MAST::InstructionList.new($res_reg, $MVM_reg_obj)
});
$ops.add_hll_op('Raku', 'p6capturelex', -> $qastcomp, $op {
    my $code_res := $qastcomp.as_mast($op[0], :want($MVM_reg_obj));
    my uint $callsite_id := $*MAST_FRAME.callsites.get_callsite_id_from_args(
        [$op[0]], [$code_res]);
    op_dispatch_v('raku-capture-lex', $callsite_id, [$code_res.result_reg]);
    $code_res
});
$ops.add_hll_op('nqp', 'p6capturelexwhere', -> $qastcomp, $op {
    my $code_res := $qastcomp.as_mast($op[0], :want($MVM_reg_obj));
    my uint $callsite_id := $*MAST_FRAME.callsites.get_callsite_id_from_args(
        [$op[0]], [$code_res]);
    op_dispatch_v('raku-capture-lex-callers', $callsite_id, [$code_res.result_reg]);
    $code_res
});
$ops.add_hll_op('Raku', 'p6bindassert', -> $qastcomp, $op {
    my $temp := QAST::Node.unique('bind_value');
    $qastcomp.as_mast(QAST::Stmt.new(
        QAST::Op.new(
            :op('bind'),
            QAST::Var.new( :name($temp), :decl('var'), :scope('local') ),
            $op[0]
        ),
        QAST::Op.new(
            :op('dispatch'),
            QAST::SVal.new( :value('raku-bind-assert') ),
            QAST::Var.new( :name($temp), :scope('local') ),
            QAST::Op.new(
                :op('decont'),
                QAST::Var.new( :name($temp), :scope('local') )
            ),
            $op[1]
        )
    ))
});
$ops.add_hll_moarop_mapping('Raku', 'p6stateinit', 'p6stateinit');
$ops.add_hll_moarop_mapping('Raku', 'p6setpre', 'p6setpre');
$ops.add_hll_moarop_mapping('Raku', 'p6clearpre', 'p6clearpre');
$ops.add_hll_moarop_mapping('Raku', 'p6setfirstflag', 'p6setfirstflag');
$ops.add_hll_moarop_mapping('Raku', 'p6takefirstflag', 'p6takefirstflag');
$ops.add_hll_op('Raku', 'p6return', :!inlinable, -> $qastcomp, $op {
    my $value_res := $qastcomp.as_mast($op[0], :want($MVM_reg_obj));
    my $ex_reg := $*REGALLOC.fresh_o();
    MAST::Op.new( :op('exception'), $ex_reg );
    MAST::Op.new( :op('exreturnafterunwind'), $ex_reg );
    $*REGALLOC.release_register($ex_reg, $MVM_reg_obj);
    MAST::Op.new( :op('return_o'), $value_res.result_reg );
    MAST::InstructionList.new($value_res.result_reg, $MVM_reg_obj)
});
$ops.add_hll_moarop_mapping('Raku', 'p6getouterctx', 'p6getouterctx', :decont(0));
$ops.add_hll_moarop_mapping('Raku', 'p6captureouters', 'p6captureouters', 0);
$ops.add_hll_moarop_mapping('nqp', 'p6captureouters2', 'p6captureouters', 0);
$ops.add_hll_op('Raku', 'p6argvmarray', -> $qastcomp, $op {
    my $res_reg := $*REGALLOC.fresh_o();
    MAST::Op.new( :op('param_sp'), $res_reg,
        MAST::IVal.new( :value(0), :size(16) ));
    my $i_reg    := $*REGALLOC.fresh_i();
    my $n_reg    := $*REGALLOC.fresh_i();
    my $cmp_reg  := $*REGALLOC.fresh_i();
    my $tmp_reg  := $*REGALLOC.fresh_o();
    my $lbl_next := MAST::Label.new();
    my $lbl_done := MAST::Label.new();
    MAST::Op.new( :op('elems'), $n_reg, $res_reg );
    MAST::Op.new( :op('const_i64'), $i_reg, MAST::IVal.new( :value(0) ) );
    $*MAST_FRAME.add-label($lbl_next);
    MAST::Op.new( :op('lt_i'), $cmp_reg, $i_reg, $n_reg );
    MAST::Op.new( :op('unless_i'), $cmp_reg, $lbl_done );
    MAST::Op.new( :op('atpos_o'), $tmp_reg, $res_reg, $i_reg );
    MAST::Op.new( :op('hllize'), $tmp_reg, $tmp_reg );
    MAST::Op.new( :op('bindpos_o'), $res_reg, $i_reg, $tmp_reg );
    MAST::Op.new( :op('const_i64'), $cmp_reg, MAST::IVal.new( :value(1) ) );
    MAST::Op.new( :op('add_i'), $i_reg, $i_reg, $cmp_reg );
    MAST::Op.new( :op('goto'), $lbl_next );
    $*MAST_FRAME.add-label($lbl_done);
    $*REGALLOC.release_register($i_reg, $MVM_reg_int64);
    $*REGALLOC.release_register($n_reg, $MVM_reg_int64);
    $*REGALLOC.release_register($cmp_reg, $MVM_reg_int64);
    $*REGALLOC.release_register($tmp_reg, $MVM_reg_obj);
    MAST::InstructionList.new($res_reg, $MVM_reg_obj)
});
$ops.add_hll_op('Raku', 'p6bindattrinvres', -> $qastcomp, $op {
    my $inv_res := $qastcomp.as_mast($op[0], :want($MVM_reg_obj));

    my $ch_res := $qastcomp.as_mast(
        nqp::istype($op[1], QAST::WVal) && !nqp::isconcrete($op[1].value)
            ?? $op[1]
            !! QAST::Op.new( :op('decont'), $op[1] ),
        :want($MVM_reg_obj));

    my $val_res := $qastcomp.as_mast($op[3], :want($MVM_reg_obj));

    my $name := $op[2];
    $name := $name[2] if nqp::istype($name, QAST::Want) && $name[1] eq 'Ss';
    if nqp::istype($name, QAST::SVal) {
        MAST::Op.new( :op('bindattr_o'), $inv_res.result_reg,
            $ch_res.result_reg, MAST::SVal.new( :value($name.value) ), $val_res.result_reg,
            MAST::IVal.new( :value(-1) ));
    }
    else {
        my $nam_res := $qastcomp.as_mast($name, :want($MVM_reg_str));
        MAST::Op.new( :op('bindattrs_o'), $inv_res.result_reg,
            $ch_res.result_reg, $nam_res.result_reg, $val_res.result_reg);
        $*REGALLOC.release_register($nam_res.result_reg, $MVM_reg_str);
    }

    $*REGALLOC.release_register($ch_res.result_reg, $MVM_reg_obj);
    $*REGALLOC.release_register($val_res.result_reg, $MVM_reg_obj);
    MAST::InstructionList.new($inv_res.result_reg, $MVM_reg_obj)
});
$ops.add_hll_moarop_mapping('Raku', 'p6staticouter', 'p6staticouter');
$ops.add_hll_op('Raku', 'p6invokehandler', -> $qastcomp, $op {
    $qastcomp.as_mast(QAST::Op.new( :op('call'), $op[0], $op[1] ));
});
$ops.add_hll_op('Raku', 'p6invokeflat', -> $qastcomp, $op {
    $op[1].flat(1);
    $qastcomp.as_mast(QAST::Op.new( :op('call'), $op[0], $op[1]));
});
$ops.add_hll_op('Raku', 'p6sink', -> $qastcomp, $op {
    # Only need to do anything special if it's an object.
    my $sinkee_res := $qastcomp.as_mast($op[0]);
    if $sinkee_res.result_kind == $MVM_reg_obj {
        # Send it along to the dispatcher. Note that we do *not* decont it, as
        # we don't want to sink things that are in containers.
        my uint $callsite_id := $*MAST_FRAME.callsites.get_callsite_id_from_args(
            [$op[0]], [$sinkee_res]);
        op_dispatch_v('raku-sink', $callsite_id, [$sinkee_res.result_reg]);
        $*REGALLOC.release_register($sinkee_res.result_reg, $MVM_reg_obj);
        MAST::InstructionList.new(MAST::VOID, $MVM_reg_void);
    }
    else {
        $sinkee_res
    }
});

# Make some of them also available from NQP land, since we use them in the
# metamodel and bootstrap.
$ops.add_hll_moarop_mapping('nqp', 'p6init', 'p6init');
$ops.add_hll_moarop_mapping('nqp', 'p6settypes', 'p6settypes', 0);
$ops.add_hll_moarop_mapping('nqp', 'p6inpre', 'p6inpre');
$ops.add_hll_moarop_mapping('nqp', 'p6invokeunder', 'p6invokeunder');

# Override defor to call defined method.
$ops.add_hll_op('Raku', 'defor', -> $qastcomp, $op {
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
sub boxer($kind, $box_op, $type_op) {
    -> $qastcomp, $reg {
        my $res_reg := $*REGALLOC.fresh_register($MVM_reg_obj);
        MAST::Op.new( :op($type_op), $res_reg );
        MAST::Op.new( :op($box_op), $res_reg, $reg, $res_reg );
        $*REGALLOC.release_register($reg, $kind);
        MAST::InstructionList.new($res_reg, $MVM_reg_obj)
    }
}
$ops.add_hll_box('Raku', $MVM_reg_int64, boxer($MVM_reg_int64, 'box_i', 'hllboxtype_i'));
$ops.add_hll_box('Raku', $MVM_reg_num64, boxer($MVM_reg_num64, 'box_n', 'hllboxtype_n'));
$ops.add_hll_box('Raku', $MVM_reg_str, boxer($MVM_reg_str, 'box_s', 'hllboxtype_s'));
$ops.add_hll_box('Raku', $MVM_reg_uint64, boxer($MVM_reg_uint64, 'box_u', 'hllboxtype_i'));
QAST::MASTOperations.add_hll_unbox('Raku', $MVM_reg_int64, -> $qastcomp, $reg {
    my $res_reg := $*REGALLOC.fresh_register($MVM_reg_int64);
    MAST::Op.new( :op('decont_i'), $res_reg, $reg );
    $*REGALLOC.release_register($reg, $MVM_reg_obj);
    MAST::InstructionList.new($res_reg, $MVM_reg_int64)
});
QAST::MASTOperations.add_hll_unbox('Raku', $MVM_reg_num64, -> $qastcomp, $reg {
    my $res_reg := $*REGALLOC.fresh_register($MVM_reg_num64);
    MAST::Op.new( :op('decont_n'), $res_reg, $reg );
    $*REGALLOC.release_register($reg, $MVM_reg_obj);
    MAST::InstructionList.new($res_reg, $MVM_reg_num64)
});
QAST::MASTOperations.add_hll_unbox('Raku', $MVM_reg_str, -> $qastcomp, $reg {
    my $res_reg := $*REGALLOC.fresh_register($MVM_reg_str);
    MAST::Op.new( :op('decont_s'), $res_reg, $reg );
    $*REGALLOC.release_register($reg, $MVM_reg_obj);
    MAST::InstructionList.new($res_reg, $MVM_reg_str)
});
QAST::MASTOperations.add_hll_unbox('Raku', $MVM_reg_uint64, -> $qastcomp, $reg {
    my $res_reg := $*REGALLOC.fresh_register($MVM_reg_uint64);
    MAST::Op.new( :op('decont_u'), $res_reg, $reg );
    $*REGALLOC.release_register($reg, $MVM_reg_obj);
    MAST::InstructionList.new($res_reg, $MVM_reg_uint64)
});

# Signature binding related bits.
our $Binder;
$ops.add_hll_op('Raku', 'p6bindsig', :!inlinable, -> $qastcomp, $op {
    my $isnull_result := $*REGALLOC.fresh_i();
    my $dont_return_lbl := MAST::Label.new();
    my $bind_res := $qastcomp.as_mast(
                QAST::Op.new(
                    :op('callmethod'), :name('bind_sig'),
                    QAST::WVal.new( :value($Binder) ),
                    QAST::Op.new( :op('savecapture') ),
                ), :want($MVM_reg_obj)
            );
    MAST::Op.new( :op('isnull'), $isnull_result, $bind_res.result_reg );
    MAST::Op.new( :op('if_i'), $isnull_result, $dont_return_lbl );
    MAST::Op.new( :op('return_o'), $bind_res.result_reg );
    $*MAST_FRAME.add-label($dont_return_lbl);

    $*REGALLOC.release_register($bind_res.result_reg, $MVM_reg_obj);
    $*REGALLOC.release_register($isnull_result,       $MVM_reg_int64);
    MAST::InstructionList.new(MAST::VOID, $MVM_reg_void);
});
$ops.add_hll_op('Raku', 'p6trybindsig', :!inlinable, -> $qastcomp, $op {
    $qastcomp.as_mast(QAST::Op.new(
        :op('callmethod'), :name('try_bind_sig'), :returns(int),
        QAST::WVal.new( :value($Binder) ),
        QAST::Op.new( :op('savecapture') ),
    ), :want($MVM_reg_obj))
});
my $is_bindable := -> $qastcomp, $op {
    $qastcomp.as_mast(QAST::Op.new(
        :op('callmethod'), :name('is_bindable'),
        QAST::WVal.new( :value($Binder) ),
        |@($op)
    ));
};
$ops.add_hll_op('nqp', 'p6isbindable', :!inlinable, $is_bindable);
$ops.add_hll_op('Raku', 'p6isbindable', :!inlinable, $is_bindable);
my $p6bindcaptosig := -> $qastcomp, $op {
    $qastcomp.as_mast(QAST::Op.new(
        :op('callmethod'), :name('bind_cap_to_sig'),
        QAST::WVal.new( :value($Binder) ),
        |@($op)
    ));
};
$ops.add_hll_op('Raku', 'p6bindcaptosig', :!inlinable, $p6bindcaptosig);
proto sub trial_bind(*@args) {
    $Binder.trial_bind(|@args);
}
my $trial_bind := -> $qastcomp, $op {
    $qastcomp.as_mast(QAST::Op.new(
        :op('call'),
        QAST::WVal.new( :value(&trial_bind) ),
        |@($op)
    ));
};
$ops.add_hll_op('nqp', 'p6trialbind', :!inlinable, $trial_bind);
$ops.add_hll_op('Raku', 'p6trialbind', :!inlinable, $trial_bind);
proto sub set_binder($b) { $Binder := $b; }
proto sub get_binder()   { $Binder }
$ops.add_hll_op('nqp', 'p6setbinder', -> $qastcomp, $op {
    $qastcomp.as_mast(QAST::Op.new(
        :op('call'),
        QAST::WVal.new( :value(&set_binder) ),
        |@($op)
    ));
});
$ops.add_hll_op('Raku', 'p6box', -> $qastcomp, $op {
    $qastcomp.as_mast($op[0], :want($MVM_reg_obj))
});
$ops.add_hll_op('Raku', 'p6typecheckrv', -> $qastcomp, $op {
    if nqp::istype($op[1], QAST::WVal) {
        my $type := &get_binder().get_return_type($op[1].value);
        if nqp::isnull($type) || nqp::objprimspec(nqp::decont($type)) {
            $qastcomp.as_mast($op[0])
        }
        else {
            $qastcomp.as_mast(QAST::Op.new(
                :op('dispatch'),
                QAST::SVal.new( :value('raku-rv-typecheck') ),
                QAST::Op.new( :op('p6box'), $op[0] ),
                QAST::WVal.new( :value($type) )
            ))
        }
    }
    else {
        nqp::die('p6dtypecheckrv expects a QAST::WVal as its second child');
    }
});
sub decontrv_op($version) {
    -> $qastcomp, $op {
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
            my $type := &get_binder().get_return_type($op[0].value);
            if !nqp::isnull($type) && nqp::objprimspec(nqp::decont($type)) -> int $prim {
                if    $prim == 1 { $qastcomp.as_mast($op[1], :want($MVM_reg_int64)) }
                elsif $prim == 2 { $qastcomp.as_mast($op[1], :want($MVM_reg_num64)) }
                else             { $qastcomp.as_mast($op[1], :want($MVM_reg_str)) }
            }
            else {
                $qastcomp.as_mast(QAST::Op.new( :op("p6decontrv_internal"), $op[1], $version ));
            }
        }
    }
}
$ops.add_hll_op('Raku', 'p6decontrv', decontrv_op(''));
$ops.add_hll_op('Raku', 'p6decontrv_6c', decontrv_op('6c'));

$ops.add_hll_op('Raku', 'p6setautothreader', :inlinable, -> $qastcomp, $op {
    $qastcomp.as_mast(
        QAST::Op.new( :op('callmethod'), :name('set_autothreader'),
            QAST::WVal.new( :value($Binder) ),
            $op[0]), :want($MVM_reg_obj));
});
$ops.add_hll_op('Raku', 'p6configposbindfailover', :inlinable, -> $qastcomp, $op {
    $qastcomp.as_mast(
        QAST::Op.new( :op('callmethod'), :name('set_pos_bind_failover'),
            QAST::WVal.new( :value($Binder) ),
            $op[0], $op[1]), :want($MVM_reg_obj));
});

# vim: expandtab sw=4
