my $ops := nqp::getcomp('QAST').operations;

# Perl 6 opcode specific mappings.
$ops.add_hll_pirop_mapping('perl6', 'p6box_i', 'perl6_box_int', 'Pi', :inlinable(1));
$ops.add_hll_pirop_mapping('perl6', 'p6box_n', 'perl6_box_num', 'Pn', :inlinable(1));
$ops.add_hll_pirop_mapping('perl6', 'p6box_s', 'perl6_box_str', 'Ps', :inlinable(1));
$ops.add_hll_pirop_mapping('perl6', 'p6bigint', 'perl6_box_bigint', 'Pn', :inlinable(1));
$ops.add_hll_pirop_mapping('perl6', 'p6parcel', 'perl6_parcel_from_rpa', 'PPP', :inlinable(1));
$ops.add_hll_pirop_mapping('perl6', 'p6listiter', 'perl6_iter_from_rpa', 'PPP', :inlinable(1));
$ops.add_hll_pirop_mapping('perl6', 'p6list', 'perl6_list_from_rpa', 'PPPP', :inlinable(1));
$ops.add_hll_pirop_mapping('perl6', 'p6listitems', 'perl6_listitems', 'PP', :inlinable(1));
$ops.add_hll_pirop_mapping('perl6', 'p6decont', 'perl6_decontainerize', 'PP', :inlinable(1));
$ops.add_hll_pirop_mapping('perl6', 'p6recont_ro', 'perl6_recontainerize_to_ro', 'PP', :inlinable(1));
$ops.add_hll_pirop_mapping('perl6', 'p6store', 'perl6_container_store', '0PP', :inlinable(1));
$ops.add_hll_pirop_mapping('perl6', 'p6type', 'perl6ize_type', 'PP', :inlinable(1));
$ops.add_hll_pirop_mapping('perl6', 'p6takedisp', 'perl6_take_dispatcher', 'v');
$ops.add_hll_pirop_mapping('perl6', 'p6var', 'perl6_var', 'PP', :inlinable(1));
$ops.add_hll_pirop_mapping('perl6', 'p6reprname', 'perl6_repr_name', 'PP', :inlinable(1));
$ops.add_hll_pirop_mapping('perl6', 'p6definite', 'perl6_definite', 'PP', :inlinable(1));
$ops.add_hll_pirop_mapping('perl6', 'p6multidispatch', 'perl6_enter_multi_dispatch_from_onlystar_block', 'P');
$ops.add_hll_pirop_mapping('perl6', 'p6multidispatchlex', 'perl6_enter_multi_dispatch_in_lexical_context', 'P');
$ops.add_hll_pirop_mapping('perl6', 'p6bindsig', 'bind_signature', 'v');
$ops.add_hll_pirop_mapping('perl6', 'p6isbindable', 'perl6_is_sig_bindable', 'IPP');
$ops.add_hll_pirop_mapping('perl6', 'p6bindcaptosig', 'perl6_bind_sig_to_cap', '0PP');
$ops.add_hll_pirop_mapping('perl6', 'p6typecheckrv', 'perl6_type_check_return_value', '0PP', :inlinable(1));
$ops.add_hll_pirop_mapping('perl6', 'p6decontrv', 'perl6_decontainerize_return_value', 'PP', :inlinable(1));
$ops.add_hll_pirop_mapping('perl6', 'p6capturelex', 'perl6_capture_lex', '0P');
$ops.add_hll_pirop_mapping('perl6', 'p6vmcodetoobj', 'perl6_code_object_from_parrot_sub', 'PP');
$ops.add_hll_pirop_mapping('perl6', 'p6bindassert', 'perl6_assert_bind_ok', '0PP');
$ops.add_hll_pirop_mapping('perl6', 'p6getpackage', 'perl6_get_package_through_who', 'PPs');
$ops.add_hll_pirop_mapping('perl6', 'p6stateinit', 'perl6_state_needs_init', 'I');
$ops.add_hll_pirop_mapping('perl6', 'p6setpre', 'perl6_set_checking_pre', 'v');
$ops.add_hll_pirop_mapping('perl6', 'p6clearpre', 'perl6_clear_checking_pre', 'v');
$ops.add_hll_pirop_mapping('perl6', 'p6setfirstflag', 'perl6_set_block_first_flag', '0P');
$ops.add_hll_pirop_mapping('perl6', 'p6takefirstflag', 'perl6_take_block_first_flag', 'I');
$ops.add_hll_pirop_mapping('perl6', 'p6return', 'perl6_returncc', '0P');
$ops.add_hll_pirop_mapping('perl6', 'p6routinereturn', 'perl6_return_from_routine', '0P');
$ops.add_hll_pirop_mapping('perl6', 'p6assoccode', 'perl6_associate_sub_code_object', 'vPP');
$ops.add_hll_pirop_mapping('perl6', 'p6getouterctx', 'perl6_get_outer_ctx', 'PP');
$ops.add_hll_pirop_mapping('perl6', 'p6mdthunk', 'perl6_multi_dispatch_thunk', 'PP');
$ops.add_hll_pirop_mapping('perl6', 'p6mdcandthunk', 'perl6_multi_dispatch_cand_thunk', 'PPi');
$ops.add_hll_pirop_mapping('perl6', 'tclc', 'titlecase', 'Ss', :inlinable(1));
$ops.add_hll_op('perl6', 'p6getcallsig', -> $qastcomp, $op {
    my $reg := $*REGALLOC.fresh_p();
    my $ops := $qastcomp.post_new('Ops', :result($reg));
    $ops.push_pirop('set', $reg, 'CALL_SIG');
    $ops
});
$ops.add_hll_op('perl6', 'p6bool', :inlinable(1), -> $qastcomp, $op {
    my $cpost := $qastcomp.as_post($op[0]);
    my $reg := $*REGALLOC.fresh_p();
    my $ops := $qastcomp.post_new('Ops', :result($reg));
    $ops.push($cpost);
    if nqp::lc($qastcomp.infer_type($cpost.result)) eq 'i' {
        $ops.push_pirop('perl6_booleanize', $reg, $cpost);
    }
    else {
        my $reg_i := $*REGALLOC.fresh_i();
        $ops.push_pirop('istrue', $reg_i, $cpost);
        $ops.push_pirop('perl6_booleanize', $reg, $reg_i);
    }
    $ops
});

# Override defor to avoid v-table call.
$ops.add_hll_op('perl6', 'defor', :inlinable(1), -> $qastcomp, $op {
    if +$op.list != 2 {
        nqp::die("Operation 'defor' needs 2 operands");
    }
    my $ops := PIRT::Ops.new();
    my $lbl := PIRT::Label.new(:name('defor'));
    my $dreg := $*REGALLOC.fresh_p();
    my $rreg := $*REGALLOC.fresh_p();
    my $test := $qastcomp.coerce($qastcomp.as_post($op[0]), 'P');
    my $then := $qastcomp.coerce($qastcomp.as_post($op[1]), 'P');
    $ops.push($test);
    $ops.push_pirop('set', $rreg, $test);
    $ops.push_pirop('callmethod', "'defined'", $rreg, :result($dreg));
    $ops.push_pirop('if', $dreg, $lbl);
    $ops.push($then);
    $ops.push_pirop('set', $rreg, $then);
    $ops.push($lbl);
    $ops.result($rreg);
    $ops
});

# Boxing and unboxing configuration.
QAST::Operations.add_hll_box('perl6', 'i', -> $qastcomp, $post {
    my $reg := $*REGALLOC.fresh_p();
    my $ops := $qastcomp.post_new('Ops');
    $ops.push($post);
    $ops.push_pirop('perl6_box_int', $reg, $post);
    $ops.result($reg);
    $ops
});
QAST::Operations.add_hll_box('perl6', 'n', -> $qastcomp, $post {
    my $reg := $*REGALLOC.fresh_p();
    my $ops := $qastcomp.post_new('Ops');
    $ops.push($post);
    $ops.push_pirop('perl6_box_num', $reg, $post);
    $ops.result($reg);
    $ops
});
QAST::Operations.add_hll_box('perl6', 's', -> $qastcomp, $post {
    my $reg := $*REGALLOC.fresh_p();
    my $ops := $qastcomp.post_new('Ops');
    $ops.push($post);
    $ops.push_pirop('perl6_box_str', $reg, $post);
    $ops.result($reg);
    $ops
});
QAST::Operations.add_hll_unbox('perl6', 'i', -> $qastcomp, $post {
    my $reg := $*REGALLOC.fresh_i();
    my $ops := $qastcomp.post_new('Ops');
    $ops.push($post);
    $ops.push_pirop('repr_unbox_int', $reg, $post);
    $ops.result($reg);
    $ops
});
QAST::Operations.add_hll_unbox('perl6', 'n', -> $qastcomp, $post {
    my $reg := $*REGALLOC.fresh_n();
    my $ops := $qastcomp.post_new('Ops');
    $ops.push($post);
    $ops.push_pirop('set', $reg, $post);
    $ops.result($reg);
    $ops
});
QAST::Operations.add_hll_unbox('perl6', 's', -> $qastcomp, $post {
    my $reg := $*REGALLOC.fresh_s();
    my $ops := $qastcomp.post_new('Ops');
    $ops.push($post);
    $ops.push_pirop('set', $reg, $post);
    $ops.result($reg);
    $ops
});
QAST::Compiler.force_return_boxing_for_hll('perl6');
