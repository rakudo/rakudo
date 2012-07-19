my $ops := pir::compreg__Ps('QAST').operations;

# Perl 6 opcode specific mappings.
$ops.add_hll_pirop_mapping('perl6', 'p6box_i', 'perl6_box_int', 'Pi');
$ops.add_hll_pirop_mapping('perl6', 'p6box_n', 'perl6_box_num', 'Pn');
$ops.add_hll_pirop_mapping('perl6', 'p6box_s', 'perl6_box_str', 'Ps');
$ops.add_hll_pirop_mapping('perl6', 'p6bool', 'perl6_booleanize', 'Pi');
$ops.add_hll_pirop_mapping('perl6', 'p6bigint', 'perl6_box_bigint', 'Pn');
$ops.add_hll_pirop_mapping('perl6', 'p6parcel', 'perl6_parcel_from_rpa', 'PPP');
$ops.add_hll_pirop_mapping('perl6', 'p6listiter', 'perl6_iter_from_rpa', 'PPP');
$ops.add_hll_pirop_mapping('perl6', 'p6list', 'perl6_list_from_rpa', 'PPPP');
$ops.add_hll_pirop_mapping('perl6', 'p6decont', 'perl6_decontainerize', 'PP');
$ops.add_hll_pirop_mapping('perl6', 'p6recont_ro', 'perl6_recontainerize_to_ro', 'PP');
$ops.add_hll_pirop_mapping('perl6', 'attrinited', 'repr_is_attr_initialized', 'IPPs');
$ops.add_hll_pirop_mapping('perl6', 'callerid', 'perl6_callerid', 'I');
$ops.add_hll_pirop_mapping('perl6', 'ishash', 'perl6_is_hash', 'IP');
$ops.add_hll_pirop_mapping('perl6', 'lcm_i', 'lcm', 'Iii');
$ops.add_hll_pirop_mapping('perl6', 'gcd_i', 'gcd', 'Iii');
$ops.add_hll_pirop_mapping('perl6', 'sqrt_n', 'sqrt', 'NN');
$ops.add_hll_pirop_mapping('perl6', 'create', 'repr_instance_of', 'PP');
$ops.add_hll_pirop_mapping('perl6', 'exit', 'exit', 'vi');
$ops.add_hll_pirop_mapping('perl6', 'p6store', 'perl6_container_store', '0PP');
$ops.add_hll_pirop_mapping('perl6', 'p6type', 'perl6ize_type', 'PP');
$ops.add_hll_pirop_mapping('perl6', 'p6takedisp', 'perl6_take_dispatcher', 'v');
$ops.add_hll_pirop_mapping('perl6', 'p6var', 'perl6_var', 'PP');
$ops.add_hll_pirop_mapping('perl6', 'p6reprname', 'perl6_repr_name', 'PP');
$ops.add_hll_pirop_mapping('perl6', 'p6definite', 'perl6_definite', 'PP');
$ops.add_hll_pirop_mapping('perl6', 'p6multidispatch', 'perl6_enter_multi_dispatch_from_onlystar_block', 'P');
$ops.add_hll_pirop_mapping('perl6', 'p6bindsig', 'bind_signature', 'v');
$ops.add_hll_pirop_mapping('perl6', 'p6typecheckrv', 'perl6_type_check_return_value', '0P');
$ops.add_hll_pirop_mapping('perl6', 'p6decontrv', 'perl6_decontainerize_return_value', 'PP');
$ops.add_hll_pirop_mapping('perl6', 'p6capturelex', 'perl6_capture_lex', '0P');
$ops.add_hll_pirop_mapping('perl6', 'p6vmcodetoobj', 'perl6_code_object_from_parrot_sub', 'PP');
$ops.add_hll_pirop_mapping('perl6', 'p6bindassert', 'perl6_assert_bind_ok', '0PP');
$ops.add_hll_pirop_mapping('perl6', 'p6getpackage', 'perl6_get_package_through_who', 'PPs');
$ops.add_hll_pirop_mapping('perl6', 'p6stateinit', 'perl6_state_needs_init', 'I');
$ops.add_hll_pirop_mapping('perl6', 'p6setpre', 'perl6_set_checking_pre', 'v');
$ops.add_hll_pirop_mapping('perl6', 'p6clearpre', 'perl6_clear_checking_pre', 'v');
$ops.add_hll_pirop_mapping('perl6', 'p6takefirstflag', 'perl6_take_block_first_flag', 'I');
$ops.add_hll_pirop_mapping('perl6', 'p6return', 'perl6_returncc', '0P');
$ops.add_hll_pirop_mapping('perl6', 'p6assoccode', 'perl6_associate_sub_code_object', 'vPP');
$ops.add_hll_pirop_mapping('perl6', 'p6getouterctx', 'perl6_get_outer_ctx', 'PP');
$ops.add_hll_op('perl6', 'p6getcallsig', -> $qastcomp, $op {
    my $reg := $*REGALLOC.fresh_p();
    my $ops := $qastcomp.post_new('Ops', :result($reg));
    $ops.push_pirop('set', $reg, 'CALL_SIG');
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
    $ops.push_pirop('set', $reg, $post);
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
