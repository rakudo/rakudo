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
$ops.add_hll_pirop_mapping('perl6', 'p6recont_ro', 'perl6_recontainerize_to_ro', 'PP', :inlinable(1));
$ops.add_hll_pirop_mapping('perl6', 'p6store', 'perl6_container_store', '0PP', :inlinable(1));
$ops.add_hll_pirop_mapping('perl6', 'p6var', 'perl6_var', 'PP', :inlinable(1));
$ops.add_hll_pirop_mapping('perl6', 'p6reprname', 'perl6_repr_name', 'PP', :inlinable(1));
$ops.add_hll_pirop_mapping('perl6', 'p6definite', 'perl6_definite', 'PP', :inlinable(1));
$ops.add_hll_pirop_mapping('perl6', 'p6bindsig', 'bind_signature', 'v');
$ops.add_hll_pirop_mapping('perl6', 'p6isbindable', 'perl6_is_sig_bindable', 'IPP');
$ops.add_hll_pirop_mapping('perl6', 'p6bindcaptosig', 'perl6_bind_sig_to_cap', '0PP');
$ops.add_hll_pirop_mapping('perl6', 'p6trialbind', 'perl6_trial_bind_ct', 'IPPP');
$ops.add_hll_pirop_mapping('perl6', 'p6typecheckrv', 'perl6_type_check_return_value', '0PP', :inlinable(1));
$ops.add_hll_pirop_mapping('perl6', 'p6decontrv', 'perl6_decontainerize_return_value', 'PPP', :inlinable(1));
$ops.add_hll_pirop_mapping('perl6', 'p6capturelex', 'perl6_capture_lex', '0P');
$ops.add_hll_pirop_mapping('perl6', 'p6bindassert', 'perl6_assert_bind_ok', '0PP');
$ops.add_hll_pirop_mapping('perl6', 'p6stateinit', 'perl6_state_needs_init', 'I');
$ops.add_hll_pirop_mapping('perl6', 'p6setpre', 'perl6_set_checking_pre', 'v');
$ops.add_hll_pirop_mapping('perl6', 'p6clearpre', 'perl6_clear_checking_pre', 'v');
$ops.add_hll_pirop_mapping('perl6', 'p6setfirstflag', 'perl6_set_block_first_flag', '0P');
$ops.add_hll_pirop_mapping('perl6', 'p6takefirstflag', 'perl6_take_block_first_flag', 'I');
$ops.add_hll_pirop_mapping('perl6', 'p6return', 'perl6_returncc', '0P');
$ops.add_hll_pirop_mapping('perl6', 'p6routinereturn', 'perl6_return_from_routine', '0P');
$ops.add_hll_pirop_mapping('perl6', 'p6getouterctx', 'perl6_get_outer_ctx', 'PP');
$ops.add_hll_pirop_mapping('perl6', 'p6captureouters', 'capture_all_outers', 'vP');
$ops.add_hll_pirop_mapping('perl6', 'p6argvmarray', 'perl6_current_args_rpa', 'P');
$ops.add_hll_pirop_mapping('perl6', 'p6bindattrinvres', 'setattribute', '0PPsP');
$ops.add_hll_pirop_mapping('perl6', 'p6finddispatcher', 'perl6_find_dispatcher', 'Ps');
$ops.add_hll_pirop_mapping('perl6', 'p6argsfordispatcher', 'perl6_args_for_dispatcher', 'PP');
$ops.add_hll_pirop_mapping('perl6', 'p6shiftpush', 'perl6_shiftpush', '0PPi');
$ops.add_hll_pirop_mapping('perl6', 'p6arrfindtypes', 'perl6_rpa_find_types', 'IPPii');
$ops.add_hll_pirop_mapping('perl6', 'p6decodelocaltime', 'decodelocaltime', 'Pi');
$ops.add_hll_pirop_mapping('perl6', 'p6setautothreader', 'perl6_setup_junction_autothreading', 'vP');
$ops.add_hll_pirop_mapping('perl6', 'tclc', 'titlecase', 'Ss', :inlinable(1));
$ops.add_hll_op('perl6', 'p6sort', -> $qastcomp, $op {
    $qastcomp.as_post(QAST::Op.new(
        :op('callmethod'), :name('sort'),
        $op[0], $op[1]
    ))
});
my $p6bool := -> $qastcomp, $op {
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
}
$ops.add_hll_op('perl6', 'p6bool', :inlinable(1), $p6bool);
$ops.add_hll_op('perl6', 'p6staticouter', -> $qastcomp, $op {
    $qastcomp.as_post(QAST::Op.new(
        :op('callmethod'), :name('get_outer'),
        $op[0]
    ))
});

# Make some of them also available from NQP land, since we use them in the
# metamodel and bootstrap.
$ops.add_hll_op('nqp', 'p6bool', :inlinable(1), $p6bool);
$ops.add_hll_pirop_mapping('nqp', 'p6var', 'perl6_var', 'PP', :inlinable(1));
$ops.add_hll_pirop_mapping('nqp', 'p6parcel', 'perl6_parcel_from_rpa', 'PPP', :inlinable(1));
$ops.add_hll_pirop_mapping('nqp', 'p6isbindable', 'perl6_is_sig_bindable', 'IPP');
$ops.add_hll_pirop_mapping('nqp', 'p6trialbind', 'perl6_trial_bind_ct', 'IPPP');
$ops.add_hll_pirop_mapping('nqp', 'p6settypes', 'p6settypes', 'vP', :inlinable(1));
$ops.add_hll_pirop_mapping('nqp', 'p6init', 'rakudo_dynop_setup', 'v', :inlinable(1));

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
$ops.add_hll_op('perl6', 'hyper_MT', :inlinable(1), -> $qastcomp, $op {
    if +$op.list != 5 {
        nqp::die("Operation 'hyper_MT' needs 5 operands");
    }

    # input array A and B
    my $arr_a := $qastcomp.coerce($qastcomp.as_post( QAST::Op.new( :op<callmethod>, :name<FLATTENABLE_LIST>, $op[0] ) ), 'P');
    my $arr_b := $qastcomp.coerce($qastcomp.as_post( QAST::Op.new( :op<callmethod>, :name<FLATTENABLE_LIST>, $op[1] ) ), 'P');

    # the operation to perform, e.g. 'nqp_bigint_add'
    unless $op[2].has_compile_time_value {
        nqp::die("Operation must be known at compile time in op 'hyper_MT'");
    }
    my $operation := nqp::unbox_s($op[2].compile_time_value);

    # result register type usually Array or List
    my $rreg := $qastcomp.coerce($qastcomp.as_post( QAST::Op.new( :op<callmethod>, :name<FLATTENABLE_LIST>,
                    QAST::Op.new( :op<callmethod>, :name<new>, $op[3] ) ) ), 'P');
    
    # slot type, would be Int if it is a mathematical operation
    my $obj := $qastcomp.coerce($qastcomp.as_post( $op[4] ), 'P');

    my $n := $*REGALLOC.fresh_p();
    # create a proxy (green thread) that will write to a shared variable
    my $write_ops := PIRT::Ops.new();
    $write_ops.push_pirop(".param pmc results");
    $write_ops.push_pirop(".local pmc interp, task, offset, n");
    $write_ops.push_pirop(".local int i");
    $write_ops.push_pirop("interp = getinterp");
    $write_ops.push_pirop("task = interp.'current_task'()");
    $write_ops.push_pirop("n = pop task");
    $write_ops.push_pirop("i = n"); # XXX this is a hack
    $write_ops.push_pirop("offset = pop task");
    $write_ops.push_pirop('perl6_box_int', $n, 'i');
    $write_ops.push_pirop("results[offset] = $n");
    my $write_sub := PIRT::Sub.new();
    $write_sub.push($write_ops);
    $write_sub.subid('write_task');

    # create the op itself
    my $add_i_ops := PIRT::Ops.new();
    $add_i_ops.push($write_sub);
    $add_i_ops.push_pirop(".param pmc offset");
    $add_i_ops.push_pirop(".local pmc interp, task, results, array_a, array_b, write_task");
    $add_i_ops.push_pirop(".local int a, b");
    $add_i_ops.push_pirop("interp = getinterp");
    $add_i_ops.push_pirop("task = interp.'current_task'()");
    $add_i_ops.push_pirop("array_b = pop task");
    $add_i_ops.push_pirop("array_a = pop task");
    $add_i_ops.push_pirop("results = pop task");
    $add_i_ops.push_pirop("a = array_a[offset]"); # XXX this is a hack
    $add_i_ops.push_pirop("b = array_b[offset]"); # XXX this is a hack
    my $a := $*REGALLOC.fresh_p();
    my $b := $*REGALLOC.fresh_p();
    my $c := $*REGALLOC.fresh_p();
    my $sub_pmc := $*REGALLOC.fresh_p();
    $add_i_ops.push_pirop('perl6_box_int', $a, 'a');
    $add_i_ops.push_pirop('perl6_box_int', $b, 'b');
    $add_i_ops.push($obj);
    $add_i_ops.push_pirop("$operation $c, $a, $b, $obj");
    $add_i_ops.push_pirop("write_task = new ['Task']");
    $add_i_ops.push_pirop("push write_task, offset");
    $add_i_ops.push_pirop('push', 'write_task', $c);
    $add_i_ops.push_pirop(".const 'Sub' \$P0 = 'write_task'");
    $add_i_ops.push_pirop("setattribute write_task, 'code', \$P0");
    $add_i_ops.push_pirop("setattribute write_task, 'data', results");
    $add_i_ops.push_pirop("interp.'schedule_proxied'(write_task, results)");
    $add_i_ops.push_pirop("wait write_task");
    my $add_i_sub := PIRT::Sub.new();
    $add_i_sub.push($add_i_ops);
    $add_i_sub.subid('hyper_task');
    
    # the main sub, it iterates over the input arrays and creates threads (tasks)
    my $ops := PIRT::Ops.new();
    $ops.push($add_i_sub);
    $ops.push_pirop(".local pmc task, operation, starter, offset, end, interp, tasks, array_a, array_b, number");
    $ops.push_pirop(".local int offset_i");
    
    $ops.push_pirop("tasks = new ['ResizablePMCArray']");
    $ops.push($arr_a);
    $ops.push_pirop('set', 'array_a', $arr_a);
    $ops.push($arr_b);
    $ops.push_pirop('set', 'array_b', $arr_b);
    $ops.push($rreg);
    
    $ops.push_pirop("offset = new ['Integer']");
    $ops.push_pirop("offset = 0");
    $ops.push_pirop("end = new ['Integer']");
    my $end := $*REGALLOC.fresh_i();
    $ops.push_pirop("$end = '&prefix:<+>'(array_a)");
    $ops.push_pirop("end = $end");
    
    $ops.push_pirop("offset_i = offset");
    $ops.push_pirop("spawn_tasks:");
    $ops.push_pirop("task = new ['Task']");
    $ops.push_pirop('push', 'task', $rreg);
    $ops.push_pirop("push task, array_a");
    $ops.push_pirop("push task, array_b");
    $ops.push_pirop(".const 'Sub' \$P0 = 'hyper_task'");
    $ops.push_pirop("setattribute task, 'code', \$P0");
    $ops.push_pirop("number = new ['Integer']");
    $ops.push_pirop("number = offset_i");
    $ops.push_pirop("setattribute task, 'data', number");
    $ops.push_pirop("push tasks, task");
    $ops.push_pirop("schedule task");
    $ops.push_pirop("inc offset_i");
    $ops.push_pirop("if end > offset_i goto spawn_tasks");
    
    $ops.push_pirop("offset_i = offset");
    $ops.push_pirop("join_tasks:");
    $ops.push_pirop("task = tasks[offset_i]");
    $ops.push_pirop("wait task");
    $ops.push_pirop("inc offset_i");
    $ops.push_pirop("if end > offset_i goto join_tasks");
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
