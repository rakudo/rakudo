# XXX This file is very much a work in progress, steadily updating it from the
# Parrot original to work for JVM.
my $ops := nqp::getcomp('QAST').operations;

# Type containing Perl 6 specific ops.
my $TYPE_P6OPS := 'Lorg/perl6/rakudo/Ops;';

# Other types we'll refer to.
my $TYPE_OPS   := 'Lorg/perl6/nqp/runtime/Ops;';
my $TYPE_CSD   := 'Lorg/perl6/nqp/runtime/CallSiteDescriptor;';
my $TYPE_SMO   := 'Lorg/perl6/nqp/sixmodel/SixModelObject;';
my $TYPE_TC    := 'Lorg/perl6/nqp/runtime/ThreadContext;';
my $TYPE_STR   := 'Ljava/lang/String;';
my $TYPE_OBJ   := 'Ljava/lang/Object;';

# Exception categories.
my $EX_CAT_NEXT    := 4;
my $EX_CAT_REDO    := 8;
my $EX_CAT_LAST    := 16;

# Opcode types.
my $RT_OBJ  := 0;
my $RT_INT  := 1;
my $RT_NUM  := 2;
my $RT_STR  := 3;
my $RT_VOID := -1;

# Instruction constants.
my $ALOAD_1     := JAST::Instruction.new( :op('aload_1') );

# Perl 6 opcode specific mappings.
$ops.map_classlib_hll_op('perl6', 'p6box_i', $TYPE_P6OPS, 'p6box_i', [$RT_INT], $RT_OBJ, :tc);
$ops.map_classlib_hll_op('perl6', 'p6box_n', $TYPE_P6OPS, 'p6box_n', [$RT_NUM], $RT_OBJ, :tc);
$ops.map_classlib_hll_op('perl6', 'p6box_s', $TYPE_P6OPS, 'p6box_s', [$RT_STR], $RT_OBJ, :tc);
$ops.map_classlib_hll_op('perl6', 'p6bigint', $TYPE_P6OPS, 'p6bigint', [$RT_NUM], $RT_OBJ, :tc);
$ops.map_classlib_hll_op('perl6', 'p6parcel', $TYPE_P6OPS, 'p6parcel', [$RT_OBJ, $RT_OBJ], $RT_OBJ, :tc);
$ops.map_classlib_hll_op('perl6', 'p6listiter', $TYPE_P6OPS, 'p6listiter', [$RT_OBJ, $RT_OBJ], $RT_OBJ, :tc);
$ops.map_classlib_hll_op('perl6', 'p6list', $TYPE_P6OPS, 'p6list', [$RT_OBJ, $RT_OBJ, $RT_OBJ], $RT_OBJ, :tc);
$ops.map_classlib_hll_op('perl6', 'p6listitems', $TYPE_P6OPS, 'p6listitems', [$RT_OBJ], $RT_OBJ, :tc);
$ops.map_classlib_hll_op('perl6', 'p6recont_ro', $TYPE_P6OPS, 'p6recont_ro', [$RT_OBJ], $RT_OBJ, :tc);
$ops.map_classlib_hll_op('perl6', 'p6store', $TYPE_P6OPS, 'p6store', [$RT_OBJ, $RT_OBJ], $RT_OBJ, :tc);
$ops.map_classlib_hll_op('perl6', 'p6var', $TYPE_P6OPS, 'p6var', [$RT_OBJ], $RT_OBJ, :tc);
$ops.map_classlib_hll_op('perl6', 'p6reprname', $TYPE_P6OPS, 'p6reprname', [$RT_OBJ], $RT_OBJ, :tc);
$ops.map_classlib_hll_op('perl6', 'p6definite', $TYPE_P6OPS, 'p6definite', [$RT_OBJ], $RT_OBJ, :tc);
$ops.add_hll_op('perl6', 'p6bindsig', -> $qastcomp, $op {
    my $il := JAST::InstructionList.new();
    $il.append(JAST::Instruction.new( :op('aload_1') ));
    $il.append(JAST::Instruction.new( :op('aload'), 'csd' ));
    $il.append(JAST::Instruction.new( :op('aload'), '__args' ));
    $il.append(JAST::Instruction.new( :op('invokestatic'), $TYPE_P6OPS,
        "p6bindsig", $TYPE_CSD, $TYPE_TC, $TYPE_CSD, "[$TYPE_OBJ" ));
    $il.append(JAST::Instruction.new( :op('astore'), 'csd' ));
    $il.append(JAST::Instruction.new( :op('aload_1') ));
    $il.append(JAST::Instruction.new( :op('getfield'), $TYPE_TC, 'flatArgs', "[$TYPE_OBJ" ));
    $il.append(JAST::Instruction.new( :op('astore'), '__args' ));
    $ops.result($il, $RT_VOID);
});
$ops.map_classlib_hll_op('perl6', 'p6isbindable', $TYPE_P6OPS, 'p6isbindable', [$RT_OBJ, $RT_OBJ], $RT_INT, :tc);
$ops.map_classlib_hll_op('perl6', 'p6bindcaptosig', $TYPE_P6OPS, 'p6bindcaptosig', [$RT_OBJ, $RT_OBJ], $RT_OBJ, :tc);
$ops.map_classlib_hll_op('perl6', 'p6trialbind', $TYPE_P6OPS, 'p6trialbind', [$RT_OBJ, $RT_OBJ, $RT_OBJ], $RT_INT, :tc);
$ops.map_classlib_hll_op('perl6', 'p6typecheckrv', $TYPE_P6OPS, 'p6typecheckrv', [$RT_OBJ, $RT_OBJ], $RT_OBJ, :tc);
$ops.map_classlib_hll_op('perl6', 'p6decontrv', $TYPE_P6OPS, 'p6decontrv', [$RT_OBJ], $RT_OBJ, :tc);
$ops.map_classlib_hll_op('perl6', 'p6capturelex', $TYPE_P6OPS, 'p6capturelex', [$RT_OBJ], $RT_OBJ, :tc);
$ops.map_classlib_hll_op('perl6', 'p6bindassert', $TYPE_P6OPS, 'p6bindassert', [$RT_OBJ, $RT_OBJ], $RT_OBJ, :tc);
$ops.map_classlib_hll_op('perl6', 'p6stateinit', $TYPE_P6OPS, 'p6stateinit', [], $RT_INT, :tc);
$ops.map_classlib_hll_op('perl6', 'p6setpre', $TYPE_P6OPS, 'p6setpre', [], $RT_OBJ, :tc);
$ops.map_classlib_hll_op('perl6', 'p6clearpre', $TYPE_P6OPS, 'p6clearpre', [], $RT_OBJ, :tc);
$ops.map_classlib_hll_op('perl6', 'p6setfirstflag', $TYPE_P6OPS, 'p6setfirstflag', [$RT_OBJ], $RT_OBJ, :tc);
$ops.map_classlib_hll_op('perl6', 'p6takefirstflag', $TYPE_P6OPS, 'p6takefirstflag', [], $RT_INT, :tc);
$ops.map_classlib_hll_op('perl6', 'p6return', $TYPE_P6OPS, 'p6return', [$RT_OBJ], $RT_OBJ, :tc);
$ops.map_classlib_hll_op('perl6', 'p6routinereturn', $TYPE_P6OPS, 'p6routinereturn', [$RT_OBJ], $RT_OBJ, :tc);
$ops.map_classlib_hll_op('perl6', 'p6getouterctx', $TYPE_P6OPS, 'p6getouterctx', [$RT_OBJ], $RT_OBJ, :tc);
$ops.map_classlib_hll_op('perl6', 'p6captureouters', $TYPE_P6OPS, 'p6captureouters', [$RT_OBJ], $RT_OBJ, :tc);
$ops.add_hll_op('perl6', 'p6argvmarray', -> $qastcomp, $op {
    my $il := JAST::InstructionList.new();
    $il.append(JAST::Instruction.new( :op('aload_1') ));
    $il.append(JAST::Instruction.new( :op('aload'), 'csd' ));
    $il.append(JAST::Instruction.new( :op('aload'), '__args' ));
    $il.append(JAST::Instruction.new( :op('invokestatic'), $TYPE_P6OPS,
        "p6argvmarray", $TYPE_SMO, $TYPE_TC, $TYPE_CSD, "[$TYPE_OBJ" ));
    $ops.result($il, $RT_OBJ);
});
$ops.map_classlib_hll_op('perl6', 'p6bindattrinvres', $TYPE_P6OPS, 'p6bindattrinvres', [$RT_OBJ, $RT_OBJ, $RT_STR, $RT_OBJ], $RT_OBJ, :tc);
$ops.map_classlib_hll_op('perl6', 'p6finddispatcher', $TYPE_P6OPS, 'p6finddispatcher', [$RT_STR], $RT_OBJ, :tc);
$ops.map_classlib_hll_op('perl6', 'p6argsfordispatcher', $TYPE_P6OPS, 'p6argsfordispatcher', [$RT_OBJ], $RT_OBJ, :tc);
$ops.map_classlib_hll_op('perl6', 'p6shiftpush', $TYPE_P6OPS, 'p6shiftpush', [$RT_OBJ, $RT_OBJ, $RT_INT], $RT_OBJ, :tc);
$ops.map_classlib_hll_op('perl6', 'p6arrfindtypes', $TYPE_P6OPS, 'p6arrfindtypes', [$RT_OBJ, $RT_OBJ, $RT_INT, $RT_INT], $RT_INT, :tc);
$ops.map_classlib_hll_op('perl6', 'p6decodelocaltime', $TYPE_P6OPS, 'p6decodelocaltime', [$RT_INT], $RT_OBJ, :tc);
$ops.map_classlib_hll_op('perl6', 'p6setautothreader', $TYPE_P6OPS, 'p6setautothreader', [$RT_OBJ], $RT_OBJ, :tc);
$ops.map_classlib_hll_op('perl6', 'tclc', $TYPE_P6OPS, 'tclc', [$RT_STR], $RT_STR, :tc);
$ops.add_hll_op('perl6', 'p6getcallsig', -> $qastcomp, $op {
    $qastcomp.as_jast(QAST::Op.new( :op('usecapture') ))
});
my $p6bool := -> $qastcomp, $op {
    my $il := JAST::InstructionList.new();
    my $exprres := $qastcomp.as_jast($op[0]);
    $il.append($exprres.jast);
    $*STACK.obtain($il, $exprres);
    
    my $cond_type := $exprres.type;
    if $cond_type == $RT_INT {
        $il.append(JAST::PushIVal.new( :value(0) ));
        $il.append(JAST::Instruction.new( :op('lcmp') ));
    }
    elsif $cond_type == $RT_NUM {
        $il.append(JAST::PushNVal.new( :value(0.0) ));
        $il.append(JAST::Instruction.new( :op('dcmpl') ));
    }
    elsif $cond_type == $RT_STR {
        $il.append(JAST::Instruction.new( :op('invokestatic'),
            $TYPE_OPS, 'istrue_s', 'Long', $TYPE_STR ));
        $il.append(JAST::PushIVal.new( :value(0) ));
        $il.append(JAST::Instruction.new( :op('lcmp') ));
    }
    else {
        $il.append(JAST::Instruction.new( :op('aload_1') ));
        $il.append(JAST::Instruction.new( :op('invokestatic'),
            $TYPE_OPS, 'istrue', 'Long', $TYPE_SMO, $TYPE_TC ));
        $il.append(JAST::PushIVal.new( :value(0) ));
        $il.append(JAST::Instruction.new( :op('lcmp') ));
    }
    $il.append(JAST::Instruction.new( :op('aload'), 'tc' ));
    $il.append(JAST::Instruction.new( :op('invokestatic'),
        $TYPE_P6OPS, 'booleanize', $TYPE_SMO, 'I', $TYPE_TC ));
    $ops.result($il, $RT_OBJ);
};
$ops.add_hll_op('perl6', 'p6bool', $p6bool);

$ops.add_hll_op('perl6', 'p6handletake', -> $qastcomp, $op {
    $qastcomp.as_jast(QAST::Op.new( :op('handle'), $op[0], 'TAKE', $op[1]));
});

# MapIter core.
$ops.add_hll_op('perl6', 'p6mapiter', -> $qastcomp, $op {
    # Create labels.
    my $map_id := $qastcomp.unique('map');
    my $loop_lbl := JAST::Label.new( :name($map_id ~ '_loop') );
    my $done_lbl := JAST::Label.new( :name($map_id ~ '_done') );
    my $redo_lbl := JAST::Label.new( :name($map_id ~ '_redo') );
    
    # Produce handlers.
    #my $l_handler_id  := &*REGISTER_UNWIND_HANDLER($*HANDLER_IDX, $EX_CAT_LAST);
    #my $nr_handler_id := &*REGISTER_UNWIND_HANDLER($l_handler_id, $EX_CAT_NEXT + $EX_CAT_REDO);

    # Initialize.
    my $il := JAST::InstructionList.new();
    my $initres := $qastcomp.as_jast(:want($RT_VOID),
        QAST::Stmts.new(
            QAST::Op.new(
                :op('bind'),
                QAST::Var.new( :name('MapIter'), :scope('local'), :decl('var') ),
                $op[0]
            ),
            QAST::Op.new(
                :op('bind'),
                QAST::Var.new( :name('items'), :scope('local'), :decl('var') ),
                $op[1]
            ),
            QAST::Op.new(
                :op('bind'),
                QAST::Var.new( :name('rpa'), :scope('local'), :decl('var') ),
                $op[2]
            ),
            QAST::Op.new(
                :op('bind'),
                QAST::Var.new( :name('argc'), :scope('local'), :decl('var'), :returns(int) ),
                $op[3]
            ),
            QAST::Op.new(
                :op('bind'),
                QAST::Var.new( :name('count'), :scope('local'), :decl('var'), :returns(int) ),
                $op[4]
            ),
            QAST::Op.new(
                :op('bind'),
                QAST::Var.new( :name('self'), :scope('local'), :decl('var') ),
                $op[5]
            ),
            QAST::Op.new(
                :op('bind'),
                QAST::Var.new( :name('block'), :scope('local'), :decl('var') ),
                $op[6]
            ),
            QAST::Op.new(
                :op('bind'),
                QAST::Var.new( :name('args'), :scope('local'), :decl('var') ),
                QAST::Op.new( :op('list') )
            )
        ));
    $il.append($initres.jast);
    $*STACK.obtain($il, $initres);

#   .local int NEXT, is_sink
#   .local pmc handler, result
#   handler  = root_new ['parrot';'ExceptionHandler']
#   NEXT     = find_lex '$NEXT'
#   is_sink  = find_lex '$is_sink'
# 
#   set_addr handler, catch
#   handler.'handle_types'(.CONTROL_LOOP_LAST, .CONTROL_LOOP_NEXT, .CONTROL_LOOP_REDO)
#   push_eh handler

# iter_loop:
    $il.append($loop_lbl);

#   $I0 = elements rpa
#   unless $I0 < count goto iter_done
    my $rpaelemsres := $qastcomp.as_jast(:want($RT_INT),
        QAST::Op.new(
            :op('islt_i'),
            QAST::Op.new(
                :op('elems'),
                QAST::Var.new( :name('rpa'), :scope('local') )
            ),
            QAST::Var.new( :name('count'), :scope('local') )));
    $il.append($rpaelemsres.jast);
    $*STACK.obtain($il, $rpaelemsres);
    $il.append(JAST::Instruction.new( :op('l2i') ));
    $il.append(JAST::Instruction.new( :op('ifeq'), $done_lbl ));
    
#   $I0 = elements items
#   if $I0 >= argc goto have_items
#   $I0 = argc - $I0
#   $P0 = getattribute self, MapIter, '$!listiter'
#   unless $P0 goto have_items
#   $P0.'reify'($I0)
# have_items:
    my $itemsres := $qastcomp.as_jast(:want($RT_VOID),
        QAST::Stmts.new(
            QAST::Op.new(
                :op('bind'),
                QAST::Var.new( :name('itmp'), :scope('local'), :decl('var') ),
                QAST::Op.new(
                    :op('elems'),
                    QAST::Var.new( :name('items'), :scope('local') )
                )),
            QAST::Op.new(
                :op('unless'),
                QAST::Op.new(
                    :op('isge_i'),
                    QAST::Var.new( :name('itmp'), :scope('local') ),
                    QAST::Var.new( :name('argc'), :scope('local') )
                ),
                QAST::Stmts.new(
                    QAST::Op.new(
                        :op('bind'),
                        QAST::Var.new( :name('itmp'), :scope('local') ),
                        QAST::Op.new(
                            :op('sub_i'),
                            QAST::Var.new( :name('argc'), :scope('local') ),
                            QAST::Var.new( :name('itmp'), :scope('local') )
                        )),
                    QAST::Op.new(
                        :op('bind'),
                        QAST::Var.new( :name('otmp'), :scope('local'), :decl('var') ),
                        QAST::Var.new(
                            :name('$!listiter'), :scope('attribute'),
                            QAST::Var.new( :name('self'), :scope('local') ),
                            QAST::Var.new( :name('MapIter'), :scope('local') )
                        )),
                    QAST::Op.new(
                        :op('if'),
                        QAST::Var.new( :name('otmp'), :scope('local') ),
                        QAST::Op.new(
                            :op('callmethod'), :name('reify'),
                            QAST::Var.new( :name('otmp'), :scope('local') ),
                            QAST::Var.new( :name('itmp'), :scope('local') )
                        )))
            )));
    $il.append($itemsres.jast);
    $*STACK.obtain($il, $itemsres);

#   args = 0
#   perl6_shiftpush args, items, argc
#   unless args goto iter_done
    my $argres := $qastcomp.as_jast(:want($RT_INT),
        QAST::Stmts.new(
            QAST::Op.new(
                :op('setelems'),
                QAST::Var.new( :name('args'), :scope('local') ),
                QAST::IVal.new( :value(0) )
            ),
            QAST::Op.new(
                :op('istrue'),
                    QAST::Op.new(
                    :op('p6shiftpush'),
                    QAST::Var.new( :name('args'), :scope('local') ),
                    QAST::Var.new( :name('items'), :scope('local') ),
                    QAST::Var.new( :name('argc'), :scope('local') )
                ))));
    $il.append($argres.jast);
    $*STACK.obtain($il, $argres);
    $il.append(JAST::Instruction.new( :op('l2i') ));
    $il.append(JAST::Instruction.new( :op('ifeq'), $done_lbl ));

# redo:
    $il.append($redo_lbl);
#   result = block(args :flat)
#   if is_sink goto sink_result
#   push rpa, result
#   goto next
    my $invres := $qastcomp.as_jast(:want($RT_VOID),
        QAST::Op.new(
            :op('push'),
            QAST::Var.new( :name('rpa'), :scope('local') ),
            QAST::Op.new(
                :op('call'),
                QAST::Var.new( :name('block'), :scope('local') ),
                QAST::Var.new( :name('args'), :scope('local'), :flat(1) )
            )));
    $il.append($invres.jast);
    $*STACK.obtain($il, $invres);
    $il.append(JAST::Instruction.new( :op('goto'), $loop_lbl ));
    
# sink_result:
#   $I0 = repr_defined result
#   unless $I0 goto next
#   $I0 = can result, 'sink'
#   unless $I0 goto next
#   $I0 = defined result
#   unless $I0 goto next
#   result.'sink'()
#   goto next
# catch:
#   .local pmc exception, type
#   .get_results (exception)
#   null $P0
#   perl6_invoke_catchhandler $P0, exception
#   result = getattribute exception, 'payload'
#   push rpa, result
#   type = getattribute exception, 'type'
#   if type == .CONTROL_LOOP_REDO goto redo
#   if type == .CONTROL_LOOP_LAST goto last
# next:
#   unless NEXT goto iter_loop
#   block.'fire_phasers'('NEXT')
#   goto iter_loop
# last:
#   $P0 = find_lex 'Any'
#   setattribute self, MapIter, '$!items', $P0
#   setattribute self, MapIter, '$!listiter', $P0

# iter_done:
    $il.append($done_lbl);
#   pop_eh    

    $il.append(JAST::Instruction.new( :op('aconst_null') ));
    $ops.result($il, $RT_OBJ);
});

# Make some of them also available from NQP land, since we use them in the
# metamodel and bootstrap.
$ops.add_hll_op('nqp', 'p6bool', $p6bool);
$ops.map_classlib_hll_op('nqp', 'p6init', $TYPE_P6OPS, 'p6init', [], $RT_OBJ, :tc);
$ops.map_classlib_hll_op('nqp', 'p6settypes', $TYPE_P6OPS, 'p6settypes', [$RT_OBJ], $RT_OBJ, :tc);
$ops.map_classlib_hll_op('nqp', 'p6var', $TYPE_P6OPS, 'p6var', [$RT_OBJ], $RT_OBJ, :tc);
$ops.map_classlib_hll_op('nqp', 'p6parcel', $TYPE_P6OPS, 'p6parcel', [$RT_OBJ, $RT_OBJ], $RT_OBJ, :tc);
$ops.map_classlib_hll_op('nqp', 'p6isbindable', $TYPE_P6OPS, 'p6isbindable', [$RT_OBJ, $RT_OBJ], $RT_INT, :tc);
$ops.map_classlib_hll_op('nqp', 'p6trialbind', $TYPE_P6OPS, 'p6trialbind', [$RT_OBJ, $RT_OBJ, $RT_OBJ], $RT_INT, :tc);

## Override defor to avoid v-table call.
#$ops.add_hll_op('perl6', 'defor', -> $qastcomp, $op {
#    if +$op.list != 2 {
#        nqp::die("Operation 'defor' needs 2 operands");
#    }
#    my $ops := PIRT::Ops.new();
#    my $lbl := PIRT::Label.new(:name('defor'));
#    my $dreg := $*REGALLOC.fresh_p();
#    my $rreg := $*REGALLOC.fresh_p();
#    my $test := $qastcomp.coerce($qastcomp.as_post($op[0]), 'P');
#    my $then := $qastcomp.coerce($qastcomp.as_post($op[1]), 'P');
#    $ops.push($test);
#    $ops.push_pirop('set', $rreg, $test);
#    $ops.push_pirop('callmethod', "'defined'", $rreg, :result($dreg));
#    $ops.push_pirop('if', $dreg, $lbl);
#    $ops.push($then);
#    $ops.push_pirop('set', $rreg, $then);
#    $ops.push($lbl);
#    $ops.result($rreg);
#    $ops
#});

# Boxing and unboxing configuration.
$ops.add_hll_box('perl6', $RT_INT, -> $qastcomp {
    my $il := JAST::InstructionList.new();
    $il.append($ALOAD_1);
    $il.append(JAST::Instruction.new( :op('invokestatic'), $TYPE_P6OPS,
        'p6box_i', $TYPE_SMO, 'Long', $TYPE_TC ));
    $il
});
$ops.add_hll_box('perl6', $RT_NUM, -> $qastcomp {
    my $il := JAST::InstructionList.new();
    $il.append($ALOAD_1);
    $il.append(JAST::Instruction.new( :op('invokestatic'), $TYPE_P6OPS,
        'p6box_n', $TYPE_SMO, 'Double', $TYPE_TC ));
    $il
});
$ops.add_hll_box('perl6', $RT_STR, -> $qastcomp {
    my $il := JAST::InstructionList.new();
    $il.append($ALOAD_1);
    $il.append(JAST::Instruction.new( :op('invokestatic'), $TYPE_P6OPS,
        'p6box_s', $TYPE_SMO, $TYPE_STR, $TYPE_TC ));
    $il
});
#$ops.force_return_boxing_for_hll('perl6');
