my $ops := nqp::getcomp('QAST').operations;

# Type containing Perl 6 specific ops.
my $TYPE_P6OPS := 'Lorg/perl6/rakudo/RakOps;';

# Other types we'll refer to.
my $TYPE_OPS   := 'Lorg/perl6/nqp/runtime/Ops;';
my $TYPE_CSD   := 'Lorg/perl6/nqp/runtime/CallSiteDescriptor;';
my $TYPE_SMO   := 'Lorg/perl6/nqp/sixmodel/SixModelObject;';
my $TYPE_TC    := 'Lorg/perl6/nqp/runtime/ThreadContext;';
my $TYPE_CF    := 'Lorg/perl6/nqp/runtime/CallFrame;';
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
$ops.add_hll_op('perl6', 'p6bindsig', :!inlinable, -> $qastcomp, $op {
    my $il := JAST::InstructionList.new();
    $il.append(JAST::Instruction.new( :op('aload_1') ));
    $il.append(JAST::Instruction.new( :op('aload'), 'csd' ));
    $il.append(JAST::Instruction.new( :op('aload'), '__args' ));
    $il.append(JAST::Instruction.new( :op('invokestatic'), $TYPE_P6OPS,
        "p6bindsig", $TYPE_CSD, $TYPE_TC, $TYPE_CSD, "[$TYPE_OBJ" ));
    $il.append(JAST::Instruction.new( :op('dup') ));
    
    my $natlbl := JAST::Label.new( :name('p6bindsig_no_autothread') );
    $il.append(JAST::Instruction.new( :op('ifnonnull'), $natlbl ));
    $il.append(JAST::Instruction.new( :op('aload'), 'cf' ));
    $il.append(JAST::Instruction.new( :op('invokevirtual'),
        $TYPE_CF, 'leave', 'Void' ));
    $il.append(JAST::Instruction.new( :op('return') ));
    $il.append($natlbl);
    
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
$ops.map_classlib_hll_op('perl6', 'p6decontrv', $TYPE_P6OPS, 'p6decontrv', [$RT_OBJ, $RT_OBJ], $RT_OBJ, :tc);
$ops.map_classlib_hll_op('perl6', 'p6capturelex', $TYPE_P6OPS, 'p6capturelex', [$RT_OBJ], $RT_OBJ, :tc, :!inlinable);
$ops.map_classlib_hll_op('perl6', 'p6bindassert', $TYPE_P6OPS, 'p6bindassert', [$RT_OBJ, $RT_OBJ], $RT_OBJ, :tc);
$ops.map_classlib_hll_op('perl6', 'p6stateinit', $TYPE_P6OPS, 'p6stateinit', [], $RT_INT, :tc, :!inlinable);
$ops.map_classlib_hll_op('perl6', 'p6setpre', $TYPE_P6OPS, 'p6setpre', [], $RT_OBJ, :tc);
$ops.map_classlib_hll_op('perl6', 'p6clearpre', $TYPE_P6OPS, 'p6clearpre', [], $RT_OBJ, :tc);
$ops.map_classlib_hll_op('perl6', 'p6inpre', $TYPE_P6OPS, 'p6inpre', [], $RT_INT, :tc);
$ops.map_classlib_hll_op('perl6', 'p6setfirstflag', $TYPE_P6OPS, 'p6setfirstflag', [$RT_OBJ], $RT_OBJ, :tc);
$ops.map_classlib_hll_op('perl6', 'p6takefirstflag', $TYPE_P6OPS, 'p6takefirstflag', [], $RT_INT, :tc);
$ops.add_hll_op('perl6', 'p6return', :!inlinable, -> $qastcomp, $op {
    my $il := JAST::InstructionList.new();
    my $exprres := $qastcomp.as_jast($op[0], :want($RT_OBJ));
    $il.append($exprres.jast);
    $*STACK.obtain($il, $exprres);
    $il.append(JAST::Instruction.new( :op('dup') ));
    $il.append(JAST::Instruction.new( :op('aload'), 'cf' ));
    $il.append(JAST::Instruction.new( :op('invokestatic'), $TYPE_OPS,
        'return_o', 'Void', $TYPE_SMO, $TYPE_CF ));
    $il.append(JAST::Instruction.new( :op('aload'), 'cf' ));
    $il.append(JAST::Instruction.new( :op('getfield'), $TYPE_CF, 'outer', $TYPE_CF ));
    $il.append(JAST::Instruction.new( :op('iconst_1') ));
    $il.append(JAST::Instruction.new( :op('putfield'), $TYPE_CF, 'exitAfterUnwind', "Z" ));
    $il.append(JAST::Instruction.new( :op('aload'), 'cf' ));
    $il.append(JAST::Instruction.new( :op('invokevirtual'),
        $TYPE_CF, 'leave', 'Void' ));
    $il.append(JAST::Instruction.new( :op('return') ));
    $ops.result($il, $RT_OBJ);
});
$ops.map_classlib_hll_op('perl6', 'p6routinereturn', $TYPE_P6OPS, 'p6routinereturn', [$RT_OBJ], $RT_OBJ, :tc, :!inlinable);
$ops.map_classlib_hll_op('perl6', 'p6getouterctx', $TYPE_P6OPS, 'p6getouterctx', [$RT_OBJ], $RT_OBJ, :tc, :!inlinable);
$ops.map_classlib_hll_op('perl6', 'p6captureouters', $TYPE_P6OPS, 'p6captureouters', [$RT_OBJ], $RT_OBJ, :tc, :!inlinable);
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
$ops.map_classlib_hll_op('perl6', 'p6sort', $TYPE_P6OPS, 'p6sort', [$RT_OBJ, $RT_OBJ], $RT_OBJ, :tc);
$ops.map_classlib_hll_op('perl6', 'p6staticouter', $TYPE_P6OPS, 'p6staticouter', [$RT_OBJ], $RT_OBJ, :tc);
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

$ops.add_hll_op('perl6', 'p6invokeflat', -> $qastcomp, $op {
    $op[1].flat(1);
    $qastcomp.as_jast(QAST::Op.new( :op('call'), $op[0], $op[1]));
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
$ops.map_classlib_hll_op('nqp', 'p6inpre', $TYPE_P6OPS, 'p6inpre', [], $RT_INT, :tc);
$ops.map_classlib_hll_op('nqp', 'jvmrakudointerop', $TYPE_P6OPS, 'jvmrakudointerop', [], $RT_OBJ, :tc);

# Override defor to call defined method.
QAST::OperationsJAST.add_hll_op('perl6', 'defor', -> $qastcomp, $op {
    if +$op.list != 2 {
        nqp::die("Operation 'defor' needs 2 operands");
    }
    my $tmp := $op.unique('defined');
    $qastcomp.as_jast(QAST::Stmts.new(
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
