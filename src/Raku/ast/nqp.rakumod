# Create NQP ops without Call::Name roundabout
class RakuAST::Nqp
  is RakuAST::Expression
{
    has Str $.op;
    has RakuAST::ArgList $.args;

    method new(Str $op, *@args) {
        nqp::die('RakuAST::Nqp does not support nqp::const.  Use RakuAST::Nqp::Const')
          if $op eq 'const';

        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Nqp, '$!op', $op);

        my $args;
        if nqp::elems(@args) == 1 {
            my $it := @args[0];
            if nqp::istype($it, RakuAST::ArgList) {
                nqp::bindattr($obj, RakuAST::Nqp, '$!args', $it);
                $args := nqp::getattr(@args[0], RakuAST::ArgList, '$!args');
            }
            elsif nqp::istype($it, List) {
                $it.elems;  # reify
                $obj.set-args($args := nqp::getattr($it, List, '$!reified'));
            }
            else {
                $obj.set-args($args := @args);
            }
        }
        else {
            $obj.set-args($args := @args);
        }

        # We want to make use of nqp ops as simple as possible, so
        # we automatically convert common types to their RakuAST
        # equivalents.
        my int $i;
        my int $n := nqp::elems($args);
        while $i < $n {
            my $arg := $args[$i];
            if nqp::istype($arg,Str) {
                $args[$i] := RakuAST::StrLiteral.new($arg);
            }
            elsif nqp::istype($arg,Int) {
                $args[$i] := RakuAST::IntLiteral.new($arg);
            }
            ++$i;
        }

        $obj
    }

    method needs-sink-call() { False }

    method set-args($args) {
        my $arglist := nqp::create(RakuAST::ArgList);
        nqp::bindattr($arglist, RakuAST::ArgList, '$!args', $args);
        nqp::bindattr(self, RakuAST::Nqp, '$!args', $arglist);
    }

    method visit-children(Code $visitor) {
        my @args := nqp::getattr(self.args, RakuAST::ArgList, '$!args');
        for @args {
            $visitor($_);
        }
    }

    method PERFORM-CHECK(RakuAST::Resolver $resolver, RakuAST::IMPL::QASTContext $context) {
        # Avoid worries about sink context
    }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        my $call := QAST::Op.new(:op($!op));
        $!args.IMPL-ADD-QAST-ARGS($context, $call);

        # We generally want to send unboxed string/int values in for dispatch
        # arguments (although leave normal ones alone); we can't really
        # know which are which, but if we're writing out an `nqp::op`
        # just assume that they should all be unboxed; most situations
        # will see the dispatch op generated anyway.
        my int $i;
        my int $n := nqp::elems($call.list);
        while $i < $n {
            my $arg := $call[$i];
            if nqp::istype($arg, QAST::Want)
              && ($arg[1] eq 'Ss' || $arg[1] eq 'Ii') {
                $call[$i] := $arg[2];
            }
            ++$i;
        }

        if $!op eq 'handle' {
            $i := 1;
            while $i < $n {
                $call[$i] := $call[$i].value; # Unpack strings from their SVal
                $i := $i + 2;
            }
        }

        $call
    }

    method IMPL-CAN-INTERPRET() {
        my $op := $!op;
        ($op eq 'hash' || $op eq 'list_s' || $op eq 'atpos_s' || $!op eq 'p6box_i')
             && self.args.IMPL-CAN-INTERPRET;
    }

    method IMPL-INTERPRET(RakuAST::IMPL::InterpContext $context) {
        my @args := nqp::getattr(self.args, RakuAST::ArgList, '$!args');
        my $op := $!op;
        if $op eq 'hash' {
            my @args := self.args.IMPL-INTERPRET($context)[0];
            my $result := nqp::hash;
            while @args {
                my $key := nqp::shift(@args);
                my $val := nqp::shift(@args);
                $result{$key} := $val;
            }
            $result
        }
        elsif $op eq 'list_s' {
            my @args := self.args.IMPL-INTERPRET($context)[0];
            my $result := nqp::list_s;
            for @args {
                nqp::push_s($result, $_);
            }
            $result
        }
        elsif $op eq 'atpos_s' {
            my @args := self.args.IMPL-INTERPRET($context)[0];
            nqp::atpos_s(@args[0], @args[1]);
        }
        elsif $op eq 'box_i' {
            nqp::box_i(@args[0].IMPL-INTERPRET($context), Int);
        }
    }
}

class RakuAST::Nqp::Const
  is RakuAST::Expression
{
    has Str $.name;

    method new(Str $name) {
        my $obj := nqp::create(self);
        nqp::bindattr($obj, RakuAST::Nqp::Const, '$!name', $name);
        $obj
    }

    method needs-sink-call() { False }

    method IMPL-EXPR-QAST(RakuAST::IMPL::QASTContext $context) {
        QAST::Op.new(:op<const>, :name($!name));
    }

    method IMPL-CAN-INTERPRET() { True }

    method IMPL-INTERPRET(RakuAST::IMPL::InterpContext $context) {
        # Stolen from nqp/src/vm/moar/QAST/QASTOperationsMAST.nqp
        nqp::hash(
            'CCLASS_ANY',           65535,
            'CCLASS_UPPERCASE',     1,
            'CCLASS_LOWERCASE',     2,
            'CCLASS_ALPHABETIC',    4,
            'CCLASS_NUMERIC',       8,
            'CCLASS_HEXADECIMAL',   16,
            'CCLASS_WHITESPACE',    32,
            'CCLASS_PRINTING',      64,
            'CCLASS_BLANK',         256,
            'CCLASS_CONTROL',       512,
            'CCLASS_PUNCTUATION',   1024,
            'CCLASS_ALPHANUMERIC',  2048,
            'CCLASS_NEWLINE',       4096,
            'CCLASS_WORD',          8192,

            'HLL_ROLE_NONE',        0,
            'HLL_ROLE_INT',         1,
            'HLL_ROLE_NUM',         2,
            'HLL_ROLE_STR',         3,
            'HLL_ROLE_ARRAY',       4,
            'HLL_ROLE_HASH',        5,
            'HLL_ROLE_CODE',        6,

            'CONTROL_ANY',          2,
            'CONTROL_NEXT',         4,
            'CONTROL_REDO',         8,
            'CONTROL_LAST',         16,
            'CONTROL_RETURN',       32,
            'CONTROL_TAKE',         128,
            'CONTROL_WARN',         256,
            'CONTROL_SUCCEED',      512,
            'CONTROL_PROCEED',      1024,
            'CONTROL_LABELED',      4096,
            'CONTROL_AWAIT',        8192,
            'CONTROL_EMIT',         16384,
            'CONTROL_DONE',         32768,

            'STAT_EXISTS',             0,
            'STAT_FILESIZE',           1,
            'STAT_ISDIR',              2,
            'STAT_ISREG',              3,
            'STAT_ISDEV',              4,
            'STAT_CREATETIME',         5,
            'STAT_ACCESSTIME',         6,
            'STAT_MODIFYTIME',         7,
            'STAT_CHANGETIME',         8,
            'STAT_BACKUPTIME',         9,
            'STAT_UID',                10,
            'STAT_GID',                11,
            'STAT_ISLNK',              12,
            'STAT_PLATFORM_DEV',       -1,
            'STAT_PLATFORM_INODE',     -2,
            'STAT_PLATFORM_MODE',      -3,
            'STAT_PLATFORM_NLINKS',    -4,
            'STAT_PLATFORM_DEVTYPE',   -5,
            'STAT_PLATFORM_BLOCKSIZE', -6,
            'STAT_PLATFORM_BLOCKS',    -7,

            'OPEN_MODE_RO',             1,
            'OPEN_MODE_WO',             2,
            'OPEN_MODE_RW',             3,

            'PIPE_INHERIT_IN',          1,
            'PIPE_IGNORE_IN',           2,
            'PIPE_CAPTURE_IN',          4,
            'PIPE_INHERIT_OUT',         8,
            'PIPE_IGNORE_OUT',          16,
            'PIPE_CAPTURE_OUT',         32,
            'PIPE_INHERIT_ERR',         64,
            'PIPE_IGNORE_ERR',          128,
            'PIPE_CAPTURE_ERR',         256,
            'PIPE_MERGED_OUT_ERR',      512,

            'TYPE_CHECK_CACHE_DEFINITIVE',  0,
            'TYPE_CHECK_CACHE_THEN_METHOD', 1,
            'TYPE_CHECK_NEEDS_ACCEPTS',     2,

            'C_TYPE_CHAR',              -1,
            'C_TYPE_SHORT',             -2,
            'C_TYPE_INT',               -3,
            'C_TYPE_LONG',              -4,
            'C_TYPE_LONGLONG',          -5,
            'C_TYPE_SIZE_T',            -6,
            'C_TYPE_BOOL',              -7,
            'C_TYPE_ATOMIC_INT',        -8,
            'C_TYPE_FLOAT',             -1,
            'C_TYPE_DOUBLE',            -2,
            'C_TYPE_LONGDOUBLE',        -3,

            'NORMALIZE_NONE',            0,
            'NORMALIZE_NFC',             1,
            'NORMALIZE_NFD',             2,
            'NORMALIZE_NFKC',            3,
            'NORMALIZE_NFKD',            4,

            'RUSAGE_UTIME_SEC',          0,
            'RUSAGE_UTIME_MSEC',         1,
            'RUSAGE_STIME_SEC',          2,
            'RUSAGE_STIME_MSEC',         3,
            'RUSAGE_MAXRSS',             4,
            'RUSAGE_IXRSS',              5,
            'RUSAGE_IDRSS',              6,
            'RUSAGE_ISRSS',              7,
            'RUSAGE_MINFLT',             8,
            'RUSAGE_MAJFLT',             9,
            'RUSAGE_NSWAP',              10,
            'RUSAGE_INBLOCK',            11,
            'RUSAGE_OUBLOCK',            12,
            'RUSAGE_MSGSND',             13,
            'RUSAGE_MSGRCV',             14,
            'RUSAGE_NSIGNALS',           15,
            'RUSAGE_NVCSW',              16,
            'RUSAGE_NIVCSW',             17,

            'UNAME_SYSNAME',              0,
            'UNAME_RELEASE',              1,
            'UNAME_VERSION',              2,
            'UNAME_MACHINE',              3,

            'MVM_OPERAND_LITERAL',        0,
            'MVM_OPERAND_READ_REG',       1,
            'MVM_OPERAND_WRITE_REG',      2,
            'MVM_OPERAND_READ_LEX',       3,
            'MVM_OPERAND_WRITE_LEX',      4,
            'MVM_OPERAND_RW_MASK',        7,

            'MVM_OPERAND_INT8',           8,
            'MVM_OPERAND_INT16',         16,
            'MVM_OPERAND_INT32',         24,
            'MVM_OPERAND_INT64',         32,
            'MVM_OPERAND_NUM32',         40,
            'MVM_OPERAND_NUM64',         48,
            'MVM_OPERAND_STR',           56,
            'MVM_OPERAND_OBJ',           64,
            'MVM_OPERAND_INS',           72,
            'MVM_OPERAND_TYPE_VAR',      80,
            'MVM_OPERAND_LEX_OUTER',     88,
            'MVM_OPERAND_CODEREF',       96,
            'MVM_OPERAND_CALLSITE',     104,
            'MVM_OPERAND_TYPE_MASK',    248,
            'MVM_OPERAND_UINT8',        136,
            'MVM_OPERAND_UINT16',       144,
            'MVM_OPERAND_UINT32',       152,
            'MVM_OPERAND_UINT64',       160,

            'BINARY_ENDIAN_NATIVE',       0,
            'BINARY_ENDIAN_LITTLE',       1,
            'BINARY_ENDIAN_BIG',          2,

            'BINARY_SIZE_8_BIT',          0,
            'BINARY_SIZE_16_BIT',         4,
            'BINARY_SIZE_32_BIT',         8,
            'BINARY_SIZE_64_BIT',        12,

            'SOCKET_FAMILY_UNSPEC',       0,
            'SOCKET_FAMILY_INET',         1,
            'SOCKET_FAMILY_INET6',        2,
            'SOCKET_FAMILY_UNIX',         3,

            'DISP_NONE',                  0,
            'DISP_CALLSAME',              1,
            'DISP_CALLWITH',              2,
            'DISP_LASTCALL',              3,
            'DISP_NEXTCALLEE',            4,
            'DISP_ONLYSTAR',              5,
            'DISP_DECONT',                6,
            'DISP_BIND_SUCCESS',          7,
            'DISP_BIND_FAILURE',          8,
            'DISP_PROPAGATE_CALLWITH',    9,

            'SIG_ELEM_BIND_CAPTURE',        1,
            'SIG_ELEM_BIND_PRIVATE_ATTR',   2,
            'SIG_ELEM_BIND_PUBLIC_ATTR',    4,
            # BIND_PRIVATE_ATTR + BIND_PUBLIC_ATTR
            'SIG_ELEM_BIND_ATTRIBUTIVE',    6,
            'SIG_ELEM_SLURPY_POS',          8,
            'SIG_ELEM_SLURPY_NAMED',        16,
            'SIG_ELEM_SLURPY_LOL',          32,
            'SIG_ELEM_INVOCANT',            64,
            'SIG_ELEM_MULTI_INVOCANT',      128,
            'SIG_ELEM_IS_RW',               256,
            'SIG_ELEM_IS_COPY',             512,
            'SIG_ELEM_IS_RAW',              1024,
            # IS_RW + IS_COPY + IS_RAW
            'SIG_ELEM_IS_NOT_READONLY',     1792,
            'SIG_ELEM_IS_OPTIONAL',         2048,
            'SIG_ELEM_ARRAY_SIGIL',         4096,
            'SIG_ELEM_HASH_SIGIL',          8192,
            'SIG_ELEM_DEFAULT_FROM_OUTER',  16384,
            'SIG_ELEM_IS_CAPTURE',          32768,
            # SLURPY_NAMED + IS_CAPTURE
            'SIG_ELEM_ALL_NAMES_OK',        32784,
            'SIG_ELEM_UNDEFINED_ONLY',      65536,
            'SIG_ELEM_DEFINED_ONLY',        131072,
            # UNDEFINED_ONLY + DEFINED_ONLY
            'SIG_ELEM_DEFINEDNES_CHECK',    196608,
            'SIG_ELEM_TYPE_GENERIC',        524288,
            'SIG_ELEM_DEFAULT_IS_LITERAL',  1048576,
            'SIG_ELEM_NATIVE_INT_VALUE',    2097152,
            'SIG_ELEM_NATIVE_UINT_VALUE',   134217728,
            'SIG_ELEM_NATIVE_NUM_VALUE',    4194304,
            'SIG_ELEM_NATIVE_STR_VALUE',    8388608,
            # NATIVE_UINT_VALUE + NATIVE_INT_VALUE + NATIVE_NUM_VALUE + NATIVE_STR_VALUE
            'SIG_ELEM_NATIVE_VALUE',        148897792,
            'SIG_ELEM_SLURPY_ONEARG',       16777216,
            # SLURPY_POS + SLURPY_NAMED + SLURPY_LOL + SLURPY_ONEARG
            'SIG_ELEM_IS_SLURPY',           16777272,
            # SLURPY_POS + SLURPY_LOL + SLURPY_ONEARG + IS_CAPTURE
            'SIG_ELEM_SLURPY_ARITY',        16810024,
            # SLURPY_POS + SLURPY_NAMED + SLURPY_LOL + SLURPY_ONEARG + IS_CAPTURE
            'SIG_ELEM_IS_NOT_POSITIONAL',   16810040,
            'SIG_ELEM_CODE_SIGIL',          33554432,
            'SIG_ELEM_IS_COERCIVE',         67108864,
            'SIG_ELEM_IS_ITEM',             268435456,
            'SIG_ELEM_IS_EXACT_TYPE',       536870912,

            'EDGE_FATE',               0,
            'EDGE_EPSILON',            1,
            'EDGE_CODEPOINT',          2,
            'EDGE_CODEPOINT_NEG',      3,
            'EDGE_CHARCLASS',          4,
            'EDGE_CHARCLASS_NEG',      5,
            'EDGE_CHARLIST',           6,
            'EDGE_CHARLIST_NEG',       7,
            'EDGE_SUBRULE',            8,
            'EDGE_CODEPOINT_I',        9,
            'EDGE_CODEPOINT_I_NEG',   10,
            'EDGE_GENERIC_VAR',       11,
            'EDGE_CHARRANGE',         12,
            'EDGE_CHARRANGE_NEG',     13,
            'EDGE_CODEPOINT_LL',      14,
            'EDGE_CODEPOINT_I_LL',    15,
            'EDGE_CODEPOINT_M',       16,
            'EDGE_CODEPOINT_M_NEG',   17,
            'EDGE_CODEPOINT_M_LL',    18,
            'EDGE_CODEPOINT_IM',      19,
            'EDGE_CODEPOINT_IM_NEG',  20,
            'EDGE_CODEPOINT_IM_LL',   21,
            'EDGE_CHARRANGE_M',       22,
            'EDGE_CHARRANGE_M_NEG',   23,

            'MVM_reg_void',     0, # not really a register; just a result/return marker
            'MVM_reg_int8',     1,
            'MVM_reg_int16',    2,
            'MVM_reg_int32',    3,
            'MVM_reg_int64',    4,
            'MVM_reg_num32',    5,
            'MVM_reg_num64',    6,
            'MVM_reg_str',      7,
            'MVM_reg_obj',      8,
            'MVM_reg_uint8',   17,
            'MVM_reg_uint16',  18,
            'MVM_reg_uint32',  19,
            'MVM_reg_uint64',  20,

            'DEFCON_DEFINED',   1,
            'DEFCON_UNDEFINED', 2,
            # DEFINED + UNDEFINED
            'DEFCON_MASK',      3,

            'TYPE_NATIVE_INT',   4,
            'TYPE_NATIVE_NUM',   8,
            'TYPE_NATIVE_STR',  16,
            'TYPE_NATIVE_UINT', 32,
            # INT + NUM + STR + UINT
            'TYPE_NATIVE_MASK', 60,

            'BIND_RESULT_OK',       0,
            'BIND_RESULT_FAIL',     1,
            'BIND_RESULT_JUNCTION', 2,

            'BIND_VAL_OBJ',   0,
            'BIND_VAL_INT',   1,
            'BIND_VAL_NUM',   2,
            'BIND_VAL_STR',   3,
            'BIND_VAL_UINT', 10,

            'TYPE_CHECK_CACHE_DEFINITIVE',  0,
            'TYPE_CHECK_CACHE_THEN_METHOD', 1,
            'TYPE_CHECK_NEEDS_ACCEPTS',     2,
        ){$!name}
    }
}
