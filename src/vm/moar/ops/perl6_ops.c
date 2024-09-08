#define MVM_SHARED 1
#include "moar.h"

#ifdef _WIN32
#include <windows.h>
#include <time.h>
#else
#include <time.h>
#include <sys/time.h>
#endif
#include <stdio.h>

#define GET_REG(tc, idx)    (*tc->interp_reg_base)[*((MVMuint16 *)(cur_op + idx))]

/* Initializes the Raku extension ops. */
static void p6init(MVMThreadContext *tc, MVMuint8 *cur_op) {
}

static MVMuint8 s_p6captureouters[] = {
    MVM_operand_obj | MVM_operand_read_reg,
    MVM_operand_obj | MVM_operand_read_reg
};
static void p6captureouters(MVMThreadContext *tc, MVMuint8 *cur_op) {
    MVMObject *todo  = GET_REG(tc, 0).o;
    MVMObject *tgt   = GET_REG(tc, 2).o;
    MVMint64   elems = MVM_repr_elems(tc, todo);
    MVMint64   i;
    MVMFrame  *new_outer;
    if (REPR(tgt)->ID != MVM_REPR_ID_MVMCode)
        MVM_exception_throw_adhoc(tc, "p6captureouters second arg must be MVMCode");
    new_outer = ((MVMCode *)tgt)->body.outer;
    if (!new_outer)
        return;
    for (i = 0; i < elems; i++) {
        MVMObject *vm_code_obj = MVM_repr_at_pos_o(tc, todo, i);
        if (REPR(vm_code_obj)->ID == MVM_REPR_ID_MVMCode) {
            MVMFrame *outer = ((MVMCode *)vm_code_obj)->body.outer;
            MVM_ASSIGN_REF(tc, &(outer->header), outer->outer, new_outer);
        }
        else {
            MVM_exception_throw_adhoc(tc, "p6captureouters got non-code object");
        }
    }
}

static MVMuint8 s_p6stateinit[] = {
    MVM_operand_int64 | MVM_operand_write_reg
};
static void p6stateinit(MVMThreadContext *tc, MVMuint8 *cur_op) {
    GET_REG(tc, 0).i64 = tc->cur_frame->flags & MVM_FRAME_FLAG_STATE_INIT ? 1 : 0;
}

/* First FIRST, use a flag in the object header. */
#define RAKUDO_FIRST_FLAG 128

static MVMuint8 s_p6setfirstflag[] = {
    MVM_operand_obj | MVM_operand_write_reg,
    MVM_operand_obj | MVM_operand_read_reg
};
static void p6setfirstflag(MVMThreadContext *tc, MVMuint8 *cur_op) {
    MVMObject *code_obj = GET_REG(tc, 2).o;
    if (!MVM_code_iscode(tc, code_obj))
        MVM_exception_throw_adhoc(tc, "p6setfirstflag requires a bytecode handle");
    code_obj->header.flags1 |= RAKUDO_FIRST_FLAG;
    GET_REG(tc, 0).o = code_obj;
}

static MVMuint8 s_p6takefirstflag[] = {
    MVM_operand_int64 | MVM_operand_write_reg
};
static void p6takefirstflag(MVMThreadContext *tc, MVMuint8 *cur_op) {
    MVMObject *vm_code = tc->cur_frame->code_ref;
    if (vm_code->header.flags1 & RAKUDO_FIRST_FLAG) {
        vm_code->header.flags1 ^= RAKUDO_FIRST_FLAG;
        GET_REG(tc, 0).i64 = 1;
    }
    else {
        GET_REG(tc, 0).i64 = 0;
    }
}

#define RAKUDO_FRAME_PRE_FLAG MVM_FRAME_FLAG_HLL_1

static MVMuint8 s_p6setpre[] = {
    MVM_operand_obj | MVM_operand_write_reg
};
static void p6setpre(MVMThreadContext *tc, MVMuint8 *cur_op) {
    tc->cur_frame->flags |= RAKUDO_FRAME_PRE_FLAG;
    GET_REG(tc, 0).o = NULL;
}

static MVMuint8 s_p6clearpre[] = {
    MVM_operand_obj | MVM_operand_write_reg
};
static void p6clearpre(MVMThreadContext *tc, MVMuint8 *cur_op) {
    if (tc->cur_frame->flags & RAKUDO_FRAME_PRE_FLAG)
        tc->cur_frame->flags ^= RAKUDO_FRAME_PRE_FLAG;
    GET_REG(tc, 0).o = NULL;
}

static MVMuint8 s_p6inpre[] = {
    MVM_operand_int64 | MVM_operand_write_reg
};
static void p6inpre(MVMThreadContext *tc, MVMuint8 *cur_op) {
    MVMFrame *test_frame = tc->cur_frame->caller;
    if (test_frame && test_frame->flags & RAKUDO_FRAME_PRE_FLAG) {
        test_frame->flags ^= RAKUDO_FRAME_PRE_FLAG;
        GET_REG(tc, 0).i64 = 1;
    }
    else {
        GET_REG(tc, 0).i64 = 0;
    }
}

static MVMuint8 s_p6staticouter[] = {
    MVM_operand_obj | MVM_operand_write_reg,
    MVM_operand_obj | MVM_operand_read_reg
};
static void p6staticouter(MVMThreadContext *tc, MVMuint8 *cur_op) {
    MVMObject *code = GET_REG(tc, 2).o;
    if (!MVM_is_null(tc, code) && IS_CONCRETE(code) && REPR(code)->ID == MVM_REPR_ID_MVMCode) {
        MVMStaticFrame *sf = ((MVMCode *)code)->body.sf;
        GET_REG(tc, 0).o = sf->body.outer
            ? (MVMObject *)sf->body.outer->body.static_code
            : NULL;
    }
    else {
        MVM_exception_throw_adhoc(tc, "p6staticouter requires a CodeRef");
    }
}

static MVMuint8 s_p6invokeunder[] = {
    MVM_operand_obj | MVM_operand_write_reg,
    MVM_operand_obj | MVM_operand_read_reg,
    MVM_operand_obj | MVM_operand_read_reg
};
static void return_from_fake(MVMThreadContext *tc, void *sr_data) {
    MVM_frame_try_return_no_exit_handlers(tc);
}
static void p6invokeunder(MVMThreadContext *tc, MVMuint8 *cur_op) {
    MVMRegister *res  = &GET_REG(tc, 0);
    MVMObject   *fake = GET_REG(tc, 2).o;
    MVMObject   *code = GET_REG(tc, 4).o;

    /* Invoke the fake frame; note this doesn't return to the interpreter, so
     * we can do hackery after it. */
    tc->cur_frame->return_address = *(tc->interp_cur_op) + 6;
    MVM_gc_root_temp_push(tc, (MVMCollectable **)&code);
    MVM_frame_dispatch_zero_args(tc, (MVMCode *)fake);
    MVM_gc_root_temp_pop(tc);

    /* Now we call the second code ref, thus meaning it'll appear to have been
     * called by the first. We set up a special return handler to properly
     * remove it. */
    MVM_callstack_allocate_special_return(tc, return_from_fake, NULL, NULL, 0);
    tc->cur_frame->return_value = res;
    tc->cur_frame->return_type = MVM_RETURN_OBJ;
    MVM_frame_dispatch_zero_args(tc, (MVMCode *)code);
}

void dump_please() {
    fprintf(stderr, "atexit called\n");
    MVM_dump_backtrace(MVM_get_running_threads_context());
}

/* Registers the extops with MoarVM. */
MVM_DLL_EXPORT void Rakudo_ops_init(MVMThreadContext *tc) {
    MVM_ext_register_extop(tc, "p6init",  p6init, 0, NULL, NULL, NULL, 0);
    MVM_ext_register_extop(tc, "p6captureouters", p6captureouters, 2, s_p6captureouters, NULL, NULL, 0);
    MVM_ext_register_extop(tc, "p6stateinit", p6stateinit, 1, s_p6stateinit, NULL, NULL, 0);
    MVM_ext_register_extop(tc, "p6setfirstflag", p6setfirstflag, 2, s_p6setfirstflag, NULL, NULL, 0);
    MVM_ext_register_extop(tc, "p6takefirstflag", p6takefirstflag, 1, s_p6takefirstflag, NULL, NULL, 0);
    MVM_ext_register_extop(tc, "p6setpre", p6setpre, 1, s_p6setpre, NULL, NULL, 0);
    MVM_ext_register_extop(tc, "p6clearpre", p6clearpre, 1, s_p6clearpre, NULL, NULL, 0);
    MVM_ext_register_extop(tc, "p6inpre", p6inpre, 1, s_p6inpre, NULL, NULL, 0);
    MVM_ext_register_extop(tc, "p6staticouter", p6staticouter, 2, s_p6staticouter, NULL, NULL, 0);
    MVM_ext_register_extop(tc, "p6invokeunder", p6invokeunder, 3, s_p6invokeunder, NULL, NULL, MVM_EXTOP_NO_JIT);

    atexit(dump_please);
}
