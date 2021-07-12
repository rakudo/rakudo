#define MVM_SHARED 1
#include "moar.h"

#ifdef _WIN32
#include <windows.h>
#include <time.h>
#else
#include <time.h>
#include <sys/time.h>
#endif

#define GET_REG(tc, idx)    (*tc->interp_reg_base)[*((MVMuint16 *)(cur_op + idx))]

/* Dummy zero callsite. */
static MVMCallsite no_arg_callsite = { NULL, 0, 0, 0, 0, 0, NULL, NULL };

/* Initializes the Raku extension ops. */
static void p6init(MVMThreadContext *tc, MVMuint8 *cur_op) {
}

static MVMuint8 s_p6capturelex[] = {
    MVM_operand_obj | MVM_operand_write_reg,
    MVM_operand_obj | MVM_operand_read_reg,
};
static void p6capturelex(MVMThreadContext *tc, MVMuint8 *cur_op) {
    MVMObject *p6_code_obj = GET_REG(tc, 2).o;
    MVMInvocationSpec *is = STABLE(p6_code_obj)->invocation_spec;
    MVMObject *vm_code_obj;
    if (is && !MVM_is_null(tc, is->invocation_handler))
        return;
    vm_code_obj = MVM_frame_find_invokee(tc, p6_code_obj, NULL);
    if (REPR(vm_code_obj)->ID == MVM_REPR_ID_MVMCode) {
        if (((MVMCode *)vm_code_obj)->body.sf->body.outer == tc->cur_frame->static_info)
            MVM_frame_capturelex(tc, vm_code_obj);
    }
    else {
        MVM_exception_throw_adhoc(tc, "p6capturelex got non-code object");
    }
    GET_REG(tc, 0).o = p6_code_obj;
}

static MVMuint8 s_p6capturelexwhere[] = {
    MVM_operand_obj | MVM_operand_write_reg,
    MVM_operand_obj | MVM_operand_read_reg,
};
static void p6capturelexwhere(MVMThreadContext *tc, MVMuint8 *cur_op) {
    MVMObject *p6_code_obj = GET_REG(tc, 2).o;
    MVMObject *vm_code_obj = MVM_frame_find_invokee(tc, p6_code_obj, NULL);
    if (REPR(vm_code_obj)->ID == MVM_REPR_ID_MVMCode) {
        MVMFrame *find;
        MVMROOT(tc, vm_code_obj, {
            find = MVM_frame_force_to_heap(tc, tc->cur_frame);
        });
        while (find) {
            if (((MVMCode *)vm_code_obj)->body.sf->body.outer == find->static_info) {
                MVMFrame *orig = tc->cur_frame;
                tc->cur_frame = find;
                MVM_frame_capturelex(tc, vm_code_obj);
                tc->cur_frame = orig;
                break;
            }
            find = find->caller;
        }
    }
    else {
        MVM_exception_throw_adhoc(tc, "p6capturelexwhere got non-code object");
    }
    GET_REG(tc, 0).o = GET_REG(tc, 2).o;
}

static MVMuint8 s_p6getouterctx[] = {
    MVM_operand_obj | MVM_operand_write_reg,
    MVM_operand_obj | MVM_operand_read_reg
};
static void p6getouterctx(MVMThreadContext *tc, MVMuint8 *cur_op) {
    MVMObject *p6_code_obj = GET_REG(tc, 2).o;
    MVMObject *vm_code_obj = MVM_frame_find_invokee(tc, p6_code_obj, NULL);
    MVMFrame  *outer       = ((MVMCode *)vm_code_obj)->body.outer;
    if (outer)
        GET_REG(tc, 0).o = MVM_frame_context_wrapper(tc, outer);
    else
        MVM_exception_throw_adhoc(tc, "Specified code ref has no outer");
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
        MVMObject *p6_code_obj = MVM_repr_at_pos_o(tc, todo, i);
        MVMObject *vm_code_obj = MVM_frame_find_invokee(tc, p6_code_obj, NULL);
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
#ifdef MVM_COLLECTABLE_FLAGS1
#define RAKUDO_FIRST_FLAG 128
#else
#define RAKUDO_FIRST_FLAG 16384
#endif

static MVMuint8 s_p6setfirstflag[] = {
    MVM_operand_obj | MVM_operand_write_reg,
    MVM_operand_obj | MVM_operand_read_reg
};
static void p6setfirstflag(MVMThreadContext *tc, MVMuint8 *cur_op) {
    MVMObject *code_obj = GET_REG(tc, 2).o;
    MVMObject *vm_code  = MVM_frame_find_invokee(tc, code_obj, NULL);
#ifdef MVM_COLLECTABLE_FLAGS1
    vm_code->header.flags1 |= RAKUDO_FIRST_FLAG;
#else
    vm_code->header.flags |= RAKUDO_FIRST_FLAG;
#endif
    GET_REG(tc, 0).o = code_obj;
}

static MVMuint8 s_p6takefirstflag[] = {
    MVM_operand_int64 | MVM_operand_write_reg
};
static void p6takefirstflag(MVMThreadContext *tc, MVMuint8 *cur_op) {
    MVMObject *vm_code = tc->cur_frame->code_ref;
#ifdef MVM_COLLECTABLE_FLAGS1
    if (vm_code->header.flags1 & RAKUDO_FIRST_FLAG) {
        vm_code->header.flags1 ^= RAKUDO_FIRST_FLAG;
#else
    if (vm_code->header.flags & RAKUDO_FIRST_FLAG) {
        vm_code->header.flags ^= RAKUDO_FIRST_FLAG;
#endif
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

    fake = MVM_frame_find_invokee(tc, fake, NULL);
    code = MVM_frame_find_invokee(tc, code, NULL);

    /* Invoke the fake frame; note this doesn't return to the interpreter, so
     * we can do hackery after it. */
    tc->cur_frame->return_address = *(tc->interp_cur_op) + 6;
    MVMROOT(tc, code, {
        STABLE(fake)->invoke(tc, fake, &no_arg_callsite, tc->cur_frame->args);
    });

    /* Now we call the second code ref, thus meaning it'll appear to have been
     * called by the first. We set up a special return handler to properly
     * remove it. */
    MVM_args_setup_thunk(tc, res, MVM_RETURN_OBJ, &no_arg_callsite);
    MVM_frame_special_return(tc, tc->cur_frame, return_from_fake, NULL, NULL, NULL);
    STABLE(code)->invoke(tc, code, &no_arg_callsite, tc->cur_frame->args);
}

/* Registers the extops with MoarVM. */
MVM_DLL_EXPORT void Rakudo_ops_init(MVMThreadContext *tc) {
    MVM_ext_register_extop(tc, "p6init",  p6init, 0, NULL, NULL, NULL, 0);
    MVM_ext_register_extop(tc, "p6capturelex",  p6capturelex, 2, s_p6capturelex, NULL, NULL, 0);
    MVM_ext_register_extop(tc, "p6capturelexwhere",  p6capturelexwhere, 2, s_p6capturelexwhere, NULL, NULL, 0);
    MVM_ext_register_extop(tc, "p6getouterctx", p6getouterctx, 2, s_p6getouterctx, NULL, NULL, MVM_EXTOP_PURE | MVM_EXTOP_ALLOCATING);
    MVM_ext_register_extop(tc, "p6captureouters", p6captureouters, 2, s_p6captureouters, NULL, NULL, 0);
    MVM_ext_register_extop(tc, "p6stateinit", p6stateinit, 1, s_p6stateinit, NULL, NULL, 0);
    MVM_ext_register_extop(tc, "p6setfirstflag", p6setfirstflag, 2, s_p6setfirstflag, NULL, NULL, 0);
    MVM_ext_register_extop(tc, "p6takefirstflag", p6takefirstflag, 1, s_p6takefirstflag, NULL, NULL, 0);
    MVM_ext_register_extop(tc, "p6setpre", p6setpre, 1, s_p6setpre, NULL, NULL, 0);
    MVM_ext_register_extop(tc, "p6clearpre", p6clearpre, 1, s_p6clearpre, NULL, NULL, 0);
    MVM_ext_register_extop(tc, "p6inpre", p6inpre, 1, s_p6inpre, NULL, NULL, 0);
    MVM_ext_register_extop(tc, "p6staticouter", p6staticouter, 2, s_p6staticouter, NULL, NULL, 0);
    MVM_ext_register_extop(tc, "p6invokeunder", p6invokeunder, 3, s_p6invokeunder, NULL, NULL, MVM_EXTOP_NO_JIT);
}
