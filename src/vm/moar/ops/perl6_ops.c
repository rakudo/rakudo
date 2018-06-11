#define MVM_SHARED 1
#include "moar.h"
#include "container.h"

#ifdef _WIN32
#include <windows.h>
#include <time.h>
#else
#include <time.h>
#include <sys/time.h>
#endif

#ifndef MVM_spesh_get_and_use_facts
#define MVM_spesh_get_and_use_facts MVM_spesh_get_facts
#endif

#ifndef MVM_gc_root_add_permanent_desc
#define MVM_gc_root_add_permanent_desc(tc, obj_ref, description) MVM_gc_root_add_permanent(tc, obj_ref)
#endif

#define GET_REG(tc, idx)    (*tc->interp_reg_base)[*((MVMuint16 *)(cur_op + idx))]
#define REAL_BODY(tc, obj)  MVM_p6opaque_real_data(tc, OBJECT_BODY(obj))

/* Dummy zero and one-arg callsite. */
static MVMCallsite      no_arg_callsite = { NULL, 0, 0, 0, 0 };
static MVMCallsiteEntry one_arg_flags[] = { MVM_CALLSITE_ARG_OBJ };
static MVMCallsite     one_arg_callsite = { one_arg_flags, 1, 1, 1, 0 };
static MVMCallsiteEntry one_str_flags[] = { MVM_CALLSITE_ARG_STR };
static MVMCallsite     one_str_callsite = { one_str_flags, 1, 1, 1, 0 };

/* Assignment type check failed callsite. */
static MVMCallsiteEntry atcf_flags[] = { MVM_CALLSITE_ARG_STR, MVM_CALLSITE_ARG_OBJ, 
                                         MVM_CALLSITE_ARG_OBJ };
static MVMCallsite     atcf_callsite = { atcf_flags, 3, 3, 3, 0 };

/* Dispatcher vivify_for callsite. */
static MVMCallsiteEntry disp_flags[] = { MVM_CALLSITE_ARG_OBJ, MVM_CALLSITE_ARG_OBJ, 
                                         MVM_CALLSITE_ARG_OBJ, MVM_CALLSITE_ARG_OBJ };
static MVMCallsite     disp_callsite = { disp_flags, 4, 4, 4, 0 };

/* Are we initialized yet? */
static int initialized = 0;

/* Types we need. */
static MVMObject *Mu                  = NULL;
static MVMObject *Any                 = NULL;
static MVMObject *Int                 = NULL;
static MVMObject *Num                 = NULL;
static MVMObject *Str                 = NULL;
static MVMObject *Scalar              = NULL;
static MVMObject *True                = NULL;
static MVMObject *False               = NULL;
static MVMObject *ContainerDescriptor = NULL;
static MVMObject *Nil                 = NULL;

/* Useful string constants. */
static MVMString *str_return     = NULL;
static MVMString *str_dispatcher = NULL;
static MVMString *str_vivify_for = NULL;
static MVMString *str_perl6      = NULL;
static MVMString *str_p6ex       = NULL;
static MVMString *str_xnodisp    = NULL;
static MVMString *str_xatcf      = NULL;
static MVMString *str_cfr        = NULL;

/* Expose Nil and Mu for containers. */
MVMObject * get_nil() { return Nil; }
MVMObject * get_mu() { return Mu; }

/* Looks up an exception thrower. */
static MVMObject * get_thrower(MVMThreadContext *tc, MVMString *type) {
    MVMObject *ex_hash = MVM_hll_sym_get(tc, str_perl6, str_p6ex);
    return MVM_is_null(tc, ex_hash) ? ex_hash : MVM_repr_at_key_o(tc, ex_hash, type);
}

/* Reports an assignment type check failure. */
void Rakudo_assign_typecheck_failed(MVMThreadContext *tc, MVMObject *cont, MVMObject *obj) {
    MVMObject *thrower = get_thrower(tc, str_xatcf);
    if (!MVM_is_null(tc, thrower)) {
        Rakudo_Scalar *rs = (Rakudo_Scalar *)cont;
        Rakudo_ContainerDescriptor *rcd = (Rakudo_ContainerDescriptor *)rs->descriptor;
        thrower = MVM_frame_find_invokee(tc, thrower, NULL);
        MVM_args_setup_thunk(tc, NULL, MVM_RETURN_VOID, &atcf_callsite);
        tc->cur_frame->args[0].s = rcd->name;
        tc->cur_frame->args[1].o = obj;
        tc->cur_frame->args[2].o = rcd->of;
        STABLE(thrower)->invoke(tc, thrower, &atcf_callsite, tc->cur_frame->args);
    }
    else {
        MVM_exception_throw_adhoc(tc, "Type check failed in assignment");
    }
}

/* Initializes the Perl 6 extension ops. */
static void p6init(MVMThreadContext *tc, MVMuint8 *cur_op) {
    if (!initialized) {
        Rakudo_containers_setup(tc);
    }
}

/* Stashes away various type references. */
#define get_type(tc, hash, name, varname) do { \
    MVMString *key = MVM_string_utf8_decode((tc), (tc)->instance->VMString, (name), strlen((name))); \
    (varname) = MVM_repr_at_key_o((tc), (hash), key); \
    MVM_gc_root_add_permanent_desc(tc, (MVMCollectable **)&varname, name); \
} while (0)
static MVMuint8 s_p6settypes[] = {
    MVM_operand_obj | MVM_operand_read_reg
};
static void p6settypes(MVMThreadContext *tc, MVMuint8 *cur_op) {
    MVMObject *conf = GET_REG(tc, 0).o;
    MVMROOT(tc, conf, {
        get_type(tc, conf, "Mu", Mu);
        get_type(tc, conf, "Any", Any);
        get_type(tc, conf, "Int", Int);
        get_type(tc, conf, "Num", Num);
        get_type(tc, conf, "Str", Str);
        get_type(tc, conf, "Scalar", Scalar);
        get_type(tc, conf, "True", True);
        get_type(tc, conf, "False", False);
        get_type(tc, conf, "ContainerDescriptor", ContainerDescriptor);
        get_type(tc, conf, "Nil", Nil);
    });
    
    /* Strings. */
    str_return = MVM_string_ascii_decode_nt(tc, tc->instance->VMString, "RETURN");
    MVM_gc_root_add_permanent_desc(tc, (MVMCollectable **)&str_return, "RETURN");
    str_dispatcher = MVM_string_ascii_decode_nt(tc, tc->instance->VMString, "$*DISPATCHER");
    MVM_gc_root_add_permanent_desc(tc, (MVMCollectable **)&str_dispatcher, "$*DISPATCHER");
    str_vivify_for = MVM_string_ascii_decode_nt(tc, tc->instance->VMString, "vivify_for");
    MVM_gc_root_add_permanent_desc(tc, (MVMCollectable **)&str_vivify_for, "vivify_for");
    str_perl6 = MVM_string_ascii_decode_nt(tc, tc->instance->VMString, "perl6");
    MVM_gc_root_add_permanent_desc(tc, (MVMCollectable **)&str_perl6, "perl6");
    str_p6ex = MVM_string_ascii_decode_nt(tc, tc->instance->VMString, "P6EX");
    MVM_gc_root_add_permanent_desc(tc, (MVMCollectable **)&str_p6ex, "P6EX");
    str_xnodisp = MVM_string_ascii_decode_nt(tc, tc->instance->VMString, "X::NoDispatcher");
    MVM_gc_root_add_permanent_desc(tc, (MVMCollectable **)&str_xnodisp, "X::NoDispatcher");
    str_xatcf = MVM_string_ascii_decode_nt(tc, tc->instance->VMString, "X::TypeCheck::Assignment");
    MVM_gc_root_add_permanent_desc(tc, (MVMCollectable **)&str_xatcf, "X::TypeCheck::Assignment");
    str_cfr = MVM_string_ascii_decode_nt(tc, tc->instance->VMString, "X::ControlFlow::Return");
    MVM_gc_root_add_permanent_desc(tc, (MVMCollectable **)&str_cfr, "X::ControlFlow::Return");
}

/* Boxing to Perl 6 types. */
static void discover_create(MVMThreadContext *tc, MVMSpeshGraph *g, MVMSpeshIns *ins, MVMObject *type) {
    MVMSpeshFacts *tfacts = MVM_spesh_get_facts(tc, g, ins->operands[0]);
    tfacts->flags |= MVM_SPESH_FACT_CONCRETE | MVM_SPESH_FACT_KNOWN_TYPE | MVM_SPESH_FACT_DECONTED;
    tfacts->type   = type;
}

static void p6box_i_discover(MVMThreadContext *tc, MVMSpeshGraph *g, MVMSpeshIns *ins) {
    discover_create(tc, g, ins, Int);
#ifdef MVM_SPESH_FACT_KNOWN_BOX_SRC
    MVM_spesh_get_facts(tc, g, ins->operands[0])->flags |= MVM_SPESH_FACT_KNOWN_BOX_SRC;
#endif
}
static void p6box_n_discover(MVMThreadContext *tc, MVMSpeshGraph *g, MVMSpeshIns *ins) {
    discover_create(tc, g, ins, Num);
#ifdef MVM_SPESH_FACT_KNOWN_BOX_SRC
    MVM_spesh_get_facts(tc, g, ins->operands[0])->flags |= MVM_SPESH_FACT_KNOWN_BOX_SRC;
#endif
}
static void p6box_s_discover(MVMThreadContext *tc, MVMSpeshGraph *g, MVMSpeshIns *ins) {
    discover_create(tc, g, ins, Str);
#ifdef MVM_SPESH_FACT_KNOWN_BOX_SRC
    MVM_spesh_get_facts(tc, g, ins->operands[0])->flags |= MVM_SPESH_FACT_KNOWN_BOX_SRC;
#endif
}
static void p6box_u_discover(MVMThreadContext *tc, MVMSpeshGraph *g, MVMSpeshIns *ins) {
    discover_create(tc, g, ins, Int);
#ifdef MVM_SPESH_FACT_KNOWN_BOX_SRC
    MVM_spesh_get_facts(tc, g, ins->operands[0])->flags |= MVM_SPESH_FACT_KNOWN_BOX_SRC;
#endif
}

static MVMuint8 s_p6box_i[] = {
    MVM_operand_obj | MVM_operand_write_reg,
    MVM_operand_int64 | MVM_operand_read_reg,
};
static void p6box_i(MVMThreadContext *tc, MVMuint8 *cur_op) {
     GET_REG(tc, 0).o = MVM_repr_box_int(tc, Int, GET_REG(tc, 2).i64);
}
static MVMuint8 s_p6box_n[] = {
    MVM_operand_obj | MVM_operand_write_reg,
    MVM_operand_num64 | MVM_operand_read_reg,
};
static void p6box_n(MVMThreadContext *tc, MVMuint8 *cur_op) {
     GET_REG(tc, 0).o = MVM_repr_box_num(tc, Num, GET_REG(tc, 2).n64);
}
static MVMuint8 s_p6box_s[] = {
    MVM_operand_obj | MVM_operand_write_reg,
    MVM_operand_str | MVM_operand_read_reg,
};
static void p6box_s(MVMThreadContext *tc, MVMuint8 *cur_op) {
     GET_REG(tc, 0).o = MVM_repr_box_str(tc, Str, GET_REG(tc, 2).s);
}
static MVMuint8 s_p6box_u[] = {
    MVM_operand_obj | MVM_operand_write_reg,
    MVM_operand_uint64 | MVM_operand_read_reg,
};
static void p6box_u(MVMThreadContext *tc, MVMuint8 *cur_op) {
     GET_REG(tc, 0).o = MVM_repr_box_uint(tc, Int, GET_REG(tc, 2).u64);
}

/* Turns zero to False and non-zero to True. */
static MVMuint8 s_p6bool[] = {
    MVM_operand_obj | MVM_operand_write_reg,
    MVM_operand_int64 | MVM_operand_read_reg,
};
static void p6bool(MVMThreadContext *tc, MVMuint8 *cur_op) {
     GET_REG(tc, 0).o = GET_REG(tc, 2).i64 ? True : False;
}
static void p6bool_discover(MVMThreadContext *tc, MVMSpeshGraph *g, MVMSpeshIns *ins) {
    discover_create(tc, g, ins, STABLE(True)->WHAT);
#ifdef MVM_SPESH_FACT_KNOWN_BOX_SRC
    MVM_spesh_get_facts(tc, g, ins->operands[0])->flags |= MVM_SPESH_FACT_KNOWN_BOX_SRC;
#endif
}

/* The .VAR operation. Wraps in an outer Scalar container so we can actually
 * operate on the underlying Scalar, if we have a container. */
static MVMuint8 s_p6var[] = {
    MVM_operand_obj | MVM_operand_write_reg,
    MVM_operand_obj | MVM_operand_read_reg,
};
static void p6var(MVMThreadContext *tc, MVMuint8 *cur_op) {
     MVMObject *wrappee = GET_REG(tc, 2).o;
     if (STABLE(wrappee)->container_spec) {
        MVMROOT(tc, wrappee, {
            MVMObject *wrapper = MVM_repr_alloc_init(tc, Scalar);
            MVM_ASSIGN_REF(tc, &(wrapper->header), ((Rakudo_Scalar *)wrapper)->value, wrappee);
            GET_REG(tc, 0).o = wrapper;
        });
     }
     else {
        GET_REG(tc, 0).o = wrappee;
     }
}

static MVMuint8 s_p6reprname[] = {
    MVM_operand_obj | MVM_operand_write_reg,
    MVM_operand_obj | MVM_operand_read_reg,
};
static void p6reprname(MVMThreadContext *tc, MVMuint8 *cur_op) {
    MVMObject *obj = GET_REG(tc, 2).o;
    MVMROOT(tc, obj, {
        MVMObject *name = MVM_repr_alloc_init(tc, tc->instance->boot_types.BOOTStr);
        MVMROOT(tc, name, {
            MVMString *str  = MVM_string_utf8_decode(tc, tc->instance->VMString,
                obj->st->REPR->name, strlen(obj->st->REPR->name));
            MVM_repr_set_str(tc, name, str);
            GET_REG(tc, 0).o = name;
        });
    });
}
static void p6reprname_discover(MVMThreadContext *tc, MVMSpeshGraph *g, MVMSpeshIns *ins) {
    discover_create(tc, g, ins, tc->instance->boot_types.BOOTStr);
}

/* Decontainerizes the return value of a routine as needed. */
static MVMuint8 s_p6decontrv[] = {
    MVM_operand_obj | MVM_operand_write_reg,
    MVM_operand_obj | MVM_operand_read_reg,
};
static MVMObject *Iterable = NULL;
static void p6decontrv(MVMThreadContext *tc, MVMuint8 *cur_op) {
    MVMObject *retval;
    if (!Iterable)
        Iterable = MVM_frame_find_lexical_by_name(tc,
            MVM_string_ascii_decode_nt(tc, tc->instance->VMString, "Iterable"),
            MVM_reg_obj)->o;
    retval = GET_REG(tc, 2).o;
    if (MVM_is_null(tc, retval)) {
       retval = Mu;
    }
    else if (IS_CONCRETE(retval)) {
        const MVMContainerSpec *spec = STABLE(retval)->container_spec;
        if (spec == Rakudo_containers_get_scalar()) {
            Rakudo_ContainerDescriptor *cd = (Rakudo_ContainerDescriptor *)
                ((Rakudo_Scalar *)retval)->descriptor;
            if (!MVM_is_null(tc, (MVMObject *)cd) && cd->rw) {
                MVMObject *value = ((Rakudo_Scalar *)retval)->value;
                if (MVM_6model_istype_cache_only(tc, value, Iterable)) {
                    MVMROOT(tc, value, {
                        MVMObject *cont = MVM_repr_alloc_init(tc, Scalar);
                        MVM_ASSIGN_REF(tc, &(cont->header), ((Rakudo_Scalar *)cont)->value,
                            value);
                        retval = cont;
                    });
                }
                else {
                    retval = value;
                }
            }
        }
        else if (spec && spec->fetch_never_invokes) {
            MVMRegister res;
            spec->fetch(tc, retval, &res);
            retval = res.o;
        }
    }
    GET_REG(tc, 0).o = retval;
}
static void p6decontrv_spesh(MVMThreadContext *tc, MVMSpeshGraph *g, MVMSpeshBB *bb, MVMSpeshIns *ins) {
    /* If it's already deconted, can just become a set. */
    MVMSpeshFacts *obj_facts = MVM_spesh_get_and_use_facts(tc, g, ins->operands[1]);
    if (obj_facts->flags & (MVM_SPESH_FACT_DECONTED | MVM_SPESH_FACT_TYPEOBJ)) {
        MVMSpeshFacts *res_facts = MVM_spesh_get_facts(tc, g, ins->operands[0]);
        ins->info = MVM_op_get_op(MVM_OP_set);
        res_facts->flags = obj_facts->flags;
        res_facts->type = obj_facts->type;
    }
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
#define RAKUDO_FIRST_FLAG 128

static MVMuint8 s_p6setfirstflag[] = {
    MVM_operand_obj | MVM_operand_write_reg,
    MVM_operand_obj | MVM_operand_read_reg
};
static void p6setfirstflag(MVMThreadContext *tc, MVMuint8 *cur_op) {
    MVMObject *code_obj = GET_REG(tc, 2).o;
    MVMObject *vm_code  = MVM_frame_find_invokee(tc, code_obj, NULL);
    vm_code->header.flags |= RAKUDO_FIRST_FLAG;
    GET_REG(tc, 0).o = code_obj;
}

static MVMuint8 s_p6takefirstflag[] = {
    MVM_operand_int64 | MVM_operand_write_reg
};
static void p6takefirstflag(MVMThreadContext *tc, MVMuint8 *cur_op) {
    MVMObject *vm_code = tc->cur_frame->code_ref;
    if (vm_code->header.flags & RAKUDO_FIRST_FLAG) {
        vm_code->header.flags ^= RAKUDO_FIRST_FLAG;
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

static MVMuint8 s_p6finddispatcher[] = {
    MVM_operand_obj | MVM_operand_write_reg,
    MVM_operand_str | MVM_operand_read_reg
};
void store_dispatcher(MVMThreadContext *tc, void *sr_data) {
    MVMRegister **srd = (MVMRegister **)sr_data;
    srd[0]->o = srd[1]->o;
    free(srd);
}
static void p6finddispatcher(MVMThreadContext *tc, MVMuint8 *cur_op) {
    MVMFrame *ctx = MVM_frame_force_to_heap(tc, tc->cur_frame);
    ctx = tc->cur_frame->caller; /* Skip over routine using this op. */
    while (ctx) {
        /* Do we have a dispatcher here? */
        MVMRegister *disp_lex;
        MVMROOT(tc, ctx, {
            disp_lex = MVM_frame_try_get_lexical(tc, ctx, str_dispatcher, MVM_reg_obj);
        });
        if (disp_lex) {
            MVMObject *maybe_dispatcher = disp_lex->o;
            if (!MVM_is_null(tc, maybe_dispatcher)) {
                MVMObject *dispatcher = maybe_dispatcher;
                if (!IS_CONCRETE(dispatcher)) {
                    /* Need to vivify it, by calling vivify_for method. Prepare
                     * things we need to pass to it*/
                    MVMObject *meth, *p6sub, *ctx_ref, *capture;
                    MVMRegister *res_reg = &GET_REG(tc, 0);
                    MVMROOT(tc, dispatcher, {
                    MVMROOT(tc, ctx, {
                        ctx_ref = MVM_repr_alloc_init(tc, tc->instance->boot_types.BOOTContext);
                        MVM_ASSIGN_REF(tc, &(ctx_ref->header),
                                ((MVMContext *)ctx_ref)->body.context, ctx);
                        MVMROOT(tc, ctx_ref, {
                            capture = MVM_args_use_capture(tc, ctx);
                            MVMROOT(tc, capture, {
                                p6sub = MVM_frame_get_code_object(tc, (MVMCode *)ctx->code_ref);
                                MVMROOT(tc, p6sub, {
                                    meth = MVM_6model_find_method_cache_only(tc, dispatcher, str_vivify_for);
                                });
                            });
                        });
                    });
                    });

                    /* Lookup method, invoke it, and set up callback to ensure it
                     * is also stored in the lexical. */
                    meth = MVM_frame_find_invokee(tc, meth, NULL);
                    *(tc->interp_cur_op) += 4; /* Get right return address. */
                    MVM_args_setup_thunk(tc, res_reg, MVM_RETURN_OBJ, &disp_callsite);
                    {
                        MVMRegister **srd = malloc(2 * sizeof(MVMObject *));
                        srd[0] = disp_lex;
                        srd[1] = res_reg;
                        MVM_frame_special_return(tc, tc->cur_frame, store_dispatcher,
                            NULL, srd, NULL);
                    }
                    tc->cur_frame->args[0].o = dispatcher;
                    tc->cur_frame->args[1].o = p6sub;
                    tc->cur_frame->args[2].o = ctx_ref;
                    tc->cur_frame->args[3].o = capture;
                    STABLE(meth)->invoke(tc, meth, &disp_callsite, tc->cur_frame->args);
                    return;
                }
                else {
                    GET_REG(tc, 0).o = dispatcher;
                    return;
                }
            }
        }

        /* Follow dynamic chain. */
        ctx = ctx->caller;
    }

    {
        MVMObject *thrower = get_thrower(tc, str_xnodisp);
        MVMString *usage   = GET_REG(tc, 2).s;
        if (!MVM_is_null(tc, thrower)) {
            thrower = MVM_frame_find_invokee(tc, thrower, NULL);
            *(tc->interp_cur_op) += 4; /* Get right return address. */
            MVM_args_setup_thunk(tc, NULL, MVM_RETURN_VOID, &one_str_callsite);
            tc->cur_frame->args[0].s = usage;
            STABLE(thrower)->invoke(tc, thrower, &one_str_callsite, tc->cur_frame->args);
        }
        else {
            MVM_exception_throw_adhoc(tc,
                "%s is not in the dynamic scope of a dispatcher",
                MVM_string_utf8_encode_C_string(tc, usage));
        }
    }
}

static MVMuint8 s_p6argsfordispatcher[] = {
    MVM_operand_obj | MVM_operand_write_reg,
    MVM_operand_obj | MVM_operand_read_reg
};
static void p6argsfordispatcher(MVMThreadContext *tc, MVMuint8 *cur_op) {
    MVMFrame  *ctx = tc->cur_frame;
    while (ctx) {
        /* Do we have the dispatcher we're looking for? */
        MVMRegister *disp_lex;
        MVMROOT(tc, ctx, {
            disp_lex = MVM_frame_try_get_lexical(tc, ctx, str_dispatcher, MVM_reg_obj);
        });
        if (disp_lex) {
            MVMObject *maybe_dispatcher = disp_lex->o;
            MVMObject *disp             = GET_REG(tc, 2).o;
            if (maybe_dispatcher == disp) {
                GET_REG(tc, 0).o = MVM_args_use_capture(tc, ctx);
                return;
            }
        }

        /* Follow dynamic chain. */
        ctx = ctx->caller;
    }

    MVM_exception_throw_adhoc(tc, "Could not find arguments for dispatcher");
}

static MVMuint8 s_p6decodelocaltime[] = {
    MVM_operand_obj   | MVM_operand_write_reg,
    MVM_operand_int64 | MVM_operand_read_reg
};
static void p6decodelocaltime(MVMThreadContext *tc, MVMuint8 *cur_op) {
    MVMObject *result = MVM_repr_alloc_init(tc, tc->instance->boot_types.BOOTIntArray);
    const time_t t = (time_t)GET_REG(tc, 2).i64;
    struct tm tm;
#ifdef _WIN32
    tm = *localtime(&t);
#else
    localtime_r(&t, &tm);
#endif

    MVMROOT(tc, result, {
        REPR(result)->pos_funcs.set_elems(tc, STABLE(result), result, OBJECT_BODY(result), 9);
        MVM_repr_bind_pos_i(tc, result, 0, (&tm)->tm_sec);
        MVM_repr_bind_pos_i(tc, result, 1, (&tm)->tm_min);
        MVM_repr_bind_pos_i(tc, result, 2, (&tm)->tm_hour);
        MVM_repr_bind_pos_i(tc, result, 3, (&tm)->tm_mday);
        MVM_repr_bind_pos_i(tc, result, 4, (&tm)->tm_mon + 1);
        MVM_repr_bind_pos_i(tc, result, 5, (&tm)->tm_year + 1900);
        MVM_repr_bind_pos_i(tc, result, 6, (&tm)->tm_wday);
        MVM_repr_bind_pos_i(tc, result, 7, (&tm)->tm_yday);
        MVM_repr_bind_pos_i(tc, result, 8, (&tm)->tm_isdst);
    });

    GET_REG(tc, 0).o = result;
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
    MVM_ext_register_extop(tc, "p6box_i",  p6box_i, 2, s_p6box_i, NULL, p6box_i_discover, MVM_EXTOP_PURE | MVM_EXTOP_ALLOCATING);
    MVM_ext_register_extop(tc, "p6box_n",  p6box_n, 2, s_p6box_n, NULL, p6box_n_discover, MVM_EXTOP_PURE | MVM_EXTOP_ALLOCATING);
    MVM_ext_register_extop(tc, "p6box_s",  p6box_s, 2, s_p6box_s, NULL, p6box_s_discover, MVM_EXTOP_PURE | MVM_EXTOP_ALLOCATING);
    MVM_ext_register_extop(tc, "p6box_u",  p6box_u, 2, s_p6box_u, NULL, p6box_u_discover, MVM_EXTOP_PURE | MVM_EXTOP_ALLOCATING);
    MVM_ext_register_extop(tc, "p6settypes",  p6settypes, 1, s_p6settypes, NULL, NULL, 0);
    MVM_ext_register_extop(tc, "p6bool",  p6bool, 2, s_p6bool, NULL, p6bool_discover, MVM_EXTOP_PURE);
    MVM_ext_register_extop(tc, "p6var",  p6var, 2, s_p6var, NULL, NULL, MVM_EXTOP_PURE | MVM_EXTOP_ALLOCATING);
    MVM_ext_register_extop(tc, "p6reprname",  p6reprname, 2, s_p6reprname, NULL, p6reprname_discover, MVM_EXTOP_PURE | MVM_EXTOP_ALLOCATING);
    MVM_ext_register_extop(tc, "p6decontrv",  p6decontrv, 2, s_p6decontrv, p6decontrv_spesh, NULL, MVM_EXTOP_PURE);
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
    MVM_ext_register_extop(tc, "p6finddispatcher", p6finddispatcher, 2, s_p6finddispatcher, NULL, NULL, MVM_EXTOP_NO_JIT);
    MVM_ext_register_extop(tc, "p6argsfordispatcher", p6argsfordispatcher, 2, s_p6argsfordispatcher, NULL, NULL, 0);
    MVM_ext_register_extop(tc, "p6decodelocaltime", p6decodelocaltime, 2, s_p6decodelocaltime, NULL, NULL, MVM_EXTOP_ALLOCATING);
    MVM_ext_register_extop(tc, "p6staticouter", p6staticouter, 2, s_p6staticouter, NULL, NULL, 0);
    MVM_ext_register_extop(tc, "p6invokeunder", p6invokeunder, 3, s_p6invokeunder, NULL, NULL, MVM_EXTOP_INVOKISH);
}
