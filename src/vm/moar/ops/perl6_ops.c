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

#define GET_REG(tc, idx)    (*tc->interp_reg_base)[*((MVMuint16 *)(*tc->interp_cur_op + idx))]
#define REAL_BODY(tc, obj)  MVM_p6opaque_real_data(tc, OBJECT_BODY(obj))

/* Dummy zero and one-arg callsite. */
static MVMCallsite      no_arg_callsite = { NULL, 0, 0, 0 };
static MVMCallsiteEntry one_arg_flags[] = { MVM_CALLSITE_ARG_OBJ };
static MVMCallsite     one_arg_callsite = { one_arg_flags, 1, 1, 0 };
static MVMCallsiteEntry one_str_flags[] = { MVM_CALLSITE_ARG_STR };
static MVMCallsite     one_str_callsite = { one_str_flags, 1, 1, 0 };

/* Assignment type check failed callsite. */
static MVMCallsiteEntry atcf_flags[] = { MVM_CALLSITE_ARG_STR, MVM_CALLSITE_ARG_OBJ, 
                                         MVM_CALLSITE_ARG_OBJ };
static MVMCallsite     atcf_callsite = { atcf_flags, 3, 3, 0 };

/* Dispatcher vivify_for callsite. */
static MVMCallsiteEntry disp_flags[] = { MVM_CALLSITE_ARG_OBJ, MVM_CALLSITE_ARG_OBJ, 
                                         MVM_CALLSITE_ARG_OBJ, MVM_CALLSITE_ARG_OBJ };
static MVMCallsite     disp_callsite = { disp_flags, 4, 4, 0 };

/* Are we initialized yet? */
static int initialized = 0;

/* Types we need. */
static MVMObject *Mu                  = NULL;
static MVMObject *Any                 = NULL;
static MVMObject *Int                 = NULL;
static MVMObject *Num                 = NULL;
static MVMObject *Str                 = NULL;
static MVMObject *Scalar              = NULL;
static MVMObject *Parcel              = NULL;
static MVMObject *List                = NULL;
static MVMObject *ListIter            = NULL;
static MVMObject *True                = NULL;
static MVMObject *False               = NULL;
static MVMObject *ContainerDescriptor = NULL;
static MVMObject *Nil                 = NULL;

/* Default container descriptor. */
static MVMObject *default_cont_desc = NULL;

/* Useful string constants. */
static MVMString *str_return     = NULL;
static MVMString *str_dispatcher = NULL;
static MVMString *str_vivify_for = NULL;
static MVMString *str_perl6      = NULL;
static MVMString *str_p6ex       = NULL;
static MVMString *str_xnodisp    = NULL;
static MVMString *str_xatcf      = NULL;
static MVMString *str_cfr        = NULL;

/* Parcel, as laid out as a P6opaque. */
typedef struct {
    MVMP6opaque  p6o;
    MVMObject   *storage;
} Rakudo_Parcel;

/* ListIter, as laid out as a P6opaque. */
typedef struct {
    MVMP6opaque  p6o;
    MVMObject   *reified;
    MVMObject   *nextiter;
    MVMObject   *rest;
    MVMObject   *list;
} Rakudo_ListIter;

/* List, as laid out as a P6opaque. */
typedef struct {
    MVMP6opaqueBody  p6o;
    MVMObject       *items;
    MVMObject       *flattens;
    MVMObject       *nextiter;
} Rakudo_List;

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
static void p6init(MVMThreadContext *tc) {
    if (!initialized) {
        Rakudo_containers_setup(tc);
    }
}

/* Stashes away various type references. */
#define get_type(tc, hash, name, varname) do { \
    MVMString *key = MVM_string_utf8_decode((tc), (tc)->instance->VMString, (name), strlen((name))); \
    (varname) = MVM_repr_at_key_o((tc), (hash), key); \
    MVM_gc_root_add_permanent(tc, (MVMCollectable **)&varname); \
} while (0)
static MVMuint8 s_p6settypes[] = {
    MVM_operand_obj | MVM_operand_read_reg
};
static void p6settypes(MVMThreadContext *tc) {
    MVMObject *conf = GET_REG(tc, 0).o;
    MVMROOT(tc, conf, {
        get_type(tc, conf, "Mu", Mu);
        get_type(tc, conf, "Any", Any);
        get_type(tc, conf, "Int", Int);
        get_type(tc, conf, "Num", Num);
        get_type(tc, conf, "Str", Str);
        get_type(tc, conf, "Scalar", Scalar);
        get_type(tc, conf, "Parcel", Parcel);
        get_type(tc, conf, "List", List);
        get_type(tc, conf, "ListIter", ListIter);
        get_type(tc, conf, "True", True);
        get_type(tc, conf, "False", False);
        get_type(tc, conf, "ContainerDescriptor", ContainerDescriptor);
        get_type(tc, conf, "Nil", Nil);
    });
    
    /* Set up default container descriptor. */
    {
        MVMString *element;
        default_cont_desc = MVM_repr_alloc_init(tc, ContainerDescriptor);
        MVM_gc_root_add_permanent(tc, (MVMCollectable **)&default_cont_desc);
        element = MVM_string_ascii_decode_nt(tc, tc->instance->VMString, "<element>");
        MVM_ASSIGN_REF(tc, &(default_cont_desc->header),
            ((Rakudo_ContainerDescriptor *)default_cont_desc)->of, Mu);
        MVM_ASSIGN_REF(tc, &(default_cont_desc->header),
            ((Rakudo_ContainerDescriptor *)default_cont_desc)->name, element);
        ((Rakudo_ContainerDescriptor *)default_cont_desc)->rw = 1;
        MVM_ASSIGN_REF(tc, &(default_cont_desc->header),
            ((Rakudo_ContainerDescriptor *)default_cont_desc)->the_default, Any);
    }

    /* Strings. */
    str_return = MVM_string_ascii_decode_nt(tc, tc->instance->VMString, "RETURN");
    MVM_gc_root_add_permanent(tc, (MVMCollectable **)&str_return);
    str_dispatcher = MVM_string_ascii_decode_nt(tc, tc->instance->VMString, "$*DISPATCHER");
    MVM_gc_root_add_permanent(tc, (MVMCollectable **)&str_dispatcher);
    str_vivify_for = MVM_string_ascii_decode_nt(tc, tc->instance->VMString, "vivify_for");
    MVM_gc_root_add_permanent(tc, (MVMCollectable **)&str_vivify_for);
    str_perl6 = MVM_string_ascii_decode_nt(tc, tc->instance->VMString, "perl6");
    MVM_gc_root_add_permanent(tc, (MVMCollectable **)&str_perl6);
    str_p6ex = MVM_string_ascii_decode_nt(tc, tc->instance->VMString, "P6EX");
    MVM_gc_root_add_permanent(tc, (MVMCollectable **)&str_p6ex);
    str_xnodisp = MVM_string_ascii_decode_nt(tc, tc->instance->VMString, "X::NoDispatcher");
    MVM_gc_root_add_permanent(tc, (MVMCollectable **)&str_xnodisp);
    str_xatcf = MVM_string_ascii_decode_nt(tc, tc->instance->VMString, "X::TypeCheck::Assignment");
    MVM_gc_root_add_permanent(tc, (MVMCollectable **)&str_xatcf);
    str_cfr = MVM_string_ascii_decode_nt(tc, tc->instance->VMString, "X::ControlFlow::Return");
    MVM_gc_root_add_permanent(tc, (MVMCollectable **)&str_cfr);
}

/* Boxing to Perl 6 types. */
static MVMuint8 s_p6box_i[] = {
    MVM_operand_obj | MVM_operand_write_reg,
    MVM_operand_int64 | MVM_operand_read_reg,
};
static void p6box_i(MVMThreadContext *tc) {
     GET_REG(tc, 0).o = MVM_repr_box_int(tc, Int, GET_REG(tc, 2).i64);
}
static MVMuint8 s_p6box_n[] = {
    MVM_operand_obj | MVM_operand_write_reg,
    MVM_operand_num64 | MVM_operand_read_reg,
};
static void p6box_n(MVMThreadContext *tc) {
     GET_REG(tc, 0).o = MVM_repr_box_num(tc, Num, GET_REG(tc, 2).n64);
}
static MVMuint8 s_p6box_s[] = {
    MVM_operand_obj | MVM_operand_write_reg,
    MVM_operand_str | MVM_operand_read_reg,
};
static void p6box_s(MVMThreadContext *tc) {
     GET_REG(tc, 0).o = MVM_repr_box_str(tc, Str, GET_REG(tc, 2).s);
}

static MVMuint8 s_p6parcel[] = {
    MVM_operand_obj | MVM_operand_write_reg,
    MVM_operand_obj | MVM_operand_read_reg,
    MVM_operand_obj | MVM_operand_read_reg
};
static void p6parcel(MVMThreadContext *tc) {
    MVMObject *parcel = MVM_repr_alloc_init(tc, Parcel);
    MVMObject *vmarr  = GET_REG(tc, 2).o;
    MVMObject *fill   = GET_REG(tc, 4).o;
    MVM_ASSIGN_REF(tc, &(parcel->header), ((Rakudo_Parcel *)parcel)->storage, vmarr);

    if (!MVM_is_null(tc, fill)) {
        MVMint64 elems = MVM_repr_elems(tc, vmarr);
        MVMint64 i;
        for (i = 0; i < elems; i++)
            if (MVM_is_null(tc, MVM_repr_at_pos_o(tc, vmarr, i)))
                MVM_repr_bind_pos_o(tc, vmarr, i, fill);
    }

    GET_REG(tc, 0).o = parcel;
}

/* Produces a lazy Perl 6 list of the specified type with the given items. */
static MVMObject * make_listiter(MVMThreadContext *tc, MVMObject *items, MVMObject *list) {
    MVMObject *result;
    MVMROOT(tc, items, {
    MVMROOT(tc, list, {
        result = MVM_repr_alloc_init(tc, ListIter);
        MVM_ASSIGN_REF(tc, &(result->header), ((Rakudo_ListIter *)result)->rest, items);
        MVM_ASSIGN_REF(tc, &(result->header), ((Rakudo_ListIter *)result)->list, list);
    });
    });
    return result;
}
static MVMuint8 s_p6list[] = {
    MVM_operand_obj | MVM_operand_write_reg,
    MVM_operand_obj | MVM_operand_read_reg,
    MVM_operand_obj | MVM_operand_read_reg,
    MVM_operand_obj | MVM_operand_read_reg
};
static void p6list(MVMThreadContext *tc) {
     MVMObject *list = MVM_repr_alloc_init(tc, GET_REG(tc, 4).o);
     if (MVM_6model_istype_cache_only(tc, list, List)) {
        MVMROOT(tc, list, {
            MVMObject *items = GET_REG(tc, 2).o;
            if (!MVM_is_null(tc, items)) {
                MVMObject *iter = make_listiter(tc, items, list);
                MVM_ASSIGN_REF(tc, &(list->header), ((Rakudo_List *)REAL_BODY(tc, list))->nextiter, iter);
            }
            MVM_ASSIGN_REF(tc, &(list->header), ((Rakudo_List *)REAL_BODY(tc, list))->flattens, GET_REG(tc, 6).o);
        });
        GET_REG(tc, 0).o = list;
    }
    else {
        MVM_exception_throw_adhoc(tc, "p6list may only be used on a List");
    }
}

static MVMuint8 s_p6listiter[] = {
    MVM_operand_obj | MVM_operand_write_reg,
    MVM_operand_obj | MVM_operand_read_reg,
    MVM_operand_obj | MVM_operand_read_reg
};
static void p6listiter(MVMThreadContext *tc) {
    MVMObject  *arr = GET_REG(tc, 2).o;
    MVMObject *list = GET_REG(tc, 4).o;
    GET_REG(tc, 0).o = make_listiter(tc, arr, list);
}

/* Returns the $!items attribute of a List, vivifying it to a
 * low-level array if it isn't one already. */
static MVMuint8 s_p6listitems[] = {
    MVM_operand_obj | MVM_operand_write_reg,
    MVM_operand_obj | MVM_operand_read_reg
};
static void p6listitems(MVMThreadContext *tc) {
     MVMObject *list = GET_REG(tc, 2).o;
     if (MVM_6model_istype_cache_only(tc, list, List)) {
        MVMObject *items = ((Rakudo_List *)REAL_BODY(tc, list))->items;
        if (MVM_is_null(tc, items) || !IS_CONCRETE(items) || REPR(items)->ID != MVM_REPR_ID_MVMArray) {
            MVMROOT(tc, list, {
                items = MVM_repr_alloc_init(tc, tc->instance->boot_types.BOOTArray);
                MVM_ASSIGN_REF(tc, &(list->header), ((Rakudo_List *)REAL_BODY(tc, list))->items, items);
            });
        }
        GET_REG(tc, 0).o = items;
     }
     else {
        MVM_exception_throw_adhoc(tc, "p6listitems may only be used on a List");
     }
}

/* Turns zero to False and non-zero to True. */
static MVMuint8 s_p6bool[] = {
    MVM_operand_obj | MVM_operand_write_reg,
    MVM_operand_int64 | MVM_operand_read_reg,
};
static void p6bool(MVMThreadContext *tc) {
     GET_REG(tc, 0).o = GET_REG(tc, 2).i64 ? True : False;
}

/* Creates a Scalar from the specified descriptor. */
static MVMuint8 s_p6scalarfromdesc[] = {
    MVM_operand_obj | MVM_operand_write_reg,
    MVM_operand_obj | MVM_operand_read_reg,
};
static void p6scalarfromdesc(MVMThreadContext *tc) {
    MVMObject *new_scalar = MVM_repr_alloc_init(tc, Scalar);
    MVMObject *descriptor = GET_REG(tc, 2).o;
    if (MVM_is_null(tc, descriptor)) {
        descriptor = default_cont_desc;
    }
    MVM_ASSIGN_REF(tc, &(new_scalar->header), ((Rakudo_Scalar *)new_scalar)->descriptor, descriptor);
    MVM_ASSIGN_REF(tc, &(new_scalar->header), ((Rakudo_Scalar *)new_scalar)->value,
        ((Rakudo_ContainerDescriptor *)descriptor)->the_default);
    GET_REG(tc, 0).o = new_scalar;
}

static MVMuint8 s_p6recont_ro[] = {
    MVM_operand_obj | MVM_operand_write_reg,
    MVM_operand_obj | MVM_operand_read_reg,
};
static void p6recont_ro(MVMThreadContext *tc) {
    MVMObject *check = GET_REG(tc, 2).o;
    if (STABLE(check)->container_spec == Rakudo_containers_get_scalar()) {
        MVMObject *desc = ((Rakudo_Scalar *)check)->descriptor;
        if (!MVM_is_null(tc, desc) && ((Rakudo_ContainerDescriptor *)desc)->rw) {
            /* We have an rw container; re-containerize it. */
            MVMROOT(tc, check, {
                MVMObject *result = MVM_repr_alloc_init(tc, Scalar);
                MVM_ASSIGN_REF(tc, &(result->header), ((Rakudo_Scalar *)result)->value,
                    ((Rakudo_Scalar *)check)->value);
                GET_REG(tc, 0).o = result;
            });
            return;
        }
    }
    GET_REG(tc, 0).o = check;
}

/* The .VAR operation. Wraps in an outer Scalar container so we can actually
 * operate on the underlying Scalar, if we have a container. */
static MVMuint8 s_p6var[] = {
    MVM_operand_obj | MVM_operand_write_reg,
    MVM_operand_obj | MVM_operand_read_reg,
};
static void p6var(MVMThreadContext *tc) {
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
static void p6reprname(MVMThreadContext *tc) {
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

/* Decontainerizes the return value of a routine as needed. */
static MVMuint8 s_p6decontrv[] = {
    MVM_operand_obj | MVM_operand_write_reg,
    MVM_operand_obj | MVM_operand_read_reg,
};
static void p6decontrv(MVMThreadContext *tc) {
    MVMObject *retval = GET_REG(tc, 2).o;
    if (MVM_is_null(tc, retval)) {
       retval = Mu;
    }
    else if (IS_CONCRETE(retval) && STABLE(retval)->container_spec == Rakudo_containers_get_scalar()) {
        Rakudo_ContainerDescriptor *cd = (Rakudo_ContainerDescriptor *)
            ((Rakudo_Scalar *)retval)->descriptor;
        if (!MVM_is_null(tc, (MVMObject *)cd) && cd->rw) {
            MVMROOT(tc, retval, {
                MVMObject *cont = MVM_repr_alloc_init(tc, Scalar);
                MVM_ASSIGN_REF(tc, &(cont->header), ((Rakudo_Scalar *)cont)->value,
                    ((Rakudo_Scalar *)retval)->value);
                retval = cont;
            });
        }
    }
    GET_REG(tc, 0).o = retval;
}

static MVMuint8 s_p6routinereturn[] = {
    MVM_operand_obj | MVM_operand_write_reg,
    MVM_operand_obj | MVM_operand_read_reg,
};
static void p6routinereturn(MVMThreadContext *tc) {
    MVMObject *ret = MVM_frame_find_lexical_by_name_rel_caller(tc, str_return,
        tc->cur_frame)->o;
    if (!MVM_is_null(tc, ret) && IS_CONCRETE(ret) && REPR(ret)->ID == MVM_REPR_ID_Lexotic) {
        MVM_args_setup_thunk(tc, NULL, MVM_RETURN_VOID, &one_arg_callsite);
        tc->cur_frame->args[0].o = GET_REG(tc, 2).o;
        STABLE(ret)->invoke(tc, ret, &one_arg_callsite, tc->cur_frame->args);
        *(tc->interp_cur_op) -= 4; /* Oh my, what a hack... */
    }
    else {
        MVMObject *thrower = get_thrower(tc, str_cfr);
        if (!MVM_is_null(tc, thrower)) {
            thrower = MVM_frame_find_invokee(tc, thrower, NULL);
            MVM_args_setup_thunk(tc, NULL, MVM_RETURN_VOID, &no_arg_callsite);
            STABLE(thrower)->invoke(tc, thrower, &no_arg_callsite, tc->cur_frame->args);
        } else {
            MVM_exception_throw_adhoc(tc, "Attempt to return outside of any Routine");
        }
    }
}

static MVMuint8 s_p6capturelex[] = {
    MVM_operand_obj | MVM_operand_write_reg,
    MVM_operand_obj | MVM_operand_read_reg,
};
static void p6capturelex(MVMThreadContext *tc) {
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
static void p6capturelexwhere(MVMThreadContext *tc) {
    MVMObject *p6_code_obj = GET_REG(tc, 2).o;
    MVMObject *vm_code_obj = MVM_frame_find_invokee(tc, p6_code_obj, NULL);
    if (REPR(vm_code_obj)->ID == MVM_REPR_ID_MVMCode) {
        MVMFrame *find = tc->cur_frame;
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
    GET_REG(tc, 0).o = p6_code_obj;
}

static MVMuint8 s_p6getouterctx[] = {
    MVM_operand_obj | MVM_operand_write_reg,
    MVM_operand_obj | MVM_operand_read_reg
};
static void p6getouterctx(MVMThreadContext *tc) {
    MVMObject *p6_code_obj = GET_REG(tc, 2).o;
    MVMObject *vm_code_obj = MVM_frame_find_invokee(tc, p6_code_obj, NULL);
    MVMFrame  *outer       = ((MVMCode *)vm_code_obj)->body.outer;
    MVMObject *ctx         = MVM_repr_alloc_init(tc, tc->instance->boot_types.BOOTContext);
    if (!outer)
        MVM_exception_throw_adhoc(tc, "Specified code ref has no outer");
    ((MVMContext *)ctx)->body.context = MVM_frame_inc_ref(tc, outer);
    GET_REG(tc, 0).o = ctx;
}

static MVMuint8 s_p6captureouters[] = {
    MVM_operand_obj | MVM_operand_read_reg,
    MVM_operand_obj | MVM_operand_read_reg
};
static void p6captureouters(MVMThreadContext *tc) {
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
            if (outer->outer)
                MVM_frame_dec_ref(tc, outer->outer);
            outer->outer = MVM_frame_inc_ref(tc, new_outer);
        }
        else {
            MVM_exception_throw_adhoc(tc, "p6captureouters got non-code object");
        }
    }
}

static MVMuint8 s_p6stateinit[] = {
    MVM_operand_int64 | MVM_operand_write_reg
};
static void p6stateinit(MVMThreadContext *tc) {
    GET_REG(tc, 0).i64 = tc->cur_frame->flags & MVM_FRAME_FLAG_STATE_INIT ? 1 : 0;
}

/* First FIRST, use a flag in the object header. */
#define RAKUDO_FIRST_FLAG 128

static MVMuint8 s_p6setfirstflag[] = {
    MVM_operand_obj | MVM_operand_write_reg,
    MVM_operand_obj | MVM_operand_read_reg
};
static void p6setfirstflag(MVMThreadContext *tc) {
    MVMObject *code_obj = GET_REG(tc, 2).o;
    MVMObject *vm_code  = MVM_frame_find_invokee(tc, code_obj, NULL);
    vm_code->header.flags |= RAKUDO_FIRST_FLAG;
}

static MVMuint8 s_p6takefirstflag[] = {
    MVM_operand_int64 | MVM_operand_write_reg
};
static void p6takefirstflag(MVMThreadContext *tc) {
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
static void p6setpre(MVMThreadContext *tc) {
    tc->cur_frame->flags |= RAKUDO_FRAME_PRE_FLAG;
    GET_REG(tc, 0).o = NULL;
}

static MVMuint8 s_p6clearpre[] = {
    MVM_operand_obj | MVM_operand_write_reg
};
static void p6clearpre(MVMThreadContext *tc) {
    if (tc->cur_frame->flags & RAKUDO_FRAME_PRE_FLAG)
        tc->cur_frame->flags ^= RAKUDO_FRAME_PRE_FLAG;
    GET_REG(tc, 0).o = NULL;
}

static MVMuint8 s_p6inpre[] = {
    MVM_operand_int64 | MVM_operand_write_reg
};
static void p6inpre(MVMThreadContext *tc) {
    if (tc->cur_frame->flags & RAKUDO_FRAME_PRE_FLAG) {
        tc->cur_frame->flags ^= RAKUDO_FRAME_PRE_FLAG;
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
static void p6finddispatcher(MVMThreadContext *tc) {
    MVMFrame  *ctx = tc->cur_frame;
    while (ctx) {
        /* Do we have a dispatcher here? */
        MVMRegister *disp_lex = MVM_frame_try_get_lexical(tc, ctx, str_dispatcher, MVM_reg_obj);
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
                        ctx_ref = MVM_repr_alloc_init(tc, tc->instance->boot_types.BOOTContext);
                        ((MVMContext *)ctx_ref)->body.context = MVM_frame_inc_ref(tc, ctx);
                    });
                    capture = MVM_args_use_capture(tc, ctx);
                    p6sub = ((MVMCode *)ctx->code_ref)->body.code_object;

                    /* Lookup method, invoke it, and set up callback to ensure it
                     * is also stored in the lexical. */
                    meth = MVM_6model_find_method_cache_only(tc, dispatcher, str_vivify_for);
                    meth = MVM_frame_find_invokee(tc, meth, NULL);
                    *(tc->interp_cur_op) += 4; /* Get right return address. */
                    MVM_args_setup_thunk(tc, res_reg, MVM_RETURN_OBJ, &disp_callsite);
                    {
                        MVMRegister **srd = malloc(2 * sizeof(MVMObject *));
                        srd[0] = disp_lex;
                        srd[1] = res_reg;
                        tc->cur_frame->special_return      = store_dispatcher;
                        tc->cur_frame->special_return_data = srd;
                    }
                    tc->cur_frame->args[0].o = dispatcher;
                    tc->cur_frame->args[1].o = p6sub;
                    tc->cur_frame->args[2].o = ctx_ref;
                    tc->cur_frame->args[3].o = capture;
                    STABLE(meth)->invoke(tc, meth, &disp_callsite, tc->cur_frame->args);
                    *(tc->interp_cur_op) -= 4; /* Coutneract addition of ext-op size. */
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
            *(tc->interp_cur_op) -= 4; /* Coutneract addition of ext-op size. */
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
static void p6argsfordispatcher(MVMThreadContext *tc) {
    MVMFrame  *ctx = tc->cur_frame;
    while (ctx) {
        /* Do we have the dispatcher we're looking for? */
        MVMRegister *disp_lex = MVM_frame_try_get_lexical(tc, ctx, str_dispatcher, MVM_reg_obj);
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

static MVMuint8 s_p6shiftpush[] = {
    MVM_operand_obj   | MVM_operand_write_reg,
    MVM_operand_obj   | MVM_operand_read_reg,
    MVM_operand_obj   | MVM_operand_read_reg,
    MVM_operand_int64 | MVM_operand_read_reg
};
static void p6shiftpush(MVMThreadContext *tc) {
    MVMObject   *a = GET_REG(tc, 2).o;
    MVMObject   *b = GET_REG(tc, 4).o;
    MVMint64 count = GET_REG(tc, 6).i64;
    MVMint64 total = count;
    MVMint64 elems = MVM_repr_elems(tc, b);
    if (count > elems)
        count = elems;

    if (!MVM_is_null(tc, a) && total > 0) {
        MVMint64 getPos = 0;
        MVMint64 setPos = MVM_repr_elems(tc, a);
        REPR(a)->pos_funcs.set_elems(tc, STABLE(a), a, OBJECT_BODY(a), setPos + count);
        while (count > 0) {
            MVM_repr_bind_pos_o(tc, a, setPos, MVM_repr_at_pos_o(tc, b, getPos));
            count--;
            getPos++;
            setPos++;
        }
    }
    if (total > 0) {
        MVMROOT(tc, a, {
        MVMROOT(tc, b, {
            MVMObject *copy = MVM_repr_alloc_init(tc, tc->instance->boot_types.BOOTArray);
            REPR(b)->pos_funcs.splice(tc, STABLE(b), b, OBJECT_BODY(b),
                copy, 0, total);
        });
        });
    }

    GET_REG(tc, 0).o = a;
}

static MVMuint8 s_p6arrfindtypes[] = {
    MVM_operand_int64 | MVM_operand_write_reg,
    MVM_operand_obj   | MVM_operand_read_reg,
    MVM_operand_obj   | MVM_operand_read_reg,
    MVM_operand_int64 | MVM_operand_read_reg,
    MVM_operand_int64 | MVM_operand_read_reg
};
static void p6arrfindtypes(MVMThreadContext *tc) {
    MVMObject *arr    = GET_REG(tc, 2).o;
    MVMObject *types  = GET_REG(tc, 4).o;
    MVMint64   start  = GET_REG(tc, 6).i64;
    MVMint64   last   = GET_REG(tc, 8).i64;
    MVMint64   elems  = MVM_repr_elems(tc, arr);
    MVMint64   ntypes = MVM_repr_elems(tc, types);
    MVMint64   index, type_index;

    if (elems < last)
        last = elems;

    for (index = start; index < last; index++) {
        MVMObject *val = MVM_repr_at_pos_o(tc, arr, index);
        if (!MVM_is_null(tc, val) && !STABLE(val)->container_spec) {
            MVMint64 found = 0;
            for (type_index = 0; type_index < ntypes; type_index++) {
                MVMObject *type = MVM_repr_at_pos_o(tc, types, type_index);
                if (MVM_6model_istype_cache_only(tc, val, type)) {
                    found = 1;
                    break;
                }
            }
            if (found)
                break;
        }
    }

    GET_REG(tc, 0).i64 = index;
}

static MVMuint8 s_p6decodelocaltime[] = {
    MVM_operand_obj   | MVM_operand_write_reg,
    MVM_operand_int64 | MVM_operand_read_reg
};
static void p6decodelocaltime(MVMThreadContext *tc) {
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
static void p6staticouter(MVMThreadContext *tc) {
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
    MVM_frame_try_return(tc);
}
static void p6invokeunder(MVMThreadContext *tc) {
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
    tc->cur_frame->special_return = return_from_fake;
    STABLE(code)->invoke(tc, code, &no_arg_callsite, tc->cur_frame->args);
    *(tc->interp_cur_op) -= 6;
}

/* Registers the extops with MoarVM. */
MVM_DLL_EXPORT void Rakudo_ops_init(MVMThreadContext *tc) {
    MVM_ext_register_extop(tc, "p6init",  p6init, 0, NULL);
    MVM_ext_register_extop(tc, "p6box_i",  p6box_i, 2, s_p6box_i);
    MVM_ext_register_extop(tc, "p6box_n",  p6box_n, 2, s_p6box_n);
    MVM_ext_register_extop(tc, "p6box_s",  p6box_s, 2, s_p6box_s);
    MVM_ext_register_extop(tc, "p6parcel",  p6parcel, 3, s_p6parcel);
    MVM_ext_register_extop(tc, "p6listiter",  p6listiter, 3, s_p6listiter);
    MVM_ext_register_extop(tc, "p6list",  p6list, 4, s_p6list);
    MVM_ext_register_extop(tc, "p6listitems",  p6listitems, 2, s_p6listitems);
    MVM_ext_register_extop(tc, "p6settypes",  p6settypes, 1, s_p6settypes);
    MVM_ext_register_extop(tc, "p6bool",  p6bool, 2, s_p6bool);
    MVM_ext_register_extop(tc, "p6scalarfromdesc",  p6scalarfromdesc, 2, s_p6scalarfromdesc);
    MVM_ext_register_extop(tc, "p6recont_ro",  p6recont_ro, 2, s_p6recont_ro);
    MVM_ext_register_extop(tc, "p6var",  p6var, 2, s_p6var);
    MVM_ext_register_extop(tc, "p6reprname",  p6reprname, 2, s_p6reprname);
    MVM_ext_register_extop(tc, "p6decontrv",  p6decontrv, 2, s_p6decontrv);
    MVM_ext_register_extop(tc, "p6routinereturn",  p6routinereturn, 2, s_p6routinereturn);
    MVM_ext_register_extop(tc, "p6capturelex",  p6capturelex, 2, s_p6capturelex);
    MVM_ext_register_extop(tc, "p6capturelexwhere",  p6capturelexwhere, 2, s_p6capturelexwhere);
    MVM_ext_register_extop(tc, "p6getouterctx", p6getouterctx, 2, s_p6getouterctx);
    MVM_ext_register_extop(tc, "p6captureouters", p6captureouters, 2, s_p6captureouters);
    MVM_ext_register_extop(tc, "p6stateinit", p6stateinit, 1, s_p6stateinit);
    MVM_ext_register_extop(tc, "p6setfirstflag", p6setfirstflag, 2, s_p6setfirstflag);
    MVM_ext_register_extop(tc, "p6takefirstflag", p6takefirstflag, 1, s_p6takefirstflag);
    MVM_ext_register_extop(tc, "p6setpre", p6setpre, 1, s_p6setpre);
    MVM_ext_register_extop(tc, "p6clearpre", p6clearpre, 1, s_p6clearpre);
    MVM_ext_register_extop(tc, "p6inpre", p6inpre, 1, s_p6inpre);
    MVM_ext_register_extop(tc, "p6finddispatcher", p6finddispatcher, 2, s_p6finddispatcher);
    MVM_ext_register_extop(tc, "p6argsfordispatcher", p6argsfordispatcher, 2, s_p6argsfordispatcher);
    MVM_ext_register_extop(tc, "p6shiftpush", p6shiftpush, 4, s_p6shiftpush);
    MVM_ext_register_extop(tc, "p6arrfindtypes", p6arrfindtypes, 5, s_p6arrfindtypes);
    MVM_ext_register_extop(tc, "p6decodelocaltime", p6decodelocaltime, 2, s_p6decodelocaltime);
    MVM_ext_register_extop(tc, "p6staticouter", p6staticouter, 2, s_p6staticouter);
    MVM_ext_register_extop(tc, "p6invokeunder", p6invokeunder, 3, s_p6invokeunder);
}
