#define MVM_SHARED 1
#include "moar.h"
#include "container.h"

#define GET_REG(tc, idx) (*tc->interp_reg_base)[*((MVMuint16 *)(*tc->interp_cur_op + idx))]

/* Dummy, one-arg callsite. */
static MVMCallsiteEntry one_arg_flags[] = { MVM_CALLSITE_ARG_OBJ };
static MVMCallsite     one_arg_callsite = { one_arg_flags, 1, 1, 0 };

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

/* Default container descriptor. */
static MVMObject *default_cont_desc = NULL;

/* Useful string constants. */
static MVMString *str_return = NULL;

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
    MVMP6opaque  p6o;
    MVMObject   *items;
    MVMObject   *flattens;
    MVMObject   *nextiter;
} Rakudo_List;

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
    });
    
    /* Set up default container descriptor. */
    {
        MVMString *element;
        default_cont_desc = MVM_repr_alloc_init(tc, ContainerDescriptor);
        MVM_gc_root_add_permanent(tc, (MVMCollectable **)&default_cont_desc);
        element = MVM_string_ascii_decode_nt(tc, tc->instance->VMString, "<element>");
        MVM_ASSIGN_REF(tc, default_cont_desc,
            ((Rakudo_ContainerDescriptor *)default_cont_desc)->of, Mu);
        MVM_ASSIGN_REF(tc, default_cont_desc,
            ((Rakudo_ContainerDescriptor *)default_cont_desc)->name, element);
        ((Rakudo_ContainerDescriptor *)default_cont_desc)->rw = 1;
        MVM_ASSIGN_REF(tc, default_cont_desc,
            ((Rakudo_ContainerDescriptor *)default_cont_desc)->the_default, Any);
    }

    /* Strings. */
    str_return = MVM_string_ascii_decode_nt(tc, tc->instance->VMString, "RETURN");
    MVM_gc_root_add_permanent(tc, (MVMCollectable **)&str_return);
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
    MVM_ASSIGN_REF(tc, parcel, ((Rakudo_Parcel *)parcel)->storage, vmarr);

    if (fill) {
        MVMint64 elems = MVM_repr_elems(tc, vmarr);
        MVMint64 i;
        for (i = 0; i < elems; i++)
            if (!MVM_repr_at_pos_o(tc, vmarr, i))
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
        MVM_ASSIGN_REF(tc, result, ((Rakudo_ListIter *)result)->rest, items);
        MVM_ASSIGN_REF(tc, result, ((Rakudo_ListIter *)result)->list, list);
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
     MVMROOT(tc, list, {
        MVMObject *items = GET_REG(tc, 2).o;
        if (items) {
            MVMObject *iter = make_listiter(tc, items, list);
            MVM_ASSIGN_REF(tc, list, ((Rakudo_List *)list)->nextiter, iter);
        }
        MVM_ASSIGN_REF(tc, list, ((Rakudo_List *)list)->flattens, GET_REG(tc, 6).o);
     });
     GET_REG(tc, 0).o = list;
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
        MVMObject *items = ((Rakudo_List *)list)->items;
        if (!items || !IS_CONCRETE(items)) {
            MVMROOT(tc, list, {
                items = MVM_repr_alloc_init(tc, tc->instance->boot_types.BOOTArray);
                MVM_ASSIGN_REF(tc, list, ((Rakudo_List *)list)->items, items);
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
    if (!descriptor) {
        descriptor = default_cont_desc;
    }
    MVM_ASSIGN_REF(tc, new_scalar, ((Rakudo_Scalar *)new_scalar)->descriptor, descriptor);
    MVM_ASSIGN_REF(tc, new_scalar, ((Rakudo_Scalar *)new_scalar)->value,
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
        if (desc && ((Rakudo_ContainerDescriptor *)desc)->rw) {
            /* We have an rw container; re-containerize it. */
            MVMROOT(tc, check, {
                MVMObject *result = MVM_repr_alloc_init(tc, Scalar);
                MVM_ASSIGN_REF(tc, result, ((Rakudo_Scalar *)result)->value,
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
            MVM_ASSIGN_REF(tc, wrapper, ((Rakudo_Scalar *)wrapper)->value, wrappee);
            GET_REG(tc, 2).o = wrapper;
        });
     }
     else {
        GET_REG(tc, 2).o = wrappee;
     }
}

/* Type-checks the return value of a routine. */
/* XXX Due to potential nested runloop calls, this may not want doing in C. */
static MVMuint8 s_p6typecheckrv[] = {
    MVM_operand_obj | MVM_operand_read_reg,
    MVM_operand_obj | MVM_operand_read_reg,
};
static void p6typecheckrv(MVMThreadContext *tc) {
     /* XXX */
}

/* Decontainerizes the return value of a routine as needed. */
static MVMuint8 s_p6decontrv[] = {
    MVM_operand_obj | MVM_operand_write_reg,
    MVM_operand_obj | MVM_operand_read_reg,
};
static void p6decontrv(MVMThreadContext *tc) {
     MVMObject *retval = GET_REG(tc, 2).o;
     if (STABLE(retval)->container_spec == Rakudo_containers_get_scalar()) {
        Rakudo_ContainerDescriptor *cd = (Rakudo_ContainerDescriptor *)
            ((Rakudo_Scalar *)retval)->descriptor;
        if (cd && cd->rw) {
            MVMROOT(tc, retval, {
                MVMObject *cont = MVM_repr_alloc_init(tc, Scalar);
                MVM_ASSIGN_REF(tc, cont, ((Rakudo_Scalar *)cont)->value,
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
    if (ret && IS_CONCRETE(ret) && REPR(ret)->ID == MVM_REPR_ID_Lexotic) {
        tc->cur_frame->return_type    = MVM_RETURN_VOID;
        tc->cur_frame->return_address = *(tc->interp_cur_op);
        tc->cur_frame->args[0].o = GET_REG(tc, 2).o;
        STABLE(ret)->invoke(tc, ret, &one_arg_callsite, tc->cur_frame->args);
        *(tc->interp_cur_op) -= 4; /* Oh my, what a hack... */
    }
    else {
        MVM_exception_throw_adhoc(tc, "Attempt to return outside of any Routine");
    }
}

static MVMuint8 s_p6capturelex[] = {
    MVM_operand_obj | MVM_operand_write_reg,
    MVM_operand_obj | MVM_operand_read_reg,
};
static void p6capturelex(MVMThreadContext *tc) {
    MVMObject *p6_code_obj = GET_REG(tc, 2).o;
    MVMObject *vm_code_obj = MVM_frame_find_invokee(tc, p6_code_obj, NULL);
    if (REPR(vm_code_obj)->ID == MVM_REPR_ID_MVMCode)
        MVM_frame_capturelex(tc, vm_code_obj);
    else
        MVM_exception_throw_adhoc(tc, "p6captureouters got non-code object");
    GET_REG(tc, 0).o = p6_code_obj;
}

static MVMuint8 s_p6captureouters[] = {
    MVM_operand_obj | MVM_operand_read_reg
};
static void p6captureouters(MVMThreadContext *tc) {
    MVMObject *todo  = GET_REG(tc, 0).o;
    MVMint64   elems = MVM_repr_elems(tc, todo);
    MVMint64   i;
    for (i = 0; i < elems; i++) {
        MVMObject *p6_code_obj = MVM_repr_at_pos_o(tc, todo, i);
        MVMObject *vm_code_obj = MVM_frame_find_invokee(tc, p6_code_obj, NULL);
        if (REPR(vm_code_obj)->ID == MVM_REPR_ID_MVMCode) {
            MVMFrame *outer = ((MVMCode *)vm_code_obj)->body.outer;
            if (outer->outer)
                MVM_frame_dec_ref(tc, outer->outer);
            outer->outer = MVM_frame_inc_ref(tc, tc->cur_frame);
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
    /* XXX CHEAT */
    GET_REG(tc, 0).i64 = 1;
}

static MVMuint8 s_p6setfirstflag[] = {
    MVM_operand_obj | MVM_operand_write_reg,
    MVM_operand_obj | MVM_operand_read_reg
};
static void p6setfirstflag(MVMThreadContext *tc) {
    MVMObject *code_obj = GET_REG(tc, 2).o;
    MVM_exception_throw_adhoc(tc, "p6setfirstflag NYI");
}

static MVMuint8 s_p6finddispatcher[] = {
    MVM_operand_obj | MVM_operand_write_reg,
    MVM_operand_str | MVM_operand_read_reg
};
static void p6finddispatcher(MVMThreadContext *tc) {
    MVMString *usage = GET_REG(tc, 2).s;
    MVMObject *dispatcher;
    MVM_exception_throw_adhoc(tc, "p6finddispatcher NYI");
    GET_REG(tc, 0).o = dispatcher;
}

static MVMuint8 s_p6argsfordispatcher[] = {
    MVM_operand_obj | MVM_operand_write_reg,
    MVM_operand_obj | MVM_operand_read_reg
};
static void p6argsfordispatcher(MVMThreadContext *tc) {
    MVMObject *disp = GET_REG(tc, 2).o;
    MVM_exception_throw_adhoc(tc, "p6argsfordispatcher NYI");
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

    if (a != NULL && total > 0) {
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
        if (val && !STABLE(val)->container_spec) {
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
    MVMint64 since_poch = GET_REG(tc, 2).i64;
    MVM_exception_throw_adhoc(tc, "p6decodelocaltime NYI");
}

static MVMuint8 s_p6sort[] = {
    MVM_operand_obj | MVM_operand_write_reg,
    MVM_operand_obj | MVM_operand_read_reg,
    MVM_operand_obj | MVM_operand_read_reg
};
static void p6sort(MVMThreadContext *tc) {
    MVMObject    *indices = GET_REG(tc, 2).o;
    MVMObject *comparator = GET_REG(tc, 4).o;
    MVM_exception_throw_adhoc(tc, "p6sort NYI");
}

static MVMuint8 s_p6staticouter[] = {
    MVM_operand_obj | MVM_operand_write_reg,
    MVM_operand_obj | MVM_operand_read_reg
};
static void p6staticouter(MVMThreadContext *tc) {
    MVMObject *code = GET_REG(tc, 2).o;
    if (code && IS_CONCRETE(code) && REPR(code)->ID == MVM_REPR_ID_MVMCode) {
        MVMStaticFrame *sf = ((MVMCode *)code)->body.sf;
        GET_REG(tc, 0).o = sf->body.outer
            ? (MVMObject *)sf->body.outer->body.static_code
            : NULL;
    }
    else {
        MVM_exception_throw_adhoc(tc, "p6staticouter requires a CodeRef");
    }
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
    MVM_ext_register_extop(tc, "p6typecheckrv",  p6typecheckrv, 2, s_p6typecheckrv);
    MVM_ext_register_extop(tc, "p6decontrv",  p6decontrv, 2, s_p6decontrv);
    MVM_ext_register_extop(tc, "p6routinereturn",  p6routinereturn, 2, s_p6routinereturn);
    MVM_ext_register_extop(tc, "p6capturelex",  p6capturelex, 2, s_p6capturelex);
    MVM_ext_register_extop(tc, "p6captureouters", p6captureouters, 1, s_p6captureouters);
    MVM_ext_register_extop(tc, "p6stateinit", p6stateinit, 1, s_p6stateinit);
    MVM_ext_register_extop(tc, "p6setfirstflag", p6setfirstflag, 2, s_p6setfirstflag);
    MVM_ext_register_extop(tc, "p6finddispatcher", p6finddispatcher, 2, s_p6finddispatcher);
    MVM_ext_register_extop(tc, "p6argsfordispatcher", p6argsfordispatcher, 2, s_p6argsfordispatcher);
    MVM_ext_register_extop(tc, "p6shiftpush", p6shiftpush, 4, s_p6shiftpush);
    MVM_ext_register_extop(tc, "p6arrfindtypes", p6arrfindtypes, 5, s_p6arrfindtypes);
    MVM_ext_register_extop(tc, "p6decodelocaltime", p6decodelocaltime, 2, s_p6decodelocaltime);
    MVM_ext_register_extop(tc, "p6sort", p6sort, 3, s_p6sort);
    MVM_ext_register_extop(tc, "p6staticouter", p6staticouter, 2, s_p6staticouter);
}
