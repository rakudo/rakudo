#define MVM_SHARED 1
#include "moar.h"
#include "container.h"

#define GET_REG(tc, idx) (*tc->interp_reg_base)[*((MVMuint16 *)(*tc->interp_cur_op + idx))]

/* Are we initialized yet? */
static int initialized = 0;

/* Types we need. */
static MVMObject *Int      = NULL;
static MVMObject *Num      = NULL;
static MVMObject *Str      = NULL;
static MVMObject *Scalar   = NULL;
static MVMObject *Parcel   = NULL;
static MVMObject *ListIter = NULL;
static MVMObject *True     = NULL;
static MVMObject *False    = NULL;

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
        get_type(tc, conf, "Int", Int);
        get_type(tc, conf, "Num", Num);
        get_type(tc, conf, "Str", Str);
        get_type(tc, conf, "Scalar", Scalar);
        get_type(tc, conf, "Parcel", Parcel);
        get_type(tc, conf, "ListIter", ListIter);
        get_type(tc, conf, "True", True);
        get_type(tc, conf, "False", False);
    });
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

static MVMuint8 s_p6listitems[] = {
    MVM_operand_obj | MVM_operand_write_reg,
    MVM_operand_obj | MVM_operand_read_reg
};
static void p6listitems(MVMThreadContext *tc) {
     MVMObject *list = GET_REG(tc, 2).o;
     MVM_exception_throw_adhoc(tc, "p6listitems NYI");
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
        MVM_exception_throw_adhoc(tc, "default cont desc NYI");
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
    MVM_exception_throw_adhoc(tc, "p6recont_ro NYI");
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
    MVM_operand_obj | MVM_operand_read_reg,
};
static void p6decontrv(MVMThreadContext *tc) {
     /* XXX TODO */
     GET_REG(tc, 0).o = GET_REG(tc, 4).o;
}

static MVMuint8 s_p6routinereturn[] = {
    MVM_operand_obj | MVM_operand_write_reg,
    MVM_operand_obj | MVM_operand_read_reg,
};
static void p6routinereturn(MVMThreadContext *tc) {
    MVMObject *in = GET_REG(tc, 2).o;
    MVM_exception_throw_adhoc(tc, "p6routinereturn NYI");
    GET_REG(tc, 0).o = in;
}

static MVMuint8 s_p6capturelex[] = {
    MVM_operand_obj | MVM_operand_write_reg,
    MVM_operand_obj | MVM_operand_read_reg,
};
static void p6capturelex(MVMThreadContext *tc) {
    MVMObject *p6_code_obj = GET_REG(tc, 2).o;
    MVMObject *vm_code_obj = MVM_frame_find_invokee(tc, p6_code_obj);
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
        MVMObject *vm_code_obj = MVM_frame_find_invokee(tc, p6_code_obj);
        if (REPR(vm_code_obj)->ID == MVM_REPR_ID_MVMCode)
            MVM_frame_capturelex(tc, vm_code_obj);
        else
            MVM_exception_throw_adhoc(tc, "p6captureouters got non-code object");
    }
}

static MVMuint8 s_p6stateinit[] = {
    MVM_operand_int64 | MVM_operand_write_reg
};
static void p6stateinit(MVMThreadContext *tc) {
    MVM_exception_throw_adhoc(tc, "p6stateinit NYI");
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

static MVMuint8 s_p6decodelocaltime[] = {
    MVM_operand_obj   | MVM_operand_write_reg,
    MVM_operand_int64 | MVM_operand_read_reg
};
static void p6decodelocaltime(MVMThreadContext *tc) {
    MVMint64 since_poch = GET_REG(tc, 2).i64;
    MVM_exception_throw_adhoc(tc, "p6decodelocaltime NYI");
}

static MVMuint8 s_p6setautothreader[] = {
    MVM_operand_obj | MVM_operand_write_reg,
    MVM_operand_obj | MVM_operand_read_reg
};
static void p6setautothreader(MVMThreadContext *tc) {
    MVMObject *auto_threader = GET_REG(tc, 2).o;
    MVM_exception_throw_adhoc(tc, "p6setautothreader NYI");
}

static MVMuint8 s_p6staticouter[] = {
    MVM_operand_obj | MVM_operand_write_reg,
    MVM_operand_obj | MVM_operand_read_reg
};
static void p6staticouter(MVMThreadContext *tc) {
    MVMObject *code = GET_REG(tc, 2).o;
    MVM_exception_throw_adhoc(tc, "p6staticouter NYI");
}

/* Registers the extops with MoarVM. */
MVM_DLL_EXPORT void Rakudo_ops_init(MVMThreadContext *tc) {
    MVM_ext_register_extop(tc, "p6init",  p6init, 0, NULL);
    MVM_ext_register_extop(tc, "p6box_i",  p6box_i, 2, s_p6box_i);
    MVM_ext_register_extop(tc, "p6box_n",  p6box_n, 2, s_p6box_n);
    MVM_ext_register_extop(tc, "p6box_s",  p6box_s, 2, s_p6box_s);
    MVM_ext_register_extop(tc, "p6parcel",  p6parcel, 3, s_p6parcel);
    MVM_ext_register_extop(tc, "p6list",  p6list, 4, s_p6list);
    MVM_ext_register_extop(tc, "p6listitems",  p6listitems, 2, s_p6listitems);
    MVM_ext_register_extop(tc, "p6settypes",  p6settypes, 1, s_p6settypes);
    MVM_ext_register_extop(tc, "p6bool",  p6bool, 2, s_p6bool);
    MVM_ext_register_extop(tc, "p6scalarfromdesc",  p6scalarfromdesc, 2, s_p6scalarfromdesc);
    MVM_ext_register_extop(tc, "p6recont_ro",  p6recont_ro, 2, s_p6recont_ro);
    MVM_ext_register_extop(tc, "p6var",  p6var, 2, s_p6var);
    MVM_ext_register_extop(tc, "p6typecheckrv",  p6typecheckrv, 2, s_p6typecheckrv);
    MVM_ext_register_extop(tc, "p6decontrv",  p6decontrv, 3, s_p6decontrv);
    MVM_ext_register_extop(tc, "p6routinereturn",  p6routinereturn, 2, s_p6routinereturn);
    MVM_ext_register_extop(tc, "p6capturelex",  p6capturelex, 2, s_p6capturelex);
    MVM_ext_register_extop(tc, "p6captureouters", p6captureouters, 1, s_p6captureouters);
    MVM_ext_register_extop(tc, "p6stateinit", p6stateinit, 1, s_p6stateinit);
    MVM_ext_register_extop(tc, "p6finddispatcher", p6finddispatcher, 2, s_p6finddispatcher);
    MVM_ext_register_extop(tc, "p6argsfordispatcher", p6argsfordispatcher, 2, s_p6argsfordispatcher);
    MVM_ext_register_extop(tc, "p6decodelocaltime", p6decodelocaltime, 2, s_p6decodelocaltime);
    MVM_ext_register_extop(tc, "p6setautothreader", p6setautothreader, 2, s_p6setautothreader);
    MVM_ext_register_extop(tc, "p6staticouter", p6staticouter, 2, s_p6staticouter);
}
