#define MVM_SHARED 1
#include "moar.h"
#include "container.h"

/* Dummy, no-arg callsite. */
static MVMCallsite no_arg_callsite = { NULL, 0, 0, 0 };

/* Dummy callsite for type_check. */
static MVMCallsiteEntry tc_flags[] = { MVM_CALLSITE_ARG_OBJ,
                                       MVM_CALLSITE_ARG_OBJ,
                                       MVM_CALLSITE_ARG_OBJ };
static MVMCallsite     tc_callsite = { tc_flags, 3, 3, 3, 0 };

/* Registered container operation callbacks. */
typedef struct {
    MVMObject *store;
    MVMObject *store_unchecked;
    MVMObject *cas;
    MVMObject *atomic_load;
    MVMObject *atomic_store;
} RakudoContData;

static void rakudo_scalar_fetch(MVMThreadContext *tc, MVMObject *cont, MVMRegister *res) {
    MVMObject *value = ((Rakudo_Scalar *)cont)->value;
    res->o = value ? value : tc->instance->VMNull;
}

static void rakudo_scalar_fetch_i(MVMThreadContext *tc, MVMObject *cont, MVMRegister *res) {
    res->i64 = MVM_repr_get_int(tc, ((Rakudo_Scalar *)cont)->value);
}

static void rakudo_scalar_fetch_n(MVMThreadContext *tc, MVMObject *cont, MVMRegister *res) {
    res->n64 = MVM_repr_get_num(tc, ((Rakudo_Scalar *)cont)->value);
}

static void rakudo_scalar_fetch_s(MVMThreadContext *tc, MVMObject *cont, MVMRegister *res) {
    res->s = MVM_repr_get_str(tc, ((Rakudo_Scalar *)cont)->value);
}

MVMObject * get_nil();
MVMObject * get_mu();

static void finish_store(MVMThreadContext *tc, MVMObject *cont, MVMObject *obj) {
    Rakudo_Scalar *rs = (Rakudo_Scalar *)cont;
    MVMObject *whence;

    /* Store the value. */
    MVM_ASSIGN_REF(tc, &(cont->header), rs->value, obj);

    /* Run any whence closure. */
    whence = rs->whence;
    if (whence && IS_CONCRETE(whence)) {
        MVMObject *code = MVM_frame_find_invokee(tc, whence, NULL);
        MVM_args_setup_thunk(tc, NULL, MVM_RETURN_VOID, &no_arg_callsite);
        rs->whence = NULL;
        STABLE(code)->invoke(tc, code, &no_arg_callsite, tc->cur_frame->args);
    }
}

typedef struct {
    MVMObject   *cont;
    MVMObject   *obj;
    MVMRegister  res;
} type_check_data;
void Rakudo_assign_typecheck_failed(MVMThreadContext *tc, MVMObject *cont, MVMObject *obj);
static void type_check_ret(MVMThreadContext *tc, void *sr_data) {
    type_check_data *tcd = (type_check_data *)sr_data;
    MVMObject *cont = tcd->cont;
    MVMObject *obj  = tcd->obj;
    MVMint64   res  = tcd->res.i64;
    free(tcd);
    if (res)
        finish_store(tc, cont, obj);
    else
         Rakudo_assign_typecheck_failed(tc, cont, obj);
}
static void mark_type_check_ret_data(MVMThreadContext *tc, MVMFrame *frame,
        MVMGCWorklist *worklist) {
    type_check_data *tcd = (type_check_data *)frame->extra->special_return_data;
    MVM_gc_worklist_add(tc, worklist, &tcd->cont);
    MVM_gc_worklist_add(tc, worklist, &tcd->obj);
}

static void ensure_assignable(MVMThreadContext *tc, Rakudo_ContainerDescriptor *rcd) {
    MVMint64 rw = 0;
    if (rcd && IS_CONCRETE(rcd))
        rw = rcd->rw;
    if (!rw) {
        if (rcd && IS_CONCRETE(rcd) && rcd->name) {
            char *c_name = MVM_string_utf8_encode_C_string(tc, rcd->name);
            char *waste[] = { c_name, NULL };
            MVM_exception_throw_adhoc_free(tc, waste,
                "Cannot assign to a readonly variable (%s) or a value", c_name);
        }
        else {
            MVM_exception_throw_adhoc(tc, "Cannot assign to a readonly variable or a value");
        }
    }
}

static MVMint32 type_check_store(MVMThreadContext *tc, MVMObject *cont, MVMObject *obj,
                                 Rakudo_ContainerDescriptor *rcd, MVMSpecialReturn callback) {
    /* Check against the type-check cache first (common, fast-path
     * case). */
    MVMint64 mode = STABLE(rcd->of)->mode_flags & MVM_TYPE_CHECK_CACHE_FLAG_MASK;
    if (rcd->of != get_mu() && !MVM_6model_istype_cache_only(tc, obj, rcd->of)) {
        /* Failed. If the cache is definitive, we certainly have an error. */
        if (STABLE(obj)->type_check_cache &&
            (mode & MVM_TYPE_CHECK_CACHE_THEN_METHOD) == 0 &&
            (mode & MVM_TYPE_CHECK_NEEDS_ACCEPTS) == 0) {
             Rakudo_assign_typecheck_failed(tc, cont, obj);
             return 1;
         }

        /* If we get here, need to call .^type_check on the value we're
         * checking, unless it's an accepts check. */
        if (!STABLE(obj)->type_check_cache || (mode & MVM_TYPE_CHECK_CACHE_THEN_METHOD)) {
            MVMObject *HOW, *meth;
            MVMROOT(tc, cont, {
            MVMROOT(tc, obj, {
            MVMROOT(tc, rcd, {
                HOW = MVM_6model_get_how_obj(tc, rcd->of);
                MVMROOT(tc, HOW, {
                    meth = MVM_6model_find_method_cache_only(tc, HOW,
                        tc->instance->str_consts.type_check);
                });
            });
            });
            });
            if (meth) {
                /* Set up the call, using a fake register in special return
                 * data as the target. */
                MVMObject *code = MVM_frame_find_invokee(tc, meth, NULL);
                type_check_data *tcd = malloc(sizeof(type_check_data));
                tcd->cont    = cont;
                tcd->obj     = obj;
                tcd->res.i64 = 0;
                MVM_args_setup_thunk(tc, &tcd->res, MVM_RETURN_INT, &tc_callsite);
                MVM_frame_special_return(tc, tc->cur_frame, callback, NULL,
                    tcd, mark_type_check_ret_data);
                tc->cur_frame->args[0].o = HOW;
                tc->cur_frame->args[1].o = obj;
                tc->cur_frame->args[2].o = rcd->of;
                STABLE(code)->invoke(tc, code, &tc_callsite, tc->cur_frame->args);
                return 1;
            }
        }

        /* If the flag to call .accepts_type on the target value is set, do so. */
        if (mode & MVM_TYPE_CHECK_NEEDS_ACCEPTS) {
            MVMObject *HOW, *meth;
            MVMROOT(tc, cont, {
            MVMROOT(tc, obj, {
            MVMROOT(tc, rcd, {
                HOW = MVM_6model_get_how_obj(tc, rcd->of);
                MVMROOT(tc, HOW, {
                    meth = MVM_6model_find_method_cache_only(tc, HOW,
                        tc->instance->str_consts.accepts_type);
                });
            });
            });
            });
            if (meth) {
                /* Set up the call, using the result register as the target. */
                MVMObject *code = MVM_frame_find_invokee(tc, meth, NULL);
                type_check_data *tcd = malloc(sizeof(type_check_data));
                tcd->cont    = cont;
                tcd->obj     = obj;
                tcd->res.i64 = 0;
                MVM_args_setup_thunk(tc, &tcd->res, MVM_RETURN_INT, &tc_callsite);
                MVM_frame_special_return(tc, tc->cur_frame, callback, NULL,
                    tcd, mark_type_check_ret_data);
                tc->cur_frame->args[0].o = HOW;
                tc->cur_frame->args[1].o = rcd->of;
                tc->cur_frame->args[2].o = obj;
                STABLE(code)->invoke(tc, code, &tc_callsite, tc->cur_frame->args);
                return 1;
            }
            else {
                MVM_exception_throw_adhoc(tc,
                    "Expected 'accepts_type' method, but none found in meta-object");
            }
        }
    }

    return 0;
}

static void rakudo_scalar_store(MVMThreadContext *tc, MVMObject *cont, MVMObject *value) {
    RakudoContData *data = (RakudoContData *)STABLE(cont)->container_data;
    MVMObject *code = MVM_frame_find_invokee(tc, data->store, NULL);
    MVMCallsite *cs = MVM_callsite_get_common(tc, MVM_CALLSITE_ID_TWO_OBJ);
    MVM_args_setup_thunk(tc, NULL, MVM_RETURN_VOID, cs);
    tc->cur_frame->args[0].o = cont;
    tc->cur_frame->args[1].o = value;
    STABLE(code)->invoke(tc, code, cs, tc->cur_frame->args);
}

static void rakudo_scalar_store_i(MVMThreadContext *tc, MVMObject *cont, MVMint64 value) {
    MVMObject *boxed;
    MVMROOT(tc, cont, {
        boxed = MVM_repr_box_int(tc, MVM_hll_current(tc)->int_box_type, value);
    });
    rakudo_scalar_store(tc, cont, boxed);
}

static void rakudo_scalar_store_n(MVMThreadContext *tc, MVMObject *cont, MVMnum64 value) {
    MVMObject *boxed;
    MVMROOT(tc, cont, {
        boxed = MVM_repr_box_num(tc, MVM_hll_current(tc)->num_box_type, value);
    });
    rakudo_scalar_store(tc, cont, boxed);
}

static void rakudo_scalar_store_s(MVMThreadContext *tc, MVMObject *cont, MVMString *value) {
    MVMObject *boxed;
    MVMROOT(tc, cont, {
        boxed = MVM_repr_box_str(tc, MVM_hll_current(tc)->str_box_type, value);
    });
    rakudo_scalar_store(tc, cont, boxed);
}

static void rakudo_scalar_store_unchecked(MVMThreadContext *tc, MVMObject *cont, MVMObject *obj) {
    finish_store(tc, cont, obj);
}

static void rakudo_scalar_gc_mark_data(MVMThreadContext *tc, MVMSTable *st, MVMGCWorklist *worklist) {
    RakudoContData *data = (RakudoContData *)st->container_data;
    MVM_gc_worklist_add(tc, worklist, &data->store);
    MVM_gc_worklist_add(tc, worklist, &data->store_unchecked);
    MVM_gc_worklist_add(tc, worklist, &data->cas);
    MVM_gc_worklist_add(tc, worklist, &data->atomic_load);
    MVM_gc_worklist_add(tc, worklist, &data->atomic_store);
}

static void rakudo_scalar_gc_free_data(MVMThreadContext *tc, MVMSTable *st) {
    MVM_free_null(st->container_data);
}

static void rakudo_scalar_serialize(MVMThreadContext *tc, MVMSTable *st, MVMSerializationWriter *writer) {
    RakudoContData *data = (RakudoContData *)st->container_data;
    MVM_serialization_write_ref(tc, writer, data->store);
    MVM_serialization_write_ref(tc, writer, data->store_unchecked);
    MVM_serialization_write_ref(tc, writer, data->cas);
    MVM_serialization_write_ref(tc, writer, data->atomic_load);
    MVM_serialization_write_ref(tc, writer, data->atomic_store);
}

static void rakudo_scalar_deserialize(MVMThreadContext *tc, MVMSTable *st, MVMSerializationReader *reader) {
    RakudoContData *data = (RakudoContData *)st->container_data;
    MVM_ASSIGN_REF(tc, &(st->header), data->store, MVM_serialization_read_ref(tc, reader));
    MVM_ASSIGN_REF(tc, &(st->header), data->store_unchecked, MVM_serialization_read_ref(tc, reader));
    MVM_ASSIGN_REF(tc, &(st->header), data->cas, MVM_serialization_read_ref(tc, reader));
    MVM_ASSIGN_REF(tc, &(st->header), data->atomic_load, MVM_serialization_read_ref(tc, reader));
    MVM_ASSIGN_REF(tc, &(st->header), data->atomic_store, MVM_serialization_read_ref(tc, reader));
}

static void rakudo_scalar_spesh(MVMThreadContext *tc, MVMSTable *st, MVMSpeshGraph *g, MVMSpeshBB *bb, MVMSpeshIns *ins) {
    switch (ins->info->opcode) {
    case MVM_OP_decont: {
        MVMSpeshOperand *old_operands = ins->operands;
        ins->info = MVM_op_get_op(MVM_OP_sp_p6oget_o);
        ins->operands = MVM_spesh_alloc(tc, g, 3 * sizeof(MVMSpeshOperand));
        ins->operands[0] = old_operands[0];
        ins->operands[1] = old_operands[1];
        ins->operands[2].lit_i16 = offsetof( Rakudo_Scalar, value ) - offsetof( MVMObjectStooge, data );
        break;
        }
    default: break;
    }
}

static MVMint32 rakudo_scalar_can_store(MVMThreadContext *tc, MVMObject *cont) {
    Rakudo_Scalar *rs = (Rakudo_Scalar *)cont;
    Rakudo_ContainerDescriptor *rcd = (Rakudo_ContainerDescriptor *)rs->descriptor;
    return rcd && IS_CONCRETE(rcd) && rcd->rw;
}

static void finish_cas(MVMThreadContext *tc, MVMObject *cont, MVMObject *expected,
                       MVMObject *value, MVMRegister *result) {
    Rakudo_Scalar *rs = (Rakudo_Scalar *)cont;
    result->o = (MVMObject *)MVM_casptr(&(rs->value), expected, value);
    MVM_gc_write_barrier(tc, (MVMCollectable *)cont, (MVMCollectable *)value);
}

typedef struct {
    MVMObject   *cont;
    MVMObject   *expected;
    MVMObject   *value;
    MVMRegister *cas_result;
    MVMRegister  res;
} cas_type_check_data;
static void cas_type_check_ret(MVMThreadContext *tc, void *sr_data) {
    cas_type_check_data *tcd = (cas_type_check_data *)sr_data;
    MVMObject *cont = tcd->cont;
    MVMObject *expected = tcd->expected;
    MVMObject *value = tcd->value;
    MVMRegister *cas_result = tcd->cas_result;
    MVMint64 res = tcd->res.i64;
    free(tcd);
    if (res)
        finish_cas(tc, cont, expected, value, cas_result);
    else
         Rakudo_assign_typecheck_failed(tc, cont, value);
}
static void mark_cas_type_check_ret_data(MVMThreadContext *tc, MVMFrame *frame,
        MVMGCWorklist *worklist) {
    cas_type_check_data *tcd = (cas_type_check_data *)frame->extra->special_return_data;
    MVM_gc_worklist_add(tc, worklist, &tcd->cont);
    MVM_gc_worklist_add(tc, worklist, &tcd->expected);
    MVM_gc_worklist_add(tc, worklist, &tcd->value);
}

static void rakudo_scalar_cas(MVMThreadContext *tc, MVMObject *cont,
                              MVMObject *expected, MVMObject *value,
                              MVMRegister *result) {
    Rakudo_Scalar *rs = (Rakudo_Scalar *)cont;
    Rakudo_ContainerDescriptor *rcd = (Rakudo_ContainerDescriptor *)rs->descriptor;
    ensure_assignable(tc, rcd);

    /* Handle Nil and type-checking. */
    if (!value) {
        MVM_exception_throw_adhoc(tc, "Cannot cas a null value into a Perl 6 scalar");
    }
    else {
        MVMint64 mode;
        if (STABLE(value)->WHAT == get_nil()) {
            value = rcd->the_default;
        }

        /* Check against the type-check cache first (common, fast-path
         * case). */
        mode = STABLE(rcd->of)->mode_flags & MVM_TYPE_CHECK_CACHE_FLAG_MASK;
        if (rcd->of != get_mu() && !MVM_6model_istype_cache_only(tc, value, rcd->of)) {
            /* Failed. If the cache is definitive, we certainly have an error. */
            if (STABLE(value)->type_check_cache &&
                (mode & MVM_TYPE_CHECK_CACHE_THEN_METHOD) == 0 &&
                (mode & MVM_TYPE_CHECK_NEEDS_ACCEPTS) == 0) {
                 Rakudo_assign_typecheck_failed(tc, cont, value);
                 return;
             }

            /* If we get here, need to call .^type_check on the value we're
             * checking, unless it's an accepts check. */
            if (!STABLE(value)->type_check_cache || (mode & MVM_TYPE_CHECK_CACHE_THEN_METHOD)) {
                MVMObject *HOW, *meth;
                MVMROOT(tc, cont, {
                MVMROOT(tc, expected, {
                MVMROOT(tc, value, {
                MVMROOT(tc, rcd, {
                    HOW = MVM_6model_get_how_obj(tc, rcd->of);
                    MVMROOT(tc, HOW, {
                        meth = MVM_6model_find_method_cache_only(tc, HOW,
                            tc->instance->str_consts.type_check);
                    });
                });
                });
                });
                });
                if (meth) {
                    /* Set up the call, using a fake register in special return
                     * data as the target. */
                    MVMObject *code = MVM_frame_find_invokee(tc, meth, NULL);
                    cas_type_check_data *tcd = malloc(sizeof(cas_type_check_data));
                    tcd->cont = cont;
                    tcd->expected = expected;
                    tcd->value = value;
                    tcd->cas_result = result;
                    tcd->res.i64 = 0;
                    MVM_args_setup_thunk(tc, &tcd->res, MVM_RETURN_INT, &tc_callsite);
                    MVM_frame_special_return(tc, tc->cur_frame, cas_type_check_ret, NULL,
                        tcd, mark_cas_type_check_ret_data);
                    tc->cur_frame->args[0].o = HOW;
                    tc->cur_frame->args[1].o = value;
                    tc->cur_frame->args[2].o = rcd->of;
                    STABLE(code)->invoke(tc, code, &tc_callsite, tc->cur_frame->args);
                    return;
                }
            }

            /* If the flag to call .accepts_type on the target value is set, do so. */
            if (mode & MVM_TYPE_CHECK_NEEDS_ACCEPTS) {
                MVMObject *HOW, *meth;
                MVMROOT(tc, cont, {
                MVMROOT(tc, expected, {
                MVMROOT(tc, value, {
                MVMROOT(tc, rcd, {
                    HOW = MVM_6model_get_how_obj(tc, rcd->of);
                    MVMROOT(tc, HOW, {
                        meth = MVM_6model_find_method_cache_only(tc, HOW,
                            tc->instance->str_consts.accepts_type);
                    });
                });
                });
                });
                });
                if (meth) {
                    /* Set up the call, using the result register as the target. */
                    MVMObject *code = MVM_frame_find_invokee(tc, meth, NULL);
                    cas_type_check_data *tcd = malloc(sizeof(cas_type_check_data));
                    tcd->cont = cont;
                    tcd->expected = expected;
                    tcd->value = value;
                    tcd->cas_result = result;
                    tcd->res.i64 = 0;
                    MVM_args_setup_thunk(tc, &tcd->res, MVM_RETURN_INT, &tc_callsite);
                    MVM_frame_special_return(tc, tc->cur_frame, cas_type_check_ret, NULL,
                        tcd, mark_cas_type_check_ret_data);
                    tc->cur_frame->args[0].o = HOW;
                    tc->cur_frame->args[1].o = rcd->of;
                    tc->cur_frame->args[2].o = value;
                    STABLE(code)->invoke(tc, code, &tc_callsite, tc->cur_frame->args);
                    return;
                }
                else {
                    MVM_exception_throw_adhoc(tc,
                        "Expected 'accepts_type' method, but none found in meta-object");
                }
            }
        }
    }

    /* Type check passed without needing invocation; finish the CAS. */
    finish_cas(tc, cont, expected, value, result);
}

static MVMObject * rakudo_scalar_atomic_load(MVMThreadContext *tc, MVMObject *cont) {
    MVMObject *value = (MVMObject *)MVM_load(&(((Rakudo_Scalar *)cont)->value));
    return value ? value : tc->instance->VMNull;
}

static void finish_atomic_store(MVMThreadContext *tc, MVMObject *cont, MVMObject *obj) {
    Rakudo_Scalar *rs = (Rakudo_Scalar *)cont;
    MVM_store(&(rs->value), obj);
    MVM_gc_write_barrier(tc, (MVMCollectable *)cont, (MVMCollectable *)obj);
}

static void atomic_store_type_check_ret(MVMThreadContext *tc, void *sr_data) {
    type_check_data *tcd = (type_check_data *)sr_data;
    MVMObject *cont = tcd->cont;
    MVMObject *obj  = tcd->obj;
    MVMint64   res  = tcd->res.i64;
    free(tcd);
    if (res)
        finish_atomic_store(tc, cont, obj);
    else
         Rakudo_assign_typecheck_failed(tc, cont, obj);
}

void rakudo_scalar_atomic_store(MVMThreadContext *tc, MVMObject *cont, MVMObject *value) {
    Rakudo_Scalar *rs = (Rakudo_Scalar *)cont;
    Rakudo_ContainerDescriptor *rcd = (Rakudo_ContainerDescriptor *)rs->descriptor;
    ensure_assignable(tc, rcd);
    if (!value)
        MVM_exception_throw_adhoc(tc, "Cannot assign a null value to a Perl 6 scalar");
    if (STABLE(value)->WHAT == get_nil())
        value = rcd->the_default;
    if (!type_check_store(tc, cont, value, rcd, atomic_store_type_check_ret))
        finish_atomic_store(tc, cont, value); /* Type check didn't invoke. */
}

static const MVMContainerSpec rakudo_scalar_spec = {
    "rakudo_scalar",
    rakudo_scalar_fetch,
    rakudo_scalar_fetch_i,
    rakudo_scalar_fetch_n,
    rakudo_scalar_fetch_s,
    rakudo_scalar_store,
    rakudo_scalar_store_i,
    rakudo_scalar_store_n,
    rakudo_scalar_store_s,
    rakudo_scalar_store_unchecked,
    rakudo_scalar_spesh,
    rakudo_scalar_gc_mark_data,
    rakudo_scalar_gc_free_data,
    rakudo_scalar_serialize,
    rakudo_scalar_deserialize,
    rakudo_scalar_can_store,
    rakudo_scalar_cas,
    rakudo_scalar_atomic_load,
    rakudo_scalar_atomic_store,
    1
};

static void rakudo_scalar_set_container_spec(MVMThreadContext *tc, MVMSTable *st) {
    RakudoContData *data = MVM_calloc(1, sizeof(RakudoContData));
    st->container_data = data;
    st->container_spec = &rakudo_scalar_spec;
}

static MVMObject * grab_one_value(MVMThreadContext *tc, MVMObject *config, const char *key) {
    MVMString *key_str;
    MVMROOT(tc, config, {
        key_str = MVM_string_ascii_decode_nt(tc, tc->instance->VMString, key);
    });
    if (!MVM_repr_exists_key(tc, config, key_str))
        MVM_exception_throw_adhoc(tc, "Container spec must be configured with a '%s'", key);
    return MVM_repr_at_key_o(tc, config, key_str);
}
static void rakudo_scalar_configure_container_spec(MVMThreadContext *tc, MVMSTable *st, MVMObject *config) {
    RakudoContData *data = (RakudoContData *)st->container_data;
    MVMROOT2(tc, st, config, {
        MVMObject *value;
        value = grab_one_value(tc, config, "store");
        MVM_ASSIGN_REF(tc, &(st->header), data->store, value);
    });
}

static const MVMContainerConfigurer ContainerConfigurer = {
    rakudo_scalar_set_container_spec,
    rakudo_scalar_configure_container_spec
};

void Rakudo_containers_setup(MVMThreadContext *tc) {
    MVM_6model_add_container_config(tc,
        MVM_string_ascii_decode_nt(tc, tc->instance->VMString, "rakudo_scalar"), &ContainerConfigurer);
}

MVMContainerSpec * Rakudo_containers_get_scalar() {
    return (MVMContainerSpec *)&rakudo_scalar_spec;
}
