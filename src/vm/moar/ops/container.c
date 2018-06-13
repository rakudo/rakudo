#define MVM_SHARED 1
#include "moar.h"
#include "container.h"

/* Registered container operation callbacks. */
typedef struct {
    MVMObject *store;
    MVMObject *store_unchecked;
    MVMObject *cas;
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

static void rakudo_scalar_store_unchecked(MVMThreadContext *tc, MVMObject *cont, MVMObject *value) {
    RakudoContData *data = (RakudoContData *)STABLE(cont)->container_data;
    MVMObject *code = MVM_frame_find_invokee(tc, data->store_unchecked, NULL);
    MVMCallsite *cs = MVM_callsite_get_common(tc, MVM_CALLSITE_ID_TWO_OBJ);
    MVM_args_setup_thunk(tc, NULL, MVM_RETURN_VOID, cs);
    tc->cur_frame->args[0].o = cont;
    tc->cur_frame->args[1].o = value;
    STABLE(code)->invoke(tc, code, cs, tc->cur_frame->args);
}

static void rakudo_scalar_gc_mark_data(MVMThreadContext *tc, MVMSTable *st, MVMGCWorklist *worklist) {
    RakudoContData *data = (RakudoContData *)st->container_data;
    MVM_gc_worklist_add(tc, worklist, &data->store);
    MVM_gc_worklist_add(tc, worklist, &data->store_unchecked);
    MVM_gc_worklist_add(tc, worklist, &data->cas);
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
    MVM_serialization_write_ref(tc, writer, data->atomic_store);
}

static void rakudo_scalar_deserialize(MVMThreadContext *tc, MVMSTable *st, MVMSerializationReader *reader) {
    RakudoContData *data = (RakudoContData *)st->container_data;
    MVM_ASSIGN_REF(tc, &(st->header), data->store, MVM_serialization_read_ref(tc, reader));
    MVM_ASSIGN_REF(tc, &(st->header), data->store_unchecked, MVM_serialization_read_ref(tc, reader));
    MVM_ASSIGN_REF(tc, &(st->header), data->cas, MVM_serialization_read_ref(tc, reader));
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
    return !MVM_is_null(tc, rs->descriptor);
}

static void rakudo_scalar_cas(MVMThreadContext *tc, MVMObject *cont,
                              MVMObject *expected, MVMObject *value,
                              MVMRegister *result) {
    RakudoContData *data = (RakudoContData *)STABLE(cont)->container_data;
    MVMObject *code = MVM_frame_find_invokee(tc, data->cas, NULL);
    MVMCallsite *cs = MVM_callsite_get_common(tc, MVM_CALLSITE_ID_TYPECHECK);
    MVM_args_setup_thunk(tc, result, MVM_RETURN_OBJ, cs);
    tc->cur_frame->args[0].o = cont;
    tc->cur_frame->args[1].o = expected;
    tc->cur_frame->args[2].o = value;
    STABLE(code)->invoke(tc, code, cs, tc->cur_frame->args);
}

static MVMObject * rakudo_scalar_atomic_load(MVMThreadContext *tc, MVMObject *cont) {
    MVMObject *value = (MVMObject *)MVM_load(&(((Rakudo_Scalar *)cont)->value));
    return value ? value : tc->instance->VMNull;
}

void rakudo_scalar_atomic_store(MVMThreadContext *tc, MVMObject *cont, MVMObject *value) {
    RakudoContData *data = (RakudoContData *)STABLE(cont)->container_data;
    MVMObject *code = MVM_frame_find_invokee(tc, data->atomic_store, NULL);
    MVMCallsite *cs = MVM_callsite_get_common(tc, MVM_CALLSITE_ID_TWO_OBJ);
    MVM_args_setup_thunk(tc, NULL, MVM_RETURN_VOID, cs);
    tc->cur_frame->args[0].o = cont;
    tc->cur_frame->args[1].o = value;
    STABLE(code)->invoke(tc, code, cs, tc->cur_frame->args);
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
        value = grab_one_value(tc, config, "store_unchecked");
        MVM_ASSIGN_REF(tc, &(st->header), data->store_unchecked, value);
        value = grab_one_value(tc, config, "cas");
        MVM_ASSIGN_REF(tc, &(st->header), data->cas, value);
        value = grab_one_value(tc, config, "atomic_store");
        MVM_ASSIGN_REF(tc, &(st->header), data->atomic_store, value);
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
