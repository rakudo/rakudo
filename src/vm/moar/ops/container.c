#define MVM_SHARED 1
#include "moar.h"
#include "container.h"

/* Dummy, no-arg callsite. */
static MVMCallsite no_arg_callsite = { NULL, 0, 0, 0 };

static void rakudo_scalar_fetch(MVMThreadContext *tc, MVMObject *cont, MVMRegister *res) {
    res->o = ((Rakudo_Scalar *)cont)->value;
}

static void rakudo_scalar_store(MVMThreadContext *tc, MVMObject *cont, MVMObject *obj) {
    MVMObject *whence;

    /* XXX Type check, rw-ness check */
    MVM_ASSIGN_REF(tc, cont, ((Rakudo_Scalar *)cont)->value, obj);

    /* Run any whence closure. */
    whence = ((Rakudo_Scalar *)cont)->whence;
    if (whence && IS_CONCRETE(whence)) {
        MVMObject *code = MVM_frame_find_invokee(tc, whence, NULL);
        tc->cur_frame->return_type    = MVM_RETURN_VOID;
        tc->cur_frame->return_address = *(tc->interp_cur_op);
        STABLE(code)->invoke(tc, code, &no_arg_callsite, tc->cur_frame->args);
        ((Rakudo_Scalar *)cont)->whence = NULL;
    }
}

static void rakudo_scalar_store_unchecked(MVMThreadContext *tc, MVMObject *cont, MVMObject *obj) {
    MVMObject *whence;

    /* Store the value. */
    MVM_ASSIGN_REF(tc, cont, ((Rakudo_Scalar *)cont)->value, obj);

    /* Run any whence closure. */
    whence = ((Rakudo_Scalar *)cont)->whence;
    if (whence && IS_CONCRETE(whence)) {
        MVMObject *code = MVM_frame_find_invokee(tc, whence, NULL);
        tc->cur_frame->return_type    = MVM_RETURN_VOID;
        tc->cur_frame->return_address = *(tc->interp_cur_op);
        STABLE(code)->invoke(tc, code, &no_arg_callsite, tc->cur_frame->args);
        ((Rakudo_Scalar *)cont)->whence = NULL;
    }
}

static void rakudo_scalar_serialize(MVMThreadContext *tc, MVMSTable *st, MVMSerializationWriter *writer) {
    /* Nothing to do. */
}

static void rakudo_scalar_deserialize(MVMThreadContext *tc, MVMSTable *st, MVMSerializationReader *reader) {
    /* Nothing to do. */
}

static const MVMContainerSpec rakudo_scalar_spec = {
    "rakudo_scalar",
    rakudo_scalar_fetch,
    rakudo_scalar_store,
    rakudo_scalar_store_unchecked,
    NULL,
    NULL,
    rakudo_scalar_serialize,
    rakudo_scalar_deserialize,
    1
};

static void rakudo_scalar_set_container_spec(MVMThreadContext *tc, MVMSTable *st) {
    st->container_spec = &rakudo_scalar_spec;
}

static void rakudo_scalar_configure_container_spec(MVMThreadContext *tc, MVMSTable *st, MVMObject *config) {
    /* Nothing to do. */
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
    return &rakudo_scalar_spec;
}
