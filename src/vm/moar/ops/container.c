#define MVM_SHARED 1
#include "moar.h"
#include "container.h"

static void rakudo_scalar_fetch(MVMThreadContext *tc, MVMObject *cont, MVMRegister *res) {
    res->o = ((Rakudo_Scalar *)cont)->value;
}

static void rakudo_scalar_store(MVMThreadContext *tc, MVMObject *cont, MVMObject *obj) {
    MVM_exception_throw_adhoc(tc, "rakudo_scalar NYI");
    
}

static void rakudo_scalar_store_unchecked(MVMThreadContext *tc, MVMObject *cont, MVMObject *obj) {
    MVM_exception_throw_adhoc(tc, "rakudo_scalar NYI");
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
    rakudo_scalar_deserialize
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
