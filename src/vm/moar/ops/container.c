#define MVM_SHARED 1
#include "moar.h"
#include "container.h"

/* Dummy, no-arg callsite. */
static MVMCallsite no_arg_callsite = { NULL, 0, 0, 0 };

static void rakudo_scalar_fetch(MVMThreadContext *tc, MVMObject *cont, MVMRegister *res) {
    res->o = ((Rakudo_Scalar *)cont)->value;
}

MVMObject * get_nil();
MVMObject * get_mu();

static void finish_store(MVMThreadContext *tc, MVMObject *cont, MVMObject *obj) {
    Rakudo_Scalar *rs = (Rakudo_Scalar *)cont;
    MVMObject *whence;

    /* Store the value. */
    MVM_ASSIGN_REF(tc, cont, rs->value, obj);

    /* Run any whence closure. */
    whence = rs->whence;
    if (whence && IS_CONCRETE(whence)) {
        MVMObject *code = MVM_frame_find_invokee(tc, whence, NULL);
        MVM_args_setup_thunk(tc, NULL, MVM_RETURN_VOID, &no_arg_callsite);
        rs->whence = NULL;
        STABLE(code)->invoke(tc, code, &no_arg_callsite, tc->cur_frame->args);
    }
}

static void typecheck_failed(MVMThreadContext *tc, MVMObject *cont, MVMObject *obj) {
    /* XXX TODO: Improve this error reporting by looking up typed thrower. */
    MVM_exception_throw_adhoc(tc, "Type check failed in assignment");
}

static void rakudo_scalar_store(MVMThreadContext *tc, MVMObject *cont, MVMObject *obj) {
    Rakudo_Scalar *rs = (Rakudo_Scalar *)cont;
    Rakudo_ContainerDescriptor *rcd = (Rakudo_ContainerDescriptor *)rs->descriptor;
    MVMint64 rw = 0;

    /* Check it's an assignable container. */
    if (rcd)
        rw = rcd->rw;
    if (!rw)
        MVM_exception_throw_adhoc(tc, "Cannot assign to a readonly variable or a value");

    /* Handle Nil and type-checking. */
    if (!obj) {
        MVM_exception_throw_adhoc(tc, "Cannot assign a null value to a Perl 6 scalar");
    }
    else if (STABLE(obj)->WHAT == get_nil()) {
        if (rcd) {
            obj = rcd->the_default;
        }
    }
    else {
        /* Check against the type-check cache first (common, fast-path
         * case). */
        MVMint64 mode = STABLE(rcd->of)->mode_flags & MVM_TYPE_CHECK_CACHE_FLAG_MASK;
        if (rcd->of != get_mu() && !MVM_6model_istype_cache_only(tc, obj, rcd->of)) {
            /* Failed. If the cache is definitive, we certainly have an error. */
            if ((mode & MVM_TYPE_CHECK_CACHE_THEN_METHOD) == 0 &&
                (mode & MVM_TYPE_CHECK_NEEDS_ACCEPTS) == 0)
                typecheck_failed(tc, cont, obj);

            /* XXX TODO: other cases. */
        }
    }

    /* Complete the store. */
    finish_store(tc, cont, obj);
}

static void rakudo_scalar_store_unchecked(MVMThreadContext *tc, MVMObject *cont, MVMObject *obj) {
    MVMObject *whence;
    Rakudo_Scalar *rs = (Rakudo_Scalar *)cont;

    /* Handle Nil assignment. */
    if (obj && STABLE(obj)->WHAT == get_nil()) {
        Rakudo_ContainerDescriptor *rcd = (Rakudo_ContainerDescriptor *)rs->descriptor;
        if (rcd)
            obj = rcd->the_default;
        else
            MVM_exception_throw_adhoc(tc, "Cannot assign to a readonly variable or a value");
    }

    /* Complete the store. */
    finish_store(tc, cont, obj);
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
