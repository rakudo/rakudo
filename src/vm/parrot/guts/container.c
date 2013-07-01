#define PARROT_IN_EXTENSION
#include "parrot/parrot.h"
#include "parrot/extend.h"
#include "container.h"
#include "sixmodelobject.h"
#include "bind.h"
#include "exceptions.h"
#include "types.h"

static PMC *scalar_type = NULL;
void Rakudo_cont_set_scalar_type(PMC *type) { scalar_type = type; }

/* Grabs obj.HOW.name(obj) so we can display type name in error. */
static STRING * type_name(PARROT_INTERP, PMC *obj) {
    PMC *how     = STABLE(obj)->HOW;
    PMC *old_ctx = Parrot_pcc_get_signature(interp, CURRENT_CONTEXT(interp));
    PMC *meth    = VTABLE_find_method(interp, how, Parrot_str_new(interp, "name", 0));
    PMC *cappy   = Parrot_pmc_new(interp, enum_class_CallContext);
    VTABLE_push_pmc(interp, cappy, how);
    VTABLE_push_pmc(interp, cappy, obj);
    Parrot_pcc_invoke_from_sig_object(interp, meth, cappy);
    cappy = Parrot_pcc_get_signature(interp, CURRENT_CONTEXT(interp));
    Parrot_pcc_set_signature(interp, CURRENT_CONTEXT(interp), old_ctx);
    return VTABLE_get_string_keyed_int(interp, cappy, 0);
}

static PMC * rakudo_scalar_fetch(PARROT_INTERP, PMC *cont) {
    return ((Rakudo_Scalar *)PMC_data(cont))->value;
}

static void rakudo_scalar_store(PARROT_INTERP, PMC *cont, PMC *value) {
    Rakudo_Scalar *scalar = (Rakudo_Scalar *)PMC_data(cont);
    INTVAL rw = 0;
    INTVAL ok = 0;

    if (!PMC_IS_NULL(scalar->descriptor))
        rw = ((Rakudo_ContainerDescriptor *)PMC_data(scalar->descriptor))->rw;
    if (!rw) {
        Parrot_ex_throw_from_c_args(interp, NULL, EXCEPTION_INVALID_OPERATION,
            "Cannot assign to a readonly variable or a value");
    }

    if (!PMC_IS_NULL(scalar->descriptor)) {
        Rakudo_ContainerDescriptor *desc = ((Rakudo_ContainerDescriptor *)PMC_data(scalar->descriptor));
        ok = STABLE(value)->type_check(interp, value, desc->of);
        if (!ok) {
            if ( STABLE(value)->WHAT == Rakudo_types_nil_get() ) {
                value = desc->the_default;
            }
            else {
                PMC *thrower = Rakudo_get_thrower(interp, "X::TypeCheck::Assignment");
                if PMC_IS_NULL(thrower)
                    Parrot_ex_throw_from_c_args(interp, NULL, EXCEPTION_INVALID_OPERATION,
                        "Type check failed in assignment to '%S'; expected '%S' but got '%S'",
                        desc->name, type_name(interp, desc->of), type_name(interp, value));
                else
                    Parrot_pcc_invoke_sub_from_c_args(interp, thrower,
                            "SPP->", desc->name, value, desc->of);
            }
        }
    }
    else {
        Parrot_ex_throw_from_c_args(interp, NULL, EXCEPTION_INVALID_OPERATION,
            "Type check failed in assignment");
    }

    if (!PMC_IS_NULL(scalar->whence)) {
        PMC *cappy = Parrot_pmc_new(interp, enum_class_CallContext);
        Parrot_pcc_invoke_from_sig_object(interp, scalar->whence, cappy);
        scalar->whence = PMCNULL;
    }
    
    /* If we get here, all is fine; store the value. */
    scalar->value = value;
    PARROT_GC_WRITE_BARRIER(interp, cont);
}

static void rakudo_scalar_store_unchecked(PARROT_INTERP, PMC *cont, PMC *value) {
    Rakudo_Scalar *scalar = (Rakudo_Scalar *)PMC_data(cont);

    if (!PMC_IS_NULL(scalar->whence)) {
        PMC *cappy = Parrot_pmc_new(interp, enum_class_CallContext);
        Parrot_pcc_invoke_from_sig_object(interp, scalar->whence, cappy);
        scalar->whence = PMCNULL;
    }
    
    /* If we get here, all is fine; store the value. */
    scalar->value = value;
    PARROT_GC_WRITE_BARRIER(interp, cont);
}

static void rakudo_scalar_gc_mark_data(PARROT_INTERP, STable *st) {
    /* No data to mark. */
}

static void rakudo_scalar_gc_free_data(PARROT_INTERP, STable *st) {
    /* No data to free. */
}

static void rakudo_scalar_serialize(PARROT_INTERP, STable *st, SerializationWriter *writer) {
    /* No data to serialize. */
}
    
static void rakudo_scalar_deserialize(PARROT_INTERP, STable *st, SerializationReader *reader) {
    /* No data to deserialize. */
}

static ContainerSpec *rakudo_scalar_spec = NULL;

static void rakudo_scalar_set_container_spec(PARROT_INTERP, STable *st) {
    st->container_data = NULL;
    st->container_spec = rakudo_scalar_spec;
}
    
static void rakudo_scalar_configure_container_spec(PARROT_INTERP, STable *st, PMC *config) {
    /* Nothing to configure here. */
}

/* Sets up the container specification for Rakudo's container handling. */
void Rakudo_cont_register(PARROT_INTERP) {
    ContainerConfigurer *cc = mem_sys_allocate(sizeof(ContainerConfigurer));
    
    rakudo_scalar_spec = mem_sys_allocate(sizeof(ContainerSpec));
    rakudo_scalar_spec->name = Parrot_str_new_constant(interp, "rakudo_scalar");
    rakudo_scalar_spec->fetch = rakudo_scalar_fetch;
    rakudo_scalar_spec->store = rakudo_scalar_store;
    rakudo_scalar_spec->store_unchecked = rakudo_scalar_store_unchecked;
    rakudo_scalar_spec->gc_mark_data = rakudo_scalar_gc_mark_data;
    rakudo_scalar_spec->gc_free_data = rakudo_scalar_gc_free_data;
    rakudo_scalar_spec->serialize = rakudo_scalar_serialize;
    rakudo_scalar_spec->deserialize = rakudo_scalar_deserialize;
    
    cc->set_container_spec = rakudo_scalar_set_container_spec;
    cc->configure_container_spec = rakudo_scalar_configure_container_spec;
    
    REGISTER_DYNAMIC_CONTAINER_CONFIG(interp,
        Parrot_str_new_constant(interp, "rakudo_scalar"),
        cc);
}

/* Function wrapper around DECONT macro; potentially can go away at some
 * point. */
PMC *Rakudo_cont_decontainerize(PARROT_INTERP, PMC *var) {
    return DECONT(interp, var);
}

/* Perl 6 storage semantics. If it's a container, just use the container
 * protocol to handle it. Otherwise, fall back to calling a STORE
 * method. */
void Rakudo_cont_store(PARROT_INTERP, PMC *cont, PMC *value,
                       INTVAL type_check, INTVAL rw_check) {
    ContainerSpec *spec = STABLE(cont)->container_spec;
    if (spec) {
        /* Ensure the value we're storing is a 6model type. */
        if (value->vtable->base_type != Rakudo_smo_id())
            Parrot_ex_throw_from_c_args(interp, NULL, EXCEPTION_INVALID_OPERATION,
                "Cannot assign a non-Perl 6 value to a Perl 6 container");
        
        if (type_check || rw_check)
            spec->store(interp, cont, DECONT(interp, value));
        else
            spec->store_unchecked(interp, cont, DECONT(interp, value));
    }
    else {
        PMC *meth = VTABLE_find_method(interp, cont, Parrot_str_new(interp, "STORE", 0));
        if (!PMC_IS_NULL(meth)) {
            PMC *old_ctx = Parrot_pcc_get_signature(interp, CURRENT_CONTEXT(interp));
            PMC *cappy   = Parrot_pmc_new(interp, enum_class_CallContext);
            VTABLE_push_pmc(interp, cappy, cont);
            VTABLE_push_pmc(interp, cappy, value);
            Parrot_pcc_invoke_from_sig_object(interp, meth, cappy);
            Parrot_pcc_set_signature(interp, CURRENT_CONTEXT(interp), old_ctx);
        }
        else {
            PMC * thrower = Rakudo_get_thrower(interp, "X::Assignment::RO");
            if (PMC_IS_NULL(thrower))
                Parrot_ex_throw_from_c_args(interp, NULL, EXCEPTION_INVALID_OPERATION,
                    "Cannot assign to a non-container");
            else
                Parrot_pcc_invoke_sub_from_c_args(interp, thrower, "->");
        }
    }
}

/* Checks if the thing we have is a rw scalar. */
INTVAL Rakudo_cont_is_rw_scalar(PARROT_INTERP, PMC *check) {
    if (IS_CONCRETE(check) && STABLE(check)->WHAT == scalar_type) {
        Rakudo_Scalar *scalar = (Rakudo_Scalar *)PMC_data(check);
        if (!PMC_IS_NULL(scalar->descriptor))
            return ((Rakudo_ContainerDescriptor *)PMC_data(scalar->descriptor))->rw;
    }
    return 0;
}

/* Creates a new Scalar container with the associated container
 * descriptor. */
PMC * Rakudo_cont_scalar_from_descriptor(PARROT_INTERP, PMC *descriptor) {
    PMC *new_scalar = REPR(scalar_type)->allocate(interp, STABLE(scalar_type));
    REPR(new_scalar)->initialize(interp, STABLE(new_scalar), OBJECT_BODY(new_scalar));
    ((Rakudo_Scalar *)PMC_data(new_scalar))->descriptor = descriptor;
    PARROT_GC_WRITE_BARRIER(interp, new_scalar);
    return new_scalar;
}

/* Creates a new Scalar container with the associated container
 * descriptor. */
PMC * Rakudo_cont_scalar_with_value_no_descriptor(PARROT_INTERP, PMC *value) {
    PMC *new_scalar = REPR(scalar_type)->allocate(interp, STABLE(scalar_type));
    REPR(new_scalar)->initialize(interp, STABLE(new_scalar), OBJECT_BODY(new_scalar));
    ((Rakudo_Scalar *)PMC_data(new_scalar))->value = value;
    PARROT_GC_WRITE_BARRIER(interp, new_scalar);
    return new_scalar;
}
