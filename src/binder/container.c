#define PARROT_IN_EXTENSION
#include "parrot/parrot.h"
#include "parrot/extend.h"
#include "container.h"
#include "sixmodelobject.h"

static PMC *scalar_type = NULL;
void Rakudo_cont_set_scalar_type(PMC *type) { scalar_type = type; }

/* Takes a value potentially in a container and decontainerizes it. If
 * enough was configured to take an optimal slot-access path, just does
 * that. */
PMC *Rakudo_cont_decontainerize(PARROT_INTERP, PMC *var) {
    ContainerSpec *spec = STABLE(var)->container_spec;
    if (spec) {
        if (!PMC_IS_NULL(spec->value_slot.class_handle)) {
            /* Just get slot. */
            return VTABLE_get_attr_keyed(interp, var, spec->value_slot.class_handle,
                spec->value_slot.attr_name);
        }
        else {
            /* Invoke FETCH method. */
            PMC *old_ctx = Parrot_pcc_get_signature(interp, CURRENT_CONTEXT(interp));
            PMC *meth    = VTABLE_find_method(interp, var, Parrot_str_new(interp, "FETCH", 0));
            PMC *cappy   = Parrot_pmc_new(interp, enum_class_CallContext);
            VTABLE_push_pmc(interp, cappy, var);
            Parrot_pcc_invoke_from_sig_object(interp, meth, cappy);
            cappy = Parrot_pcc_get_signature(interp, CURRENT_CONTEXT(interp));
            Parrot_pcc_set_signature(interp, CURRENT_CONTEXT(interp), old_ctx);
            return VTABLE_get_pmc_keyed_int(interp, cappy, 0);
        }
    }
    return var;
}

/* Does type check and rw check only if needed and then stores. Note that
 * it only really skips them if it's Scalar. */
void Rakudo_cont_store(PARROT_INTERP, PMC *cont, PMC *value,
                       INTVAL type_check, INTVAL rw_check) {
    /* Ensure we have a container. */
    STable *cont_st = STABLE(cont);
    INTVAL is_container = cont_st->container_spec != NULL;
    
    /* If it's a scalar container, optimized path. */
    if (is_container && cont_st->WHAT == scalar_type) {
        Rakudo_Scalar *scalar = (Rakudo_Scalar *)PMC_data(cont);
        if (rw_check) {
            INTVAL rw = 0;
            if (!PMC_IS_NULL(scalar->descriptor))
                rw = ((Rakudo_ContainerDescriptor *)PMC_data(scalar->descriptor))->rw;
            if (!rw) {
                Parrot_ex_throw_from_c_args(interp, NULL, EXCEPTION_INVALID_OPERATION,
                    "Cannot assign to a readonly variable or a value");
            }
        }
        if (type_check) {
            INTVAL ok = 0;
            if (!PMC_IS_NULL(scalar->descriptor)) {
                PMC *type = ((Rakudo_ContainerDescriptor *)PMC_data(scalar->descriptor))->of;
                ok = STABLE(value)->type_check(interp, value, type);
            }
            if (!ok) {
                Parrot_ex_throw_from_c_args(interp, NULL, EXCEPTION_INVALID_OPERATION,
                    "Type check failed in assignment");
            }
        }
        
        /* If we get here, all is fine; store the value. */
        scalar->value = value;
    }
    
    /* Otherwise, use STORE call. */
    else if (is_container) {
        PMC *old_ctx = Parrot_pcc_get_signature(interp, CURRENT_CONTEXT(interp));
        PMC *meth    = VTABLE_find_method(interp, cont, Parrot_str_new(interp, "STORE", 0));
        PMC *cappy   = Parrot_pmc_new(interp, enum_class_CallContext);
        VTABLE_push_pmc(interp, cappy, cont);
        VTABLE_push_pmc(interp, cappy, value);
        Parrot_pcc_invoke_from_sig_object(interp, meth, cappy);
        Parrot_pcc_set_signature(interp, CURRENT_CONTEXT(interp), old_ctx);
    }
    
    /* If we get here, it's not a container at all. */
    else {
        Parrot_ex_throw_from_c_args(interp, NULL, EXCEPTION_INVALID_OPERATION,
            "Cannot assign to a non-container");
    }
}

/* Creates a new Scalar container with the associated container
 * descriptor. */
PMC * Rakudo_cont_scalar_from_descriptor(PARROT_INTERP, PMC *descriptor) {
    PMC *new_scalar = REPR(scalar_type)->instance_of(interp, scalar_type);
    ((Rakudo_Scalar *)PMC_data(new_scalar))->descriptor = descriptor;
    return new_scalar;
}
