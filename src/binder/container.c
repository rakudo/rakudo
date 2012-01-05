#define PARROT_IN_EXTENSION
#include "parrot/parrot.h"
#include "parrot/extend.h"
#include "container.h"
#include "sixmodelobject.h"
#include "bind.h"

static PMC *scalar_type = NULL;
void Rakudo_cont_set_scalar_type(PMC *type) { scalar_type = type; }

/* Takes a value potentially in a container and decontainerizes it. If
 * enough was configured to take an optimal slot-access path, just does
 * that. */
PMC *Rakudo_cont_decontainerize(PARROT_INTERP, PMC *var) {
    ContainerSpec *spec;
    
    /* Fast path for Perl 6 Scalar containers. */
    if (IS_CONCRETE(var)) {
        if (STABLE(var)->WHAT == scalar_type)
            return ((Rakudo_Scalar *)PMC_data(var))->value;
        
        /* Otherwise, fall back to the usual API. */
        spec = STABLE(var)->container_spec;
        if (spec) {
            if (!PMC_IS_NULL(spec->value_slot.class_handle)) {
                /* Just get slot. */
                return VTABLE_get_attr_keyed(interp, var, spec->value_slot.class_handle,
                    spec->value_slot.attr_name);
            }
            else {
                /* Invoke FETCH method. */
                PMC *old_ctx = Parrot_pcc_get_signature(interp, CURRENT_CONTEXT(interp));
                PMC *cappy   = Parrot_pmc_new(interp, enum_class_CallContext);
                VTABLE_push_pmc(interp, cappy, var);
                Parrot_pcc_invoke_from_sig_object(interp, spec->fetch_method, cappy);
                cappy = Parrot_pcc_get_signature(interp, CURRENT_CONTEXT(interp));
                Parrot_pcc_set_signature(interp, CURRENT_CONTEXT(interp), old_ctx);
                return VTABLE_get_pmc_keyed_int(interp, cappy, 0);
            }
        }
    }
    return var;
}

/* Grabs obj.HOW.name(obj) so we can display type name in error. */
static STRING * typename(PARROT_INTERP, PMC *obj) {
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

/* Does type check and rw check only if needed and then stores. Note that
 * it only really skips them if it's Scalar. */
void Rakudo_cont_store(PARROT_INTERP, PMC *cont, PMC *value,
                       INTVAL type_check, INTVAL rw_check) {
    /* Ensure the value we're storing is a 6model type. */
    if (value->vtable->base_type != Rakudo_smo_id())
        Parrot_ex_throw_from_c_args(interp, NULL, EXCEPTION_INVALID_OPERATION,
            "Cannot assign a non-Perl 6 value to a Perl 6 container");

    /* If it's a scalar container, optimized path. */
    if (PMC_IS_NULL(cont)) {
        Parrot_ex_throw_from_c_args(interp, NULL, EXCEPTION_INVALID_OPERATION,
            "Cannot assign into a PMCNULL container");
    }
    if (STABLE(cont)->WHAT == scalar_type) {
        Rakudo_Scalar *scalar = (Rakudo_Scalar *)PMC_data(cont);
        PMC *value_decont = Rakudo_cont_decontainerize(interp, value);
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
                Rakudo_ContainerDescriptor *desc = ((Rakudo_ContainerDescriptor *)PMC_data(scalar->descriptor));
                ok = STABLE(value_decont)->type_check(interp, value_decont, desc->of);
                if (!ok) {
                    Parrot_ex_throw_from_c_args(interp, NULL, EXCEPTION_INVALID_OPERATION,
                        "Type check failed in assignment to '%S'; expected '%S' but got '%S'",
                        desc->name, typename(interp, desc->of), typename(interp, value_decont));
                }
            }
            else {
                Parrot_ex_throw_from_c_args(interp, NULL, EXCEPTION_INVALID_OPERATION,
                    "Type check failed in assignment");
            }
        }

        if (!PMC_IS_NULL(scalar->whence)) {
            PMC *cappy = Parrot_pmc_new(interp, enum_class_CallContext);
            Parrot_pcc_invoke_from_sig_object(interp, scalar->whence, cappy);
            scalar->whence = PMCNULL;
        }
        
        /* If we get here, all is fine; store the value. */
        scalar->value = value_decont;
        PARROT_GC_WRITE_BARRIER(interp, cont);
    }
    
    /* Otherwise, use STORE call. */
    else {
        PMC *meth = STABLE(cont)->container_spec ?
            STABLE(cont)->find_method(interp, cont, Parrot_str_new(interp, "STORE", 0), NO_HINT) :
            VTABLE_find_method(interp, cont, Parrot_str_new(interp, "STORE", 0));
        if (!PMC_IS_NULL(meth)) {
            PMC *old_ctx = Parrot_pcc_get_signature(interp, CURRENT_CONTEXT(interp));
            PMC *cappy   = Parrot_pmc_new(interp, enum_class_CallContext);
            VTABLE_push_pmc(interp, cappy, cont);
            VTABLE_push_pmc(interp, cappy, value);
            Parrot_pcc_invoke_from_sig_object(interp, meth, cappy);
            Parrot_pcc_set_signature(interp, CURRENT_CONTEXT(interp), old_ctx);
        }
        else {
            Parrot_ex_throw_from_c_args(interp, NULL, EXCEPTION_INVALID_OPERATION,
                "Cannot assign to a non-container");
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

/* Creates and populates a new container descriptor. */
PMC * Rakudo_create_container_descriptor(PARROT_INTERP, PMC *type, PMC *of, INTVAL rw, STRING *name) {
    PMC *result = REPR(type)->allocate(interp, STABLE(type));
    Rakudo_ContainerDescriptor *desc = (Rakudo_ContainerDescriptor *)PMC_data(result);
    REPR(result)->initialize(interp, STABLE(result), OBJECT_BODY(result));
    desc->of = of;
    desc->rw = rw;
    desc->name = name;
    PARROT_GC_WRITE_BARRIER(interp, result);
    return result;
}
