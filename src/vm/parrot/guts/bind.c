/*
$Id$
Copyright (C) 2009-2013, The Perl Foundation.
*/

#define PARROT_IN_EXTENSION
#include "parrot/parrot.h"
#include "parrot/extend.h"
#include "pmc_callcontext.h"
#include "bind.h"
#include "container.h"
#include "types.h"
#include "sixmodelobject.h"

/* Cache of Parrot type IDs and some strings. */
static INTVAL smo_id            = 0;
static INTVAL qrpa_id           = 0;
static STRING *ACCEPTS          = NULL;
static STRING *HOW              = NULL;
static STRING *DO_str           = NULL;
static STRING *SELF_str         = NULL;
static STRING *NAME_str         = NULL;
static STRING *BLOCK_str        = NULL;
static STRING *CAPTURE_str      = NULL;
static STRING *STORAGE_str      = NULL;
static STRING *REST_str         = NULL;
static STRING *LIST_str         = NULL;
static STRING *HASH_str         = NULL;
static STRING *FLATTENS_str     = NULL;
static STRING *SHAPE_str        = NULL;
static STRING *NEXTITER_str     = NULL;
static STRING *HASH_SIGIL_str   = NULL;
static STRING *ARRAY_SIGIL_str  = NULL;
static STRING *BANG_TWIGIL_str  = NULL;
static STRING *SCALAR_SIGIL_str = NULL;
static STRING *NAMED_str        = NULL;
static STRING *INSTANTIATE_GENERIC_str = NULL;

/* Initializes our cached versions of some strings and type IDs that we
 * use very commonly. For strings, this should mean we only compute their
 * hash value once, rather than every time we create and consume them. */
static void setup_binder_statics(PARROT_INTERP) {
    ACCEPTS          = Parrot_str_new_constant(interp, "ACCEPTS");
    HOW              = Parrot_str_new_constant(interp, "HOW");
    DO_str           = Parrot_str_new_constant(interp, "$!do");
    NAME_str         = Parrot_str_new_constant(interp, "name");
    SELF_str         = Parrot_str_new_constant(interp, "self");
    BLOCK_str        = Parrot_str_new_constant(interp, "Block");
    CAPTURE_str      = Parrot_str_new_constant(interp, "Capture");
    STORAGE_str      = Parrot_str_new_constant(interp, "$!storage");
    REST_str         = Parrot_str_new_constant(interp, "$!rest");
    LIST_str         = Parrot_str_new_constant(interp, "$!list");
    HASH_str         = Parrot_str_new_constant(interp, "$!hash");
    FLATTENS_str     = Parrot_str_new_constant(interp, "$!flattens");
    SHAPE_str        = Parrot_str_new_constant(interp, "$!shape");
    NEXTITER_str     = Parrot_str_new_constant(interp, "$!nextiter");
    HASH_SIGIL_str   = Parrot_str_new_constant(interp, "%");
    ARRAY_SIGIL_str  = Parrot_str_new_constant(interp, "@");
    BANG_TWIGIL_str  = Parrot_str_new_constant(interp, "!");
    SCALAR_SIGIL_str = Parrot_str_new_constant(interp, "$");
    NAMED_str        = Parrot_str_new_constant(interp, "named");
    INSTANTIATE_GENERIC_str = Parrot_str_new_constant(interp, "instantiate_generic");
    
    smo_id  = Parrot_pmc_get_type_str(interp, Parrot_str_new(interp, "SixModelObject", 0));
    qrpa_id = Parrot_pmc_get_type_str(interp, Parrot_str_new(interp, "QRPA", 0));
}


/* Gets the ID of a 6model object PMC. */
INTVAL Rakudo_smo_id(void) { return smo_id; }

/* Checks that a PMC is a native list object */
INTVAL Rakudo_isnqplist(PMC *pmc) {
    return (INTVAL)(pmc->vtable->base_type == qrpa_id
                    || pmc->vtable->base_type == enum_class_ResizablePMCArray);
}


/* Return the type we'd box a native value to. */
static PMC *
box_type(Rakudo_BindVal bv) {
    switch (bv.type) {
        case BIND_VAL_INT:
            return Rakudo_types_int_get();
        case BIND_VAL_NUM:
            return Rakudo_types_num_get();
        case BIND_VAL_STR:
            return Rakudo_types_str_get();
        default:
            return Rakudo_types_mu_get();
    }
}


/* Boxes a native value. */
static PMC *
create_box(PARROT_INTERP, Rakudo_BindVal bv) {
    PMC *box_type_obj = box_type(bv);
    PMC *boxed = REPR(box_type_obj)->allocate(interp, STABLE(box_type_obj));
    switch (bv.type) {
        case BIND_VAL_INT:
            REPR(boxed)->box_funcs->set_int(interp, STABLE(boxed), OBJECT_BODY(boxed), bv.val.i);
            break;
        case BIND_VAL_NUM:
            REPR(boxed)->box_funcs->set_num(interp, STABLE(boxed), OBJECT_BODY(boxed), bv.val.n);
            break;
        case BIND_VAL_STR:
            REPR(boxed)->box_funcs->set_str(interp, STABLE(boxed), OBJECT_BODY(boxed), bv.val.s);
            break;
    }
    return boxed;
}


/* Creates a Parcel from a RPA, filling PMCNULL elements if needed. */
/* This function gets shared with perl6.ops for the perl6_parcel_from_rpa op. */
PMC *
Rakudo_binding_parcel_from_rpa(PARROT_INTERP, PMC *rpa, PMC *fill) {
    PMC *type = Rakudo_types_parcel_get();
    PMC *parcel = REPR(type)->allocate(interp, STABLE(type));
    VTABLE_set_attr_keyed(interp, parcel, type, STORAGE_str, rpa);

    if (!PMC_IS_NULL(fill)) {
        INTVAL elems = VTABLE_elements(interp, rpa);
        INTVAL i;
        for (i = 0; i < elems; i++) {
            if (PMC_IS_NULL(VTABLE_get_pmc_keyed_int(interp, rpa, i)))
                VTABLE_set_pmc_keyed_int(interp, rpa, i, fill);
        }
    }

    return parcel;
}
        

/* Creates a ListIter from a RPA */
/* This function gets shared with perl6.ops for the perl6_iter_from_rpa op. */
PMC *
Rakudo_binding_iter_from_rpa(PARROT_INTERP, PMC *rpa, PMC *list) {
    PMC *type = Rakudo_types_listiter_get();
    PMC *iter = REPR(type)->allocate(interp, STABLE(type));
    VTABLE_set_attr_keyed(interp, iter, type, REST_str, rpa);
    VTABLE_set_attr_keyed(interp, iter, type, LIST_str, list);
    return iter;
}


/* Creates a List from type and a RPA, initializing the iterator */
/* This function gets shared with perl6.ops for the perl6_list_from_rpa op. */
PMC *
Rakudo_binding_list_from_rpa(PARROT_INTERP, PMC *rpa, PMC *type, PMC *flattens) {
    PMC *list = REPR(type)->allocate(interp, STABLE(type));
    PMC *List = Rakudo_types_list_get();
    if (!PMC_IS_NULL(rpa)) 
        VTABLE_set_attr_keyed(interp, list, List, NEXTITER_str,
            Rakudo_binding_iter_from_rpa(interp, rpa, list));
    VTABLE_set_attr_keyed(interp, list, List, FLATTENS_str, flattens);
    return list;
}


/* Creates a Perl 6 Array. */
static PMC *
Rakudo_binding_create_positional(PARROT_INTERP, PMC *rpa) {
    PMC *Array = Rakudo_types_array_get();
    PMC *Whatever = Rakudo_types_whatever_get();
    PMC *list = Rakudo_binding_list_from_rpa(interp, rpa, Array,
        Rakudo_types_bool_true_get());
    VTABLE_set_attr_keyed(interp, list, Array, SHAPE_str,
        REPR(Whatever)->allocate(interp, STABLE(Whatever)));
    return list;
}


/* Creates a Perl 6 List. */
static PMC *
Rakudo_binding_create_list(PARROT_INTERP, PMC *rpa) {
    return Rakudo_binding_list_from_rpa(interp, rpa, Rakudo_types_list_get(),
               Rakudo_types_bool_true_get());
}


/* Creates a Perl 6 LoL. */
static PMC *
Rakudo_binding_create_lol(PARROT_INTERP, PMC *rpa) {
    return Rakudo_binding_list_from_rpa(interp, rpa, Rakudo_types_lol_get(),
               Rakudo_types_bool_false_get());
}


/* Creates a Perl 6 Hash. */
static PMC *
Rakudo_binding_create_hash(PARROT_INTERP, PMC *storage) {
    PMC *type = Rakudo_types_hash_get();
    PMC *hash = REPR(type)->allocate(interp, STABLE(type));
    VTABLE_set_attr_keyed(interp, hash, Rakudo_types_enummap_get(), STORAGE_str, storage);
    return hash;
}


static STRING *
Rakudo_binding_arity_fail(PARROT_INTERP, PMC *params, INTVAL num_params,
                          INTVAL num_pos_args, INTVAL too_many) {
    STRING *result;
    INTVAL arity = 0;
    INTVAL count = 0;
    INTVAL i;
    const char *whoz_up = too_many ? "Too many" : "Not enough";

    /* Work out how many we could have been passed. */
    for (i = 0; i < num_params; i++) {
        Rakudo_Parameter *param = (Rakudo_Parameter *)PMC_data(
            VTABLE_get_pmc_keyed_int(interp, params, i));

        if (!PMC_IS_NULL(param->named_names))
            continue;
        if (param->flags & SIG_ELEM_SLURPY_NAMED)
            continue;
        if (param->flags & SIG_ELEM_SLURPY_POS) {
            count = -1;
        }
        else if (param->flags & SIG_ELEM_IS_OPTIONAL) {
            count++;
        }
        else {
            count++;
            arity++;
        }
    }

    /* Now generate decent error. */
    if (arity == count)
        result = Parrot_sprintf_c(interp, "%s positional parameters passed; got %d but expected %d",
                whoz_up, num_pos_args, arity);
    else if (count == -1)
        result = Parrot_sprintf_c(interp, "%s positional parameters passed; got %d but expected at least %d",
                whoz_up, num_pos_args, arity);
    else
        result = Parrot_sprintf_c(interp, "%s positional parameters passed; got %d but expected between %d and %d",
                whoz_up, num_pos_args, arity, count);
    return result;
}


/* Binds any type captures a variable has. */
static void
Rakudo_binding_bind_type_captures(PARROT_INTERP, PMC *lexpad, Rakudo_Parameter *param, Rakudo_BindVal value) {
    PMC * type_obj = value.type == BIND_VAL_OBJ ?
        STABLE(Rakudo_cont_decontainerize(interp, value.val.o))->WHAT :
        box_type(value);
    PMC * iter     = VTABLE_get_iter(interp, param->type_captures);
    while (VTABLE_get_bool(interp, iter)) {
        STRING *name = VTABLE_shift_string(interp, iter);
        VTABLE_set_pmc_keyed_str(interp, lexpad, name, type_obj);
    }
}


/* Assigns an attributive parameter to the desired attribute. */
static INTVAL
Rakudo_binding_assign_attributive(PARROT_INTERP, PMC *lexpad, Rakudo_Parameter *param,
                                  Rakudo_BindVal value, PMC *decont_value, STRING **error) {
    PMC *assignee = PMCNULL;
    PMC *assigner;

    /* Find self. */
    PMC *self = VTABLE_get_pmc_keyed_str(interp, lexpad,
            Parrot_str_new(interp, "self", 0));
    if (PMC_IS_NULL(self)) {
        if (error)
            *error = Parrot_sprintf_c(interp,
                    "Unable to bind attributive parameter '%S' - could not find self",
                    param->variable_name);
        return BIND_RESULT_FAIL;
    }
    
    /* Ensure it's not native; NYI. */
    if (value.type != BIND_VAL_OBJ) {
        *error = Parrot_sprintf_c(interp,
            "Binding to natively typed attributive parameter '%S' not supported",
            param->variable_name);
        return BIND_RESULT_FAIL;
    }

    /* If it's private, just need to fetch the attribute. */
    if (param->flags & SIG_ELEM_BIND_PRIVATE_ATTR) {
        assignee = VTABLE_get_attr_keyed(interp, self, param->attr_package,
            param->variable_name);
    }

    /* Otherwise if it's public, do a method call to get the assignee. */
    else {
        PMC *meth = VTABLE_find_method(interp, self, param->variable_name);
        if (PMC_IS_NULL(meth)) {
            if (error)
                *error = Parrot_sprintf_c(interp,
                        "Unable to bind attributive parameter '$.%S' - could not find method '%S'",
                        param->variable_name,
                        param->variable_name);
            return BIND_RESULT_FAIL;
        }
        Parrot_ext_call(interp, meth, "Pi->P", self, &assignee);
    }

    Rakudo_cont_store(interp, assignee, decont_value, 1, 1);
    return BIND_RESULT_OK;
}


/* Returns an appropriate failure mode (junction fail or normal fail). */
static INTVAL junc_or_fail(PARROT_INTERP, PMC *value) {
    if (value->vtable->base_type == smo_id && STABLE(value)->WHAT == Rakudo_types_junction_get())
        return BIND_RESULT_JUNCTION;
    else
        return BIND_RESULT_FAIL;
}


/* Binds a single argument into the lexpad, after doing any checks that are
 * needed. Also handles any type captures. If there is a sub signature, then
 * re-enters the binder. Returns one of the BIND_RESULT_* codes. */
static INTVAL
Rakudo_binding_bind_one_param(PARROT_INTERP, PMC *lexpad, Rakudo_Signature *signature, Rakudo_Parameter *param,
                              Rakudo_BindVal orig_bv, INTVAL no_nom_type_check, STRING **error) {
    PMC            *decont_value = NULL;
    INTVAL          desired_native;
    Rakudo_BindVal  bv;
    
    /* Check if boxed/unboxed expections are met. */
    desired_native = param->flags & SIG_ELEM_NATIVE_VALUE;
    if ((desired_native == 0 && orig_bv.type == BIND_VAL_OBJ) ||
        (desired_native == SIG_ELEM_NATIVE_INT_VALUE && orig_bv.type == BIND_VAL_INT) ||
        (desired_native == SIG_ELEM_NATIVE_NUM_VALUE && orig_bv.type == BIND_VAL_NUM) ||
        (desired_native == SIG_ELEM_NATIVE_STR_VALUE && orig_bv.type == BIND_VAL_STR))
    {
        /* We have what we want. */
        bv = orig_bv;
    }
    else if (desired_native == 0) {
        /* We need to do a boxing operation. */
        bv.type = BIND_VAL_OBJ;
        bv.val.o = create_box(interp, orig_bv);
    }
    else if (orig_bv.val.o->vtable->base_type == smo_id) {
        storage_spec spec;
        decont_value = Rakudo_cont_decontainerize(interp, orig_bv.val.o);
        spec = REPR(decont_value)->get_storage_spec(interp, STABLE(decont_value));
        switch (desired_native) {
            case SIG_ELEM_NATIVE_INT_VALUE:
                if (spec.can_box & STORAGE_SPEC_CAN_BOX_INT) {
                    bv.type = BIND_VAL_INT;
                    bv.val.i = REPR(decont_value)->box_funcs->get_int(interp, STABLE(decont_value), OBJECT_BODY(decont_value));
                }
                else {
                    if (error)
                        *error = Parrot_sprintf_c(interp, "Cannot unbox argument to '%S' as a native int",
                            param->variable_name);
                    return BIND_RESULT_FAIL;
                }
                break;
            case SIG_ELEM_NATIVE_NUM_VALUE:
                if (spec.can_box & STORAGE_SPEC_CAN_BOX_NUM) {
                    bv.type = BIND_VAL_NUM;
                    bv.val.n = REPR(decont_value)->box_funcs->get_num(interp, STABLE(decont_value), OBJECT_BODY(decont_value));
                }
                else {
                    if (error)
                        *error = Parrot_sprintf_c(interp, "Cannot unbox argument to '%S' as a native num",
                            param->variable_name);
                    return BIND_RESULT_FAIL;
                }
                break;
            case SIG_ELEM_NATIVE_STR_VALUE:
                if (spec.can_box & STORAGE_SPEC_CAN_BOX_STR) {
                    bv.type = BIND_VAL_STR;
                    bv.val.s = REPR(decont_value)->box_funcs->get_str(interp, STABLE(decont_value), OBJECT_BODY(decont_value));
                }
                else {
                    if (error)
                        *error = Parrot_sprintf_c(interp, "Cannot unbox argument to '%S' as a native str",
                            param->variable_name);
                    return BIND_RESULT_FAIL;
                }
                break;
            default:
                if (error)
                    *error = Parrot_sprintf_c(interp, "Cannot unbox argument to '%S' as a native type",
                        param->variable_name);
                return BIND_RESULT_FAIL;
        }
        decont_value = NULL;
    }
    else if (orig_bv.val.o->vtable->base_type == enum_class_Integer &&
             desired_native == SIG_ELEM_NATIVE_INT_VALUE) {
         bv.type = BIND_VAL_INT;
         bv.val.i = VTABLE_get_integer(interp, orig_bv.val.o);
    }
    else if (orig_bv.val.o->vtable->base_type == enum_class_Float &&
             desired_native == SIG_ELEM_NATIVE_NUM_VALUE) {
         bv.type = BIND_VAL_NUM;
         bv.val.n = VTABLE_get_number(interp, orig_bv.val.o);
    }
    else if (orig_bv.val.o->vtable->base_type == enum_class_String &&
             desired_native == SIG_ELEM_NATIVE_STR_VALUE) {
         bv.type = BIND_VAL_STR;
         bv.val.s = VTABLE_get_string(interp, orig_bv.val.o);
    }
    else {
        if (error)
            *error = Parrot_sprintf_c(interp, "Cannot unbox argument to '%S' as a native type",
                param->variable_name);
        return BIND_RESULT_FAIL;
    }
    
    /* By this point, we'll either have an object that we might be able to
     * bind if it passes the type check, or a native value that needs no
     * further checking. */
    if (bv.type == BIND_VAL_OBJ) {
        /* Ensure the value is a 6model object; if not, marshall it to one. */
        if (bv.val.o->vtable->base_type != smo_id) {
            bv.val.o = Rakudo_types_parrot_map(interp, bv.val.o);
            if (bv.val.o->vtable->base_type != smo_id) {
                *error = Parrot_sprintf_c(interp, "Unmarshallable foreign language value passed for parameter '%S'",
                        param->variable_name);
                return BIND_RESULT_FAIL;
            }
        }

        /* We pretty much always need to de-containerized value, so get it
         * right off. */
        decont_value = Rakudo_cont_decontainerize(interp, bv.val.o);
    
        /* Skip nominal type check if not needed. */
        if (!no_nom_type_check) {
            PMC *nom_type;
            
            /* Is the nominal type generic and in need of instantiation? (This
             * can happen in (::T, T) where we didn't learn about the type until
             * during the signature bind). */
            if (param->flags & SIG_ELEM_NOMINAL_GENERIC) {
                PMC *HOW = STABLE(param->nominal_type)->HOW;
                PMC *ig  = VTABLE_find_method(interp, HOW, INSTANTIATE_GENERIC_str);
                Parrot_ext_call(interp, ig, "PiPP->P", HOW, param->nominal_type,
                    lexpad, &nom_type);
            }
            else {
                nom_type = param->nominal_type;
            }

            /* If not, do the check. If the wanted nominal type is Mu, then
             * anything goes. */
            if (nom_type != Rakudo_types_mu_get() &&
                    (decont_value->vtable->base_type != smo_id ||
                     !STABLE(decont_value)->type_check(interp, decont_value, nom_type))) {
                /* Type check failed; produce error if needed. */
                if (error) {
                    PMC    * got_how       = STABLE(decont_value)->HOW;
                    PMC    * exp_how       = STABLE(nom_type)->HOW;
                    PMC    * got_name_meth = VTABLE_find_method(interp, got_how, NAME_str);
                    PMC    * exp_name_meth = VTABLE_find_method(interp, exp_how, NAME_str);
                    STRING * expected, * got;
                    Parrot_ext_call(interp, got_name_meth, "PiP->S", got_how, bv.val.o, &got);
                    Parrot_ext_call(interp, exp_name_meth, "PiP->S", exp_how, nom_type, &expected);
                    *error = Parrot_sprintf_c(interp, "Nominal type check failed for parameter '%S'; expected %S but got %S instead",
                                param->variable_name, expected, got);
                }
                
                /* Report junction failure mode if it's a junction. */
                return junc_or_fail(interp, decont_value);
            }
            
            /* Also enforce definedness constraints. */
            if (param->flags & SIG_ELEM_DEFINEDNES_CHECK) {
                INTVAL defined = IS_CONCRETE(decont_value);
                if (defined && param->flags & SIG_ELEM_UNDEFINED_ONLY) {
                    if (error) {
                        if (param->flags & SIG_ELEM_INVOCANT) {
                            *error = Parrot_sprintf_c(interp,
                                "Invocant requires a type object, but an object instance was passed" );
                        }
                        else {
                            *error = Parrot_sprintf_c(interp,
                                "Parameter '%S' requires a type object, but an object instance was passed",
                                param->variable_name);
                        }
                    }
                    return junc_or_fail(interp, decont_value);
                }
                if (!defined && param->flags & SIG_ELEM_DEFINED_ONLY) {
                    if (error) {
                        if (param->flags & SIG_ELEM_INVOCANT) {
                            *error = Parrot_sprintf_c(interp,
                                "Invocant requires an instance, but a type object was passed" );
                        }
                        else {
                            *error = Parrot_sprintf_c(interp,
                                "Parameter '%S' requires an instance, but a type object was passed",
                                param->variable_name);
                        }
                    }
                    return junc_or_fail(interp, decont_value);
                }
            }
        }
    }

    /* Do we have any type captures to bind? */
    if (!PMC_IS_NULL(param->type_captures)) {
        Rakudo_binding_bind_type_captures(interp, lexpad, param, bv);
    }

    /* Do a coercion, if one is needed. */
    if (!PMC_IS_NULL(param->coerce_type)) {
        /* Coercing natives not possible - nothing to call a method on. */
        if (bv.type != BIND_VAL_OBJ) {
            if (error)
                *error = Parrot_sprintf_c(interp,
                    "Unable to coerce natively typed parameter '%S'",
                    param->variable_name);
            return BIND_RESULT_FAIL;
        }

        /* Only coerce if we don't already have the correct type. */
        if (!STABLE(decont_value)->type_check(interp, decont_value, param->coerce_type)) {
            PMC *coerce_meth = VTABLE_find_method(interp, decont_value, param->coerce_method);
            if (!PMC_IS_NULL(coerce_meth)) {
                Parrot_ext_call(interp, coerce_meth, "Pi->P", decont_value, &decont_value);
            }
            else {
                /* No coercion method availale; whine and fail to bind. */
                if (error) {
                    PMC    * got_how       = STABLE(decont_value)->HOW;
                    PMC    * got_name_meth = VTABLE_find_method(interp, got_how, NAME_str);
                    STRING * got;
                    Parrot_ext_call(interp, got_name_meth, "PiP->S", got_how, decont_value, &got);
                    *error = Parrot_sprintf_c(interp,
                            "Unable to coerce value for '%S' from %S to %S; no coercion method defined",
                            param->variable_name, got, param->coerce_method);
                }
                return BIND_RESULT_FAIL;
            }
        }
    }

    /* If it's not got attributive binding, we'll go about binding it into the
     * lex pad. */
    if (!(param->flags & SIG_ELEM_BIND_ATTRIBUTIVE) && !STRING_IS_NULL(param->variable_name)) {
        /* Is it native? If so, just go ahead and bind it. */
        if (bv.type != BIND_VAL_OBJ) {
            switch (bv.type) {
                case BIND_VAL_INT:
                    VTABLE_set_integer_keyed_str(interp, lexpad, param->variable_name, bv.val.i);
                    break;
                case BIND_VAL_NUM:
                    VTABLE_set_number_keyed_str(interp, lexpad, param->variable_name, bv.val.n);
                    break;
                case BIND_VAL_STR:
                    VTABLE_set_string_keyed_str(interp, lexpad, param->variable_name, bv.val.s);
                    break;
            }
        }
        
        /* Otherwise it's some objecty case. */
        else if (param->flags & SIG_ELEM_IS_RW) {
            /* XXX TODO Check if rw flag is set; also need to have a
             * wrapper container that carries extra constraints. */
            VTABLE_set_pmc_keyed_str(interp, lexpad, param->variable_name, bv.val.o);
        }
        else if (param->flags & SIG_ELEM_IS_PARCEL) {
            /* Just bind the thing as is into the lexpad. */
            VTABLE_set_pmc_keyed_str(interp, lexpad, param->variable_name, bv.val.o);
        }
        else {
            /* If it's an array, copy means make a new one and store,
             * and a normal bind is a straightforward binding plus
             * adding a constraint. */
            if (param->flags & SIG_ELEM_ARRAY_SIGIL) {
                PMC *bindee = decont_value;
                if (param->flags & SIG_ELEM_IS_COPY) {
                    bindee = Rakudo_binding_create_positional(interp,
                        Parrot_pmc_new(interp, enum_class_ResizablePMCArray));
                    Rakudo_cont_store(interp, bindee, decont_value, 0, 0);
                }
                VTABLE_set_pmc_keyed_str(interp, lexpad, param->variable_name, bindee);
            }
            
            /* If it's a hash, similar approach to array. */
            else if (param->flags & SIG_ELEM_HASH_SIGIL) {
                PMC *bindee = decont_value;
                if (param->flags & SIG_ELEM_IS_COPY) {
                    bindee = Rakudo_binding_create_hash(interp,
                        Parrot_pmc_new(interp, enum_class_Hash));
                    Rakudo_cont_store(interp, bindee, decont_value, 0, 0);
                }
                VTABLE_set_pmc_keyed_str(interp, lexpad, param->variable_name, bindee);
            }
            
            /* If it's a scalar, we always need to wrap it into a new
             * container and store it, for copy or ro case (the rw bit
             * in the container descriptor takes care of the rest). */
            else {
                PMC *new_cont = Rakudo_cont_scalar_from_descriptor(interp, param->container_descriptor);
                Rakudo_cont_store(interp, new_cont, decont_value, 0, 0);
                VTABLE_set_pmc_keyed_str(interp, lexpad, param->variable_name, new_cont);
            }
        }
    }

    /* Is it the invocant? If so, also have to bind to self lexical. */
    if (param->flags & SIG_ELEM_INVOCANT)
        VTABLE_set_pmc_keyed_str(interp, lexpad, SELF_str, decont_value);

    /* Handle any constraint types (note that they may refer to the parameter by
     * name, so we need to have bound it already). */
    if (!PMC_IS_NULL(param->post_constraints)) {
        PMC * code_type         = Rakudo_types_code_get();
        PMC * const constraints = param->post_constraints;
        INTVAL num_constraints  = VTABLE_elements(interp, constraints);
        INTVAL i;
        for (i = 0; i < num_constraints; i++) {
            /* Check we meet the constraint. */
            PMC *cons_type    = VTABLE_get_pmc_keyed_int(interp, constraints, i);
            PMC *accepts_meth = VTABLE_find_method(interp, cons_type, ACCEPTS);
            PMC *old_ctx      = Parrot_pcc_get_signature(interp, CURRENT_CONTEXT(interp));
            PMC *cappy        = Parrot_pmc_new(interp, enum_class_CallContext);
            if (STABLE(cons_type)->type_check(interp, cons_type, code_type))
                Parrot_sub_capture_lex(interp,
                    VTABLE_get_attr_keyed(interp, cons_type, code_type, DO_str));
            VTABLE_push_pmc(interp, cappy, cons_type);
            switch (bv.type) {
                case BIND_VAL_OBJ:
                    VTABLE_push_pmc(interp, cappy, bv.val.o);
                    break;
                case BIND_VAL_INT:
                    VTABLE_push_integer(interp, cappy, bv.val.i);
                    break;
                case BIND_VAL_NUM:
                    VTABLE_push_float(interp, cappy, bv.val.n);
                    break;
                case BIND_VAL_STR:
                    VTABLE_push_string(interp, cappy, bv.val.s);
                    break;
            }
            Parrot_pcc_invoke_from_sig_object(interp, accepts_meth, cappy);
            cappy = Parrot_pcc_get_signature(interp, CURRENT_CONTEXT(interp));
            Parrot_pcc_set_signature(interp, CURRENT_CONTEXT(interp), old_ctx);
            if (!VTABLE_get_bool(interp, VTABLE_get_pmc_keyed_int(interp, cappy, 0))) {
                if (error)
                    *error = Parrot_sprintf_c(interp, "Constraint type check failed for parameter '%S'",
                            param->variable_name);
                return BIND_RESULT_FAIL;
            }
        }
    }

    /* If it's attributive, now we assign it. */
    if (param->flags & SIG_ELEM_BIND_ATTRIBUTIVE) {
        INTVAL result = Rakudo_binding_assign_attributive(interp, lexpad, param, bv, decont_value, error);
        if (result != BIND_RESULT_OK)
            return result;
    }

    /* If it has a sub-signature, bind that. */
    if (!PMC_IS_NULL(param->sub_llsig) && bv.type == BIND_VAL_OBJ) {
        /* Turn value into a capture, unless we already have one. */
        PMC *capture = PMCNULL;
        INTVAL result;
        if (param->flags & SIG_ELEM_IS_CAPTURE) {
            capture = decont_value;
        }
        else {
            PMC *meth    = VTABLE_find_method(interp, decont_value, Parrot_str_new(interp, "Capture", 0));
            if (PMC_IS_NULL(meth)) {
                if (error)
                    *error = Parrot_sprintf_c(interp, "Could not turn argument into capture");
                return BIND_RESULT_FAIL;
            }
            Parrot_ext_call(interp, meth, "Pi->P", decont_value, &capture);
        }

        /* Recurse into signature binder. */
        result = Rakudo_binding_bind(interp, lexpad, param->sub_llsig,
                capture, no_nom_type_check, error);
        if (result != BIND_RESULT_OK)
        {
            if (error) {
                /* Note in the error message that we're in a sub-signature. */
                *error = Parrot_str_concat(interp, *error,
                        Parrot_str_new(interp, " in sub-signature", 0));

                /* Have we a variable name? */
                if (!STRING_IS_NULL(param->variable_name)) {
                    *error = Parrot_str_concat(interp, *error,
                            Parrot_str_new(interp, " of parameter ", 0));
                    *error = Parrot_str_concat(interp, *error, param->variable_name);
                }
            }
            return result;
        }
    }

    /* Binding of this parameter was thus successful - we're done. */
    return BIND_RESULT_OK;
}


/* This takes a signature element and either runs the closure to get a default
 * value if there is one, or creates an appropriate undefined-ish thingy. */
static PMC *
Rakudo_binding_handle_optional(PARROT_INTERP, Rakudo_Parameter *param, PMC *lexpad) {
    PMC *cur_lex;

    /* Is the "get default from outer" flag set? */
    if (param->flags & SIG_ELEM_DEFAULT_FROM_OUTER) {
        PMC *outer_ctx    = Parrot_pcc_get_outer_ctx(interp, CURRENT_CONTEXT(interp));
        PMC *outer_lexpad = Parrot_pcc_get_lex_pad(interp, outer_ctx);
        return VTABLE_get_pmc_keyed_str(interp, outer_lexpad, param->variable_name);
    }

    /* Do we have a default value or value closure? */
    else if (!PMC_IS_NULL(param->default_value)) {
        if (param->flags & SIG_ELEM_DEFAULT_IS_LITERAL) {
            return param->default_value;
        }
        else {
            /* Thunk; run it to get a value. */
            PMC *old_ctx = Parrot_pcc_get_signature(interp, CURRENT_CONTEXT(interp));
            PMC *cappy   = Parrot_pmc_new(interp, enum_class_CallContext);
            Parrot_pcc_invoke_from_sig_object(interp, param->default_value, cappy);
            cappy = Parrot_pcc_get_signature(interp, CURRENT_CONTEXT(interp));
            Parrot_pcc_set_signature(interp, CURRENT_CONTEXT(interp), old_ctx);
            return VTABLE_get_pmc_keyed_int(interp, cappy, 0);
        }
    }

    /* Otherwise, go by sigil to pick the correct default type of value. */
    else {
        if (param->flags & SIG_ELEM_ARRAY_SIGIL) {
            return Rakudo_binding_create_positional(interp, PMCNULL);
        }
        else if (param->flags & SIG_ELEM_HASH_SIGIL) {
            return Rakudo_binding_create_hash(interp, Parrot_pmc_new(interp, enum_class_Hash));
        }
        else {
            return param->nominal_type;
        }
    }
}


/* Extracts bind value information for a positional parameter. */
static Rakudo_BindVal
get_positional_bind_val(PARROT_INTERP, struct Pcc_cell *pc_positionals, PMC *capture, INTVAL cur_pos_arg) {
    Rakudo_BindVal cur_bv;
    if (pc_positionals) {
        switch (pc_positionals[cur_pos_arg].type) {
            case BIND_VAL_INT:
                cur_bv.type = BIND_VAL_INT;
                cur_bv.val.i = pc_positionals[cur_pos_arg].u.i;
                break;
            case BIND_VAL_NUM:
                cur_bv.type = BIND_VAL_NUM;
                cur_bv.val.n = pc_positionals[cur_pos_arg].u.n;
                break;
            case BIND_VAL_STR:
                cur_bv.type = BIND_VAL_STR;
                cur_bv.val.s = pc_positionals[cur_pos_arg].u.s;
                break;
            default:
                cur_bv.type = BIND_VAL_OBJ;
                cur_bv.val.o = pc_positionals[cur_pos_arg].u.p;
        }
    }
    else {
        cur_bv.type = BIND_VAL_OBJ;
        cur_bv.val.o = VTABLE_get_pmc_keyed_int(interp, capture, cur_pos_arg);
    }
    return cur_bv;
}


/* Takes a signature along with positional and named arguments and binds them
 * into the provided lexpad (actually, anything that has a Hash interface will
 * do). Returns BIND_RESULT_OK if binding works out, BIND_RESULT_FAIL if there
 * is a failure and BIND_RESULT_JUNCTION if the failure was because of a
 * Junction being passed (meaning we need to auto-thread). */
INTVAL
Rakudo_binding_bind(PARROT_INTERP, PMC *lexpad, PMC *sig_pmc, PMC *capture,
                    INTVAL no_nom_type_check, STRING **error) {
    INTVAL            i, num_pos_args;
    INTVAL            bind_fail   = 0;
    INTVAL            cur_pos_arg = 0;
    Rakudo_Signature *sig         = (Rakudo_Signature *)PMC_data(sig_pmc);
    PMC              *params      = sig->params;
    INTVAL            num_params  = VTABLE_elements(interp, params);
    Rakudo_BindVal    cur_bv;

    /* If we do have some named args, we want to make a clone of the hash
     * to work on. We'll delete stuff from it as we bind, and what we have
     * left over can become the slurpy hash or - if we aren't meant to be
     * taking one - tell us we have a problem. */
    PMC *named_args_copy = PMCNULL;

    /* If we have a |$foo that's followed by slurpies, then we can suppress
     * any future arity checks. */
    INTVAL suppress_arity_fail = 0;
    
    /* If it's a Parrot capture, it may contain natively typed arguments.
     * NOTE: This is a really an encapsulation breakage; if Parrot folks
     * change stuff and this breaks, it's not Parrot's fault. */
    struct Pcc_cell * pc_positionals = NULL;

    /* Set up statics. */
    if (!smo_id)
        setup_binder_statics(interp);

    /* If we've got a CallContext, just has an attribute with list of named
     * parameter names. Otherwise, it's probably a Perl 6 Capture and we need
     * to extract its parts. */
    if (capture->vtable->base_type == enum_class_CallContext) {
        PMC *named_names = VTABLE_get_attr_str(interp, capture, NAMED_str);
        if (!PMC_IS_NULL(named_names)) {
            PMC *iter = VTABLE_get_iter(interp, named_names);
            named_args_copy = Parrot_pmc_new(interp, enum_class_Hash);
            while (VTABLE_get_bool(interp, iter)) {
                STRING *name = VTABLE_shift_string(interp, iter);
                VTABLE_set_pmc_keyed_str(interp, named_args_copy, name,
                        VTABLE_get_pmc_keyed_str(interp, capture, name));
            }
        }
        GETATTR_CallContext_positionals(interp, capture, pc_positionals);
    }
    else if (capture->vtable->base_type == smo_id &&
            STABLE(capture)->type_check(interp, capture, Rakudo_types_capture_get())) {
        PMC *captype   = Rakudo_types_capture_get();
        PMC *list_part = VTABLE_get_attr_keyed(interp, capture, captype, LIST_str);
        PMC *hash_part = VTABLE_get_attr_keyed(interp, capture, captype, HASH_str);
        capture = Rakudo_isnqplist(list_part) 
                    ?  list_part 
                    : Parrot_pmc_new(interp, enum_class_ResizablePMCArray);
        if (hash_part->vtable->base_type == enum_class_Hash) {
            PMC *iter = VTABLE_get_iter(interp, hash_part);
            named_args_copy = Parrot_pmc_new(interp, enum_class_Hash);
            while (VTABLE_get_bool(interp, iter)) {
                STRING *arg_copy_name = VTABLE_shift_string(interp, iter);
                VTABLE_set_pmc_keyed_str(interp, named_args_copy, arg_copy_name,
                    VTABLE_get_pmc_keyed_str(interp, hash_part, arg_copy_name));
            }
        }
    }
    else {
        Parrot_ex_throw_from_c_args(interp, NULL, EXCEPTION_INVALID_OPERATION,
                "Internal Error: Rakudo_binding_bind passed invalid Capture");
    }

    /* Now we'll walk through the signature and go about binding things. */
    num_pos_args = VTABLE_elements(interp, capture);
    for (i = 0; i < num_params; i++) {
        Rakudo_Parameter *param = (Rakudo_Parameter *)PMC_data(
                VTABLE_get_pmc_keyed_int(interp, params, i));

        /* Is it looking for us to bind a capture here? */
        if (param->flags & SIG_ELEM_IS_CAPTURE) {
            /* Capture the arguments from this point forwards into a Capture.
             * Of course, if there's no variable name we can (cheaply) do pretty
             * much nothing. */
            if (STRING_IS_NULL(param->variable_name)) {
                bind_fail = BIND_RESULT_OK;
            }
            else {
                PMC *captype    = Rakudo_types_capture_get();
                PMC *capsnap    = REPR(captype)->allocate(interp, STABLE(captype));
                PMC *pos_args   = Parrot_pmc_new(interp, enum_class_ResizablePMCArray);
                PMC *named_args = Parrot_pmc_new(interp, enum_class_Hash);
                INTVAL k;
                VTABLE_set_attr_keyed(interp, capsnap, captype, LIST_str, pos_args);
                VTABLE_set_attr_keyed(interp, capsnap, captype, HASH_str, named_args);
                for (k = cur_pos_arg; k < num_pos_args; k++) {
                    cur_bv = get_positional_bind_val(interp, pc_positionals, capture, k);
                    VTABLE_push_pmc(interp, pos_args, cur_bv.type == BIND_VAL_OBJ ?
                        cur_bv.val.o :
                        create_box(interp, cur_bv));
                }
                if (!PMC_IS_NULL(named_args_copy)) {
                    PMC *iter = VTABLE_get_iter(interp, named_args_copy);
                    while (VTABLE_get_bool(interp, iter)) {
                        STRING *name = VTABLE_shift_string(interp, iter);
                        VTABLE_set_pmc_keyed_str(interp, named_args, name,
                            VTABLE_get_pmc_keyed_str(interp, named_args_copy, name));
                    }
                }
                cur_bv.type = BIND_VAL_OBJ;
                cur_bv.val.o = capsnap;
                bind_fail = Rakudo_binding_bind_one_param(interp, lexpad, sig, param, cur_bv,
                        no_nom_type_check, error);
            }
            if (bind_fail) {
                return bind_fail;
            }
            else if (i + 1 == num_params) {
                /* Since a capture acts as "the ultimate slurpy" in a sense, if
                 * this is the last parameter in the signature we can return
                 * success right off the bat. */
                return BIND_RESULT_OK;
            }
            else {
                Rakudo_Parameter *next_param = (Rakudo_Parameter *)PMC_data(
                    VTABLE_get_pmc_keyed_int(interp, params, i + 1));
                if (next_param->flags & (SIG_ELEM_SLURPY_POS | SIG_ELEM_SLURPY_NAMED))
                    suppress_arity_fail = 1;
            }
        }

        /* Could it be a named slurpy? */
        else if (param->flags & SIG_ELEM_SLURPY_NAMED) {
            /* We'll either take the current named arguments copy hash which
             * will by definition contain all unbound named parameters and use
             * that. Otherwise, putting Mu in there is fine; Hash is smart
             * enough to know what to do. */
            PMC *slurpy = PMC_IS_NULL(named_args_copy) ?
                    Rakudo_types_mu_get() :
                    named_args_copy;
            cur_bv.type = BIND_VAL_OBJ;
            cur_bv.val.o = Rakudo_binding_create_hash(interp, slurpy);
            bind_fail = Rakudo_binding_bind_one_param(interp, lexpad, sig, param,
                    cur_bv, no_nom_type_check, error);
            if (bind_fail)
                return bind_fail;
            
            /* Nullify named arguments hash now we've consumed it, to mark all
             * is well. */
            named_args_copy = PMCNULL;
        }

        /* Otherwise, maybe it's a positional. */
        else if (PMC_IS_NULL(param->named_names)) {
            /* Slurpy or LoL-slurpy? */
            if (param->flags & (SIG_ELEM_SLURPY_POS | SIG_ELEM_SLURPY_LOL)) {
                /* Create Perl 6 array, create RPA of all remaining things, then
                 * store it. */
                PMC *temp = Parrot_pmc_new(interp, enum_class_ResizablePMCArray);
                while (cur_pos_arg < num_pos_args) {
                    cur_bv = get_positional_bind_val(interp, pc_positionals, capture, cur_pos_arg);
                    VTABLE_push_pmc(interp, temp, cur_bv.type == BIND_VAL_OBJ ?
                        cur_bv.val.o :
                        create_box(interp, cur_bv));
                    cur_pos_arg++;
                }
                cur_bv.type = BIND_VAL_OBJ;
                cur_bv.val.o = param->flags & SIG_ELEM_SLURPY_POS ?
                    (param->flags & SIG_ELEM_IS_RW ?
                        Rakudo_binding_create_list(interp, temp) :
                        Rakudo_binding_create_positional(interp, temp)) :
                    Rakudo_binding_create_lol(interp, temp);
                bind_fail = Rakudo_binding_bind_one_param(interp, lexpad, sig, param,
                        cur_bv, no_nom_type_check, error);
                if (bind_fail)
                    return bind_fail;
            }

            /* Otherwise, a positional. */
            else {
                /* Do we have a value?. */
                if (cur_pos_arg < num_pos_args) {
                    /* Easy - just bind that. */
                    cur_bv = get_positional_bind_val(interp, pc_positionals, capture, cur_pos_arg);
                    bind_fail = Rakudo_binding_bind_one_param(interp, lexpad, sig, param,
                            cur_bv, no_nom_type_check, error);
                    if (bind_fail)
                        return bind_fail;
                    cur_pos_arg++;
                }
                else {
                    /* No value. If it's optional, fetch a default and bind that;
                     * if not, we're screwed. Note that we never nominal type check
                     * an optional with no value passed. */
                    if (param->flags & SIG_ELEM_IS_OPTIONAL) {
                        cur_bv.type = BIND_VAL_OBJ;
                        cur_bv.val.o = Rakudo_binding_handle_optional(interp, param, lexpad);
                        bind_fail = Rakudo_binding_bind_one_param(interp, lexpad, sig, param,
                                cur_bv, 0, error);
                        if (bind_fail)
                            return bind_fail;
                    }
                    else {
                        if (error)
                            *error = Rakudo_binding_arity_fail(interp, params, num_params, num_pos_args, 0);
                        return BIND_RESULT_FAIL;
                    }
                }
            }
        }

        /* Else, it's a non-slurpy named. */
        else {
            /* Try and get hold of value. */
            PMC *value = PMCNULL;
            INTVAL num_names = VTABLE_elements(interp, param->named_names);
            INTVAL j;
            if (!PMC_IS_NULL(named_args_copy)) {
                for (j = 0; j < num_names; j++) {
                    STRING *name = VTABLE_get_string_keyed_int(interp, param->named_names, j);
                    value = VTABLE_get_pmc_keyed_str(interp, named_args_copy, name);
                    if (!PMC_IS_NULL(value)) {
                        /* Found a value. Delete entry from to-bind args and stop looking. */
                        VTABLE_delete_keyed_str(interp, named_args_copy, name);
                        break;
                    }
                }
            }

            /* Did we get one? */
            if (PMC_IS_NULL(value)) {
                /* Nope. We'd better hope this param was optional... */
                if (param->flags & SIG_ELEM_IS_OPTIONAL) {
                    cur_bv.type = BIND_VAL_OBJ;
                    cur_bv.val.o = Rakudo_binding_handle_optional(interp, param, lexpad);
                    bind_fail = Rakudo_binding_bind_one_param(interp, lexpad, sig, param,
                            cur_bv, 0, error);
                }
                else if (!suppress_arity_fail) {
                    if (error)
                        *error = Parrot_sprintf_c(interp, "Required named parameter '%S' not passed",
                                VTABLE_get_string_keyed_int(interp, param->named_names, 0));
                    return BIND_RESULT_FAIL;
                }
            }
            else {
                cur_bv.type = BIND_VAL_OBJ;
                cur_bv.val.o = value;
                bind_fail = Rakudo_binding_bind_one_param(interp, lexpad, sig, param,
                        cur_bv, 0, error);
            }

            /* If we got a binding failure, return it. */
            if (bind_fail)
                return bind_fail;
        }
    }

    /* Do we have any left-over args? */
    if (cur_pos_arg < num_pos_args && !suppress_arity_fail) {
        /* Oh noes, too many positionals passed. */
        if (error)
            *error = Rakudo_binding_arity_fail(interp, params, num_params, num_pos_args, 1);
        return BIND_RESULT_FAIL;
    }
    if (!PMC_IS_NULL(named_args_copy) && VTABLE_elements(interp, named_args_copy)) {
        /* Oh noes, unexpected named args. */
        if (error) {
            INTVAL num_extra = VTABLE_elements(interp, named_args_copy);
            PMC *iter        = VTABLE_get_iter(interp, named_args_copy);
            if (num_extra == 1) {
                *error = Parrot_sprintf_c(interp, "Unexpected named parameter '%S' passed",
                        VTABLE_shift_string(interp, iter));
            }
            else {
                INTVAL first  = 1;
                STRING *comma = Parrot_str_new(interp, ", ", 0);
                *error = Parrot_sprintf_c(interp, "%d unexpected named parameters passed (", num_extra);
                while (VTABLE_get_bool(interp, iter)) {
                    STRING *name = VTABLE_shift_string(interp, iter);
                    if (!first)
                        *error = Parrot_str_concat(interp, *error, comma);
                    else
                        first = 0;
                    *error = Parrot_str_concat(interp, *error, name);
                }
                *error = Parrot_str_concat(interp, *error, Parrot_str_new(interp, ")", 0));
            }
        }
        return BIND_RESULT_FAIL;
    }

    /* If we get here, we're done. */
    return BIND_RESULT_OK;
}

/* Compile time trial binding; tries to determine at compile time whether
 * certain binds will/won't work. */
INTVAL Rakudo_binding_trial_bind(PARROT_INTERP, PMC *sig_pmc, PMC *capture) {
    INTVAL            i, num_pos_args, got_prim;
    INTVAL            cur_pos_arg = 0;
    Rakudo_Signature *sig         = (Rakudo_Signature *)PMC_data(sig_pmc);
    PMC              *params      = sig->params;
    INTVAL            num_params  = VTABLE_elements(interp, params);

    /* Grab arguments. */
    struct Pcc_cell * pc_positionals = NULL;
    if (capture->vtable->base_type == enum_class_CallContext)
        GETATTR_CallContext_positionals(interp, capture, pc_positionals);
    else
        return TRIAL_BIND_NOT_SURE;

    /* Set up statics. */
    if (!smo_id)
        setup_binder_statics(interp);
        
    /* If there's a single capture parameter, then we're OK. (Worth
     * handling especially as it's the common case for protos). */
    if (num_params == 1) {
        Rakudo_Parameter *param = (Rakudo_Parameter *)PMC_data(
                VTABLE_get_pmc_keyed_int(interp, params, 0));
        if (param->flags & SIG_ELEM_IS_CAPTURE)
            return TRIAL_BIND_OK;
    }
        
    /* Walk through the signature and consider the parameters. */
    num_pos_args = VTABLE_elements(interp, capture);
    for (i = 0; i < num_params; i++) {
        Rakudo_Parameter *param = (Rakudo_Parameter *)PMC_data(
                VTABLE_get_pmc_keyed_int(interp, params, i));
        
        /* If the parameter is anything other than a boring old
         * positional parameter, we won't analyze it. */
        if (param->flags & ~(
                SIG_ELEM_MULTI_INVOCANT | SIG_ELEM_IS_PARCEL |
                SIG_ELEM_IS_COPY | SIG_ELEM_ARRAY_SIGIL |
                SIG_ELEM_HASH_SIGIL | SIG_ELEM_NATIVE_VALUE |
                SIG_ELEM_IS_OPTIONAL))
            return TRIAL_BIND_NOT_SURE;
        if (!PMC_IS_NULL(param->named_names))
            return TRIAL_BIND_NOT_SURE;
        if (!PMC_IS_NULL(param->post_constraints))
            return TRIAL_BIND_NOT_SURE;
        if (!PMC_IS_NULL(param->type_captures))
            return TRIAL_BIND_NOT_SURE;

        /* Do we have an argument for this parameter? */
        if (cur_pos_arg >= num_pos_args) {
            /* No; if it's not optional, fail.*/
            if (!(param->flags & SIG_ELEM_IS_OPTIONAL))
                return TRIAL_BIND_NO_WAY;
        }
        else {
            /* Yes, need to consider type. */
            got_prim = pc_positionals[cur_pos_arg].type;
            if (param->flags & SIG_ELEM_NATIVE_VALUE) {
                if (got_prim == BIND_VAL_OBJ) {
                    /* We got an object; if we aren't sure we can unbox, we can't
                    * be sure about the dispatch. */
                    PMC *arg = pc_positionals[cur_pos_arg].u.p;
                    storage_spec spec = REPR(arg)->get_storage_spec(interp, STABLE(arg));
                    switch (param->flags & SIG_ELEM_NATIVE_VALUE) {
                        case SIG_ELEM_NATIVE_INT_VALUE:
                            if (!(spec.can_box & STORAGE_SPEC_CAN_BOX_INT))
                                return TRIAL_BIND_NOT_SURE;
                            break;
                        case SIG_ELEM_NATIVE_NUM_VALUE:
                            if (!(spec.can_box & STORAGE_SPEC_CAN_BOX_NUM))
                                return TRIAL_BIND_NOT_SURE;
                            break;
                        case SIG_ELEM_NATIVE_STR_VALUE:
                            if (!(spec.can_box & STORAGE_SPEC_CAN_BOX_STR))
                                return TRIAL_BIND_NOT_SURE;
                            break;
                        default:
                            /* WTF... */
                            return TRIAL_BIND_NOT_SURE;
                    }
                }
                else {
                    /* If it's the wrong type of native, there's no way it
                    * can ever bind. */
                    if (((param->flags & SIG_ELEM_NATIVE_INT_VALUE) && got_prim != BIND_VAL_INT) ||
                        ((param->flags & SIG_ELEM_NATIVE_NUM_VALUE) && got_prim != BIND_VAL_NUM) ||
                        ((param->flags & SIG_ELEM_NATIVE_STR_VALUE) && got_prim != BIND_VAL_STR))
                        return TRIAL_BIND_NO_WAY;
                }
            }
            else {
                /* Work out a parameter type to consider, and see if it matches. */
                PMC * const arg =
                    got_prim == BIND_VAL_OBJ ? pc_positionals[cur_pos_arg].u.p :
                    got_prim == BIND_VAL_INT ? Rakudo_types_int_get() :
                    got_prim == BIND_VAL_NUM ? Rakudo_types_num_get() :
                                            Rakudo_types_str_get();
                if (param->nominal_type != Rakudo_types_mu_get() &&
                        !STABLE(arg)->type_check(interp, arg, param->nominal_type)) {
                    /* If it failed because we got a junction, may auto-thread;
                    * hand back "not sure" for now. */
                    if (STABLE(arg)->WHAT == Rakudo_types_junction_get())
                        return TRIAL_BIND_NOT_SURE;
                    
                    /* It failed to, but that doesn't mean it can't work at runtime;
                    * we perhaps want an Int, and the most we know is we have an Any,
                    * which would include Int. However, the Int ~~ Str case can be
                    * rejected now, as there's no way it'd ever match. Basically, we
                    * just flip the type check around. */
                    return STABLE(param->nominal_type)->type_check(interp, param->nominal_type, arg) ?
                        TRIAL_BIND_NOT_SURE : TRIAL_BIND_NO_WAY;
                }
            }
        }

        /* Continue to next argument. */
        cur_pos_arg++;
    }

    /* If we have any left over arguments, it's a binding fail. */
    if (cur_pos_arg < num_pos_args)
        return TRIAL_BIND_NO_WAY;

    /* Otherwise, if we get there, all is well. */
    return TRIAL_BIND_OK;
}

/*
 * Local variables:
 *   c-file-style: "parrot"
 * End:
 * vim: expandtab shiftwidth=4:
 */
