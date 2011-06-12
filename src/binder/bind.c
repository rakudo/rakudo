/*
$Id$
Copyright (C) 2009-2011, The Perl Foundation.
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
static STRING *ACCEPTS          = NULL;
static STRING *HOW              = NULL;
static STRING *DO_str           = NULL;
static STRING *SELF_str         = NULL;
static STRING *NAME_str         = NULL;
static STRING *BLOCK_str        = NULL;
static STRING *SCALAR_str       = NULL;
static STRING *CAPTURE_str      = NULL;
static STRING *SNAPCAP_str      = NULL;
static STRING *STORAGE_str      = NULL;
static STRING *REST_str         = NULL;
static STRING *P6_SCALAR_str    = NULL;
static STRING *SHORTNAME_str    = NULL;
static STRING *HASH_SIGIL_str   = NULL;
static STRING *ARRAY_SIGIL_str  = NULL;
static STRING *BANG_TWIGIL_str  = NULL;
static STRING *SCALAR_SIGIL_str = NULL;
static STRING *NAMED_str        = NULL;

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
    SCALAR_str       = Parrot_str_new_constant(interp, "scalar");
    CAPTURE_str      = Parrot_str_new_constant(interp, "Capture");
    SNAPCAP_str      = Parrot_str_new_constant(interp, "!snapshot_capture");
    STORAGE_str      = Parrot_str_new_constant(interp, "$!storage");
    REST_str         = Parrot_str_new_constant(interp, "$!rest");
    P6_SCALAR_str    = Parrot_str_new_constant(interp, "Perl6Scalar");
    SHORTNAME_str    = Parrot_str_new_constant(interp, "shortname");
    HASH_SIGIL_str   = Parrot_str_new_constant(interp, "%");
    ARRAY_SIGIL_str  = Parrot_str_new_constant(interp, "@");
    BANG_TWIGIL_str  = Parrot_str_new_constant(interp, "!");
    SCALAR_SIGIL_str = Parrot_str_new_constant(interp, "$");
    NAMED_str        = Parrot_str_new_constant(interp, "named");

    smo_id = pmc_type(interp, Parrot_str_new(interp, "SixModelObject", 0));
}

/* Creates a Perl 6 Array. */
static PMC *
Rakudo_binding_create_positional(PARROT_INTERP, PMC *rest) {
    PMC *type  = Rakudo_types_array_get();
    PMC *array = REPR(type)->instance_of(interp, type);
    VTABLE_set_attr_keyed(interp, array, Rakudo_types_list_get(), REST_str, rest);
    return array;
}


/* Creates a Perl 6 Hash. */
static PMC *
Rakudo_binding_create_hash(PARROT_INTERP, PMC *storage) {
    PMC *type = Rakudo_types_hash_get();
    PMC *hash = REPR(type)->instance_of(interp, type);
    VTABLE_set_attr_keyed(interp, hash, Rakudo_types_enummap_get(), STORAGE_str, storage);
    return hash;
}


/* Creates a Perl 6 object of the type given by C<classname> */
static PMC *
Rakudo_binding_create(PARROT_INTERP, STRING *classname) {
    PMC *ns        = Parrot_hll_get_ctx_HLL_namespace(interp);
    PMC *class_ns  = Parrot_ns_get_namespace_keyed_str(interp, ns, classname);
    PMC *class_obj = VTABLE_get_class(interp, class_ns);
    PMC *result    = VTABLE_instantiate(interp, class_obj, PMCNULL);
    return result;
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
Rakudo_binding_bind_type_captures(PARROT_INTERP, PMC *lexpad, Rakudo_Parameter *param, PMC *value) {
    PMC * type_obj = STABLE(value)->WHAT;
    PMC * iter     = VTABLE_get_iter(interp, param->type_captures);
    while (VTABLE_get_bool(interp, iter)) {
        STRING *name = VTABLE_shift_string(interp, iter);
        VTABLE_set_pmc_keyed_str(interp, lexpad, name, type_obj);
    }
}


/* Assigns an attributive parameter to the desired attribute. */
static INTVAL
Rakudo_binding_assign_attributive(PARROT_INTERP, PMC *lexpad, Rakudo_Parameter *param,
                                  PMC *value, STRING **error) {
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

    /* If it's private, just need to fetch the attribute. */
    if (param->flags & SIG_ELEM_BIND_PRIVATE_ATTR) {
        assignee = VTABLE_get_attr_str(interp, self, param->variable_name);
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

    /* Now look up infix:<=> and do the assignment. */
    assigner = VTABLE_get_pmc_keyed_str(interp, Parrot_hll_get_ctx_HLL_namespace(interp),
            Parrot_str_new(interp, "!only_infix:=", 0));
    Parrot_ext_call(interp, assigner, "PP", assignee, value);

    return BIND_RESULT_OK;
}


/* Binds a single argument into the lexpad, after doing any checks that are
 * needed. Also handles any type captures. If there is a sub signature, then
 * re-enters the binder. Returns one of the BIND_RESULT_* codes. */
static INTVAL
Rakudo_binding_bind_one_param(PARROT_INTERP, PMC *lexpad, Rakudo_Signature *signature, Rakudo_Parameter *param,
                              PMC *value, INTVAL no_nom_type_check, STRING **error) {
    /* We pretty much always need to de-containerized value, so get it
     * right off. */
    PMC *decont_value = Rakudo_cont_decontainerize(interp, value);
    
    /* Skip nominal type check if not needed. */
    if (!no_nom_type_check) {
        /* If not, do the check. If the wanted nominal type is Mu, then
		 * anything goes. */
        if (param->nominal_type != Rakudo_types_mu_get() &&
                (decont_value->vtable->base_type != smo_id ||
                 !STABLE(decont_value)->type_check(interp, decont_value, param->nominal_type))) {
            /* Type check failed; produce error if needed. */
            if (error) {
                PMC    * got_how       = STABLE(decont_value)->HOW;
                PMC    * exp_how       = STABLE(param->nominal_type)->HOW;
                PMC    * got_name_meth = VTABLE_find_method(interp, got_how, NAME_str);
                PMC    * exp_name_meth = VTABLE_find_method(interp, exp_how, NAME_str);
                STRING * expected, * got;
                Parrot_ext_call(interp, got_name_meth, "PiP->S", got_how, value, &got);
                Parrot_ext_call(interp, exp_name_meth, "PiP->S", exp_how, param->nominal_type, &expected);
                *error = Parrot_sprintf_c(interp, "Nominal type check failed for parameter '%S'; expected %S but got %S instead",
                            param->variable_name, expected, got);
            }
            
            /* Report junction failure mode if it's a junction. */
            if (decont_value->vtable->base_type == smo_id &&
                    STABLE(decont_value)->WHAT == Rakudo_types_junction_get())
                return BIND_RESULT_JUNCTION;
            else
                return BIND_RESULT_FAIL;
        }
        
        /* Also enforce definedness constraints. */
        if (param->flags & SIG_ELEM_DEFINEDNES_CHECK) {
            INTVAL defined = REPR(decont_value)->defined(interp, decont_value);
            if (defined && param->flags & SIG_ELEM_UNDEFINED_ONLY) {
                if (error)
                    *error = Parrot_sprintf_c(interp,
                        "Parameter '%S' requires a type object, but an object instance was passed",
                        param->variable_name);
                return BIND_RESULT_FAIL;
            }
            if (!defined && param->flags & SIG_ELEM_DEFINED_ONLY) {
                if (error)
                    *error = Parrot_sprintf_c(interp,
                        "Parameter '%S' requires an instance, but a type object was passed",
                        param->variable_name);
                return BIND_RESULT_FAIL;
            }
        }
    }

    /* Do we have any type captures to bind? */
    if (!PMC_IS_NULL(param->type_captures))
        Rakudo_binding_bind_type_captures(interp, lexpad, param, decont_value);

    /* Do a coercion, if one is needed. */
    if (!STRING_IS_NULL(param->coerce_to)) {
        PMC *coerce_meth = VTABLE_find_method(interp, decont_value, param->coerce_to);
        if (!PMC_IS_NULL(coerce_meth)) {
            Parrot_ext_call(interp, coerce_meth, "Pi->P", decont_value, &decont_value);
        }
        else {
            /* No coercion method availale; whine and fail to bind. */
            if (error) {
                PMC    * how_meth   = VTABLE_find_method(interp, decont_value, HOW);
                PMC    * value_how, * value_type;
                STRING * got;
                Parrot_ext_call(interp, how_meth, "Pi->P", decont_value, &value_how);
                value_type = VTABLE_get_attr_str(interp, value_how, SHORTNAME_str);
                got        = VTABLE_get_string(interp, value_type);
                *error = Parrot_sprintf_c(interp,
                        "Unable to coerce value for '%S' from %S to %S; no coercion method defined",
                        param->variable_name, got, param->coerce_to);
            }
            return BIND_RESULT_FAIL;
        }
    }

    /* If it's not got attributive binding, we'll go about binding it into the
     * lex pad. */
    if (!(param->flags & SIG_ELEM_BIND_ATTRIBUTIVE) && !STRING_IS_NULL(param->variable_name)) {
        /* Is it "is rw"? */
        if (param->flags & SIG_ELEM_IS_RW) {
            /* XXX TODO Check if rw flag is set; also need to have a
             * wrapper container that carries extra constraints. */
            VTABLE_set_pmc_keyed_str(interp, lexpad, param->variable_name, value);
        }
        else if (param->flags & SIG_ELEM_IS_PARCEL) {
            /* Just bind the thing as is into the lexpad. */
            VTABLE_set_pmc_keyed_str(interp, lexpad, param->variable_name, value);
        }
        else {
            /* If it's an array, copy means make a new one and store,
             * and a normal bind is a straightforward binding plus
             * adding a constraint. */
            if (param->flags & SIG_ELEM_ARRAY_SIGIL) {
                /* XXX TODO. */
                VTABLE_set_pmc_keyed_str(interp, lexpad, param->variable_name, decont_value);
            }
            
            /* If it's a hash, similar approach to array. */
            else if (param->flags & SIG_ELEM_HASH_SIGIL) {
                /* XXX TODO. */
                VTABLE_set_pmc_keyed_str(interp, lexpad, param->variable_name, decont_value);
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
        PMC * const constraints = param->post_constraints;
        INTVAL num_constraints  = VTABLE_elements(interp, constraints);
        PMC * result            = PMCNULL;
        INTVAL i;
        for (i = 0; i < num_constraints; i++) {
            /* XXX fix this crap... */
            /*PMC *cons_type    = VTABLE_get_pmc_keyed_int(interp, constraints, i);
            PMC *accepts_meth = VTABLE_find_method(interp, cons_type, ACCEPTS);
            if (VTABLE_isa(interp, cons_type, BLOCK_str))
                Parrot_sub_capture_lex(interp,
                    VTABLE_get_attr_str(interp, cons_type, DO_str));
            Parrot_ext_call(interp, accepts_meth, "PiP->P", cons_type, value, &result);
            if (!VTABLE_get_bool(interp, result)) {
                if (error)
                    *error = Parrot_sprintf_c(interp, "Constraint type check failed for parameter '%S'",
                            param->variable_name);
                return BIND_RESULT_FAIL;
            }*/
        }
    }

    /* If it's attributive, now we assign it. */
    if (param->flags & SIG_ELEM_BIND_ATTRIBUTIVE) {
        INTVAL result = Rakudo_binding_assign_attributive(interp, lexpad, param, decont_value, error);
        if (result != BIND_RESULT_OK)
            return result;
    }

    /* If it has a sub-signature, bind that. */
    if (!PMC_IS_NULL(param->sub_llsig)) {
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

    /* Do we have a default value closure? */
    else if (!PMC_IS_NULL(param->default_closure)) {
        /* Run it to get a value. */
        PMC *result = PMCNULL;
        Parrot_sub_capture_lex(interp, param->default_closure);
        /* XXX stop using inefficient Parrot_ext_call... :-/ */
        Parrot_ext_call(interp, param->default_closure, "->P", &result);
        return result;
    }

    /* Otherwise, go by sigil to pick the correct default type of value. */
    else {
        if (param->flags & SIG_ELEM_ARRAY_SIGIL) {
            return Rakudo_binding_create_positional(interp, PMCNULL);
        }
        else if (param->flags & SIG_ELEM_HASH_SIGIL) {
            return Rakudo_binding_create_hash(interp, pmc_new(interp, enum_class_Hash));
        }
        else {
            return param->nominal_type;
        }
    }
}


/* Takes a signature along with positional and named arguments and binds them
 * into the provided lexpad (actually, anything that has a Hash interface will
 * do). Returns BIND_RESULT_OK if binding works out, BIND_RESULT_FAIL if there
 * is a failure and BIND_RESULT_JUNCTION if the failure was because of a
 * Junction being passed (meaning we need to auto-thread). */
INTVAL
Rakudo_binding_bind(PARROT_INTERP, PMC *lexpad, PMC *sig_pmc, PMC *capture,
                    INTVAL no_nom_type_check, STRING **error) {
    INTVAL            i;
    INTVAL            bind_fail;
    INTVAL            cur_pos_arg        = 0;
    INTVAL            num_pos_args       = VTABLE_elements(interp, capture);
    PMC              *named_names        = PMCNULL;
    Rakudo_Signature *sig                = (Rakudo_Signature *)PMC_data(sig_pmc);
    PMC              *params             = sig->params;
    INTVAL            num_params         = VTABLE_elements(interp, params);

    /* If we do have some named args, we want to make a clone of the hash
     * to work on. We'll delete stuff from it as we bind, and what we have
     * left over can become the slurpy hash or - if we aren't meant to be
     * taking one - tell us we have a problem. */
    PMC *named_args_copy = PMCNULL;

    /* If we have a |$foo that's followed by slurpies, then we can suppress
     * any future arity checks. */
    INTVAL suppress_arity_fail = 0;

    /* Set up statics. */
    if (!smo_id)
        setup_binder_statics(interp);

    /* If we've got a CallContext, just has an attribute with list of named
     * parameter names. Otherwise, it's a Capture and we need to do .hash and
     * grab out the keys. */
    if (capture->vtable->base_type == enum_class_CallContext) {
        named_names = VTABLE_get_attr_str(interp, capture, NAMED_str);
    }
    else if (VTABLE_isa(interp, capture, CAPTURE_str)) {
        PMC *meth = VTABLE_find_method(interp, capture, Parrot_str_new(interp, "!PARROT_NAMEDS", 0));
        PMC *hash = PMCNULL;
        PMC *iter;
        Parrot_ext_call(interp, meth, "Pi->P", capture, &hash);
        iter = VTABLE_get_iter(interp, hash);
        if (VTABLE_get_bool(interp, iter)) {
            named_names = pmc_new(interp, enum_class_ResizableStringArray);
            while (VTABLE_get_bool(interp, iter))
                VTABLE_push_string(interp, named_names, VTABLE_shift_string(interp, iter));
        }
    }
    else {
        Parrot_ex_throw_from_c_args(interp, NULL, EXCEPTION_INVALID_OPERATION,
                "Internal Error: Rakudo_binding_bind passed invalid Capture");
    }

    /* Make copy of the named arguments. */
    if (!PMC_IS_NULL(named_names)) {
        PMC *iter = VTABLE_get_iter(interp, named_names);
        named_args_copy = pmc_new(interp, enum_class_Hash);
        while (VTABLE_get_bool(interp, iter)) {
            STRING *name = VTABLE_shift_string(interp, iter);
            VTABLE_set_pmc_keyed_str(interp, named_args_copy, name,
                    VTABLE_get_pmc_keyed_str(interp, capture, name));
        }
    }

    /* Now we'll walk through the signature and go about binding things. */
    for (i = 0; i < num_params; i++) {
        Rakudo_Parameter *param = (Rakudo_Parameter *)PMC_data(
                VTABLE_get_pmc_keyed_int(interp, params, i));

        /* Is it looking for us to bind a capture here? */
        if (param->flags & SIG_ELEM_IS_CAPTURE) {
            /* XXX In the long run, we need to snapshot any current CaptureCursor.
             * For now, we don't have that, so we just build off the current
             * capture. Of course, if there's no variable name we can (cheaply)
             * do pretty much nothing. */
            if (STRING_IS_NULL(param->variable_name)) {
                bind_fail = BIND_RESULT_OK;
            }
            else {
                PMC *ns       = Parrot_hll_get_ctx_HLL_namespace(interp);
                PMC *snapper  = Parrot_ns_get_global(interp, ns, SNAPCAP_str);
                PMC *snapshot = PMCNULL;
                Parrot_ext_call(interp, snapper, "PiIP->P", capture, cur_pos_arg, named_args_copy, &snapshot);
                bind_fail = Rakudo_binding_bind_one_param(interp, lexpad, sig, param, snapshot,
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
             * that, or just create an empty one. */
            PMC *slurpy = PMC_IS_NULL(named_args_copy) ?
                    pmc_new(interp, enum_class_Hash) :
                    named_args_copy;
            bind_fail = Rakudo_binding_bind_one_param(interp, lexpad, sig, param,
                    Rakudo_binding_create_hash(interp, slurpy), no_nom_type_check, error);
            if (bind_fail)
                return bind_fail;

            /* Nullify named arguments hash now we've consumed it, to mark all
             * is well. */
            named_args_copy = PMCNULL;
        }

        /* Otherwise, maybe it's a positional. */
        else if (PMC_IS_NULL(param->named_names)) {
            /* Slurpy? */
            if (param->flags & SIG_ELEM_SLURPY_POS) {
                /* Create Perl 6 array, create RPA of all remaining things, then
                 * store it. */
                PMC *temp = pmc_new(interp, enum_class_ResizablePMCArray);
                while (cur_pos_arg < num_pos_args) {
                    VTABLE_push_pmc(interp, temp, VTABLE_get_pmc_keyed_int(interp, capture, cur_pos_arg));
                    cur_pos_arg++;
                }
                bind_fail = Rakudo_binding_bind_one_param(interp, lexpad, sig, param,
                        Rakudo_binding_create_positional(interp, temp), no_nom_type_check, error);
                if (bind_fail)
                    return bind_fail;
            }

            /* Otherwise, a positional. */
            else {
                /* Do we have a value?. */
                if (cur_pos_arg < num_pos_args) {
                    /* Easy - just bind that. */
                    PMC *arg = VTABLE_get_pmc_keyed_int(interp, capture, cur_pos_arg);
                    bind_fail = Rakudo_binding_bind_one_param(interp, lexpad, sig, param,
                            arg, no_nom_type_check, error);
                    if (bind_fail)
                        return bind_fail;
                    cur_pos_arg++;
                }
                else {
                    /* No value. If it's optional, fetch a default and bind that;
                     * if not, we're screwed. Note that we never nominal type check
                     * an optional with no value passed. */
                    if (param->flags & SIG_ELEM_IS_OPTIONAL) {
                        PMC *value = Rakudo_binding_handle_optional(interp, param, lexpad);
                        bind_fail = Rakudo_binding_bind_one_param(interp, lexpad, sig, param,
                                value, 1, error);
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
                    value = Rakudo_binding_handle_optional(interp, param, lexpad);
                    bind_fail = Rakudo_binding_bind_one_param(interp, lexpad, sig, param,
                            value, 1, error);
                }
                else if (!suppress_arity_fail) {
                    if (error)
                        *error = Parrot_sprintf_c(interp, "Required named parameter '%S' not passed",
                                VTABLE_get_string_keyed_int(interp, param->named_names, 0));
                    return BIND_RESULT_FAIL;
                }
            }
            else {
                bind_fail = Rakudo_binding_bind_one_param(interp, lexpad, sig, param,
                        value, 0, error);
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

/*
 * Local variables:
 *   c-file-style: "parrot"
 * End:
 * vim: expandtab shiftwidth=4:
 */
