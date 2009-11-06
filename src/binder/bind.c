/*
$Id$
Copyright (C) 2009, The Perl Foundation.
*/

#define PARROT_IN_EXTENSION
#include "parrot/parrot.h"
#include "parrot/extend.h"
#include "bind.h"
#include "../pmc/pmc_p6lowlevelsig.h"


/* Cache of the type ID for low level signatures. */
static INTVAL lls_id = 0;

/* Names of types we create. */
#define PERL6_ARRAY "Perl6Array"
#define PERL6_HASH  "Hash"

/* Unwraps things inside a scalar reference. */
static PMC *
descalarref(PARROT_INTERP, PMC *ref) {
    INTVAL p6s_id = pmc_type(interp, string_from_literal(interp, "Perl6Scalar"));
    INTVAL or_id  = pmc_type(interp, string_from_literal(interp, "ObjectRef"));
    while (ref->vtable->base_type == or_id || ref->vtable->base_type == p6s_id)
        ref = VTABLE_get_pmc(interp, ref);
    return ref;
}


static STRING *
Rakudo_binding_arity_fail(PARROT_INTERP, llsig_element **elements, INTVAL num_elements,
                          INTVAL num_pos_args, INTVAL too_many) {
    STRING *result;
    INTVAL arity = 0;
    INTVAL count = 0;
    INTVAL i;
    char *whoz_up = too_many ? "Too many" : "Not enough";

    /* Work out how many we could have been passed. */
    for (i = 0; i < num_elements; i++) {
        if (!PMC_IS_NULL(elements[i]->named_names))
            continue;
        if (elements[i]->flags & SIG_ELEM_SLURPY_NAMED)
            continue;
        if (elements[i]->flags & SIG_ELEM_SLURPY_POS) {
            count = -1;
        }
        else if (elements[i]->flags & SIG_ELEM_IS_OPTIONAL) {
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
Rakudo_binding_bind_type_captures(PARROT_INTERP, PMC *lexpad, llsig_element *sig_info, PMC *value) {
    /* Obtain type object. */    
    PMC    * meta_obj = PMCNULL;
    PMC    * type_obj = PMCNULL;
    PMC    * iter;
    STRING * const HOW      = string_from_literal(interp, "HOW");
    PMC    * const how_meth = VTABLE_find_method(interp, value, HOW);
    Parrot_ext_call(interp, how_meth, "Pi->P", value, &meta_obj); 
    type_obj = VTABLE_get_attr_str(interp, meta_obj, string_from_literal(interp, "protoobject"));

    /* Iterate over symbols we need to bind this to, and bind 'em. */
    iter = VTABLE_get_iter(interp, sig_info->type_captures);
    while (VTABLE_get_bool(interp, iter)) {
        STRING *name = VTABLE_shift_string(interp, iter);
        VTABLE_set_pmc_keyed_str(interp, lexpad, name, type_obj);
    }
}


/* Assigns an attributive parameter to the desired attribute. */
static INTVAL
Rakudo_binding_assign_attributive(PARROT_INTERP, PMC *lexpad, llsig_element *sig_info,
                                  PMC *value, STRING **error) {
    PMC *assignee = PMCNULL;
    PMC *assigner;

    /* Find self. */
    PMC *self = VTABLE_get_pmc_keyed_str(interp, lexpad,
            string_from_literal(interp, "self"));
    if (PMC_IS_NULL(self)) {
        if (error)
            *error = Parrot_sprintf_c(interp,
                    "Unable to bind attributive parameter '%S' - could not find self",
                    sig_info->variable_name);
        return BIND_RESULT_FAIL;
    }

    /* If it's private, just need to fetch the attribute. */
    if (sig_info->flags & SIG_ELEM_BIND_PRIVATE_ATTR) {
        assignee = VTABLE_get_attr_str(interp, self, sig_info->variable_name);
    }

    /* Otherwise if it's public, do a method call to get the assignee. */
    else {
        PMC *meth = VTABLE_find_method(interp, self, sig_info->variable_name);
        if (PMC_IS_NULL(meth)) {
            if (error)
                *error = Parrot_sprintf_c(interp,
                        "Unable to bind attributive parameter '$.%S' - could not find method '%S'",
                        sig_info->variable_name,
                        sig_info->variable_name);
            return BIND_RESULT_FAIL;
        }
        Parrot_ext_call(interp, meth, "Pi->P", self, &assignee);
    }

    /* Now look up infix:<=> and do the assignment. */
    assigner = Parrot_find_global_n(interp, Parrot_get_ctx_HLL_namespace(interp),
            string_from_literal(interp, "!only_infix:="));
    Parrot_ext_call(interp, assigner, "PP", assignee, value);

    return BIND_RESULT_OK;
}


/* Binds a single argument into the lexpad, after doing any checks that are
 * needed. Also handles any type captures. If there is a sub signature, then
 * re-enters the binder. Returns one of the BIND_RESULT_* codes. */
static INTVAL
Rakudo_binding_bind_one_param(PARROT_INTERP, PMC *lexpad, llsig_element *sig_info,
                              PMC *value, INTVAL no_nom_type_check, STRING **error) {
    /* If we need to do a type check, do one. */
    if (!no_nom_type_check) {
        STRING * const ACCEPTS = string_from_literal(interp, "ACCEPTS");
        PMC * const type_obj   = sig_info->nominal_type;
        PMC * accepts_meth     = VTABLE_find_method(interp, type_obj, ACCEPTS);
        PMC * result           = PMCNULL;
        Parrot_ext_call(interp, accepts_meth, "PiP->P", type_obj, value, &result);
        if (!VTABLE_get_bool(interp, result)) {
            /* Type check failed. However, for language inter-op, we do some
             * extra checks if the type is just Positional, Associative, or
             * Callable and the thingy we have matches those enough. */
            /* XXX TODO: Implement language interop checks. */
            if (error) {
                STRING * const perl = string_from_literal(interp, "perl");
                STRING * const HOW  = string_from_literal(interp, "HOW");
                PMC    * perl_meth  = VTABLE_find_method(interp, type_obj, perl);
                PMC    * how_meth   = VTABLE_find_method(interp, value, HOW);
                STRING * expected, * got;
                PMC    * value_how, * value_type;
                Parrot_ext_call(interp, perl_meth, "Pi->S", type_obj, &expected);
                Parrot_ext_call(interp, how_meth, "Pi->P", value, &value_how);
                value_type = VTABLE_get_attr_str(interp, value_how, string_from_literal(interp, "shortname"));
                got        = VTABLE_get_string(interp, value_type);
                *error = Parrot_sprintf_c(interp, "Nominal type check failed for parameter '%S'; expected %S but got %S instead",
                            sig_info->variable_name, expected, got);
            }
            if (VTABLE_isa(interp, value, string_from_literal(interp, "Junction")))
                return BIND_RESULT_JUNCTION;
            else
                return BIND_RESULT_FAIL;
        }
    }
    
    /* Do we have any type captures to bind? */
    if (!PMC_IS_NULL(sig_info->type_captures))
        Rakudo_binding_bind_type_captures(interp, lexpad, sig_info, value);

    /* Apply context. */
    if (sig_info->flags & SIG_ELEM_ARRAY_SIGIL) {
        STRING *Array   = string_from_literal(interp, "Array");
        PMC *array_meth = VTABLE_find_method(interp, value, Array);
        value = descalarref(interp, value);
        if (!PMC_IS_NULL(array_meth))
            Parrot_ext_call(interp, array_meth, "Pi->P", value, &value);
    }
    else if (sig_info->flags & SIG_ELEM_HASH_SIGIL) {
        STRING *Hash   = string_from_literal(interp, "Hash");
        PMC *hash_meth = VTABLE_find_method(interp, value, Hash);
        value = descalarref(interp, value);
        if (!PMC_IS_NULL(hash_meth))
            Parrot_ext_call(interp, hash_meth, "Pi->P", value, &value);
    }
    else {
        STRING *Scalar   = string_from_literal(interp, "Scalar");
        PMC *scalar_meth = VTABLE_find_method(interp, value, Scalar);
        if (!PMC_IS_NULL(scalar_meth))
            Parrot_ext_call(interp, scalar_meth, "Pi->P", value, &value);
    }

    /* If it's not got attributive binding, we'll go about binding it into the
     * lex pad. */
    if (!(sig_info->flags & SIG_ELEM_BIND_ATTRIBUTIVE)) {
        /* Is it "is rw"? */
        if (sig_info->flags & SIG_ELEM_IS_RW) {
            /* XXX TODO Check if rw flag is set, after rw refactor is done. */
            /* If it has a name, bind it into the lexpad. */
            if (sig_info->variable_name)
                VTABLE_set_pmc_keyed_str(interp, lexpad, sig_info->variable_name, value);
        }
        else if (sig_info->flags & SIG_ELEM_IS_REF) {
            /* XXX TODO Implement is ref. */
            if (error)
                *error = string_from_literal(interp, "is ref not yet implemented");
            return BIND_RESULT_FAIL;
        }
        else if (sig_info->flags & SIG_ELEM_IS_COPY) {
            /* Clone the value appropriately, wrap it into an ObjectRef, and bind it. */
            if (sig_info->variable_name) {
                PMC *copy, *ref, *store_meth;
                if (sig_info->flags & SIG_ELEM_ARRAY_SIGIL) {
                    STRING *STORE = string_from_literal(interp, "!STORE");
                    copy          = pmc_new(interp, pmc_type(interp, string_from_literal(interp, PERL6_ARRAY)));
                    store_meth    = VTABLE_find_method(interp, copy, STORE);
                    Parrot_ext_call(interp, store_meth, "PiP", copy, value);
                }
                else if (sig_info->flags & SIG_ELEM_HASH_SIGIL) {
                    STRING *STORE = string_from_literal(interp, "!STORE");
                    copy          = pmc_new(interp, pmc_type(interp, string_from_literal(interp, PERL6_HASH)));
                    store_meth    = VTABLE_find_method(interp, copy, STORE);
                    Parrot_ext_call(interp, store_meth, "PiP", copy, value);
                }
                else {
                    copy = VTABLE_clone(interp, value);
                }
                ref = pmc_new_init(interp, pmc_type(interp,
                        string_from_literal(interp, "ObjectRef")), copy);
                VTABLE_set_pmc_keyed_str(interp, lexpad, sig_info->variable_name, ref);
            }
        }
        else {
            /* Read only. Wrap it into a ObjectRef, mark readonly and bind it. */
            if (sig_info->variable_name) {
                PMC *ref  = pmc_new_init(interp, pmc_type(interp,
                        string_from_literal(interp, "ObjectRef")), value);
                VTABLE_set_pmc_keyed_str(interp, lexpad, sig_info->variable_name, ref);
            }
        }
    }

    /* Is it the invocant? If so, also have to bind to self lexical. */
    if (sig_info->flags & SIG_ELEM_INVOCANT)
        VTABLE_set_pmc_keyed_str(interp, lexpad, string_from_literal(interp, "self"), value);

    /* Handle any constraint types (note that they may refer to the parameter by
     * name, so we need to have bound it already). */
    if (!PMC_IS_NULL(sig_info->post_constraints)) {
        STRING * const ACCEPTS  = string_from_literal(interp, "ACCEPTS");
        PMC * const constraints = sig_info->post_constraints;
        INTVAL num_constraints  = VTABLE_elements(interp, constraints);
        PMC * result            = PMCNULL;
        INTVAL i;
        for (i = 0; i < num_constraints; i++) {
            PMC *cons_type    = VTABLE_get_pmc_keyed_int(interp, constraints, i);
            PMC *accepts_meth = VTABLE_find_method(interp, cons_type, ACCEPTS);
            if (VTABLE_isa(interp, cons_type, string_from_literal(interp, "Sub")))
                Parrot_capture_lex(interp, cons_type);
            Parrot_ext_call(interp, accepts_meth, "PiP->P", cons_type, value, &result);
            if (!VTABLE_get_bool(interp, result)) {
                if (error)
                    *error = Parrot_sprintf_c(interp, "Constraint type check failed for parameter '%S'",
                            sig_info->variable_name);
                return BIND_RESULT_FAIL;
            }
        }
    }

    /* If it's attributive, now we assign it. */
    if (sig_info->flags & SIG_ELEM_BIND_ATTRIBUTIVE) {
        INTVAL result = Rakudo_binding_assign_attributive(interp, lexpad, sig_info, value, error);
        if (result != BIND_RESULT_OK)
            return result;
    }

    /* If it has a sub-signature, bind that. */
    if (!PMC_IS_NULL(sig_info->sub_signature)) {
        /* Turn value into a capture. */
        PMC *capture = PMCNULL;
        PMC *meth    = VTABLE_find_method(interp, value, string_from_literal(interp, "Capture"));
        INTVAL result;
        if (PMC_IS_NULL(meth)) {
            if (error)
                *error = Parrot_sprintf_c(interp, "Could not turn argument into capture");
            return BIND_RESULT_FAIL;
        }
        Parrot_ext_call(interp, meth, "Pi->P", value, &capture);
        
        /* Recurse into signature binder. */
        result = Rakudo_binding_bind_signature(interp, lexpad, sig_info->sub_signature,
                capture, no_nom_type_check, error);
        if (result != BIND_RESULT_OK)
            return result;
    }

    /* Binding of this parameter was thus successful - we're done. */
    return BIND_RESULT_OK;
}


/* This takes a signature element and either runs the closure to get a default
 * value if there is one, or creates an appropriate undefined-ish thingy. */
static PMC *
Rakudo_binding_handle_optional(PARROT_INTERP, llsig_element *sig_info, PMC *lexpad) {
    PMC *cur_lex;
    
    /* Do we have a default value closure? */
    if (!PMC_IS_NULL(sig_info->default_closure)) {
        /* Run it to get a value. */
        PMC *result = PMCNULL;
        Parrot_capture_lex(interp, sig_info->default_closure);
        Parrot_ext_call(interp, sig_info->default_closure, "->P", &result);
        return result;
    }

    /* Did the value already get initialized to something? (We can avoid re-creating a
     * PMC if so.) */
    else if (!PMC_IS_NULL(cur_lex = VTABLE_get_pmc_keyed_str(interp, lexpad, sig_info->variable_name))) {
        return cur_lex;
    }

    /* Otherwise, go by sigil to pick the correct default type of value. */
    else {
        if (sig_info->flags & SIG_ELEM_ARRAY_SIGIL) {
            return pmc_new(interp, pmc_type(interp, string_from_literal(interp, PERL6_ARRAY)));
        }
        else if (sig_info->flags & SIG_ELEM_HASH_SIGIL) {
            return pmc_new(interp, pmc_type(interp, string_from_literal(interp, PERL6_HASH)));
        }
        else {
            return pmc_new(interp, pmc_type(interp, string_from_literal(interp, "Perl6Scalar")));
        }
    }
}


/* Takes a signature along with positional and named arguments and binds them
 * into the provided lexpad (actually, anything that has a Hash interface will
 * do). Returns BIND_RESULT_OK if binding works out, BIND_RESULT_FAIL if there
 * is a failure and BIND_RESULT_JUNCTION if the failure was because of a
 * Junction being passed (meaning we need to auto-thread). */
INTVAL
Rakudo_binding_bind_signature(PARROT_INTERP, PMC *lexpad, PMC *signature,
                              PMC *capture, INTVAL no_nom_type_check,
                              STRING **error) {
    INTVAL        i;
    INTVAL        bind_fail;
    INTVAL        cur_pos_arg = 0;
    INTVAL        num_pos_args = VTABLE_elements(interp, capture);
    PMC           *named_names = PMCNULL;
    llsig_element **elements;
    INTVAL        num_elements;
    PMC           *named_to_pos_cache;

    /* Lazily allocated array of bindings to positionals of nameds. */
    PMC **pos_from_named = NULL;

    /* If we do have some named args, we want to make a clone of the hash
     * to work on. We'll delete stuff from it as we bind, and what we have
     * left over can become the slurpy hash or - if we aren't meant to be
     * taking one - tell us we have a problem. */
    PMC *named_args_copy = PMCNULL;

    /* Check that we have a valid signature and pull the bits out of it. */
    if (!lls_id)
        lls_id = pmc_type(interp, string_from_literal(interp, "P6LowLevelSig"));
    if (signature->vtable->base_type != lls_id)
        Parrot_ex_throw_from_c_args(interp, NULL, EXCEPTION_INVALID_OPERATION,
                "Internal Error: Rakudo_binding_bind_signature passed invalid signature");
    GETATTR_P6LowLevelSig_elements(interp, signature, elements);
    GETATTR_P6LowLevelSig_num_elements(interp, signature, num_elements);
    GETATTR_P6LowLevelSig_named_to_pos_cache(interp, signature, named_to_pos_cache);

    /* Build nameds -> position hash for named positional arguments,
     * if it was not yet built. */
    if (PMC_IS_NULL(named_to_pos_cache)) {
        named_to_pos_cache = pmc_new(interp, enum_class_Hash);
        SETATTR_P6LowLevelSig_named_to_pos_cache(interp, signature, named_to_pos_cache);
        for (i = 0; i < num_elements; i++) {
            /* If we find a named argument, we're done with the positionals. */
            if (!PMC_IS_NULL(elements[i]->named_names))
                break;

            /* Skip slurpies (may be a slurpy block, so can't just break). */
            if (elements[i]->flags & SIG_ELEM_SLURPY)
                continue;

            /* Provided it has a name... */
            if (elements[i]->variable_name) {
                /* Strip any sigil, then stick in named to positional array. */
                STRING *store = elements[i]->variable_name;
                STRING *sigil = Parrot_str_substr(interp, store, 0, 1, NULL, 0);
                STRING *twigil = Parrot_str_substr(interp, store, 1, 1, NULL, 0);
                if (Parrot_str_equal(interp, sigil, string_from_literal(interp, "$")) ||
                        Parrot_str_equal(interp, sigil, string_from_literal(interp, "@")) ||
                        Parrot_str_equal(interp, sigil, string_from_literal(interp, "%")))
                    store = Parrot_str_substr(interp, store, 1,
                            Parrot_str_byte_length(interp, store), NULL, 0);
                if (Parrot_str_equal(interp, twigil, string_from_literal(interp, "!")))
                    store = Parrot_str_substr(interp, store, 1,
                            Parrot_str_byte_length(interp, store), NULL, 0);
                VTABLE_set_integer_keyed_str(interp, named_to_pos_cache, store, i);
            }
        }
    }

    /* If we've got a CallSignature, just has an attribute with list of named
     * parameter names. Otherwise, it's a Capture and we need to do .hash and
     * grab out the keys. */
    if (capture->vtable->base_type == enum_class_CallSignature ||
            VTABLE_isa(interp, capture, string_from_literal(interp, "CallSignature"))) {
        named_names = VTABLE_get_attr_str(interp, capture, string_from_literal(interp, "named"));
    }
    else if (VTABLE_isa(interp, capture, string_from_literal(interp, "Capture"))) {
        PMC *meth = VTABLE_find_method(interp, capture, string_from_literal(interp, "hash"));
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
                "Internal Error: Rakudo_binding_bind_signature passed invalid Capture");
    }

    /* First, consider named arguments, to see if there are any that we will
     * be wanting to bind positionally. */
    if (!PMC_IS_NULL(named_names)) {
        PMC *iter = VTABLE_get_iter(interp, named_names);
        named_args_copy = pmc_new(interp, pmc_type(interp, string_from_literal(interp, PERL6_HASH)));
        while (VTABLE_get_bool(interp, iter)) {
            STRING *name = VTABLE_shift_string(interp, iter);
            if (VTABLE_exists_keyed_str(interp, named_to_pos_cache, name)) {
                /* Found one. We'll stash it away for quick access to bind it
                 * later. */
                INTVAL pos = VTABLE_get_integer_keyed_str(interp, named_to_pos_cache, name);
                if (!pos_from_named)
                    pos_from_named = mem_sys_allocate_zeroed(sizeof(PMC *) * num_elements);
                pos_from_named[pos] = VTABLE_get_pmc_keyed_str(interp, capture, name);
            }
            else {
                /* Otherwise, we'll enter it into the hash of things to bind
                 * to nameds. */
                VTABLE_set_pmc_keyed_str(interp, named_args_copy, name,
                        VTABLE_get_pmc_keyed_str(interp, capture, name));
            }
        }
    }

    /* Now we'll walk through the signature and go about binding things. */
    for (i = 0; i < num_elements; i++) {
        /* Is it a positional sourced from a named? */
        if (pos_from_named && pos_from_named[i]) {
            /* We have the value - try bind this parameter. */
            bind_fail = Rakudo_binding_bind_one_param(interp, lexpad, elements[i],
                    pos_from_named[i], no_nom_type_check, error);
            if (bind_fail) {
                if (pos_from_named)
                    mem_sys_free(pos_from_named);
                return bind_fail;
            }
        }

        /* Could it be a named slurpy? */
        else if (elements[i]->flags & SIG_ELEM_SLURPY_NAMED) {
            /* We'll either take the current named arguments copy hash which
             * will by definition contain all unbound named parameters and use
             * that, or just create an empty one. */
            PMC *slurpy = PMC_IS_NULL(named_args_copy) ?
                    pmc_new(interp, pmc_type(interp, string_from_literal(interp, PERL6_HASH))) :
                    named_args_copy;
            bind_fail = Rakudo_binding_bind_one_param(interp, lexpad, elements[i],
                    slurpy, no_nom_type_check, error);
            if (bind_fail) {
                if (pos_from_named)
                    mem_sys_free(pos_from_named);
                return bind_fail;
            }

            /* Nullify named arguments hash now we've consumed it, to mark all
             * is well. */
            named_args_copy = PMCNULL;
        }

        /* Otherwise, maybe it's a positional. */
        else if (PMC_IS_NULL(elements[i]->named_names)) {
            /* Slurpy? */
            if (elements[i]->flags & SIG_ELEM_SLURPY_POS) {
                /* Create Perl 6 array, create RPA of all remaining things, then
                 * store it. */
                PMC *slurpy     = pmc_new(interp, pmc_type(interp, string_from_literal(interp, PERL6_ARRAY)));
                PMC *temp       = pmc_new(interp, enum_class_ResizablePMCArray);
                STRING *STORE   = string_from_literal(interp, "!STORE");
                PMC *store_meth = VTABLE_find_method(interp, slurpy, STORE);
                while (cur_pos_arg < num_pos_args) {
                    VTABLE_push_pmc(interp, temp, VTABLE_get_pmc_keyed_int(interp, capture, cur_pos_arg));
                    cur_pos_arg++;
                }
                Parrot_ext_call(interp, store_meth, "PiP", slurpy, temp);
                bind_fail = Rakudo_binding_bind_one_param(interp, lexpad, elements[i],
                        slurpy, no_nom_type_check, error);
                if (bind_fail) {
                    if (pos_from_named)
                        mem_sys_free(pos_from_named);
                    return bind_fail;
                }
            }

            /* Otherwise, a positional. */
            else {
                /* Do we have a value?. */
                if (cur_pos_arg < num_pos_args) {
                    /* Easy - just bind that. */
                    PMC *arg = VTABLE_get_pmc_keyed_int(interp, capture, cur_pos_arg);
                    bind_fail = Rakudo_binding_bind_one_param(interp, lexpad, elements[i],
                            arg, no_nom_type_check, error);
                    if (bind_fail) {
                        if (pos_from_named)
                            mem_sys_free(pos_from_named);
                        return bind_fail;
                    }
                    cur_pos_arg++;
                }
                else {
                    /* No value. If it's optional, fetch a default and bind that;
                     * if not, we're screwed. Note that we never nominal type check
                     * an optional with no value passed. */
                    if (elements[i]->flags & SIG_ELEM_IS_OPTIONAL) {
                        PMC *value = Rakudo_binding_handle_optional(interp, elements[i], lexpad);
                        bind_fail = Rakudo_binding_bind_one_param(interp, lexpad, elements[i],
                                value, 1, error);
                        if (bind_fail) {
                            if (pos_from_named)
                                mem_sys_free(pos_from_named);
                            return bind_fail;
                        }
                    }
                    else {
                        if (error)
                            *error = Rakudo_binding_arity_fail(interp, elements, num_elements, num_pos_args, 0);
                        if (pos_from_named)
                            mem_sys_free(pos_from_named);
                        return BIND_RESULT_FAIL;
                    }
                }
            }
        }

        /* Else, it's a non-slurpy named. */
        else {
            /* Try and get hold of value. */
            PMC *value = PMCNULL;
            INTVAL num_names = VTABLE_elements(interp, elements[i]->named_names);
            INTVAL j;
            if (!PMC_IS_NULL(named_args_copy)) {
                for (j = 0; j < num_names; j++) {
                    STRING *name = VTABLE_get_string_keyed_int(interp, elements[i]->named_names, j);
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
                if (elements[i]->flags & SIG_ELEM_IS_OPTIONAL) {
                    value = Rakudo_binding_handle_optional(interp, elements[i], lexpad);
                    bind_fail = Rakudo_binding_bind_one_param(interp, lexpad, elements[i],
                            value, 1, error);
                }
                else {
                    if (error)
                        *error = Parrot_sprintf_c(interp, "Required named parameter '%S' not passed",
                                VTABLE_get_string_keyed_int(interp, elements[i]->named_names, 0));
                    if (pos_from_named)
                        mem_sys_free(pos_from_named);
                    return BIND_RESULT_FAIL;
                }
            }
            else {
                bind_fail = Rakudo_binding_bind_one_param(interp, lexpad, elements[i],
                        value, 0, error);
            }

            /* If we get here, we have a value. Bind it. */
            if (bind_fail) {
                if (pos_from_named)
                    mem_sys_free(pos_from_named);
                return bind_fail;
            }
        }
    }

    /* Free pos_from_named - we no longer need it. */
    if (pos_from_named)
        mem_sys_free(pos_from_named);

    /* Do we have any left-over args? */
    if (cur_pos_arg < num_pos_args) {
        /* Oh noes, too many positionals passed. */
        if (error)
            *error = *error = Rakudo_binding_arity_fail(interp, elements, num_elements, num_pos_args, 1);
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
                STRING *comma = string_from_literal(interp, ", ");
                *error = Parrot_sprintf_c(interp, "%d unexpected named parameters passed (", num_extra);
                while (VTABLE_get_bool(interp, iter)) {
                    STRING *name = VTABLE_shift_string(interp, iter);
                    if (!first)
                        *error = Parrot_str_append(interp, *error, comma);
                    else
                        first = 0;
                    *error = Parrot_str_append(interp, *error, name);
                }
                *error = Parrot_str_append(interp, *error, string_from_literal(interp, ")"));
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
