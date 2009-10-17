/*
$Id$
Copyright (C) 2009, The Perl Foundation.
*/

#define PARROT_IN_EXTENSION
#include "parrot/parrot.h"
#include "parrot/extend.h"
#include "bind.h"


/* Unwraps things inside a scalar reference. */
static PMC *
descalarref(PARROT_INTERP, PMC *ref) {
    INTVAL p6s_id = pmc_type(interp, string_from_literal(interp, "Perl6Scalar"));
    INTVAL or_id  = pmc_type(interp, string_from_literal(interp, "ObjectRef"));
    while (ref->vtable->base_type == or_id || ref->vtable->base_type == p6s_id)
        ref = VTABLE_get_pmc(interp, ref);
    return ref;
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
        PMC * result           = (PMC *)Parrot_run_meth_fromc_args(interp,
                accepts_meth, type_obj, ACCEPTS, "PP", value);
        if (!VTABLE_get_bool(interp, result)) {
            /* Type check failed. However, for language inter-op, we do some
             * extra checks if the type is just Positional, Associative, or
             * Callable and the thingy we have matches those enough. */
            /* XXX TODO: Implement language interop checks. */
            /* XXX TODO: Good type check error message. */
            if (error)
                *error = string_from_literal(interp, "Nominal type check failed");
            if (VTABLE_isa(interp, value, string_from_literal(interp, "Junction")))
                return BIND_RESULT_JUNCTION;
            else
                return BIND_RESULT_FAIL;
        }
    }
    
    /* Do we have any type captures to bind? */
    if (!PMC_IS_NULL(sig_info->type_captures)) {
        /* Obtain type object. */
        STRING * const HOW   = string_from_literal(interp, "HOW");
        PMC * const how_meth = VTABLE_find_method(interp, value, HOW);
        PMC * const meta_obj = (PMC *)Parrot_run_meth_fromc_args(interp,
                how_meth, value, HOW, "P");
        PMC * const type_obj = VTABLE_get_attr_str(interp, meta_obj,
                string_from_literal(interp, "protoobject"));

        /* Iterate over symbols we need to bind this to, and bind 'em. */
        PMC * const iter = VTABLE_get_iter(interp, sig_info->type_captures);
        while (VTABLE_get_bool(interp, iter)) {
            STRING *name = VTABLE_shift_string(interp, iter);
            VTABLE_set_pmc_keyed_str(interp, lexpad, name, type_obj);
        }
    }

    /* Apply context. */
    if (sig_info->flags & SIG_ELEM_ARRAY_SIGIL) {
        STRING *Array   = string_from_literal(interp, "Array");
        PMC *array_meth = VTABLE_find_method(interp, value, Array);
        value = descalarref(interp, value);
        if (!PMC_IS_NULL(array_meth))
            value = (PMC *)Parrot_run_meth_fromc_args(interp, array_meth, value, Array, "P");
    }
    else if (sig_info->flags & SIG_ELEM_HASH_SIGIL) {
        STRING *Hash   = string_from_literal(interp, "Hash");
        PMC *hash_meth = VTABLE_find_method(interp, value, Hash);
        value = descalarref(interp, value);
        if (!PMC_IS_NULL(hash_meth))
            value = (PMC *)Parrot_run_meth_fromc_args(interp, hash_meth, value, Hash, "P");
    }
    else {
        STRING *Scalar   = string_from_literal(interp, "Scalar");
        PMC *scalar_meth = VTABLE_find_method(interp, value, Scalar);
        if (!PMC_IS_NULL(scalar_meth))
            value = (PMC *)Parrot_run_meth_fromc_args(interp, scalar_meth, value, Scalar, "P");
    }

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
                copy          = pmc_new(interp, pmc_type(interp, string_from_literal(interp, "Perl6Array")));
                store_meth    = VTABLE_find_method(interp, copy, STORE);
                Parrot_run_meth_fromc_args(interp, store_meth, copy, STORE, "vP", value);
            }
            else if (sig_info->flags & SIG_ELEM_HASH_SIGIL) {
                STRING *STORE = string_from_literal(interp, "!STORE");
                copy          = pmc_new(interp, pmc_type(interp, string_from_literal(interp, "Perl6Hash")));
                store_meth    = VTABLE_find_method(interp, copy, STORE);
                Parrot_run_meth_fromc_args(interp, store_meth, copy, STORE, "vP", value);
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
            VTABLE_setprop(interp, ref, string_from_literal(interp, "readonly"), ref);
            VTABLE_set_pmc_keyed_str(interp, lexpad, sig_info->variable_name, ref);
        }
    }

    /* Handle any constraint types (note that they may refer to the parameter by
     * name, so we need to have bound it already). */
    if (!PMC_IS_NULL(sig_info->post_constraints)) {
        STRING * const ACCEPTS  = string_from_literal(interp, "ACCEPTS");
        PMC * const constraints = sig_info->post_constraints;
        INTVAL num_constraints  = VTABLE_elements(interp, constraints);
        PMC * result;
        INTVAL i;
        for (i = 0; i < num_constraints; i++) {
            PMC *cons_type    = VTABLE_get_pmc_keyed_int(interp, constraints, i);
            PMC *accepts_meth = VTABLE_find_method(interp, cons_type, ACCEPTS);
            if (VTABLE_isa(interp, cons_type, string_from_literal(interp, "Sub")))
                Parrot_capture_lex(interp, cons_type);
            result = (PMC *)Parrot_run_meth_fromc_args(interp, accepts_meth, cons_type, ACCEPTS, "PP", value);
            if (!VTABLE_get_bool(interp, result)) {
                /* XXX TODO: Good type check error message. */
                if (error)
                    *error = string_from_literal(interp, "Constraint type check failed");
                return BIND_RESULT_FAIL;
            }
        }
    }

    /* If it has a sub-signature, bind that. */
    if (!PMC_IS_NULL(sig_info->sub_signature)) {
        /* XXX TODO: Fill out how we obtain pos_args and named_args, or I
         * guess probably just a capture. But otherwise, it's just a case
         * of recursing. */
        if (error)
            *error = string_from_literal(interp, "Sub-signatures not yet implemented");
        return BIND_RESULT_FAIL;
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
        Parrot_capture_lex(interp, sig_info->default_closure);
        return (PMC *)Parrot_call_sub(interp, sig_info->default_closure, "P");
    }

    /* Did the value already get initialized to something? (We can avoid re-creating a
     * PMC if so.) */
    else if (!PMC_IS_NULL(cur_lex = VTABLE_get_pmc_keyed_str(interp, lexpad, sig_info->variable_name))) {
        return cur_lex;
    }

    /* Otherwise, go by sigil to pick the correct default type of value. */
    else {
        if (sig_info->flags & SIG_ELEM_ARRAY_SIGIL) {
            return pmc_new(interp, pmc_type(interp, string_from_literal(interp, "Perl6Array")));
        }
        else if (sig_info->flags & SIG_ELEM_HASH_SIGIL) {
            return pmc_new(interp, pmc_type(interp, string_from_literal(interp, "Perl6Hash")));
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
PARROT_DYNEXT_EXPORT
Rakudo_binding_bind_signature(PARROT_INTERP, PMC *lexpad,
                              llsig_element **elements, INTVAL num_elements,
                              PMC *pos_args, PMC *named_args,
                              INTVAL no_nom_type_check, STRING **error) {
    INTVAL i;
    INTVAL bind_fail;
    INTVAL cur_pos_arg = 0;
    INTVAL num_pos_args = VTABLE_elements(interp, pos_args);

    /* Lazily allocated array of bindings to positionals of nameds. */
    PMC **pos_from_named = NULL;

    /* If we do have some named args, we want to make a clone of the hash
     * to work on. We'll delete stuff from it as we bind, and what we have
     * left over can become the slurpy hash or - if we aren't meant to be
     * taking one - tell us we have a problem. */
    PMC *named_args_copy = PMCNULL;

    /* Build nameds -> position hash for named positional arguments. */
    /* XXX We only need do this on the first binding, not every one - add
     * logic to cache this instead. For extra minor speed win, use Hash
     * directly perhaps, to avoid a level of indirection through the PMC
     * interface. */
    PMC *named_to_pos_cache = pmc_new(interp, enum_class_Hash);
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
            if (Parrot_str_equal(interp, sigil, string_from_literal(interp, "$")) ||
                    Parrot_str_equal(interp, sigil, string_from_literal(interp, "@")) ||
                    Parrot_str_equal(interp, sigil, string_from_literal(interp, "%")))
                store = Parrot_str_substr(interp, store, 1,
                        Parrot_str_byte_length(interp, store), NULL, 0);
            VTABLE_set_integer_keyed_str(interp, named_to_pos_cache, store, i);
        }
    }

    /* First, consider named arguments, to see if there are any that we will
     * be wanting to bind positionally. */
    if (VTABLE_elements(interp, named_args)) {
        PMC *iter = VTABLE_get_iter(interp, named_args);
        named_args_copy = pmc_new(interp, pmc_type(interp, string_from_literal(interp, "Perl6Hash")));
        while (VTABLE_get_bool(interp, iter)) {
            STRING *name = VTABLE_shift_string(interp, iter);
            if (VTABLE_exists_keyed_str(interp, named_to_pos_cache, name)) {
                /* Found one. We'll stash it away for quick access to bind it
                 * later. */
                INTVAL pos = VTABLE_get_integer_keyed_str(interp, named_to_pos_cache, name);
                if (!pos_from_named)
                    pos_from_named = mem_sys_allocate_zeroed(sizeof(PMC *) * num_elements);
                pos_from_named[pos] = VTABLE_get_pmc_keyed_str(interp, named_args, name);
            }
            else {
                /* Otherwise, we'll enter it into the hash of things to bind
                 * to nameds. */
                VTABLE_set_pmc_keyed_str(interp, named_args_copy, name,
                        VTABLE_get_pmc_keyed_str(interp, named_args, name));
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
            if (bind_fail)
                return bind_fail;
        }

        /* Could it be a named slurpy? */
        else if (elements[i]->flags & SIG_ELEM_SLURPY_NAMED) {
            /* We'll either take the current named arguments copy hash which
             * will by definition contain all unbound named parameters and use
             * that, or just create an empty one. */
            PMC *slurpy = PMC_IS_NULL(named_args_copy) ?
                    pmc_new(interp, pmc_type(interp, string_from_literal(interp, "Perl6Hash"))) :
                    named_args_copy;
            bind_fail = Rakudo_binding_bind_one_param(interp, lexpad, elements[i],
                    slurpy, no_nom_type_check, error);
            if (bind_fail)
                return bind_fail;

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
                PMC *slurpy     = pmc_new(interp, pmc_type(interp, string_from_literal(interp, "Perl6Array")));
                PMC *temp       = pmc_new(interp, enum_class_ResizablePMCArray);
                STRING *STORE   = string_from_literal(interp, "!STORE");
                PMC *store_meth = VTABLE_find_method(interp, slurpy, STORE);
                while (cur_pos_arg < num_pos_args) {
                    VTABLE_push_pmc(interp, temp, VTABLE_get_pmc_keyed_int(interp, pos_args, cur_pos_arg));
                    cur_pos_arg++;
                }
                Parrot_run_meth_fromc_args(interp, store_meth, slurpy, STORE, "vP", temp);
                bind_fail = Rakudo_binding_bind_one_param(interp, lexpad, elements[i],
                        slurpy, no_nom_type_check, error);
                if (bind_fail)
                    return bind_fail;
            }

            /* Otherwise, a positional. */
            else {
                /* Is it the invocant? If so, already handled out of band. */
                if (elements[i]->flags & SIG_ELEM_INVOCANT) {
                    cur_pos_arg++;
                    continue;
                }

                /* Do we have a value?. */
                else if (cur_pos_arg < num_pos_args) {
                    /* Easy - just bind that. */
                    PMC *arg = VTABLE_get_pmc_keyed_int(interp, pos_args, cur_pos_arg);
                    bind_fail = Rakudo_binding_bind_one_param(interp, lexpad, elements[i],
                            arg, no_nom_type_check, error);
                    if (bind_fail)
                        return bind_fail;
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
                        if (bind_fail)
                            return bind_fail;
                    }
                    else {
                        /* XXX TODO: Make this error have some numbers in it,
                         * or other useful info. */
                        if (error)
                            *error = string_from_literal(interp, "Not enough positional parameters passed");
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
                    /* XXX TODO: Make this error include missing name. */
                    if (error)
                        *error = string_from_literal(interp, "Required named parameter not passed");
                    return BIND_RESULT_FAIL;
                }
            }
            else {
                bind_fail = Rakudo_binding_bind_one_param(interp, lexpad, elements[i],
                        value, 0, error);
            }

            /* If we get here, we have a value. Bind it. */
            if (bind_fail)
                return bind_fail;
        }
    }

    /* Do we have any left-over args? */
    if (cur_pos_arg < num_pos_args) {
        /* Oh noes, too many positionals passed. XXX TODO: improve error. */
        if (error)
            *error = string_from_literal(interp, "Too many positional parameters passed");
        return BIND_RESULT_FAIL;
    }
    if (!PMC_IS_NULL(named_args_copy) && VTABLE_elements(interp, named_args_copy)) {
        /* Oh noes, unexpected named args. XXX TODO: error needs to have names. */
        if (error)
            *error = string_from_literal(interp, "Unexpected named parameters passed");
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
