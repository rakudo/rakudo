/*
$Id$
Copyright (C) 2008-2011, The Perl Foundation.

=head1 NAME

src/binder/multidispatch.c - Perl 6 multi-dispatcher

=head1 DESCRIPTION

This implements Perl 6 multiple dispatch.

=cut

*/

#define PARROT_IN_EXTENSION
#include "parrot/parrot.h"
#include "parrot/extend.h"
#include "bind.h"
#include "multidispatch.h"
#include "container.h"
#include "types.h"
#include "sixmodelobject.h"

/* Some constants for candidate sorter. */
#define EDGE_REMOVAL_TODO -1
#define EDGE_REMOVED      -2

/* Some constants for the dispatcher. */
#define MMD_ONE_RESULT   0
#define MMD_MANY_RESULTS 1

/* Special value we set arity to when we have a slurpy. */
#define SLURPY_ARITY     1 << 30

/*

=head1 FUNCTIONS

=over 4

=item C<static INTVAL is_narrower(PARROT_INTERP, Rakudo_md_candidate_info *a, Rakudo_md_candidate_info *b)>

Takes two candidates and determines if the first one is narrower than the
second. Returns a true value if they are.

=cut

*/
static INTVAL is_narrower(PARROT_INTERP, Rakudo_md_candidate_info *a, Rakudo_md_candidate_info *b) {
    INTVAL narrower = 0;
    INTVAL tied = 0;
    INTVAL i, types_to_check;

    /* Work out how many parameters to compare, factoring in slurpiness
     * and optionals. */
    if (a->num_types == b->num_types)
        types_to_check = a->num_types;
    else if (a->min_arity == b->min_arity)
        types_to_check = a->num_types > b->num_types ? b->num_types : a->num_types;
    else if (a->max_arity != SLURPY_ARITY && b->max_arity == SLURPY_ARITY)
        return 1;
    else
        return 0;

    /* Analyse each parameter in the two candidates. */
    for (i = 0; i < types_to_check; i++) {
        PMC * const type_obj_a = a->types[i];
        PMC * const type_obj_b = b->types[i];
        if (type_obj_a == type_obj_b) {
            /* Same type; narrower if first has constraints and other doesn't;
             * tied if neither has constraints or both have constraints. */
            if (!PMC_IS_NULL(a->constraints[i]) && PMC_IS_NULL(b->constraints[i]))
                narrower++;
            else if ((PMC_IS_NULL(a->constraints[i]) && PMC_IS_NULL(b->constraints[i]))
                 ||
                    (!PMC_IS_NULL(a->constraints[i]) && !PMC_IS_NULL(b->constraints[i])))
                tied++;
        }
        else if ((a->type_flags[i] & TYPE_NATIVE_MASK) && !(b->type_flags[i] & TYPE_NATIVE_MASK))
        {
            /* Narrower because natives always are. */
            narrower++;
        }
        else if ((b->type_flags[i] & TYPE_NATIVE_MASK) && !(a->type_flags[i] & TYPE_NATIVE_MASK))
        {
            /* Wider; skip over here so we don't go counting this as tied in
             * the next branch. */
        }
        else {
            if (STABLE(type_obj_a)->type_check(interp, type_obj_a, type_obj_b)) {
                /* Narrower - note it and we're done. */
                narrower++;
            }
            else {
                /* Make sure it's tied, rather than the other way around. */
                if (!STABLE(type_obj_b)->type_check(interp, type_obj_b, type_obj_a))
                    tied++;
            }
        }
    }

    /* If one is narrower than the other from current analysis, we're done. */
    if (narrower >= 1 && narrower + tied == types_to_check)
        return 1;

    /* If they aren't tied, we're also done. */
    else if (tied != types_to_check)
        return 0;

    /* Otherwise, we see if one has a slurpy and the other not. A lack of
     * slurpiness makes the candidate narrower. */

    if (a->max_arity != SLURPY_ARITY && b->max_arity == SLURPY_ARITY) {
        return 1;
    }

    /* Also narrower if the first needs a bind check and the second doesn't, if
     * we wouldn't deem the other one narrower than this one int terms of
     * slurpyness. Otherwise, they're tied. */
    return !(b->max_arity != SLURPY_ARITY && a->max_arity == SLURPY_ARITY)
        && (a->bind_check && !(b->bind_check));
}


/*

=item C<static Rakudo_md_candidate_info** sort_candidates(PMC *candidates)>

Takes a ResizablePMCArray of the candidates, collects information about them
and then does a topological sort of them.

=cut

*/
static Rakudo_md_candidate_info** sort_candidates(PARROT_INTERP, PMC *candidates) {
    INTVAL i;
    const char *error = NULL;

    /* Allocate results array (just allocate it for worst case, which
     * is no ties ever, so a null between all of them, and then space
     * for the terminating null. */
    INTVAL num_candidates = VTABLE_elements(interp, candidates);
    Rakudo_md_candidate_info ** const result = mem_allocate_n_zeroed_typed(
            2 * num_candidates + 2, Rakudo_md_candidate_info*);

    /* Create a node for each candidate in the graph. */
    Rakudo_md_candidate_graph_node ** const graph = mem_allocate_n_zeroed_typed(
            num_candidates + 1, Rakudo_md_candidate_graph_node*);

    INTVAL insert_pos = 0;

    for (i = 0; i < num_candidates; i++) {
        PMC                      *param_pmc;
        Rakudo_Signature         *sig;
        Rakudo_md_candidate_info *info;
        INTVAL                    num_params;
        INTVAL                    j;
        INTVAL                    significant_param;

        /* Get information about this candidate. */
        PMC * const candidate = VTABLE_get_pmc_keyed_int(interp, candidates, i);

        /* Create it an entry. */
        info      = mem_allocate_zeroed_typed(Rakudo_md_candidate_info);
        info->sub = candidate;

        /* Get hold of signature. */
        info->signature = ((Rakudo_Code *)PMC_data(candidate))->signature;
        sig = (Rakudo_Signature *)PMC_data(info->signature);
        num_params = VTABLE_elements(interp, sig->params);

        /* Type information. */
        info->types         = mem_allocate_n_zeroed_typed(num_params + 1, PMC*);
        info->type_flags    = mem_allocate_n_zeroed_typed(num_params + 1, INTVAL);
        info->constraints   = mem_allocate_n_zeroed_typed(num_params + 1, PMC*);
        significant_param = 0;

        for (j = 0; j < num_params; j++) {
            PMC              *param_pmc = VTABLE_get_pmc_keyed_int(interp, sig->params, j);
            Rakudo_Parameter *param     = (Rakudo_Parameter *)PMC_data(param_pmc);
            
            /* If it's named (and not slurpy) don't need its type info but we
             * will need a bindability check during the dispatch for it. */
            if (!PMC_IS_NULL(param->named_names)) {
                if (!(param->flags & SIG_ELEM_IS_OPTIONAL) &&
                        VTABLE_elements(interp, param->named_names) == 1)
                    info->req_named = VTABLE_get_string_keyed_int(interp, param->named_names, 0);
                info->bind_check = 1;
                continue;
            }

            /* If it's got a sub-signature, also need a bind check. */
            if (!PMC_IS_NULL(param->sub_llsig))
                info->bind_check = 1;

            /* If it's named slurpy, we're done. */
            if (param->flags & SIG_ELEM_SLURPY_NAMED)
                break;

            /* Otherwise, positional or slurpy and contributes to arity. */
            if (param->flags & SIG_ELEM_SLURPY_POS || param->flags & SIG_ELEM_IS_CAPTURE) {
                info->max_arity = SLURPY_ARITY;
                break;
            }
            else if (param->flags & SIG_ELEM_IS_OPTIONAL) {
                info->max_arity++;
            }
            else {
                info->max_arity++;
                info->min_arity++;
            }

            /* Record type info for this parameter. */
            if (param->flags & SIG_ELEM_NOMINAL_GENERIC) {
                info->bind_check = 1;
                info->types[significant_param] = Rakudo_types_any_get();
            }
            else {
                info->types[significant_param] = param->nominal_type;
            }
            info->constraints[significant_param] = param->post_constraints;
            if (!PMC_IS_NULL(info->constraints[significant_param]))
                info->bind_check = 1;
            if (param->flags & SIG_ELEM_MULTI_INVOCANT)
                info->num_types++;
            if (param->flags & SIG_ELEM_DEFINED_ONLY)
                info->type_flags[significant_param] = DEFCON_DEFINED;
            else if (param->flags & SIG_ELEM_UNDEFINED_ONLY)
                info->type_flags[significant_param] = DEFCON_UNDEFINED;
            if (param->flags & SIG_ELEM_NATIVE_INT_VALUE)
                info->type_flags[significant_param] += TYPE_NATIVE_INT;
            else if (param->flags & SIG_ELEM_NATIVE_NUM_VALUE)
                info->type_flags[significant_param] += TYPE_NATIVE_NUM;
            else if (param->flags & SIG_ELEM_NATIVE_STR_VALUE)
                info->type_flags[significant_param] += TYPE_NATIVE_STR;
            significant_param++;
        }

        /* Add it to graph node, and initialize list of edges. */
        graph[insert_pos]        = mem_allocate_zeroed_typed(Rakudo_md_candidate_graph_node);
        graph[insert_pos]->info  = info;
        graph[insert_pos]->edges = mem_allocate_n_zeroed_typed(
            num_candidates, Rakudo_md_candidate_graph_node*);

        insert_pos++;
    }

    /* If we found duplicate protos, don't go any further. */
    if (!error) {
        INTVAL candidates_to_sort;
        INTVAL result_pos;

        /* The actual number of candidates needs to discount any protos. */
        num_candidates = insert_pos;

        /* Now analyze type narrowness of the candidates relative to each other
         * and create the edges. */
        for (i = 0; i < num_candidates; i++) {
            INTVAL j;
            for (j = 0; j < num_candidates; j++) {
                if (i == j)
                    continue;
                if (is_narrower(interp, graph[i]->info, graph[j]->info)) {
                    graph[i]->edges[graph[i]->edges_out] = graph[j];
                    graph[i]->edges_out++;
                    graph[j]->edges_in++;
                }
            }
        }

        /* Perform the topological sort. */
        candidates_to_sort = num_candidates;
        result_pos         = 0;

        while (candidates_to_sort > 0) {
            const INTVAL rem_start_point = result_pos;

            /* Find any nodes that have no incoming edges and add them to
             * results. */
            for (i = 0; i < num_candidates; i++) {
                if (graph[i]->edges_in == 0) {
                    /* Add to results. */
                    result[result_pos] = graph[i]->info;
                    graph[i]->info     = NULL;
                    result_pos++;
                    candidates_to_sort--;
                    graph[i]->edges_in = EDGE_REMOVAL_TODO;
                }
            }
            if (rem_start_point == result_pos) {
                error = "Circularity detected in multi sub types.";
                break;
            }

            /* Now we need to decrement edges in counts for things that had
             * edges from candidates we added here. */
            for (i = 0; i < num_candidates; i++) {
                if (graph[i]->edges_in == EDGE_REMOVAL_TODO) {
                    INTVAL j;
                    for (j = 0; j < graph[i]->edges_out; j++)
                        graph[i]->edges[j]->edges_in--;
                    graph[i]->edges_in = EDGE_REMOVED;
                }
            }

            /* This is end of a tied group, so leave a gap. */
            result_pos++;
        }
    }

    /* Free memory associated with the graph. */
    for (i = 0; i < num_candidates; i++) {
        Rakudo_md_candidate_info *info = graph[i]->info;
        if (info) {
            if (info->types)
                mem_sys_free(info->types);
            if (info->type_flags)
                mem_sys_free(info->type_flags);
            if (info->constraints)
                mem_sys_free(info->constraints);
            mem_sys_free(info);
        }
        mem_sys_free(graph[i]->edges);
        mem_sys_free(graph[i]);
    }

    mem_sys_free(graph);

    /* If we had an error, free memory for result array and throw exception. */
    if (error) {
        mem_sys_free(result);
        Parrot_ex_throw_from_c_args(interp, 0, 1, error);
    }

    return result;
}


/*

=item C<static INTVAL has_junctional_args(PARROT_INTERP, INTVAL num_args, struct Pcc_cell * pc_positionals)>

Checks if any of the args are junctional.

=cut

*/

static INTVAL has_junctional_args(PARROT_INTERP, INTVAL num_args, struct Pcc_cell * pc_positionals) {
    INTVAL i;

    for (i = 0; i < num_args; i++) {
        if (pc_positionals[i].type == BIND_VAL_OBJ) {
            if (pc_positionals[i].u.p->vtable->base_type == Rakudo_smo_id()) {
                PMC * const arg = Rakudo_cont_decontainerize(interp, pc_positionals[i].u.p);
                if (STABLE(arg)->WHAT == Rakudo_types_junction_get())
                    return 1;
            }
        }
    }
    
    return 0;
}


/*

=item C<static Rakudo_md_candidate_info ** obtain_candidate_list(PARROT_INTERP,
        INTVAL has_cache, Rakudo_Code *code_obj)>

Gets the sorted candiate list (either from cache, or by producing it).

=cut

*/
static Rakudo_md_candidate_info ** obtain_candidate_list(PARROT_INTERP,
        INTVAL has_cache, PMC *dispatcher, Rakudo_Code *code_obj) {
    if (has_cache) {
        return ((Rakudo_md_cache *)VTABLE_get_pointer(interp,
            code_obj->dispatcher_cache))->candidates;
    }
    else {
        Rakudo_md_cache *cache = mem_allocate_zeroed_typed(Rakudo_md_cache);
        cache->candidates = sort_candidates(interp, code_obj->dispatchees);
        code_obj->dispatcher_cache = Parrot_pmc_new(interp, enum_class_Pointer);
        VTABLE_set_pointer(interp, code_obj->dispatcher_cache, cache);
        PARROT_GC_WRITE_BARRIER(interp, dispatcher);
        return cache->candidates;
    }
}


/*

=item C<PMC * Rakudo_md_ct_dispatch(PARROT_INTERP, PMC *dispatcher, PMC *capture, PMC **result)>

Tries to resolve a multi-dispatch at compile time. Returns a flag
and, if a dispatch is possible, sets the result.

=cut

*/

INTVAL
Rakudo_md_ct_dispatch(PARROT_INTERP, PMC *dispatcher, PMC *capture, PMC **result) {
    /* Get hold of the candidates. */
    Rakudo_Code *code_obj  = (Rakudo_Code *)PMC_data(dispatcher);
    INTVAL       has_cache = !PMC_IS_NULL(code_obj->dispatcher_cache);
    Rakudo_md_candidate_info **cands = obtain_candidate_list(interp, has_cache,
        dispatcher, code_obj);
    
    /* Current dispatch state. */
    Rakudo_md_candidate_info **cur_candidate = cands;
    INTVAL type_mismatch, type_check_count, type_match_possible;
    INTVAL all_native     = 1;
    INTVAL seen_all       = 0;
    INTVAL arity_possible = 0;
    INTVAL type_possible  = 0;
    PMC *cur_result = PMCNULL;
    
    /* Grab positionals. */
    struct Pcc_cell * pc_positionals;
    INTVAL num_args = VTABLE_elements(interp, capture);
    if (capture->vtable->base_type == enum_class_CallContext)
        GETATTR_CallContext_positionals(interp, capture, pc_positionals);
    else
        return MD_CT_NOT_SURE;
    
    /* Look through the candidates. If we see anything that needs a bind
     * check or a definedness check, we can't decide it at compile time,
     * so bail out immediately. */
    while (1) {
        INTVAL used_defcon = 0;
        INTVAL i;

        /* Did we reach the end of a tied group? If so, note we can only
         * consider the narrowest group, *unless* they are all natively
         * typed candidates in which case we can look a bit further.
         * We also exit if we found something. */
        if (*cur_candidate == NULL) {
            if (cur_candidate[1] && all_native && PMC_IS_NULL(cur_result)) {
                cur_candidate++;
                continue;
            }
            else {
                seen_all = cur_candidate[1] == NULL;
                break;
            }
        }

        /* Check if it's admissable by arity. */
        if (num_args < (*cur_candidate)->min_arity
        ||  num_args > (*cur_candidate)->max_arity) {
            cur_candidate++;
            continue;
        }
        
        /* If we got this far, something at least matched on arity. */
        arity_possible = 1;

        /* Check if it's admissable by type. */
        type_check_count = (*cur_candidate)->num_types > num_args ?
                           num_args :
                           (*cur_candidate)->num_types;
        type_mismatch = 0;
        type_match_possible = 1;

        for (i = 0; i < type_check_count; i++) {
            PMC * const type_obj = (*cur_candidate)->types[i];
            INTVAL type_flags    = (*cur_candidate)->type_flags[i];
            INTVAL got_prim      = pc_positionals[i].type;
            if (type_flags & TYPE_NATIVE_MASK) {
                /* Looking for a natively typed value. Did we get one? */
                if (got_prim == BIND_VAL_OBJ) {
                    /* Object; won't do. */
                    type_mismatch = 1;
                    break;
                }
                if (((type_flags & TYPE_NATIVE_INT) && got_prim != BIND_VAL_INT) ||
                    ((type_flags & TYPE_NATIVE_NUM) && got_prim != BIND_VAL_NUM) ||
                    ((type_flags & TYPE_NATIVE_STR) && got_prim != BIND_VAL_STR)) {
                    /* Mismatch. */
                    type_mismatch = 1;
                    type_match_possible = 0;
                    break;
                }
            }
            else {
                /* Work out parameter. */
                PMC * const param =
                    got_prim == BIND_VAL_OBJ ? pc_positionals[i].u.p :
                    got_prim == BIND_VAL_INT ? Rakudo_types_int_get() :
                    got_prim == BIND_VAL_NUM ? Rakudo_types_num_get() :
                                               Rakudo_types_str_get();

                /* If we're here, it's a non-native. */
                all_native = 0;
                
                /* Check type. If that doesn't rule it out, then check if it's
                 * got definedness constraints. If it does, note that; if we
                 * match but depend on definedness constraints we can't do
                 * any more. */
                if (type_obj != Rakudo_types_mu_get() &&
                        !STABLE(param)->type_check(interp, param, type_obj)) {
                    type_mismatch = 1;
                    
                    /* We didn't match, but that doesn't mean we cannot at
                     * runtime (e.g. the most we know about the type could
                     * be that it's Any, but at runtime that feasibly could
                     * be Int). In some cases we never could though (Str
                     * passed to an Int parameter). */
                    if (!STABLE(type_obj)->type_check(interp, type_obj, param))
                        type_match_possible = 0;
                }
                else if ((*cur_candidate)->type_flags[i] & DEFCON_MASK) {
                    used_defcon = 1;
                }
            }
        }
        if (type_match_possible)
            type_possible = 1;
        if (type_mismatch) {
            cur_candidate++;
            continue;
        }
        if (used_defcon)
            return MD_CT_NOT_SURE;

        /* If it's possible but needs a bind check, we're not going to be
         * able to decide it. */
        if ((*cur_candidate)->bind_check)
            return MD_CT_NOT_SURE;

        /* If we get here, it's the result. Well, unless we already had one,
         * in which case we're in bother 'cus we don't know how to disambiguate
         * at compile time. */
        if (PMC_IS_NULL(cur_result)) {
            cur_result = (*cur_candidate)->sub;
            cur_candidate++;
        }
        else {
            return MD_CT_NOT_SURE;
        }
    }
    
    /* If we saw all the candidates, and got no result, and the arity never
     * matched or when it did there was no way any candidates could get
     * passed matching types, then we know it would never work. */
    if (seen_all && (!arity_possible || !type_possible) && PMC_IS_NULL(cur_result)) {
        /* Ensure no junctional args before we flag the failure. */
        return has_junctional_args(interp, num_args, pc_positionals) ?
            MD_CT_NOT_SURE :
            MD_CT_NO_WAY;
    }
    
    /* If we got a result, return it. */
    if (!PMC_IS_NULL(cur_result)) {
        *result = cur_result;
        return MD_CT_DECIDED;
    }

    /* Otherwise, dunno...we'll have to find out at runtime. */
    return MD_CT_NOT_SURE;
}

/*

=back

=cut

*/

/*
 * Local variables:
 *   c-file-style: "parrot"
 * End:
 * vim: expandtab shiftwidth=4:
 */
