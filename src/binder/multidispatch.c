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
     * slurpiness makes the candidate narrower. Otherwise, they're tied. */
    return a->max_arity != SLURPY_ARITY && b->max_arity == SLURPY_ARITY;
}


/*

=item C<static candidate_info** sort_candidates(PMC *candidates)>

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
            2 * num_candidates + 1, Rakudo_md_candidate_info*);

    /* Create a node for each candidate in the graph. */
    Rakudo_md_candidate_graph_node ** const graph = mem_allocate_n_zeroed_typed(
            num_candidates, Rakudo_md_candidate_graph_node*);

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
        info->definednesses = mem_allocate_n_zeroed_typed(num_params + 1, INTVAL);
        info->constraints   = mem_allocate_n_zeroed_typed(num_params + 1, PMC*);
        significant_param = 0;

        for (j = 0; j < num_params; j++) {
            PMC              *param_pmc = VTABLE_get_pmc_keyed_int(interp, sig->params, j);
            Rakudo_Parameter *param     = (Rakudo_Parameter *)PMC_data(param_pmc);
            
            /* If it's named (and not slurpy) don't need its type info but we
             * will need a bindability check during the dispatch for it. */
            if (!PMC_IS_NULL(param->named_names)) {
                if (!(param->flags & SIG_ELEM_IS_OPTIONAL))
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
            if (param->flags & SIG_ELEM_SLURPY_POS) {
                info->max_arity = SLURPY_ARITY;
            }
            else if (param->flags & SIG_ELEM_IS_OPTIONAL) {
                info->max_arity++;
            }
            else {
                info->max_arity++;
                info->min_arity++;
            }

            /* Record type info for this parameter. */
            info->types[significant_param]       = param->nominal_type;
            info->constraints[significant_param] = param->post_constraints;
            if (!PMC_IS_NULL(info->constraints[significant_param]))
                info->bind_check = 1;
            if (param->flags & SIG_ELEM_MULTI_INVOCANT)
                info->num_types++;
            if (param->flags & SIG_ELEM_DEFINED_ONLY)
                info->definednesses[significant_param] = DEFCON_DEFINED;
            else if (param->flags & SIG_ELEM_UNDEFINED_ONLY)
                info->definednesses[significant_param] = DEFCON_UNDEFINED;
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

=item C<static INTVAL has_junctional_args(PARROT_INTERP, PMC *args)>

Checks if any of the args are junctional.

=cut

*/

static INTVAL has_junctional_args(PARROT_INTERP, PMC *args) {
    const INTVAL num_args = VTABLE_elements(interp, args);
    INTVAL i;

    for (i = 0; i < num_args; i++) {
        /* XXX TODO: junction type detection.
        PMC * const arg = VTABLE_get_pmc_keyed_int(interp, args, i);
        */
    }
    
    return 0;
}


/*

=item C<static STRING* dump_signature(PARROT_INTERP, STRING *so_far, PMC *sub)>

Utility for getting hold of the signature dump for a sub, which aids us in
producing awesomer errors.

=cut

*/
static STRING* dump_signature(PARROT_INTERP, STRING *so_far, PMC *sub) {
    STRING * const sig_name  = Parrot_str_new(interp, "signature", 0);
    STRING * const perl_name = Parrot_str_new(interp, "perl", 0);
    STRING * const newline   = Parrot_str_new(interp, "\n", 0);
    PMC    * sig_meth, *sig_obj, *perl_meth, * sig_perl;
    sig_meth = VTABLE_find_method(interp, sub, sig_name);
    Parrot_ext_call(interp, sig_meth, "Pi->P", sub, &sig_obj);
    perl_meth = VTABLE_find_method(interp, sig_obj, perl_name);
    Parrot_ext_call(interp, perl_meth, "Pi->P", sig_obj, &sig_perl);
    so_far = Parrot_str_concat(interp, so_far,
        REPR(sig_perl)->get_str(interp, sig_perl));
    so_far = Parrot_str_concat(interp, so_far, newline);
    return so_far;
}


/*

=item C<static PMC* find_best_candidate(PARROT_INTERP, Rakudo_md_candidate_info **candidates,
                                        INTVAL num_candidates, PMC *capture, opcode_t *next)>

Runs the Perl 6 MMD algorithm. Returns either the one winning unambiguous
candidate or throws an error saying that the dispatch failed if there were no
candidates or that it was ambiguous if there were tied candidates.

=cut

*/

static PMC* find_best_candidate(PARROT_INTERP, Rakudo_md_candidate_info **candidates,
                                INTVAL num_candidates, PMC *capture, opcode_t *next) {
    Rakudo_md_candidate_info **cur_candidate    = candidates;
    Rakudo_md_candidate_info **possibles        = mem_allocate_n_typed(num_candidates, Rakudo_md_candidate_info *);
    PMC                       *junctional_res   = PMCNULL;
    const INTVAL               num_args         = VTABLE_elements(interp, capture);
    INTVAL                     possibles_count  = 0;
    INTVAL                     pure_type_result = 1;
    INTVAL                     type_check_count;
    INTVAL                     type_mismatch;
    
    /* Iterate over the candidates and collect best ones; terminate
     * when we see two nulls (may break out earlier). */
    while (1) {
        INTVAL i;

        if (*cur_candidate == NULL) {
            /* We've hit the end of a tied group now. If any of them have a
             * bindability check requirement, we'll do any of those now. */
            if (possibles_count) {
                Rakudo_md_candidate_info **new_possibles = NULL;
                INTVAL new_possibles_count = 0;
                INTVAL i;

                for (i = 0; i < possibles_count; i++) {
                    interp->current_cont = NEED_CONTINUATION;
                    Parrot_pcc_set_signature(interp, CURRENT_CONTEXT(interp), NULL);

                    /* First, if there's a required named parameter and it was
                     * not passed, we can very quickly eliminate this candidate
                     * without doing a full bindability check. */
                    if (possibles[i]->req_named) {
                        if (!VTABLE_exists_keyed_str(interp, capture, possibles[i]->req_named)) {
                            /* Required named arg not passed, so we eliminate
                             * it right here. Flag that we've built a list of
                             * new possibles, and that this was not a pure
                             * type-based result that we can cache. */

                            if (!new_possibles)
                                new_possibles = mem_allocate_n_typed(num_candidates, Rakudo_md_candidate_info *);
                            pure_type_result = 0;
                            continue;
                        }
                    }

                    /* Otherwise, may need full bind check. */
                    if (possibles[i]->bind_check) {
                        /* We'll invoke the sub (but not re-enter the runloop)
                         * and then attempt to bind the signature. */
                        /*opcode_t *where  = VTABLE_invoke(interp, possibles[i]->sub, next);
                        PMC      *lexpad = Parrot_pcc_get_lex_pad(interp, CURRENT_CONTEXT(interp));
                        PMC      *sig    = possibles[i]->signature;
                        INTVAL bind_check_result = Rakudo_binding_bind(interp, lexpad,
                              sig, capture, 1, NULL);
                        */
                        /* XXX In the future, we can actually keep the context if we only
                         * need one candidate, and then hand back the current PC and mark
                         * the context as not needing a bind. Just needs some code re-org.
                         * For now, we always clean up the ret-cont again. */
                        /*where = VTABLE_invoke(interp, Parrot_pcc_get_continuation(interp, CURRENT_CONTEXT(interp)), where);*/
                        
                        /* XXX Review the above to see if it's really needed. For now, we
                         * can try the following. */
                        PMC *sig      = possibles[i]->signature;
                        PMC *fake_pad = pmc_new(interp, enum_class_Hash);
                        INTVAL bind_check_result = Rakudo_binding_bind(interp, fake_pad,
                              sig, capture, 1, NULL);

                        /* If we haven't got a possibles storage space, allocate it now. */
                        if (!new_possibles)
                            new_possibles = mem_allocate_n_typed(num_candidates, Rakudo_md_candidate_info *);

                        /* If we don't fail, need to put this one onto the list
                         * (note that needing a junction dispatch is OK). */
                        if (bind_check_result != BIND_RESULT_FAIL) {
                            new_possibles[new_possibles_count] = possibles[i];
                            new_possibles_count++;
                        }

                        /* Since we had to do a bindability check, this is not
                         * a result we can cache on nominal type. */
                        pure_type_result = 0;
                    }
                }

                /* If we have an updated list of possibles, free old one and use this
                 * new one from here on in. */
                if (new_possibles) {
                    mem_sys_free(possibles);
                    possibles = new_possibles;
                    possibles_count = new_possibles_count;
                }
            }

            /* Now we have eliminated any that fail the bindability check. If
             * we've a result, we can stop. */
            if (possibles_count)
                break;
            
            /* Otherwise, we keep looping and looking, unless we really hit the end. */
            if (cur_candidate[1]) {
                cur_candidate++;
                continue;
            }
            else {
                break;
            }
        }

        /* Check if it's admissable by arity. */
        if (num_args < (*cur_candidate)->min_arity
        ||  num_args > (*cur_candidate)->max_arity) {
            cur_candidate++;
            continue;
        }

        /* Check if it's admissable by type. */
        type_check_count = (*cur_candidate)->num_types > num_args ?
                           num_args :
                           (*cur_candidate)->num_types;
        type_mismatch = 0;

        for (i = 0; i < type_check_count; i++) {
            PMC * const param        = Rakudo_cont_decontainerize(interp,
                VTABLE_get_pmc_keyed_int(interp, capture, i));
            PMC * const type_obj     = (*cur_candidate)->types[i];
            if (type_obj != Rakudo_types_mu_get() &&
                    !STABLE(param)->type_check(interp, param, type_obj)) {
                type_mismatch = 1;
                break;
            }
            else if ((*cur_candidate)->definednesses[i]) {
                INTVAL defined = REPR(param)->defined(interp, param);
                INTVAL desired = (*cur_candidate)->definednesses[i];
                if ((defined && desired == DEFCON_UNDEFINED) ||
                        (!defined && desired == DEFCON_DEFINED)) {
                    type_mismatch = 1;
                    break;
                }
            }
        }

        if (type_mismatch) {
            cur_candidate++;
            continue;
        }

        /* If we get here, it's an admissable candidate; add to list. */
        possibles[possibles_count] = *cur_candidate;
        possibles_count++;
        cur_candidate++;
    }

    /* Check is default trait if we still have multiple options and we want one. */
    if (possibles_count > 1) {
        /* Locate any default candidates; if we find multiple defaults, this is
         * no help, so we'll not bother collection just which ones are good. */
        Rakudo_md_candidate_info *default_cand = NULL;
        INTVAL i;

        for (i = 0; i < possibles_count; i++) {
            PMC * const default_meth = VTABLE_find_method(interp, possibles[i]->sub,
                    Parrot_str_new(interp, "default", 0));
            if (!PMC_IS_NULL(default_meth)) {
                PMC *result = PMCNULL;;
                Parrot_ext_call(interp, default_meth, "Pi->P", possibles[i]->sub, &result);
                if (VTABLE_get_bool(interp, result)) {
                    if (default_cand == NULL) {
                        default_cand = possibles[i];
                    }
                    else {
                        default_cand = NULL;
                        break;
                    }
                }
            }
        }
        if (default_cand) {
            possibles[0] = default_cand;
            possibles_count = 1;
        }
    }

    /* If we're at a single candidate here, and we also know there's no
     * type constraints that follow, we can cache the result. */
    if (possibles_count == 1 && pure_type_result) {
        /* XXX TODO: Cache! */
    }

    /* Perhaps we found nothing but have junctional arguments? */
    if (possibles_count == 0 && has_junctional_args(interp, capture)) {
        /* Look up multi junction dispatcher, clone it, attach this multi-sub
         * as a property and hand that back as the dispatch result. We also
         * stick it in the MMD cache for next time around. */
        /* XXX TODO... */
        junctional_res = PMCNULL;
    }

    /* Need a unique candidate. */
    if (possibles_count == 1) {
        PMC *result = possibles[0]->sub;
        mem_sys_free(possibles);
        return result;
    }
    else if (!PMC_IS_NULL(junctional_res)) {
        mem_sys_free(possibles);
        return junctional_res;
    }
    else if (possibles_count == 0) {
        /* Get signatures of all possible candidates. We dump them in the
         * order in which we search for them. */
        STRING *signatures = Parrot_str_new(interp, "", 0);
        cur_candidate = candidates;
        while (1) {
            if (!cur_candidate[0] && !cur_candidate[1])
                break;
            if (cur_candidate[0])
                signatures = dump_signature(interp, signatures, (*cur_candidate)->sub);
            cur_candidate++;
        }

        mem_sys_free(possibles);
        Parrot_ex_throw_from_c_args(interp, next, 1,
            "No applicable candidates found to dispatch to for '%Ss'. Available candidates are:\n%Ss",
                VTABLE_get_string(interp, candidates[0]->sub),
                signatures);
    }
    else {
        /* Get signatures of ambiguous candidates. */
        STRING *signatures = Parrot_str_new(interp, "", 0);
        INTVAL i;
        for (i = 0; i < possibles_count; i++)
            signatures = dump_signature(interp, signatures, possibles[i]->sub);
        
        mem_sys_free(possibles);
        Parrot_ex_throw_from_c_args(interp, next, 1,
            "Ambiguous dispatch to multi '%Ss'. Ambiguous candidates had signatures:\n%Ss",
                VTABLE_get_string(interp, candidates[0]->sub), signatures);
    }
}


/*

=item C<PMC * Rakudo_md_dispatch(PARROT_INTERP, PMC *dispatcher, opcode_t *next)>

Gets the candidate list, does sorting if we didn't already do so, and
enters the multi dispatcher.

=cut

*/
PMC *
Rakudo_md_dispatch(PARROT_INTERP, PMC *dispatcher, PMC *capture, opcode_t *next) {
    /* XXX Need to just sort once and cache...and have the
     * actual multi-dispatch cache here too. */
    Rakudo_Code *code_obj   = (Rakudo_Code *)PMC_data(dispatcher);
    PMC         *candidates = code_obj->dispatchees;
    INTVAL       num_cands  = VTABLE_elements(interp, candidates);
    return find_best_candidate(interp, sort_candidates(interp, candidates),
        num_cands, capture, next);
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
