#define PARROT_IN_EXTENSION
#include "parrot/parrot.h"
#include "parrot/extend.h"
#include "exceptions.h"
PMC * Rakudo_get_thrower(PARROT_INTERP, const char * name) {
        PMC * const hll_ns = Parrot_hll_get_ctx_HLL_namespace(interp);
        PMC * const ex_hash = Parrot_ns_find_namespace_global(interp,
                    hll_ns, Parrot_str_new_constant(interp, "P6EX"));
        PMC * const thrower = PMC_IS_NULL(ex_hash)
            ? PMCNULL
            : VTABLE_get_pmc_keyed_str(interp,
                    ex_hash, Parrot_str_new(interp, name, 0));
        return thrower;
}

