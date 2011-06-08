/* This collects together cached versions of various interesting
 * types that we often want to box to, as well as mapping Parroty
 * types into Perl 6 ones. */
 
#define PARROT_IN_EXTENSION
#include "parrot/parrot.h"
#include "sixmodelobject.h"
#include "types.h"

static PMC * Mu        = NULL;
static PMC * Junction  = NULL;
static PMC * Int       = NULL;
static PMC * Num       = NULL;
static PMC * Str       = NULL;
static PMC * BoolFalse = NULL;
static PMC * BoolTrue  = NULL;

void Rakudo_types_mu_set(PMC * type) { Mu = type; }
PMC * Rakudo_types_mu_get(void) { return Mu; }

void Rakudo_types_junction_set(PMC * type) { Junction = type; }
PMC * Rakudo_types_junction_get(void) { return Junction; }

void Rakudo_types_int_set(PMC * type) { Int = type; }
PMC * Rakudo_types_int_get(void) { return Int; }

void Rakudo_types_num_set(PMC * type) { Num = type; }
PMC * Rakudo_types_num_get(void) { return Num; }

void Rakudo_types_str_set(PMC * type) { Str = type; }
PMC * Rakudo_types_str_get(void) { return Str; }

void Rakudo_types_bool_false_set(PMC * type) { BoolFalse = type; }
PMC * Rakudo_types_bool_false_get(void) { return BoolFalse; }

void Rakudo_types_bool_true_set(PMC * type) { BoolTrue = type; }
PMC * Rakudo_types_bool_true_get(void) { return BoolTrue; }

PMC * Rakudo_types_parrot_map(PARROT_INTERP, PMC * to_map) {
    PMC *result;
    switch (to_map->vtable->base_type) {
        case enum_class_String:
            result = REPR(Str)->instance_of(interp, Str);
            REPR(result)->set_str(interp, result, VTABLE_get_string(interp, to_map));
            break;
        case enum_class_Integer:
            result = REPR(Int)->instance_of(interp, Int);
            REPR(result)->set_int(interp, result, VTABLE_get_integer(interp, to_map));
            break;
        case enum_class_Float:
            result = REPR(Num)->instance_of(interp, Num);
            REPR(result)->set_num(interp, result, VTABLE_get_number(interp, to_map));
            break;
        default:
            result = to_map;
    }
    return result;
}
