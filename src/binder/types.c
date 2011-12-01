/* This collects together cached versions of various interesting
 * types that we often want to box to, as well as mapping Parroty
 * types into Perl 6 ones. */
 
#define PARROT_IN_EXTENSION
#include "parrot/parrot.h"
#include "sixmodelobject.h"
#include "types.h"
#include "bind.h"

static PMC * Mu         = NULL;
static PMC * Any        = NULL;
static PMC * Junction   = NULL;
static PMC * Int        = NULL;
static PMC * Num        = NULL;
static PMC * Str        = NULL;
static PMC * Parcel     = NULL;
static PMC * List       = NULL;
static PMC * ListIter   = NULL;
static PMC * Nil        = NULL;
static PMC * Array      = NULL;
static PMC * LoL        = NULL;
static PMC * EnumMap    = NULL;
static PMC * _Hash      = NULL;
static PMC * Capture    = NULL;
static PMC * Code       = NULL;
static PMC * BoolFalse  = NULL;
static PMC * BoolTrue   = NULL;
static PMC * PackageHOW = NULL;
static PMC * JunctionThreader = NULL;

void Rakudo_types_mu_set(PMC * type) { Mu = type; }
PMC * Rakudo_types_mu_get(void) { return Mu; }

void Rakudo_types_any_set(PMC * type) { Any = type; }
PMC * Rakudo_types_any_get(void) { return Any; }

void Rakudo_types_junction_set(PMC * type) { Junction = type; }
PMC * Rakudo_types_junction_get(void) { return Junction; }

void Rakudo_types_int_set(PMC * type) { Int = type; }
PMC * Rakudo_types_int_get(void) { return Int; }

void Rakudo_types_num_set(PMC * type) { Num = type; }
PMC * Rakudo_types_num_get(void) { return Num; }

void Rakudo_types_str_set(PMC * type) { Str = type; }
PMC * Rakudo_types_str_get(void) { return Str; }

void Rakudo_types_parcel_set(PMC * type) { Parcel = type; }
PMC * Rakudo_types_parcel_get(void) { return Parcel; }

void Rakudo_types_list_set(PMC * type) { List = type; }
PMC * Rakudo_types_list_get(void) { return List; }

void Rakudo_types_listiter_set(PMC * type) { ListIter = type; }
PMC * Rakudo_types_listiter_get(void) { return ListIter; }

void Rakudo_types_nil_set(PMC * type) { Nil = type; }
PMC * Rakudo_types_nil_get(void) { return Nil; }

void Rakudo_types_array_set(PMC * type) { Array = type; }
PMC * Rakudo_types_array_get(void) { return Array; }

void Rakudo_types_lol_set(PMC * type) { LoL = type; }
PMC * Rakudo_types_lol_get(void) { return LoL; }

void Rakudo_types_enummap_set(PMC * type) { EnumMap = type; }
PMC * Rakudo_types_enummap_get(void) { return EnumMap; }

void Rakudo_types_hash_set(PMC * type) { _Hash = type; }
PMC * Rakudo_types_hash_get(void) { return _Hash; }

void Rakudo_types_capture_set(PMC * type) { Capture = type; }
PMC * Rakudo_types_capture_get(void) { return Capture; }

void Rakudo_types_code_set(PMC * type) { Code = type; }
PMC * Rakudo_types_code_get(void) { return Code; }

void Rakudo_types_bool_false_set(PMC * type) { BoolFalse = type; }
PMC * Rakudo_types_bool_false_get(void) { return BoolFalse; }

void Rakudo_types_bool_true_set(PMC * type) { BoolTrue = type; }
PMC * Rakudo_types_bool_true_get(void) { return BoolTrue; }

void Rakudo_types_packagehow_set(PMC * type) { PackageHOW = type; }
PMC * Rakudo_types_packagehow_get(void) { return PackageHOW; }

void Rakudo_types_junction_threader_set(PMC * threader) { JunctionThreader = threader; }
PMC * Rakudo_types_junction_threader_get(void) { return JunctionThreader; }

PMC * Rakudo_types_parrot_map(PARROT_INTERP, PMC * to_map) {
    PMC *result;
    switch (to_map->vtable->base_type) {
        case enum_class_String:
            result = REPR(Str)->allocate(interp, STABLE(Str));
            REPR(result)->initialize(interp, STABLE(result), OBJECT_BODY(result));
            REPR(result)->box_funcs->set_str(interp, STABLE(result), OBJECT_BODY(result), VTABLE_get_string(interp, to_map));
            PARROT_GC_WRITE_BARRIER(interp, result);
            break;
        case enum_class_Integer:
            result = REPR(Int)->allocate(interp, STABLE(Int));
            REPR(result)->initialize(interp, STABLE(result), OBJECT_BODY(result));
            REPR(result)->box_funcs->set_int(interp, STABLE(result), OBJECT_BODY(result), VTABLE_get_integer(interp, to_map));
            break;
        case enum_class_Float:
            result = REPR(Num)->allocate(interp, STABLE(Num));
            REPR(result)->initialize(interp, STABLE(result), OBJECT_BODY(result));
            REPR(result)->box_funcs->set_num(interp, STABLE(result), OBJECT_BODY(result), VTABLE_get_number(interp, to_map));
            break;
        case enum_class_ResizablePMCArray:
            result = Rakudo_binding_parcel_from_rpa(interp, to_map, Mu);
            break;
        case enum_class_Hash:
            result = REPR(_Hash)->allocate(interp, STABLE(_Hash));
            REPR(result)->initialize(interp, STABLE(result), OBJECT_BODY(result));
            VTABLE_set_attr_keyed(interp, result, EnumMap, Parrot_str_new_constant(interp, "$!storage"), to_map);
            break;
        case enum_class_Null:
            result = Mu;
            break;
        default:
            result = to_map;
    }
    return result;
}
