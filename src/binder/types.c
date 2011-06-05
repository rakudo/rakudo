/* This collects together cached versions of various interesting
 * types that we often want to box to, as well as mapping Parroty
 * types into Perl 6 ones. */
 
#define PARROT_IN_EXTENSION
#include "parrot/parrot.h"

static PMC *Mu        = NULL;
static PMC *Junction  = NULL;
static PMC *Int       = NULL;
static PMC *Num       = NULL;
static PMC *Str       = NULL;
static PMC *BoolFalse = NULL;
static PMC *BoolTrue  = NULL;

void Rakudo_types_mu_set(PMC *type) { Mu = type; }
PMC* Rakduo_types_mu_get() { return Mu; }

void Rakudo_types_junction_set(PMC *type) { Junction = type; }
PMC* Rakduo_types_junction_get() { return Junction; }

void Rakudo_types_int_set(PMC *type) { Int = type; }
PMC* Rakduo_types_int_get() { return Int; }

void Rakudo_types_num_set(PMC *type) { Num = type; }
PMC* Rakduo_types_num_get() { return Num; }

void Rakudo_types_str_set(PMC *type) { Str = type; }
PMC* Rakduo_types_str_get() { return Str; }

void Rakudo_types_bool_false_set(PMC *type) { BoolFalse = type; }
PMC* Rakduo_types_bool_false_get() { return BoolFalse; }

void Rakudo_types_bool_true_set(PMC *type) { BoolTrue = type; }
PMC* Rakduo_types_bool_true_get() { return BoolTrue; }
