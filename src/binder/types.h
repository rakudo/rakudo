#ifndef TYPES_H_GUARD
#define TYPES_H_GUARD

void Rakudo_types_mu_set(PMC * type);
PMC * Rakudo_types_mu_get(void);

void Rakudo_types_junction_set(PMC * type);
PMC * Rakudo_types_junction_get(void);

void Rakudo_types_int_set(PMC * type);
PMC * Rakudo_types_int_get(void);

void Rakudo_types_num_set(PMC * type);
PMC * Rakudo_types_num_get(void);

void Rakudo_types_str_set(PMC * type);
PMC * Rakudo_types_str_get(void);

void Rakudo_types_bool_false_set(PMC * type);
PMC * Rakudo_types_bool_false_get(void);

void Rakudo_types_bool_true_set(PMC * type);
PMC * Rakudo_types_bool_true_get(void);

PMC * Rakudo_types_parrot_map(PARROT_INTERP, PMC * to_map);

#endif
