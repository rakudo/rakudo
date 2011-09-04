#ifndef TYPES_H_GUARD
#define TYPES_H_GUARD

void Rakudo_types_mu_set(PMC * type);
PMC * Rakudo_types_mu_get(void);

void Rakudo_types_any_set(PMC * type);
PMC * Rakudo_types_any_get(void);

void Rakudo_types_junction_set(PMC * type);
PMC * Rakudo_types_junction_get(void);

void Rakudo_types_int_set(PMC * type);
PMC * Rakudo_types_int_get(void);

void Rakudo_types_num_set(PMC * type);
PMC * Rakudo_types_num_get(void);

void Rakudo_types_str_set(PMC * type);
PMC * Rakudo_types_str_get(void);

void Rakudo_types_parcel_set(PMC * type);
PMC * Rakudo_types_parcel_get(void);

void Rakudo_types_list_set(PMC * type);
PMC * Rakudo_types_list_get(void);

void Rakudo_types_listiter_set(PMC * type);
PMC * Rakudo_types_listiter_get(void);

void Rakudo_types_nil_set(PMC * type);
PMC * Rakudo_types_nil_get(void);

void Rakudo_types_array_set(PMC * type);
PMC * Rakudo_types_array_get(void);

void Rakudo_types_lol_set(PMC * type);
PMC * Rakudo_types_lol_get(void);

void Rakudo_types_enummap_set(PMC * type);
PMC * Rakudo_types_enummap_get(void);

void Rakudo_types_hash_set(PMC * type);
PMC * Rakudo_types_hash_get(void);

void Rakudo_types_capture_set(PMC * type);
PMC * Rakudo_types_capture_get(void);

void Rakudo_types_code_set(PMC * type);
PMC * Rakudo_types_code_get(void);

void Rakudo_types_bool_false_set(PMC * type);
PMC * Rakudo_types_bool_false_get(void);

void Rakudo_types_bool_true_set(PMC * type);
PMC * Rakudo_types_bool_true_get(void);

void Rakudo_types_packagehow_set(PMC * type);
PMC * Rakudo_types_packagehow_get(void);

void Rakudo_types_junction_threader_set(PMC * threader);
PMC * Rakudo_types_junction_threader_get(void);

PMC * Rakudo_types_parrot_map(PARROT_INTERP, PMC * to_map);

#endif
