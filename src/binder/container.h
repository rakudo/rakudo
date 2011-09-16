/* The ContainerDescriptor class. Depends on P6opaque object layout. */
typedef struct {
    PMC    *st;                 /* S-table, though we don't care about that here. */
    PMC    *sc;                 /* Serialization context, though we don't care about that here. */
    PMC    *of;                 /* Type of value. */
    INTVAL  rw;                 /* Non-zero if we can write. */
    STRING *name;               /* The name of the container, if any. */
} Rakudo_ContainerDescriptor;

/* The Scalar class. Depends on P6opaque object layout. */
typedef struct {
    PMC    *st;                 /* S-table, though we don't care about that here. */
    PMC    *sc;                 /* Serialization context, though we don't care about that here. */
    PMC    *descriptor;         /* Container descriptor. */
    PMC    *value;              /* The currently held value. */
    PMC    *whence;             /* Any whence property */
} Rakudo_Scalar;

/* Various functions related to container manipulations. */
void Rakudo_cont_set_scalar_type(PMC *type);
PMC * Rakudo_cont_decontainerize(PARROT_INTERP, PMC *var);
void Rakudo_cont_store(PARROT_INTERP, PMC *cont, PMC *value, INTVAL type_check, INTVAL rw_check);
PMC * Rakudo_cont_scalar_from_descriptor(PARROT_INTERP, PMC *container_descriptor);
PMC * Rakudo_cont_scalar_with_value_no_descriptor(PARROT_INTERP, PMC *value);
INTVAL Rakudo_cont_is_rw_scalar(PARROT_INTERP, PMC *check);
PMC * Rakudo_create_container_descriptor(PARROT_INTERP, PMC *type, PMC *of, INTVAL rw, STRING *name);
