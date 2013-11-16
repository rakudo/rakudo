/* The ContainerDescriptor class. Depends on P6opaque object layout. */
typedef struct {
    MVMP6opaque  p6o;
    MVMObject   *of;                /* Type of value. */
    MVMint64     rw;                /* Non-zero if we can write. */
    MVMString   *name;              /* The name of the container, if any. */
    MVMObject   *the_default;       /* The default value if any. */
    MVMint64     is_dynamic;        /* The container is dynamically visible */
} Rakudo_ContainerDescriptor;

/* The Scalar class. Depends on P6opaque object layout. */
typedef struct {
    MVMP6opaque  p6o;
    MVMObject   *descriptor;        /* Container descriptor. */
    MVMObject   *value;             /* The currently held value. */
    MVMObject   *whence;            /* Any whence property */
} Rakudo_Scalar;

void Rakudo_containers_setup(MVMThreadContext *tc);
