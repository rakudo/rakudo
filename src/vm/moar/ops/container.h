/* The Scalar class. Depends on P6opaque object layout. */
typedef struct {
    MVMP6opaque  p6o;
    MVMObject   *descriptor;        /* Container descriptor. */
    MVMObject   *value;             /* The currently held value. */
    MVMObject   *whence;            /* Any whence property */
} Rakudo_Scalar;

void Rakudo_containers_setup(MVMThreadContext *tc);
MVMContainerSpec * Rakudo_containers_get_scalar();
