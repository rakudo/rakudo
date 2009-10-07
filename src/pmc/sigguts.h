/* Flags that can be set on a signature element. */
#define SIG_ELEM_BIND_CAPTURE      1
#define SIG_ELEM_BIND_PRIVATE_ATTR 2
#define SIG_ELEM_BIND_PUBLIC_ATTR  4
#define SIG_ELEM_SLURPY_POS        8
#define SIG_ELEM_SLURPY_NAMED      16
#define SIG_ELEM_SLURPY_BLOCK      32
#define SIG_ELEM_INVOCANT          64
#define SIG_ELEM_MULTI_INVOCANT    128
#define SIG_ELEM_IS_RW             256
#define SIG_ELEM_IS_COPY           512
#define SIG_ELEM_IS_REF            1024
#define SIG_ELEM_IS_OPTIONAL       2048


/* Data structure to describe a single element in the signature. */
typedef struct llsig_element {
    STRING *variable_name;    /* The name in the lexpad to bind to, if any. */
    PMC    *named_names;      /* List of the name(s) that a named parameter has. */
    PMC    *type_captures;    /* Name(s) that we bind the type of a parameter to. */
    INTVAL flags;             /* Various flags about the parameter. */
    PMC    *nominal_type;     /* The nominal type of the parameter. */
    PMC    *post_constraints; /* Junction of any extra constraints. */
    PMC    *sub_signature;    /* Any nested signature. */
} llsig_element;
