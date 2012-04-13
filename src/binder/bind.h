/* Flags that can be set on a signature element. */
#define SIG_ELEM_BIND_CAPTURE        1
#define SIG_ELEM_BIND_PRIVATE_ATTR   2
#define SIG_ELEM_BIND_PUBLIC_ATTR    4
#define SIG_ELEM_BIND_ATTRIBUTIVE    (SIG_ELEM_BIND_PRIVATE_ATTR | SIG_ELEM_BIND_PUBLIC_ATTR)
#define SIG_ELEM_SLURPY_POS          8
#define SIG_ELEM_SLURPY_NAMED        16
#define SIG_ELEM_SLURPY_LOL          32
#define SIG_ELEM_SLURPY              (SIG_ELEM_SLURPY_POS | SIG_ELEM_SLURPY_NAMED | SIG_ELEM_SLURPY_LOL)
#define SIG_ELEM_INVOCANT            64
#define SIG_ELEM_MULTI_INVOCANT      128
#define SIG_ELEM_IS_RW               256
#define SIG_ELEM_IS_COPY             512
#define SIG_ELEM_IS_PARCEL           1024
#define SIG_ELEM_IS_OPTIONAL         2048
#define SIG_ELEM_ARRAY_SIGIL         4096
#define SIG_ELEM_HASH_SIGIL          8192
#define SIG_ELEM_DEFAULT_FROM_OUTER  16384
#define SIG_ELEM_IS_CAPTURE          32768
#define SIG_ELEM_UNDEFINED_ONLY      65536
#define SIG_ELEM_DEFINED_ONLY        131072
#define SIG_ELEM_DEFINEDNES_CHECK    (SIG_ELEM_UNDEFINED_ONLY | SIG_ELEM_DEFINED_ONLY)
#define SIG_ELEM_METHOD_SLURPY_NAMED 262144
#define SIG_ELEM_NOMINAL_GENERIC     524288
#define SIG_ELEM_DEFAULT_IS_LITERAL  1048576
#define SIG_ELEM_NATIVE_INT_VALUE    2097152
#define SIG_ELEM_NATIVE_NUM_VALUE    4194304
#define SIG_ELEM_NATIVE_STR_VALUE    8388608
#define SIG_ELEM_NATIVE_VALUE        (SIG_ELEM_NATIVE_INT_VALUE | SIG_ELEM_NATIVE_NUM_VALUE | SIG_ELEM_NATIVE_STR_VALUE)

/* This is how a parameter looks on the inside. Actually, this is a C struct
 * that should match the computed object layout by P6opaque for the type
 * Parameter. So if changing that, this needs to be changed here. */
typedef struct {
    PMC    *st;                   /* S-table, though we don't care about that here. */
    PMC    *sc;                   /* Serialization context, though we don't care about that here. */
    STRING *variable_name;        /* The name in the lexpad to bind to, if any. */
    PMC    *named_names;          /* List of the name(s) that a named parameter has. */
    PMC    *type_captures;        /* Name(s) that we bind the type of a parameter to. */
    INTVAL flags;                 /* Various flags about the parameter. */
    PMC    *nominal_type;         /* The nominal type of the parameter. */
    PMC    *post_constraints;     /* Array of any extra constraints; we will do a
                                   * smart-match against each of them. For now, we
                                   * always expect an array of blocks. */
    PMC    *coerce_type;          /* The type to coerce the value to, if any. */
    STRING *coerce_method;        /* Name of the method to call to coerce; for X we do $val.X. */
    PMC    *sub_llsig;            /* Any nested signature. */
    PMC    *default_value;        /* The default value or a thunk producing it. */
    PMC    *container_descriptor; /* Descriptor for the container we bind into, if any. */
    PMC    *attr_package;         /* Package part of an attributive binding. */
} Rakudo_Parameter;

/* This is how a signature looks on the inside. Actually, this is a C struct
 * that should match the computed object layout by P6opaque for the type
 * Signature. So if changing that, this needs to be changed here. */
typedef struct {
    PMC    *st;                 /* S-table, though we don't care about that here. */
    PMC    *sc;                 /* Serialization context, though we don't care about that here. */
    PMC    *params;             /* Array of objects that are all parameters. */
    PMC    *rtype;              /* Return type. */
} Rakudo_Signature;

/* 
 * ALREADY_CHECKED can be flagged on a CallContext, and indicates that we have
 * determined that all of the arguments can be bound to positional parameters
 * without any further type checking (because the multi-dispatch told us so).
 */
#define PObj_P6BINDER_ALREADY_CHECKED_FLAG PObj_private0_FLAG

/* 
 * CHECKING_PRE flags that we are currently checking pre-conditions. If we
 * get an exception during this, it suppresses running of various other
 * phasers.
 */
#define PObj_P6_CHECKING_PRE_FLAG PObj_private0_FLAG

/* Flags that the block has state variables and that this is the first time
 * that we are visiting the block and so they need initializing. */
#define PObj_P6LEXPAD_STATE_INIT_FLAG PObj_private1_FLAG
#define PObj_SUB_FIRST_FLAG PObj_private7_FLAG

/* Gets the ID of a 6model object PMC. */
INTVAL Rakudo_smo_id(void);

/* Functions we want to share to provide the interface to the binder. */
INTVAL Rakudo_binding_bind(PARROT_INTERP, PMC *lexpad, PMC *sig_pmc,
                    PMC *capture, INTVAL no_nom_type_check,
                    STRING **error);
void Rakudo_binder_set_top_type(PMC *type);
PMC * Rakudo_binder_get_top_type(void);
void Rakudo_binder_set_junction_type(PMC *type);
PMC * Rakudo_binder_get_junction_type(void);
/* for perl6.ops */
PMC * Rakudo_binding_parcel_from_rpa(PARROT_INTERP, PMC *rpa, PMC *fill);
PMC * Rakudo_binding_iter_from_rpa(PARROT_INTERP, PMC *rpa, PMC *list);
PMC * Rakudo_binding_list_from_rpa(PARROT_INTERP, PMC *rpa, PMC *type, PMC *flat);

/* Things Rakudo_binding_bind_llsig may return to indicate a problem. */
#define BIND_RESULT_OK       0
#define BIND_RESULT_FAIL     1
#define BIND_RESULT_JUNCTION 2

/* The value we're going to bind. */
#define BIND_VAL_INT 1
#define BIND_VAL_NUM 2
#define BIND_VAL_STR 3
#define BIND_VAL_OBJ 4
typedef struct {    
    union {
        PMC      *o;
        INTVAL    i;
        FLOATVAL  n;
        STRING   *s;
    } val;
    char type;
} Rakudo_BindVal;

/* Nabbed from Parrot, since it's not exposed and it's the only way
 * (so far as I can tell) to get at the underlying primitive type
 * being passed. */
typedef struct Pcc_cell
{
    union u {
        PMC     *p;
        STRING  *s;
        INTVAL   i;
        FLOATVAL n;
    } u;
    INTVAL type;
} Pcc_cell;

/* Compile time trial binding function and result flags. */
#define TRIAL_BIND_NOT_SURE  0   /* Plausible, but need to check at runtime. */
#define TRIAL_BIND_OK        1   /* Bind will always work out. */
#define TRIAL_BIND_NO_WAY   -1   /* Bind could never work out. */
INTVAL Rakudo_binding_trial_bind(PARROT_INTERP, PMC *sig_pmc, PMC *capture);
