/* Flags we have on types. */
#define DEFCON_NONE      0
#define DEFCON_DEFINED   1
#define DEFCON_UNDEFINED 2
#define DEFCON_MASK      (DEFCON_DEFINED | DEFCON_UNDEFINED)
#define TYPE_NATIVE_INT  4
#define TYPE_NATIVE_NUM  8
#define TYPE_NATIVE_STR  16
#define TYPE_NATIVE_MASK (TYPE_NATIVE_INT | TYPE_NATIVE_NUM | TYPE_NATIVE_STR)

/* Compile time dispatch result. */
#define MD_CT_NOT_SURE  0   /* Needs a runtime dispatch. */
#define MD_CT_DECIDED   1   /* Worked it out; see result. */
#define MD_CT_NO_WAY    -1  /* Proved it'd never manage to dispatch. */

/* This is how a Code looks on the inside. Once again, C struct that should
 * match what P6opaque computes for the Code class. */
typedef struct {
    PMC    *st;                 /* S-table, though we don't care about that here. */
    PMC    *sc;                 /* Serialization context, though we don't care about that here. */
    PMC    *_do;                /* Lower-level code object. */
    PMC    *signature;          /* Signature object. */
    PMC    *state_vars;         /* Storage for state variables. */
    PMC    *phasers;            /* Hash mapping phaser names to lists of phasers. */
    PMC    *dispatchees;        /* List of dispatchees, if any. */
    PMC    *dispatcher_cache;   /* Holder for any dispatcher cache. */
    PMC    *dispatcher;         /* The parent dispatcher, if any. */
    PMC    *md_thunk;           /* Multi-dispatcher thunk. */
    INTVAL  rw;                 /* Is it rw? */
} Rakudo_Code;

/* Represents a candidate. We extract various bits of information about it when
 * we are building the sorted candidate list and store them in here for fast
 * access during a dispatch. */
typedef struct {
    PMC    *sub;           /* The sub that is the candidate. */
    PMC    *signature;     /* The signature of the sub. */
    PMC   **types;         /* Class or role type constraints for each parameter. */
    INTVAL *type_flags;    /* Definedness and native flags for each of the types. */
    PMC   **constraints;   /* Refinement type constraints for each parameter. */
    INTVAL  num_types;     /* Number of entries in the above two arrays. */
    INTVAL  min_arity;     /* Number of required positional arguments. */
    INTVAL  max_arity;     /* Number of required and optional positional arguments. */
    INTVAL  bind_check;    /* A true value if any parameters have constraints and/or are named. */
    STRING *req_named;     /* Name of one required named argument, if any. This is to allow us
                            * to quickly rule out candidates disambiguated by a required named
                            * argument, as is the common case for traits. */
} Rakudo_md_candidate_info;

/* Maximum positional arity we cache up to. (Good to make it a
 * power of 2.) */
#define MD_CACHE_MAX_ARITY 4

/* Maximum entries we cache per arity. (Good to make it a
 * power of 2.) */
#define MD_CACHE_MAX_ENTRIES 16

/* The cached info that we keep per arity. */
typedef struct {
    /* The number of entries in the cache. */
    INTVAL num_entries;

    /* This is a bunch of type IDs. We allocate it arity * MAX_ENTRIES
     * big and go through it in arity sized chunks. */
    INTVAL *type_ids;

    /* The results we return from the cache. */
    PMC **results;
} Rakudo_md_arity_cache;

/* Multi-dispatcher cache info, which we will hang off the dispatcher
 * cache slot in a dispatcher sub. */
typedef struct {
    /* The sorted candidate list. */
    Rakudo_md_candidate_info **candidates;

    /* The fast, per-arity cache. */
    Rakudo_md_arity_cache arity_caches[MD_CACHE_MAX_ARITY];

    /* Zero-arity cached result. */
    PMC *zero_arity;
} Rakudo_md_cache;

/* Represents the produced information about a candidate as well as the graph
 * edges originating from it. The edges array contains pointers to the edges
 * in the graph that we have arrows to. */
typedef struct candidate_graph_node {
    Rakudo_md_candidate_info     *info;
    struct candidate_graph_node **edges;
    INTVAL                        edges_in;
    INTVAL                        edges_out;
} Rakudo_md_candidate_graph_node;

/* This is the entry point to the multi dispatcher, which chooses a
 * candidate to invoke. */
PMC *
Rakudo_md_dispatch(PARROT_INTERP, PMC *dispatcher, PMC *capture, opcode_t *next);

/* This gets all matches for a given capture. */
PMC *
Rakudo_md_get_all_matches(PARROT_INTERP, PMC *dispatcher, PMC *capture);

/* Tries to resolve a multi-dispatch at compile time. Returns a flag
 * and, if a dispatch is possible, sets the result. */
INTVAL
Rakudo_md_ct_dispatch(PARROT_INTERP, PMC *dispatcher, PMC *capture, PMC **result);
