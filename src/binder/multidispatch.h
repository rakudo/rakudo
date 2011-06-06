#define DEFCON_NONE      0
#define DEFCON_DEFINED   1
#define DEFCON_UNDEFINED 2

/* This is how a Code looks on the inside. Once again, C struct that should
 * match what P6opaque computes for the Code class. */
typedef struct {
    PMC    *st;                 /* S-table, though we don't care about that here. */
    PMC    *sc;                 /* Serialization context, though we don't care about that here. */
    PMC    *spill;              /* Attribute spill storage. */
    PMC    *_do;                /* Lower-level code object. */
    PMC    *signature;          /* Signature object. */
    PMC    *dispatchees;        /* List of dispatchees, if any. */
    PMC    *dispatcher_info;    /* Holder for any extra dispatcher info. */
} Rakudo_Code;

/* Represents a candidate. We extract various bits of information about it when
 * we are building the sorted candidate list and store them in here for fast
 * access during a dispatch. */
typedef struct {
    PMC    *sub;           /* The sub that is the candidate. */
    PMC    *signature;     /* The signature of the sub. */
    PMC   **types;         /* Class or role type constraints for each parameter. */
    INTVAL *definednesses; /* Definedness flags for each of the types. */
    PMC   **constraints;   /* Refinement type constraints for each parameter. */
    INTVAL  num_types;     /* Number of entries in the above two arrays. */
    INTVAL  min_arity;     /* Number of required positional arguments. */
    INTVAL  max_arity;     /* Number of required and optional positional arguments. */
    INTVAL  bind_check;    /* A true value if any parameters have constraints and/or are named. */
    STRING *req_named;     /* Name of one required named argument, if any. This is to allow us
                            * to quickly rule out candidates disambiguated by a required named
                            * argument, as is the common case for traits. */
} Rakudo_md_candidate_info;

/* Overall multi-dispatcher info, which we will hang off the dispatcher
 * info slot in a dispatcher sub. */
typedef struct {
    Rakudo_md_candidate_info **candidates;
    /* XXX TODO: Cache goes here also. */
} Rakudo_md_info;

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
