#ifndef STORAGE_SPEC_H_GUARD
#define STORAGE_SPEC_H_GUARD

/* This data structure describes what storage a given representation
 * needs if something of that representation is to be embedded in
 * another place. For any representation that expects to be used
 * as a kind of reference type, it will just want to be a pointer.
 * But for other things, they would prefer to be "inlined" into
 * the object. */
typedef struct {
    /* 0 if this is to be referenced, anything else otherwise. */
    INTVAL inlineable;

    /* For things that want to be inlined, the number of bits of
     * storage they need and what kind of byte-boundary they want to
     * be aligned to. Ignored otherwise. */
    INTVAL bits;
    INTVAL align;

    /* For things that are inlined, if they are just storage of a
     * primitive type and can unbox, this says what primitive type
     * that they unbox to. */
    INTVAL boxed_primitive;
    
    /* The types that this one can box/unbox to. */
    INTVAL can_box;
    
    /* For ints, whether it's an usigned value. */
    INTVAL is_unsigned;
} storage_spec;

/* Inlined or not. */
#define STORAGE_SPEC_REFERENCE  0
#define STORAGE_SPEC_INLINED    1

/* Possible options for boxed primitives. */
#define STORAGE_SPEC_BP_NONE    0
#define STORAGE_SPEC_BP_INT     1
#define STORAGE_SPEC_BP_NUM     2
#define STORAGE_SPEC_BP_STR     3

/* can_box bit field values. */
#define STORAGE_SPEC_CAN_BOX_INT     1
#define STORAGE_SPEC_CAN_BOX_NUM     2
#define STORAGE_SPEC_CAN_BOX_STR     4

#endif
