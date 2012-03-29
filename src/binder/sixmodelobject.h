/* This file contains various structure definitions for the 6model object
 * meta-model implementation. */

#ifndef SIXMODELOBJECT_H_GUARD
#define SIXMODELOBJECT_H_GUARD

#include "storage_spec.h"
#include "serialization.h"

/* The commonalities shared between all 6model objects, no matter what the
 * REPR is. This struct should be placed as the first thing in the object
 * struct used by all representations. */
typedef struct {
    PMC *stable;  /* The shared table. */
    PMC *sc;      /* Serialization context. */
} SixModelObjectCommonalities;

/* An example object, mostly used to compute the offset of the data part of
 * a 6model object. */
typedef struct {
    SixModelObjectCommonalities common;
    void *data;
} SixModelObjectStooge;

/* This is used to identify an attribute for various types of cache. */
typedef struct {
    PMC    *class_handle;   /* Class handle */
    STRING *attr_name;      /* Name of the attribute. */
    INTVAL  hint;           /* Hint for use in static/gradual typing. */
} AttributeIdentifier;

/* Language interop information that we hold if the type is declaring a
 * container of some sort. */
typedef struct {
    AttributeIdentifier  value_slot;
    PMC                 *fetch_method;
} ContainerSpec;

/* How do we turn something of this type into a boolean? */
typedef struct {
    INTVAL mode;
    PMC *  method;
} BoolificationSpec;

/* Boolification mode flags. */
#define BOOL_MODE_CALL_METHOD                   0
#define BOOL_MODE_UNBOX_INT                     1
#define BOOL_MODE_UNBOX_NUM                     2
#define BOOL_MODE_UNBOX_STR_NOT_EMPTY           3
#define BOOL_MODE_UNBOX_STR_NOT_EMPTY_OR_ZERO   4
#define BOOL_MODE_NOT_TYPE_OBJECT               5
#define BOOL_MODE_BIGINT                        6

/* Controls the way that type checks are performed. By default, if there is
 * a type check cache we treat it as definitive. However, it's possible to
 * declare that in the case the type check cache has no entry we should fall
 * back to asking the .HOW.type_check method (set TYPE_CHECK_CACHE_THEN_METHOD).
 * While a normal type check asks a value if it supports another type, the
 * TYPE_CHECK_NEEDS_ACCEPTS flag results in a call to .accepts_type on the
 * HOW of the thing we're checking the value against, giving it a chance to
 * decide answer. */
#define TYPE_CHECK_CACHE_DEFINITIVE    0
#define TYPE_CHECK_CACHE_THEN_METHOD   1
#define TYPE_CHECK_NEEDS_ACCEPTS       2
#define TYPE_CHECK_CACHE_FLAG_MASK     3

/* This flag is set if we consider the method cche authoritative. */
#define METHOD_CACHE_AUTHORITATIVE     4

/* S-Tables (short for Shared Table) contains the commonalities shared between
 * a (HOW, REPR) pairing (for example, (HOW for the class Dog, P6Opaque). */
typedef struct SixModel_REPROps REPROps;
struct SixModel_STable {
    /* The representation operation table. */
    REPROps *REPR;
    
    /* Any data specific to this type that the REPR wants to keep. */
    void *REPR_data;

    /* The meta-object. */
    PMC *HOW;

    /* The type-object. */
    PMC *WHAT;

    /* The method finder. */
    PMC * (*find_method) (PARROT_INTERP, PMC *obj, STRING *name, INTVAL hint);

    /* By-name method dispatch cache. */
    PMC *method_cache;

    /* The computed v-table for static dispatch. */
    PMC **vtable;

    /* The length of the v-table. */
    INTVAL vtable_length;

    /* The type checker. */
    INTVAL (*type_check) (PARROT_INTERP, PMC *obj, PMC *checkee);

    /* Array of type objects. If this is set, then it is expected to contain
     * the type objects of all types that this type is equivalent to (e.g.
     * all the things it isa and all the things it does). */
    PMC **type_check_cache;

    /* The length of the type check cache. */
    INTVAL type_check_cache_length;
    
    /* The type checking mode and method cache mode (see flags for this
     * above). */
    INTVAL mode_flags;

    /* An ID solely for use in caches that last a VM instance. Thus it
     * should never, ever be serialized and you should NEVER make a
     * type directory based upon this ID. Otherwise you'll create memory
     * leaks for anonymous types, and other such screwups. */
    INTVAL type_cache_id;
    
    /* If this is a container, then this contains information needed in
     * order to fetch the value in it. If not, it'll be null, which can
     * be taken as a "not a container" indication. */
    ContainerSpec *container_spec;
    
    /* Information - if any - about how we can turn something of this type
     * into a boolean. */
    BoolificationSpec *boolification_spec;
    
    /* The underlying package stash. */
    PMC *WHO;
    
    /* Serialization context that this s-table belongs to. */
    PMC *sc;

    /* Parrot-specific set of v-table to method mappings, for overriding
     * of Parrot v-table functions. */
    PMC **parrot_vtable_mapping;

	/* Parrot-specific set of v-table to object method mappings. */
	AttributeIdentifier *parrot_vtable_handler_mapping;
    
    /* The PMC that wraps this s-table. */
    PMC *stable_pmc;
};

/* A representation is what controls the layout of an object and access and
 * manipulation of the memory it manages. This includes attribute storage
 * for objects in the usual OO sense. However, representations may also
 * represent other things, such as pointers and arrays.
 *
 * The following structs define the representation API. There are some
 * things that all representations have. There are some others that a
 * representation will only implement if it plays that "role".*/
typedef struct SixModel_REPROps_Attribute {
    /* Gets the current value for an object attribute. For non-flattened
     * objects - that is, reference types - this just returns the object
     * stored in the attribute. For the flattened case, this will auto-box. */
    PMC * (*get_attribute_boxed) (PARROT_INTERP, STable *st, void *data,
        PMC *class_handle, STRING *name, INTVAL hint);

    /* Gets a reference to the memory location of an attribute. Note
     * that this is only valid so long as the object itself is alive. */
    void * (*get_attribute_ref) (PARROT_INTERP, STable *st, void *data,
        PMC *class_handle, STRING *name, INTVAL hint);

    /* Binds the given object value to the specified attribute. If it's
     * a reference type attribute, this just simply sets the value in 
     * place. If instead it's some other flattened in representation, then
     * the value should be a boxed form of the data to store.*/
    void (*bind_attribute_boxed) (PARROT_INTERP, STable *st, void *data,
        PMC *class_handle, STRING *name, INTVAL hint, PMC *value);

    /* Binds a flattened in attribute to the value at the passed reference.
     * Like with the get_attribute_ref function, presumably the thing calling
     * this knows about the type of the attribute it is supplying data for.
     * copy_to will be used to copy the data in to place. */
    void (*bind_attribute_ref) (PARROT_INTERP, STable *st, void *data,
        PMC *class_handle, STRING *name, INTVAL hint, void *value);

    /* Gets the hint for the given attribute ID. */
    INTVAL (*hint_for) (PARROT_INTERP, STable *st, PMC *class_handle, STRING *name);

    /* Checks if an attribute has been initialized. */
    INTVAL (*is_attribute_initialized) (PARROT_INTERP, STable *st, void *data,
        PMC *class_handle, STRING *name, INTVAL hint);
} REPROps_Attribute;
typedef struct SixModel_REPROps_Boxing {
    /* Used with boxing. Sets an integer value, for representations that
     * can hold one. */
    void (*set_int) (PARROT_INTERP, STable *st, void *data, INTVAL value);

    /* Used with boxing. Gets an integer value, for representations that
     * can hold one. */
    INTVAL (*get_int) (PARROT_INTERP, STable *st, void *data);

    /* Used with boxing. Sets a floating point value, for representations that
     * can hold one. */
    void (*set_num) (PARROT_INTERP, STable *st, void *data, FLOATVAL value);

    /* Used with boxing. Gets a floating point value, for representations that
     * can hold one. */
    FLOATVAL (*get_num) (PARROT_INTERP, STable *st, void *data);

    /* Used with boxing. Sets a string value, for representations that
     * can hold one. */
    void (*set_str) (PARROT_INTERP, STable *st, void *data, STRING *value);

    /* Used with boxing. Gets a string value, for representations that
     * can hold one. */
    STRING * (*get_str) (PARROT_INTERP, STable *st, void *data);

    /* Some objects serve primarily as boxes of others, inlining them. This gets
     * gets the reference to such things, using the representation ID to distinguish
     * them. */
    void * (*get_boxed_ref) (PARROT_INTERP, STable *st, void *data, INTVAL repr_id);
} REPROps_Boxing;
typedef struct SixModel_REPROps_Indexing {
    /* Get the address of the element at the specified position. May return null if
     * nothing is there, or throw to indicate out of bounds, or vivify. */
    void * (*at_pos_ref) (PARROT_INTERP, STable *st, void *data, INTVAL index);

    /* Get a boxed object representing the element at the specified position. If the
     * object is already a reference type, simply returns that. */
    PMC * (*at_pos_boxed) (PARROT_INTERP, STable *st, void *data, INTVAL index);

    /* Binds the value at the specified address into the array at the specified index.
     * may auto-vivify or throw. */
    void (*bind_pos_ref) (PARROT_INTERP, STable *st, void *data, INTVAL index, void *addr);

    /* Binds the object at the specified address into the array at the specified index.
     * For arrays of non-reference types, expects a compatible type. */
    void (*bind_pos_boxed) (PARROT_INTERP, STable *st, void *data, INTVAL index, PMC *obj);

    /* Gets the number of elements. */
    INTVAL (*elems) (PARROT_INTERP, STable *st, void *data);

    /* Pre-allocates the specified number of slots. */
    void (*preallocate) (PARROT_INTERP, STable *st, void *data, INTVAL count);

    /* Trim to the specified number of slots. */
    void (*trim_to) (PARROT_INTERP, STable *st, void *data, INTVAL count);

    /* Make a "hole" the specified number of elements in size at the specified index.
     * Used for implementing things like unshift, splice, etc. */
    void (*make_hole) (PARROT_INTERP, STable *st, void *data, INTVAL at_index, INTVAL count);

    /* Delete the specified number of elements (that is, actually shuffle the ones
     * after them into their place). Used for implementing things like shift, splice,
     * etc. */
    void (*delete_elems) (PARROT_INTERP, STable *st, void *data, INTVAL at_index, INTVAL count);

    /* Gets the STable representing the declared element type. */
    STable * (*get_elem_stable) (PARROT_INTERP, STable *st);
} REPROps_Indexing;
struct SixModel_REPROps {
    /* Creates a new type object of this representation, and
     * associates it with the given HOW. Also sets up a new
     * representation instance if needed. */
    PMC * (*type_object_for) (PARROT_INTERP, PMC *HOW);

    /* Allocates a new, but uninitialized object, based on the
     * specified s-table. */
    PMC * (*allocate) (PARROT_INTERP, STable *st);

    /* Used to initialize the body of an object representing the type
     * describe by the specified s-table. DATA points to the body. It
     * may recursively call initialize for any flattened objects. */
    void (*initialize) (PARROT_INTERP, STable *st, void *data);
    
    /* For the given type, copies the object data from the source memory
     * location to the destination one. Note that it may actually be more
     * involved than a straightforward bit of copying; what's important is
     * that the representation knows about that. Note that it may have to
     * call copy_to recursively on representations of any flattened objects
     * within its body. */
    void (*copy_to) (PARROT_INTERP, STable *st, void *src, void *dest);

    /* Attribute access REPR function table. */
    struct SixModel_REPROps_Attribute *attr_funcs;
    
    /* Boxing REPR function table. */
    struct SixModel_REPROps_Boxing *box_funcs;

    /* Indexing REPR function table. */
    struct SixModel_REPROps_Indexing *idx_funcs;
    
    /* Gets the storage specification for this representation. */
    storage_spec (*get_storage_spec) (PARROT_INTERP, STable *st);
    
    /* Handles an object changing its type. The representation is responsible
     * for doing any changes to the underlying data structure, and may reject
     * changes that it's not willing to do (for example, a representation may
     * choose to only handle switching to a subclass). It is also left to update
     * the S-Table pointer as needed; while in theory this could be factored
     * out, the representation probably knows more about timing issues and
     * thread safety requirements. */
    void (*change_type) (PARROT_INTERP, PMC *Object, PMC *NewType);
    
    /* Object serialization. Writes the objects body out using the passed
     * serialization writer. */
    void (*serialize) (PARROT_INTERP, STable *st, void *data, SerializationWriter *writer);
    
    /* Object deserialization. Reads the objects body in using the passed
     * serialization reader. */
    void (*deserialize) (PARROT_INTERP, STable *st, void *data, SerializationReader *reader);
    
    /* REPR data serialization. Seserializes the per-type representation data that
     * is attached to the supplied STable. */
    void (*serialize_repr_data) (PARROT_INTERP, STable *st, SerializationWriter *writer);
    
    /* REPR data deserialization. Deserializes the per-type representation data and
     * attaches it to the supplied STable. */
    void (*deserialize_repr_data) (PARROT_INTERP, STable *st, SerializationReader *reader);
    
    /* This Parrot-specific addition to the API is used to mark an object. */
    void (*gc_mark) (PARROT_INTERP, STable *st, void *data);

    /* This Parrot-specific addition to the API is used to free an object. */
    void (*gc_free) (PARROT_INTERP, PMC *object);

    /* This is called to do any cleanup of resources when an object gets
     * embedded inside another one. Never called on a top-level object. */
    void (*gc_cleanup) (PARROT_INTERP, STable *st, void *data);

    /* This Parrot-specific addition to the API is used to mark a REPR instance. */
    void (*gc_mark_repr_data) (PARROT_INTERP, STable *st);

    /* This Parrot-specific addition to the API is used to free a REPR instance. */
    void (*gc_free_repr_data) (PARROT_INTERP, STable *st);
    
    /* The representation's ID. */
    INTVAL ID;
    
    /* The representation's name. */
    STRING *name;
};

/* Hint value to indicate the absence of an attribute lookup or method
 * dispatch hint. */
#define NO_HINT -1

/* Various handy macros for getting at important stuff. */
#define STABLE_PMC(o)    (((SixModelObjectCommonalities *)PMC_data(o))->stable)
#define STABLE(o)        ((STable *)PMC_data(STABLE_PMC(o)))
#define SC_PMC(o)        (((SixModelObjectCommonalities *)PMC_data(o))->sc)
#define STABLE_STRUCT(p) ((STable *)PMC_data(p))
#define REPR(o)          (STABLE(o)->REPR)
#define OBJECT_BODY(o)   (&(((SixModelObjectStooge *)PMC_data(o))->data))

/* Macro for getting/setting type-objectness. */
#define IS_CONCRETE(o)         (!PObj_flag_TEST(private0, (o)))
#define MARK_AS_TYPE_OBJECT(o) PObj_flag_SET(private0, (o))

/* Object model initialization. */
void SixModelObject_initialize(PARROT_INTERP, PMC **knowhow, PMC **knowhow_attribute);

/* Some utility functions. */
void set_wrapping_object(PMC *wrapper);
PMC * wrap_object(PARROT_INTERP, void *obj);
PMC * create_stable(PARROT_INTERP, REPROps *REPR, PMC *HOW);
PMC * decontainerize(PARROT_INTERP, PMC *var);

/* Dynamic representation registration. */
typedef INTVAL (* rf) (PARROT_INTERP, STRING *name, REPROps * (*reg) (PARROT_INTERP, void *, void *));
#define REGISTER_DYNAMIC_REPR(interp, name, reg_func) \
    ((rf) \
        VTABLE_get_pointer(interp, \
            VTABLE_get_pmc_keyed_str(interp, interp->root_namespace, \
                Parrot_str_new_constant(interp, "_REGISTER_REPR"))))(interp, name, reg_func)

#endif
