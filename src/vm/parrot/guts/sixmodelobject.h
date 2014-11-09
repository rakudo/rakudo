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

/* Container specification information, for types that serve as containers.
 * A container is something that can be assigned into. It may be some kind
 * of container object (like Perl 6's Scalar) or it may be a reference to a
 * native lexical or object field. The function table determines the way it
 * behaves. */
typedef struct {
    /* Fetches a value out of a container. Used for decontainerization. */
    PMC * (*fetch) (PARROT_INTERP, PMC *cont);
    
    /* Stores a value in a container. Used for assignment. */
    void (*store) (PARROT_INTERP, PMC *cont, PMC *obj);
    
    /* Stores a value in a container, without any checking of it (this
     * assumes an optimizer or something else already did it). Used for
     * assignment. */
    void (*store_unchecked) (PARROT_INTERP, PMC *cont, PMC *obj);
    
    /* Name of this container specification. */
    STRING *name;
    
    /* Marks container data, if any. */
    void (*gc_mark_data) (PARROT_INTERP, STable *st);

    /* Frees container data, if any. */
    void (*gc_free_data) (PARROT_INTERP, STable *st);
    
    /* Serializes the container data, if any. */
    void (*serialize) (PARROT_INTERP, STable *st, SerializationWriter *writer);
    
    /* Deserializes the container data, if any. */
    void (*deserialize) (PARROT_INTERP, STable *st, SerializationReader *reader);
} ContainerSpec;

/* A container configurer knows how to attach a certain type of container
 * to an STable and configure it. */
typedef struct {
    /* Sets this container spec in place for the specified STable. */ 
    void (*set_container_spec) (PARROT_INTERP, STable *st);
    
    /* Configures the container spec with the specified info. */
    void (*configure_container_spec) (PARROT_INTERP, STable *st, PMC *config);
} ContainerConfigurer;

/* How do we invoke this thing? Specifies either an attribute to look at for
 * an invokable thing, or alternatively a method to call. */
typedef struct {
    AttributeIdentifier  value_slot;
    PMC                 *invocation_handler;
} InvocationSpec;

/* How do we turn something of this type into a boolean? */
typedef struct {
    INTVAL mode;
    PMC *  method;
} BoolificationSpec;

/* Defines and struct we use to access inlined members. */
#define NATIVE_VALUE_INT    1
#define NATIVE_VALUE_FLOAT  2
#define NATIVE_VALUE_STRING 3

typedef struct {
    union {
        INTVAL    intval;
        FLOATVAL  floatval;
        STRING   *stringval;
    } value;
    INTVAL type;
} NativeValue;

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

/* HLL type roles. */
#define HLL_ROLE_NONE       0
#define HLL_ROLE_INT        1
#define HLL_ROLE_NUM        2
#define HLL_ROLE_STR        3
#define HLL_ROLE_ARRAY      4
#define HLL_ROLE_HASH       5
#define HLL_ROLE_CODE       6

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
     * order to fetch the value in it or assign a value to it. If not,
     * it'll be null, which can be taken as a "not a container" indication. */
    ContainerSpec *container_spec;
    
    /* Data that the container spec may need to function. */
    /* Any data specific to this type that the REPR wants to keep. */
    void *container_data;
    
    /* If this is invokable, then this contains information needed to
     * figure out how to invoke it. If not, it'll be null. */
    InvocationSpec *invocation_spec;
    
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
    
    /* The HLL that this type is owned by, if any. */
    INTVAL hll_owner;
    
    /* The role that the type plays in the HLL, if any. */
    INTVAL hll_role;
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

    /* Fetch the value of the attribute into the value struct. The caller sets
     * the type field of value to the type requested, and it's the caller's
     * responsibility to make sure this is compatible with the stored
     * attribute. */
    void (*get_attribute_native) (PARROT_INTERP, STable *st, void *data,
        PMC *class_handle, STRING *name, INTVAL hint, NativeValue *value);

    /* Binds the given object value to the specified attribute. If it's
     * a reference type attribute, this just simply sets the value in 
     * place. If instead it's some other flattened in representation, then
     * the value should be a boxed form of the data to store.*/
    void (*bind_attribute_boxed) (PARROT_INTERP, STable *st, void *data,
        PMC *class_handle, STRING *name, INTVAL hint, PMC *value);

    /* Set the value of a flattened attribute. It is the caller's
     * responsibility to set value to a type compatible with the type of the
     * attribute being set. */
    void (*bind_attribute_native) (PARROT_INTERP, STable *st, void *data,
        PMC *class_handle, STRING *name, INTVAL hint, NativeValue *value);

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
typedef struct SixModel_REPROps_Positional {
    /* Get a flattened native value, of the type specified in value->type. It
     * is the caller's responsibility to make sure the stored data is of the
     * appropriate type. May throw to indicate out of bounds, or vivify. */
    void (*at_pos_native) (PARROT_INTERP, STable *st, void *data, INTVAL index, NativeValue *value);

    /* Get a boxed object representing the element at the specified position. If the
     * object is already a reference type, simply returns that. */
    PMC * (*at_pos_boxed) (PARROT_INTERP, STable *st, void *data, INTVAL index);

    /* Sets the value at the specified index of the array. May auto-vivify or throw. */
    void (*bind_pos_native) (PARROT_INTERP, STable *st, void *data, INTVAL index, NativeValue *value);

    /* Binds the object at the specified address into the array at the specified index.
     * For arrays of non-reference types, expects a compatible type. */
    void (*bind_pos_boxed) (PARROT_INTERP, STable *st, void *data, INTVAL index, PMC *obj);
    
    /* Pushes an object. */
    void (*push_boxed) (PARROT_INTERP, STable *st, void *data, PMC *obj);
    
    /* Pops an object. */
    PMC * (*pop_boxed) (PARROT_INTERP, STable *st, void *data);
    
    /* Unshifts an object. */
    void (*unshift_boxed) (PARROT_INTERP, STable *st, void *data, PMC *obj);
    
    /* Shifts an object. */
    PMC * (*shift_boxed) (PARROT_INTERP, STable *st, void *data);
    
    /* Gets the STable representing the declared element type. */
    STable * (*get_elem_stable) (PARROT_INTERP, STable *st);
} REPROps_Positional;
typedef struct SixModel_REPROps_Associative {
    /* Gets the value at the specified key. */
    PMC * (*at_key_boxed) (PARROT_INTERP, STable *st, void *data, STRING *key);
    
    /* Binds a value to the specified key. */
    void (*bind_key_boxed) (PARROT_INTERP, STable *st, void *data, STRING *key, PMC *value);
    
    /* Checks if the specified key exists. */
    INTVAL (*exists_key) (PARROT_INTERP, STable *st, void *data, STRING *key);
    
    /* Deletes the specified key. */
    void (*delete_key) (PARROT_INTERP, STable *st, void *data, STRING *key);
} REPROps_Associative;
struct SixModel_REPROps {
    /* Creates a new type object of this representation, and
     * associates it with the given HOW. Also sets up a new
     * representation instance if needed. */
    PMC * (*type_object_for) (PARROT_INTERP, PMC *HOW);

    /* Composes the representation, passing any composition info. This
     * is the way a meta-object provides configuration to a REPR, which
     * it may then use to do layout, etc. */
    void (*compose) (PARROT_INTERP, STable *st, PMC *repr_info);

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

    /* Positional REPR function table. */
    struct SixModel_REPROps_Positional *pos_funcs;

    /* Associative REPR function table. */
    struct SixModel_REPROps_Associative *ass_funcs;
    
    /* Gets the number of elements, if it's relevant. */
    INTVAL (*elems) (PARROT_INTERP, STable *st, void *data);
    
    /* Gets the storage specification for this representation. */
    void (*get_storage_spec) (PARROT_INTERP, STable *st, storage_spec *spec);
    
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

/* Macro for decontainerization. */
#define DECONT(interp, o) (IS_CONCRETE(o) && STABLE(o)->container_spec ? \
    STABLE(o)->container_spec->fetch(interp, o) : \
    o)

/* Write barriers for noticing changes to objects or STables with an SC. */
typedef void (* obj_sc_barrier_func) (PARROT_INTERP, PMC *obj);
typedef void (* st_sc_barrier_func) (PARROT_INTERP, STable *st);
#define OBJ_SC_WRITE_BARRIER(o) \
    if (SC_PMC(o)) { \
        ((obj_sc_barrier_func) \
        D2FPTR(VTABLE_get_pointer(interp, \
            VTABLE_get_pmc_keyed_str(interp, interp->root_namespace, \
                Parrot_str_new_constant(interp, "_OBJ_SC_BARRIER")))))(interp, o); \
    }
#define ST_SC_WRITE_BARRIER(st) \
    if ((st)->sc) { \
        ((st_sc_barrier_func) \
        D2FPTR(VTABLE_get_pointer(interp, \
            VTABLE_get_pmc_keyed_str(interp, interp->root_namespace, \
                Parrot_str_new_constant(interp, "_ST_SC_BARRIER")))))(interp, st); \
    }

/* Object model initialization. */
void SixModelObject_initialize(PARROT_INTERP, PMC **knowhow, PMC **knowhow_attribute);

/* Some utility functions. */
void set_wrapping_object(PMC *wrapper);
PMC * wrap_object(PARROT_INTERP, void *obj);
PMC * create_stable(PARROT_INTERP, REPROps *REPR, PMC *HOW);
PMC * decontainerize(PARROT_INTERP, PMC *var);
PMC * get_hll_config(PARROT_INTERP, STRING *hll);
PMC * hllize(PARROT_INTERP, PMC *obj, INTVAL hll_id);

/* Dynamic representation registration. */
typedef PMC * (*wrap_object_t)(PARROT_INTERP, void *obj);
typedef PMC * (*create_stable_t)(PARROT_INTERP, REPROps *REPR, PMC *HOW);
typedef INTVAL (* rf) (PARROT_INTERP, STRING *name, REPROps * (*reg) (PARROT_INTERP, wrap_object_t, create_stable_t));
#define REGISTER_DYNAMIC_REPR(interp, name, reg_func) \
    ((rf) \
        VTABLE_get_pointer(interp, \
            VTABLE_get_pmc_keyed_str(interp, interp->root_namespace, \
                Parrot_str_new_constant(interp, "_REGISTER_REPR"))))(interp, name, reg_func)

/* Dynamic container configuration registration. */
typedef void (*cspec_conf) (PARROT_INTERP, STRING *name, ContainerConfigurer *configurer);
#define REGISTER_DYNAMIC_CONTAINER_CONFIG(interp, name, configurer) \
    ((cspec_conf) \
        VTABLE_get_pointer(interp, \
            VTABLE_get_pmc_keyed_str(interp, interp->root_namespace, \
                Parrot_str_new_constant(interp, "_REGISTER_CONTCONF"))))(interp, name, configurer)

#endif
