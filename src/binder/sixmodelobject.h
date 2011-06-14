/* This file contains various structure definitions for the 6model object
 * meta-model implementation. */

#ifndef SIXMODELOBJECT_H_GUARD
#define SIXMODELOBJECT_H_GUARD

#include "storage_spec.h"

/* The commonalities shared between all 6model objects, no matter what the
 * REPR is. This struct should be placed as the first thing in the object
 * struct used by all representations. */
typedef struct {
    PMC *stable;  /* The shared table. */
    PMC *sc;      /* Serialization context. */
} SixModelObjectCommonalities;

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

/* Controls the way that type checks are performed. By default, if there is
 * a type check cache we treat it as definitive. However, it's possible to
 * declare that in the case the type check cache has no entry we should fall
 * back to asking the .HOW.type_check method (set TYPE_CHECK_CACHE_THEN_METHOD).
 * While a normal type check asks a value if it suppots another type, the
 * TYPE_CHECK_NEEDS_ACCEPTS flag results in a call to .accepts_type on the
 * HOW of the thing we're checking the value against, giving it a chance to
 * decide answer. */
#define TYPE_CHECK_CACHE_DEFINITIVE    0
#define TYPE_CHECK_CACHE_THEN_METHOD   1
#define TYPE_CHECK_NEEDS_ACCEPTS       2

/* S-Tables (short for Shared Table) contains the commonalities shared between
 * a (HOW, REPR) pairing (for example, (HOW for the class Dog, P6Opaque). */
typedef struct {
    /* The representation. */
    PMC *REPR;
    
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
    
    /* The type checking mode (see flags for this above). */
    INTVAL type_check_mode;

    /* An ID solely for use in caches that last a VM instance. Thus it
     * should never, ever be serialized and you should NEVER make a
     * type directory based upon this ID. Otherwise you'll create memory
     * leaks for anonymous types, and other such screwups. */
    INTVAL type_cache_id;
    
    /* If this is a container, then this contains information needed in
     * order to fetch the value in it. If not, it'll be null, which can
     * be taken as a "not a container" indication. */
    ContainerSpec *container_spec;
    
    /* The underlying package stash. */
    PMC *WHO;

    /* Parrot-specific set of v-table to method mappings, for overriding
     * of Parrot v-table functions. */
    PMC **parrot_vtable_mapping;

	/* Parrot-specific set of v-table to object method mappings. */
	AttributeIdentifier *parrot_vtable_handler_mapping;
} STable;

/* A representation is what controls the layout of an object and storage of
 * attributes, as well as how it boxes/unboxes to the three primitive types
 * INTVAL, FLOATVAL and STRING * (if it can).
 *
 * All representations will either use this struct directly or have it as
 * the first element in their own struct followed by any data they want to
 * keep for representation instance. Essentially, it defines the set of
 * functions that a representation should implement to fulfil the
 * representation API. */
typedef struct {
    /* Creates a new type object of this representation, and
     * associates it with the given HOW. Also sets up a new
     * representation instance if needed. */
    PMC * (*type_object_for) (PARROT_INTERP, PMC *HOW);

    /* Creates a new instance based on the type object. */
    PMC * (*instance_of) (PARROT_INTERP, PMC *WHAT);

    /* Checks if a given object is defined (from the point of
     * view of the representation). */
    INTVAL (*defined) (PARROT_INTERP, PMC *Obj);

    /* Gets the current value for an object attribute. */
    PMC * (*get_attribute) (PARROT_INTERP, PMC *Object, PMC *ClassHandle, STRING *Name, INTVAL Hint);

    /* Gets the current value for a native int attribute. */
    INTVAL (*get_attribute_int) (PARROT_INTERP, PMC *Object, PMC *ClassHandle, STRING *Name, INTVAL Hint);

    /* Gets the current value for a native num attribute. */
    FLOATVAL (*get_attribute_num) (PARROT_INTERP, PMC *Object, PMC *ClassHandle, STRING *Name, INTVAL Hint);

    /* Gets the current value for a native str attribute. */
    STRING * (*get_attribute_str) (PARROT_INTERP, PMC *Object, PMC *ClassHandle, STRING *Name, INTVAL Hint);

    /* Binds the given object value to the specified attribute. */
    void (*bind_attribute) (PARROT_INTERP, PMC *Object, PMC *ClassHandle, STRING *Name, INTVAL Hint, PMC *Value);

    /* Binds the given int value to the specified attribute. */
    void (*bind_attribute_int) (PARROT_INTERP, PMC *Object, PMC *ClassHandle, STRING *Name, INTVAL Hint, INTVAL Value);

    /* Binds the given num value to the specified attribute. */
    void (*bind_attribute_num) (PARROT_INTERP, PMC *Object, PMC *ClassHandle, STRING *Name, INTVAL Hint, FLOATVAL Value);

    /* Binds the given str value to the specified attribute. */
    void (*bind_attribute_str) (PARROT_INTERP, PMC *Object, PMC *ClassHandle, STRING *Name, INTVAL Hint, STRING *Value);

    /* Gets the hint for the given attribute ID. */
    INTVAL (*hint_for) (PARROT_INTERP, PMC *ClassHandle, STRING *Name);

    /* Clones the object, optionally cloning any containers in its
     * attributes. */
    PMC * (*clone) (PARROT_INTERP, PMC *to_clone);
    
    /* Used with boxing. Sets an integer value, for representations that
     * can hold one. */
    void (*set_int) (PARROT_INTERP, PMC *Object, INTVAL Value);

    /* Used with boxing. Gets an integer value, for representations that
     * can hold one. */
    INTVAL (*get_int) (PARROT_INTERP, PMC *Object);

    /* Used with boxing. Sets a floating point value, for representations that
     * can hold one. */
    void (*set_num) (PARROT_INTERP, PMC *Object, FLOATVAL Value);

    /* Used with boxing. Gets a floating point value, for representations that
     * can hold one. */
    FLOATVAL (*get_num) (PARROT_INTERP, PMC *Object);

    /* Used with boxing. Sets a string value, for representations that
     * can hold one. */
    void (*set_str) (PARROT_INTERP, PMC *Object, STRING *Value);

    /* Used with boxing. Gets a string value, for representations that
     * can hold one. */
    STRING * (*get_str) (PARROT_INTERP, PMC *Object);

    /* This Parrot-specific addition to the API is used to mark an object. */
    void (*gc_mark) (PARROT_INTERP, PMC *Object);

    /* This Parrot-specific addition to the API is used to free an object. */
    void (*gc_free) (PARROT_INTERP, PMC *Object);

    /* This Parrot-specific addition to the API is used to mark a REPR instance. */
    void (*gc_mark_repr) (PARROT_INTERP, STable *st);

    /* This Parrot-specific addition to the API is used to free a REPR instance. */
    void (*gc_free_repr) (PARROT_INTERP, STable *st);

    /* Gets the storage specification for this representation. */
    storage_spec (*get_storage_spec) (PARROT_INTERP, STable *st);
} REPRCommonalities;

/* Hint value to indicate the absence of an attribute lookup or method
 * dispatch hint. */
#define NO_HINT -1

/* Various handy macros for getting at important stuff. */
#define STABLE_PMC(o)    (((SixModelObjectCommonalities *)PMC_data(o))->stable)
#define STABLE(o)        ((STable *)PMC_data(STABLE_PMC(o)))
#define SC_PMC(o)        (((SixModelObjectCommonalities *)PMC_data(o))->sc)
#define STABLE_STRUCT(p) ((STable *)PMC_data(p))
#define REPR_PMC(o)      (STABLE(o)->REPR)
#define REPR(o)          ((REPRCommonalities *)PMC_data(REPR_PMC(o)))
#define REPR_STRUCT(p)   ((REPRCommonalities *)PMC_data(p))

/* Object model initialization. */
void SixModelObject_initialize(PARROT_INTERP, PMC **knowhow, PMC **knowhow_attribute);

/* Some utility functions. */
PMC * wrap_repr(PARROT_INTERP, void *REPR);
PMC * wrap_object(PARROT_INTERP, void *obj);
PMC * create_stable(PARROT_INTERP, PMC *REPR, PMC *HOW);
PMC * decontainerize(PARROT_INTERP, PMC *var);

#endif
