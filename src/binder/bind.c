/*
$Id$
Copyright (C) 2009, The Perl Foundation.
*/

#include "parrot/parrot.h"
#include "sigguts.h"

/* Takes a signature along with positional and named arguments and binds them
 * into the provided lexpad (actually, anything that has a Hash interface will
 * do). Returns 0 if binding fails, and non-zero otherwise. */
INTVAL
Rakudo_binding_bind_signature(PARROT_INTERP, PMC *lexpad, PMC *signature,
                               PMC *pos_args, PMC *named_args,
                               INTVAL no_nom_type_check, STRING **error) {
    return 0;
}


/* Binds a single argument into the lexpad, after doing any checks that are
 * needed. Also handles any type captures. If there is a sub signature, then
 * re-enters the binder. Returns 0 if binding fails, and non-zero otherwise. */
INTVAL
Rakudo_binding_bind_one_param(PARROT_INTERP, PMC *lexpad, llsig_element *sig_info,
                              PMC *value, INTVAL no_nom_type_check, STRING **error) {
    return 0;
}

/*
 * Local variables:
 *   c-file-style: "parrot"
 * End:
 * vim: expandtab shiftwidth=4:
 */
