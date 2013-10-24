#define MVM_SHARED 1
#include "moar.h"

/* Initializes the Perl 6 extension ops. */
static void p6init(MVMThreadContext *tc) {
}

/* Registers the extops with MoarVM. */
void Rakudo_ops_init(MVMThreadContext *tc) {
    MVM_ext_register_extop(tc, "p6init",  p6init, 0, NULL);
}
