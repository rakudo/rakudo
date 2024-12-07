# Shorthand for loading a control-c handler that will produce
# a full backtrace of all threads on STDERR when control-c is
# pressed.

signal(SIGINT).tap: { VM.ownup }

# vim: expandtab shiftwidth=4
