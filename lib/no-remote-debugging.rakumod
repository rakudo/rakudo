# Loading this module (either directly or through setting
# the RAKUDO_OPT environment variable) will check whether
# the process has activated the remote debugger, and will
# exit with a note if remote debugging is possible.

INIT {
    if VM.remote-debugging {
        note "Sorry, no remote debugging allowed";
        exit 2;
    }
}

# vim: expandtab shiftwidth=4
