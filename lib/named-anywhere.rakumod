# The sole purpose of this module is to make it easier to activate
# the possibility of having MAIN accept named arguments anywhere on
# the command line.

with %*SUB-MAIN-OPTS -> %opts {
    %opts<named-anywhere> = True;
}
else {
    PROCESS::<%SUB-MAIN-OPTS> = :named-anywhere;
}

# vim: expandtab shiftwidth=4
