#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

int main(int argc, char *argv[])
{
    int moar_argc;
    int exec_argc;
    int c;
    char **exec_argv;
    char *moar = "@c_escape(@nfp(@MOAR@)@)@";

    moar_argc = 6;

    // program name + moar args + passed args (without program name) + NULL pointer
    exec_argc = 1 + moar_argc + (argc - 1) + 1;
    exec_argv = malloc(exec_argc * sizeof(void*));

    exec_argv[0] = "@c_escape(@nfp(@MOAR@)@)@";

    // Set up moar args.
    exec_argv[1] = "--execname=@c_escape(@nfp(@exec_name@)@)@";
    exec_argv[2] = "--libpath=@c_escape(@nfp(@base_dir@)@)@";
    exec_argv[3] = "--libpath=@c_escape(@nfp(@base_dir@/blib)@)@";
    exec_argv[4] = "--libpath=@c_escape(@nfp(@nqp_lib_dir@)@)@";
    exec_argv[5] = "@c_escape(@nfp(@base_dir@/@mbc@)@)@";
    exec_argv[6] = "--nqp-lib=@c_escape(@nfp(@base_dir@/blib)@)@";

    // Copy passed args.
    for (c = 0; c < argc - 1; c++) {
        exec_argv[1 + moar_argc + c] = argv[c + 1];
    }

    exec_argv[exec_argc - 1] = NULL;

    execv(moar, exec_argv);

    // execv doesn't return on successful exec.
    fprintf(stderr, "ERROR: Failed to execute moar. Error code: %i\n", errno);
    return EXIT_FAILURE;
}
