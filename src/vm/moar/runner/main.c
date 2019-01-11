#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <uv.h>
#include <moar.h>

#ifndef _WIN32
#  include <libgen.h>
#endif

#ifndef _WIN32
#  include "signal.h"
#endif

#ifndef _WIN32
#  include <unistd.h>
#else
#  include <process.h>
#endif

#if defined(_MSC_VER)
#define strtoll _strtoi64
#endif

/* flags need to be sorted alphabetically */

enum {
    NOT_A_FLAG = -2,
    UNKNOWN_FLAG = -1,

    FLAG_SUSPEND,
    FLAG_DUMP,
    FLAG_TRACING,

    OPT_DEBUGPORT
};

static const char *const FLAGS[] = {
    "--debug-suspend",
    "--dump",
    "--full-cleanup",
    "--tracing",
};

static int cmp_flag(const void *key, const void *value)
{
    return strcmp(key, *(char **)value);
}

static int starts_with(const char *str, const char *want) {
    size_t str_len  = strlen(str);
    size_t want_len = strlen(want);
    return str_len < want_len
        ? 0
        : strncmp(str, want, want_len) == 0;
}

static int parse_flag(const char *arg)
{
    const char *const *found;

    if (!arg || arg[0] != '-')
        return NOT_A_FLAG;

    found = bsearch(arg, FLAGS, sizeof FLAGS / sizeof *FLAGS, sizeof *FLAGS, cmp_flag);

    if (found)
        return (int)(found - FLAGS);
    else if (starts_with(arg, "--debug-port="))
        return OPT_DEBUGPORT;
    else
        return UNKNOWN_FLAG;
}

#ifndef _WIN32
int main(int argc, char *argv[])
#else
int wmain(int argc, wchar_t *wargv[])
#endif
{
    MVMInstance *instance;
    int          res;
    char        *perl6_home;
    size_t       perl6_home_size;
    char        *nqp_home;
    size_t       nqp_home_size;
    char        *perl6_file;
    char        *exec_path;
    size_t       exec_path_size;
    char        *dir_path;
    int          dir_path_size;
    char        *lib_path[3];

#ifdef _WIN32
    char **argv = MVM_UnicodeToUTF8_argv(argc, wargv);
#endif

    int dump         = 0;
    int argi         = 1;
    int flag;
    int new_argc     = 0;

    unsigned int interval_id;
    char telemeh_inited = 0;

    MVMuint32 debugserverport = 0;
    int start_suspended = 0;

    /* Filter out VM arguments from the command line. */

    for (; (flag = parse_flag(argv[argi])) != NOT_A_FLAG; ++argi) {
        switch (flag) {
            case FLAG_DUMP:
            dump = 1;
            continue;

#if MVM_TRACING
            case FLAG_TRACING:
            MVM_interp_enable_tracing();
            continue;
#endif

            case FLAG_SUSPEND:
            start_suspended = 1;
            continue;

            case OPT_DEBUGPORT: {
                MVMint64 port;
                char *portstr = argv[argi] + strlen("--debugport=") + 1;
                char *endptr;
                port = strtoll(portstr, &endptr, 10);
                if (*endptr != '\0') {
                    fprintf(stderr, "ERROR: Invalid characters in debug port flag: %s\n", portstr);
                    return EXIT_FAILURE;
                }
                if (port <= 1024 || 65535 < port) {
                    fprintf(stderr, "ERROR: debug server port out of range. We only accept ports above 1024 and below 65535. (got: %"PRIi64")\n", port);
                    return EXIT_FAILURE;
                }
                debugserverport = (MVMuint32)port;
                break;
            }

            default:
            argv[new_argc++] = argv[argi];
        }
    }

    /* Move over the remaining arguments. */

    for (; argv[argi]; ++argi) {
        argv[new_argc++] = argv[argi];
    }


#ifdef HAVE_TELEMEH
    if (getenv("MVM_TELEMETRY_LOG")) {
        char path[256];
        FILE *fp;
        snprintf(path, 255, "%s.%d", getenv("MVM_TELEMETRY_LOG"),
#ifdef _WIN32
             _getpid()
#else
             getpid()
#endif
             );
        fp = fopen(path, "w");
        if (fp) {
            MVM_telemetry_init(fp);
            telemeh_inited = 1;
            interval_id = MVM_telemetry_interval_start(0, "moarvm startup");
        }
    }
#endif

    /* Retrieve the executable directory path. */

    exec_path_size = 4096;
    exec_path = (char*)malloc(exec_path_size);
    res = MVM_exepath(exec_path, &exec_path_size);
    while (res < 0 && exec_path_size < 4096*8) {
        exec_path_size *= 2;
        exec_path = (char*)realloc(exec_path, exec_path_size);
        res = MVM_exepath(exec_path, &exec_path_size);
    }
    if (res < 0) {
        fprintf(stderr, "ERROR: Could not retrieve executable path.\n");
        return EXIT_FAILURE;
    }

    dir_path = (char*)malloc(exec_path_size);
    memcpy(dir_path, exec_path, exec_path_size);
#ifdef _WIN32
    PathRemoveFileSpecA(dir_path);
#else
    dir_path = dirname(dir_path);
#endif
    dir_path_size = strlen(dir_path);

    /* Put together PERL6_HOME and NQP_HOME. */

    perl6_home = getenv("PERL6_HOME");
    if (perl6_home) {
        perl6_home_size = strlen(perl6_home);
#ifdef _WIN32
        if (*(perl6_home + perl6_home_size - 1) == '\\') {
#else
        if (*(perl6_home + perl6_home_size - 1) == '/') {
#endif
            *(perl6_home + perl6_home_size - 1) = '\0';
            perl6_home_size--;
        }
    }
    else {
        perl6_home = (char*)malloc(dir_path_size + 50);
        memcpy(perl6_home, dir_path, dir_path_size);
#ifdef _WIN32
        strcpy(perl6_home + dir_path_size, "\\..\\share\\perl6");
#else
        strcpy(perl6_home + dir_path_size, "/../share/perl6");
#endif
        perl6_home_size = strlen(perl6_home);
    }

    nqp_home = getenv("NQP_HOME");
    if (nqp_home) {
        nqp_home_size = strlen(nqp_home);
#ifdef _WIN32
        if (*(nqp_home + nqp_home_size - 1) == '\\') {
#else
        if (*(nqp_home + nqp_home_size - 1) == '/') {
#endif
            *(nqp_home + nqp_home_size - 1) = '\0';
            nqp_home_size--;
        }
    }
    else {
        nqp_home = (char*)malloc(dir_path_size + 50);
        memcpy(nqp_home, dir_path, dir_path_size);
#ifdef _WIN32
        strcpy(nqp_home + dir_path_size, "\\..\\share\\nqp");
#else
        strcpy(nqp_home + dir_path_size, "/../share/nqp");
#endif
        nqp_home_size = strlen(nqp_home);
    }

    /* Put together the lib paths and perl6_file path. */

    lib_path[0] = (char*)malloc(dir_path_size + 50);
    lib_path[1] = (char*)malloc(dir_path_size + 50);
    lib_path[2] = (char*)malloc(dir_path_size + 50);
    perl6_file  = (char*)malloc(dir_path_size + 50);

    memcpy(lib_path[0], nqp_home,     nqp_home_size);
    memcpy(lib_path[1], perl6_home, perl6_home_size);
    memcpy(lib_path[2], perl6_home, perl6_home_size);
    memcpy(perl6_file,  perl6_home, perl6_home_size);

#ifdef _WIN32
    strcpy(lib_path[0] +   nqp_home_size, "\\lib");
    strcpy(lib_path[1] + perl6_home_size, "\\lib");
    strcpy(lib_path[2] + perl6_home_size, "\\runtime");
    strcpy(perl6_file  + perl6_home_size, "\\runtime\\perl6.moarvm");
#else
    strcpy(lib_path[0] +   nqp_home_size, "/lib");
    strcpy(lib_path[1] + perl6_home_size, "/lib");
    strcpy(lib_path[2] + perl6_home_size, "/runtime");
    strcpy(perl6_file  + perl6_home_size, "/runtime/perl6.moarvm");
#endif

    printf("%s %s\n", nqp_home, perl6_home);

    /* Start up the VM. */

    instance   = MVM_vm_create_instance();

    MVM_vm_set_clargs(instance, new_argc, argv);
    MVM_vm_set_prog_name(instance, perl6_file);
    MVM_vm_set_exec_name(instance, exec_path);
    MVM_vm_set_lib_path(instance, 3, (const char **)lib_path);

    /* Ignore SIGPIPE by default, since we error-check reads/writes. This does
     * not prevent users from setting up their own signal handler for SIGPIPE,
     * which will take precedence over this ignore. */
#ifndef _WIN32
    signal(SIGPIPE, SIG_IGN);
#endif

    if (debugserverport > 0) {
        MVM_debugserver_init(instance->main_thread, debugserverport);

        if (start_suspended) {
            instance->main_thread->gc_status = MVMGCStatus_INTERRUPT | MVMSuspendState_SUSPEND_REQUEST;
        }
    }

    if (dump) MVM_vm_dump_file(instance, perl6_file);
    else MVM_vm_run_file(instance, perl6_file);

#ifdef HAVE_TELEMEH
    if (getenv("MVM_TELEMETRY_LOG") && telemeh_inited) {
        MVM_telemetry_interval_stop(0, interval_id, "moarvm teardown");
        MVM_telemetry_finish();
    }
#endif

    MVM_vm_exit(instance);

    free(lib_path[0]);
    free(lib_path[1]);
    free(lib_path[2]);
    free(perl6_file);
    free(exec_path);
    free(dir_path);
    free(perl6_home);
    free(nqp_home);
}
