#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <uv.h>
#include <moar.h>

#ifdef _WIN32
#  include <sys/types.h>
#  include <sys/stat.h>
#  include <process.h>
#  include <shlwapi.h>
#  if defined(_MSC_VER)
#    define strtoll _strtoi64
#  endif
#else
#  include <sys/stat.h>
#  include <libgen.h>
#  include <unistd.h>
#  include "signal.h"
#endif

#define STRINGIFY1(x) #x
#define STRINGIFY(x) STRINGIFY1(x)

/* flags need to be sorted alphabetically */

enum {
    NOT_A_FLAG = -2,
    UNKNOWN_FLAG = -1,

    FLAG_SUSPEND,
    FLAG_FULL_CLEANUP,
    FLAG_TRACING,

    OPT_DEBUGPORT
};

static const char *const FLAGS[] = {
    "--debug-suspend",
    "--full-cleanup",
    "--tracing",
};

static int cmp_flag(const void *key, const void *value) {
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

int file_exists(const char *path) {
#ifdef _WIN32
    struct _stat sb;
    return _stat(path, &sb) == 0;
#else
    struct stat *sb = malloc(sizeof(struct stat));
    int res         = stat(path, sb) == 0;
    free(sb);
    return res;
#endif
}

void platformify_path(char *path) {
#ifdef _WIN32
    int i;
    for (i = 0; path[i]; i++) {
        if (path[i] == '/') {
            path[i] = '\\';
        }
    }
#endif
}

int retrieve_home(
          char   *out_home,
    const char   *rel_home,
    const size_t  rel_home_size,
    const char   *env_var,
          char   *exec_dir_path,
          size_t  exec_dir_path_size,
    const char   *check_file,
    const size_t  check_file_size
) {
    char   *check_file_path;
    char   *env_home         = getenv(env_var);
    size_t  home_size        = exec_dir_path_size + rel_home_size;
    int     ret;

    if (env_home) {
        strcpy(out_home, env_home);
        home_size = strlen(out_home);
#ifdef _WIN32
        if (*(out_home + home_size - 1) == '\\') {
#else
        if (*(out_home + home_size - 1) == '/') {
#endif
            *(out_home + home_size - 1) = '\0';
            home_size--;
        }
    }
    else {
        strncpy(out_home, exec_dir_path, home_size);
        strncat(out_home, rel_home, rel_home_size);
        platformify_path(out_home + exec_dir_path_size);
    }

    check_file_path = (char*)malloc(home_size + check_file_size + 1);
    strncpy(check_file_path, out_home, home_size + check_file_size);
    strncat(check_file_path, check_file, check_file_size);

    ret = file_exists(check_file_path);
    free(check_file_path);
    return ret;
}

#ifndef _WIN32
int main(int argc, char *argv[])
#else
int wmain(int argc, wchar_t *wargv[])
#endif
{
    MVMInstance *instance;

    char   *exec_path;
    size_t  exec_path_size;
    int     res;

    char   *dir_path;
    char   *dir_path_temp;
    size_t  dir_path_size;

          char   *nqp_home;
          size_t  nqp_home_size;
    const char    nqp_rel_path[14]    = "/../share/nqp";
    const size_t  nqp_rel_path_size   = 13;
    const char    nqp_check_path[28]  = "/lib/NQPCORE.setting.moarvm";
    const size_t  nqp_check_path_size = 27;

          char   *perl6_home;
          size_t  perl6_home_size;
    const char    perl6_rel_path[16]    = "/../share/perl6";
    const size_t  perl6_rel_path_size   = 15;
    const char    perl6_check_path[22]  = "/runtime/perl6.moarvm";
    const size_t  perl6_check_path_size = 21;

    char *lib_path[3];
    char *perl6_file;

#ifdef _WIN32
    char **argv = MVM_UnicodeToUTF8_argv(argc, wargv);
#endif

    int full_cleanup = 0;
    int argi         = 1;
    int flag;
    int new_argc     = 0;

    unsigned int interval_id;
    char telemeh_inited = 0;

    MVMuint32 debugserverport = 0;
    int start_suspended = 0;

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

    /* Filter out VM arguments from the command line. */

    for (; (flag = parse_flag(argv[argi])) != NOT_A_FLAG; ++argi) {
        switch (flag) {
            case FLAG_FULL_CLEANUP:
            full_cleanup = 1;
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
                char *portstr = argv[argi] + strlen("--debug-port=");
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

    /* The +1 is the trailing \0 terminating the string. */
    dir_path_temp = (char*)malloc(exec_path_size + 1);
    memcpy(dir_path_temp, exec_path, exec_path_size + 1);
#ifdef _WIN32
    PathRemoveFileSpecA(dir_path_temp);
    dir_path_size = strlen(dir_path_temp);
    dir_path      = (char*)malloc(dir_path_size + 1);
    memcpy(dir_path, dir_path_temp, dir_path_size + 1);
#else
    dir_path      = dirname(dir_path_temp);
    dir_path_size = strlen(dir_path);
#endif

    /* Retrieve PERL6_HOME and NQP_HOME. */

#ifdef STATIC_NQP_HOME
    nqp_home = STRINGIFY(STATIC_NQP_HOME);
#else
    nqp_home = (char*)malloc(dir_path_size + nqp_rel_path_size + 1);
    if (!retrieve_home(nqp_home, nqp_rel_path, nqp_rel_path_size, "NQP_HOME",
            dir_path, dir_path_size, nqp_check_path, nqp_check_path_size)) {
        fprintf(stderr, "ERROR: NQP_HOME is invalid: %s\n", nqp_home);
        return EXIT_FAILURE;
    }
#endif
    nqp_home_size = strlen(nqp_home);

#ifdef STATIC_PERL6_HOME
    perl6_home = STRINGIFY(STATIC_PERL6_HOME);
#else
    perl6_home = (char*)malloc(dir_path_size + perl6_rel_path_size + 1);
    if (!retrieve_home(perl6_home, perl6_rel_path, perl6_rel_path_size, "PERL6_HOME",
            dir_path, dir_path_size, perl6_check_path, perl6_check_path_size)) {
        fprintf(stderr, "ERROR: PERL6_HOME is invalid: %s\n", perl6_home);
        return EXIT_FAILURE;
    }
#endif
    perl6_home_size = strlen(perl6_home);

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
#ifdef MOAR_PERL6_RUNNER_DEBUG
    strcpy(perl6_file  + perl6_home_size, "\\runtime\\perl6-debug.moarvm");
#else
    strcpy(perl6_file  + perl6_home_size, "\\runtime\\perl6.moarvm");
#endif
#else
    strcpy(lib_path[0] +   nqp_home_size, "/lib");
    strcpy(lib_path[1] + perl6_home_size, "/lib");
    strcpy(lib_path[2] + perl6_home_size, "/runtime");
#ifdef MOAR_PERL6_RUNNER_DEBUG
    strcpy(perl6_file  + perl6_home_size, "/runtime/perl6-debug.moarvm");
#else
    strcpy(perl6_file  + perl6_home_size, "/runtime/perl6.moarvm");
#endif
#endif

    /* Start up the VM. */

    instance = MVM_vm_create_instance();

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

    MVM_vm_run_file(instance, perl6_file);

#ifdef HAVE_TELEMEH
    if (getenv("MVM_TELEMETRY_LOG") && telemeh_inited) {
        MVM_telemetry_interval_stop(0, interval_id, "moarvm teardown");
        MVM_telemetry_finish();
    }
#endif

    free(lib_path[0]);
    free(lib_path[1]);
    free(lib_path[2]);
    free(perl6_file);
    free(exec_path);
#ifdef _WIN32
    /* dirname's return value is either on the stack or is the same pointer
     * that was passed to it depending on the version of libc used, which leads
     * to double frees. */
    free(dir_path);
#endif
    free(dir_path_temp);
#ifndef STATIC_PERL6_HOME
    free(perl6_home);
#endif
#ifndef STATIC_NQP_HOME
    free(nqp_home);
#endif

    if (full_cleanup) {
        MVM_vm_destroy_instance(instance);
        return EXIT_SUCCESS;
    }
    else {
        MVM_vm_exit(instance);
    }
}
