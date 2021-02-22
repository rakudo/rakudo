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
#  include <io.h>
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

    OPT_DEBUGPORT,

    OPT_RAKUDO_HOME
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
    else if (starts_with(arg, "--rakudo-home="))
        return OPT_RAKUDO_HOME;
    else
        return UNKNOWN_FLAG;
}

int file_exists(const char *path) {
#ifdef _WIN32
    int             res;
    struct _stat    sb;
    const int       len   = MultiByteToWideChar(CP_UTF8, 0, path, -1, NULL, 0);
    wchar_t * const wpath = (wchar_t *)malloc(len * sizeof(wchar_t));
    MultiByteToWideChar(CP_UTF8, 0, path, -1, (LPWSTR)wpath, len);
    res = _wstat(wpath, &sb);
    MVM_free(wpath);
    return res == 0;
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
          char  **out_home,
    const char   *rel_home,
    const size_t  rel_home_size,
    const char   *env_var,
          char   *exec_dir_path,
          size_t  exec_dir_path_size,
    const char   *check_file,
    const size_t  check_file_size,
          char   *static_home,
          char   *options_home
) {
    char   *check_file_path;
    size_t  home_size;
    int     ret;
    char   *env_home         = getenv(env_var);

    if (options_home) {
        *out_home = options_home;
        home_size = strlen(*out_home);
    }
    else if (env_home) {
        home_size = strlen(env_home);
        *out_home = (char*)malloc(home_size + 1);
        strcpy(*out_home, env_home);
#ifdef _WIN32
        if (*(*out_home + home_size - 1) == '\\') {
#else
        if (*(*out_home + home_size - 1) == '/') {
#endif
            *(*out_home + home_size - 1) = '\0';
            home_size--;
        }
    }
    else if (static_home) {
        *out_home = static_home;
        home_size = strlen(*out_home);
    }
    else {
        home_size = exec_dir_path_size + rel_home_size;
        *out_home = (char*)malloc(home_size + 1);
        strncpy(*out_home, exec_dir_path, home_size);
        strncat(*out_home, rel_home, rel_home_size);
        platformify_path(*out_home + exec_dir_path_size);
    }

    check_file_path = (char*)malloc(home_size + check_file_size + 1);
    strncpy(check_file_path, *out_home, home_size + check_file_size);
    strncat(check_file_path, check_file, check_file_size);

    ret = file_exists(check_file_path);
    free(check_file_path);
    return ret;
}

#if defined(_WIN32) && defined(SUBSYSTEM_WINDOWS)
int set_std_handle_to_nul(FILE *file, int fd, BOOL read, int std_handle_type) {
    /* Found on https://docs.microsoft.com/en-us/cpp/c-runtime-library/reference/get-osfhandle?view=vs-2019:
       
       "When stdin, stdout, and stderr aren't associated with a stream (for example, in a Windows
       application without a console window), the file descriptor values for these streams are
       returned from _fileno as the special value -2. Similarly, if you use a 0, 1, or 2 as the
       file descriptor parameter instead of the result of a call to _fileno, _get_osfhandle also
       returns the special value -2 when the file descriptor is not associated with a stream, and
       does not set errno. However, this is not a valid file handle value, and subsequent calls
       that attempt to use it are likely to fail."
       
       See https://jdebp.eu/FGA/redirecting-standard-io.html
           https://stackoverflow.com/a/50358201 (Especially the comments of Eryk Sun)
    */
    FILE *stream;
    HANDLE new_handle;

    if (_fileno(file) != -2 || _get_osfhandle(fd) != -2)
        // The handles are initialized. Don't touch!
        return 1;
    
    /* FD 1 is in an error state (_get_osfhandle(1) == -2). Close it. The FD number is up for grabs
       after this call. */
    if (_close(fd) != 0)
        return 0;
    
    /* FILE *stdout is in an error state (_fileno(stdout) == -2). Reopen it to a "NUL:" file. This
       will take the next free FD number. So it's important to call this sequentially for FD 0, 1
       and 2. */
    if (freopen_s(&stream, "NUL:", read ? "r" : "w", file) != 0)
        return 0;
    
    /* Set the underlying Windows handle as the STD handler. */
    new_handle = (HANDLE)_get_osfhandle(fd);
    if (!SetStdHandle(std_handle_type, new_handle))
        return 0;
    
    return 1;
}
#endif

#if defined(_WIN32) && defined(SUBSYSTEM_WINDOWS)
int wWinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPWSTR lpCmdLine, INT nCmdShow) {
    int argc;
    LPWSTR *wargv = CommandLineToArgvW(GetCommandLineW(), &argc);
    char **argv = MVM_UnicodeToUTF8_argv(argc, wargv);
    LocalFree(wargv);
#elif defined(_WIN32) && !defined(SUBSYSTEM_WINDOWS)
int wmain(int argc, wchar_t *wargv[]) {
    char **argv = MVM_UnicodeToUTF8_argv(argc, wargv);
#else
int main(int argc, char *argv[]) {
#endif
    MVMInstance *instance;

    char   *exec_path;
    size_t  exec_path_size;

    char   *exec_dir_path_temp;
    char   *exec_dir_path;
    size_t  exec_dir_path_size;

          char   *nqp_home;
          size_t  nqp_home_size;
          char   *static_nqp_home     = 0;
    const char    nqp_rel_path[14]    = "/../share/nqp";
    const size_t  nqp_rel_path_size   = 13;
    const char    nqp_check_path[28]  = "/lib/NQPCORE.setting.moarvm";
    const size_t  nqp_check_path_size = 27;

          char   *rakudo_home;
          size_t  rakudo_home_size;
          char   *static_rakudo_home    = 0;
          char   *option_rakudo_home    = 0;
    const char    perl6_rel_path[16]    = "/../share/perl6";
    const size_t  perl6_rel_path_size   = 15;
    const char    perl6_check_path[22]  = "/runtime/perl6.moarvm";
    const size_t  perl6_check_path_size = 21;

    char *lib_path[3];
    char *perl6_file;

    int full_cleanup = 0;
    int argi         = 1;
    int flag;
    int new_argc     = 0;

    MVMuint32 debugserverport = 0;
    int start_suspended = 0;
    
#if defined(_WIN32) && defined(SUBSYSTEM_WINDOWS)
    /* When using the 'windows' subsystem the standard IO handles are not
       connected. This causes a program abort when accessing the handles. To
       prevent these aborts, we redirect the handles to NUL in this case.
    */
    
    /* Set our own handles. */
    if (!set_std_handle_to_nul(stdin,  0, 1, STD_INPUT_HANDLE))  return EXIT_FAILURE;
    if (!set_std_handle_to_nul(stdout, 1, 0, STD_OUTPUT_HANDLE)) return EXIT_FAILURE;
    if (!set_std_handle_to_nul(stderr, 2, 0, STD_ERROR_HANDLE))  return EXIT_FAILURE;
    
    /* MoarVM - as a DLL, and the way it's compiled (/MT) has it's own CRT and thus it's own CRT STD handles.
       So MoarVM also needs to fix up its CRT STD handles.
       See: https://docs.microsoft.com/de-de/cpp/c-runtime-library/potential-errors-passing-crt-objects-across-dll-boundaries
            https://docs.microsoft.com/en-us/cpp/c-runtime-library/crt-library-features
    */
    if (!MVM_set_std_handles_to_nul()) return EXIT_FAILURE;
#endif

    /* Retrieve the executable directory path. */

#ifdef STATIC_EXEC_PATH
    exec_path = STRINGIFY(STATIC_EXEC_PATH);
    exec_path_size = strlen(exec_path);
#else
    int res;
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
#endif

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

            case OPT_RAKUDO_HOME:
                option_rakudo_home = argv[argi] + strlen("--rakudo-home=");
                break;

            default:
            argv[new_argc++] = argv[argi];
        }
    }

    /* Move over the remaining arguments. */

    for (; argv[argi]; ++argi) {
        argv[new_argc++] = argv[argi];
    }


#ifdef HAVE_TELEMEH
    unsigned int interval_id = 0;
    char telemeh_inited = 0;
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
    exec_dir_path_temp = (char*)malloc(exec_path_size + 1);
    memcpy(exec_dir_path_temp, exec_path, exec_path_size + 1);
#ifdef _WIN32
    PathRemoveFileSpecA(exec_dir_path_temp);
    exec_dir_path_size = strlen(exec_dir_path_temp);
    exec_dir_path      = (char*)malloc(exec_dir_path_size + 1);
    memcpy(exec_dir_path, exec_dir_path_temp, exec_dir_path_size + 1);
#else
    exec_dir_path      = dirname(exec_dir_path_temp);
    exec_dir_path_size = strlen(exec_dir_path);
#endif

    /* Retrieve RAKUDO_HOME and NQP_HOME. */

#ifdef STATIC_NQP_HOME
    static_nqp_home = STRINGIFY(STATIC_NQP_HOME);
#endif
    if (!retrieve_home(&nqp_home, nqp_rel_path, nqp_rel_path_size, "NQP_HOME",
            exec_dir_path, exec_dir_path_size, nqp_check_path,
            nqp_check_path_size, static_nqp_home, 0)) {
        fprintf(stderr, "ERROR: NQP_HOME is invalid: %s\n", nqp_home);
        return EXIT_FAILURE;
    }
    nqp_home_size = strlen(nqp_home);

#ifdef STATIC_RAKUDO_HOME
    static_rakudo_home = STRINGIFY(STATIC_RAKUDO_HOME);
#endif
    /* XXX Isn't it time to move RAKUDO_HOME in front of PERL6_HOME?? */
    if (getenv("PERL6_HOME")) {
        if (!retrieve_home(&rakudo_home, perl6_rel_path, perl6_rel_path_size,
                "PERL6_HOME", exec_dir_path, exec_dir_path_size,
                perl6_check_path, perl6_check_path_size, static_rakudo_home,
                option_rakudo_home)) {
            fprintf(stderr, "ERROR: PERL6_HOME is invalid: %s\n", rakudo_home);
            return EXIT_FAILURE;
        }
    }
    else {
        if (!retrieve_home(&rakudo_home, perl6_rel_path, perl6_rel_path_size,
                "RAKUDO_HOME", exec_dir_path, exec_dir_path_size,
                perl6_check_path, perl6_check_path_size, static_rakudo_home,
                option_rakudo_home)) {
            fprintf(stderr, "ERROR: RAKUDO_HOME is invalid: %s\n", rakudo_home);
            return EXIT_FAILURE;
        }
    }
    rakudo_home_size = strlen(rakudo_home);

    /* Put together the lib paths and perl6_file path. */

    lib_path[0] = (char*)malloc(nqp_home_size   + 50);
    lib_path[1] = (char*)malloc(rakudo_home_size + 50);
    lib_path[2] = (char*)malloc(rakudo_home_size + 50);
    perl6_file  = (char*)malloc(rakudo_home_size + 50);

    memcpy(lib_path[0], nqp_home,     nqp_home_size);
    memcpy(lib_path[1], rakudo_home, rakudo_home_size);
    memcpy(lib_path[2], rakudo_home, rakudo_home_size);
    memcpy(perl6_file,  rakudo_home, rakudo_home_size);

#ifdef _WIN32
    strcpy(lib_path[0] +   nqp_home_size, "\\lib");
    strcpy(lib_path[1] + rakudo_home_size, "\\lib");
    strcpy(lib_path[2] + rakudo_home_size, "\\runtime");
#ifdef MOAR_RAKUDO_RUNNER_DEBUG
    strcpy(perl6_file  + rakudo_home_size, "\\runtime\\perl6-debug.moarvm");
#else
    strcpy(perl6_file  + rakudo_home_size, "\\runtime\\perl6.moarvm");
#endif
#else
    strcpy(lib_path[0] +   nqp_home_size, "/lib");
    strcpy(lib_path[1] + rakudo_home_size, "/lib");
    strcpy(lib_path[2] + rakudo_home_size, "/runtime");
#ifdef MOAR_RAKUDO_RUNNER_DEBUG
    strcpy(perl6_file  + rakudo_home_size, "/runtime/perl6-debug.moarvm");
#else
    strcpy(perl6_file  + rakudo_home_size, "/runtime/perl6.moarvm");
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
#ifndef STATIC_EXEC_PATH
    free(exec_path);
#endif
#if defined(_WIN32) && !defined(STATIC_NQP_HOME)
    /* dirname's return value is either on the stack or is the same pointer
     * that was passed to it depending on the version of libc used, which leads
     * to double frees. */
    free(exec_dir_path);
#endif
    free(exec_dir_path_temp);
#ifndef STATIC_RAKUDO_HOME
    free(rakudo_home);
#else
    if (getenv("PERL6_HOME") || getenv("RAKUDO_HOME"))
        free(rakudo_home);
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
