/* This file contains the code for the executable runner template. The
 * executable is the basis for the script wrappers on Windows that Rakudo
 * generates for all scripts it installs. On Linux a simple shell wrapper
 * is used instead. So `share/perl6/site/bin/zef.exe` will be an instance of
 * this code.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <windows.h>

#include "whereami.h"
#include "exec_size.h"

#define CHAR wchar_t
#define strlen wcslen
#define strcpy wcscpy
#define strcat wcscat
#define strchr wcschr
#define fopen _wfopen
#define putenv _wputenv

const int marker_size = 34;
const wchar_t *marker = L"EXEC_RUNNER_WRAPPER_CONFIG_MARKER";

const int failure_exit = 107;

// Loosely according to https://blogs.msdn.microsoft.com/twistylittlepassagesallalike/2011/04/23/everyone-quotes-command-line-arguments-the-wrong-way/
int argvQuote(wchar_t *in, wchar_t *out) {
    int ipos;
    int opos;
    int bs_count;
    int c;

    ipos = 0;
    opos = 0;

    if (!wcschr(in, L' ') && !wcschr(in, L'"') && !wcschr(in, L'\t') && !wcschr(in, L'\n') && !wcschr(in, L'\v')) {
        if (out) wcscpy(out, in);
        return wcslen(in) + 1;
    }

    if (out) out[opos] = L'\"';
    opos++;

    while (in[ipos] != 0) {
        bs_count = 0;
        while (in[ipos] != 0 && in[ipos] == L'\\') {
            ipos++;
            bs_count++;
        }

        if (in[ipos] == 0) {
            for (c = 0; c < (bs_count * 2); c++) {
                if (out) out[opos] = L'\\';
                opos++;
            }
            break;
        }
        else if (in[ipos] == L'\"') {
            for (c = 0; c < (bs_count * 2 + 1); c++) {
                if (out) out[opos] = L'\\';
                opos++;
            }
            if (out) out[opos] = in[ipos];
            opos++;
        }
        else {
            for (c = 0; c < bs_count; c++) {
                if (out) out[opos] = L'\\';
                opos++;
            }
            if (out) out[opos] = in[ipos];
            opos++;
        }

        ipos++;
    }

    if (out) out[opos] = L'\"';
    opos++;
    if (out) out[opos] = 0;
    opos++;

    return opos;
}

typedef enum {
    MISSING,
    PLAIN,
    CMD_ARGS,
    PLAT_SEP,
    ABS,
    ABS_SLASH,
} ArgOp;

long find_config(FILE *file_handle) {
    return EXEC_LEN;
}

int read_malloced_str(FILE *file_handle, CHAR **out_str) {
    unsigned short length;
    size_t read_size = fread(&length, sizeof(unsigned short), 1, file_handle);
    *out_str = malloc(length * sizeof(CHAR));
    read_size = fread(*out_str, sizeof(CHAR), length, file_handle);
    if (read_size != length) {
        return -1;
    }

    return read_size;
}

void change_separators(wchar_t *path, wchar_t slash) {
    wchar_t search  = slash ? '\\' : '/';
    wchar_t replace = slash ? '/'  : '\\';
    wchar_t *runner = strchr(path, search);
    while (runner) {
        *runner = replace;
        runner = strchr(runner, search);
    }
}

// cwd must have a trailing '/'
void make_relative_path_absolute(CHAR **path, CHAR *cwd) {
    CHAR *new_path = malloc((strlen(*path) + strlen(cwd) + 1) * sizeof(CHAR));
    strcpy(new_path, cwd);
    strcat(new_path, *path);
    free(*path);
    *path = new_path;
}

ArgOp read_argop(FILE *file_handle) {
    unsigned char opval;
    if (fread(&opval, sizeof(unsigned char), 1, file_handle) != 1) {
        fprintf(stderr, "EXEC_RUNNER_WRAPPER: Arg type not found. Aborting.\n");
        return failure_exit;
    }
    ArgOp op = opval;
    return op;
}

void process_path(ArgOp op, CHAR **path, CHAR *base) {
    switch (op) {
        case PLAT_SEP:
            change_separators(*path, 0);
            break;
        case ABS:
            make_relative_path_absolute(path, base);
            change_separators(*path, 0);
            break;
        case ABS_SLASH:
            make_relative_path_absolute(path, base);
            change_separators(*path, 1);
            break;
    }
}

int wmain(int argc, wchar_t *argv[]) {
    STARTUPINFOW si;
    PROCESS_INFORMATION pi;
    ArgOp op;

    CHAR **exec_env = {NULL};

    FILE *file_handle;
    size_t read_size;
    int c, d;

    // Retrieve own path
    int my_path_len = wai_getExecutablePath(NULL, 0, NULL);
    int my_dir_name_len;
    char *my_path_char = calloc(my_path_len, sizeof(char*));
    wai_getExecutablePath(my_path_char, my_path_len, &my_dir_name_len);
    int my_path_len_wchar = MultiByteToWideChar(CP_UTF8, 0, my_path_char, -1, NULL, 0);
    wchar_t *my_path = calloc(my_path_len_wchar, sizeof(wchar_t*));
    my_path_len_wchar = MultiByteToWideChar(CP_UTF8, 0, my_path_char, -1, my_path, my_path_len_wchar);
    free(my_path_char);
    if (my_path_len_wchar == 0) return failure_exit;
    

    // Find config
    file_handle = fopen(my_path, L"r");
    if (!file_handle) {
        fprintf(stderr, "EXEC_RUNNER_WRAPPER: Couldn't open executable file for reading. Aborting.\n");
        return failure_exit;
    }
    long config_offset = find_config(file_handle);
    fseek(file_handle, config_offset, SEEK_SET);

    CHAR *marker_buf = calloc(marker_size, sizeof(CHAR));
    read_size = fread(marker_buf, sizeof(CHAR), marker_size, file_handle);
    if (read_size != marker_size) {
        fprintf(stderr, "EXEC_RUNNER_WRAPPER: Config not found. Aborting.\n");
        return failure_exit;
    }
    if (memcmp(marker, marker_buf, marker_size) != 0) {
        fprintf(stderr, "EXEC_RUNNER_WRAPPER: Marker not found. Aborting.\n");
        return failure_exit;
    }
    free(marker_buf);

    // Trim executable path to directory name
    my_path[my_dir_name_len + 1] = '\0';

    // Read program
    op = read_argop(file_handle);
    CHAR *program;
    if (read_malloced_str(file_handle, &program) < 0) return failure_exit;

    if (op == PLAIN && wcspbrk(program, L"/") == NULL) {
        SetSearchPathMode(BASE_SEARCH_PATH_ENABLE_SAFE_SEARCHMODE);
        DWORD prog_path_buf_len = SearchPathW(NULL, program, NULL, 0, NULL, NULL);
        if (prog_path_buf_len == 0) {
            fprintf(stderr, "EXEC_RUNNER_WRAPPER: Couldn't find program. Aborting.\n");
            return failure_exit;
        }
        CHAR *prog_path_buf = calloc(prog_path_buf_len, sizeof(CHAR));
        if (SearchPathW(NULL, program, NULL, prog_path_buf_len, prog_path_buf, NULL) == 0) {
            fprintf(stderr, "EXEC_RUNNER_WRAPPER: Couldn't find program. Aborting.\n");
            return failure_exit;
        }
        free(program);
        program = prog_path_buf;
    }
    else {
        process_path(op, &program, my_path);
    }

    // Read arg0 exists
    char has_arg0;
    if (fread(&has_arg0, sizeof(char), 1, file_handle) != 1) {
        fprintf(stderr, "EXEC_RUNNER_WRAPPER: Has arg0 flag not found. Aborting.\n");
        return failure_exit;
    }

    // Read arg0
    CHAR *arg0;
    if (has_arg0 == 1)
        if (read_malloced_str(file_handle, &arg0) < 0) return failure_exit;

    // Read config arg count
    unsigned short config_argc;
    if (fread(&config_argc, sizeof(unsigned short), 1, file_handle) != 1) {
        fprintf(stderr, "EXEC_RUNNER_WRAPPER: Arg count not found. Aborting.\n");
        return failure_exit;
    }

    // First arg is the program name, last arg is a NULL.
    unsigned short exec_argc = 1 + config_argc + 1;
    CHAR **exec_argv = calloc(exec_argc, sizeof(void*));

    // Set program name.
    exec_argv[0] = has_arg0 ? arg0 : program;

    int exec_pos = 1;
    for (c = 0; c < config_argc; c++) {
        op = read_argop(file_handle);

        if (op == CMD_ARGS) {
            // Copy passed args (without program name).
            // - 2 because the <cmd_args> entry in the config_args must not be counted
            // and the program name (in argv0) isn't copied.
            exec_argc += argc - 2;
            exec_argv = realloc(exec_argv, exec_argc * sizeof(void*));
            for (d = 1; d < argc; d++) {
                exec_argv[exec_pos] = argv[d];
                exec_pos++;
            }
        }
        else {
            if (read_malloced_str(file_handle, &exec_argv[exec_pos]) < 0) return failure_exit;
            process_path(op, &exec_argv[exec_pos], my_path);
            exec_pos++;
        }
    }

    exec_argv[exec_argc - 1] = NULL;

    // Read CWD
    op = read_argop(file_handle);
    CHAR *cwd = NULL;
    if (op != MISSING) {
        if (read_malloced_str(file_handle, &cwd) < 0) return failure_exit;
        process_path(op, &cwd, my_path);
    }

    // Read config environment variable count
    unsigned short config_envc;
    if (fread(&config_envc, sizeof(unsigned short), 1, file_handle) != 1) {
        fprintf(stderr, "EXEC_RUNNER_WRAPPER: Env count not found. Aborting.\n");
        return failure_exit;
    }

    // Read config environment vars
    CHAR **config_envv = calloc(config_envc, sizeof(CHAR *));
    for (int c = 0; c < config_envc; c++) {
        if (read_malloced_str(file_handle, &config_envv[c]) < 0) return failure_exit;
    }

    // Read config environment ignore variable count
    unsigned short config_env_ignorec;
    if (fread(&config_env_ignorec, sizeof(unsigned short), 1, file_handle) != 1) {
        fprintf(stderr, "EXEC_RUNNER_WRAPPER: Env ignore count not found. Aborting.\n");
        return failure_exit;
    }

    // Read config environment ignore vars
    CHAR **config_env_ignorev = calloc(config_env_ignorec, sizeof(CHAR *));
    for (c = 0; c < config_env_ignorec; c++) {
        if (read_malloced_str(file_handle, &config_env_ignorev[c]) < 0) return failure_exit;
    }

    fclose(file_handle);

    // Put the new environment together.
    int buflen = 200; 
    wchar_t *buf = calloc(buflen, sizeof(wchar_t));
    for (c = 0; c < config_env_ignorec; c++) {
        while (wcslen(config_env_ignorev[c]) > buflen - 1) {
            buflen *= 2;
            buf = realloc(buf, buflen * sizeof(wchar_t));
        }
        wcscpy(buf, config_env_ignorev[c]);
        free(config_env_ignorev[c]);
        wcscat(buf, L"=");
        if (_wputenv(buf) != 0) return failure_exit;
    }
    free(buf);
    free(config_env_ignorev);

    for (c = 0; c < config_envc; c++) {
        putenv(config_envv[c]);
        // Must not free config_envv[c]. That string is not copied, but now
        // part of the environment.
        //free(config_envv[c]);
    }
    free(config_envv);

    free(my_path);

    // Encode program name
    size_t arg_size = argvQuote(program, NULL);
    wchar_t *program_encoded = calloc(arg_size, sizeof(wchar_t));
    argvQuote(program, program_encoded);
    
    // Put command line string together
    // Size of the final command line string:
    // cmd_line_size = size of each argument + one space each - the last space + the trailing \0
    size_t cmd_line_size = 0;
    wchar_t *cmd_line = NULL;
    // -1 because we must ignore the last element, a NULL.
    for (c = 0; c < exec_argc - 1; c++) {
        // argvQuote leaves space for a trailing \0
        arg_size = argvQuote(exec_argv[c], NULL);
        cmd_line = realloc(cmd_line, (cmd_line_size + arg_size) * sizeof(wchar_t));
        argvQuote(exec_argv[c], cmd_line + cmd_line_size);
        // Can't just free exec_argv[c]. They are possibly not malloced, but from argv.
        //free(exec_argv[c]);
        cmd_line_size += arg_size;
        cmd_line[cmd_line_size - 1] = L' ';
    }
    free(program);
    free(exec_argv);
    cmd_line[cmd_line_size - 1] = 0;

    // Execute the command and wait for it to finish.
    ZeroMemory(&si, sizeof(si));
    si.cb = sizeof(si);

    ZeroMemory(&pi, sizeof(pi));

    BOOL success = CreateProcessW(
        program_encoded, // ApplicationName
        cmd_line,        // CommandLine
        NULL,            // ProcessAttributes
        NULL,            // ThreadAttributes
        FALSE,           // InheritHandles
        0,               // CreationFlags
        NULL,            // Environment
        cwd,             // CurrentDirectory
        &si,             // StartupInfo
        &pi);            // ProcessInformation

    if (!success) {
        fwprintf(stderr, L"EXEC_RUNNER_WRAPPER: Failed to execute %s. Error code: %ld\n", program_encoded, GetLastError());
        return failure_exit;
    }

    // I guess processes only signal when they are done. So no need to check the return value.
    WaitForSingleObject(pi.hProcess, INFINITE);
    DWORD exit_code;
    success = GetExitCodeProcess(pi.hProcess, &exit_code);

    CloseHandle(pi.hProcess);
    CloseHandle(pi.hThread);

    if (!success) {
        fwprintf(stderr, L"EXEC_RUNNER_WRAPPER: Couldn't retrieve exit code. Error code: %ld\n", GetLastError());
        return failure_exit;
    }

    return exit_code;
}

