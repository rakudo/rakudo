#include <unistd.h>

#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT extern
#endif

DLLEXPORT int fib(int n) {
    return n <= 1 ? 1 : fib(n - 2) + fib(n - 1);
}

DLLEXPORT void hang_around() {
    while (1)
        sleep(1);
}
