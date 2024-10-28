#include <stdint.h>
#include <stdio.h>
#include <stdbool.h>

#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT extern
#endif


typedef struct {
	union {
		int64_t x;
	};
} Outer;

DLLEXPORT void prinx(Outer *out) {
	printf("Have %lx\n", out->x);
}
