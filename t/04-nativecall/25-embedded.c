#include <stdint.h>
#include <stdio.h>
#include <stdbool.h>

typedef struct {
	union {
		int64_t x;
	};
} Outer;

void prinx(Outer *out) {
	printf("Have %lx\n", out->x);
}
