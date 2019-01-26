#include <stdlib.h>
#ifdef _WIN32
#define DLLEXPORT __declspec(dllexport)
#else
#define DLLEXPORT extern
#endif

#include <stdint.h>

struct Simple {
    int64_t bigint;
    int32_t smallint;
    float   smallnum;
};

DLLEXPORT float PassSimple(struct Simple x) {
    return x.smallnum * x.bigint * x.smallint;
}

DLLEXPORT struct Simple ReturnsSimple(float x) {
    struct Simple retval;
    retval.smallnum = x;
    return retval;
}

DLLEXPORT void TakeStructSimpleCopyCallback(void (*cb)(struct Simple), float x) {
    struct Simple s;
    s.smallnum = x;
    cb(s);
}

DLLEXPORT float CheckReturnsStructSimpleCopy(struct Simple (*cb)(int32_t, float), int32_t x, float y) {
    struct Simple r = cb(x, y);
    return r.smallnum * r.bigint;
}

struct Point {
    float x;
    float y;
};

struct Size {
    float width;
    float height;
};

struct Rect {
    struct Point origin;
    struct Size  size;
};

DLLEXPORT float PassRect(struct Rect obj) {
    return obj.origin.x + obj.origin.y + obj.size.width * obj.size.height;
}
