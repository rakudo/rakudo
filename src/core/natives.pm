my native int is repr('P6int') is Int { }
my native int1 is repr('P6int') is Int is nativesize(1) { }
my native int2 is repr('P6int') is Int is nativesize(2) { }
my native int4 is repr('P6int') is Int is nativesize(4) { }
my native int8 is repr('P6int') is Int is nativesize(8) { }
my native int16 is repr('P6int') is Int is nativesize(16) { }
my native int32 is repr('P6int') is Int is nativesize(32) { }

my native num is repr('P6num') is Num { }
my native num32 is repr('P6num') is Num is nativesize(32) { }
my native num64 is repr('P6num') is Num is nativesize(64) { }

my native str is repr('P6str') is Str { }
