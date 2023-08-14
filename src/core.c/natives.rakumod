my native   int is repr('P6int') is Int { }
my native  int8 is repr('P6int') is Int is nativesize( 8) { }
my native int16 is repr('P6int') is Int is nativesize(16) { }
my native int32 is repr('P6int') is Int is nativesize(32) { }
my native int64 is repr('P6int') is Int is nativesize(64) { }

my native   uint is repr('P6int') is Int is unsigned { }
my native  uint8 is repr('P6int') is Int is nativesize( 8) is unsigned { }
my native   byte is repr('P6int') is Int is nativesize( 8) is unsigned { }
my native uint16 is repr('P6int') is Int is nativesize(16) is unsigned { }
my native uint32 is repr('P6int') is Int is nativesize(32) is unsigned { }
my native uint64 is repr('P6int') is Int is nativesize(64) is unsigned { }

my native   num is repr('P6num') is Num { }
my native num32 is repr('P6num') is Num is nativesize(32) { }
my native num64 is repr('P6num') is Num is nativesize(64) { }

my native   str is repr('P6str') is Str { }

# vim: expandtab shiftwidth=4
