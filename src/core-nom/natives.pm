# The native int type defualts to whatever a system int is. Note
# that we may run into some nasty issues when people try to use
# int64 on a system with a 32-bit definition of int.

my class Int { ... }
my native int is Int
    is repr('P6int')
    { }

my native int1 is Int
    is repr('P6int')
    is reprspec({ REPRSpec::NativeIntSpec.new(:bits(1)) })
    { }
my native int2 is Int
    is repr('P6int')
    is reprspec({ REPRSpec::NativeIntSpec.new(:bits(2)) })
    { }
my native int4 is Int
    is repr('P6int')
    is reprspec({ REPRSpec::NativeIntSpec.new(:bits(4)) })
    { }
my native int8 is Int
    is repr('P6int')
    is reprspec({ REPRSpec::NativeIntSpec.new(:bits(8)) })
    { }
my native int16 is Int
    is repr('P6int')
    is reprspec({ REPRSpec::NativeIntSpec.new(:bits(16)) })
    { }
my native int32 is Int
    is repr('P6int')
    is reprspec({ REPRSpec::NativeIntSpec.new(:bits(32)) })
    { }
my native int64 is Int
    is repr('P6int')
    is reprspec({ REPRSpec::NativeIntSpec.new(:bits(64)) })
    { }

my native uint1 is Int
    is repr('P6int')
    is reprspec({ REPRSpec::NativeIntSpec.new(:bits(1), :unsigned(1)) })
    { }
my native uint2 is Int
    is repr('P6int')
    is reprspec({ REPRSpec::NativeIntSpec.new(:bits(2), :unsigned(1)) })
    { }
my native uint4 is Int
    is repr('P6int')
    is reprspec({ REPRSpec::NativeIntSpec.new(:bits(4), :unsigned(1)) })
    { }
my native uint8 is Int
    is repr('P6int')
    is reprspec({ REPRSpec::NativeIntSpec.new(:bits(8), :unsigned(1)) })
    { }
my native uint16 is Int
    is repr('P6int')
    is reprspec({ REPRSpec::NativeIntSpec.new(:bits(16), :unsigned(1)) })
    { }
my native uint32 is Int
    is repr('P6int')
    is reprspec({ REPRSpec::NativeIntSpec.new(:bits(32), :unsigned(1)) })
    { }
my native uint64 is Int
    is repr('P6int')
    is reprspec({ REPRSpec::NativeIntSpec.new(:bits(64), :unsigned(1)) })
    { }

my class Num { ... }
my native num is Num
    is repr('P6num')
    { }

my native num32 is Num
    is repr('P6num')
    is reprspec({ REPRSpec::NativeNumSpec.new(:bits(32)) })
    { }
my native num64 is Num
    is repr('P6num')
    is reprspec({ REPRSpec::NativeNumSpec.new(:bits(64)) })
    { }
