use nqp;
unit module NativeCall::Types;

sub nativecast($target-type, $source) {
    nqp::nativecallcast(nqp::decont($target-type),
        nqp::decont(map_return_type($target-type)), nqp::decont($source));
}

our native long     is Int is ctype("long")     is repr("P6int") { };
our native longlong is Int is ctype("longlong") is repr("P6int") { };
our native ulong     is Int is ctype("long")     is unsigned is repr("P6int") { };
our native ulonglong is Int is ctype("longlong") is unsigned is repr("P6int") { };
our native size_t    is Int is ctype("size_t")   is unsigned is repr("P6int") { };
our native ssize_t   is Int is ctype("size_t")               is repr("P6int") { };
our native bool      is Int is ctype("bool")                 is repr("P6int") { };
our class void                                  is repr('Uninstantiable') { };
# Expose a Pointer class for working with raw pointers.
our class Pointer                               is repr('CPointer') {
    method of() { void }

    multi method new() {
        self.CREATE()
    }
    multi method new(int $addr) {
        nqp::box_i($addr, ::?CLASS)
    }
    multi method new(Int $addr) {
        nqp::box_i(nqp::unbox_i(nqp::decont($addr)), ::?CLASS)
    }

    method Numeric(::?CLASS:D:) { self.Int }
    method Int(::?CLASS:D:) {
        nqp::p6box_i(nqp::unbox_i(self))
    }

    proto method Bool() {*}
    multi method Bool(::?CLASS:U: --> False) { }
    multi method Bool(::?CLASS:D:) { so self.Int }


    method deref(::?CLASS:D \ptr:) { self ?? nativecast(void, ptr) !! fail("Can't dereference a Null Pointer") }

    multi method gist(::?CLASS:U:) { '(' ~ self.^name ~ ')' }
    multi method gist(::?CLASS:D:) {
        if self.Int -> $addr {
            self.^name ~ '<' ~ $addr.fmt('%#x') ~ '>'
        }
        else {
            self.^name ~ '<NULL>'
        }
    }

    multi method raku(::?CLASS:U:) { self.^name }
    multi method raku(::?CLASS:D:) { self.^name ~ '.new(' ~ self.Int ~ ')' }

    my role TypedPointer[::TValue] {
        method of() { TValue }
        method deref(::?CLASS:D \ptr:) { self ?? nativecast(TValue, ptr) !! fail("Can't dereference a Null Pointer"); }
        method add(Int $off) returns Pointer {
            die "Can't do arithmetic with a void pointer"
                if TValue.isa(void);
            nqp::box_i(self.Int + nqp::nativecallsizeof(TValue) * $off, self.WHAT);
        }
        method succ {
            self.add(1);
        }
        method pred {
            self.add(-1);
        }
        method AT-POS(Int $pos) {
            nqp::nativecallcast(
                TValue,
                nqp::istype(TValue, Int) ?? Int
                                         !! nqp::istype(TValue, Num) ?? Num !! TValue,
                nqp::box_i(nqp::unbox_i(self) + nqp::nativecallsizeof(TValue) * $pos, Pointer)
            )
        }
    }
    method ^parameterize(Mu:U \p, Mu:U \t) {
        die "A typed pointer can only hold:\n" ~
            "  (u)int8, (u)int16, (u)int32, (u)int64, (u)long, (u)longlong, num16, num32, (s)size_t, bool, Str\n" ~
            "  and types with representation: CArray, CPointer, CStruct, CPPStruct and CUnion" ~
            "not: {t.^name}"
            unless t ~~ Int|Num|Bool || t === Str|void || t.REPR eq any <CStruct CUnion CPPStruct CPointer CArray>;
        my $w := p.^mixin: TypedPointer[t.WHAT];
        $w.^set_name: "{p.^name}[{t.^name}]";
        $w;
    }
}

# CArray class, used to represent C arrays.
our class CArray is repr('CArray') is array_type(Pointer) {
    method AT-POS(::?CLASS:D: $pos) { die "CArray cannot be used without a type" }

    my role IntTypedCArray[::TValue] does Positional[TValue] is array_type(TValue) {
        multi method AT-POS(::?CLASS:D \arr: $pos) is raw {
            nqp::atposref_i(nqp::decont(arr), $pos);
        }
        multi method AT-POS(::?CLASS:D \arr: Int $pos) is raw {
            nqp::atposref_i(nqp::decont(arr), $pos);
        }
        multi method ASSIGN-POS(::?CLASS:D \arr: Int $pos, int $assignee) {
            nqp::bindpos_i(nqp::decont(arr), nqp::unbox_i($pos), $assignee);
        }
        multi method ASSIGN-POS(::?CLASS:D \arr: Int $pos, Int $assignee) {
            nqp::bindpos_i(nqp::decont(arr), nqp::unbox_i($pos), nqp::unbox_i($assignee));
        }

        multi method allocate(::?CLASS:U \type: int $elems) {
            my $arr := nqp::create(type);
            nqp::bindpos_i($arr, $elems - 1, 0);
            $arr;
        }
        multi method allocate(::?CLASS:U \type: Int:D $elems) {
            my $arr := nqp::create(type);
            nqp::bindpos_i($arr, $elems - 1, 0);
            $arr;
        }
    }

    my role NumTypedCArray[::TValue] does Positional[TValue] is array_type(TValue) {
        multi method AT-POS(::?CLASS:D \arr: $pos) is raw {
            nqp::atposref_n(nqp::decont(arr), $pos);
        }
        multi method AT-POS(::?CLASS:D \arr: Int $pos) is raw {
            nqp::atposref_n(nqp::decont(arr), $pos);
        }
        multi method ASSIGN-POS(::?CLASS:D \arr: Int $pos, num $assignee) {
            nqp::bindpos_n(nqp::decont(arr), nqp::unbox_i($pos), $assignee);
        }
        multi method ASSIGN-POS(::?CLASS:D \arr: Int $pos, Num $assignee) {
            nqp::bindpos_n(nqp::decont(arr), nqp::unbox_i($pos), nqp::unbox_n($assignee));
        }

        multi method allocate(::?CLASS:U \type: int $elems) {
            my $arr := nqp::create(type);
            nqp::bindpos_n($arr, $elems - 1, 0e0);
            $arr;
        }
        multi method allocate(::?CLASS:U \type: Int:D $elems) {
            my $arr := nqp::create(type);
            nqp::bindpos_n($arr, $elems - 1, 0e0);
            $arr;
        }
    }

    my role TypedCArray[::TValue] does Positional[TValue] is array_type(TValue) {
        multi method AT-POS(::?CLASS:D \arr: $pos) is rw {
            Proxy.new:
                FETCH => method () {
                    nqp::atpos(nqp::decont(arr), nqp::unbox_i($pos.Int))
                },
                STORE => method ($v) {
                    nqp::bindpos(nqp::decont(arr), nqp::unbox_i($pos.Int), nqp::decont($v));
                    self
                }
        }
        multi method AT-POS(::?CLASS:D \arr: Int $pos) is rw {
            Proxy.new:
                FETCH => method () {
                    nqp::atpos(nqp::decont(arr), $pos)
                },
                STORE => method ($v) {
                    nqp::bindpos(nqp::decont(arr), $pos, nqp::decont($v));
                    self
                }
        }
        multi method ASSIGN-POS(::?CLASS:D \arr: Int $pos, \assignee) {
            nqp::bindpos(nqp::decont(arr), nqp::unbox_i($pos), nqp::decont(assignee));
        }

        multi method allocate(::?CLASS:U: int $elems) {
            my $arr  := nqp::create(self);
            my $type := ::?CLASS.^array_type;
            nqp::bindpos($arr, $_, nqp::create($type)) for ^$elems;
            $arr;
        }
        multi method allocate(::?CLASS:U: Int:D $elems) {
            my $arr  := nqp::create(self);
            my $type := ::?CLASS.^array_type;
            nqp::bindpos($arr, $_, nqp::create($type)) for ^$elems;
            $arr;
        }
    }
    method ^parameterize(Mu:U \arr, Mu:U \t) {
        my $mixin;
        if t ~~ Int {
            $mixin := IntTypedCArray[t.WHAT];
        }
        elsif t ~~ Num {
            $mixin := NumTypedCArray[t.WHAT];
        }
        else {
            die "A C array can only hold:\n" ~
                "  (u)int8, (u)int16, (u)int32, (u)int64, (u)long, (u)longlong, num16, num32, (s)size_t, bool, Str\n" ~
                "  and types with representation: CArray, CPointer, CStruct, CPPStruct and CUnion\n" ~
                "not: {t.^name}"
                unless t === Str || t.REPR eq 'CStruct' | 'CPPStruct' | 'CUnion' | 'CPointer' | 'CArray';
            $mixin := TypedCArray[t];
        }
        my $what := arr.^mixin: $mixin;
        $what.^set_name("{arr.^name}[{t.^name}]");
        $what;
    }

    method Str { self.join(' ') }

    method elems { nqp::elems(self) }

    method list {
        do for ^self.elems { self.AT-POS($_) }
    }

    multi method new() { nqp::create(self) }
    multi method new(*@values) { self.new(@values) }
    multi method new(@values) {
        if @values.elems -> $n {
            my int $elems = $n - 1;
            my $result   := nqp::create(self);  # XXX setelems would be nice
            $result.ASSIGN-POS($elems,@values.AT-POS($elems)); # fake setelems
            my int $i = -1;
            nqp::while(
              nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
              $result.ASSIGN-POS($i,@values.AT-POS($i)),
            );
            $result
        }
        else {
            nqp::create(self)
        }
    }
}

# duplicated code from NativeCall.pm to support Pointer.deref
multi sub map_return_type(Mu $type) { Mu }
multi sub map_return_type($type) {
    nqp::istype($type, Int) ?? Int
                            !! nqp::istype($type, Num) ?? Num !! $type;
}

# vim: expandtab shiftwidth=4
