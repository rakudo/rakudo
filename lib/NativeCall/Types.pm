use nqp;
unit module NativeCall::Types;

our native long     is Int is ctype("long")     is repr("P6int") { };
our native longlong is Int is ctype("longlong") is repr("P6int") { };
our native ulong     is Int is ctype("long")     is unsigned is repr("P6int") { };
our native ulonglong is Int is ctype("longlong") is unsigned is repr("P6int") { };
our class void                                  is repr('Uninstantiable') { };
# Expose a Pointer class for working with raw pointers.
our class Pointer                               is repr('CPointer') { };

# need to introduce the roles in there in an augment, because you can't
# inherit from types that haven't been properly composed.
use MONKEY-TYPING;
augment class Pointer {
    method of() { void }

    method ^name($) { 'Pointer' }

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
        nqp::p6box_i(nqp::unbox_i(nqp::decont(self)))
    }

    method deref(::?CLASS:D \ptr:) { nativecast(void, ptr) }

    multi method gist(::?CLASS:U:) { '(' ~ self.^name ~ ')' }
    multi method gist(::?CLASS:D:) {
        if self.Int -> $addr {
            self.^name ~ '<' ~ $addr.fmt('%#x') ~ '>'
        }
        else {
            self.^name ~ '<NULL>'
        }
    }

    multi method perl(::?CLASS:U:) { self.^name }
    multi method perl(::?CLASS:D:) { self.^name ~ '.new(' ~ self.Int ~ ')' }

    my role TypedPointer[::TValue = void] is Pointer is repr('CPointer') {
        method of() { TValue }
        # method ^name($obj) { 'Pointer[' ~ TValue.^name ~ ']' }
        method deref(::?CLASS:D \ptr:) { nativecast(TValue, ptr) }
    }
    method ^parameterize($, Mu:U \t) {
        die "A typed pointer can only hold integers, numbers, strings, CStructs, CPointers or CArrays (not {t.^name})"
            unless t ~~ Int|Num|Bool || t === Str|void || t.REPR eq any <CStruct CUnion CPPStruct CPointer CArray>;
        my \typed := TypedPointer[t];
        typed.^inheritalize;
    }
}

# CArray class, used to represent C arrays.
our class CArray is repr('CArray') is array_type(Pointer) { };

# need to introduce the roles in there in an augment, because you can't 
# inherit from types that haven't been properly composed.
use MONKEY-TYPING;
augment class CArray {
    method AT-POS(CArray:D: $pos) { die "CArray cannot be used without a type" }

    my role IntTypedCArray[::TValue] does Positional[TValue] is CArray is repr('CArray') is array_type(TValue) {
        multi method AT-POS(::?CLASS:D \arr: $pos) is rw {
            Proxy.new:
                FETCH => method () {
                    nqp::p6box_i(nqp::atpos_i(nqp::decont(arr), nqp::unbox_i($pos.Int)))
                },
                STORE => method (int $v) {
                    nqp::bindpos_i(nqp::decont(arr), nqp::unbox_i($pos.Int), $v);
                    self
                }
        }
        multi method AT-POS(::?CLASS:D \arr: int $pos) is rw {
            Proxy.new:
                FETCH => method () {
                    nqp::p6box_i(nqp::atpos_i(nqp::decont(arr), $pos))
                },
                STORE => method (int $v) {
                    nqp::bindpos_i(nqp::decont(arr), $pos, $v);
                    self
                }
        }
        multi method ASSIGN-POS(::?CLASS:D \arr: int $pos, int $assignee) {
            nqp::bindpos_i(nqp::decont(arr), $pos, $assignee);
        }
        multi method ASSIGN-POS(::?CLASS:D \arr: Int $pos, int $assignee) {
            nqp::bindpos_i(nqp::decont(arr), nqp::unbox_i($pos), $assignee);
        }
        multi method ASSIGN-POS(::?CLASS:D \arr: Int $pos, Int $assignee) {
            nqp::bindpos_i(nqp::decont(arr), nqp::unbox_i($pos), nqp::unbox_i($assignee));
        }
        multi method ASSIGN-POS(::?CLASS:D \arr: int $pos, Int $assignee) {
            nqp::bindpos_i(nqp::decont(arr), $pos, nqp::unbox_i($assignee));
        }
    }

    my role NumTypedCArray[::TValue] does Positional[TValue] is CArray is repr('CArray') is array_type(TValue) {
        multi method AT-POS(::?CLASS:D \arr: $pos) is rw {
            Proxy.new:
                FETCH => method () {
                    nqp::p6box_n(nqp::atpos_n(nqp::decont(arr), nqp::unbox_i($pos.Int)))
                },
                STORE => method (num $v) {
                    nqp::bindpos_n(nqp::decont(arr), nqp::unbox_i($pos.Int), $v);
                    self
                }
        }
        multi method AT-POS(::?CLASS:D \arr: int $pos) is rw {
            Proxy.new:
                FETCH => method () {
                    nqp::p6box_n(nqp::atpos_n(nqp::decont(arr), $pos))
                },
                STORE => method (num $v) {
                    nqp::bindpos_n(nqp::decont(arr), $pos, $v);
                    self
                }
        }
        multi method ASSIGN-POS(::?CLASS:D \arr: int $pos, num $assignee) {
            nqp::bindpos_n(nqp::decont(arr), $pos, $assignee);
        }
        multi method ASSIGN-POS(::?CLASS:D \arr: Int $pos, num $assignee) {
            nqp::bindpos_n(nqp::decont(arr), nqp::unbox_i($pos), $assignee);
        }
        multi method ASSIGN-POS(::?CLASS:D \arr: Int $pos, Num $assignee) {
            nqp::bindpos_n(nqp::decont(arr), nqp::unbox_i($pos), nqp::unbox_n($assignee));
        }
        multi method ASSIGN-POS(::?CLASS:D \arr: int $pos, Num $assignee) {
            nqp::bindpos_n(nqp::decont(arr), $pos, nqp::unbox_n($assignee));
        }
    }

    my role TypedCArray[::TValue] does Positional[TValue] is CArray is repr('CArray') is array_type(TValue) {
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
        multi method AT-POS(::?CLASS:D \arr: int $pos) is rw {
            Proxy.new:
                FETCH => method () {
                    nqp::atpos(nqp::decont(arr), $pos)
                },
                STORE => method ($v) {
                    nqp::bindpos(nqp::decont(arr), $pos, nqp::decont($v));
                    self
                }
        }
        multi method ASSIGN-POS(::?CLASS:D \arr: int $pos, \assignee) {
            nqp::bindpos(nqp::decont(arr), $pos, nqp::decont(assignee));
        }
        multi method ASSIGN-POS(::?CLASS:D \arr: Int $pos, \assignee) {
            nqp::bindpos(nqp::decont(arr), nqp::unbox_i($pos), nqp::decont(assignee));
        }
    }
    method ^parameterize($, Mu:U \t) {
        my $typed;
        if t ~~ Int {
            $typed := IntTypedCArray[t.WHAT];
        }
        elsif t ~~ Num {
            $typed := NumTypedCArray[t.WHAT];
        }
        else {
            die "A C array can only hold integers, numbers, strings, CStructs, CPointers or CArrays (not {t.^name})"
                unless t === Str || t.REPR eq 'CStruct' | 'CPointer' | 'CArray';
            $typed := TypedCArray[t];
        }
        $typed.^inheritalize();
    }
}

# duplicated code from NativeCall.pm to support Pointer.deref
multi sub map_return_type(Mu $type) { Mu }
multi sub map_return_type($type) {
    nqp::istype($type, Int) ?? Int
                            !! nqp::istype($type, Num) ?? Num !! $type;
}

sub nativecast($target-type, $source) {
    nqp::nativecallcast(nqp::decont($target-type),
        nqp::decont(map_return_type($target-type)), nqp::decont($source));
}
