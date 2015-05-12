use nqp;

module NativeCall;

# Throwaway type just to get us some way to get at the NativeCall
# representation.
my class native_callsite is repr('NativeCall') { }

# Maps a chosen string encoding to a type recognized by the native call engine.
sub string_encoding_to_nci_type($enc) {
    given $enc {
        when 'utf8'  { 'utf8str'  }
        when 'utf16' { 'utf16str' }
        when 'ascii' { 'asciistr' }
        default      { die "Unknown string encoding for native call: $enc"; }
    }
}

# Builds a hash of type information for the specified parameter.
sub param_hash_for(Parameter $p, :$with-typeobj) {
    my Mu $result := nqp::hash();
    my $type := $p.type();
    nqp::bindkey($result, 'typeobj', nqp::decont($type)) if $with-typeobj;
    nqp::bindkey($result, 'rw', nqp::unbox_i(1)) if $p.rw;
    if $type ~~ Str {
        my $enc := $p.?native_call_encoded() || 'utf8';
        nqp::bindkey($result, 'type', nqp::unbox_s(string_encoding_to_nci_type($enc)));
        nqp::bindkey($result, 'free_str', nqp::unbox_i(1));
    }
    elsif $type ~~ Callable {
        nqp::bindkey($result, 'type', nqp::unbox_s(type_code_for($p.type)));
        my $info := param_list_for($p.sub_signature, :with-typeobj);
        nqp::unshift($info, return_hash_for($p.sub_signature, :with-typeobj));
        nqp::bindkey($result, 'callback_args', $info);
    }
    else {
        nqp::bindkey($result, 'type', nqp::unbox_s(type_code_for($p.type)));
    }
    $result
}

# Builds the list of parameter information for a callback argument.
sub param_list_for(Signature $sig, :$with-typeobj) {
    my Mu $arg_info := nqp::list();
    for $sig.params -> $p {
        nqp::push($arg_info, param_hash_for($p, :with-typeobj($with-typeobj)))
    }

    $arg_info;
}

# Builds a hash of type information for the specified return type.
sub return_hash_for(Signature $s, &r?, :$with-typeobj) {
    my Mu $result := nqp::hash();
    my $returns := $s.returns;
    nqp::bindkey($result, 'typeobj', nqp::decont($returns)) if $with-typeobj;
    if $returns ~~ Str {
        my $enc := &r.?native_call_encoded() || 'utf8';
        nqp::bindkey($result, 'type', nqp::unbox_s(string_encoding_to_nci_type($enc)));
        nqp::bindkey($result, 'free_str', nqp::unbox_i(0));
    }
    # TODO: If we ever want to handle function pointers returned from C, this
    # bit of code needs to handle that.
    else {
        nqp::bindkey($result, 'type',
            $returns =:= Mu ?? 'void' !! nqp::unbox_s(type_code_for($returns)));
    }
    $result
}

my native long      is Int is ctype("long")                 is repr("P6int") is export(:types, :DEFAULT) { };
my native longlong  is Int is ctype("longlong")             is repr("P6int") is export(:types, :DEFAULT) { };
my native ulong     is Int is ctype("long")     is unsigned is repr("P6int") is export(:types, :DEFAULT) { };
my native ulonglong is Int is ctype("longlong") is unsigned is repr("P6int") is export(:types, :DEFAULT) { };
my class void                                  is repr('Uninstantiable') is export(:types, :DEFAULT) { };
# Expose a Pointer class for working with raw pointers.
my class Pointer                               is repr('CPointer') is export(:types, :DEFAULT) { };

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
            unless t ~~ Int || t ~~ Num || t === Str || t === void || t.REPR eq 'CStruct' | 'CUnion' | 'CPPStruct' | 'CPointer' | 'CArray';
        my \typed := TypedPointer[t];
        typed.^inheritalize;
    }
}
my constant OpaquePointer is export(:types, :DEFAULT) = Pointer;

# Gets the NCI type code to use based on a given Perl 6 type.
my %type_map =
    'int8'     => 'char',
    'int16'    => 'short',
    'int32'    => 'int',
    'int64'    => 'longlong',
    'long'     => 'long',
    'int'      => 'long',
    'longlong' => 'longlong',
    'Int'      => 'longlong',
    'uint8'    => 'uchar',
    'uint16'   => 'ushort',
    'uint32'   => 'uint',
    'uint64'   => 'ulonglong',
    'ulonglong' => 'ulonglong',
    'ulong'    => 'ulong',
    'uint'     => 'ulong',
    'num32'    => 'float',
    'num64'    => 'double',
    'num'      => 'double',
    'Num'      => 'double',
    'Callable' => 'callback';

my %repr_map =
    'CStruct'   => 'cstruct',
    'CPointer'  => 'cpointer',
    'CArray'    => 'carray',
    'CUnion'    => 'cunion',
    'VMArray'   => 'vmarray',
    ;
sub type_code_for(Mu ::T) {
    return %type_map{T.^name}
        if %type_map{T.^name}:exists;
    if %repr_map{T.REPR} -> $mapped {
        return $mapped;
    }
    # the REPR of a Buf or Blob type object is Uninstantiable, so
    # needs an extra special case here that isn't covered in the
    # hash lookup above.
    return 'vmarray'  if T ~~ Blob;
    return 'cpointer' if T ~~ Pointer;
    die "Unknown type {T.^name} used in native call.\n" ~
        "If you want to pass a struct, be sure to use the CStruct representation.\n" ~
        "If you want to pass an array, be sure to use the CArray type.";
}

multi sub map_return_type(Mu $type) { Mu }
multi sub map_return_type($type) {
    nqp::istype($type, Int) ?? Int
                            !! nqp::istype($type, Num) ?? Num !! $type;
}

my role NativeCallSymbol[Str $name] {
    method native_symbol()  { $name }
}

sub guess_library_name($lib) {
    my $libname;
    if ($lib ~~ Callable) {
        $libname = $lib();
    }
    else {
        $libname = $lib;
    }

    if !$libname.DEFINITE { '' }
    elsif $libname ~~ /\.<.alpha>+$/ { $libname }
    elsif $libname ~~ /\.so(\.<.digit>+)+$/ { $libname }
    elsif $*VM.config<load_ext> :exists { $libname ~ $*VM.config<load_ext> }
    elsif $*VM.config<nativecall.so> :exists {
        if $*KERNEL.name eq 'darwin' {
            ($libname ~ '.' ~ $*VM.config<nativecall.so>).IO.absolute
        }
        else {
            $libname ~ '.' ~ $*VM.config<nativecall.so>
        }
    }
    elsif $*VM.config<dll> :exists {
        my $ext = $*VM.config<dll>;
        $ext ~~ s/^.*\%s//;
        "$libname$ext";
    }
    elsif $*DISTRO.is-win { "{$libname}.dll"; }
    # TODO: more extension guessing
    else { "{$libname}.so"; }
}

# This role is mixed in to any routine that is marked as being a
# native call.
my role Native[Routine $r, $libname where Str|Callable] {
    has int $!setup;
    has native_callsite $!call is box_target;
    has Mu $!rettype;

    method CALL-ME(|args) {
        unless $!setup {
            my Mu $arg_info := param_list_for($r.signature);
            my str $conv = self.?native_call_convention || '';
            nqp::buildnativecall(self,
                nqp::unbox_s(guess_library_name($libname)),    # library name
                nqp::unbox_s(self.?native_symbol // $r.name),      # symbol to call
                nqp::unbox_s($conv),        # calling convention
                $arg_info,
                return_hash_for($r.signature, $r));
            $!setup = 1;
            $!rettype := nqp::decont(map_return_type($r.returns));
        }

        my Mu $args := nqp::getattr(nqp::decont(args), Capture, '$!list');
        if nqp::elems($args) != $r.signature.arity {
            X::TypeCheck::Argument.new(
                :objname($.name),
                :arguments(nqp::p6list($args, Array, Mu).map(*.^name))
                :signature("    Expected: " ~ try $r.signature.perl),
            ).throw
        }

        nqp::nativecall($!rettype, self, $args)
    }
}

# Role for carrying extra calling convention information.
my role NativeCallingConvention[$name] {
    method native_call_convention() { $name };
}

# Role for carrying extra string encoding information.
my role NativeCallEncoded[$name] {
    method native_call_encoded() { $name };
}

# CArray class, used to represent C arrays.
my class CArray is export(:types, :DEFAULT) is repr('CArray') is array_type(Pointer) { };

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

multi sub postcircumfix:<[ ]>(CArray:D \array, $pos) is export(:DEFAULT, :types) {
    $pos ~~ Iterable ?? $pos.map: { array.AT-POS($_) } !! array.AT-POS($pos);
}
multi sub postcircumfix:<[ ]>(CArray:D \array, *@pos) is export(:DEFAULT, :types) {
    @pos.map: { array.AT-POS($_) };
}


multi trait_mod:<is>(Routine $r, :$symbol!) is export(:DEFAULT, :traits) {
    $r does NativeCallSymbol[$symbol];
}

# Specifies that the routine is actually a native call, into the
# current executable (platform specific) or into a named library
multi trait_mod:<is>(Routine $r, :$native!) is export(:DEFAULT, :traits) {
    $r does Native[$r, $native === True ?? Str !! $native];
}

# Specifies the calling convention to use for a native call.
multi trait_mod:<is>(Routine $r, :$nativeconv!) is export(:DEFAULT, :traits) {
    $r does NativeCallingConvention[$nativeconv];
}

# Ways to specify how to marshall strings.
multi trait_mod:<is>(Parameter $p, :$encoded!) is export(:DEFAULT, :traits) {
    $p does NativeCallEncoded[$encoded];
}
multi trait_mod:<is>(Routine $p, :$encoded!) is export(:DEFAULT, :traits) {
    $p does NativeCallEncoded[$encoded];
}

multi trait_mod:<is>(Attribute $a, :$inlined!) is export(:DEFAULT, :traits) {
    nqp::bindattr_i(nqp::decont($a), $a.WHAT, '$!inlined', 1);
}

role ExplicitlyManagedString {
    has $.cstr is rw;
}

multi explicitly-manage(Str $x is rw, :$encoding = 'utf8') is export(:DEFAULT,
:utils) {
    $x does ExplicitlyManagedString;
    my $class = class CStr is repr('CStr') { method encoding() { $encoding; } };
    $x.cstr = nqp::box_s(nqp::unbox_s($x), nqp::decont($class));
}

multi refresh($obj) is export(:DEFAULT, :utils) {
    nqp::nativecallrefresh($obj);
    1;
}

sub nativecast($target-type, $source) is export(:DEFAULT) {
    nqp::nativecallcast(nqp::decont($target-type),
        nqp::decont(map_return_type($target-type)), nqp::decont($source));
}

sub nativesizeof($obj) is export(:DEFAULT) {
    nqp::nativecallsizeof($obj)
}

sub cglobal($libname, $symbol, $target-type) is export is rw {
    Proxy.new(
        FETCH => -> $ {
            nqp::nativecallglobal(
                nqp::unbox_s(guess_library_name($libname)),
                nqp::unbox_s($symbol),
                nqp::decont($target-type),
                nqp::decont(map_return_type($target-type)))
        },
        STORE => -> | { die "Writing to C globals NYI" }
    )
}

# vim:ft=perl6
