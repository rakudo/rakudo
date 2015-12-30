use nqp;

module NativeCall {

use NativeCall::Types;
use NativeCall::Compiler::GNU;
use NativeCall::Compiler::MSVC;

my constant long          is export(:types, :DEFAULT) = NativeCall::Types::long;
my constant longlong      is export(:types, :DEFAULT) = NativeCall::Types::longlong;
my constant ulong         is export(:types, :DEFAULT) = NativeCall::Types::ulong;
my constant ulonglong     is export(:types, :DEFAULT) = NativeCall::Types::ulonglong;
my constant bool          is export(:types, :DEFAULT) = NativeCall::Types::bool;
my constant size_t        is export(:types, :DEFAULT) = NativeCall::Types::size_t;
my constant void          is export(:types, :DEFAULT) = NativeCall::Types::void;
my constant CArray        is export(:types, :DEFAULT) = NativeCall::Types::CArray;
my constant Pointer       is export(:types, :DEFAULT) = NativeCall::Types::Pointer;
my constant OpaquePointer is export(:types, :DEFAULT) = NativeCall::Types::Pointer;

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
sub param_list_for(Signature $sig, &r?, :$with-typeobj) {
    my Mu $arg_info := nqp::list();
    my @params = $sig.params;
    @params.pop if &r ~~ Method && @params[*-1].name eq '%_';
    for @params -> $p {
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

my %signed_ints_by_size =
    1 => 'int',
    2 => 'short',
    4 => 'int',
    8 => 'longlong',
;

# Gets the NCI type code to use based on a given Perl 6 type.
my %type_map =
    'int8'     => 'char',
    'bool'     => %signed_ints_by_size{nativesizeof(bool)},
    'Bool'     => %signed_ints_by_size{nativesizeof(bool)},
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
    'size_t'   => %signed_ints_by_size{nativesizeof(size_t)},
    'num32'    => 'float',
    'num64'    => 'double',
    'longdouble' => 'longdouble',
    'num'      => 'double',
    'Num'      => 'double',
    'Callable' => 'callback';

my %repr_map =
    'CStruct'   => 'cstruct',
    'CPPStruct' => 'cppstruct',
    'CPointer'  => 'cpointer',
    'CArray'    => 'carray',
    'CUnion'    => 'cunion',
    'VMArray'   => 'vmarray',
    ;
sub type_code_for(Mu ::T) {
    my $shortname = T.^shortname;
    return %type_map{$shortname}
        if %type_map{$shortname}:exists;
    if %repr_map{T.REPR} -> $mapped {
        return $mapped;
    }
    # the REPR of a Buf or Blob type object is Uninstantiable, so
    # needs an extra special case here that isn't covered in the
    # hash lookup above.
    return 'vmarray'  if T ~~ Blob;
    return 'cpointer' if T ~~ Pointer;
    die "Unknown type {T.^name} used in native call.\n" ~
        "If you want to pass a struct, be sure to use the CStruct or CPPStruct representation.\n" ~
        "If you want to pass an array, be sure to use the CArray type.";
}

sub gen_native_symbol(Routine $r, :$cpp-name-mangler) {
    if $r.package.REPR eq 'CPPStruct' {
        $cpp-name-mangler($r, $r.?native_symbol // ($r.package.^name ~ '::' ~ $r.name))
    }
    elsif $r.?native_call_mangled {
        $cpp-name-mangler($r, $r.?native_symbol // $r.name)
    }
    else {
        $r.?native_symbol // $r.name
    }
}

multi sub map_return_type(Mu $type) { Mu }
multi sub map_return_type($type) {
    nqp::istype($type, Int) ?? Int
                            !! nqp::istype($type, Num) ?? Num !! $type;
}

my role NativeCallSymbol[Str $name] {
    method native_symbol()  { $name }
}

sub guess_library_name($lib) is export(:TEST) {
    my $libname;
    my $apiversion = '';
    my Str $ext = '';
    given $lib {
        when Callable {
           return $lib();
        }
        when List {
           $libname = $lib[0];
           $apiversion = $lib[1];
        }
        when Str {
           $libname = $lib;
        }
    }
    return '' unless $libname.DEFINITE;
    #Already a full name?
    return $libname if ($libname ~~ /\.<.alpha>+$/ or $libname ~~ /\.so(\.<.digit>+)+$/);
    note "NativeCall: Consider adding the api version of the library you want to use, sub foo is native($libname, v1)" if $libname ~~ /^<-[\.\/\\]>+$/ and $apiversion eq '';

    return $*VM.platform-library-name($libname.IO, :version($apiversion || Version)).Str;
}

sub check_routine_sanity(Routine $r) is export(:TEST) {
    #Maybe this should use the hash already existing?
    sub validnctype (Mu ::T) {
      return True if %repr_map{T.REPR}:exists and T.REPR ne 'CArray' | 'CPointer';
      return True if T.^name eq 'Str' | 'str' | 'Bool';
      return False if T.REPR eq 'P6opaque';
      return False if T.HOW.^can("nativesize") && T.^nativesize == 0; #to disting int and int32 for example
      return validnctype(T.of) if T.REPR eq 'CArray' | 'CPointer' and T.^can('of');
      return True;
    }
    my $sig = $r.signature;
    for @($sig.params).kv -> $i, $param {
        next if $r ~~ Method and ($i < 1 or $i == $sig.params.elems - 1); #Method have two extra parameters
        if $param.type ~~ Callable {
          # We probably want to check the given routine type too here. but I don't know how
          next;
        }
        next unless $param.type ~~ Buf | Blob #Buf are Uninstantiable, make this buggy
        || $param.type.^can('gist'); #FIXME, it's to handle case of class A { sub foo(A) is native) }, the type is not complete
        if !validnctype($param.type) {
           die "In '{$r.name}' routine declaration - Not an accepted NativeCall type for parameter [{$i + 1}] {$param.name ?? $param.name !! ''} : {$param.type.^name}\n" ~
           "-->For Numerical type, use the appropriate int32/int64/num64...";
        }
    }
    return True if $r.returns.REPR eq 'CPointer' | 'CStruct' | 'CPPStruct'; #Meh fix but 'imcomplete' type are a pain
    if $r.returns.^name ne 'Mu' && !validnctype($r.returns) {
        die "The returning type of '{$r.name}' --> {$r.returns.^name} is errornous. You should not return a non NativeCall supported type (like Int inplace of int32), truncating errors can appear with different architectures";
    }
}

my %lib;
my @cpp-name-mangler =
    &NativeCall::Compiler::MSVC::mangle_cpp_symbol,
    &NativeCall::Compiler::GNU::mangle_cpp_symbol,
;

sub guess-name-mangler(Routine $r, Str $libname) {
    if $r.package.REPR eq 'CPPStruct' {
        my $sym = $r.?native_symbol // ($r.package.^name ~ '::' ~ $r.name);
        for @cpp-name-mangler -> &mangler {
            return &mangler if try cglobal($libname, mangler($r, $sym), Pointer)
        }
        die "Don't know how to mangle symbol '$sym' for library '$libname'"
    }
    elsif $r.?native_call_mangled {
        my $sym = $r.?native_symbol // $r.name;
        for @cpp-name-mangler -> &mangler {
            return &mangler if try cglobal($libname, mangler($r, $sym), Pointer)
        }
        die "Don't know how to mangle symbol '$sym' for library '$libname'"
    }
}

# This role is mixed in to any routine that is marked as being a
# native call.
my role Native[Routine $r, $libname where Str|Callable|List] {
    has int $!setup;
    has native_callsite $!call is box_target;
    has Mu $!rettype;
    has $!cpp-name-mangler;

    method !setup() {
        my $guessed_libname = guess_library_name($libname);
        $!cpp-name-mangler  = %lib{$guessed_libname} //
            (%lib{$guessed_libname} = guess-name-mangler($r, $guessed_libname));
        my Mu $arg_info := param_list_for($r.signature, $r);
        my str $conv = self.?native_call_convention || '';
        nqp::buildnativecall(self,
            nqp::unbox_s($guessed_libname),                           # library name
            nqp::unbox_s(gen_native_symbol($r, :$!cpp-name-mangler)), # symbol to call
            nqp::unbox_s($conv),        # calling convention
            $arg_info,
            return_hash_for($r.signature, $r));
        $!setup = 1;
        $!rettype := nqp::decont(map_return_type($r.returns));
    }

    method CALL-ME(|args) {
        self!setup unless $!setup;

        my Mu $args := nqp::getattr(nqp::decont(args), Capture, '$!list');
        if nqp::elems($args) != $r.signature.arity {
            X::TypeCheck::Argument.new(
                :objname($.name),
                :arguments(args.list.map(*.^name))
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

my role NativeCallMangled[$name] {
    method native_call_mangled() { $name }
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
    check_routine_sanity($r);
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

multi trait_mod:<is>(Routine $p, :$mangled!) is export(:DEFAULT, :traits) {
    $p does NativeCallMangled[$mangled === True ?? 'C++' !! $mangled];
}

role ExplicitlyManagedString {
    has $.cstr is rw;
}

multi explicitly-manage(Str $x, :$encoding = 'utf8') is export(:DEFAULT,
:utils) {
    $x does ExplicitlyManagedString;
    my $class = class CStr is repr('CStr') { method encoding() { $encoding; } };
    $x.cstr = nqp::box_s(nqp::unbox_s($x), nqp::decont($class));
}

role CPPConst {
    method cpp-const() { 1 }
}
multi trait_mod:<is>(Parameter $p, :$cpp-const!) is export(:DEFAULT, :traits) {
    $p does CPPConst;
}

role CPPRef {
    method cpp-ref() { 1 }
}
multi trait_mod:<is>(Parameter $p, :$cpp-ref!) is export(:DEFAULT, :traits) {
    $p does CPPRef;
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

}

sub EXPORT(|) {
    use NQPHLL:from<NQP>;
    my role HAS-decl-grammar {
        # This is a direct copy of scope_declarator:sym<has>, besides the uppercase spelling.
        token scope_declarator:sym<HAS> {
            :my $*LINE_NO := HLL::Compiler.lineof(self.orig(), self.from(), :cache(1));
            <sym>
            :my $*HAS_SELF := 'partial';
            :my $*ATTR_INIT_BLOCK;
            <scoped('has')>
        }
    }
    my role HAS-decl-actions {
        method scope_declarator:sym<HAS>(Mu $/) {
            # my $scoped := $<scoped>.ast;
            my Mu $scoped := nqp::atkey(nqp::findmethod($/, 'hash')($/), 'scoped').ast;
            my Mu $attr   := $scoped.ann('metaattr');
            if $attr.package.REPR ne 'CStruct'
            && $attr.package.REPR ne 'CPPStruct'
            && $attr.package.REPR ne 'CUnion' {
                die "Can only use HAS-scoped attributes in classes with repr CStruct, CPPStruct and CUnion, not " ~ $attr.package.REPR;
            }
            if nqp::objprimspec($attr.type) != 0 {
                warn "Useless use of HAS scope on an attribute with type { $attr.type.^name }.";
            }
            # Mark $attr as inlined, that's why we do all this.
            nqp::bindattr_i(nqp::decont($attr), $attr.WHAT, '$!inlined', 1);
            # make $scoped
            nqp::bindattr(nqp::decont($/), $/.WHAT, '$!made', $scoped);
        }
    }
    my Mu $MAIN-grammar := nqp::atkey(%*LANG, 'MAIN');
    my Mu $MAIN-actions := nqp::atkey(%*LANG, 'MAIN-actions');
    nqp::bindkey(%*LANG, 'MAIN',         $MAIN-grammar.HOW.mixin($MAIN-grammar, HAS-decl-grammar));
    nqp::bindkey(%*LANG, 'MAIN-actions', $MAIN-actions.HOW.mixin($MAIN-actions, HAS-decl-actions));

    {}
}

# vim:ft=perl6
