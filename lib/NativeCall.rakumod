use nqp;
use QAST:from<NQP>;

#- C compiler specfic ----------------------------------------------------------
use NativeCall::Compiler::GNU;
use NativeCall::Compiler::MSVC;

my constant $cpp-name-manglers = nqp::list(
  &NativeCall::Compiler::MSVC::mangle_cpp_symbol,
  &NativeCall::Compiler::GNU::mangle_cpp_symbol,
);

#- re-export constants ---------------------------------------------------------
use NativeCall::Types;

my constant long          is export(:types, :DEFAULT) =
  NativeCall::Types::long;
my constant longlong      is export(:types, :DEFAULT) =
  NativeCall::Types::longlong;
my constant ulong         is export(:types, :DEFAULT) =
  NativeCall::Types::ulong;
my constant ulonglong     is export(:types, :DEFAULT) =
  NativeCall::Types::ulonglong;
my constant bool          is export(:types, :DEFAULT) =
  NativeCall::Types::bool;
my constant size_t        is export(:types, :DEFAULT) =
  NativeCall::Types::size_t;
my constant ssize_t       is export(:types, :DEFAULT) =
  NativeCall::Types::ssize_t;
my constant void          is export(:types, :DEFAULT) =
  NativeCall::Types::void;
my constant CArray        is export(:types, :DEFAULT) =
  NativeCall::Types::CArray;
my constant Pointer       is export(:types, :DEFAULT) =
  NativeCall::Types::Pointer;
my constant OpaquePointer is export(:types, :DEFAULT) =
  NativeCall::Types::Pointer;
my constant ExplicitlyManagedString =
  NativeCall::Types::ExplicitlyManagedString;

#- constants -------------------------------------------------------------------
# Compile time lookup structures and aliases
my constant $repr_map = nqp::hash(
  "CArray",    "carray",
  "CPPStruct", "cppstruct",
  "CPointer",  "cpointer",
  "CStruct",   "cstruct",
  "CUnion",    "cunion",
  "VMArray",   "vmarray",
);

my constant $ints_by_size =
  nqp::list_s( "", "char", "short", "", "int", "", "", "", "longlong" );
my constant $uints_by_size =
  nqp::list_s( "", "uchar", "ushort", "", "uint", "", "", "", "ulonglong" );

# Gets the NCI type code to use based on a given Raku type.
my constant $type_map = nqp::hash(
  "Bool",       nqp::atpos_s($ints_by_size,nqp::nativecallsizeof(bool)),
  "bool",       nqp::atpos_s($ints_by_size,nqp::nativecallsizeof(bool)),
  "Callable",   "callback",
  "Int",        "longlong",
  "int",        "long",
  "int16",      "short",
  "int32",      "int",
  "int64",      "longlong",
  "int8",       "char",
  "long",       "long",
  "longdouble", "longdouble",
  "longlong",   "longlong",
  "Num",        "double",
  "num",        "double",
  "num32",      "float",
  "num64",      "double",
  "size_t",     nqp::atpos_s($uints_by_size,nqp::nativecallsizeof(size_t)),
  "ssize_t",    nqp::atpos_s($ints_by_size,nqp::nativecallsizeof(ssize_t)),
  "uint",       "ulong",
  "uint16",     "ushort",
  "uint32",     "uint",
  "uint64",     "ulonglong",
  "uint8",      "uchar",
  "ulong",      "ulong",
  "ulonglong",  "ulonglong",
);

my constant $use-dispatcher :=
    $*RAKU.compiler.?supports-op('dispatch_v')
      ?? do { require NativeCall::Dispatcher; True }
      !! False;

#- lexical roles ---------------------------------------------------------------

# Role for carrying extra calling convention information.
my role NativeCallingConvention[$name] {
    method native_call_convention() { $name };
}

# Role for carrying extra string encoding information.
my role NativeCallEncoded[$name] {
    method native_call_encoded() { $name };
}

# Role for carrying a mangled library name
my role NativeCallMangled[$name] {
    method native_call_mangled() { $name }
}

# Role for setting the native symbol to be used
my role NativeCallSymbol[Str $name] {
    method native_symbol()  { $name }
}

# Roles for marking C++ properties
my role CPPConst { method cpp-const(--> 1) { } }
my role CPPRef   { method cpp-ref(  --> 1) { } }

#- NativeCall ------------------------------------------------------------------
# The namespace for much of NativeCall's functionality

module NativeCall {

# Throwaway type just to get us some way to get at the NativeCall
# representation.
    my class Callsite is repr('NativeCall') { }

# Role mixed in to any routine that is marked as being a native call
    my $mangler-for-lib := nqp::hash;
    our role Native[
      Routine $routine,
      $libname where Str | Callable | List | IO::Path | Distribution::Resource
    ] {
        has Callsite $!call is box_target;
        has Mu       $!rettype;
        has          $!cpp-name-mangler;
        has Pointer  $!entry-point;
        has int      $!arity;
        has int      $!any-optionals;
        has int      $!any-callbacks;
        has str      $!name;

        method CUSTOM-DISPATCHER(--> str) { 'raku-nativecall' }

        method call()    { $!call    }
        method rettype() { $!rettype }

        INIT my Lock $setup-lock .= new;
        method !setup() {
            $setup-lock.protect: {
                nqp::neverrepossess(self);
                return if nqp::unbox_i($!call);

                # Make sure that C++ methods are treated as mangled
                # (unless set otherwise)
                self does NativeCallMangled[True]
                  if self.package.REPR eq 'CPPStruct'
                  && !self.does(NativeCallMangled);

                # if needed, try to guess mangler
                my str $guessed-libname = guess_library_name($libname);
                $!cpp-name-mangler := nqp::ifnull(
                  nqp::atkey($mangler-for-lib,$guessed-libname),
                  nqp::bindkey(
                    $mangler-for-lib,
                    $guessed-libname,
                    guess-name-mangler($routine, $!name, $guessed-libname)
                  )
                ) if self.does(NativeCallMangled) && $routine.?native_call_mangled;

                my $signature := $routine.signature;
                my $params := nqp::getattr($signature.params, List, '$!reified');

                my Mu $arg_info := param_list_for($signature, $routine);
                my str $conv     = self.?native_call_convention || '';

                $!rettype := nqp::decont(map_return_type($routine.returns))
                  unless $!rettype;
                $!arity    = $signature.arity;

                my int $m = nqp::elems($params);
                my int $i;
                while $i < $m {
                    my $param := nqp::atpos($params, $i++);
                    $!any-optionals = 1 if $param.optional;
                    $!any-callbacks = 1 if nqp::istype($param.type,Callable);
                }

                nqp::buildnativecall(
                  self,
                  $guessed-libname,  # library name
                  nqp::unbox_s(      # symbol to call
                    gen_native_symbol($routine, $!name, :$!cpp-name-mangler)
                  ),
                  $conv,             # calling convention
                  $arg_info,
                  ($libname && nqp::istype($libname,Distribution::Resource))
                    ?? return_hash_for(
                         $signature,
                         $routine,
                         :$!entry-point,
                         :&resolve-libname,
                         :resolve-libname-arg($libname),
                       )
                    !! return_hash_for($signature, $routine, :$!entry-point)
                );
            }
        }

        method !decont-for-type($type) {
            nqp::istype($type,Str)
              ?? 'decont_s'
              !! nqp::istype($type,Int)
                ?? 'decont_i'
                !! nqp::istype($type,Num)
                  ?? 'decont_n'
                  !! 'decont';
        }

        method !arity-error(\args) {
            X::TypeCheck::Argument.new(
                :objname($.name),
                :arguments(args.list.map(*.^name)),
                :signature(try $routine.signature.gist),
            ).throw
        }

        method setup() {
            self!setup() unless nqp::unbox_i($!call);
        }

        method setup-nativecall() {
            $!name = self.name;

            unless $use-dispatcher {

                # finish compilation of the original routine so our changes won't
                # become undone right afterwards
                $*W.unstub_code_object(self, Code) if $*W;  # XXX RakuAST

                my $replacement := -> |c {
                    self!setup unless nqp::unbox_i($!call);

                    my Mu $args := nqp::getattr(nqp::decont(c), Capture, '@!list');
                    my int $nr-args = nqp::elems($args);
                    self!arity-error(c) if $nr-args != $!arity;

                    if $!any-callbacks {
                        my int $i;
                        while $i < $nr-args {
                            my $arg := nqp::decont(nqp::atpos($args, $i));
                            nqp::bindpos($args,$i,nqp::getattr($arg,Code,'$!do'))
                              if nqp::istype_nd($arg,Code);
                            ++$i;
                        }
                    }
                    nqp::nativecall($!rettype, self, $args)
                }

                my $do := nqp::getattr($replacement, Code, '$!do');
                nqp::bindattr(self, Code, '$!do', $do);
                nqp::setcodename($do, $!name);
            }
        }

        method soft(--> True) {} # prevent inlining of original function body
    }
}

#- local helper subs -----------------------------------------------------------

# Maps a chosen string encoding to a type recognized by the native call engine.
my sub string_encoding_to_nci_type(str $enc) {
    nqp::iseq_s($enc,"utf8")
      ?? "utf8str"
      !! nqp::iseq_s($enc,"ascii")
        ?? "asciistr"
        !! nqp::iseq_s($enc,"utf16")
          ?? "utf16str"
          !! die "Unknown string encoding for native call: $enc"
}

# Builds a hash of type information for the specified parameter.
my sub param_hash_for(Parameter $p) {
    my Mu $result := nqp::hash;
    my    $type   := $p.type;

    nqp::bindkey($result, 'typeobj', nqp::decont($type));
    nqp::bindkey($result, 'rw', nqp::unbox_i(1))
      if $p.rw;

    if nqp::istype($type,Str) {
        my $enc := $p.?native_call_encoded // 'utf8';
        nqp::bindkey(
          $result, 'type', nqp::unbox_s(string_encoding_to_nci_type($enc))
        );
        nqp::bindkey($result, 'free_str', nqp::unbox_i(1));
    }

    elsif nqp::istype($type,Callable) {
        nqp::bindkey($result, 'type', nqp::unbox_s(type_code_for($type)));
        my $info := param_list_for($p.sub_signature);
        nqp::unshift($info, return_hash_for($p.sub_signature, :with-typeobj));
        nqp::bindkey($result, 'callback_args', $info);
    }

    else {
        nqp::bindkey($result, 'type', nqp::unbox_s(type_code_for($type)));
    }

    $result
}

# Builds the list of parameter information for a callback argument.
my sub param_list_for(Signature $sig, &r?) {
    my $params   := nqp::getattr($sig.params,List,'$!reified');
    my int $elems = nqp::elems($params);

    # not sending Method's default slurpy *%_ (which is always last)
    --$elems
      if nqp::istype(&r,Method)
      && nqp::atpos($params,$elems - 1).name eq '%_';

    # build list
    my $result := nqp::setelems(nqp::list,$elems);
    my int $i;
    nqp::while(
      $i < $elems,
      nqp::stmts(
        nqp::bindpos($result,$i, param_hash_for(nqp::atpos($params,$i))),
        ++$i
      )
    );

    $result
}

# Builds a hash of type information for the specified return type.
my sub return_hash_for(
  Signature $s,
            &r?,
           :$with-typeobj,
           :$entry-point,
           :$resolve-libname,
           :$resolve-libname-arg
) {
    my Mu $result := nqp::hash;
    my $returns   := $s.returns;

    nqp::bindkey(
      $result, 'typeobj', nqp::decont($returns)
    ) if $with-typeobj;
    nqp::bindkey(
      $result, 'entry_point', nqp::decont($entry-point)
    ) if $entry-point;
    nqp::bindkey(
      $result, 'resolve_lib_name_arg', nqp::decont($resolve-libname-arg)
    ) if $resolve-libname-arg;
    nqp::bindkey(
      $result, 'resolve_lib_name', nqp::getattr(nqp::decont($resolve-libname), Code, '$!do')
    ) if $resolve-libname;

    if nqp::istype($returns,Str) {
        my $enc := &r.?native_call_encoded // 'utf8';
        nqp::bindkey(
          $result, 'type', nqp::unbox_s(string_encoding_to_nci_type($enc))
        );
        nqp::bindkey($result, 'free_str', nqp::unbox_i(0));
    }

    # TODO: If we ever want to handle function pointers returned from C, this
    # bit of code needs to handle that.
    else {
        nqp::bindkey(
          $result, 'type', nqp::eqaddr($returns,Mu)
            ?? 'void'
            !! nqp::unbox_s(type_code_for($returns))
        );
    }
    $result
}

my sub type_code_for(Mu ::T) {
    nqp::ifnull(
      nqp::atkey($type_map,T.^shortname),
      nqp::ifnull(
        nqp::atkey($repr_map,T.REPR),

    # the REPR of a Buf or Blob type object is Uninstantiable, so
    # needs an extra special case here that isn't covered in the
    # hash lookup above.
        nqp::istype(T,Blob)
          ?? "vmarray"
          !! nqp::istype(T,Pointer)
            ?? "cpointer"
            !! die qq:to/ERROR/.naive-word-wrapper))
Unknown type {T.^name} used in native call.  If you want to pass a struct, be sure to use the CStruct or CPPStruct representation.  If you want to pass an array, be sure to use the CArray type.
ERROR
}

my sub gen_native_symbol(Routine $r, $name, :$cpp-name-mangler) {
    my $symbol := $r.?native_symbol;

    # Native symbol or name is said to be already mangled
    !$r.?native_call_mangled
      ?? ($symbol // $name)
      # Mangle C++ classes
      !! $r.package.REPR eq 'CPPStruct'
        ?? $cpp-name-mangler(
             $r, $symbol // ($r.package.^name ~ '::' ~ $name)
           )
        # Mangle C
        !!  $cpp-name-mangler($r, $symbol // $name)
}

my sub guess-name-mangler(Routine $r, $name, Str $libname) {

    my sub mangler-for($sym) {
        my int $m = nqp::elems($cpp-name-manglers);
        my int $i;
        while $i < $m {
            my &mangler := nqp::atpos($cpp-name-manglers, $i);
            (try cglobal($libname, mangler($r, $sym), Pointer))
              ?? (return &mangler)
              !! ++$i;
        }
        die "Don't know how to mangle symbol '$sym' for library '$libname'";
    }

    if $r.package.REPR eq 'CPPStruct' {
        mangler-for $r.?native_symbol // ($r.package.^name ~ '::' ~ $name)
    }
    elsif $r.?native_call_mangled {
        mangler-for $r.?native_symbol // $name
    }
}

my sub resolve-libname($libname) {
    CATCH { default { note $_ } }
    $libname.platform-library-name.Str
}

my multi sub map_return_type(Mu $type) { $type }
my multi sub map_return_type(Int     ) { Int   }
my multi sub map_return_type(Num     ) { Num   }

#- exportable subs -------------------------------------------------------------

my sub nativesizeof($obj) is export(:DEFAULT) {
    nqp::nativecallsizeof($obj)
}

my proto guess_library_name(|) is export(:TEST) {*}
my multi guess_library_name(IO::Path $lib) {
    guess_library_name($lib.absolute)
}
my multi guess_library_name(Distribution::Resource $lib) {
    $lib.platform-library-name.Str
}
my multi guess_library_name(Callable $lib) {
    $lib()
}
my multi guess_library_name(List $lib) {
    guess_library_name($lib[0], $lib[1])
}
my multi guess_library_name(Str $libname, $apiversion='') {
    $libname.DEFINITE
      ?? $libname ~~ /[\.<.alpha>+ | \.so [\.<.digit>+]+ ] $/
          ?? $libname                          # Already a full name
          !! $*VM.platform-library-name(
               $libname.IO, :version($apiversion || Version)
             ).Str
      !! ''
}

my sub explicitly-manage(
  Str $x, :$encoding = 'utf8'
) is export(:DEFAULT, :utils) {
    my class CStr is repr<CStr> {
        method encoding() { $encoding }
    }

    $x does ExplicitlyManagedString;
    $x.cstr = nqp::box_s(nqp::unbox_s($x), nqp::decont(CStr))
}

my multi refresh($obj --> 1) is export(:DEFAULT, :utils) {
    nqp::nativecallrefresh($obj);
}

my proto sub nativecast(|) is export(:DEFAULT) {*}
my multi sub nativecast(Signature $target-type, $source) {
    my $r := sub { };
    $r does NativeCall::Native[$r, Str];
    $r.setup-nativecall;
    nqp::bindattr($r, Code, '$!signature', nqp::decont($target-type));
    nqp::bindattr($r, $r.WHAT, '$!entry-point', $source);
    $r
}

my multi sub nativecast(Int $target-type, $source) {
    nqp::nativecallcast(nqp::decont($target-type),
        Int, nqp::decont($source));
}

my multi sub nativecast(Num $target-type, $source) {
    nqp::nativecallcast(nqp::decont($target-type),
        Num, nqp::decont($source));
}

my multi sub nativecast($target-type, $source) {
    nqp::nativecallcast(nqp::decont($target-type),
        nqp::decont($target-type), nqp::decont($source));
}

my sub cglobal($libname, $symbol, $target-type) is export is rw {
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

my sub check_routine_sanity(Routine $routine) is export(:TEST) {

    # Maybe this should use the hash already existing?
    my sub validnctype(Mu ::T) {
        my str $REPR = T.REPR;

        return 1
          if nqp::existskey($repr_map,$REPR)
          && $REPR ne 'CArray'
          && $REPR ne 'CPointer';

        my str $name = T.^name;
        $name eq 'Str' || $name eq 'str' || $name eq 'Bool'
          ?? 1
          !! $REPR eq 'P6opaque'
            #to disting int and int32 for example
            || (T.HOW.^can("nativesize")
                 && nqp::not_i(nqp::defined(T.^nativesize)))
            ?? 0
            !! ($REPR eq 'CArray' || $REPR eq 'CPointer') && T.^can('of')
              ?? validnctype(T.of)
              !! 1
    }

    my $sig = $routine.signature;
    for @($sig.params).kv -> $i, $param {
        next if nqp::istype($routine,Method) and ($i < 1 or $i == $sig.params.elems - 1); #Method have two extra parameters
        if nqp::istype($param.type,Callable) {
          # We probably want to check the given routine type too here. but I don't know how
          next;
        }
        next unless nqp::istype($param.type,Blob) #Buf are Uninstantiable, make this buggy
        || $param.type.^can('gist'); #FIXME, it's to handle case of class A { sub foo(A) is native) }, the type is not complete
        if !validnctype($param.type) {
           warn "In '{$routine.name}' routine declaration - Not an accepted NativeCall type"
            ~ " for parameter [{$i + 1}] {$param.name ?? $param.name !! ''} : {$param.type.^name}\n"
            ~ " --> For Numerical type, use the appropriate int32/int64/num64...";
        }
    }

    my $returns := $routine.returns;
    my str $REPR = $returns.REPR;

    # Meh fix but 'incomplete' type are a pain
    if $REPR eq 'CPointer' || $REPR eq 'CStruct' || $REPR eq 'CPPStruct' {
        True
    }
    elsif $returns.^name ne 'Mu' && nqp::not_i(validnctype($returns)) {
        warn qq:to/WARNING/.chomp.naive-word-wrapper;
The returning type of '{$routine.name}' --> {$returns.^name} is erroneous.
You should not return a non NativeCall supported type (like Int inplace
of int32), truncating errors can appear with different architectures
WARNING
    }
}

#- exportable postcircumfixes --------------------------------------------------

# CArray as a positional
multi sub postcircumfix:<[ ]>(
  CArray:D \array, $pos
) is raw is export(:DEFAULT, :types) is default {
    nqp::istype($pos, Iterable)
      ?? $pos.map: { array.AT-POS($_) }
      !! array.AT-POS($pos);
}
multi sub postcircumfix:<[ ]>(
  CArray:D \array, *@pos
) is raw is export(:DEFAULT, :types) {
    @pos.map: { array.AT-POS($_) }
}
multi sub postcircumfix:<[ ]>(
  CArray:D \array, Callable:D $block
) is raw is export(:DEFAULT, :types) {
    my $*INDEX := 'Effective index';
    array[$block.POSITIONS(array)]
}
multi sub postcircumfix:<[ ]>(
  CArray:D \array
) is export(:DEFAULT, :types) {
    nqp::decont(array)
}
multi sub postcircumfix:<[ ]>(
  CArray:D \array, Whatever:D
) is export(:DEFAULT, :types) {
    array[^array.elems]
}
multi sub postcircumfix:<[ ]>(
  CArray:D \array, HyperWhatever:D
) is export(:DEFAULT, :types) {
    NYI('HyperWhatever in CArray index').throw;
}

#- exportable trait_mods -------------------------------------------------------
multi trait_mod:<is>(Routine $r, :$symbol!) is export(:DEFAULT, :traits) {
    $r does NativeCallSymbol[$symbol];
}

# Specifies the calling convention to use for a native call.
multi trait_mod:<is>(Routine $r, :$nativeconv!) is export(:DEFAULT, :traits) {
    $r does NativeCallingConvention[$nativeconv];
}

# Ways to specify how to marshall strings.
multi trait_mod:<is>(Parameter $p, :$encoded!) is export(:DEFAULT, :traits) {
    $p does NativeCallEncoded[$encoded];
}
multi trait_mod:<is>(Routine $r, :$encoded!) is export(:DEFAULT, :traits) {
    $r does NativeCallEncoded[$encoded];
}

multi trait_mod:<is>(Routine $r, :$mangled!) is export(:DEFAULT, :traits) {
    $r does NativeCallMangled[$mangled === True ?? 'C++' !! $mangled];
}

multi trait_mod:<is>(
  Routine $r, :cpp-const($)!
) is export(:DEFAULT, :traits) {
    $r does CPPConst;
}
multi trait_mod:<is>(Parameter $p, :cpp-const($)!) is export(:DEFAULT, :traits) {
    $p does CPPConst;
}

multi trait_mod:<is>(Parameter $p, :cpp-ref($)!) is export(:DEFAULT, :traits) {
    $p does CPPRef;
}

#- handling exports ------------------------------------------------------------

sub EXPORT(|) {
    # Specifies that the routine is actually a native call, into the
    # current executable (platform specific) or into a named library
    my $native_trait := multi trait_mod:<is>(Routine $r, :$native!) {
        check_routine_sanity($r);
        $r does NativeCall::Native[$r, $native === True ?? Str !! $native];
        $r.setup-nativecall;
    };
    Map.new(
        '&trait_mod:<is>' => $native_trait.dispatcher,
    );
}

# vim: expandtab shiftwidth=4
