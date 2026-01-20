BEGIN {
    # Re-parent meta-objects so they appear to be under Any.
    Metamodel::ClassHOW.HOW.reparent(Metamodel::ClassHOW, Any);
    Metamodel::ConcreteRoleHOW.HOW.reparent(Metamodel::ConcreteRoleHOW, Any);
    Metamodel::CurriedRoleHOW.HOW.reparent(Metamodel::CurriedRoleHOW, Any);
    Metamodel::EnumHOW.HOW.reparent(Metamodel::EnumHOW, Any);
    Metamodel::GenericHOW.HOW.reparent(Metamodel::GenericHOW, Any);
    Metamodel::ModuleHOW.HOW.reparent(Metamodel::ModuleHOW, Any);
    Metamodel::NativeHOW.HOW.reparent(Metamodel::NativeHOW, Any);
    Metamodel::PackageHOW.HOW.reparent(Metamodel::PackageHOW, Any);
    Metamodel::ParametricRoleGroupHOW.HOW.reparent(Metamodel::ParametricRoleGroupHOW, Any);
    Metamodel::ParametricRoleHOW.HOW.reparent(Metamodel::ParametricRoleHOW, Any);
    Metamodel::SubsetHOW.HOW.reparent(Metamodel::SubsetHOW, Any);
    Metamodel::GrammarHOW.HOW.compose(Metamodel::GrammarHOW);
#?if !moar
    Metamodel::BaseDispatcher.HOW.reparent(Metamodel::BaseDispatcher, Any);
    Metamodel::MethodDispatcher.HOW.compose(Metamodel::MethodDispatcher);
    Metamodel::MultiDispatcher.HOW.compose(Metamodel::MultiDispatcher);
    Metamodel::WrapDispatcher.HOW.compose(Metamodel::WrapDispatcher);
#?endif
}

my constant CORE-SETTING-REV = do {
    # Turn CORE-SETTING-REV into kind of an allomorph except that we cannot use the actual Allomorph class since it is
    # not available at the beginning of CORE compilation and this is where we need the symbol in first place. Therefore
    # it gets its initial value as a plain integer and it is only now as we can eventually mixin the public interface
    # role into it. Besides, Allomorph is a string in first place, whereas CORE-SETTING-REV must represent the internal
    # representation which is now an integer.
    my class LanguageRevision {
        has int $!language-revision is box_target;
        has Str $!p6rev;
        method p6rev(::?CLASS:D:) {
            nqp::isconcrete($!p6rev)
                ?? $!p6rev
                !! ($!p6rev := nqp::getcomp('Raku').lvs.p6rev(nqp::unbox_i(self)))
        }
        # The default Int, Numeric, and Real coercions return the object itself, but we need a fresh copy.
        multi method Int(::?CLASS:D:)     { nqp::box_i($!language-revision, Int) }
        multi method Numeric(::?CLASS:D:) { nqp::box_i($!language-revision, Int) }
        multi method Real(::?CLASS:D:)    { nqp::box_i($!language-revision, Int) }
        multi method Str(::?CLASS:D:)     { self.p6rev }
        multi method gist(::?CLASS:D:)    { self.p6rev }
        method Version(::?CLASS:D:) {
            nqp::getcomp('Raku').lvs.as-public-repr($!language-revision, :as-version)
        }
    }
    nqp::box_i(1, LanguageRevision)
}
Metamodel::Configuration.set_language_revision_type(CORE-SETTING-REV.WHAT);

BEGIN {
    # Create pun at compile time as buf8 is used extensively in file I/O and module loading
    buf8.elems;

    # Mark all subs that are implementation details, as implementation detail.
    # In any other code, this would have been done as a trait on the actual
    # sub definition.  But doing that in the setting *before* the Routine
    # class is actually a HLL thing, makes it an unCallable.  So we do these
    # routines and methods here, at the end of setting compilation.
    trait_mod:<is>($_, :implementation-detail) for
      &CLONE-HASH-DECONTAINERIZED,
      &CLONE-LIST-DECONTAINERIZED,
      &HYPERWHATEVER,
      &dd,
      &DUMP,
      &DYNAMIC,
      &RETURN-LIST,
      &SLICE_MORE_HASH,
      &SLICE_ONE_HASH,
      &THROW,
      &THROW-NIL,

      Code.^find_method("POSITIONS"),
      Mu.^find_method("DUMP"),
      Mu.^find_method("DUMP-OBJECT-ATTRS"),
      Mu.^find_method("DUMP-PIECES"),
      Mu.^find_method("WALK")
    ;

    trait_mod:<is>($_, :revision-gated("6.c")) for
      Array.^find_method('splice'),
      Any.^find_method('splice')
    ;
}

{
    # XXX TODO: https://github.com/rakudo/rakudo/issues/2433
    # my $perl := BEGIN Perl.new;
    Rakudo::Internals.REGISTER-DYNAMIC: '$*PERL', {
        PROCESS::<$PERL> := Raku.new;
    }
    Rakudo::Internals.REGISTER-DYNAMIC: '$*RAKU', {
        PROCESS::<$RAKU> := Raku.new;
    }
}

## From Stringy.rakumod
proto sub infix:<ne>(Mu $?, Mu $?, *%) is pure {*}
multi sub infix:<ne>(    --> Bool::True) { }
multi sub infix:<ne>(Any --> Bool::True) { }
#multi sub infix:<ne>(Mu \a, Mu \b) is revision-gated("6.c") { not a eq b }
#multi sub infix:<ne>(Mu \a, Mu \b) is revision-gated("6.e") { (a eq b) eq False }
multi sub infix:<ne>(Mu \a, Mu \b) { (a eq b) eq False }
#multi sub infix:<ne>(\a, \b) is revision-gated("6.c") { a.Stringy ne b.Stringy }
#multi sub infix:<ne>(\a, \b) is revision-gated("6.e") { (a.Stringy eq b.Stringy) eq False }
multi sub infix:<ne>(\a, \b) { (a.Stringy eq b.Stringy) eq False }

## From Str.rakumod
multi sub infix:<ne>(Str:D $a, Str:D $b --> Bool:D) {
  nqp::hllbool(nqp::isne_s(nqp::unbox_s($a),nqp::unbox_s($b)))
}
multi sub infix:<ne>(str $a, str $b --> Bool:D) {
  nqp::hllbool(nqp::isne_s($a, $b))
}
## From Buf.rakumod
multi sub infix:<ne> (Blob:D $a, Blob:D $b --> Bool:D) {
  nqp::hllbool(
          nqp::not_i(nqp::eqaddr($a,$b) || $a.SAME($b))
  )
}
&infix:<ne>.set_op_props();

# Required for use in the optimizer
nqp::bindhllsym('Raku', 'Mu:U', Mu:U);

#?if moar
# Cannot be added in the Uni class, as we don't have native arrays
# then yet, so it must be done here as an augment.
augment class Uni {
    multi method new(Uni: array[uint32] \codepoints) {
        my $uni      := nqp::create(self);
        my int $elems = nqp::elems(codepoints);
        my int $i = -1;
        my int $code;

        nqp::while(
          nqp::islt_i(++$i,$elems),
          nqp::if(nqp::isgt_i($code = nqp::atpos_i(codepoints,$i), 0x10ffff)
                  || (nqp::isle_i(0xd800, $code) && nqp::isle_i($code, 0xdfff))
                  || nqp::islt_i($code, 0),
            X::InvalidCodepoint.new(:$code).throw,
            nqp::push_i($uni,$code))
        );

        $uni
    }
}
#?endif

# This cannot live in Rakudo::Internals proper because allomorphs are
# not yet known at that stage
augment class Rakudo::Internals {
    my Lock $fetch-lock := Lock.new;
    method FETCH-USER-GROUP(Str:D $what) {
        $fetch-lock.protect: {
            unless PROCESS::{$what}:exists {
                if self.IS-WIN {
                    if $what eq '$USER' {
                        PROCESS::<$USER> := try qx/whoami/.chomp;
                    }
                    # $what eq '$GROUP'
                    elsif (try qx|whoami /groups /FO csv /nh|) -> $groups {
                        PROCESS::<$GROUP> :=
                          $groups.split('","',2).head.substr(1);
                    }
                    # alas
                    else {
                        PROCESS::<$GROUP> := Nil;
                    }
                }
                elsif (try qx/LC_MESSAGES=POSIX id/) -> $id {
                    if $id ~~ m/^
                      [ uid "=" $<uid>=(\d+) ]
                      [ "(" $<user>=(<-[ ) ]>+) ")" ]
                      \s+
                      [ gid "=" $<gid>=(\d+) ]
                      [ "(" $<group>=(<-[ ) ]>+) ")" ]
                    / { 
                        PROCESS::<$USER>  := IntStr.new(+$<uid>,~$<user>);
                        PROCESS::<$GROUP> := IntStr.new(+$<gid>,~$<group>);
                    }
            
                    # alas, no support yet
                    else {
                        PROCESS::<$USER>  := Nil;
                        PROCESS::<$GROUP> := Nil;
                    }
                }
            }
            PROCESS::{$what}
        }
    }
}

# Subs that are DEPRECATED are moved here so that the "is DEPRECATED" trait
# can be applied without bootstrapping issues.

sub parse-names(Str:D \names) is DEPRECATED('uniparse') {
    names.uniparse
}

sub to-json(|c)
  is implementation-detail
  is DEPRECATED('JSON::Fast, JSON::Tiny or JSON::Pretty from https://raku.land/')
{
    Rakudo::Internals::JSON.to-json(|c);
}

sub from-json($text)
  is implementation-detail
  is DEPRECATED('JSON::Fast, JSON::Tiny or JSON::Pretty from https://raku.land/')
{
    Rakudo::Internals::JSON.from-json($text);
}

proto sub gethostname(*%) is implementation-detail {*}
multi sub gethostname(--> Str:D) is DEPRECATED('$*KERNEL.hostname') {
    $*KERNEL.hostname
}

augment class Cool {

    # Methods that are DEPRECATED are moved here and augmented into the classes
    # they belong to without bootstrapping issues.
    method parse-names(Cool:D: --> Str:D) is DEPRECATED('uniparse') {
        self.uniparse
    }
    method path(Cool:D: --> IO::Path:D) is DEPRECATED('IO') {
        self.IO
    }

    # Allow for creating an AST out of a string, for core debugging mainly
    method AST(Cool:D:
      Mu    $slang? is copy,
      Bool :$expression,  # return the first expression
      Bool :$compunit,    # return the whole compunit, not statement-list
      Mu   :$grammar is copy = nqp::gethllsym('Raku','Grammar'),
      Mu   :$actions         = nqp::gethllsym('Raku','Actions'),
    ) {

        # Make sure we don't use the EVAL's MAIN context for the
        # currently compiling compilation unit
        my $*CTXSAVE;
        my $eval_ctx := nqp::getattr(CALLER::,PseudoStash,'$!ctx');

        # Some context
        my $?FILES :='EVAL_' ~ Rakudo::Internals::EvalIdSource.next-id;
        my $*INSIDE-EVAL := 1;

        # Slang specified by string, go fetch it
        $slang = "use L10N::$slang; L10N::$slang".EVAL
          if nqp::istype($slang,Str);

        # Got a slang to mix in
        $grammar = $grammar.^mixin($slang)
          unless nqp::eqaddr(nqp::decont($slang),Mu);

        # Convert to RakuAST
        my $compiler := nqp::getcomp('Raku');
        my $ast := $compiler.compile:
          self.Str,
          :outer_ctx($eval_ctx),
          :global(GLOBAL),
          :language_version($compiler.language_version),
          |(:optimize($_) with $compiler.cli-options<optimize>),
          :target<ast>, :compunit_ok(1), :$grammar, :$actions;

        $expression
          ?? $ast.statement-list.statements.head.expression
          !! $compunit
            ?? $ast
            !! $ast.statement-list
    }
}

# Make sure all affected subclasses are aware of additions to their parents
BEGIN .^compose for
  Str, Int, Num, Rat, Complex,
  IntStr, NumStr, RatStr, ComplexStr,
  List, Array, array, Match, Range, Seq,
  int, int8, int16, int32, int64,
  uint, uint8, uint16, uint32, uint64,
  byte, num, num32, num64, str,
  IterationBuffer
;

BEGIN Metamodel::ClassHOW.exclude_parent(Mu);

{YOU_ARE_HERE}

# vim: expandtab shiftwidth=4
