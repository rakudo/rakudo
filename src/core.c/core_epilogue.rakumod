BEGIN {
    # Re-parent meta-objects so they appear to be under Any.
    Perl6::Metamodel::ClassHOW.HOW.reparent(Perl6::Metamodel::ClassHOW, Any);
    Perl6::Metamodel::ConcreteRoleHOW.HOW.reparent(Perl6::Metamodel::ConcreteRoleHOW, Any);
    Perl6::Metamodel::CurriedRoleHOW.HOW.reparent(Perl6::Metamodel::CurriedRoleHOW, Any);
    Perl6::Metamodel::EnumHOW.HOW.reparent(Perl6::Metamodel::EnumHOW, Any);
    Perl6::Metamodel::GenericHOW.HOW.reparent(Perl6::Metamodel::GenericHOW, Any);
    Perl6::Metamodel::ModuleHOW.HOW.reparent(Perl6::Metamodel::ModuleHOW, Any);
    Perl6::Metamodel::NativeHOW.HOW.reparent(Perl6::Metamodel::NativeHOW, Any);
    Perl6::Metamodel::PackageHOW.HOW.reparent(Perl6::Metamodel::PackageHOW, Any);
    Perl6::Metamodel::ParametricRoleGroupHOW.HOW.reparent(Perl6::Metamodel::ParametricRoleGroupHOW, Any);
    Perl6::Metamodel::ParametricRoleHOW.HOW.reparent(Perl6::Metamodel::ParametricRoleHOW, Any);
    Perl6::Metamodel::SubsetHOW.HOW.reparent(Perl6::Metamodel::SubsetHOW, Any);
    Perl6::Metamodel::GrammarHOW.HOW.compose(Perl6::Metamodel::GrammarHOW);
#?if !moar
    Perl6::Metamodel::BaseDispatcher.HOW.reparent(Perl6::Metamodel::BaseDispatcher, Any);
    Perl6::Metamodel::MethodDispatcher.HOW.compose(Perl6::Metamodel::MethodDispatcher);
    Perl6::Metamodel::MultiDispatcher.HOW.compose(Perl6::Metamodel::MultiDispatcher);
    Perl6::Metamodel::WrapDispatcher.HOW.compose(Perl6::Metamodel::WrapDispatcher);
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
