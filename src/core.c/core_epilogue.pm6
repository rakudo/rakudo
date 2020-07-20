# Re-parent meta-objects so they appear to be under Any.
BEGIN {
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
    Perl6::Metamodel::BaseDispatcher.HOW.reparent(Perl6::Metamodel::BaseDispatcher, Any);
    Perl6::Metamodel::MethodDispatcher.HOW.compose(Perl6::Metamodel::MethodDispatcher);
    Perl6::Metamodel::MultiDispatcher.HOW.compose(Perl6::Metamodel::MultiDispatcher);
    Perl6::Metamodel::WrapDispatcher.HOW.compose(Perl6::Metamodel::WrapDispatcher);
}

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
      &SLICE_MORE_LIST,
      &SLICE_ONE_HASH,
      &SLICE_ONE_LIST,
      &THROW,
      &THROW-NIL,

      Code.^find_method("POSITIONS"),
      Mu.^find_method("DUMP"),
      Mu.^find_method("DUMP-OBJECT-ATTRS"),
      Mu.^find_method("DUMP-PIECES"),
      Mu.^find_method("WALK")
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

BEGIN Metamodel::ClassHOW.exclude_parent(Mu);

{YOU_ARE_HERE}

# vim: expandtab shiftwidth=4
