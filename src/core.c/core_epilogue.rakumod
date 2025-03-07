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

#-------------------------------------------------------------------------------

augment class Code {

    # Shortcut to literalizing a value
    my sub literalize(Mu $value) {
        RakuAST::Literal.from-value($value)
    }

    # Shortcut to creating an "is" trait
    my sub trait-is(str $name) {
        RakuAST::Trait::Is.new(
          name => RakuAST::Name.from-identifier($name)
        )
    }

    # Create a simple type, possibly consisting of more than one part
    my sub make-simple-type(str $name) {
        my str @parts = $name.split('::');
        RakuAST::Type::Simple.new(
          @parts.elems == 1
            ?? RakuAST::Name.from-identifier(@parts.head)
            !! RakuAST::Name.from-identifier-parts(|@parts)
        )
    }

    # Create a RakuAST version of a given type, with any
    # parameterizations and coercions
    my sub TypeAST(Mu $type) {

        # Looks like a coercion type
        if $type.HOW.^name.contains('::Metamodel::CoercionHOW') {
            RakuAST::Type::Coercion.new(
              base-type  => TypeAST($type.^target_type),
              constraint => TypeAST($type.^constraint_type)
            )
        }

        # Looks like a parameterized type
        elsif nqp::can($type.HOW,"roles") && $type.^roles -> @roles {
            my $role := @roles.head;
            if $role.HOW.^name.contains('::Metamodel::ParametricRoleGroupHOW') {
                make-simple-type($type.^name)
            }
            else {
                my @args is List = @roles.head.^role_arguments.map(&TypeAST);
                RakuAST::Type::Parameterized.new(
                  base-type => TypeAST($type.^mro[1]),
                  args      => RakuAST::ArgList.new(|@args)
                )
            }
        }

        # A simple type will suffice if there is no coercion or
        # parameterization
        else {
            make-simple-type($type.^name)
        }
    }

    # Create a RakuAST version of a Signature object
    my sub SignatureAST(Signature:D $signature) {
        my %args;
        %args<parameters> := $signature.params.map(&ParameterAST).List;

        my $returns := $signature.returns;
        if nqp::isconcrete($returns) {
            %args<returns> = literalize($returns);
        }
        elsif nqp::not_i(nqp::eqaddr($returns,Mu)) {
            %args<returns> = make-simple-type($returns);
        }

        RakuAST::Signature.new(|%args)
    }

    # Create a RakuAST version of a Parameter object
    my sub ParameterAST(Parameter:D $parameter, *%_) {
        my $slurpytypes := BEGIN nqp::hash(
          '*',   RakuAST::Parameter::Slurpy::Flattened,
          '**',  RakuAST::Parameter::Slurpy::Unflattened,
          '+',   RakuAST::Parameter::Slurpy::SingleArgument
        );

        my str $sigil = $parameter.sigil;
        my %args;

        my sub add-is-trait-if-set(str $name) {
            %args<traits>.push(trait-is($name))
              if !$parameter.capture && $parameter."$name"();
        }

        # Strip off any Positional[] and Associative[] for arrays
        # and hashes, because otherwise we would get them stacked
        # on top of each other.  But don't set the type if the
        # role was the only constraint, as that will be handled
        # later by the sigil
        my sub set-role-type(Mu \type, Mu \role) {
            unless nqp::eqaddr(type,role) {
                %args<type> = TypeAST(
                  type.HOW.^name.contains('::Metamodel::CurriedRoleHOW')
                    && nqp::eqaddr(type.^curried_role,role)
                    ?? type.^role_arguments.head
                    !! type
                );
            }
        }

        if $parameter.type_captures -> @captures {
            %args<type-captures> = RakuAST::Type::Capture.new(
                make-simple-type(@captures.head.^name)
            );
        }
        else {
            my $type := %_<type>:exists ?? %_<type> !! $parameter.type;

            $sigil eq '@'
              ?? set-role-type($type, Positional)
              !! $sigil eq '%'
                ?? set-role-type($type, Associative)
                !! (%args<type> = TypeAST($type));
        }

        # Some kind of capture as target
        if $sigil eq '|' {
            %args<target> = RakuAST::ParameterTarget::Term.new(
              RakuAST::Name.from-identifier(%_<name> // $parameter.name)
            );
            %args<slurpy> = RakuAST::Parameter::Slurpy::Capture;
        }

        # An ordinary target
        else {
            %args<target> = RakuAST::ParameterTarget::Var.new(
              name => %_<name> // ($parameter.name || $sigil)
            );
        }

        %args<slurpy> = nqp::atkey($slurpytypes,$parameter.prefix)
          if $parameter.slurpy;

        if $parameter.named_names -> @names {
            %args<names>    = @names;
            %args<optional> = False unless $parameter.optional;
        }
        else {
            %args<optional> = True if $parameter.optional;
        }

        add-is-trait-if-set($_) for <raw rw copy>;

        # If a default value was explicitely specified, we're handling
        # a name argument, which by definition has to become optional
        # if it has a default
        if %_<default>:exists {
            %args<default> = literalize(%_<default>);
            %args<optional>:delete;
        }

        # Check if the parameter has a default value, and make sure the
        # RakuAST version of it has that as well
        else {
            my $default := nqp::getattr($parameter,Parameter,'$!default_value');
            %args<default> = literalize($default)
              unless nqp::isnull($default);
        }

        if $parameter.constraint_list -> @constraints {
            my $head := @constraints.head;
            if @constraints.elems == 1
              && nqp::not_i(nqp::istype($head,Code)) {
                %args<value> = $head;  # literalize($head)) ??
                %args<target>:delete;
            }
            else {
                %args<where> = literalize($head);
            }
        }

        %args<sub-signature> = SignatureAST($_)
          with $parameter.sub_signature;

        RakuAST::Parameter.new(|%args)
    }

#-------------------------------------------------------------------------------
    method assuming(Code:D: **@positionals is raw, *%nameds) {
        my int $nr-positionals = @positionals.elems;

        # Parameters of new signature
        my @new;

        # Arguments to be passed to original code
        my $args := RakuAST::ArgList.new;

        # Current index into positionals
        my int $index;

        # Curry the positional at the given index
        my sub curry-positional($i --> Nil) {
            $args.push(
              nqp::iscont(my $value := @positionals[$i])
                || nqp::istype($value,Positional)
                || nqp::istype($value,Associative)
                ?? RakuAST::ApplyPostfix.new(
                     operand => RakuAST::Var::Lexical.new("\@positionals"),
                     postfix => RakuAST::Postcircumfix::ArrayIndex.new(
                       index => RakuAST::SemiList.new(
                         RakuAST::Statement::Expression.new(
                           expression => RakuAST::IntLiteral.new($i)
                         )
                       )
                     )
                   )
                !! RakuAST::Literal.from-value($value)
            );
        }

        # Add all positional values as arguments
        my sub curry-all-positionals(--> Nil) {
            curry-positional($_) for $index ..^ $nr-positionals;
            $index = $nr-positionals;
        }

        # Add a flattened capture to the args with the given name
        my sub add-capture-to-args(str $name --> Nil) {
            $args.push(
              RakuAST::ApplyPrefix.new(
                prefix  => RakuAST::Prefix.new("|"),
                operand => RakuAST::Term::Name.new(
                  RakuAST::Name.from-identifier($name)
                )
              )
            );
        }

        # Add all named values as arguments
        my sub curry-all-nameds(--> Nil) {
            $args.push(
              RakuAST::ColonPair::Value.new(
                key   => .key,
                value => literalize(.value)
              )
            ) for %nameds;
            %nameds = ();
        }

        # Add capture by this name at the end, as both parameter and
        # argument.  Needed in case of sub-signatures
        my str $capture-name;

        # Type captures by name and what they should be if assumed
        my $type-captures-seen := nqp::hash;
        my $seen-capture-param;

        # Set up an array with the parameters of the assumee, so that
        # we can add parameters later in case of sub-signatures
        my @old = self.signature.params;

        my int $i;
        while @old && @old.shift -> $parameter {
            ++$i;

            # Nameless parameters that need to be passed on, need to
            # have a name to allow them to be passed on.  So generate
            # one depending on the index of the parameter, and give it
            # a name that cannot be generated from source code to
            # prevent collisions
            my str $name  = $parameter.name || "$parameter.sigil()param.$i";

            # Add current parameter to new signature
            my sub add-parameter-to-signature(*%_ --> Nil) {
                @new.push(ParameterAST(
                  $parameter,
                  :type(nqp::ifnull(
                    nqp::atkey($type-captures-seen,$parameter.type.^name),
                    $parameter.type
                  )),
                  :$name,
                  |%_
                ));
            }

            # Add current parameter to args (when passing through somehow)
            my sub add-parameter-to-args(--> Nil) {
                my $head := $parameter.constraint_list.head;
                $args.push(
                  nqp::isconcrete($head)
                    && nqp::not_i(nqp::istype($head,Code))
                    ?? RakuAST::Literal.from-value($head)
                    !! RakuAST::Var::Lexical.new($name)
                );
            }

            # Add all possible forms of type capture
            my sub add-type-captures(Mu $target, @types) {
                for @types {
                    nqp::bindkey(
                      $type-captures-seen,$_,$target
                    );
                    nqp::bindkey(
                      $type-captures-seen,"Positional[$_]",$target
                    );
                    nqp::bindkey(
                      $type-captures-seen,"Associative[$_]",$target
                    );
                }
            }

            # Throw if the given value would not bind to the current parameter
            my sub typecheck-value(Mu $value --> Nil) {
                my $type    := $parameter.type;
                my $HOWname := $type.HOW.^name;

                # alas, typecheck failed
                sub failed-check() {
                    X::TypeCheck::Binding::Parameter.new(
                      :$parameter,
                      :what($value.^name),
                      :symbol($parameter.name),
                      :got($value),
                      :expected($type)
                    ).throw;
                }

                if $HOWname.contains('::Metamodel::NativeHOW') {
                    my $typemap := BEGIN nqp::hash(
                      'int8',  Int, 'int16',  Int, 'int32',  Int, 'int64',  Int,
                      'uint8', Int, 'uint16', Int, 'uint32', Int, 'uint64', Int,
                      'num32', Num, 'num64',  Num,
                      'int',   Int, 'num',    Num, 'str',    Str, 'uint',   Int
                    );
                    failed-check unless nqp::istype(
                      $value,
                      nqp::atkey($typemap,$type.^name)
                    );
                }
                elsif $HOWname.contains('::GenericHOW' | '::CurriedRoleHOW') {
                    # cannot check at this time
                }
                elsif nqp::not_i(nqp::istype($value,$type)) {
                    failed-check;
                }
            }

            # Advertises itself as a named argument
            if $parameter.named  {

                # A slurpy hash, usually *%_ implicitely defined in methods
                if $parameter.slurpy {
                    curry-all-nameds;
                    add-parameter-to-signature;
                }

                # A single named argument
                else {
                    my @names  := $parameter.named_names;
                    my str $key = @names.head;

                    # Appears to be assumed, set up optional named with
                    # changed default (the value specified), because it
                    # will *still* be possible to override an assumed
                    # named argument by specifying it in the call to the
                    # produced code
                    with @names.first({ %nameds{$_}:exists }, :k) -> $i {
                        my str $name = @names[$i];
                        my $default := %nameds{$name};
                        typecheck-value($default);
                        add-parameter-to-signature(:$default);
                        %nameds{$name}:delete;
                    }

                    # Not to be assumed, so just pass on
                    else {
                        add-parameter-to-signature;
                    }

                    # Set up as :$foo in the call
                    $args.push(
                      RakuAST::ColonPair::Variable.new(
                        key   => $key,
                        value => RakuAST::Var::Lexical.new($name)
                      )
                    );
                }
            }

            # A capture parameter will eat all positional and named
            # values to be assumed
            elsif $parameter.capture {

                # Only add the first capture seen: multiple captures
                # with different subsignatures are apparently a thing
                # but we only need to look at the first one in that
                # case
                unless $seen-capture-param {

                    # If there is a sub-signature, its positional
                    # parameters effectively becomes the signature
                    # to be working with from now on, and the
                    # current parameter (a capture) should *not*
                    # be added to the new signature at this point
                    if $parameter.sub_signature -> $sub-signature {
                        @old = $sub-signature.params.grep(!*.named);
                        $capture-name = $name
                          if $sub-signature.params.first(*.named);
                    }

                    # No sub-signature, so do the normal processing
                    else {

                        # Artificial names are created with their sigil.
                        # However it appears this shouldn't be done for
                        # captures.  This probably exposes some internal
                        # RakuAST inconsistency to be solved at a later
                        # time
                        $name = $name.substr(1) if $name.starts-with('|param');

                        curry-all-positionals;
                        curry-all-nameds;
                        add-capture-to-args($name);
                        add-parameter-to-signature;
                    }
                    $seen-capture-param = True;
                }
            }

            # A slurpy (positional) will eat all positional values
            elsif $parameter.slurpy {
                curry-all-positionals;
                $args.push(RakuAST::ApplyPrefix.new(
                  prefix  => RakuAST::Prefix.new("|"),
                  operand => RakuAST::Var::Lexical.new($name)
                ));
                add-parameter-to-signature;
            }

            # No values left, so we need to pass this parameter through
            elsif $index == $nr-positionals {
                add-parameter-to-args;
                add-parameter-to-signature;
            }

            # Need to not assume anything for this parameter, so let
            # it pass through
            elsif nqp::istype(@positionals[$index],Whatever) {
                ++$index;  # don't look at this one again
                add-parameter-to-args;
                add-parameter-to-signature;
            }

            # About to assume a parameter, but this parameter has a
            # capture type (::T).  Make sure we remember the type by
            # name, and what it should become for any parameter that
            # has the same type as this capture type
            elsif $parameter.type_captures -> @types {
                add-type-captures(@positionals[$index].WHAT, @types);
                curry-positional($index++);
            }

            # Need to assume this parameter, but check value first
            else {
                typecheck-value(@positionals[$index]);
                curry-positional($index++);
            }
        }

        # Found a capture with a sub-signature, so we need to add a clean
        # capture to the signature and pass it on in the arguments
        if $capture-name {
            @new.push(RakuAST::Parameter.new(
              target => RakuAST::ParameterTarget::Term.new(
                RakuAST::Name.from-identifier($capture-name)
              ),
              slurpy => RakuAST::Parameter::Slurpy::Capture
            ));
            curry-all-nameds;
            add-capture-to-args($capture-name);
        }

        # More values specified than can be accepted
        if $index < $nr-positionals {
            die "Too many positionals";
        }

        # Any left named values
        elsif %nameds.keys -> @nogo {
            die "Unexpected @nogo[]";
        }

        # Create the Callable wrapper and return it
        my %args =
          signature => RakuAST::Signature.new(
            parameters => @new.List,
          ),
          body      => RakuAST::Blockoid.new(
            RakuAST::StatementList.new(
              RakuAST::Statement::Expression.new(
                expression => RakuAST::ApplyPostfix.new(
                  operand => RakuAST::Term::Self.new,
                  postfix => RakuAST::Call::Term.new(:$args),
                )
              )
            )
          )
        ;
        if nqp::istype(self,Routine) {
            %args<multiness> = "proto" if self.is_dispatcher;
            %args<traits>.push(trait-is("rw")) if self.rw;
            %args<name> =
              RakuAST::Name.from-identifier("assumed." ~ self.name);
            RakuAST::Sub.new(|%args).EVAL
        }
        else {
            RakuAST::PointyBlock.new(|%args).EVAL
        }
    }
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
