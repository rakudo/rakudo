my class Parameter { # declared in BOOTSTRAP
    # class Parameter is Any
    #     has str $!variable_name
    #     has @!named_names
    #     has @!type_captures
    #     has int $!flags
    #     has @!post_constraints
    #     has Signature $!sub_signature
    #     has Code $!default_value
    #     has Mu $!container_descriptor;
    #     has Mu $!attr_package;
    #     has Mu $!why;

    my constant $SIG_ELEM_BIND_CAPTURE       = 1 +<  0;
    my constant $SIG_ELEM_BIND_PRIVATE_ATTR  = 1 +<  1;
    my constant $SIG_ELEM_BIND_PUBLIC_ATTR   = 1 +<  2;
    my constant $SIG_ELEM_SLURPY_POS         = 1 +<  3;
    my constant $SIG_ELEM_SLURPY_NAMED       = 1 +<  4;
    my constant $SIG_ELEM_SLURPY_LOL         = 1 +<  5;
    my constant $SIG_ELEM_INVOCANT           = 1 +<  6;
    my constant $SIG_ELEM_MULTI_INVOCANT     = 1 +<  7;
    my constant $SIG_ELEM_IS_RW              = 1 +<  8;
    my constant $SIG_ELEM_IS_COPY            = 1 +<  9;
    my constant $SIG_ELEM_IS_RAW             = 1 +< 10;
    my constant $SIG_ELEM_IS_OPTIONAL        = 1 +< 11;
    my constant $SIG_ELEM_ARRAY_SIGIL        = 1 +< 12;
    my constant $SIG_ELEM_HASH_SIGIL         = 1 +< 13;
    my constant $SIG_ELEM_DEFAULT_FROM_OUTER = 1 +< 14;
    my constant $SIG_ELEM_IS_CAPTURE         = 1 +< 15;
    my constant $SIG_ELEM_UNDEFINED_ONLY     = 1 +< 16;
    my constant $SIG_ELEM_DEFINED_ONLY       = 1 +< 17;
    my constant $SIG_ELEM_DEFAULT_IS_LITERAL = 1 +< 20;
    my constant $SIG_ELEM_SLURPY_ONEARG      = 1 +< 24;
    my constant $SIG_ELEM_CODE_SIGIL         = 1 +< 25;
    my constant $SIG_ELEM_IS_COERCIVE        = 1 +< 26;

    my constant $SIG_ELEM_IS_NOT_POSITIONAL = $SIG_ELEM_SLURPY_POS
                                           +| $SIG_ELEM_SLURPY_NAMED
                                           +| $SIG_ELEM_SLURPY_LOL
                                           +| $SIG_ELEM_SLURPY_ONEARG
                                           +| $SIG_ELEM_IS_CAPTURE;
    my constant $SIG_ELEM_IS_SLURPY = $SIG_ELEM_SLURPY_POS
                                   +| $SIG_ELEM_SLURPY_NAMED
                                   +| $SIG_ELEM_SLURPY_LOL
                                   +| $SIG_ELEM_SLURPY_ONEARG;
    my constant $SIG_ELEM_IS_NOT_READONLY = $SIG_ELEM_IS_RW
                                         +| $SIG_ELEM_IS_COPY
                                         +| $SIG_ELEM_IS_RAW;

#?if !js
    my constant $sigils2bit = nqp::hash(
#?endif
#?if js
    my $sigils2bit := nqp::hash(
#?endif
      Q/@/, $SIG_ELEM_ARRAY_SIGIL,
      Q/%/, $SIG_ELEM_HASH_SIGIL,
      Q/&/, $SIG_ELEM_CODE_SIGIL,
      Q/\/, $SIG_ELEM_IS_RAW,
      Q/|/, $SIG_ELEM_IS_CAPTURE +| $SIG_ELEM_IS_RAW,
    );
    sub set-sigil-bits(str $sigil, \flags --> Nil) {
        if nqp::atkey($sigils2bit,$sigil) -> $bit {
            flags +|= $bit
        }
    }

    sub definitize-type(Str:D $type, Bool:D $definite --> Mu) {
        Metamodel::DefiniteHOW.new_type(:base_type(::($type)), :$definite)
    }

    sub str-to-type(Str:D $type, Int:D $flags is rw --> Mu) {
        if $type.ends-with(Q/:D/) {
            $flags +|= $SIG_ELEM_DEFINED_ONLY;
            definitize-type($type.chop(2), True)
        }
        elsif $type.ends-with(Q/:U/) {
            $flags +|= $SIG_ELEM_UNDEFINED_ONLY;
            definitize-type($type.chop(2), False)
        }
        elsif $type.ends-with(Q/:_/) {
            ::($type.chop(2))
        }
        else {
            ::($type)
        }
    }

    submethod BUILD(
        Parameter:D:
        Str:D  :$name           is copy = "",
        Int:D  :$flags          is copy = 0,
        Bool:D :$named          is copy = False,
        Bool:D :$optional       is copy = False,
        Bool:D :$mandatory      is copy = False,
        Bool:D :$is-copy        = False,
        Bool:D :$is-raw         = False,
        Bool:D :$is-rw          = False,
        Bool:D :$multi-invocant = True,
               *%args  # type / default / where / sub_signature captured through %_
        --> Nil
      ) {

        if $name {                                 # specified a name?

            if $name.ends-with(Q/!/) {
                $name      = $name.substr(0,*-1);
                $mandatory = True;
            }
            elsif $name.ends-with(Q/?/) {
                $name     = $name.substr(0,*-1);
                $optional = True;
            }

            my $sigil = $name.substr(0,1);

            if $sigil eq Q/:/ {
                $name  = $name.substr(1);
                $sigil = $name.substr(0,1);
                $named = True;
            }
            elsif $sigil eq Q/+/ {
                $name  = $name.substr(1);
                $sigil = $name.substr(0,1);
                $flags +|= $SIG_ELEM_IS_RAW +| $SIG_ELEM_SLURPY_ONEARG;
            }

            if $name.ends-with(Q/)/) {
                if $named {
                    my $start = $name.index(Q/(/); # XXX handle multiple
                    @!named_names := nqp::list_s($name.substr(0,$start));
                    $name := $name.substr($start + 1, *-1);
                }
                else {
                    die "Can only specify alternative names on named parameters: $name";
                }
            }

            if $sigil eq Q/*/ {                     # is it a slurpy?
                $name  = $name.substr(1);
                $sigil = $name.substr(0,1);

                if %args.EXISTS-KEY('type') {
                    die "Slurpy named parameters with type constraints are not supported|"
                }

                if $sigil eq Q/*/ {                  # is it a double slurpy?
                    $name  = $name.substr(1);
                    $sigil = $name.substr(0,1);
                    $flags +|= $SIG_ELEM_SLURPY_LOL;
                }
                elsif $sigil eq Q/@/ {               # a slurpy array?
                    $flags +|= $SIG_ELEM_SLURPY_POS;
                }
                elsif $sigil eq Q/%/ {               # a slurpy hash?
                    $flags +|= $SIG_ELEM_SLURPY_NAMED;
                }
            }

            if $name.substr(1,1) -> $twigil {
                if $twigil eq Q/!/ {
                    $flags +|= $SIG_ELEM_BIND_PRIVATE_ATTR;
                }
                elsif $twigil eq Q/./ {
                    $flags +|= $SIG_ELEM_BIND_PUBLIC_ATTR;
                }
            }

            set-sigil-bits($sigil, $flags);
            $name = $name.substr(1) if $sigil eq Q/\/ || $sigil eq Q/|/;
        }

        if %args.EXISTS-KEY('type') {
            my $type := %args.AT-KEY('type');
            if $type.DEFINITE {
                if nqp::istype($type,Str) {
                    if $type.ends-with(Q/)/) {
                        my $start = $type.index(Q/(/);
                        my $constraint-type :=
                          str-to-type($type.substr($start + 1, *-1), my $);
                        my $target-type :=
                          str-to-type($type.substr(0, $start), $flags);
                        $!type := Metamodel::CoercionHOW.new_type($target-type, $constraint-type);
                    }
                    else {
                        $!type := str-to-type($type, $flags)
                    }
                }
                else {
                    $!type := $type.WHAT;
                }
            }
            else {
                $!type := $type;
            }
        }
        else {
            $!type := Any;
        }

        if %args.EXISTS-KEY('default') {
            my $default := %args.AT-KEY('default');
            if nqp::istype($default,Code) {
                $!default_value := $default;
            }
            else {
                nqp::bind($!default_value,$default);
                $flags +|= $SIG_ELEM_DEFAULT_IS_LITERAL;
            }
            $flags +|= $SIG_ELEM_IS_OPTIONAL;
        }

        if %args.EXISTS-KEY('where') {
            nqp::bind(@!post_constraints,nqp::list(%args.AT-KEY('where')));
        }

        if %args.EXISTS-KEY('sub-signature') {
            $!sub_signature := %args.AT-KEY('sub-signature');
        }

        if $named {
            $flags +|= $SIG_ELEM_IS_OPTIONAL unless $mandatory;
            @!named_names := nqp::list_s($name.substr(1))
              unless @!named_names;
        }
        else {
            $flags +|= $SIG_ELEM_IS_OPTIONAL if $optional;
        }

        $flags +|= $SIG_ELEM_MULTI_INVOCANT if $multi-invocant;
        $flags +|= $SIG_ELEM_IS_COPY        if $is-copy;
        $flags +|= $SIG_ELEM_IS_RAW         if $is-raw;
        $flags +|= $SIG_ELEM_IS_RW          if $is-rw;
        $flags +|= $SIG_ELEM_IS_COERCIVE    if $!type.HOW.archetypes.coercive;

        $!variable_name = $name if $name;
        $!flags = $flags;
    }

    method name(Parameter:D: --> Str:D) {
        nqp::isnull_s($!variable_name) ?? '' !! $!variable_name
    }

    method usage-name(Parameter:D: --> Str:D) {
        nqp::isnull_s($!variable_name)
          ?? ''
          !! nqp::iseq_i(nqp::index('@$%&',nqp::substr($!variable_name,0,1)),-1)
            ?? $!variable_name
            !! nqp::iseq_i(nqp::index('*!.',nqp::substr($!variable_name,1,1)),-1)
              ?? nqp::substr($!variable_name,1)
              !! nqp::substr($!variable_name,2)
    }

    method sigil(Parameter:D: --> Str:D) {
        nqp::bitand_i($!flags,$SIG_ELEM_IS_CAPTURE)
          ?? '|'
          !! nqp::isnull_s($!variable_name)
            ?? nqp::bitand_i($!flags,$SIG_ELEM_ARRAY_SIGIL)
              ?? '@'
              !!  nqp::bitand_i($!flags,$SIG_ELEM_HASH_SIGIL)
                ?? '%'
                !! nqp::bitand_i($!flags,$SIG_ELEM_CODE_SIGIL)
                  ?? '&'
                  !! nqp::bitand_i($!flags,$SIG_ELEM_IS_RAW)
                    && $.name
                    && nqp::isnull($!default_value)
                    ?? '\\'
                    !! '$'
            !! nqp::bitand_i($!flags,$SIG_ELEM_IS_RAW) && nqp::iseq_i(
                 nqp::index('@$%&',nqp::substr($!variable_name,0,1)),-1)
              ?? '\\'
              !! nqp::substr($!variable_name,0,1)
    }

    method twigil(Parameter:D: --> Str:D) {
        nqp::bitand_i($!flags,$SIG_ELEM_BIND_PUBLIC_ATTR)
          ?? '.'
          !! nqp::bitand_i($!flags,$SIG_ELEM_BIND_PRIVATE_ATTR)
            ?? '!'
            !! nqp::isnull_s($!variable_name)
              ?? ''
              !! nqp::eqat($!variable_name,"*",1)
                ?? '*'
                !! ''
    }

    method prefix(Parameter:D: --> Str:D) {
        nqp::bitand_i($!flags, nqp::bitor_i($SIG_ELEM_SLURPY_POS, $SIG_ELEM_SLURPY_NAMED))
          ?? '*'
          !! nqp::bitand_i($!flags, $SIG_ELEM_SLURPY_LOL)
            ?? '**'
            !! nqp::bitand_i($!flags, $SIG_ELEM_SLURPY_ONEARG)
              ?? '+'
              !! ''
    }

    method suffix(Parameter:D: --> Str:D) {
        nqp::isnull(@!named_names)
          ?? nqp::bitand_i($!flags, $SIG_ELEM_IS_OPTIONAL) && nqp::isnull($!default_value)
            ?? '?'
            !! ''
          !! nqp::bitand_i($!flags, $SIG_ELEM_IS_OPTIONAL)
            ?? ''
            !! '!'
    }

    method modifier(Parameter:D: --> Str:D) {
        nqp::bitand_i($!flags,$SIG_ELEM_DEFINED_ONLY)
          ?? ':D'
          !! nqp::bitand_i($!flags,$SIG_ELEM_UNDEFINED_ONLY)
            ?? ':U'
            !! ''
    }

    method constraint_list(Parameter:D: --> List:D) {
        nqp::isnull(@!post_constraints) ?? () !! nqp::hllize(@!post_constraints)
    }

    method constraints(Parameter:D: --> Junction:D) {
        all(nqp::isnull(@!post_constraints) ?? () !! nqp::hllize(@!post_constraints))
    }

    method type(Parameter:D: --> Mu) { $!type }

    # XXX Must be marked as DEPRECATED
    method coerce_type(Parameter:D: --> Mu) { $!type.HOW.archetypes.coercive ?? $!type.^target_type !! Mu }

    method nominal_type(Parameter:D --> Mu) { $!type.HOW.archetypes.nominalizable ?? $!type.^nominalize !! $!type }

    method named_names(Parameter:D: --> List:D) {
        nqp::if(
          @!named_names && (my int $elems = nqp::elems(@!named_names)),
          nqp::stmts(
            (my $buf := nqp::setelems(nqp::create(IterationBuffer),$elems)),
            (my int $i = -1),
            nqp::while(
              nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
              nqp::bindpos($buf,$i,nqp::atpos_s(@!named_names,$i))
            ),
            $buf.List
          ),
          nqp::create(List)
        )
    }

    method named(Parameter:D: --> Bool:D) {
        nqp::hllbool(nqp::not_i(nqp::isnull(@!named_names)) || nqp::bitand_i($!flags,$SIG_ELEM_SLURPY_NAMED))
    }
    method positional(Parameter:D: --> Bool:D) {
        nqp::hllbool(nqp::isnull(@!named_names) && nqp::iseq_i(nqp::bitand_i($!flags,$SIG_ELEM_IS_NOT_POSITIONAL),0))
    }
    method slurpy(Parameter:D: --> Bool:D) {
        nqp::hllbool(nqp::bitand_i($!flags,$SIG_ELEM_IS_SLURPY))
    }
    method optional(Parameter:D: --> Bool:D) {
        nqp::hllbool(nqp::bitand_i($!flags,$SIG_ELEM_IS_OPTIONAL))
    }
    method raw(Parameter:D: --> Bool:D) {
        nqp::hllbool(nqp::bitand_i($!flags,$SIG_ELEM_IS_RAW))
    }
    method capture(Parameter:D: --> Bool:D) {
        nqp::hllbool(nqp::bitand_i($!flags,$SIG_ELEM_IS_CAPTURE))
    }
    method rw(Parameter:D: --> Bool:D) {
        nqp::hllbool(nqp::bitand_i($!flags,$SIG_ELEM_IS_RW))
    }
    method onearg(Parameter:D: --> Bool:D) {
        nqp::hllbool(nqp::bitand_i($!flags,$SIG_ELEM_SLURPY_ONEARG))
    }
    method copy(Parameter:D: --> Bool:D) {
        nqp::hllbool(nqp::bitand_i($!flags,$SIG_ELEM_IS_COPY))
    }
    method readonly(Parameter:D: --> Bool:D) {
        nqp::hllbool(nqp::iseq_i(nqp::bitand_i($!flags,$SIG_ELEM_IS_NOT_READONLY),0))
    }
    method invocant(Parameter:D: --> Bool:D) {
        nqp::hllbool(nqp::bitand_i($!flags,$SIG_ELEM_INVOCANT))
    }
    method multi-invocant(Parameter:D: --> Bool:D) {
        nqp::hllbool(nqp::bitand_i($!flags,$SIG_ELEM_MULTI_INVOCANT))
    }

    method default(Parameter:D: --> Code:_) {
        nqp::isnull($!default_value)
          ?? Code
          !! nqp::bitand_i($!flags,$SIG_ELEM_DEFAULT_IS_LITERAL)
            ?? { $!default_value }
            !! $!default_value
    }

    method type_captures(Parameter:D: --> List:D) {
        nqp::if(
          @!type_captures && (my int $elems = nqp::elems(@!type_captures)),
          nqp::stmts(
            (my $buf := nqp::setelems(nqp::create(IterationBuffer),$elems)),
            (my int $i = -1),
            nqp::while(
              nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
              nqp::bindpos($buf,$i,nqp::atpos_s(@!type_captures,$i))
            ),
            $buf.List
          ),
          nqp::create(List)
        )
    }

    multi method ACCEPTS(Parameter:D: Parameter:D \other --> Bool:D) {

        # we're us
        my \o := nqp::decont(other);
        return True if nqp::eqaddr(self,o);

        # nominal type is acceptable
        if $!type.ACCEPTS(nqp::getattr(o,Parameter,'$!type')) {
            my \oflags := nqp::getattr(o,Parameter,'$!flags');

            # flags are not same, so we need to look more in depth
            if nqp::isne_i($!flags,oflags) {

                # here not defined only, or both defined only
                return False
                  unless nqp::isle_i(
                    nqp::bitand_i($!flags,$SIG_ELEM_DEFINED_ONLY),
                    nqp::bitand_i( oflags,$SIG_ELEM_DEFINED_ONLY))

                # here not undefined only, or both undefined only
                  && nqp::isle_i(
                    nqp::bitand_i($!flags,$SIG_ELEM_UNDEFINED_ONLY),
                    nqp::bitand_i( oflags,$SIG_ELEM_UNDEFINED_ONLY))

                # here is rw, or both is rw
                  && nqp::isle_i(
                    nqp::bitand_i($!flags,$SIG_ELEM_IS_RW),
                    nqp::bitand_i( oflags,$SIG_ELEM_IS_RW))

                # other is optional, or both are optional
                  && nqp::isle_i(
                    nqp::bitand_i( oflags,$SIG_ELEM_IS_OPTIONAL),
                    nqp::bitand_i($!flags,$SIG_ELEM_IS_OPTIONAL))

                # other is slurpy positional, or both are slurpy positional
                  && nqp::isle_i(
                    nqp::bitand_i( oflags,$SIG_ELEM_SLURPY_POS),
                    nqp::bitand_i($!flags,$SIG_ELEM_SLURPY_POS))

                # other is slurpy named, or both are slurpy named
                  && nqp::isle_i(
                    nqp::bitand_i( oflags,$SIG_ELEM_SLURPY_NAMED),
                    nqp::bitand_i($!flags,$SIG_ELEM_SLURPY_NAMED))

                # other is slurpy one arg, or both are slurpy one arg
                  && nqp::isle_i(
                    nqp::bitand_i( oflags,$SIG_ELEM_SLURPY_ONEARG),
                    nqp::bitand_i($!flags,$SIG_ELEM_SLURPY_ONEARG))

                # here is part of MMD, or both are part of MMD
                  && nqp::isle_i(
                    nqp::bitand_i($!flags,$SIG_ELEM_MULTI_INVOCANT),
                    nqp::bitand_i( oflags,$SIG_ELEM_MULTI_INVOCANT));
            }
        }

        # nominal type not same
        else {
            return False;
        }

        # have nameds here
        my \onamed_names := nqp::getattr(o,Parameter,'@!named_names');
        if @!named_names {

            # nameds there
            if onamed_names {

                # too many nameds there, can never be subset
                my int $elems = nqp::elems(@!named_names);
                return False
                  if nqp::isgt_i(nqp::elems(onamed_names),$elems);

                # set up lookup hash
                my \lookup := nqp::hash;
                my int $i   = -1;
                nqp::bindkey(lookup,nqp::atpos_s(@!named_names,$i),1)
                  while nqp::islt_i(++$i,$elems);

                # make sure the other nameds are all here
                $elems = nqp::elems(onamed_names);
                $i     = -1;
                return False unless
                  nqp::existskey(lookup,nqp::atpos_s(onamed_names,$i))
                  while nqp::islt_i(++$i,$elems);
            }
        }

        # no nameds here, but we do there (implies not a subset)
        elsif onamed_names {
            return False;
        }

        # we have sub sig and not the same
        my \osub_signature := nqp::getattr(o,Parameter,'$!sub_signature');
        if $!sub_signature {
            return False
              unless osub_signature
              && $!sub_signature.ACCEPTS(osub_signature);
        }

        # no sub sig, but other has one
        elsif osub_signature {
            return False;
        }

        # we have a post constraint
        if nqp::islist(@!post_constraints) {

            # callable means runtime check, so no match
            return False
              if nqp::istype(nqp::atpos(@!post_constraints,0),Callable);

            # other doesn't have a post constraint
            my \opc := nqp::getattr(o,Parameter,'@!post_constraints');
            return False unless nqp::islist(opc);

            # other post constraint is a Callable, so runtime check, so no match
            return False if nqp::istype(nqp::atpos(opc,0),Callable);

            # not same literal value
            return False
              unless nqp::atpos(@!post_constraints,0).ACCEPTS(
                nqp::atpos(opc,0));
        }

        # we don't, other *does* have a post constraint
        elsif nqp::islist(nqp::getattr(o,Parameter,'@!post_constraints')) {
            return False;
        }

        # it's a match!
        True;
    }

    multi method raku(Parameter:D: Mu:U :$elide-type = Any --> Str:D) {
        my $raku = '';
        $raku ~= "::$_ " for @.type_captures;

        my $modifier = $.modifier;
        my $type     = $!type.^name;
        if $!flags +& $SIG_ELEM_ARRAY_SIGIL or
            $!flags +& $SIG_ELEM_HASH_SIGIL or
            $!flags +& $SIG_ELEM_CODE_SIGIL {
            $type ~~ / .*? \[ <( .* )> \] $$/;
            $raku ~= $/ ~ $modifier if $/;
        }
        elsif $modifier or
                !nqp::eqaddr($!type, nqp::decont($elide-type)) {
            $raku ~= $type ~ $modifier;
        }

        my $prefix     = $.prefix;
        my $sigil      = $.sigil;
        my $twigil     = $.twigil;
        my $usage-name = $.usage-name // '';
        my $name       = '';
        if $prefix eq '+' && $sigil eq '\\' {
            # We don't want \ to end up in the name of slurpy parameters, but
            # we still need to know whether or not they have this sigil later.
            $name ~= $usage-name;
        } else {
            $name ~= $sigil ~ $twigil ~ $usage-name;
        }
        if nqp::isconcrete(@!named_names) {
            my $var-is-named = False;
            my @outer-names  = gather for @.named_names {
                if !$var-is-named && $_ eq $usage-name {
                    $var-is-named = True;
                } else {
                    .take;
                }
            };
            $name = ":$name" if $var-is-named;
            $name = ":$_\($name)" for @outer-names;
        }

        my $rest = '';
        if $!flags +& $SIG_ELEM_IS_RW {
            $rest ~= ' is rw';
        } elsif $!flags +& $SIG_ELEM_IS_COPY {
            $rest ~= ' is copy';
        }
        if $!flags +& $SIG_ELEM_IS_RAW && $sigil ne '\\' | '|' {
            # Do not emit cases of anonymous '\' which we cannot reparse
            # This is all due to unspace.
            $rest ~= ' is raw';
        }
        unless nqp::isnull($!sub_signature) {
            $rest ~= ' ' ~ $!sub_signature.raku.substr: 1;
        }
        unless nqp::isnull(@!post_constraints) {
            # it's a Cool constant
            if !$rest
              && $name eq '$'
              && nqp::elems(@!post_constraints) == 1
              && nqp::istype(
                   (my \value := nqp::atpos(@!post_constraints,0)),
                   Cool
                 ) {
                return value.raku;
            }

            $rest ~= ' where { ... }';
        }
        if $.default {
            $rest ~= " = $!default_value.raku()";
        }
        elsif $!flags +& $SIG_ELEM_DEFAULT_FROM_OUTER {
            $rest ~= " = OUTER::<$name>";
        }

        $name = "$prefix$name$.suffix";
        $raku ~= ($raku ?? ' ' !! '') ~ $name if $name;
        $raku ~= $rest if $rest;
        $raku
    }

    method sub_signature(Parameter:D: --> Signature:_) {
        nqp::isnull($!sub_signature) ?? Signature !! $!sub_signature
    }

    method set_why(Parameter:D: $why --> Nil) {
        $!why := $why;
    }

    method set_default(Parameter:D: Code:D $default --> Nil) {
        $!default_value := $default;
    }
}

multi sub infix:<eqv>(Parameter:D \a, Parameter:D \b) {

    # we're us
    return True if a =:= b;

    # different container type
    return False unless a.WHAT =:= b.WHAT;

    # different nominal or coerce type
    my \atype = nqp::getattr(a,Parameter,'$!type');
    my \btype = nqp::getattr(b,Parameter,'$!type');
    # (atype is btype) && (btype is atype) ensures type equivalence. Works for different curryings of a parametric role
    # which are parameterized with the same argument. nqp::eqaddr is not applicable here because if coming from
    # different compunits the curryings would be different typeobject instances.
    return False
        unless
            (atype.HOW.archetypes.generic && btype.HOW.archetypes.generic)
            || (nqp::istype(atype, btype)
                && nqp::istype(btype, atype));

    # different flags
    return False
      if nqp::isne_i(
        nqp::getattr(a,Parameter,'$!flags'),
        nqp::getattr(b,Parameter,'$!flags')
      );

    # only pass if both subsignatures are defined and equivalent
    my \asub_signature := nqp::getattr(a,Parameter,'$!sub_signature');
    my \bsub_signature := nqp::getattr(b,Parameter,'$!sub_signature');
    if asub_signature {
        return False
          unless bsub_signature
          && (asub_signature eqv bsub_signature);
    }
    elsif bsub_signature {
        return False;
    }

    # first is named
    if a.named {

        # other is not named
        return False unless b.named;

        # not both actually have a name (e.g. *%_ doesn't)
        my $anames := nqp::getattr(a.named_names,List,'$!reified');
        my $bnames := nqp::getattr(b.named_names,List,'$!reified');
        my int $adefined = nqp::defined($anames);
        return False if nqp::isne_i($adefined,nqp::defined($bnames));

        # not same basic name
        return False
          if $adefined
          && nqp::isne_s(nqp::atpos($anames,0),nqp::atpos($bnames,0));
    }

    # unnamed vs named
    elsif b.named {
        return False;
    }

    # first has a post constraint
    my Mu $pca := nqp::getattr(a,Parameter,'@!post_constraints');
    if nqp::islist($pca) {

        # callable means runtime check, so no match
        return False if nqp::istype(nqp::atpos($pca,0),Callable);

        # second doesn't have a post constraint
        my Mu $pcb := nqp::getattr(b,Parameter,'@!post_constraints');
        return False unless nqp::islist($pcb);

        # second is a Callable, so runtime check, so no match
        return False if nqp::istype(nqp::atpos($pcb,0),Callable);

        # not same literal value
        return False unless nqp::atpos($pca,0) eqv nqp::atpos($pcb,0);
    }

    # first doesn't, second *does* have a post constraint
    elsif nqp::islist(nqp::getattr(b,Parameter,'@!post_constraints')) {
        return False;
    }

    # it's a match
    True
}

# vim: expandtab shiftwidth=4
