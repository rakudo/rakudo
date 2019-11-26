my class Parameter { # declared in BOOTSTRAP
    # class Parameter is Any
    #     has str $!variable_name
    #     has @!named_names
    #     has @!type_captures
    #     has int $!flags
    #     has Mu $!nominal_type
    #     has @!post_constraints
    #     has Mu $!coerce_type
    #     has str $!coerce_method
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

    my $sigils2bit := nqp::null;
    sub set-sigil-bits(str $sigil, \flags --> Nil) {
        if nqp::atkey(
          nqp::ifnull(
            $sigils2bit,
            $sigils2bit := nqp::hash(
              Q/@/, $SIG_ELEM_ARRAY_SIGIL,
              Q/%/, $SIG_ELEM_HASH_SIGIL,
              Q/&/, $SIG_ELEM_CODE_SIGIL,
              Q/\/, $SIG_ELEM_IS_RAW,
              Q/|/, $SIG_ELEM_IS_CAPTURE +| $SIG_ELEM_IS_RAW,
            )
          ),
          $sigil
        ) -> $bit {
            flags +|= $bit
        }
    }

    sub definitize-type(Str:D $type, Bool:D $definite) {
        Metamodel::DefiniteHOW.new_type(:base_type(::($type)), :$definite)
    }

    sub str-to-type(Str:D $type, $flags is rw) {
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
       Str:D $name      is copy = "",
       Int:D $flags     is copy = 0,
      Bool:D $named     is copy = False,
      Bool:D $optional  is copy = False,
      Bool:D $mandatory is copy = False,
      Bool:D $is-copy = False,
      Bool:D $is-raw = False,
      Bool:D $is-rw = False,
      Bool:D $multi-invocant = True,
             *%args  # type / default / where / sub_signature captured through %_
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

                if %_.EXISTS-KEY('type') {
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
                        $!nominal_type :=
                          str-to-type($type.substr($start + 1, *-1), my $);
                        $!coerce_type :=
                          str-to-type($type.substr(0, $start), $flags);
                    }
                    else {
                        $!nominal_type := str-to-type($type, $flags)
                    }
                }
                else {
                    $!nominal_type := $type.WHAT;
                }
            }
            else {
                $!nominal_type := $type;
            }
        }
        else {
            $!nominal_type := Any;
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

        $!variable_name = $name if $name;
        $!flags = $flags;
        self
    }

    method name() {
        nqp::isnull_s($!variable_name) ?? Nil !! $!variable_name
    }
    method usage-name() {
        nqp::iseq_i(nqp::index('@$%&',nqp::substr($!variable_name,0,1)),-1)
          ?? $!variable_name
          !! nqp::iseq_i(nqp::index('*!',nqp::substr($!variable_name,1,1)),-1)
            ?? nqp::substr($!variable_name,1)
            !! nqp::substr($!variable_name,2)

    }

    method sigil() {
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
                    ?? '\\'
                    !! '$'
            !! nqp::bitand_i($!flags,$SIG_ELEM_IS_RAW) && nqp::iseq_i(
                 nqp::index('@$%&',nqp::substr($!variable_name,0,1)),-1)
              ?? '\\'
              !! nqp::substr($!variable_name,0,1)
    }

    method twigil() {
        nqp::bitand_i($!flags,$SIG_ELEM_BIND_PUBLIC_ATTR)
          ?? '.'
          !! nqp::bitand_i($!flags,$SIG_ELEM_BIND_PRIVATE_ATTR)
            ?? '!'
            !! nqp::isnull_s($!variable_name)
              ?? ''
              !! nqp::iseq_s(nqp::substr($!variable_name,1,1),"*")
                ?? '*'
                !! ''
    }
    method modifier() {
        nqp::bitand_i($!flags,$SIG_ELEM_DEFINED_ONLY)
          ?? ':D'
          !! nqp::bitand_i($!flags,$SIG_ELEM_UNDEFINED_ONLY)
            ?? ':U'
            !! ''
    }

    method constraint_list() {
        nqp::isnull(@!post_constraints) ?? () !!
            nqp::hllize(@!post_constraints)
    }

    method constraints() {
        all(nqp::isnull(@!post_constraints) ?? () !!
            nqp::hllize(@!post_constraints))
    }

    method type() { $!nominal_type }
    method coerce_type() { $!coerce_type }
    method named_names() {
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
    method named() {
        nqp::hllbool(
          nqp::not_i(nqp::isnull(@!named_names)) || nqp::bitand_i($!flags,$SIG_ELEM_SLURPY_NAMED)
        )
    }

    method positional() {
        nqp::hllbool(
          nqp::isnull(@!named_names)
          && nqp::iseq_i(nqp::bitand_i($!flags,$SIG_ELEM_IS_NOT_POSITIONAL),0)
        )
    }

    method slurpy() {
        nqp::hllbool(nqp::bitand_i($!flags,$SIG_ELEM_IS_SLURPY))
    }
    method optional() {
        nqp::hllbool(nqp::bitand_i($!flags,$SIG_ELEM_IS_OPTIONAL))
    }
    method raw() {
        nqp::hllbool(nqp::bitand_i($!flags,$SIG_ELEM_IS_RAW))
    }
    method capture() {
        nqp::hllbool(nqp::bitand_i($!flags,$SIG_ELEM_IS_CAPTURE))
    }
    method rw() {
        nqp::hllbool(nqp::bitand_i($!flags,$SIG_ELEM_IS_RW))
    }
    method onearg() {
        nqp::hllbool(nqp::bitand_i($!flags,$SIG_ELEM_SLURPY_ONEARG))
    }
    method copy() {
        nqp::hllbool(nqp::bitand_i($!flags,$SIG_ELEM_IS_COPY))
    }
    method readonly() {
        nqp::hllbool(
          nqp::iseq_i(nqp::bitand_i($!flags,$SIG_ELEM_IS_NOT_READONLY),0)
        )
    }
    method invocant() {
        nqp::hllbool(nqp::bitand_i($!flags,$SIG_ELEM_INVOCANT))
    }
    method multi-invocant() {
        nqp::hllbool(nqp::bitand_i($!flags,$SIG_ELEM_MULTI_INVOCANT))
    }
    method default() {
        nqp::isnull($!default_value)
          ?? Any
          !! nqp::bitand_i($!flags,$SIG_ELEM_DEFAULT_IS_LITERAL)
            ?? { $!default_value }
            !! $!default_value
    }
    method type_captures() {
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

    method !flags() { $!flags }

    multi method ACCEPTS(Parameter:D: Parameter:D \other) {

        # we're us
        my \o := nqp::decont(other);
        return True if nqp::eqaddr(self,o);

        # nominal type is acceptable
        if $!nominal_type.ACCEPTS(nqp::getattr(o,Parameter,'$!nominal_type')) {
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

    multi method perl(Parameter:D: Mu:U :$elide-type = Any) {
        my $perl = '';
        my $rest = '';
        my $type = $!nominal_type.^name;
        $type = $!coerce_type.^name ~ "($type)"
          unless nqp::isnull($!coerce_type);
        my $modifier = self.modifier;

        $perl ~= "::$_ " for @($.type_captures);
        if $!flags +& $SIG_ELEM_ARRAY_SIGIL or
            $!flags +& $SIG_ELEM_HASH_SIGIL or
            $!flags +& $SIG_ELEM_CODE_SIGIL {
            $type ~~ / .*? \[ <( .* )> \] $$/;
            $perl ~= $/ ~ $modifier if $/;
        }
        elsif $modifier or
                !nqp::eqaddr($!nominal_type, nqp::decont($elide-type)) {
            $perl ~= $type ~ $modifier;
        }
        my $name = $.name;
        if $name {
            if $!flags +& $SIG_ELEM_IS_CAPTURE {
                $name = '|' ~ $name;
            } elsif $!flags +& $SIG_ELEM_IS_RAW {
                $name = '\\' ~ $name without '@$%&'.index(substr($name,0,1));
            }
        } else {
            if $!flags +& $SIG_ELEM_IS_CAPTURE {
                $name = '|';
            } elsif $!flags +& $SIG_ELEM_ARRAY_SIGIL {
                $name = '@';
            } elsif $!flags +& $SIG_ELEM_HASH_SIGIL {
                $name = '%';
            } elsif $!flags +& $SIG_ELEM_CODE_SIGIL {
                $name = '&';
            } else {
                $name = '$';
            }
        }
        my $default = self.default();
        if self.slurpy {
            $name = $!flags +& $SIG_ELEM_SLURPY_ONEARG
              ?? "+$name"
              !! $!flags +& $SIG_ELEM_SLURPY_LOL
                ?? "**$name"
                !! "*$name";

        } elsif self.named {
            my $name1 := substr($name,1);
            if @(self.named_names).first({$_ && $_ eq $name1}) {
                $name = ':' ~ $name;
            }
            for @(self.named_names).grep({$_ && $_ ne $name1}) {
                $name = ':' ~ $_ ~ '(' ~ $name ~ ')';
            }
            $name ~= '!' unless self.optional;
        } elsif self.optional
          && !$default
          && not $!flags +& $SIG_ELEM_DEFAULT_FROM_OUTER {
            $name ~= '?';
        }
        if $!flags +& $SIG_ELEM_IS_RW {
            $rest ~= ' is rw';
        } elsif $!flags +& $SIG_ELEM_IS_COPY {
            $rest ~= ' is copy';
        }
        if $!flags +& $SIG_ELEM_IS_RAW {
            # Do not emit cases of anonymous '\' which we cannot reparse
            # This is all due to unspace.
            $rest ~= ' is raw' unless $name eq '|' or $name.starts-with('\\');
        }
        unless nqp::isnull($!sub_signature) {
            my $sig = $!sub_signature.perl();
            $sig ~~ s/^^ ':'//;
            $rest ~= ' ' ~ $sig;
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
                return value.perl;
            }

            $rest ~= ' where { ... }';
        }
        if $default {
            $rest ~= " = $!default_value.perl()";
        }
        elsif $!flags +& $SIG_ELEM_DEFAULT_FROM_OUTER {
            $rest ~= " = OUTER::<$name>";
        }
        if $name or $rest {
            $perl ~= ($perl ?? ' ' !! '') ~ $name;
        }
        $perl ~ $rest;
    }

    method sub_signature(Parameter:D:) {
        nqp::isnull($!sub_signature) ?? Any !! $!sub_signature
    }

    method set_why($why --> Nil) {
        $!why := $why;
    }

    method set_default(Code:D $default --> Nil) {
        $!default_value := $default;
    }
}

multi sub infix:<eqv>(Parameter:D \a, Parameter:D \b) {

    # we're us
    return True if a =:= b;

    # different container type
    return False unless a.WHAT =:= b.WHAT;

    # different nominal or coerce type
    my $acoerce := nqp::getattr(a,Parameter,'$!coerce_type');
    my $bcoerce := nqp::getattr(b,Parameter,'$!coerce_type');
    return False
      unless nqp::iseq_s(
          nqp::getattr(a,Parameter,'$!nominal_type').^name,
          nqp::getattr(b,Parameter,'$!nominal_type').^name
        )
      && nqp::iseq_s(
          nqp::isnull($acoerce) ?? "" !! $acoerce.^name,
          nqp::isnull($bcoerce) ?? "" !! $bcoerce.^name
        );

    # different flags
    return False
      if nqp::isne_i(
        nqp::getattr(a,Parameter,'$!flags'),
        nqp::getattr(b,Parameter,'$!flags')
      );

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

# vim: ft=perl6 expandtab sw=4
