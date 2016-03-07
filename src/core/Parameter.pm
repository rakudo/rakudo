my class Parameter { # declared in BOOTSTRAP
    # class Parameter is Any {
    #     has str $!variable_name
    #     has Mu $!named_names
    #     has Mu $!type_captures
    #     has int $!flags
    #     has Mu $!nominal_type
    #     has Mu $!post_constraints
    #     has Mu $!coerce_type
    #     has str $!coerce_method
    #     has Mu $!sub_signature
    #     has Mu $!default_value
    #     has Mu $!container_descriptor;
    #     has Mu $!attr_package;
    #     has Mu $!why;

    my constant $SIG_ELEM_BIND_CAPTURE       = 1;
    my constant $SIG_ELEM_BIND_PRIVATE_ATTR  = 2;
    my constant $SIG_ELEM_BIND_PUBLIC_ATTR   = 4;
    my constant $SIG_ELEM_SLURPY_POS         = 8;
    my constant $SIG_ELEM_SLURPY_NAMED       = 16;
    my constant $SIG_ELEM_SLURPY_LOL         = 32;
    my constant $SIG_ELEM_INVOCANT           = 64;
    my constant $SIG_ELEM_MULTI_INVOCANT     = 128;
    my constant $SIG_ELEM_IS_RW              = 256;
    my constant $SIG_ELEM_IS_COPY            = 512;
    my constant $SIG_ELEM_IS_RAW             = 1024;
    my constant $SIG_ELEM_IS_OPTIONAL        = 2048;
    my constant $SIG_ELEM_ARRAY_SIGIL        = 4096;
    my constant $SIG_ELEM_HASH_SIGIL         = 8192;
    my constant $SIG_ELEM_IS_CAPTURE         = 32768;
    my constant $SIG_ELEM_UNDEFINED_ONLY     = 65536;
    my constant $SIG_ELEM_DEFINED_ONLY       = 131072;
    my constant $SIG_ELEM_SLURPY_ONEARG      = 16777216;

    my constant $SIG_ELEM_IS_NOT_POSITIONAL = $SIG_ELEM_SLURPY_POS
                                           +| $SIG_ELEM_SLURPY_NAMED
                                           +| $SIG_ELEM_IS_CAPTURE;
    my constant $SIG_ELEM_IS_SLURPY = $SIG_ELEM_SLURPY_POS
                                   +| $SIG_ELEM_SLURPY_NAMED
                                   +| $SIG_ELEM_SLURPY_LOL
                                   +| $SIG_ELEM_SLURPY_ONEARG;
    my constant $SIG_ELEM_IS_NOT_READONLY = $SIG_ELEM_IS_RW
                                         +| $SIG_ELEM_IS_COPY
                                         +| $SIG_ELEM_IS_RAW;

    method name() {
        nqp::isnull_s($!variable_name) ?? Nil !! $!variable_name
    }
    method usage-name() {
        nqp::iseq_i(nqp::index('@$%&',nqp::substr($!variable_name,0,1)),-1)
          ?? $!variable_name
          !! nqp::substr($!variable_name,1)
    }

    method sigil() {
        nqp::bitand_i($!flags,$SIG_ELEM_IS_CAPTURE)
          ?? '|'
          !! nqp::isnull_s($!variable_name)
            ?? nqp::bitand_i($!flags,$SIG_ELEM_ARRAY_SIGIL)
              ?? '@'
              !!  nqp::bitand_i($!flags,$SIG_ELEM_HASH_SIGIL)
                ?? '%'
                !! nqp::eqat(nqp::unbox_s($!nominal_type.^name),'Callable',0)
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
        nqp::isnull($!post_constraints) ?? () !!
            nqp::hllize($!post_constraints)
    }

    method constraints() {
        all(nqp::isnull($!post_constraints) ?? () !!
            nqp::hllize($!post_constraints))
    }

    method type() { $!nominal_type }
    method named_names() {
        nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',$!named_names)
    }
    method named() {
        nqp::p6bool(
          $!named_names || nqp::bitand_i($!flags,$SIG_ELEM_SLURPY_NAMED)
        )
    }

    method positional() {
        nqp::p6bool(
          nqp::isnull($!named_names)
          && nqp::iseq_i(nqp::bitand_i($!flags,$SIG_ELEM_IS_NOT_POSITIONAL),0)
        )
    }

    method slurpy() {
        nqp::p6bool(nqp::bitand_i($!flags,$SIG_ELEM_IS_SLURPY))
    }
    method optional() {
        nqp::p6bool(nqp::bitand_i($!flags,$SIG_ELEM_IS_OPTIONAL))
    }
    method raw() {
        nqp::p6bool(nqp::bitand_i($!flags,$SIG_ELEM_IS_RAW))
    }
    method capture() {
        nqp::p6bool(nqp::bitand_i($!flags,$SIG_ELEM_IS_CAPTURE))
    }
    method rw() {
        nqp::p6bool(nqp::bitand_i($!flags,$SIG_ELEM_IS_RW))
    }
    method onearg() {
        nqp::p6bool(nqp::bitand_i($!flags,$SIG_ELEM_SLURPY_ONEARG))
    }
    method copy() {
        nqp::p6bool(nqp::bitand_i($!flags,$SIG_ELEM_IS_COPY))
    }
    method readonly() {
        nqp::p6bool(
          nqp::iseq_i(nqp::bitand_i($!flags,$SIG_ELEM_IS_NOT_READONLY),0)
        )
    }
    method invocant() {
        nqp::p6bool(nqp::bitand_i($!flags,$SIG_ELEM_INVOCANT))
    }
    method multi-invocant() {
        nqp::p6bool(nqp::bitand_i($!flags,$SIG_ELEM_MULTI_INVOCANT))
    }
    method default() {
        nqp::isnull($!default_value)
          ?? Any
          !! nqp::istype($!default_value,Code)
            ?? $!default_value
            !! { $!default_value }
    }
    method type_captures() {
        nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',$!type_captures)
    }

    method !flags() { $!flags }

    multi method ACCEPTS(Parameter:D: Parameter:D \other) {

        # we're us
        my \o := nqp::decont(other);
        return True if self =:= o;

        # nominal type is acceptable
        if $!nominal_type.ACCEPTS(nqp::getattr(o,Parameter,'$!nominal_type')) {
            my $oflags := nqp::getattr(o,Parameter,'$!flags');

            # here not defined only, or both defined only
            return False
              unless nqp::isle_i(
                nqp::bitand_i($!flags,$SIG_ELEM_DEFINED_ONLY),
                nqp::bitand_i($oflags,$SIG_ELEM_DEFINED_ONLY))

            # here not undefined only, or both undefined only
              && nqp::isle_i(
                nqp::bitand_i($!flags,$SIG_ELEM_UNDEFINED_ONLY),
                nqp::bitand_i($oflags,$SIG_ELEM_UNDEFINED_ONLY))

            # here is rw, or both is rw
              && nqp::isle_i(
                nqp::bitand_i($!flags,$SIG_ELEM_IS_RW),
                nqp::bitand_i($oflags,$SIG_ELEM_IS_RW))

            # here is part of MMD, or both are part of MMD
              && nqp::isle_i(
                nqp::bitand_i($!flags,$SIG_ELEM_MULTI_INVOCANT),
                nqp::bitand_i($oflags,$SIG_ELEM_MULTI_INVOCANT));
        }

        # nominal type not same
        else {
            return False;
        }

        # we have sub sig and not the same
        my $osub_signature := nqp::getattr(o,Parameter,'$!sub_signature');
        if $!sub_signature {
            return False
              unless $osub_signature
              && $!sub_signature.ACCEPTS($osub_signature);
        }

        # no sub sig, but other has one
        elsif $osub_signature {
            return False;
        }

        # have nameds here
        my $onamed_names := nqp::getattr(o,Parameter,'$!named_names');
        if $!named_names {

            # nameds there
            if $onamed_names {

                # too many nameds there, can never be subset
                my int $elems = nqp::elems($!named_names);
                return False
                  if nqp::isgt_i(nqp::elems($onamed_names),$elems);

                # set up lookup hash
                my $lookup := nqp::hash;
                my int $i   = -1;
                nqp::bindkey($lookup,nqp::atpos($!named_names,$i),1)
                  while nqp::islt_i($i = nqp::add_i($i,1),$elems);

                # make sure the other nameds are all here
                $elems = nqp::elems($onamed_names);
                $i     = -1;
                return False unless
                  nqp::existskey($lookup,nqp::atpos($onamed_names,$i))
                  while nqp::islt_i($i = nqp::add_i($i,1),$elems);
            }
        }

        # no nameds here, but we do there (implies not a subset)
        elsif $onamed_names {
            return False;
        }

        # we have a post constraint
        if nqp::islist($!post_constraints) {

            # callable means runtime check, so no match
            return False
              if nqp::istype(nqp::atpos($!post_constraints,0),Callable);

            # other doesn't have a post constraint
            my Mu $opc := nqp::getattr(o,Parameter,'$!post_constraints');
            return False unless nqp::islist($opc);

            # other post constraint is a Callable, so runtime check, so no match
            return False if nqp::istype(nqp::atpos($opc,0),Callable);

            # not same literal value
            return False
              unless nqp::atpos($!post_constraints,0).ACCEPTS(
                nqp::atpos($opc,0));
        }

        # we don't, other *does* have a post constraint
        elsif nqp::islist(nqp::getattr(o,Parameter,'$!post_constraints')) {
            return False;
        }

        # it's a match!
        True;
    }

    multi method perl(Parameter:D: Mu:U :$elide-type = Any, :&where = -> $ { 'where { ... }' }) {
        my $perl = '';
        my $rest = '';
        my $type = $!nominal_type.^name;
        my $modifier = self.modifier;

        $perl ~= "::$_ " for @($.type_captures);
        # XXX Need a CODE_SIGIL too?
        if $!flags +& $SIG_ELEM_ARRAY_SIGIL or
            $!flags +& $SIG_ELEM_HASH_SIGIL or
            $type ~~ /^^ Callable >> / {
            $type ~~ / .*? \[ <( .* )> \] $$/;
            $perl ~= $/ ~ $modifier if $/;
        }
        elsif $modifier or
                !nqp::eqaddr(nqp::decont($!nominal_type), nqp::decont($elide-type)) {
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
            } elsif $type ~~ /^^ Callable >> / {
                $name = '&';
            } else {
                $name = '$';
            }
        }
        my $default = self.default();
        if self.slurpy {
            $name = ($!flags +& $SIG_ELEM_SLURPY_ONEARG ?? '+' !! ($!flags +& $SIG_ELEM_SLURPY_LOL ?? "**" !! "*") ~ $name);
        } elsif self.named {
            my $name1 := substr($name,1);
            if @(self.named_names).first({$_ && $_ eq $name1}) {
                $name = ':' ~ $name;
            }
            for @(self.named_names).grep({$_ && $_ ne $name1}) {
                $name = ':' ~ $_ ~ '(' ~ $name ~ ')';
            }
            $name ~= '!' unless self.optional;
        } elsif self.optional && !$default {
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
            $rest ~= ' is raw' unless $name.starts-with('\\');
        }
        unless nqp::isnull($!sub_signature) {
            my $sig = $!sub_signature.perl();
            $sig ~~ s/^^ ':'//;
            $rest ~= ' ' ~ $sig;
        }
        unless nqp::isnull($!post_constraints) {
            my $where = &where(self);
            return Nil without $where;
            $rest ~= " $where";
        }
        $rest ~= " = $!default_value.perl()" if $default;
        if $name or $rest {
            $perl ~= ($perl ?? ' ' !! '') ~ $name;
        }
        $perl ~ $rest;
    }

    method sub_signature(Parameter:D:) {
        nqp::isnull($!sub_signature) ?? Any !! $!sub_signature
    }

    method set_why($why) {
        $!why := $why;
    }
}

multi sub infix:<eqv>(Parameter \a, Parameter \b) {

    # we're us
    return True if a =:= b;

    # different nominal or coerce type
    return False
      unless nqp::iseq_s(
          nqp::getattr(a,Parameter,'$!nominal_type').^name,
          nqp::getattr(b,Parameter,'$!nominal_type').^name
        )
      && nqp::iseq_s(
          nqp::getattr(a,Parameter,'$!coerce_type').^name,
          nqp::getattr(b,Parameter,'$!coerce_type').^name
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
    my Mu $pca := nqp::getattr(a,Parameter,'$!post_constraints');
    if nqp::islist($pca) {

        # callable means runtime check, so no match
        return False if nqp::istype(nqp::atpos($pca,0),Callable);

        # second doesn't have a post constraint
        my Mu $pcb := nqp::getattr(b,Parameter,'$!post_constraints');
        return False unless nqp::islist($pcb);

        # second is a Callable, so runtime check, so no match
        return False if nqp::istype(nqp::atpos($pcb,0),Callable);

        # not same literal value
        return False unless nqp::atpos($pca,0) eqv nqp::atpos($pcb,0);
    }

    # first doesn't, second *does* have a post constraint
    elsif nqp::islist(nqp::getattr(b,Parameter,'$!post_constraints')) {
        return False;
    }

    # it's a match
    True
}

# vim: ft=perl6 expandtab sw=4
