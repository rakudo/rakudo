my class Block { # declared in BOOTSTRAP
    # class Block is Code
    #     has Mu $!phasers;
    #     has Mu $!why;

    method of(Block:D:)      { nqp::getattr(self,Code,'$!signature').returns }
    method returns(Block:D:) { nqp::getattr(self,Code,'$!signature').returns }

    method add_phaser(Str:D \name, &block --> Nil) {
        $!phasers := nqp::hash
          unless nqp::attrinited(self,Block,'$!phasers');

        my str $name = name;
        nqp::bindkey($!phasers,$name,nqp::list)
          unless nqp::existskey($!phasers,$name);

        if nqp::iseq_s($name,'LEAVE') || nqp::iseq_s($name,'KEEP') || nqp::iseq_s($name,'UNDO') {
            nqp::unshift(nqp::atkey($!phasers,$name),&block);
            self.add_phaser('!LEAVE-ORDER', &block);
        }
        elsif nqp::iseq_s($name,'NEXT') || nqp::iseq_s($name,'!LEAVE-ORDER') || nqp::iseq_s($name,'POST') {
            nqp::unshift(nqp::atkey($!phasers,$name),&block);
        }
        else {
            nqp::push(nqp::atkey($!phasers,$name),&block);
        }
    }

    method fire_phasers(Str $name --> Nil) {
        nqp::if(
          nqp::attrinited(self,Block,'$!phasers')
            && nqp::existskey($!phasers,$name),
          nqp::stmts(
            (my $iter := nqp::iterator(nqp::atkey($!phasers,$name))),
            nqp::while($iter,nqp::shift($iter)(),:nohandler)
          )
        )
    }

    method has-phasers() { nqp::attrinited(self,Block,'$!phasers') }

    method has-phaser(Str:D \name) {
        nqp::attrinited(self,Block,'$!phasers')
          && nqp::existskey($!phasers,nqp::unbox_s(name))
    }

    method phasers(Str:D $name) {
        nqp::attrinited(self,Block,'$!phasers')
          && nqp::existskey($!phasers,nqp::unbox_s($name))
          ?? nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',
               nqp::atkey($!phasers,nqp::unbox_s($name)))
          !! ()
    }

    method assuming(Block:D $self: |primers) {
        my $sig = nqp::getattr(nqp::decont($self), Code, '$!signature');

        # A ::() that does not throw.  Also does not need to deal
        # with chunks or sigils.
        my sub soft_indirect_name_lookup($name) {
            my @parts    = $name.split('::');

            my Mu $thing := ::.EXISTS-KEY(@parts[0]);
            return False unless $thing;
            $thing := ::.AT-KEY(@parts.shift);
            for @parts {
                return False unless $thing.WHO.EXISTS-KEY($_);
                $thing := $thing.WHO{$_};
            }
            True;
        }

        # sub strip-parm
        # This is mostly a stripped-down version of Parameter.perl, removing
        # where clauses, turning "= { ... }" from defaults into just
        # "?", removing type captures, subsignatures, and undeclared types
        # (e.g. types set to or parameterized by captured types.)
        my sub strip_parm (Parameter:D $parm, :$make_optional = False) {
            my $type = $parm.type.^name;
            my $perl = $type;
            my $rest = '';
            my $sigil = $parm.sigil;
            my $elide_agg_cont= so ($sigil eqv '@'
                                    or $sigil eqv '%'
                                    or $type ~~ /^^ Callable >> /);

            $perl = '' if $elide_agg_cont;
            unless $type eq "Any" {
                my int $FIRST = 1; # broken FIRST workaround
                while ($type ~~ / (.*?) \[ (.*) \] $$/) {
#                   FIRST {  # seems broken in setting
                    if $FIRST { # broken FIRST workaround
                        $perl = $elide_agg_cont
                          ?? ~$1
                          !! ~$/;
                        $FIRST = 0;
                    }
                    $type = ~$1;
                    unless soft_indirect_name_lookup(~$0) {
                        $perl = '';
                        last
                    };
                }
                $perl = '' unless soft_indirect_name_lookup($type);
            }
            $perl ~= $parm.modifier if $perl ne '';

            my $name = $parm.name;
            if !$name and $parm.raw {
                $name = '$';
            } elsif !$name or !$name.starts-with($sigil) {
                $name = $sigil ~ $parm.twigil ~ ($name // '');
            }

            if $parm.slurpy {
                $name = '*' ~ $name;
            } elsif $parm.named {
                my @names := $parm.named_names;
                $name = ':' ~ $_ ~ '(' ~ $name ~ ')'for @names;
                $name ~= '!' unless ($parm.optional or $make_optional);
                $name ~= '?' if ($make_optional);
            } elsif $parm.optional or $parm.default {
                $name ~= '?';
            }

            if $parm.rw {
                $rest ~= ' is rw';
            } elsif $parm.copy {
                $rest ~= ' is copy';
            }
            if $parm.raw {
                $rest ~= ' is raw' unless $name.starts-with('\\');
            }
            if $name or $rest {
                $perl ~= ($perl ?? ' ' !! '') ~ $name;
            }
            $perl ~ $rest;
        }

        # If we have only one parameter and it is a capture with a
        # subsignature, we might as will jump down into it.
        while +$sig.params == 1
              and $sig.params[0].capture
              and $sig.params[0].sub_signature {
            $sig = $sig.params[0].sub_signature;
        }

        my @plist = (); # Positionals in the returned closure's signature
        my @clist = (); # The positional args used to call the original code
        my @tlist = (); # Positional params to verify binding primers against
        my @alist = (); # Primers as positional arguments after processing

        # Find a name safe to use across slurpies, captures and sigilless
        my $safename = '_';
        $safename ~= '_' while $sig.params.first:
            { $_.name.defined and $_.name eq $safename and
              ($_.slurpy or $_.sigil eq '\\' or $_.sigil eq '|') };
        my $capwrap = $safename ~ '_';
        $capwrap ~= '_' while $sig.params.first:
            { $_.name.defined and $_.name eq $capwrap and
                  ($_.slurpy or $_.sigil eq '\\' or $_.sigil eq '|') };


        # Look for slurpies and captures
        my $slurp_p = $sig.params.first: {.slurpy and .sigil eq '@'};
        my $slurp_n = $sig.params.first: {.slurpy and .sigil eq '%'};
        $slurp_p //= ();
        $slurp_n //= ();

        # This gets sticky.  A bare capture will take anything
        # you throw at it.  A capture with a subsignature, not always.
        # Both will raise Signature.count to Inf, unfortunately,
        # and neither counts towards Signature.arity.  That might
        # eventually change as it is LTA.
        #
        # We have no real use for any captures defined in the original
        # signature, but if there is one, we must emulate its slurpylike
        # effects.  We cannot tell if it actually has slurpylike
        # effects without looking at subsignatures, recursively,
        # but really Signature should be able to tell us that.
        #
        # Until then, we will add slurpy behaviors, assuming we
        # do not already have them, if we see a capture.
        my $need_cap = ($sig.count == Inf and not ($slurp_p and $slurp_n));
        if $need_cap {
            $need_cap = False;
            for $sig.params.grep(*.capture) {
                $need_cap = True;
                last;
            }
        }
        # For now this is how we fabricate parameters.
        my &safeparms = EVAL
            sprintf('sub (|%s) { }', $safename);
        if ($need_cap) {
            $slurp_p ||= &safeparms.signature.params[0];
            $slurp_n ||= &safeparms.signature.params[0];
        }
        # Normal Positionals
        my Int $idx = -1;
        for $sig.params.grep(*.positional) -> $parm {
            $idx++;
            unless $idx < primers.list.elems {
                @plist.push($parm);
                @clist.push($capwrap ~ '[' ~ @plist.end ~ ']');
                next;
            }
            given primers.list[$idx] {
                when Whatever { @plist.push($parm);
                                @clist.push($capwrap ~ '[' ~ @plist.end ~ ']');
                              }
                when Nil      { @alist.push($parm.type);
                                @clist.push($parm.type.^name);
                                @tlist.push($parm);
                              }
                default       { @alist.push($_);
                                @clist.push("primers.list[$idx]");
                                @tlist.push($parm);
                              }
            }
        }
        my $widx = @plist.end;
        @tlist.push($slurp_p) if $slurp_p;
        @plist.push($slurp_p) if $slurp_p and not $slurp_p.capture;

        $idx++;
        my $cidx = 0;

        # Even if we prime above the arity, do it anyway, for errors.
        while ($idx < primers.list.elems) {
            given primers.list[$idx] {
                when Whatever {
                    @clist.push($capwrap ~ '[' ~ ++$widx ~ ']');
                }
                when Nil {
                    my $t = "Any";
                    if $slurp_p {
                        unless $slurp_p.capture {
                            $t = $slurp_p.type.of.^name
                        }
                    }
                    @alist.push($t);
                    @clist.push($t);
                }
                default {
                    @alist.push($_);
                    @clist.push("primers.list[$idx]");
                }
            }
            $idx++;
        }
        if $slurp_p {
            @clist.push('|' ~ $capwrap ~ '[' ~ ++$widx ~ '..*-1]' );
            # If it is a true slurpy we already pushed it to $plist
            $slurp_p = () unless $slurp_p.capture;
        }

        # Normal Nameds.
        # I noted this:
        # perl6 -e 'sub a (*%A, :$a?, *%B) { %A.say; %B.say }; a(:a(1));'
        # {:a(1)}<>
        # {}<>
        # I am going to treat that as a feature and preserve the behavior.
        # So we will care for ordering of the named parameters in the
        # user-facing signature as well, for introspection purposes.
        my %ahash = primers.hash;
        my @phash = $sig.params.grep: *.named;
        my @thash = $sig.params.grep: {
            .named and (
                .slurpy or
                any(%ahash.keys) eq any(.named_names.list)
            )
        }
        @phash .= map: {
            my @names = .named_names.list;
            my $p = strip_parm($_);
            if not .optional and any(%ahash.keys) eq any(@names) {
                # Make mandatory parameters optional once they have
                # been supplied at least once.
                $p = strip_parm($_, :make_optional);
            }
            $p;
        }
        if ($slurp_n and $slurp_n.capture and !($slurp_n === $slurp_p)) {
            @phash.push(strip_parm($slurp_n));
        }
        my $error = False;
        EVAL(sprintf('anon sub trybind (%s) { }(|@alist, |%%ahash);',
                     (flat @tlist.map(&strip_parm),
                           @thash.map(&strip_parm)).join(", "))
             );

        my $f;
        my $primed_sig = (flat @plist.map(&strip_parm), @phash,
                          ($slurp_p ?? strip_parm($slurp_p) !! ())).join(", ");
        $primed_sig ~= ' --> ' ~ $sig.returns.^name;

        $f = EVAL sprintf(
            '{ my $res = (my proto __PRIMED_ANON (%s) { {*} });
               my multi __PRIMED_ANON (|%s(%s)) {
                   my %%chash := %s.hash;
                   $self(%s%s |{ %%ahash, %%chash }); # |{} workaround RT#77788
               };
               $res }()',
            $primed_sig, $capwrap, $primed_sig, $capwrap,
            (flat @clist).join(", "),
            (@clist ?? ',' !! '')
        );

        $error ~~ Exception ?? $f but Failure.new($error) !! $f;
    }

    multi method perl(Block:D:) {
        "-> {self.signature.perl.substr(2,*-1)} \{ #`({self.WHICH}) ... \}"
    }

    method WHY() {
        if nqp::isnull($!why) {
            Nil
        } else {
            $!why.set_docee(self);
            $!why
        }
    }

    method set_why($why) {
        $!why := $why;
    }

    # helper method for array slicing
    method pos(Block:D $self: \list) {
      nqp::if(
        (nqp::istype(
          (my $n := nqp::getattr(
            nqp::getattr($self,Code,'$!signature'),Signature,'$!count')
          ),Num) && nqp::isnanorinf($n)) || nqp::iseq_i(nqp::unbox_i($n),1),
        $self(nqp::if(nqp::isconcrete(list),list.elems,0)),
        $self(|(nqp::if(nqp::isconcrete(list),list.elems,0) xx $n))
      )
    }
}

# vim: ft=perl6 expandtab sw=4
