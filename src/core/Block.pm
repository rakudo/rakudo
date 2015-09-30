my class Block { # declared in BOOTSTRAP
    # class Block is Code {
    #     has Mu $!phasers;
    #     has Mu $!why;

    method of(Block:D:)      { nqp::getattr(self,Code,'$!signature').returns }
    method returns(Block:D:) { nqp::getattr(self,Code,'$!signature').returns }

    method add_phaser(Str $name, &block) {
        nqp::isnull($!phasers) &&
            nqp::bindattr(self, Block, '$!phasers', nqp::hash());
        nqp::existskey($!phasers, nqp::unbox_s($name)) ||
            nqp::bindkey($!phasers, nqp::unbox_s($name), nqp::list());
        if $name eq 'LEAVE' || $name eq 'KEEP' || $name eq 'UNDO' {
            nqp::unshift(nqp::atkey($!phasers, nqp::unbox_s($name)), &block);
            self.add_phaser('!LEAVE-ORDER', &block);
        }
        elsif $name eq 'NEXT' || $name eq '!LEAVE-ORDER' || $name eq 'POST' {
            nqp::unshift(nqp::atkey($!phasers, nqp::unbox_s($name)), &block);
        }
        else {
            nqp::push(nqp::atkey($!phasers, nqp::unbox_s($name)), &block);
        }
    }

    method fire_phasers(str $name) {
        if !nqp::isnull($!phasers) && nqp::existskey($!phasers, $name) {
            my Mu $iter := nqp::iterator(nqp::atkey($!phasers, $name));
            nqp::shift($iter).() while $iter;
        }
    }

    method has-phasers() { !nqp::isnull($!phasers) }

    method phasers(Str $name) {
        unless nqp::isnull($!phasers) {
            if nqp::existskey($!phasers, nqp::unbox_s($name)) {
                return nqp::p6bindattrinvres(nqp::create(List), List, '$!reified',
                    nqp::atkey($!phasers, nqp::unbox_s($name)));
            }
        }
        ()
    }

    method assuming(Block:D $self: |primers) {
        my $sig = nqp::getattr($self,Code,'$!signature');

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
                my $i = 0; # broken FIRST workaround
                while ($type ~~ / (.*?) \[ (.*) \] $$/) {
                    my $slash0 = ~$0;
                    my $slash1 = ~$1;
#                   FIRST {  # seems broken
                    unless ($i++) { # broken FIRST workaaround
                        $perl = ~$/;
                        if $elide_agg_cont {
                           $perl = ~$slash1;
                        }
                    }
                    $type = ~$slash1;
                    unless soft_indirect_name_lookup($slash0) {
                        $perl = "";
                        last
                    };
                }
                unless soft_indirect_name_lookup($type) {
                    $perl = "";
                };
            }
#Introspection fail.  There is no introspection to access these flags.
#Skipped for now.
#            if $!flags +& $SIG_ELEM_DEFINED_ONLY {
#                $perl ~= ':D' if $perl ne '';
#            } elsif $!flags +& $SIG_ELEM_UNDEFINED_ONLY {
#                $perl ~= ':U' if $perl ne '';
#            }
            my $name = $parm.name;
            if not $name.defined or !$name.starts-with($sigil) {
                $name = $sigil ~ $parm.twigil ~ ($name // "");
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
            if  $parm.raw {
                if     not $.name {
                    if $name eq '$' and not $rest {
                        $name = '\\';
                    }
                    elsif $name.starts-with('\\') and ($rest or $name ne '\\') {
                        $name = '$' ~ $name.substr(1);
                    }
                }
                if !$name.starts-with('\\') {
                    $rest ~= ' is raw';
                }
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
        # do not aready have them, if we see a capture.
        my $need_cap = $sig.count == Inf and not ($slurp_p and $slurp_n);
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
        if ($slurp_n and $slurp_n.capture and $slurp_n !=== $slurp_p) {
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
        my $perl = '-> ';
        # lose colon prefix and parens
        $perl ~= substr(self.signature().perl,2,*-1);
        $perl ~= ' { #`(' ~ self.WHICH ~ ') ... }';
        $perl
    }

    method WHY() {
        if nqp::isnull($!why) {
            Any
        } else {
            $!why.set_docee(self);
            $!why
        }
    }

    method set_why($why) {
        $!why := $why;
    }
}

# vim: ft=perl6 expandtab sw=4
