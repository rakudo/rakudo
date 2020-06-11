my class Match is Capture is Cool does NQPMatchRole {
    my Mu $EMPTY_LIST := nqp::list();
    my Mu $EMPTY_HASH := nqp::hash();
    my Mu $NO_CAPS    := nqp::hash();

    # When nothing's `made`, we get an NQPMu that we'd like to replace
    # with Nil; all Rakudo objects typecheck as Mu, while NQPMu doesn't
    method ast()  { nqp::if(nqp::istype($!made, Mu),$!made,Nil) }
    method made() { nqp::if(nqp::istype($!made, Mu),$!made,Nil) }

    method Int(--> Int:D) { self.Str.Int }

    method STR() is implementation-detail {
        nqp::if(
          nqp::eqaddr(nqp::getattr(self,Match,'$!match'),NQPdidMATCH),
          self.Str,
          self.MATCH.Str
        )
    }

    method MATCH() is implementation-detail {
        nqp::unless(
          nqp::eqaddr(nqp::getattr(self,Match,'$!match'),NQPdidMATCH),
          nqp::if(                           # must still set up
            nqp::islt_i(
              nqp::getattr_i(self,Match,'$!pos'),
              nqp::getattr_i(self,Match,'$!from')
            ) || nqp::isnull(my $rxsub := nqp::getattr(self,Match,'$!regexsub'))
              || nqp::isnull(my $CAPS := nqp::tryfindmethod($rxsub,'CAPS'))
              || nqp::isnull(my $captures := $CAPS($rxsub))
              || nqp::not_i($captures.has-captures),
            nqp::stmts(                      # no captures
              nqp::bindattr(self,Capture,'@!list',$EMPTY_LIST),
              nqp::bindattr(self,Capture,'%!hash',$EMPTY_HASH),
              nqp::bindattr(self,Match,'$!match',NQPdidMATCH)  # mark as set up
            ),
            self!MATCH-CAPTURES($captures)  # go reify all the captures
          )
        );

        self
    }

    # Version of MATCH that does *not* need to check for being set up already
    # as this will only be called from within MATCH-CAPTURES, which by
    # definition is called only when the Match object is not set up yet.
    method SUBMATCH() is implementation-detail {
        nqp::if(                           # must still set up
          nqp::islt_i(
            nqp::getattr_i(self,Match,'$!pos'),
            nqp::getattr_i(self,Match,'$!from')
          ) || nqp::isnull(my $rxsub := nqp::getattr(self,Match,'$!regexsub'))
            || nqp::isnull(my $CAPS := nqp::tryfindmethod($rxsub,'CAPS'))
            || nqp::isnull(my $captures := $CAPS($rxsub))
            || nqp::not_i($captures.has-captures),
          nqp::stmts(                      # no captures
            nqp::bindattr(self,Capture,'@!list',$EMPTY_LIST),
            nqp::bindattr(self,Capture,'%!hash',$EMPTY_HASH),
            nqp::bindattr(self,Match,'$!match',NQPdidMATCH)  # mark as set up
          ),
          self!MATCH-CAPTURES($captures)  # go reify all the captures
        );

        self
    }

    method !MATCH-CAPTURES(Mu $captures --> Nil) {
        # Initialize capture lists.
        my $list := nqp::findmethod($captures,'prepare-raku-list')($captures);
        my $hash := nqp::findmethod($captures,'prepare-raku-hash')($captures);

        # walk the capture stack and populate the Match.
        if nqp::istrue(my $cs := nqp::getattr(self,Match,'$!cstack')) {

            # only one destination, avoid repeated hash lookups
            if $captures.onlyname -> str $onlyname {

                # numeric: <= ord("9") so positional capture
                my Mu $dest := nqp::atpos(
                  nqp::islt_i(nqp::ord($onlyname),58) ?? $list !! $hash,
                  $onlyname
                );

                # simpLy reify all the cursors
                my int $i = -1;
                nqp::while(
                  nqp::islt_i(($i = nqp::add_i($i,1)),nqp::elems($cs)),
                  nqp::stmts(
                    (my $cursor := nqp::atpos($cs,$i)),
                    nqp::unless(
                      nqp::isnull_s(nqp::getattr_s($cursor,$?CLASS,'$!name')),
                      nqp::push($dest,$cursor.SUBMATCH)  # recurse
                    )
                  )
                );
            }

            # more than one destination
            else {
                my int $i = -1;
                nqp::while(
                  nqp::islt_i(($i = nqp::add_i($i,1)),nqp::elems($cs)),
                  nqp::stmts(                               # handle this cursor
                    (my $cursor := nqp::atpos($cs,$i)),
                    (my str $name = nqp::getattr_s($cursor,$?CLASS,'$!name')),
                    nqp::if(
                      nqp::not_i(nqp::isnull_s($name))
                        && nqp::isge_i(nqp::chars($name),1),
                      nqp::stmts(                           # has a name
                        (my $match := $cursor.SUBMATCH),  # recurse
                        nqp::if(
                          nqp::iseq_s($name,'$!from')
                            || nqp::iseq_s($name,'$!to'),
                          nqp::bindattr_i(self,Match,$name, # it's from|to
                            nqp::getattr_i($match,Match,'$!from')),
                          nqp::stmts(                       # other name(s)
                            (my $names := nqp::split('=',$name)),
                            nqp::while(
                              nqp::elems($names),
                              nqp::if(
                                nqp::iscclass(
                                  nqp::const::CCLASS_NUMERIC,
                                  ($name = nqp::shift($names)),
                                  0
                                ),
                                nqp::if(                    # positional capture
                                  nqp::istype(
                                    (my $p := nqp::atpos($list,$name)),
                                    List
                                  ),
                                  nqp::push(
                                    nqp::getattr($p,List,'$!reified'),
                                    $match
                                  ),
                                  nqp::bindpos($list,$name,$match)  # XXX
                                ),
                                nqp::if(                    # named capture
                                  nqp::istype(
                                    (my $n := nqp::atkey($hash,$name)),
                                    List
                                  ),
                                  nqp::push(
                                    nqp::getattr($n,List,'$!reified'),
                                    $match
                                  ),
                                  nqp::bindkey($hash,$name,$match)  # XXX
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
            }
        }

        # save in object
        nqp::bindattr(self,Capture,'@!list',
          nqp::isconcrete($list) ?? $list !! $EMPTY_LIST);
        nqp::bindattr(self,Capture,'%!hash',$hash);

        # We've produced the captures. If we know we're finished and will
        # never be backtracked into, we can release cstack and regexsub.
        nqp::unless(
          nqp::defined(nqp::getattr(self,Match,'$!bstack')),
          nqp::bindattr(self,Match,'$!cstack',
            nqp::bindattr(self,Match,'$!regexsub',nqp::null)
          )
        );

        # mark as set up
        nqp::bindattr(self,Match,'$!match',NQPdidMATCH);
    }

    # from !cursor_next in nqp
    method CURSOR_NEXT() is raw is implementation-detail {
        nqp::if(
          nqp::defined($!restart),
          $!restart(self),
          nqp::stmts(
            (my $cur := self."!cursor_start_cur"()),
            $cur."!cursor_fail"(),
            $cur
          )
        )
    }

#?if js
    my sub move_cursor($target, $pos) {
       nqp::chars(nqp::substrnfg(nqp::substr($target, $pos), 0, 1)) || 1;
    }
#?endif

    # adapted from !cursor_more in nqp
    method CURSOR_OVERLAP() is raw is implementation-detail {
        my $new := nqp::create(self);
        nqp::bindattr(  $new,$?CLASS,'$!shared',$!shared);
        nqp::bindattr(  $new,$?CLASS,'$!braid',$!braid);
        nqp::bindattr_i($new,$?CLASS,'$!from',
          nqp::bindattr_i($new,$?CLASS,'$!to',-1));
        nqp::bindattr_i($new,$?CLASS,'$!pos',nqp::add_i($!from,1));
        $!regexsub($new)
    }

    # adapted from !cursor_more in nqp
    method CURSOR_MORE() is raw is implementation-detail {
        my $new := nqp::create(self);
        nqp::bindattr(  $new,$?CLASS,'$!shared',$!shared);
        nqp::bindattr(  $new,$?CLASS,'$!braid',$!braid);
        nqp::bindattr_i($new,$?CLASS,'$!from',
          nqp::bindattr_i($new,$?CLASS,'$!to',-1));
        nqp::bindattr_i($new,$?CLASS,'$!pos',nqp::isge_i($!from,$!pos)
#?if !js
          ?? nqp::add_i($!from,1)
#?endif
#?if js
          ?? nqp::add_i($!from, move_cursor(self.target, $!pos))
#?endif
          !! $!pos);
        $!regexsub($new)
    }

    # INTERPOLATE will iterate over the string $tgt beginning at position 0.
    # If it can't match against pattern var (or any element of var if it is an array)
    # it will increment $pos and try again. Therefore it is important to only match
    # against the current position.
    # $i is case insensitive flag
    # $m is ignore accent marks flag
    # $s is for sequential matching instead of junctive
    # $a is true if we are in an assertion

    # INTERPOLATE's parameters are non-optional since the ops for optional params
    # aren't currently JITted on MoarVM
    proto method INTERPOLATE(|) is implementation-detail {*}

    multi method INTERPOLATE(Callable:D \var, $, $, $, $, $) {
        # Call it if it is a routine. This will capture if requested.
        (var)(self)
    }

    multi method INTERPOLATE(Iterable:D \var, int \im, int \monkey, int \s, $, \context) {
        my $maxmatch;
        my \cur    := self.'!cursor_start_cur'();
        my str $tgt = cur.target;
        my int $eos = nqp::chars($tgt);

        my int $maxlen = -1;
        my int $pos    = nqp::getattr_i(cur, $?CLASS, '$!from');
        my int $start  = 1;
        my int $nomod  = im == 0;

        my Mu $order;

        X::Syntax::Reserved.new(
            reserved => "use of hashes in regexes",
        ).throw if nqp::istype(var,Hash);

        # Looks something we need to loop over
        if !nqp::iscont(var) {

            my \varlist  := var.list;
            my int $elems = varlist.elems; # reifies
            my \list     := nqp::getattr(varlist,List,'$!reified');

            # Order matters for sequential matching, so no NFA involved.
            if s {
                $order := list;
            }

            # prepare to run the NFA if var is array-ish.
            else {
                my Mu \nfa  := QRegex::NFA.new;
                my Mu \alts := nqp::setelems(nqp::list,$elems);
                my int $fate = 0;
                my int $j    = -1;

                while nqp::islt_i(++$j,$elems) {
                    my Mu $topic := nqp::atpos(list,$j);
                    nqp::bindpos(alts,$j,$topic);

                    # A Regex already.
                    if nqp::istype($topic,Regex) {
                        nfa.mergesubstates($start,0,nqp::decont($fate),
                          nqp::findmethod($topic,'NFA')($topic),
                          Mu);
                    }

                    # The pattern is a string.
                    else {
                        my Mu \lit  := QAST::Regex.new(
                          :rxtype<literal>, $topic,
                          :subtype( $nomod
                            ?? ''
                            !! im == 2
                              ?? im == 1
                                ?? 'ignorecase+ignoremark'
                                !! 'ignoremark'
                              !! 'ignorecase')
                        );
                        my Mu \nfa2 := QRegex::NFA.new;
                        my Mu \node := nqp::findmethod(nfa2,'addnode')(nfa2,lit);
                        nfa.mergesubstates($start,0,nqp::decont($fate),
                          nqp::findmethod(node,'save')(node,:non_empty(1)),
                          Mu);
                    }
                    ++$fate;
                }

                # Now run the NFA
                my Mu \fates := nqp::findmethod(nfa,'run')(nfa,$tgt,$pos);
                my int $count = nqp::elems(fates);
                nqp::setelems(($order := nqp::list),$count);
                $j = -1;
                nqp::bindpos($order,$j,
                  nqp::atpos(alts,nqp::atpos_i(fates,$j)))
                  while nqp::islt_i(++$j,$count);
            }
        }

        # Use the var as it is if it's not array-ish.
        else {
            $order := nqp::list(var);
        }

        my str $topic_str;
        my int $omax = nqp::elems($order);
        my int $o    = -1;
        while nqp::islt_i(++$o,$omax) {
            my Mu $topic := nqp::atpos($order,$o);
            my $match;
            my int $len;

            # A Regex already.
            if nqp::istype($topic,Regex) {
                $match := self.$topic;
                $len    = $match.pos - $match.from;
            }

            # The pattern is a string. $len and and $topic_str are used
            # later on if this condition does not hold.
            elsif nqp::iseq_i(($len = nqp::chars($topic_str = $topic.Str)),0) {
                $match = 1;
            }

            # no modifier, match literally
            elsif $nomod {
                $match = nqp::eqat($tgt, $topic_str, $pos);
            }

#?if moar
            # ignoremark+ignorecase
            elsif im == 3 {
                $match = nqp::eqaticim($tgt, $topic_str, $pos);
            }

            # ignoremark
            elsif im == 2 {
                $match = nqp::eqatim($tgt, $topic_str, $pos);
            }

            # ignorecase
            elsif im == 1 {
                $match = nqp::eqatic($tgt, $topic_str, $pos);
            }
#?endif
#?if !moar

# This branch is required because neither the JVM nor the JS implementations
# have the nqp::eqat* ops. However, nqp::ordbaseat just throws a NYI
# exception for both, so the code doesn't actually work.

            # ignoremark(+ignorecase?)
            elsif im == 2 || im == 3 {
                my int $k = -1;

                # ignorecase+ignoremark
                if im == 3 {
                    my str $tgt_fc   = nqp::fc(nqp::substr($tgt,$pos,$len));
                    my str $topic_fc = nqp::fc($topic_str);
                    Nil while nqp::islt_i(++$k,$len)
                      && nqp::iseq_i(
                        nqp::ordbaseat($tgt_fc, nqp::add_i($pos,$k)),
                        nqp::ordbaseat($topic_fc, $k)
                      );
                }

                # ignoremark
                else {
                    Nil while nqp::islt_i(++$k, $len)
                      && nqp::iseq_i(
                        nqp::ordbaseat($tgt, nqp::add_i($pos,$k)),
                        nqp::ordbaseat($topic_str, $k)
                      );
                }

                $match = nqp::iseq_i($k,$len); # match if completed
            }

            # ignorecase
            else {
                $match = nqp::iseq_s(
                  nqp::fc(nqp::substr($tgt, $pos, $len)),
                  nqp::fc($topic_str)
                )
            }
#?endif

            if $match
              && nqp::isgt_i($len,$maxlen)
              && nqp::isle_i(nqp::add_i($pos,$len),$eos) {
                $maxlen    = $len;
                $maxmatch := $match;
                last if s; # stop here for sequential alternation
            }
        }

        nqp::istype($maxmatch, Match)
          ?? $maxmatch
          !! nqp::isge_i($maxlen,0)
            ?? cur.'!cursor_pass'(nqp::add_i($pos,$maxlen), '')
            !! cur
    }

    multi method INTERPOLATE(Associative:D \var, int \im, $, $, $, \context) {
        my \cur    := self.'!cursor_start_cur'();
        my $maxmatch;
        my str $tgt = cur.target;

        my int $maxlen = -1;
        my int $pos    = nqp::getattr_i(cur, $?CLASS, '$!from');

        my str $topic_str;
        my $match;
        my int $len;

        # The pattern is a string. $len and and $topic_str are used
        # later on if this condition does not hold.
        if nqp::iseq_i(($len = nqp::chars($topic_str = var.Str)),0) {
            $match = 1;
        }

        # no modifier, match literally
        elsif im == 0 {
            $match = nqp::eqat($tgt, $topic_str, $pos);
        }

#?if moar
        # ignoremark+ignorecase
        elsif im == 3 {
            $match = nqp::eqaticim($tgt, $topic_str, $pos);
        }

        # ignoremark
        elsif im == 2 {
            $match = nqp::eqatim($tgt, $topic_str, $pos);
        }

        # ignorecase
        elsif im == 1 {
            $match = nqp::eqatic($tgt, $topic_str, $pos);
        }
#?endif
#?if !moar

# This branch is required because neither the JVM nor the JS implementations
# have the nqp::eqat* ops. However, nqp::ordbaseat just throws a NYI
# exception for both, so the code doesn't actually work.

        # ignoremark(+ignorecase?)
        elsif im == 2 || im == 3 {
            my int $k = -1;

            # ignorecase+ignoremark
            if im == 3 {
                my str $tgt_fc   = nqp::fc(nqp::substr($tgt,$pos,$len));
                my str $topic_fc = nqp::fc($topic_str);
                Nil while nqp::islt_i(++$k,$len)
                  && nqp::iseq_i(
                    nqp::ordbaseat($tgt_fc, nqp::add_i($pos,$k)),
                    nqp::ordbaseat($topic_fc, $k)
                  );
            }

            # ignoremark
            else {
                Nil while nqp::islt_i(++$k, $len)
                  && nqp::iseq_i(
                    nqp::ordbaseat($tgt, nqp::add_i($pos,$k)),
                    nqp::ordbaseat($topic_str, $k)
                  );
            }

            $match = nqp::iseq_i($k,$len); # match if completed
        }

        # ignorecase
        else {
            $match = nqp::iseq_s(
              nqp::fc(nqp::substr($tgt, $pos, $len)),
              nqp::fc($topic_str)
            )
        }
#?endif

        if $match
          && nqp::isgt_i($len,$maxlen)
          && nqp::isle_i(nqp::add_i($pos,$len),nqp::chars($tgt)) {
            $maxlen    = $len;
            $maxmatch := $match;
        }

        nqp::istype($maxmatch, Match)
          ?? $maxmatch
          !! nqp::isge_i($maxlen,0)
            ?? cur.'!cursor_pass'(nqp::add_i($pos,$maxlen), '')
            !! cur
    }

    multi method INTERPOLATE(Regex:D \var, int \im, int \monkey, $, $, $) {
        my $maxmatch;
        my \cur    := self.'!cursor_start_cur'();

        my int $maxlen = -1;
        my int $pos    = nqp::getattr_i(cur, $?CLASS, '$!from');
        my Mu $topic := var;
        my $match := self.$topic;

        if $match {
            my int $len = $match.pos - $match.from;

            if nqp::isgt_i($len,$maxlen)
               && nqp::isle_i(nqp::add_i($pos,$len),nqp::chars(cur.target)) {
                $maxlen    = $len;
                $maxmatch := $match;
            }
        }

        nqp::istype($maxmatch, Match)
          ?? $maxmatch
          !! nqp::isge_i($maxlen,0)
            ?? cur.'!cursor_pass'(nqp::add_i($pos,$maxlen), '')
            !! cur
    }

    multi method INTERPOLATE(Mu:D \var, int \im, int \monkey, $, $, \context) {
        my \cur     = self.'!cursor_start_cur'();
        my str $tgt = cur.target;

        my int $maxlen = -1;
        my int $pos    = nqp::getattr_i(cur, $?CLASS, '$!from');

        my str $topic_str;
        my $match;
        my int $len;

        # The pattern is a zero length string. $len and and $topic_str
        # are used later on if this condition does not hold.
        if nqp::iseq_i(($len = nqp::chars($topic_str = var.Str)),0) {
            $match = 1;
        }

        # no modifier, match literally
        elsif im == 0 {
            $match = nqp::eqat($tgt, $topic_str, $pos);
        }

#?if !jvm
        # ignoremark+ignorecase
        elsif im == 3 {
            $match = nqp::eqaticim($tgt, $topic_str, $pos);
        }

        # ignoremark
        elsif im == 2 {
            $match = nqp::eqatim($tgt, $topic_str, $pos);
        }

        # ignorecase
        elsif im == 1 {
            $match = nqp::eqatic($tgt, $topic_str, $pos);
        }
#?endif
#?if jvm

# This branch is required because neither the JVM nor the JS implementations
# have the nqp::eqat* ops. However, nqp::ordbaseat just throws a NYI
# exception for both, so the code doesn't actually work.

        # ignoremark(+ignorecase?)
        elsif im == 2 || im == 3 {
            my int $k = -1;

            # ignorecase+ignoremark
            if im == 3 {
                my str $tgt_fc   = nqp::fc(nqp::substr($tgt,$pos,$len));
                my str $topic_fc = nqp::fc($topic_str);
                Nil while nqp::islt_i(++$k,$len)
                  && nqp::iseq_i(
                    nqp::ordbaseat($tgt_fc, nqp::add_i($pos,$k)),
                    nqp::ordbaseat($topic_fc, $k)
                  );
            }

            # ignoremark
            else {
                Nil while nqp::islt_i(++$k, $len)
                  && nqp::iseq_i(
                    nqp::ordbaseat($tgt, nqp::add_i($pos,$k)),
                    nqp::ordbaseat($topic_str, $k)
                  );
            }

            $match = nqp::iseq_i($k,$len); # match if completed
        }

        # ignorecase
        else {
            $match = nqp::iseq_s(
              nqp::fc(nqp::substr($tgt, $pos, $len)),
              nqp::fc($topic_str)
            )
        }
#?endif

        if $match
          && nqp::isgt_i($len,$maxlen)
          && nqp::isle_i(nqp::add_i($pos,$len),nqp::chars($tgt)) {
            $maxlen    = $len;
        }

        nqp::isge_i($maxlen,0)
          ?? cur.'!cursor_pass'(nqp::add_i($pos,$maxlen), '')
          !! cur
    }

    multi method INTERPOLATE(Mu:U \var, $, $, $, $, $) {
        self."!cursor_start_cur"()
    }

    proto method INTERPOLATE_ASSERTION(|) is implementation-detail {*}

    multi method INTERPOLATE_ASSERTION(Associative:D $, $, $, $, $, $) {
        return self.'!cursor_start_cur'().'!cursor_start_cur'()
    }

    multi method INTERPOLATE_ASSERTION(Iterable:D \var, int \im, int \monkey, int \s, $, \context) {
        my $maxmatch;
        my \cur    := self.'!cursor_start_cur'();
        my str $tgt = cur.target;
        my int $eos = nqp::chars($tgt);

        my int $maxlen = -1;
        my int $pos    = nqp::getattr_i(cur, $?CLASS, '$!from');
        my int $start  = 1;
        my int $nomod  = im == 0;

        my Mu $order := nqp::list();

        # Looks something we need to loop over
        if !nqp::iscont(var) {
            my \varlist  := var.list;
            my int $elems = varlist.elems; # reifies
            my \list     := nqp::getattr(varlist,List,'$!reified');

            # Order matters for sequential matching, so no NFA involved.
            if s {
                $order := list;
            }

            # prepare to run the NFA if var is array-ish.
            else {
                my Mu \nfa  := QRegex::NFA.new;
                my Mu \alts := nqp::setelems(nqp::list,$elems);
                my int $fate = 0;
                my int $j    = -1;

                while nqp::islt_i(++$j,$elems) {
                    my Mu $topic := nqp::atpos(list,$j);
                    nqp::bindpos(alts,$j,$topic);

                    # We are in a regex assertion, the strings we get will
                    # be treated as regex rules.
                    return cur.'!cursor_start_cur'() if nqp::istype($topic,Associative);
                    my $rx := MAKE_REGEX($topic,im == 1 || im == 3,im == 2 || im == 3,monkey,context);
                    nfa.mergesubstates($start,0,nqp::decont($fate),nqp::findmethod($rx,'NFA')($rx),Mu);

                    ++$fate;
                }

                # Now run the NFA
                my Mu \fates := nqp::findmethod(nfa,'run')(nfa,$tgt,$pos);
                my int $count = nqp::elems(fates);
                nqp::setelems($order,$count);
                $j = -1;
                nqp::bindpos($order,$j,nqp::atpos(alts,nqp::atpos_i(fates,$j)))
                  while nqp::islt_i(++$j,$count);
            }
        }

        # Use the var as it is if it's not array-ish.
        else {
            nqp::push($order, var);
        }

        my str $topic_str;
        my int $omax = nqp::elems($order);
        my int $o    = -1;
        while nqp::islt_i(++$o,$omax) {
            my Mu $topic := nqp::atpos($order,$o);
            my $match;
            my int $len;

            # We are in a regex assertion, the strings we get will be
            # treated as regex rules.
            return cur.'!cursor_start_cur'()
              if nqp::istype($topic,Associative);

            my $rx := MAKE_REGEX($topic,im == 1 || im == 3,im == 2 || im == 3,monkey,context);
            $match := self.$rx;
            $len    = $match.pos - $match.from;

            if $match
              && nqp::isgt_i($len,$maxlen)
              && nqp::isle_i(nqp::add_i($pos,$len),$eos) {
                $maxlen    = $len;
                $maxmatch := $match;
                last if s; # stop here for sequential alternation
            }
        }

        nqp::istype($maxmatch, Match)
          ?? $maxmatch
          !! nqp::isge_i($maxlen,0)
            ?? cur.'!cursor_pass'(nqp::add_i($pos,$maxlen), '')
            !! cur
    }

    multi method INTERPOLATE_ASSERTION(Mu:D \var, int \im, int \monkey, $, $, \context) {
        # We are in a regex assertion, the strings we get will be
        # treated as regex rules.
        my $rx     := MAKE_REGEX(var,im == 1 || im == 3,im == 2 || im == 3,monkey,context);
        my Match \match  := self.$rx;
        my int $len = match.pos - match.from;

        match.Bool
          && nqp::isgt_i($len,-1)
          && nqp::isle_i(nqp::add_i(nqp::getattr_i(self, $?CLASS, '$!pos'),$len),nqp::chars(self.target))
          ?? match
          !! self.'!cursor_start_fail'()
    }

    method CALL_SUBRULE($rule, |c) is implementation-detail {
        $rule(self, |c)
    }

    method DYNQUANT_LIMITS($mm) is implementation-detail {
        # Treat non-Range values as range with that value on both end points
        # Throw for non-Numeric or NaN Ranges, or if minimum limit is +Inf
        # Convert endpoints that are less than 0 to 0, then,
        # throw if Range is empty.
        nqp::if(
          nqp::istype($mm,Range),
          nqp::if(
               nqp::isfalse(nqp::istype((my $min := $mm.min),Numeric))
            || nqp::isfalse(nqp::istype((my $max := $mm.max),Numeric))
            || $min.isNaN || $max.isNaN,
            X::Syntax::Regex::QuantifierValue.new(:non-numeric-range).throw,
            nqp::if(
              $min == Inf,
              X::Syntax::Regex::QuantifierValue.new(:inf).throw,
              nqp::stmts(
                nqp::if(
                  nqp::islt_i(
                    ($min := nqp::add_i($min == -Inf ?? -1 !! $min.Int,
                      $mm.excludes-min)),
                    0),
                  $min := 0),
                nqp::if(
                  $max == Inf,
                  nqp::list_i($min,-1),
                  nqp::stmts(
                    nqp::if(
                      $max == -Inf || nqp::islt_i(
                        ($max := nqp::sub_i($max.Int,$mm.excludes-max)),0),
                      $max := 0),
                    nqp::if(
                      nqp::islt_i($max, $min),
                      X::Syntax::Regex::QuantifierValue.new(:empty-range).throw,
                      nqp::list_i($min,$max))))))),
          nqp::if(
            nqp::istype((my $v := $mm.Int), Failure),
            nqp::stmts(
              ($v.so), # handle Failure
              nqp::if(
                nqp::istype($mm,Numeric) && nqp::isfalse($mm.isNaN),
                nqp::if(
                  $mm == Inf,
                  X::Syntax::Regex::QuantifierValue.new(:inf).throw,
                  nqp::list_i(0,0)), # if we got here, $mm is -Inf, treat as zero
                X::Syntax::Regex::QuantifierValue.new(:non-numeric).throw)),
            nqp::if(
              nqp::islt_i($v,0),
              nqp::list_i(0,0),
              nqp::list_i($v,$v))))
    }

    method OTHERGRAMMAR($grammar, $name, |) is implementation-detail {
        my $lang_cursor := $grammar.'!cursor_init'(self.target(), :p(self.pos()));
        $lang_cursor.clone_braid_from(self);
        $lang_cursor."$name"();
    }

    method INDMETHOD($name, |c) is implementation-detail {
        self."$name"(|c);
    }

    method INDRULE($rule, |c) is implementation-detail {
        $rule(self, |c)
    }

    method RECURSE() is implementation-detail {
        nqp::getlexdyn('$?REGEX')(self)
    }

    my role CachedCompiledRegex {
        has $.regex;
    }
    multi sub MAKE_REGEX(Regex \arg, $, $, int \monkey, $) {
        arg
    }
    multi sub MAKE_REGEX(CachedCompiledRegex \arg, $, $, int \monkey, $) {
        arg.regex
    }
    multi sub MAKE_REGEX(\arg, \i, \m, int \monkey, \context) {
        my $*RESTRICTED = "Prohibited regex interpolation"
         unless monkey;  # Comes from when regex was originally compiled.

        my \rx = EVAL('anon regex { ' ~ nqp::if(i,
          nqp::if(m,
            ':i :m ',
            ':i '),
          nqp::if(m,
            ':m ',
            ' ')) ~ arg ~ '}', :context(context));
        arg does CachedCompiledRegex(rx);
        rx
    }

    submethod BUILD(
        :$orig = '',
        :$from = 0,
        :to(:$pos),
        :ast(:$made),
        :$shared,
        :$braid,
        :$list,
        :$hash)
    {
        # :build tells !cursor_init that it's too late to do a CREATE
        self.'!cursor_init'($orig, :build, :p($pos), :$shared, :$braid);
        nqp::bindattr_i(self, Match,   '$!from', $from);
        nqp::bindattr(  self, Match,   '$!made', nqp::decont($made)) if $made.defined;
    }

    method clone() is raw { nqp::clone(self) }

    multi method WHICH(Match:D: --> ObjAt:D) {
        self.Mu::WHICH # skip Capture's as Match is not a value type
    }

    proto method Bool(|) {*}
    multi method Bool(Match:U: --> False) { }
    multi method Bool(Match:D:) { nqp::hllbool($!pos >= $!from) }

    multi method Numeric(Match:D:) {
        self.Str.Numeric
    }
    multi method ACCEPTS(Match:D: Any $) { self }

    method prematch(Match:D:) {
        nqp::substr(self.target,0,$!from)
    }
    method postmatch(Match:D:) {
        nqp::substr(self.target,self.to)
    }

    method caps(Match:D:) {
        my @caps;
        for self.pairs -> $p {
            if nqp::istype($p.value,List) {
                @caps.push: $p.key => $_ for $p.value.list
            } elsif $p.value.DEFINITE {
                @caps.push: $p
            }
        }
        @caps.sort: -> $a { $a.value.from +< 32 + $a.value.pos }
    }

    method chunks(Match:D:) {
        my $prev = $!from;
        my $target := self.target;
        gather {
            for self.caps {
                if .value.from > $prev {
                    take '~' => substr($target,$prev, .value.from - $prev)
                }
                take $_;
                $prev = .value.pos;
            }
            take '~' => substr($target,$prev, $!pos - $prev) if $prev < $!pos;
        }
    }

    multi method raku(Match:D: --> Str:D) {
        my $attrs := nqp::list_s;

        nqp::push_s($attrs,(orig => self.orig // '').raku);
        nqp::push_s($attrs,(from => self.from // 0).raku);
        nqp::push_s($attrs,(pos  => self.pos // 0).raku);
        if self.Capture::list -> @list { nqp::push_s($attrs,:@list.raku) }
        if self.Capture::hash -> %hash { nqp::push_s($attrs,:%hash.raku) }
        nqp::push_s($attrs,(made => $_).raku) with self.made;

        nqp::concat('Match.new(',nqp::concat(nqp::join(', ',$attrs),')'))
    }
    multi method gist (Match:D: $d = 0) {
        return "#<failed match>" unless self;
        my $s = ' ' x ($d + 1);
        my $r = ("=> " if $d) ~ "\x[FF62]{self}\x[FF63]\n";
        for @.caps {
            $r ~= $s ~ (.key // '?') ~ ' ' ~ .value.gist($d + 1)
        }
        $d == 0 ?? $r.chomp !! $r;
    }

    method replace-with(Match:D: Str() $replacement --> Str:D) {
        self.prematch ~ $replacement ~ self.postmatch
    }
}

multi sub infix:<eqv>(Match:D \a, Match:D \b) {
    a =:= b
    ||
    [&&] (
        a.pos  eqv b.pos,
        a.from eqv b.from,
        a.orig eqv b.orig,
        (a.made // Any) eqv (b.made // Any),
        (a.Capture::list // nqp::list ) eqv (b.Capture::list // nqp::list ),
        (a.Capture::hash // nqp::hash ) eqv (b.Capture::hash // nqp::hash )
    );
}


sub make(Mu \made) {
    nqp::bindattr(nqp::decont(nqp::getlexcaller('$/')),Match,'$!made',made)
}


# vim: expandtab sw=4
