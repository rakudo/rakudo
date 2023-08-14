augment class Match {

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
}

# vim: expandtab shiftwidth=4
