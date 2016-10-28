my class Cursor does NQPCursorRole {
    has $!made; # Need it to survive re-creations of the match object.
    my Mu $EMPTY_LIST := nqp::list();
    my Mu $NO_CAPS    := nqp::hash();

    multi method Bool(Cursor:D:) {
        nqp::p6bool(
          nqp::isge_i(
            nqp::getattr_i(self, Cursor, '$!pos'),
            nqp::getattr_i(self, Cursor, '$!from')
          )
        )
    }

    method STR() {
        nqp::if(
          nqp::istype((my $match := nqp::getattr(self,Cursor,'$!match')),Match)
            && nqp::isconcrete($match),
          $match.Str,
          self!MATCH.Str
        )
    }

    method MATCH() {
        nqp::if(
          nqp::istype((my $match := nqp::getattr(self,Cursor,'$!match')),Match)
            && nqp::isconcrete($match),
          $match,
          self!MATCH
        )
    }

    method !MATCH() {
        my $match := nqp::create(Match);
        nqp::bindattr($match, Match, '$!orig', nqp::findmethod(self, 'orig')(self));
        my int $from = nqp::getattr_i(self, Cursor, '$!from');
        my int $to   = nqp::getattr_i(self, Cursor, '$!pos');
        nqp::bindattr_i($match, Match, '$!from', $from);
        nqp::bindattr_i($match, Match, '$!to', $to);
        nqp::bindattr($match, Match, '$!made', nqp::getattr(self, Cursor, '$!made'));
        nqp::bindattr($match, Match, '$!CURSOR', self);
        my Mu $list;
        my Mu $hash := nqp::hash();
        if $to >= $from {
            # For captures with lists, initialize the lists.
            my $caplist := $NO_CAPS;
            my $rxsub   := nqp::getattr(self, Cursor, '$!regexsub');
            my Mu $onlyname := '';
            my int $namecount = 0;

            if !nqp::isnull($rxsub) && nqp::defined($rxsub) {
                $caplist := nqp::can($rxsub, 'CAPS') ?? nqp::findmethod($rxsub, 'CAPS')($rxsub) !! nqp::null();
                if !nqp::isnull($caplist) && nqp::istrue($caplist) {
                    my $iter := nqp::iterator($caplist);
                    while $iter {
                        my $curcap := nqp::shift($iter);
                        $namecount = $namecount + 1;
#?if jvm
                        my Mu $curval := nqp::iterval($curcap);
                        if (nqp::isint($curval) && nqp::isge_i($curval, 2))
                        || (nqp::isnum($curval) && nqp::p6box_n($curval) >= 2) {
#?endif
#?if !jvm
                        if nqp::iterval($curcap) >= 2 {
#?endif
                            my str $name = nqp::iterkey_s($curcap);
                            $onlyname := $name if $namecount == 1;
                            nqp::iscclass(nqp::const::CCLASS_NUMERIC, $name, 0)
                                ?? nqp::bindpos(
                                        nqp::if(nqp::isconcrete($list), $list, ($list := nqp::list())),
                                        nqp::fromstr_I($name, Int), [])
                                !! nqp::bindkey($hash, $name, []);
                        }
                    }
                }
            }

            # Walk the Cursor stack and populate the Cursor.
            my Mu $cs := nqp::getattr(self, Cursor, '$!cstack');
            if nqp::isnull($cs) || !nqp::istrue($cs) {}
#?if !jvm
            elsif !$caplist {}
#?endif
            elsif $namecount == 1 && $onlyname ne '' && nqp::eqat($onlyname,'$!',0) {
                # If there's only one destination, avoid repeated hash lookups
                my int $cselems = nqp::elems($cs);
                my int $csi = -1;
                my Mu $dest;

                # numeric: <= ord("9") so positional capture
                $dest := nqp::islt_i(nqp::ord($onlyname),58)
                  ?? nqp::atpos($list, $onlyname)
                  !! nqp::atkey($hash, $onlyname);

                while nqp::islt_i(++$csi,$cselems) {
                    my $subcur := nqp::atpos($cs, $csi);
                    my $name   := nqp::getattr($subcur, $?CLASS, '$!name');
                    nqp::push($dest,$subcur.MATCH())
                      if !nqp::isnull($name) && nqp::defined($name);
                }
            }
            else {
                my int $cselems = nqp::elems($cs);
                my int $csi     = -1;
                while nqp::islt_i(++$csi,$cselems) {
                    my Mu $subcur   := nqp::atpos($cs, $csi);
                    my Mu $name     := nqp::getattr($subcur, $?CLASS, '$!name');
                    if !nqp::isnull($name) && nqp::defined($name) && $name ne '' {
                        my Mu $submatch := $subcur.MATCH;
                        if nqp::eqat($name, '$', 0) && ($name eq '$!from' || $name eq '$!to') {
                            nqp::bindattr_i($match, Match, $name, $submatch.from);
                        }
                        elsif nqp::index($name, '=') < 0 {
                            my Mu $capval     := nqp::atkey($caplist, $name);
#?if jvm
                            my int $needs_list = nqp::isconcrete($capval) &&
                                ((nqp::isint($capval) && nqp::isge_i($capval, 2)) ||
                                (nqp::isnum($capval) && nqp::p6box_n($capval) >= 2));
#?endif
#?if !jvm
                            my int $needs_list = nqp::isconcrete($capval) && $capval >= 2;
#?endif
                            if nqp::iscclass(nqp::const::CCLASS_NUMERIC, $name, 0) {
                                $list := nqp::list() unless nqp::isconcrete($list);
                                $needs_list
                                    ?? nqp::atpos($list, nqp::fromstr_I(nqp::unbox_s($name), Int)).append($submatch)
                                    !! nqp::bindpos($list, nqp::fromstr_I(nqp::unbox_s($name), Int), $submatch);
                            }
                            else {
                                $needs_list
                                    ?? nqp::atkey($hash, $name).append($submatch)
                                    !! nqp::bindkey($hash, $name, $submatch);
                            }
                        }
                        else {
                            my Mu $names := nqp::split('=', $name);
                            my $iter     := nqp::iterator($names);
                            while $iter {
                                my str $name   = nqp::unbox_s(nqp::shift($iter));
                                my Mu $capval := nqp::atkey($caplist, $name);
#?if jvm
                                my int $needs_list = nqp::isconcrete($capval) &&
                                    ((nqp::isint($capval) && nqp::isge_i($capval, 2)) ||
                                    (nqp::isnum($capval) && nqp::p6box_n($capval) >= 2));
#?endif
#?if !jvm
                                my int $needs_list = nqp::isconcrete($capval) && $capval >= 2;
#?endif
                                if nqp::iscclass(nqp::const::CCLASS_NUMERIC, $name, 0) {
                                    $list := nqp::list() unless nqp::isconcrete($list);
                                    $needs_list
                                        ?? nqp::atpos($list, nqp::fromstr_I($name, Int)).append($submatch)
                                        !! nqp::bindpos($list, nqp::fromstr_I($name, Int), $submatch);
                                }
                                else {
                                    $needs_list
                                        ?? nqp::atkey($hash, $name).append($submatch)
                                        !! nqp::bindkey($hash, $name, $submatch);
                                }
                            }
                        }
                    }
                }
            }
        }
        nqp::bindattr($match, Capture, '$!list', nqp::isconcrete($list) ?? $list !! $EMPTY_LIST);
        nqp::bindattr($match, Capture, '$!hash', $hash);
        nqp::bindattr(self, Cursor, '$!match', $match);

        # Once we've produced the captures, and if we know we're finished and
        # will never be backtracked into, we can release cstack and regexsub.
        unless nqp::defined(nqp::getattr(self, Cursor, '$!bstack')) {
            nqp::bindattr(self, Cursor, '$!cstack', nqp::null());
            nqp::bindattr(self, Cursor, '$!regexsub', nqp::null());
        }

        $match;
    }

    method CURSOR_NEXT() {   # from !cursor_next in nqp
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

    method CURSOR_OVERLAP() {  # adapted from !cursor_more in nqp
        nqp::stmts(
          (my $new := nqp::create(self)),
          nqp::bindattr(  $new,$?CLASS,'$!shared',$!shared),
          nqp::bindattr_i($new,$?CLASS,'$!from',-1),
          nqp::bindattr_i($new,$?CLASS,'$!pos',nqp::add_i($!from,1)),
          $!regexsub($new)
        )
    }

    method CURSOR_MORE() {  # adapted from !cursor_more in nqp
        nqp::stmts(
          (my $new := nqp::create(self)),
          nqp::bindattr(  $new,$?CLASS,'$!shared',$!shared),
          nqp::bindattr_i($new,$?CLASS,'$!from',-1),
          nqp::bindattr_i($new,$?CLASS,'$!pos',
            nqp::if(
              nqp::isge_i($!from,$!pos),
              nqp::add_i($!from,1),
              $!pos
            )
          ),
          $!regexsub($new)
        )
    }

    # INTERPOLATE will iterate over the string $tgt beginning at position 0.
    # If it can't match against pattern var (or any element of var if it is an array)
    # it will increment $pos and try again. Therefor it is important to only match
    # against the current position.
    # $i is case insensitive flag
    # $s is for sequential matching instead of junctive
    # $a is true if we are in an assertion
    method INTERPOLATE(\var, int $i, int $m, int $monkey, int $s, int $a = 0, $context = PseudoStash) {
        if nqp::isconcrete(var) {
            # Call it if it is a routine. This will capture if requested.
            return (var)(self) if nqp::istype(var,Callable);

            my $maxmatch;
            my $cur    := self.'!cursor_start_cur'();
            my str $tgt = $cur.target;
            my int $eos = nqp::chars($tgt);

            my int $maxlen = -1;
            my int $pos    = nqp::getattr_i($cur, $?CLASS, '$!from');
            my int $start  = 1;
            my int $nomod  = !($i || $m);

            my Mu $order := nqp::list();

            # Looks something we need to loop over
            if nqp::istype(var, Iterable) and !nqp::iscont(var) {
                my $varlist  := var.list;
                my int $elems = $varlist.elems; # reifies
                my $list     := nqp::getattr($varlist,List,'$!reified');

                # Order matters for sequential matching, so no NFA involved.
                if $s {
                    $order := $list;
                }

                # prepare to run the NFA if var is array-ish.
                else {
                    my Mu $nfa  := QRegex::NFA.new;
                    my Mu $alts := nqp::setelems(nqp::list,$elems);
                    my int $fate = 0;
                    my int $j    = -1;

                    while nqp::islt_i(++$j,$elems) {
                        my Mu $topic := nqp::atpos($list,$j);
                        nqp::bindpos($alts,$j,$topic);

                        # We are in a regex assertion, the strings we get will
                        # be treated as regex rules.
                        if $a {
                            return $cur.'!cursor_start_cur'()
                              if nqp::istype($topic,Associative);

                            my $rx := MAKE_REGEX($topic,$i,$m,$monkey,$context);
                            $nfa.mergesubstates($start,0,nqp::decont($fate),
                              nqp::findmethod($rx,'NFA')($rx),
                              Mu);
                        }

                        # A Regex already.
                        elsif nqp::istype($topic,Regex) {
                            $nfa.mergesubstates($start,0,nqp::decont($fate),
                              nqp::findmethod($topic,'NFA')($topic),
                              Mu);
                        }

                        # The pattern is a string.
                        else {
                            my Mu $lit  := QAST::Regex.new(
                              :rxtype<literal>, $topic,
                              :subtype( $nomod
                                ?? ''
                                !! $m
                                  ?? $i
                                    ?? 'ignorecase+ignoremark'
                                    !! 'ignoremark'
                                  !! 'ignorecase')
                            );
                            my Mu $nfa2 := QRegex::NFA.new;
                            my Mu $node := nqp::findmethod($nfa2,'addnode')($nfa2,$lit);
                            $nfa.mergesubstates($start,0,nqp::decont($fate),
                              nqp::findmethod($node,'save')($node,:non_empty(1)),
                              Mu);
                        }
                        ++$fate;
                    }

                    # Now run the NFA
                    my Mu $fates := nqp::findmethod($nfa,'run')($nfa,$tgt,$pos);
                    my int $count = nqp::elems($fates);
                    nqp::setelems($order,$count);
                    $j = -1;
                    nqp::bindpos($order,$j,
                      nqp::atpos($alts,nqp::atpos_i($fates,$j)))
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
                if $a {
                    return $cur.'!cursor_start_cur'()
                      if nqp::istype($topic,Associative);

                    my $rx := MAKE_REGEX($topic,$i,$m,$monkey,$context);
                    $match := self.$rx;
                    $len    = $match.pos - $match.from;
                }

                # A Regex already.
                elsif nqp::istype($topic,Regex) {
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
                    $match = nqp::eqat($tgt, $topic_str, $pos)
                }

                # ignoremark(+ignorecase?)
                elsif $m {
                    my int $k = -1;

                    # ignorecase+ignoremark
                    if $i {
                        my str $tgt_lc   = nqp::lc(nqp::substr($tgt,$pos,$len));
                        my str $topic_lc = nqp::lc($topic_str);
                        Nil while nqp::islt_i(++$k,$len)
                          && nqp::iseq_i(
                            nqp::ordbaseat($tgt_lc, nqp::add_i($pos,$k)),
                            nqp::ordbaseat($topic_lc, $k)
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
                      nqp::lc(nqp::substr($tgt, $pos, $len)),
                      nqp::lc($topic_str)
                    )
                }

                if $match
                  && nqp::isgt_i($len,$maxlen)
                  && nqp::isle_i(nqp::add_i($pos,$len),$eos) {
                    $maxlen    = $len;
                    $maxmatch := $match;
                    last if $s; # stop here for sequential alternation
                }
            }

            nqp::istype($maxmatch,Cursor)
              ?? $maxmatch
              !! nqp::isge_i($maxlen,0)
                ?? $cur.'!cursor_pass'(nqp::add_i($pos,$maxlen), '')
                !! $cur
        }
        else {
            self."!cursor_start_cur"()
        }
    }

    method CALL_SUBRULE($rule, |c) {
        $rule(self, |c)
    }

    method DYNQUANT_LIMITS($mm) {
        nqp::istype($mm,Range)
          ?? $mm.min == Inf
            ?? die 'Range minimum in quantifier (**) cannot be +Inf'
            !! $mm.max == -Inf
              ?? die 'Range maximum in quantifier (**) cannot be -Inf'
              !! nqp::list_i(
                   $mm.min  <   0 ??  0 !! $mm.min.Int,
                   $mm.max == Inf ?? -1 !! $mm.max.Int)
          !! $mm == -Inf || $mm == Inf
            ?? Failure.new('Fixed quantifier cannot be infinite')
            !! nqp::list_i($mm.Int, $mm.Int)
    }

    method OTHERGRAMMAR($grammar, $name, |) {
        my $lang_cursor := $grammar.'!cursor_init'(self.target(), :p(self.pos()));
        $lang_cursor."$name"();
    }

    method INDMETHOD($name, |c) {
        self."$name"(|c);
    }

    method INDRULE($rule, |c) {
        $rule(self, |c)
    }

    method RECURSE() {
        nqp::getlexdyn('$?REGEX')(self)
    }

    sub MAKE_REGEX($arg, int $i, int $m, int $monkey, $context) {
        my role CachedCompiledRegex {
            has $.regex;
        }
        if nqp::istype($arg,Regex) {
            $arg
        }
        elsif nqp::istype($arg, CachedCompiledRegex) {
            $arg.regex
        }
        else {
            my $*RESTRICTED = "Prohibited regex interpolation"
             unless $monkey;  # Comes from when regex was originally compiled.

            my $rx := $i
              ?? $m
                ?? EVAL("anon regex \{ :i :m $arg\}", :$context)
                !! EVAL("anon regex \{ :i $arg\}",    :$context)
              !! $m
                ?? EVAL("anon regex \{ :m $arg\}",    :$context)
                !! EVAL("anon regex \{ $arg\}",       :$context);
            $arg does CachedCompiledRegex($rx);
            $rx
        }
    }
}

# vim: ft=perl6 expandtab sw=4
