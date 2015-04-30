my class Cursor does NQPCursorRole {
    has $!made; # Need it to survive re-creations of the match object.
    my Mu $EMPTY_LIST := nqp::list();
    my Mu $NO_CAPS    := nqp::hash();

    multi method Bool(Cursor:D:) {
        nqp::getattr_i(self, Cursor, '$!pos') >= nqp::getattr_i(self, Cursor, '$!from')
    }

    method MATCH() {
        my $match := nqp::getattr(self, Cursor, '$!match');
        return $match if nqp::istype($match, Match) && nqp::isconcrete($match);
        $match := nqp::create(Match);
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
            if !nqp::isnull($rxsub) && nqp::defined($rxsub) {
                $caplist := nqp::can($rxsub, 'CAPS') ?? nqp::findmethod($rxsub, 'CAPS')($rxsub) !! nqp::null();
                if !nqp::isnull($caplist) && nqp::istrue($caplist) {
                    my $iter := nqp::iterator($caplist);
                    while $iter {
                        my $curcap := nqp::shift($iter);
#?if jvm
                        my Mu $curval := nqp::iterval($curcap);
                        if (nqp::isint($curval) && nqp::isge_i($curval, 2))
                        || (nqp::isnum($curval) && nqp::p6box_n($curval) >= 2) {
#?endif
#?if !jvm
                        if nqp::iterval($curcap) >= 2 {
#?endif
                            my str $name = nqp::iterkey_s($curcap);
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
#?if jvm
            if !nqp::isnull($cs) && nqp::istrue($cs) {
#?endif
#?if !jvm
            if $caplist && !nqp::isnull($cs) && nqp::istrue($cs) {
#?endif
                my int $cselems = nqp::elems($cs);
                my int $csi     = 0;
                while $csi < $cselems {
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
                                    ?? nqp::atpos($list, nqp::fromstr_I(nqp::unbox_s($name), Int)).push($submatch)
                                    !! nqp::bindpos($list, nqp::fromstr_I(nqp::unbox_s($name), Int), $submatch);
                            }
                            else {
                                $needs_list
                                    ?? nqp::atkey($hash, $name).push($submatch)
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
                                        ?? nqp::atpos($list, nqp::fromstr_I($name, Int)).push($submatch)
                                        !! nqp::bindpos($list, nqp::fromstr_I($name, Int), $submatch);
                                }
                                else {
                                    $needs_list
                                        ?? nqp::atkey($hash, $name).push($submatch)
                                        !! nqp::bindkey($hash, $name, $submatch);
                                }
                            }
                        }
                    }
                    $csi = nqp::add_i($csi, 1);
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

    method MATCH_SAVE() {
        nqp::getattr_i(self, Cursor, '$!pos') < 0 ?? Nil !! self.MATCH()
    }

    # INTERPOLATE will iterate over the string $tgt beginning at position 0.
    # If it can't match against pattern var (or any element of var if it is an array)
    # it will increment $pos and try again. Therefor it is important to only match
    # against the current position.
    # $i is case insensitive flag
    # $s is for sequential matching instead of junctive
    # $a is true if we are in an assertion
    method INTERPOLATE(\var, $i = 0, $s = 0, $a = 0) {
        if nqp::isconcrete(var) {
            # Call it if it is a routine. This will capture if requested.
            return (var)(self) if nqp::istype(var,Callable);
            my $maxlen := -1;
            my $maxmatch;
            my $cur := self.'!cursor_start_cur'();
            my $pos := nqp::getattr_i($cur, $?CLASS, '$!from');
            my $tgt := $cur.target;
            my $eos := nqp::chars($tgt);
            my $fate   := 0;
            my $count  := 0;
            my $start  := 1;
            my Mu $alts := nqp::list();
            my Mu $order := nqp::list();

            if nqp::istype(var, Positional) and !nqp::iscont(var) {
                if $s {
                    # The order matters for sequential matching, therefor no NFA involved.
                    nqp::push($order,$_) for var.list;
                }
                else {
                    my Mu $nfa := QRegex::NFA.new;
                    # prepare to run the NFA if var is array-ish.
                    for var.list -> $topic {
                        nqp::push($alts, $topic);
                        if $a {
                            # We are in a regex assertion, the strings we get will be treated as
                            # regex rules.
                            return $cur.'!cursor_start_cur'()
                              if nqp::istype($topic,Associative);
                            my $rx := MAKE_REGEX($topic, :$i);
                            my Mu $nfas := nqp::findmethod($rx, 'NFA')($rx);
                            $nfa.mergesubstates($start, 0, $fate, $nfas, Mu);
                        }
                        elsif nqp::istype($topic,Regex) {
                            # A Regex already.
                            my Mu $nfas := nqp::findmethod($topic, 'NFA')($topic);
                            $nfa.mergesubstates($start, 0, $fate, $nfas, Mu);
                        }
                        else {
                            # The pattern is a string.
                            my Mu $lit  := QAST::Regex.new( :rxtype<literal>, $topic,
                                                            :subtype( $i ?? 'ignorecase' !! '') );
                            my Mu $nfa2 := QRegex::NFA.new;
                            my Mu $node := nqp::findmethod($nfa2, 'addnode')($nfa2, $lit);
                            my Mu $save := nqp::findmethod($node, 'save')($node, :non_empty(1));
                            $nfa.mergesubstates($start, 0, $fate, $save, Mu);
                        }
                        $fate := $fate + 1;
                    }

                    # Now run the NFA
                    my Mu $fates := nqp::findmethod($nfa, 'run')($nfa, $tgt, $pos);
                    $fate        := 0;
                    $count       := nqp::elems($fates);
                    while nqp::islt_i($fate, $count) {
                        my $thing := nqp::atpos_i($fates, $fate);
                        nqp::push($order, nqp::atpos($alts, $thing));
                        $fate := nqp::add_i($fate, 1);
                    }
                }
            }
            else {
                # Use the var as it is if it's not array-ish.
                nqp::push($order, var);
            }

            my int $omax = nqp::elems($order);
            loop (my int $o = 0; $o < $omax; $o = $o + 1) {
                my Mu $topic := nqp::atpos($order,$o);
                my $match;
                my $len;

                if $a {
                    # We are in a regex assertion, the strings we get will be treated as
                    # regex rules.
                    return $cur.'!cursor_start_cur'()
                      if nqp::istype($topic,Associative);
                    my $rx := MAKE_REGEX($topic, :$i);
                    $match := self.$rx;
                    $len   := $match.pos - $match.from;
                }
                elsif nqp::istype($topic,Regex) {
                    # A Regex already.
                    $match := self.$topic;
                    $len   := $match.pos - $match.from;
                }
                else {
                    # The pattern is a string.
                    my str $topic_str = $topic.Str;
                    $len   := nqp::chars( $topic_str );
                    $match := $len < 1
                            ||  ($i ?? nqp::lc(nqp::substr($tgt, $pos, $len)) eq nqp::lc($topic_str)
                                    !! nqp::eqat($tgt, $topic_str, $pos));
                }

                if $match && $len > $maxlen && $pos + $len <= $eos {
                    $maxlen := $len;
                    $maxmatch := $match;
                    last if $s; # stop here for sequential alternation
                }
            }

            return $maxmatch if nqp::istype($maxmatch, Cursor);
            $cur.'!cursor_pass'($pos + $maxlen, '') if $maxlen >= 0;
            $cur
        }
        else {
            self."!cursor_start_cur"()
        }
    }

    method CALL_SUBRULE($rule, |c) {
        $rule(self, |c)
    }

    method DYNQUANT_LIMITS($mm) {
        if nqp::istype($mm,Range) {
            die 'Range minimum in quantifier (**) cannot be +Inf' if $mm.min ==  Inf;
            die 'Range maximum in quantifier (**) cannot be -Inf' if $mm.max == -Inf;
            nqp::list_i($mm.min < 0 ?? 0 !! $mm.min.Int, $mm.max == Inf ?? -1 !! $mm.max.Int)
        }
        else {
            fail 'Fixed quantifier cannot be infinite' if $mm == -Inf || $mm == Inf;
            nqp::list_i($mm.Int, $mm.Int)
        }
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
}

sub MAKE_REGEX($arg, :$i) {
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
        my $rx := $i ?? EVAL("anon regex \{ :i $arg\}") !! EVAL("anon regex \{ $arg\}");
        $arg does CachedCompiledRegex($rx);
        $rx
    }
}

# vim: ft=perl6 expandtab sw=4
