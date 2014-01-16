my class Cursor does NQPCursorRole {
    has $!ast; # Need it to survive re-creations of the match object.

    method MATCH() {
        my $match := nqp::getattr(self, Cursor, '$!match');
        return $match if nqp::istype($match, Match) && nqp::isconcrete($match);
        $match := nqp::create(Match);
        nqp::bindattr($match, Match, '$!orig', nqp::findmethod(self, 'orig')(self));
        nqp::bindattr_i($match, Match, '$!from', nqp::getattr_i(self, Cursor, '$!from'));
        nqp::bindattr_i($match, Match, '$!to', nqp::getattr_i(self, Cursor, '$!pos'));
        nqp::bindattr($match, Match, '$!ast', nqp::getattr(self, Cursor, '$!ast'));
        nqp::bindattr($match, Match, '$!CURSOR', self);
        my Mu $list := nqp::list();
        my Mu $hash := nqp::hash();
        if $match.Bool {
            my Mu $caphash := nqp::findmethod(Cursor, 'CAPHASH')(self);
            my Mu $capiter := nqp::iterator($caphash);
            while $capiter {
                my Mu $kv := nqp::shift($capiter);
                my str $key = nqp::iterkey_s($kv);
                my Mu $value := nqp::hllize(nqp::atkey($caphash, $key));
                if $key eq '$!from' || $key eq '$!to' {
                    nqp::bindattr_i($match, Match, $key, $value.from);
                }
                else {
                    $value := nqp::p6list($value, List, Mu)
                        if nqp::islist($value);
                    nqp::iscclass(nqp::const::CCLASS_NUMERIC, $key, 0)
                      ?? nqp::bindpos($list, $key, $value)
                      !! nqp::bindkey($hash, $key, $value);
                }
            }
        }
        nqp::bindattr($match, Capture, '$!list', $list);
        nqp::bindattr($match, Capture, '$!hash', $hash);
        nqp::bindattr(self, Cursor, '$!match', $match);
        $match;
    }

    method MATCH_SAVE() {
        nqp::getattr_i(self, Cursor, '$!pos') < 0 ?? Nil !! self.MATCH()
    }

    # INTERPOLATE will iterate over the string $tgt beginning at position 0.
    # If it can't match against pattern $var (or any element of $var if it is an array)
    # it will increment $pos and try again. Therefor it is important to only match
    # against the current position.
    # $i is case insensitive flag
    # $s is for sequential matching instead of junctive
    # $a is true if we are in an assertion
    method INTERPOLATE($var, $i = 0, $s = 0, $a = 0) {
        if nqp::isconcrete($var) {
            # Call it if it is a routine. This will capture if requested.
            return $var(self) if $var ~~ Callable;
            my $maxlen := -1;
            my $cur := self.'!cursor_start_cur'();
            my $pos := nqp::getattr_i($cur, $?CLASS, '$!from');
            my $tgt := $cur.target;
            my $eos := nqp::chars($tgt);
            my Mu $nfa := QRegex::NFA.new;
            my $fate   := 0;
            my $count  := 0;
            my $start  := 1;
            my Mu $alts := nqp::list();
            my Mu $order := nqp::list();

            if nqp::istype($var, Positional) {
                if $s {
                    # The order matters for sequential matching, therefor no NFA involved.
                    $order := $var.list;
                }
                else {
                    # prepare to run the NFA if $var is array-ish.
                    for $var.list -> $topic {
                        nqp::push($alts, $topic);
                        if $a {
                            # We are in a regex assertion, the strings we get will be treated as
                            # regex rules.
                            return $cur.'!cursor_start_cur'() if $topic ~~ Associative;
                            my $rx := MAKE_REGEX($topic, :$i);
                            my Mu $nfas := nqp::findmethod($rx, 'NFA')($rx);
                            $nfa.mergesubstates($start, 0, $fate, $nfas, Mu);
                        }
                        elsif $topic ~~ Regex {
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
                # Use the $var as it is if it's not array-ish.
                $order := $var;
            }

            for $order -> $topic {
                my $match;
                my $len;
                
                if $a {
                    # We are in a regex assertion, the strings we get will be treated as
                    # regex rules.
                    return $cur.'!cursor_start_cur'() if $topic ~~ Associative;
                    my $rx := MAKE_REGEX($topic, :$i);
                    $match := (nqp::substr($tgt, $pos, $eos - $pos) ~~ $rx).Str;
                    $len   := nqp::chars( $match );
                }
                elsif $topic ~~ Regex {
                    # A Regex already.
                    $match := nqp::substr($tgt, $pos, $eos - $pos) ~~ $topic;
                    
                    # In order to return the correct result we need to match from the
                    # current position only.
                    next if $match.from;
                    $match := ~$match;
                    $len   := nqp::chars( $match );
                }
                else {
                    # The pattern is a string.
                    my str $topic_str = $topic.Str;
                    $len   := nqp::chars( $topic_str );
                    $match := $len < 1
                            ||  ($i ?? nqp::lc(nqp::substr($tgt, $pos, $len)) eq nqp::lc($topic_str)
                                    !! nqp::substr($tgt, $pos, $len) eq $topic_str);
                }

                if $match && $len > $maxlen && $pos + $len <= $eos {
                    $maxlen := $len;
                    last if $s; # stop here for sequential alternation
                }
            }

            $cur.'!cursor_pass'($pos + $maxlen, '') if $maxlen >= 0;
            $cur
        }
        else {
            self."!cursor_start_cur"()
        }
    }
    
    method OTHERGRAMMAR($grammar, $name, |) {
        my $lang_cursor := $grammar.'!cursor_init'(self.target(), :p(self.pos()));
        $lang_cursor."$name"(); 
    }
    
    method RECURSE() {
        nqp::getlexdyn('$?REGEX')(self)
    }
}

sub MAKE_REGEX($arg, :$i) {
    my role CachedCompiledRegex {
        has $.regex;
    }
    if $arg ~~ Regex {
        $arg
    }
    elsif nqp::istype($arg, CachedCompiledRegex) {
        $arg.regex
    }
    else {
        my Mu $chars := nqp::split('', $arg);
        my $k := 0;
        my $iter := nqp::iterator($chars);
        while $iter {
            my $ord := nqp::ord( nqp::shift($iter) );
            nqp::bindpos($chars, $k, "\\c[$ord]") if $ord <= 32;
            $k := $k + 1;
        }
        my $arg2 := nqp::join('', $chars);
        my $rx := $i ?? EVAL("anon regex \{ :i ^$arg2\}") !! EVAL("anon regex \{ ^$arg2\}");
        $arg does CachedCompiledRegex($rx);
        $rx
    }
}


