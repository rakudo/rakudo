my class Match is Capture is Cool does NQPMatchRole {
    my Mu $EMPTY_LIST := nqp::list();
    my Mu $NO_CAPS    := nqp::hash();
    my Mu $DID_MATCH  := nqp::create(NQPdidMATCH);

    method ast()  { nqp::if(nqp::isconcrete($!made),$!made,Nil) }
    method made() { nqp::if(nqp::isconcrete($!made),$!made,Nil) }

    method STR() {
        nqp::if(
          nqp::istype(nqp::getattr(self,Match,'$!match'), NQPdidMATCH),
          self.Str,
          self!MATCH.Str
        )
    }

    method MATCH() {
        nqp::if(
          nqp::istype(nqp::getattr(self,Match,'$!match'), NQPdidMATCH),
          self,
          self!MATCH
        )
    }

    method !MATCH() {
        my int $from = nqp::getattr_i(self, Match, '$!from');
        my int $pos  = nqp::getattr_i(self, Match, '$!pos');
        my Mu $list;
        my Mu $hash := nqp::hash();
        if nqp::isge_i($pos, $from) {
            # For captures with lists, initialize the lists.
            my $caplist := $NO_CAPS;
            my $rxsub   := nqp::getattr(self, Match, '$!regexsub');
            my str $onlyname  = '';
            my int $namecount = 0;

            if nqp::not_i(nqp::isnull($rxsub)) {
                $caplist := nqp::can($rxsub, 'CAPS') ?? nqp::findmethod($rxsub, 'CAPS')($rxsub) !! nqp::null();
                if nqp::not_i(nqp::isnull($caplist)) && nqp::istrue($caplist) {
                    my $iter := nqp::iterator($caplist);
                    my str $name;
                    while $iter {
                        $namecount = nqp::add_i($namecount, 1);
                        if nqp::iterval(nqp::shift($iter)) >= 2 {
                            $name = nqp::iterkey_s($iter);
                            $onlyname = $name if nqp::iseq_i($namecount, 1);
                            nqp::iscclass(nqp::const::CCLASS_NUMERIC, $name, 0)
                                ?? nqp::bindpos(
                                        nqp::if(nqp::isconcrete($list), $list, ($list := nqp::list())),
                                        nqp::fromstr_I($name, Int), [])
                                !! nqp::bindkey($hash, $name, []);
                        }
                    }
                }
            }

            # Walk the capture stack and populate the Match.
            my Mu $cs := nqp::getattr(self, Match, '$!cstack');
            if nqp::isnull($cs) || nqp::not_i(nqp::istrue($cs)) {}
#?if !jvm
            elsif nqp::not_i(nqp::istrue($caplist)) {}
#?endif
            elsif nqp::iseq_i($namecount, 1) && nqp::isgt_i(nqp::chars($onlyname), 0) && nqp::eqat($onlyname, '$!', 0) {
                # If there's only one destination, avoid repeated hash lookups
                my int $cselems = nqp::elems($cs);
                my int $csi = -1;
                my Mu $dest;

                # numeric: <= ord("9") so positional capture
                $dest := nqp::islt_i(nqp::ord($onlyname),58)
                  ?? nqp::atpos($list, $onlyname)
                  !! nqp::atkey($hash, $onlyname);

                my $subcur;
                my str $name;
                while nqp::islt_i(++$csi,$cselems) {
                    $subcur := nqp::atpos($cs, $csi);
                    $name    = nqp::getattr_s($subcur, $?CLASS, '$!name');
                    nqp::push($dest,$subcur.MATCH())
                      if nqp::not_i(nqp::isnull_s($name));
                }
            }
            else {
                my int $cselems = nqp::elems($cs);
                my int $csi     = -1;
                my $subcur;
                my str $name;
                while nqp::islt_i(++$csi,$cselems) {
                    $subcur := nqp::atpos($cs, $csi);
                    $name    = nqp::getattr_s($subcur, $?CLASS, '$!name');
                    if nqp::not_i(nqp::isnull_s($name)) && nqp::isgt_i(nqp::chars($name), 0) {
                        my Mu $submatch := $subcur.MATCH;
                        if nqp::eqat($name, '$', 0) && (nqp::iseq_s($name, '$!from') || nqp::iseq_s($name, '$!to')) {
                            nqp::bindattr_i(self, Match, $name, $submatch.from);
                        }
                        elsif nqp::islt_i(nqp::index($name, '='), 0) {
                            my Mu $capval     := nqp::atkey($caplist, $name);
                            my int $needs_list = nqp::isconcrete($capval) && $capval >= 2;
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
                        else {
                            my $names := nqp::split('=', $name);
                            my $iter  := nqp::iterator($names);
                            my Mu $capval;
                            my int $needs_list;
                            while $iter {
                                $name    = nqp::shift($iter);
                                $capval := nqp::atkey($caplist, $name);
                                $needs_list = nqp::isconcrete($capval) && $capval >= 2;
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
        nqp::bindattr(self, Capture, '@!list', nqp::isconcrete($list) ?? $list !! $EMPTY_LIST);
        nqp::bindattr(self, Capture, '%!hash', $hash);
        nqp::bindattr(self, Match, '$!match', $DID_MATCH);

        # Once we've produced the captures, and if we know we're finished and
        # will never be backtracked into, we can release cstack and regexsub.
        unless nqp::defined(nqp::getattr(self, Match, '$!bstack')) {
            nqp::bindattr(self, Match, '$!cstack', nqp::null());
            nqp::bindattr(self, Match, '$!regexsub', nqp::null());
        }

        self;
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
          nqp::bindattr(  $new,$?CLASS,'$!braid',$!braid),
          nqp::bindattr_i($new,$?CLASS,'$!from',-1),
          nqp::bindattr_i($new,$?CLASS,'$!pos',nqp::add_i($!from,1)),
          nqp::bindattr_i($new,$?CLASS,'$!to',-1),
          $!regexsub($new)
        )
    }

    method CURSOR_MORE() {  # adapted from !cursor_more in nqp
        nqp::stmts(
          (my $new := nqp::create(self)),
          nqp::bindattr(  $new,$?CLASS,'$!shared',$!shared),
          nqp::bindattr(  $new,$?CLASS,'$!braid',$!braid),
          nqp::bindattr_i($new,$?CLASS,'$!from',-1),
          nqp::bindattr_i($new,$?CLASS,'$!pos',
            nqp::if(
              nqp::isge_i($!from,$!pos),
              nqp::add_i($!from,1),
              $!pos
            )
          ),
          nqp::bindattr_i($new,$?CLASS,'$!to',-1),
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
                    $match = nqp::eqat($tgt, $topic_str, $pos);
                }

                # ignoremark+ignorecase
                elsif $m && $i {
                    $match = nqp::eqaticim($tgt, $topic_str, $pos);
                }

                # ignoremark
                elsif $m {
                    $match = nqp::eqatim($tgt, $topic_str, $pos);
                }

                # ignorecase
                elsif $i {
                    $match = nqp::eqatic($tgt, $topic_str, $pos);
                }

                if $match
                  && nqp::isgt_i($len,$maxlen)
                  && nqp::isle_i(nqp::add_i($pos,$len),$eos) {
                    $maxlen    = $len;
                    $maxmatch := $match;
                    last if $s; # stop here for sequential alternation
                }
            }

            nqp::istype($maxmatch, Match)
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
        $lang_cursor.clone_braid_from(self);
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

    method clone() {
	my $new := nqp::clone(self);
	$new;
    }

    multi method WHICH (Match:D:) {
        self.Mu::WHICH # skip Capture's as Match is not a value type
    }

    proto method Bool(|) { * }
    multi method Bool(Match:U:) { False }
    multi method Bool(Match:D:) { nqp::p6bool($!pos >= $!from) }

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
            if nqp::istype($p.value,Array) {
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

    multi method perl(Match:D:) {
        my %attrs;
        %attrs.ASSIGN-KEY("orig", (self.orig // '' ).perl);
        %attrs.ASSIGN-KEY("from", (self.from // 0  ).perl);
        %attrs.ASSIGN-KEY("pos",  (self.pos  // 0  ).perl);
        %attrs.ASSIGN-KEY("made", (self.made // Any).perl);
        %attrs.ASSIGN-KEY("list", (self.Capture::list // [] ).perl);
        %attrs.ASSIGN-KEY("hash", (self.Capture::hash // {} ).perl);

        'Match.new('
            ~ %attrs.fmt('%s => %s', ', ')
            ~ ')'
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
    my $slash := nqp::getlexcaller('$/');
    nqp::bindattr( nqp::decont($slash),        Match,  '$!made', made );
}


# vim: ft=perl6 expandtab sw=4
