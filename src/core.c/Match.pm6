my class Match is Capture is Cool does NQPMatchRole {
# from NQPMatchRole
#    has int $!from;  # start position of match
#    has int $!pos;   # current cursor position
#    has int $!to;    # (if negative, use $!pos)
#    has $!shared;    # shared parse attributes, see ParseShared
#    has $!braid;     # current braid
#    has $!bstack;    # backtracking stack
#    has $!cstack;    # captures stack
#    has $!regexsub;  # actual sub for running the regex
#    has $!restart;   # sub for restarting a search
#    has $!made;      # value set by "make"
#    has $!match;     # flag indicating Match object set up (NQPdidMATCH)
#    has str $!name;  # name if named capture

#?if !js
    my constant $EMPTY_LIST = nqp::list();
    my constant $EMPTY_HASH = nqp::hash();
#?endif
#?if js
    my $EMPTY_LIST := nqp::list();
    my $EMPTY_HASH := nqp::hash();
#?endif

    # When nothing's `made`, we get an NQPMu that we'd like to replace
    # with Nil; all Rakudo objects typecheck as Mu, while NQPMu doesn't
    method ast()  { nqp::istype($!made, Mu) ?? $!made !! Nil }
    method made() { nqp::istype($!made, Mu) ?? $!made !! Nil }

    method Int(--> Int:D) { self.Str.Int }

    method Str() is raw { self.NQPMatchRole::Str }

    method STR() is implementation-detail {
        nqp::eqaddr(nqp::getattr(self,Match,'$!match'),NQPdidMATCH)
          ?? self.Str
          !! self.MATCH.Str
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
                      nqp::push($dest,$cursor.MATCH)  # recurse
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
                        (my $match := $cursor.MATCH),  # recurse
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
                                  nqp::istype(nqp::atpos($list,$name),Array),
                                  nqp::atpos($list,$name).push($match),
                                  nqp::bindpos($list,$name,$match)  # XXX
                                ),
                                nqp::if(                    # named capture
                                  nqp::istype(nqp::atkey($hash,$name),Array),
                                  nqp::atkey($hash,$name).push($match),
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

    method !sort-on-from-pos() {
        nqp::add_i(
          nqp::bitshiftl_i(nqp::getattr_i(self,Match,'$!from'),32),
          nqp::getattr_i(self,Match,'$!pos')
        )
    }

    method caps(Match:D:) {
        my $caps := nqp::list;
        for self.pairs {
            my \key   := .key;
            my \value := .value;

            if nqp::istype(value,List) {
                nqp::push($caps,Pair.new(key, $_)) for value.list;
            }
            elsif nqp::isconcrete(value) {
                nqp::push($caps,$_);
            }
        }
        Rakudo::Sorting.MERGESORT-REIFIED-LIST-AS(
          $caps, *.value!sort-on-from-pos
        )
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


# vim: expandtab shiftwidth=4
