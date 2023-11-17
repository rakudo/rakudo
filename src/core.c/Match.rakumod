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

#-------------------------------------------------------------------------------
# Internal subroutine accessors to regex engine (embedded) attributes.  Note
# that some of these are scoped to "our" to allow infix:<eqv> to access the
# the attributes outside of the Match namespace.

    our sub from(Mu \SELF --> int) is raw is implementation-detail {
        nqp::getattr_i(nqp::decont(SELF),Match,'$!from')
    }
    our sub pos(Mu \SELF --> int) is raw is implementation-detail {
        nqp::getattr_i(nqp::decont(SELF),Match,'$!pos')
    }
    our sub to(Mu \SELF --> int) is raw is implementation-detail {
        nqp::getattr_i(nqp::decont(SELF),Match,'$!to')
    }

    # When nothing's `made`, we get an NQPMu that we'd like to replace
    # with Nil; all Rakudo objects typecheck as Mu, while NQPMu doesn't
    our sub made(Mu \SELF) is raw is implementation-detail {
        nqp::istype((my $made := nqp::getattr(SELF,Match,'$!made')),Mu)
          ?? $made
          !! Nil
    }

    our sub orig(Mu \SELF) is raw is implementation-detail {
        my $shared := nqp::getattr(SELF,Match,'$!shared');
        nqp::getattr($shared,$shared.WHAT,'$!orig')
    }

    # Special purpose accessor to return either the "to", or if that has
    # been marked as invalid, the "pos"
    sub to-or-pos(Mu \SELF --> int) is raw {
        nqp::getattr_i(SELF,Match,'$!to') < 0
          ?? nqp::getattr_i(SELF,Match,'$!pos')
          !! nqp::getattr_i(SELF,Match,'$!to')
    }

    # Setter / Marker to indicate HLL Match object is set up
    sub set-up(Mu \SELF --> Nil) {
        nqp::bindattr(SELF,Match,'$!match',NQPdidMATCH)
    }
    sub is-set-up(Mu \SELF) {
        nqp::eqaddr(nqp::getattr(SELF,Match,'$!match'),NQPdidMATCH)
    }

    sub regexsub(Mu \SELF) is raw { nqp::getattr(SELF,Match,'$!regexsub') }
    sub restart( Mu \SELF) is raw { nqp::getattr(SELF,Match,'$!restart')  }
    sub bstack(  Mu \SELF) is raw { nqp::getattr(SELF,Match,'$!bstack')   }
    sub cstack(  Mu \SELF) is raw { nqp::getattr(SELF,Match,'$!cstack')   }
#    sub shared(  Mu \SELF) is raw { nqp::getattr(SELF,Match,'$!shared')   }
#    sub braid(   Mu \SELF) is raw { nqp::getattr(SELF,Match,'$!braid')    }

    sub target(Mu \SELF --> str) is raw {
        my $shared := nqp::getattr(SELF,Match,'$!shared');
        nqp::getattr_s($shared,$shared.WHAT,'$!target')
    }

#-------------------------------------------------------------------------------
# Internal subroutine mutators of regex engine (embedded) attributes.  Note
# that some of these are scoped to "our" to allow sub make to access the
# the attributes outside of the Match namespace.

    our sub set-made(Mu \SELF, Mu \made) is raw is implementation-detail {
        nqp::bindattr(SELF,Match,'$!made',
          made.defined ?? nqp::decont(made) !! nqp::null
        );
    }

    sub set-from(Mu \SELF, Mu \value) is raw {
        nqp::bindattr_i(SELF,Match,'$!from',value)
    }
    sub set-pos(Mu \SELF, Mu \value) is raw {
        nqp::bindattr_i(SELF,Match,'$!pos',value)
    }
    sub reset-from-to(Mu \SELF --> Nil) {
        nqp::bindattr_i(SELF,Match,'$!from',
          nqp::bindattr_i(SELF,Match,'$!to',-1)
        )
    }
    sub release-regexsub-cstack(Mu \SELF --> Nil) {
        nqp::bindattr(SELF,Match,'$!regexsub',nqp::null);
        nqp::bindattr(SELF,Match,'$!cstack',nqp::null);
    }
    sub copy-shared-braid(Mu \to, Mu \from --> Nil) {
        nqp::bindattr(to,Match,'$!shared',nqp::getattr(from,Match,'$!shared'));
        nqp::bindattr(to,Match,'$!braid', nqp::getattr(from,Match,'$!braid'));
    }

#-------------------------------------------------------------------------------
# Derived internal "methods"

    sub as-string(Mu \SELF --> str) is raw {
        pos(SELF) >= from(SELF)
          ?? nqp::substr(target(SELF),from(SELF),to-or-pos(SELF) - from(SELF))
          !! ''
    }
    sub as-integer(Mu \SELF --> Int:D) is raw { as-string(SELF).Int }

    sub prematch( Mu \SELF --> str) is raw {
        nqp::substr(target(SELF),0,from(SELF))
    }
    sub postmatch(Mu \SELF --> str) is raw {
        nqp::substr(target(SELF),to-or-pos(SELF))
    }

#-------------------------------------------------------------------------------
# Public method-based accessors

    method ast()  { made(self) }
    method made() { made(self) }

    method Str(--> Str:D) { as-string(self)  }
    method Int(--> Int:D) { as-integer(self) }

    method STR() is implementation-detail {
        is-set-up(self) ?? as-string(self) !! as-string(self.Match::MATCH)
    }

    method MATCH() is implementation-detail {
        nqp::unless(
          is-set-up(self),
          nqp::if(                           # must still set up
            pos(self) < from(self)
              || nqp::isnull(my $rxsub := regexsub(self))
              || nqp::isnull(my $CAPS  := nqp::tryfindmethod($rxsub,'CAPS'))
              || nqp::isnull(my $captures := $CAPS($rxsub))
              || nqp::not_i($captures.has-captures),
            nqp::stmts(                      # no captures
              nqp::bindattr(self,Capture,'@!list',$EMPTY_LIST),
              nqp::bindattr(self,Capture,'%!hash',$EMPTY_HASH),
              set-up(self)                   # mark as set up
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
        if nqp::istrue(my $cs := cstack(self)) {

            # only one destination, avoid repeated hash lookups
            if $captures.onlyname -> str $onlyname {

                # numeric: <= ord("9") so positional capture
                my Mu $dest := nqp::atpos(
                  nqp::islt_i(nqp::ord($onlyname),58) ?? $list !! $hash,
                  $onlyname
                );

                # simply reify all the cursors
                my int $i = -1;
                nqp::while(
                  nqp::islt_i(++$i,nqp::elems($cs)),
                  nqp::stmts(
                    (my $cursor := nqp::atpos($cs,$i)),
                    nqp::unless(
                      nqp::isnull_s(nqp::getattr_s($cursor,$?CLASS,'$!name')),
                      nqp::push($dest,$cursor.Match::MATCH)  # recurse
                    )
                  )
                );
            }

            # more than one destination
            else {
                my int $i = -1;
                nqp::while(
                  nqp::islt_i(++$i,nqp::elems($cs)),
                  nqp::stmts(                               # handle this cursor
                    (my $cursor := nqp::atpos($cs,$i)),
                    (my str $name = nqp::getattr_s($cursor,$?CLASS,'$!name')),
                    nqp::if(
                      nqp::not_i(nqp::isnull_s($name))
                        && nqp::isge_i(nqp::chars($name),1),
                      nqp::stmts(                           # has a name
                        (my $match := $cursor.Match::MATCH),  # recurse
                        nqp::if(
                          nqp::iseq_s($name,'$!from')
                            || nqp::iseq_s($name,'$!to'),
                          nqp::bindattr_i(self,Match,$name, # it's from|to
                            from($match)
                          ),
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
        release-regexsub-cstack(self) unless nqp::defined(bstack(self));

        # mark as set up
        set-up(self);
    }

    # from !cursor_next in nqp
    method CURSOR_NEXT() is raw is implementation-detail {
        nqp::if(
          nqp::defined(restart(self)),
          restart(self)(self),
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
        copy-shared-braid($new, self);
        reset-from-to($new);
        set-pos($new,from(self) + 1);
        regexsub(self)($new)
    }

    # adapted from !cursor_more in nqp
    method CURSOR_MORE() is raw is implementation-detail {
        my $new := nqp::create(self);
        copy-shared-braid($new, self);
        reset-from-to($new);
        set-pos($new, from(self) >= pos(self)
#?if !js
          ?? from(self) + 1
#?endif
#?if js
          ?? from(self) + move_cursor(target(self), pos(self))
#?endif
          !! pos(self));
        regexsub(self)($new)
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
        set-from(self, $from);
        set-made(self, $made);
    }

    method clone() is raw { nqp::clone(self) }

    multi method WHICH(Match:D: --> ObjAt:D) {
        self.Mu::WHICH # skip Capture's as Match is not a value type
    }

    proto method Bool(|) {*}
    multi method Bool(Match:U: --> False) { }
    multi method Bool(Match:D:) { nqp::hllbool(pos(self) >= from(self)) }

    proto method not(|) {*}
    multi method not(Match:U: --> True) { }
    multi method not(Match:D:) { nqp::hllbool(pos(self) < from(self)) }

    multi method Numeric(Match:D:) {
        self.Str.Numeric
    }
    multi method ACCEPTS(Match:D: Mu) { self }

    method prematch( Match:D: --> Str:D) { prematch(self)  }
    method postmatch(Match:D: --> Str:D) { postmatch(self) }

    method !sort-on-from-pos() {
        nqp::bitshiftl_i(from(self),32) + pos(self)
    }

    method caps(Match:D:) {
        my $caps := nqp::list;
        for self.Capture::pairs {
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
        my int $prev   = from(self);
        my str $target = target(self);
        gather {
            for self.Match::caps {
                if from(.value) > $prev {
                    take '~' => nqp::substr($target,$prev,from(.value) - $prev)
                }
                take $_;
                $prev = pos(.value);
            }
            take '~' => nqp::substr($target,$prev, pos(self) - $prev)
              if $prev < pos(self);
        }
    }

    multi method raku(Match:D: --> Str:D) {
        my $attrs := nqp::list_s;

        nqp::push_s($attrs,'orig => ' ~ (orig(self) // '').raku);
        nqp::push_s($attrs,'from => ' ~ (from(self) // 0).Str);
        nqp::push_s($attrs,'pos => '  ~ (pos( self) // 0).Str);
        if self.Capture::list -> @list { nqp::push_s($attrs,:@list.raku) }
        if self.Capture::hash -> %hash { nqp::push_s($attrs,:%hash.raku) }
        nqp::push_s($attrs,'made => ' ~ .raku) with made(self);

        nqp::concat('Match.new(',nqp::concat(nqp::join(', ',$attrs),')'))
    }
    multi method gist (Match:D: $d = 0) {
        return "#<failed match>" unless self;
        my $s = ' ' x ($d + 1);
        my $r = ("=> " if $d) ~ "｢" ~ self ~ "｣\n";
        for self.Match::caps {
            $r ~= $s ~ (.key // '?') ~ ' ' ~ &?ROUTINE(.value, $d + 1);
        }
        $d == 0 ?? $r.chomp !! $r;
    }

    method replace-with(Match:D: Str() $replacement --> Str:D) {
        prematch(self) ~ $replacement ~ postmatch(self)
    }
}

multi sub infix:<eqv>(Match:D $a, Match:D $b) {
    $a =:= $b
    ||
    [&&] (
        Match::pos($a)  eqv Match::pos($b),
        Match::from($a) eqv Match::from($b),
        Match::orig($a) eqv Match::orig($b),
        Match::made($a) eqv Match::made($b),
        ($a.Capture::list // nqp::list ) eqv ($b.Capture::list // nqp::list ),
        ($a.Capture::hash // nqp::hash ) eqv ($b.Capture::hash // nqp::hash )
    );
}

sub make(Mu \made) {
    my $slash := nqp::decont(nqp::getlexcaller('$/'));
    nqp::istype($slash,NQPMatchRole)
      ?? Match::set-made($slash, made)
      !! X::Make::MatchRequired.new(:got($slash)).throw
}

# vim: expandtab shiftwidth=4
