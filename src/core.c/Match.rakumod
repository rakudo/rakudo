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

    my constant $EMPTY_LIST = nqp::list();
    my constant $EMPTY_HASH = nqp::hash();

    method print() {
        callframe(1).my<$¢>
          ?? self.NQPMatchRole::print()
          !! self.Any::print()
    }

    # When nothing's `made`, we get an NQPMu that we'd like to replace
    # with Nil; all Rakudo objects typecheck as Mu, while NQPMu doesn't
    method ast()  { nqp::istype($!made, Mu) ?? $!made !! Nil }
    method made() { nqp::istype($!made, Mu) ?? $!made !! Nil }

    method Int(--> Int:D) { self.Match::Str.Int }

    method Str() is raw {
        $!pos >= $!from
          ?? nqp::substr(
               self.NQPMatchRole::target,
               $!from,
               nqp::sub_i(self.NQPMatchRole::to, $!from))
          !! ''
    }

    method STR() is implementation-detail {
        nqp::eqaddr(nqp::getattr(self,Match,'$!match'),NQPdidMATCH)
          ?? self.Match::Str
          !! self.Match::MATCH.Str
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
          ?? nqp::add_i($!from,1)
          !! $!pos);
        $!regexsub($new)
    }

    # Transfer capture marker positions from the capture stack into
    # $!from / $!to.  This is the part of .MATCH that determines the
    # match extent, without reifying any captures.
    method CURSOR_CAPTURE_MARKERS(--> Nil) is implementation-detail {
        my $cstack := nqp::getattr(self,Match,'$!cstack');
        nqp::if(
          $cstack,
          nqp::stmts(
            (my int $i = -1),
            nqp::while(
              nqp::islt_i(++$i,nqp::elems($cstack)),
              nqp::stmts(
                (my $cursor := nqp::atpos($cstack,$i)),
                (my str $name = nqp::getattr_s($cursor,Match,'$!name')),
                nqp::if(
                  nqp::not_i(nqp::isnull_s($name))
                    && (nqp::iseq_s($name,'$!from')
                         || nqp::iseq_s($name,'$!to')),
                  nqp::bindattr_i(self,Match,$name,
                    nqp::getattr_i($cursor,Match,'$!from'))
                )
              )
            )
          )
        )
    }

    ##### / <:General_Category{$property}> /
    my $general-category-property-lookup := nqp::hash(
        "Uppercase_Letter", "Lu",
        "Lowercase_Letter", "Ll",
        "Cased_Letter", "LC",
        "Titlecase_Letter", "Lt",
        "Modifier_Letter", "Lm",
        "Other_Letter", "Lo",
        "Nonspacing_Mark", "Mn",
        "Spacing_Mark", "Mc",
        "Enclosing_Mark", "Me",
        "Decimal_Number", "Nd",
        "digit", "Nd",
        "Connector_Punctuation", "Pc",
        "Dash_Punctuation", "Pd",
        "Open_Punctuation", "Po",
        "Close_Punctuation", "Pe",
        "Initial_Punctuation", "Pi",
        "Final_Punctuation", "Pf",
        "Other_Punctuation", "Po",
        "Math_Symbol", "Sm",
        "Currency_Symbol", "Sc",
        "Modifier_Symbol", "Sk",
        "Other_Symbol", "So",
        "Space_Separator", "Zs",
        "Line_Separator", "Zl",
        "Paragraph_Separator", "Zp",
        "cntrl", "Cc",
        "Control", "Cc",
        "Format", "Cf",
        "Surrogate", "Cs",
        "Private_Use", "Co",
        "Unassigned", "Cn"
    );
    my $general-category-family-lookup := nqp::hash(
        "Letter", "L",
        "L", "L",
        "Mark", "M",
        "M", "M",
        "Number", "N",
        "N", "N",
        "Punctuation", "P",
        "punct", "P",
        "Symbol", "S",
        "S", "S",
        "Separator", "Z",
        "Z", "Z",
        "Other", "C",
        "C", "C"
    );

    method DELEGATE-ACCEPTS($obj, $target) is implementation-detail {
        if nqp::istype($obj, Regex) {
            $obj.ACCEPTS($target) ?? 1 !! 0
        } else {
            my $constraint-property := nqp::istype($obj, Block)
                        ?? $obj()  # / <:General_Category{"Category"}> /
                        !! $obj;   # / <:General_Category("Category") + <:General_Category<Category_Property>> /

            if  nqp::istype($constraint-property, Str) && nqp::istype($target, Str) {
                nqp::iseq_s($constraint-property, $target)
                        ?? 1
                        !! (my $family := nqp::atkey($general-category-family-lookup, $constraint-property))
                                ?? nqp::iseq_s($family, nqp::substr($target, 0, 1))
                                !! (my $property := nqp::atkey($general-category-property-lookup, $constraint-property))
                                    && nqp::iseq_s($property, $target)
                                            ?? 1
                                            !! 0; # XXX Could throw about missing property
            } else {
                $constraint-property.ACCEPTS($target) ?? 1 !! 0
            }
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

    method clone() is raw { nqp::clone(self) }

    multi method WHICH(Match:D: --> ObjAt:D) {
        self.Mu::WHICH # skip Capture's as Match is not a value type
    }

    proto method Bool(|) {*}
    multi method Bool(Match:U: --> False) { }
    multi method Bool(Match:D:) { nqp::hllbool($!pos >= $!from) }

    proto method not(|) {*}
    multi method not(Match:U: --> True) { }
    multi method not(Match:D:) { nqp::hllbool($!pos < $!from) }

    multi method Numeric(Match:D:) {
        self.Str.Numeric
    }
    multi method ACCEPTS(Match:D: Mu) { self }

    method prematch(Match:D:) {
        nqp::substr(self.NQPMatchRole::target,0,$!from)
    }
    method postmatch(Match:D:) {
        nqp::substr(self.NQPMatchRole::target,self.NQPMatchRole::to)
    }

    method !sort-on-from-pos() {
        nqp::add_i(
          nqp::bitshiftl_i(nqp::getattr_i(self,Match,'$!from'),32),
          nqp::getattr_i(self,Match,'$!pos')
        )
    }

    method caps(Match:D:) {
        my $caps := nqp::list;
        for self.Match::pairs {
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
        my $target := self.NQPMatchRole::target;
        gather {
            for self.Match::caps {
                if .value.NQPMatchRole::from > $prev {
                    take '~' => substr($target,$prev,.value.NQPMatchRole::from - $prev)
                }
                take $_;
                $prev = .value.NQPMatchRole::pos;
            }
            take '~' => substr($target,$prev, $!pos - $prev) if $prev < $!pos;
        }
    }

    multi method raku(Match:D: --> Str:D) {
        my $attrs := nqp::list_s;

        nqp::push_s($attrs,(orig => self.NQPMatchRole::orig // '').raku);
        nqp::push_s($attrs,(from => self.NQPMatchRole::from // 0).raku);
        nqp::push_s($attrs,(pos  => self.NQPMatchRole::pos // 0).raku);
        if self.Capture::list -> @list { nqp::push_s($attrs,:@list.raku) }
        if self.Capture::hash -> %hash { nqp::push_s($attrs,:%hash.raku) }
        nqp::push_s($attrs,(made => $_).raku) with self.NQPMatchRole::made;

        nqp::concat('Match.new(',nqp::concat(nqp::join(', ',$attrs),')'))
    }
    multi method gist (Match:D: $d = 0) {
        return "#<failed match>" unless self;
        my $s = ' ' x ($d + 1);
        my $r = ("=> " if $d) ~ "｢" ~ self ~ "｣\n";
        for self.Match::caps {
            $r ~= $s ~ (.key // '?') ~ ' ' ~ &?ROUTINE(.value, $d + 1);
        }
        $d == 0 ?? $r.chomp !! $r
    }

    method replace-with(Match:D: Str() $replacement --> Str:D) {
        self.Match::prematch ~ $replacement ~ self.Match::postmatch
    }
}

multi sub infix:<eqv>(Match:D $a, Match:D $b) {
    $a =:= $b
    ||
    [&&] (
        $a.NQPMatchRole::pos  eqv $b.NQPMatchRole::pos,
        $a.NQPMatchRole::from eqv $b.NQPMatchRole::from,
        $a.NQPMatchRole::orig eqv $b.NQPMatchRole::orig,
        ($a.NQPMatchRole::made // Any) eqv ($b.NQPMatchRole::made // Any),
        ($a.Capture::list // nqp::list ) eqv ($b.Capture::list // nqp::list ),
        ($a.Capture::hash // nqp::hash ) eqv ($b.Capture::hash // nqp::hash )
    );
}


# Attach to the match of the nearest enclosing regex frame when there is
# one. Its cursor cannot be disturbed by user code, however a match run
# inside the regex's code block can overwrite $/. In a 6.e scope, where
# $/ carries the isolation marker, the next candidate is the Match in
# the immediate caller frame's own $/, i.e. the match that frame most
# recently established. After that comes a Match topic, so a block
# iterating match results attaches to its topic even when it never set
# up a $/ of its own. Everything else attaches to the caller's $/, as
# in an action method, whose $/ parameter carries no marker in any
# revision. The caller lookups must stay at routine level. From inside
# a nested block they would see this routine's own frame as the caller
# and find its own $/. The topic lookup must stay behind the marker
# check. A native $_ in the caller chain makes the lexical walk itself
# die, so callers outside a 6.e scope must never trigger it.
sub make(Mu \made) {
    my $cursor      := nqp::decont(nqp::getlexcaller('$¢'));
    my \pad         := nqp::ctxlexpad(nqp::ctxcallerskipthunks(nqp::ctx()));
    my \nearby-cont := nqp::existskey(pad,'$/')
      ?? nqp::atkey(pad,'$/')
      !! nqp::null();
    my \slash-cont  := nqp::getlexcaller('$/');
    my $slash       := nqp::decont(slash-cont);
    nqp::isconcrete($cursor) && nqp::istype($cursor, NQPMatchRole)
      ?? nqp::bindattr($cursor.MATCH, Match, '$!made', made)
      !! Rakudo::Internals.IS-ISOLATED-MATCH(nearby-cont)
           && nqp::isconcrete(my $nearby := nqp::decont(nearby-cont))
           && nqp::istype($nearby, NQPMatchRole)
        ?? nqp::bindattr($nearby, Match, '$!made', made)
        !! Rakudo::Internals.IS-ISOLATED-MATCH(slash-cont)
             && nqp::isconcrete(my $topic := nqp::decont(nqp::getlexcaller('$_')))
             && nqp::istype($topic, NQPMatchRole)
          ?? nqp::bindattr($topic, Match, '$!made', made)
          !! nqp::istype($slash, NQPMatchRole)
            ?? nqp::bindattr($slash,Match,'$!made',made)
            !! X::Make::MatchRequired.new(:got($slash)).throw
}

# A concrete Match matcher is a match result already, so the smartmatch
# returns it as-is rather than boolifying, the same way matching against a
# regex returns the Match. The raku-smartmatch dispatcher implements the
# same rule.
multi sub infix:<~~>(Mu \topic, Match:D $matcher) {
    $matcher
}
multi sub infix:<!~~>(Mu \topic, Match:D $matcher) {
    $matcher.not
}
# Disambiguate from the Junction topic candidates: a Junction topic still
# matches over its eigenstates.
multi sub infix:<~~>(Junction:D \topic, Match:D $matcher) {
#?if moar
    nqp::dispatch('raku-smartmatch', topic, $matcher, nqp::unbox_i(1))
#?endif
#?if !moar
    SETTING-ONLY-ACCEPTS($matcher)
      ?? topic.BOOLIFY-ACCEPTS($matcher)
      !! $matcher.ACCEPTS(topic).Bool
#?endif
}
multi sub infix:<!~~>(Junction:D \topic, Match:D $matcher) {
#?if moar
    nqp::dispatch('raku-smartmatch', topic, $matcher, nqp::unbox_i(-1))
#?endif
#?if !moar
    SETTING-ONLY-ACCEPTS($matcher)
      ?? topic.BOOLIFY-ACCEPTS($matcher, 1)
      !! $matcher.ACCEPTS(topic).not
#?endif
}

# vim: expandtab shiftwidth=4
