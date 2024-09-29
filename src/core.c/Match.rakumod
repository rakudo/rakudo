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
        $d == 0 ?? $r.Match::chomp !! $r;
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


sub make(Mu \made) {
    my $slash := nqp::decont(nqp::getlexcaller('$/'));
    nqp::istype($slash, NQPMatchRole)
        ?? nqp::bindattr($slash,Match,'$!made',made)
        !! X::Make::MatchRequired.new(:got($slash)).throw
}


# vim: expandtab shiftwidth=4
