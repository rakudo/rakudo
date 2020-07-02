my class Match is Cool does NQPMatchRole {

    method Capture(Match:D:) { self }

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
#    has $!match;     # originally a flag indicating Match object is vivified
#                       (NQPdidMATCH).  Repurposed to contain the hash of the
#                       captures: if nqp::hash($!match) is true, then the
#                       object has been vivified.  The NQP QRegex code in some
#                       situations resets this attribute, which is now fine as
#                       will cause re-vivification at a later stage in MATCH.
#    has str $!name;  # name if named capture

    # save on creating empty objects all the time
    my $EMPTY_LIST := nqp::create(IterationBuffer);
    my $EMPTY_HASH := nqp::hash;

    # This is mostly to accommodate grammars, as they do a .new on a
    # potentially already reified object.
    method new(Match:
        :$orig = '',
        str :$name = '',
        int :$from,
        int :to(:$pos),
        :ast(:$made),
        :$shared,
        :$braid,
        :$captures
    --> Match:D) {

        my $new := self.'!cursor_init'(
          $orig,
          :build(nqp::isconcrete(self)),
          :p($pos),
          :$shared,
          :$braid
        );
        nqp::bindattr_s($new,Match,'$!name',$name);
        nqp::bindattr_i($new,Match,'$!from',$from);
        nqp::bindattr($new,Match,'$!made',nqp::decont($made))
          if $made.defined;

        nqp::bindattr($new,Match,'$!match',
          $captures
            ?? nqp::getattr(nqp::decont($captures),Map,'$!storage')
            !! $EMPTY_HASH
        );
          
        $new
    }

    method MATCH() is raw is implementation-detail {
        nqp::ishash($!match)
          ?? self
          !! self!vivify-captures
    }

    method STR() is raw is implementation-detail {
        nqp::ishash($!match)
          ?? self.Str                  # already vivified
          !! self!vivify-captures.Str  # first need to vivify
    }

    method capnames(Match:D:) is raw is implementation-detail {
        nqp::istype($!regexsub,Regex)
          ?? nqp::getattr($!regexsub,Regex,'$!capnames')
          !! Nil
    }

    method captures(Match:D:) is raw is implementation-detail {
        nqp::ishash($!match) ?? $!match !! Nil
    }

    method cstack(Match:D:) is raw is implementation-detail {
        $!cstack ?? $!cstack !! Nil
    }

    method name(Match:D: --> Str:D) {
        nqp::isnull_s($!name) ?? "" !! $!name
    }

    method clone(Match:D:) is raw { nqp::clone(self) }

    # API function for $0, $1 ...
    method AT-POS(int $index) is raw {
        nqp::ifnull(nqp::atkey($!match,$index),Nil)
    }

    # API function for $/[0]:exists ...
    method EXISTS-POS(int $index --> Bool:D) {
        nqp::hllbool(nqp::existskey($!match,$index))
    }

    # Positional API functions that are not supported
    method ASSIGN-POS(int $pos, \val) {
        self!cannot("assign to index $pos")
    }
    method BIND-POS(  int $pos, \val) {
        self!cannot("bind to index $pos")
    }
    method DELETE-POS(int $pos) {
        self!cannot("delete index $pos")
    }

    # API function for $<foo>, $<bar> ...
    method AT-KEY(str $name) is raw {
        nqp::ifnull(nqp::atkey($!match,$name),Nil)
    }

    # API function for $/<foo>:exists ...
    method EXISTS-KEY(str $name --> Bool:D) {
        nqp::hllbool(nqp::existskey($!match,$name))
    }

    # Associative API functions that are not supported
    method ASSIGN-KEY(str $name, \val) {
        self!cannot("assign to key '$name'")
    }
    method BIND-KEY(  str $name, \val) {
        self!cannot("bind to key '$name'")
    }
    method DELETE-KEY(str $name) {
        self!cannot("delete key '$name'")
    }

    # too bad
    method !cannot(str $what) {
        die "Cannot $what from a '{self.^name}' object";
    }

    # Create the captures hash by first looping over all captures,
    # storing them in a list keyed to the name of the capture (which
    # can be either a positional or a named, we don't care).  Then
    # a second pass will check all of the possible captures, and
    # make sure they're properly set up, or initialized properly if
    # absent.
    method !vivify-captures() is raw {
        my $captures := nqp::hash;

        # first pass
        nqp::if(
          $!cstack,
          nqp::stmts(                                # match and captures
            (my int $i = -1),
            nqp::while(                              # perform first pass
              nqp::islt_i(($i = nqp::add_i($i,1)),nqp::elems($!cstack)),
              nqp::if(
                (my $known-as := nqp::getattr_s(
                  (my $match := nqp::atpos($!cstack,$i)),Match,'$!name')),
                nqp::if(
                  nqp::eqat($known-as,'$',0),
                  nqp::bindattr(                     # <( )> handling
                    self,Match,$known-as,nqp::getattr($match,Match,'$!from')
                  ),
                  nqp::stmts(                        # an actual capture
                    # .MATCH as some subcaptures *can* be set up already
                    $match.MATCH,                    # vivify
                    (my $names := nqp::split("=",$known-as)),
                    nqp::while(
                      nqp::elems($names),
                      nqp::if(
                        nqp::islist(my $current := nqp::atkey(
                          $captures,
                          (my str $name = nqp::shift($names))
                        )),
                        nqp::push($current,$match),   # add to existing list
                        nqp::bindkey($captures,$name, # not a list yet
                          nqp::if(
                            nqp::isnull($current),
                            $match,                     # first
                            nqp::list($current,$match)  # second
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        );

        # second pass
        nqp::if(
          nqp::istype($!regexsub,Regex)
            && (my $iter := nqp::iterator(
                 nqp::getattr($!regexsub,Regex,'$!capnames'))),
          nqp::while(
            $iter,
            nqp::if(
              (my str $key = nqp::iterkey_s(nqp::shift($iter)))
                && nqp::not_i(nqp::eqat($key,'$',0)),
              nqp::if(
                nqp::isgt_i(nqp::iterval($iter),1),
                nqp::bindkey($captures,$key,
                  nqp::p6bindattrinvres(                  # multi -> Array
                    nqp::create(Array),List,'$!reified',
                    nqp::if(
                      nqp::islist($current := nqp::ifnull(
                        nqp::atkey($captures,$key),
                        nqp::list
                      )),
                      $current,
                      nqp::list($current)
                    )
                  )
                ),
                nqp::if(                                  # single -> scalar
                  nqp::islist(my $list := nqp::atkey($captures,$key)),
                  nqp::bindkey($captures,$key,nqp::pop($list)) # the last
                )
              )
            )
          )
        );

        # bind the captures, or mark it as not having captures
        nqp::bindattr(self,Match,'$!match',
          nqp::elems($captures) ?? $captures !! $EMPTY_HASH
        );

        # Once we've produced the captures, and if we know we're finished and
        # will never be backtracked into, we can release cstack and regexsub.
        nqp::bindattr(self,Match,'$!cstack',
          nqp::bindattr(self,Match,'$!regexsub',nqp::null)
        ) unless nqp::isconcrete($!bstack);

        self
    }

    proto method Bool(|) {*}  # NQPMatchRole has only "Bool", need proto here
    multi method Bool(Match:U: --> False) { }
    multi method Bool(Match:D:) {
        nqp::hllbool(nqp::isge_i($!pos,$!from))
    }

    # Int is handled by NQPMatchRole
    multi method Numeric(Match:D:) { self.Str.Numeric }
    multi method ACCEPTS(Match:D: Any $) { self }

    # Basically NQP's .Str which gets shadowed by Cool
    method Str(Match:D: --> Str:D) {
        nqp::isge_i($!pos,$!from)
          ?? nqp::substr(
               self.target,
               $!from,
               nqp::sub_i(nqp::if(nqp::islt_i($!to,0),$!pos,$!to)
               ,$!from)
             )
          !! ''
    }

    multi method gist(Match:D: str $indent = "\n " --> Str:D) {
        if nqp::isge_i($!pos,$!from) {     # $!from is good enough here
            my $parts := nqp::list_s;
            nqp::push_s($parts,"=> ") if nqp::isne_s($indent,"\n ");
            nqp::push_s($parts,"｢");
            nqp::push_s($parts,self.Str);
            nqp::push_s($parts,"｣");

            for self.caps {
                nqp::push_s($parts,$indent);
                nqp::push_s($parts,.key.Str);
                nqp::push_s($parts," ");
                nqp::push_s($parts,.value.gist(nqp::concat($indent," ")));
            }

            nqp::join("",$parts);
        }
        else {
            "#<failed match>"
        }
    }

    multi method raku(Match:D: --> Str:D) {
        my $attrs := nqp::list_s;
        nqp::push_s($attrs,
          nqp::concat(':orig(',nqp::concat(self.target.raku,')')));
        nqp::push_s($attrs,
          nqp::concat(':from(',nqp::concat($!from,')')));
        nqp::push_s($attrs,
          nqp::concat(':pos(',nqp::concat($!pos,')')));

        if nqp::isge_i($!pos,$!from) {
            nqp::push_s($attrs,":name<$!name>") if $!name;

            nqp::push_s($attrs,
              nqp::concat(
                ':captures(',
                nqp::concat(
                  nqp::p6bindattrinvres(
                    nqp::create(Map),Map,'$!storage',$!match
                  ).raku,
                  ')'
                )
              )
            ) if nqp::ishash($!match)  # here for debugging without running .MATCH
              && nqp::elems($!match);

            with self.made {
                nqp::push_s($attrs,nqp::concat(':made(',nqp::concat(.raku,')')));
            }
        }

        nqp::concat('Match.new(',nqp::concat(nqp::join(', ',$attrs),')'))
    }

    method prematch(Match:D: --> Str:D) {
        nqp::substr(self.target,0,$!from)
    }
    method postmatch(Match:D: --> Str:D) {
        nqp::substr(self.target,self.to)
    }
    method replace-with(Match:D: Str() $replacement --> Str:D) {
        nqp::concat(
          nqp::substr(self.target,0,$!from),
          nqp::concat(
            $replacement,
            nqp::substr(self.target,self.to)
          )
        )
    }

    # Produce a value for sorting on match position
    method !sort-on-from-pos() is raw {
        nqp::add_i(nqp::bitshiftl_i($!from,32),$!pos)
    }

    # Produce all captures as they were found
    method caps(--> List:D) {
        nqp::if(
          (my $iter := nqp::iterator($!match)),
          nqp::stmts(     # haz captures
            (my $caps := nqp::list),
            nqp::while(
              $iter,
              nqp::stmts(
                (my str $key_s = nqp::iterkey_s(nqp::shift($iter))),
                (my $key := nqp::if(
                  nqp::islt_i(nqp::ord($key_s),58),
                  (my int $ = $key_s),  # numeric
                  $key_s
                )),
                nqp::if(
                  nqp::istype((my $value := nqp::iterval($iter)),Array),
                  nqp::stmts(
                    (my $list :=
                      nqp::clone(nqp::getattr($value,List,'$!reified'))),
                    nqp::while(
                      nqp::elems($list),
                      nqp::push($caps,Pair.new($key,nqp::shift($list)))
                    )
                  ),
                  nqp::push($caps,Pair.new($key,$value))
                )
              )
            ),
            Rakudo::Sorting.MERGESORT-REIFIED-LIST-AS(
              $caps, *.value!sort-on-from-pos
            ) 
          ),
          Empty       # haz no captures
        )
    }

    # Produce all captures as they were found and non-matching strings inbetween
    method chunks(Match:D: --> Seq:D) {
        my int $prev = self.from;
        my $target  := self.target;
        my $buffer  := nqp::create(IterationBuffer);

        for self.caps {
            my \value := .value;
            my int $from = value.from;

            nqp::push(
              $buffer,
              Pair.new(
                '~',
                nqp::substr($target,$prev,nqp::sub_i($from,$prev))
              )
            ) if nqp::isgt_i($from,$prev);  # before match?

            nqp::push($buffer,$_);
            $prev = value.pos;
        }

        nqp::push(
          $buffer,
          Pair.new(
            '~',
            nqp::substr($target,$prev,nqp::sub_i($!pos,$prev))
          )
        ) if nqp::islt_i($prev,$!pos);  # after last match?

        $buffer.Seq
    }

    # Produce an iterator for this Match (pairs)
    method iterator(--> Iterator:D) {
        nqp::if(
          # Historically, the Match iterator first produced all of the
          # positional captures in order, and *then* produced the named
          # captures in random order.  Some internals and some spectests
          # depend on this behaviour, so rather than just producing the
          # captures from the hash, we first need to extract the positional
          # ones in order to be able to produce them first.  Hopefully
          # we can get rid of this rigamarole at some point in the future.
          (my $iter := nqp::iterator($!match)),
          nqp::stmts(               # haz captures
            (my $positionals := nqp::list),
            (my $nameds      := nqp::clone($!match)),
            nqp::while(
              $iter,
              nqp::if(
                nqp::islt_i(
                  nqp::ord(my str $key = nqp::iterkey_s(nqp::shift($iter))),
                  58  # numeric
                ),
                nqp::stmts(
                  nqp::bindpos($positionals,$key,nqp::iterval($iter)),
                  nqp::deletekey($nameds,$key)
                )
              )
            ),
            nqp::if(
              nqp::elems($positionals),
              nqp::if(
                nqp::elems($nameds),
                Rakudo::Iterator.SequentialIterators(   # positional and nameds
                  (Rakudo::Iterator.Pairs(
                    nqp::hllize($positionals).iterator
                   ), nqp::hllize($nameds).iterator
                  ).iterator
                ),
                Rakudo::Iterator.Pairs(                 # only positionals
                  nqp::hllize($positionals).iterator
                ),
              ),
              nqp::hllize($nameds).iterator             # only nameds
            )
          ),
          Rakudo::Iterator.Empty    # haz no captures
        )
    }

    multi method pairs(Match:D: --> Seq:D)     { Seq.new: self.iterator }
    multi method kv(Match:D: --> Seq:D)        { self.pairs.map: { slip(.key, .value) } }
    multi method keys(Match:D: --> Seq:D)      { self.pairs.map: *.key }
    multi method values(Match:D: --> Seq:D)    { self.pairs.map: *.value }
    multi method antipairs(Match:D: --> Seq:D) { self.pairs.map: *.antipair }

    # create an IterationBuffer with positional captures
    method FLATTENABLE_LIST() is raw is implementation-detail {
        nqp::if(
          (my $iter := nqp::iterator($!match)),
          nqp::stmts(      # haz captures
            (my $buffer := nqp::create(IterationBuffer)),
            nqp::while(
              $iter,
              nqp::if(
                nqp::islt_i(
                  nqp::ord(my str $key = nqp::iterkey_s(nqp::shift($iter))),
                  58  # numeric
                ),
                nqp::bindpos($buffer,$key,nqp::iterval($iter))
              )
            ),
            $buffer
          ),
          $EMPTY_LIST      # haz no captures
        )
    }

    # create an nqp::hash with named captures
    method FLATTENABLE_HASH() is raw is implementation-detail {
        nqp::if(
          (my $iter := nqp::iterator($!match)),
          nqp::stmts(      # haz captures
            (my $hash := nqp::hash),
            nqp::while(
              $iter,
              nqp::if(
                nqp::isge_i(
                  nqp::ord(my str $key = nqp::iterkey_s(nqp::shift($iter))),
                  58
                ),
                nqp::bindkey($hash,$key,nqp::iterval($iter))
              )
            ),
            $hash
          ),
          $EMPTY_HASH      # haz no captures
        )
    }

    # produce a list with positional captures
    multi method list(Match:D: --> List:D) {
        nqp::p6bindattrinvres(
          nqp::create(List),List,'$!reified',self.FLATTENABLE_LIST
        )
    }

    multi method elems(Match:D: --> Int:D) {
        nqp::if(
          (my $iter := nqp::iterator($!match)),
          nqp::stmts(    # haz captures
            (my int $elems),
            nqp::while(
              $iter,
              nqp::if(
                nqp::islt_i(
                  nqp::ord(my str $key = nqp::iterkey_s(nqp::shift($iter))),
                  58  # numeric
                ),
                ($elems = nqp::add_i($elems,1))
              )
            ),
            $elems
          ),
          0              # haz no captures
        )
    }

    multi method head(Match:D:) { self.AT-POS(0) }
    multi method head(Match:D: $head) {
        $head == 1
          ?? self.AT-POS(0)
          !! self.list.head($head)
    }
    multi method tail(Match:D:) {
        nqp::elems(my \buffer := self.FLATTENABLE_LIST)
          ?? nqp::atpos(buffer,-1)  # quick way to get the last
          !! Nil
    }
    multi method tail(Match:D: $tail) {
        self.list.tail($tail)
    }

    # produce a Map (for historical reasons) with named captures
    method hash(--> Map:D) {
        nqp::p6bindattrinvres(
          nqp::create(Map),Map,'$!storage',
          nqp::getattr(self.FLATTENABLE_HASH,Map,'$!storage')
        )
    }


#?if js
    my sub move_cursor($target, $pos) {
       nqp::chars(nqp::substrnfg(nqp::substr($target, $pos), 0, 1)) || 1;
    }
#?endif

    # adapted from !cursor_more in nqp, used for :g matching
    method CURSOR_MORE() is raw is implementation-detail {
        my $new := nqp::create(self);
        nqp::bindattr(  $new,Match,'$!shared',$!shared);
        nqp::bindattr(  $new,Match,'$!braid',$!braid);
        nqp::bindattr_i($new,Match,'$!from',
          nqp::bindattr_i($new,Match,'$!to',-1));
        nqp::bindattr_i($new,Match,'$!pos',nqp::isge_i($!from,$!pos)
#?if !js
          ?? nqp::add_i($!from,1)
#?endif
#?if js
          ?? nqp::add_i($!from, move_cursor(self.target, $!pos))
#?endif
          !! $!pos);
        $!regexsub($new)
    }

    # adapted from !cursor_more in nqp, used for :ov matching
    method CURSOR_OVERLAP() is raw is implementation-detail {
        my $new := nqp::create(self);
        nqp::bindattr(  $new,Match,'$!shared',$!shared);
        nqp::bindattr(  $new,Match,'$!braid',$!braid);
        nqp::bindattr_i($new,Match,'$!from',
          nqp::bindattr_i($new,Match,'$!to',-1));
        nqp::bindattr_i($new,Match,'$!pos',nqp::add_i($!from,1));
        $!regexsub($new)
    }

    # from !cursor_next in nqp, used for :ex matching
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

    # method "made" is also known as "ast"
    BEGIN Match.^add_method("ast",
      # When nothing's `made`, we get an NQPMu that we'd like to replace
      # with Nil; all Rakudo objects typecheck as Mu, while NQPMu doesn't
      method made() { nqp::istype($!made,Mu) ?? $!made !! Nil }
    );
}

multi sub infix:<eqv>(Match:D \a, Match:D \b) {
    nqp::hllbool(
      nqp::eqaddr(nqp::decont(a),nqp::decont(b))
        || (nqp::iseq_i(
              nqp::getattr_i(nqp::decont(a),Match,'$!pos'),
              nqp::getattr_i(nqp::decont(b),Match,'$!pos')
            ) && nqp::iseq_s(a.target,b.target)
              && nqp::iseq_i(a.from,b.from)
              && a.made eqv b.made
              && a.captures eqv b.captures
           )
    )
}

sub make(Mu \made) {
    nqp::bindattr(nqp::decont(nqp::getlexcaller('$/')),Match,'$!made',made)
}

# vim: expandtab shiftwidth=4
