my class Match is Cool does NQPMatchRole {
    has $!captures;

    method MATCH() { self }
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
#    has $!match;     # flag indicating Match object set up (NQPdidMATCH)
#    has str $!name;  # name if named capture

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
        :$cstack,
        :$capnames
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

        nqp::bindattr($new,Match,'$!cstack',
          nqp::getattr(nqp::decont($cstack),List,'$!reified')
        ) if $cstack;

        nqp::bindattr(
          nqp::bindattr($new,Match,'$!regexsub',nqp::create(Regex)),
          Regex,
          '$!capnames',
          nqp::getattr(nqp::decont($capnames),Map,'$!storage')
        ) if $capnames;
          
        $new
    }

    method from() is raw {
        self!captures if $!cstack && nqp::isfalse($!captures);
        $!from
    }
    method to() is raw {
        self!captures if $!cstack && nqp::isfalse($!captures);
        nqp::islt_i($!to,0) ?? $!pos !! $!to
    }

    method capnames(Match:D:) is raw is implementation-detail {
        nqp::istype($!regexsub,Regex)
          ?? nqp::getattr($!regexsub,Regex,'$!capnames')
          !! Nil
    }

    method cstack(Match:D:) {
        nqp::isconcrete($!cstack) ?? $!cstack !! Nil
    }

    method name(Match:D: --> Str:D) {
        nqp::isnull_s($!name) ?? "" !! $!name
    }

    method clone(Match:D:) is raw { nqp::clone(self) }

    # API function for $0, $1 ...
    method AT-POS(int $index) {
        nqp::ifnull(nqp::atkey(self!captures,$index),Nil)
    }

    # API function for $/[0]:exists ...
    method EXISTS-POS(int $index --> Bool:D) {
        nqp::hllbool(nqp::existskey(self!captures,$index))
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
        nqp::ifnull(nqp::atkey(self!captures,$name),Nil)
    }

    # API function for $/<foo>:exists ...
    method EXISTS-KEY(str $name --> Bool:D) {
        nqp::hllbool(nqp::existskey(self!captures,$name))
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

    # return vivified $!captures attribute
    method !captures() is raw {
        nqp::isconcrete($!captures)
          ?? $!captures
          !! self!vivify-captures
    }

    # Create the captures hash by first looping over all captures,
    # storing them in a list keyed to the name of the capture (which
    # can be either a positional or a named, we don't care).  Then
    # a second pass will check all of the possible captures, and
    # make sure they're properly set up, or initialized properly if
    # absent.
    method !vivify-captures() is raw {
        my $captures := nqp::bindattr(self,Match,'$!captures',nqp::hash);

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
          nqp::istype($!regexsub,Regex),
          nqp::stmts(
            (my $iter :=
              nqp::iterator(nqp::getattr($!regexsub,Regex,'$!capnames'))),
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
          )
        );

        $captures
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
        self!captures if $!cstack && nqp::isfalse($!captures);

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
        self!captures if $!cstack && nqp::isfalse($!captures);

        my $attrs := nqp::list_s;
        nqp::push_s($attrs,
          nqp::concat(':orig(',nqp::concat(self.target.raku,')')));
        nqp::push_s($attrs,
          nqp::concat(':from(',nqp::concat($!from,')')));
        nqp::push_s($attrs,
          nqp::concat(':pos(',nqp::concat($!pos,')')));

        if nqp::isge_i($!pos,$!from) {
            nqp::push_s($attrs,":name<$!name>") if $!name;

            if $!cstack {
                nqp::push_s($attrs,
                  nqp::concat(
                    ':cstack',
                    nqp::p6bindattrinvres(
                      nqp::create(List),List,'$!reified',$!cstack
                    ).raku
                  )
                );
                nqp::push_s($attrs,
                  nqp::concat(
                    ':capnames(',
                    nqp::concat(
                      nqp::p6bindattrinvres(
                        nqp::create(Map),Map,'$!storage',
                        nqp::getattr($!regexsub,Regex,'$!capnames')
                      ).raku,
                      ')'
                    )
                  )
                );
            }
            with self.made {
                nqp::push_s($attrs,nqp::concat(':made(',nqp::concat(.raku,')')));
            }
        }

        nqp::concat('Match.new(',nqp::concat(nqp::join(', ',$attrs),')'))
    }

    method prematch(Match:D: --> Str:D) {
        nqp::substr(self.target,0,self.from)
    }
    method postmatch(Match:D: --> Str:D) {
        nqp::substr(self.target,self.to)
    }
    method replace-with(Match:D: Str() $replacement --> Str:D) {
        nqp::concat(
          nqp::substr(self.target,0,self.from),
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
        if nqp::elems(self!captures) {
            my $caps := nqp::list;
            my $iter := nqp::iterator($!captures);

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
            );

            Rakudo::Sorting.MERGESORT-REIFIED-LIST-AS(
              $caps, *.value!sort-on-from-pos
            ) 
        }
        else {
            Empty
        }
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
        my $positionals := nqp::list;
        my $nameds :=
          nqp::clone(nqp::getattr(self!captures,Map,'$!storage'));

        # Historically, the Match iterator first produced all of the
        # positional captures in order, and *then* produced the named
        # captures in random order.  Some internals and some spectests
        # depend on this behaviour, so rather than just producing the
        # captures from the hash, we first need to extract the positional
        # ones in order to be able to produce them first.  Hopefully
        # we can get rid of this rigamarole at some point in the future.
        my $iter := nqp::iterator($nameds);
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
        );

        # return appropriate iterator, potentially combined
        nqp::elems($positionals)
          ?? nqp::elems($nameds)
            ?? Rakudo::Iterator.SequentialIterators:
                 (Rakudo::Iterator.Pairs(nqp::hllize($positionals).iterator),
                  nqp::hllize($nameds).iterator
                 ).iterator
            !! Rakudo::Iterator.Pairs(nqp::hllize($positionals).iterator)
          !! nqp::elems($nameds)
            ?? nqp::hllize($nameds).iterator
            !! Rakudo::Iterator.Empty
    }

    multi method pairs(Match:D: --> Seq:D)     { Seq.new: self.iterator }
    multi method kv(Match:D: --> Seq:D)        { self.pairs.map: { slip(.key, .value) } }
    multi method keys(Match:D: --> Seq:D)      { self.pairs.map: *.key }
    multi method values(Match:D: --> Seq:D)    { self.pairs.map: *.value }
    multi method antipairs(Match:D: --> Seq:D) { self.pairs.map: *.antipair }

    # create an IterationBuffer with positional captures
    method FLATTENABLE_LIST() is raw is implementation-detail {
        my $buffer := nqp::create(IterationBuffer);

        my $iter := nqp::iterator(nqp::getattr(self!captures,Map,'$!storage'));
        nqp::while(
          $iter,
          nqp::if(
            nqp::islt_i(
              nqp::ord(my str $key = nqp::iterkey_s(nqp::shift($iter))),
              58  # numeric
            ),
            nqp::bindpos($buffer,$key,nqp::iterval($iter))
          )
        );

        $buffer
    }

    # create an nqp::hash with named captures
    method FLATTENABLE_HASH() is raw is implementation-detail {
        my $hash := nqp::hash;

        my $iter := nqp::iterator(nqp::getattr(self!captures,Map,'$!storage'));
        nqp::while(
          $iter,
          nqp::if(
            nqp::isge_i(
              nqp::ord(my str $key = nqp::iterkey_s(nqp::shift($iter))),
              58
            ),
            nqp::bindkey($hash,$key,nqp::iterval($iter))
          )
        );

        $hash
    }

    # produce a list with positional captures
    multi method list(Match:D: --> List:D) {
        nqp::p6bindattrinvres(
          nqp::create(List),List,'$!reified',self.FLATTENABLE_LIST
        )
    }

    multi method elems(Match:D: --> Int:D) {
        my int $elems;

        my $iter := nqp::iterator(nqp::getattr(self!captures,Map,'$!storage'));
        nqp::while(
          $iter,
          nqp::if(
            nqp::islt_i(
              nqp::ord(my str $key = nqp::iterkey_s(nqp::shift($iter))),
              58  # numeric
            ),
            ($elems = nqp::add_i($elems,1))
          )
        );

        $elems
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

    # produce a hash with named captures
    method hash(--> Map:D) { self.FLATTENABLE_HASH }  # auto-upgrades

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
              && a.cstack eqv b.cstack
           )
    )
}

sub make(Mu \made) {
    nqp::bindattr(nqp::decont(nqp::getlexcaller('$/')),Match,'$!made',made)
}

# vim: expandtab shiftwidth=4
