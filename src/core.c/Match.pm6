my class Match is Cool does NQPMatchRole {
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

    method capture-from() { self!deeper('$!from') }
    method capture-to() {
        my int $to = self!deeper('$!to');
        nqp::islt_i($to,0) ?? $!pos !! $to
    }

    # look for a capture for the given special name
    method !deeper(str $name) {
        my int $pos = nqp::getattr_i(self,Match,$name);

        # try to find capture with special name, can have multiple
        nqp::if(
          $!cstack,
          nqp::stmts(
            (my int $i = -1),
            nqp::while(
              nqp::islt_i(($i = nqp::add_i($i,1)),nqp::elems($!cstack)),
              nqp::if(
                (my $found :=
                  nqp::getattr_s(nqp::atpos($!cstack,$i),Match,'$!name'))
                  && nqp::iseq_s($found,$name),
                ($pos = nqp::getattr_i(
                  nqp::atpos($!cstack,$i)!deeper-still($name),
                  Match,
                  '$!from'
                ))
              )
            )
          )
        );

        $pos
    }

    # recursively look for a capture for the given special name
    method !deeper-still(str $name) {

        # try to find capture with special name
        nqp::if(
          $!cstack,
          nqp::stmts(
            (my int $i = -1),
            nqp::while(
              nqp::islt_i(($i = nqp::add_i($i,1)),nqp::elems($!cstack)),
              nqp::if(
                (my $found :=
                  nqp::getattr_s(nqp::atpos($!cstack,$i),Match,'$!name'))
                  && nqp::iseq_s($found,$name),
                (return nqp::atpos($!cstack,$i)!deeper-still($name))
              )
            )
          )
        );

        # not found, return object for later $!from extraction
        self
    }

    method capnames(Match:D:) is raw is implementation-detail {
        nqp::isconcrete($!regexsub)
          ?? nqp::getattr($!regexsub,Regex,'$!capnames')
          !! Nil
    }

    method cstack(Match:D:) { nqp::isconcrete($!cstack) ?? $!cstack !! Nil }
    method name(Match:D: --> Str:D) {
        nqp::isnull_s($!name) ?? "" !! $!name
    }

    method clone(Match:D:) is raw { nqp::clone(self) }

    # API function for $0, $1 ...
    method AT-POS(int $index) {
        nqp::if(
          nqp::isge_i($!pos,$!from)              # $!from is good enough here
            && (my int $max-captures = nqp::ifnull(
                 nqp::atkey(nqp::getattr($!regexsub,Regex,'$!capnames'),$index),
                 0
               )),
          nqp::if(                               # could be found
            nqp::isgt_i($max-captures,1),
            self!find-multi(my str $ = $index),  # multiple captures possible
            self!find-single(my str $ = $index)  # only single capture possble
          ),
          Nil                                    # no match / can not be found
        )
    }

    # API function for $/[0]:exists ...
    method EXISTS-POS(int $pos --> Bool:D) {
        # $!from is good enough here
        nqp::hllbool(nqp::isge_i($!pos,$!from) && self!exists(my str $ = $pos))
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
    method AT-KEY(str $name) {
        nqp::if(
          nqp::isge_i($!pos,$!from)          # $!from is good enough here
            && (my int $max-captures = nqp::ifnull(
                 nqp::atkey(nqp::getattr($!regexsub,Regex,'$!capnames'),$name),
                 0
               )),
          nqp::if(                           # could be found
            nqp::isgt_i($max-captures,1),
            self!find-multi($name),          # multiple captures possible
            self!find-single($name)          # only single capture possble
          ),
          Nil                                # no match / can not be found
        )
    }

    # API function for $/<foo>:exists ...
    method EXISTS-KEY(str $name --> Bool:D) {
        # $!from is good enough here
        nqp::hllbool(nqp::isge_i($!pos,$!from) && self!exists($name))
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

    # check for existence of a capture given by name
    method !exists(str $name) {
        nqp::if(
          $!cstack,
          nqp::stmts(
            (my int $i = -1),
            nqp::while(
              nqp::islt_i(($i = nqp::add_i($i,1)),nqp::elems($!cstack)),
              nqp::if(
                (my $known-as :=
                  nqp::getattr_s(nqp::atpos($!cstack,$i),Match,'$!name')),
                nqp::stmts(
                  (my $names := nqp::split("=",$known-as)),
                  nqp::while(
                    nqp::elems($names),
                    nqp::if(
                      nqp::iseq_s($name,nqp::shift($names)),
                      (return 1)
                    )
                  )
                )
              )
            )
          )
        );

        # check for multiple capture
        nqp::isge_i(
          nqp::ifnull(
            nqp::atkey(nqp::getattr($!regexsub,Regex,'$!capnames'),$name),
            1
          ),
          2
        )
    }

    # find a single capture like (.), knowing there are captures
    method !find-single(str $name) {
        my $result := Nil;

        # we haz captures
        nqp::if(
          $!cstack,
          nqp::stmts(
            (my int $i = -1),
            nqp::while(
              nqp::islt_i(($i = nqp::add_i($i,1)),nqp::elems($!cstack)),
              nqp::if(
                (my $known-as :=
                  nqp::getattr_s(nqp::atpos($!cstack,$i),Match,'$!name')),
                nqp::stmts(
                  (my $names := nqp::split("=",$known-as)),
                  nqp::while(
                    nqp::elems($names),
                    nqp::if(
                      nqp::iseq_s($name,nqp::shift($names)),
                      ($result := nqp::atpos($!cstack,$i))
                    )
                  )
                )
              )
            )
          )
        );

        $result
    }

    # find a multi capture, like (.)+, knowing there are captures
    method !find-multi(str $name) {
        my @captures;

        nqp::if(
          $!cstack,
          nqp::stmts(
            (my $captures :=
              nqp::bindattr(@captures,List,'$!reified',
                nqp::create(IterationBuffer))
            ),
            (my int $i = -1),
            nqp::while(
              nqp::islt_i(($i = nqp::add_i($i,1)),nqp::elems($!cstack)),
              nqp::if(
                (my $known-as :=
                  nqp::getattr_s(nqp::atpos($!cstack,$i),Match,'$!name')),
                nqp::stmts(
                  (my $names := nqp::split("=",$known-as)),
                  nqp::while(
                    nqp::elems($names),
                    nqp::if(
                      nqp::iseq_s($name,nqp::shift($names)),
                      nqp::push($captures,nqp::atpos($!cstack,$i))
                    )
                  )
                )
              )
            )
          )
        );

        @captures   # ecosystem expects an Array
    }

    proto method Bool(|) {*}  # NQPMatchRole has only "Bool", need proto here
    multi method Bool(Match:U: --> False) { }
    multi method Bool(Match:D:) {
        nqp::hllbool(nqp::isge_i($!pos,$!from))  # $!from is good enough here
    }

    # Int is handled by NQPMatchRole
    multi method Numeric(Match:D:) { self.Str.Numeric }
    multi method ACCEPTS(Match:D: Any $) { self }

    # Basically NQP's .Str which gets shadowed by Cool
    method Str(Match:D: --> Str:D) {
        my int $from = self.capture-from;  # must use capture form here
        nqp::isge_i($!pos,$from)
          ?? nqp::substr(self.target,$from,nqp::sub_i(self.capture-to,$from))
          !! ''
    }

    multi method gist(Match:D: str $indent = "\n " --> Str:D) {
        if nqp::isge_i($!pos,$!from) {  # $!from is good enough here
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

        if nqp::isge_i($!pos,$!from) {  # $!from is good enough here
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
        nqp::substr(self.target,0,self.capture-from)
    }
    method postmatch(Match:D: --> Str:D) {
        nqp::substr(self.target,self.capture-to)
    }
    method replace-with(Match:D: Str() $replacement --> Str:D) {
        nqp::concat(
          nqp::substr(self.target,0,self.capture-from),
          nqp::concat(
            $replacement,
            nqp::substr(self.target,self.capture-to)
          )
        )
    }

    # Produce a value for sorting on match position
    method !sort-on-from-pos() {
        nqp::add_i(nqp::bitshiftl_i(self.capture-from,32),$!pos)
    }

    # Produce all captures as they were found
    method caps(--> List:D) {
        if $!cstack {
            my $caps := nqp::list;
            my int $i = -1;

            nqp::while(
              nqp::islt_i(($i = nqp::add_i($i,1)),nqp::elems($!cstack)),
              nqp::if(
                (my $known-as := nqp::getattr_s(
                  (my $match := nqp::atpos($!cstack,$i)),Match,'$!name')),
                nqp::stmts(
                  (my $names := nqp::split("=",$known-as)),
                  nqp::while(
                    nqp::elems($names),
                    nqp::if(
                      nqp::isne_i(
                        nqp::ord(my str $key = nqp::shift($names)),
                        36    # not a $, assume not $!from or $!to
                      ),
                      nqp::push($caps,Pair.new(
                        nqp::if(
                          nqp::islt_i(nqp::ord($key),58),  # numeric
                          (my int $ = $key),  # converts to Int as key
                          $key
                        ),
                        $match
                      ))
                    )
                  )
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
        my int $prev = self.capture-from;
        my $target  := self.target;
        my $buffer  := nqp::create(IterationBuffer);

        for self.caps {
            my \value := .value;
            my int $from = value.capture-from;

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
        my $nameds      := nqp::hash;

        # there appear to be captures
        if $!cstack {

            # pass 1: put all captures in the hash
            my int $i = -1;

            nqp::while(
              nqp::islt_i(($i = nqp::add_i($i,1)),nqp::elems($!cstack)),
              nqp::if(
                (my $known-as :=
                  nqp::getattr_s((my $match :=
                    nqp::atpos($!cstack,$i)),Match,'$!name')
                ) && nqp::isne_i(nqp::ord($known-as),36),  # not a $
                nqp::stmts(
                  (my $names := nqp::split("=",$known-as)),
                  nqp::while(
                    nqp::elems($names),
                    nqp::stmts(
                      (my str $key = nqp::shift($names)),
                      nqp::if(
                        nqp::isnull(my $current := nqp::atkey($nameds,$key)),
                        nqp::bindkey($nameds,$key,$match),
                        nqp::if(
                          nqp::islist($current),
                          nqp::push($current,$match),
                          nqp::bindkey($nameds,$key,nqp::list($current,$match))
                        )
                      )
                    )
                  )
                )
              )
            );
        }

        # some regex action has been done
        if nqp::isconcrete($!regexsub) {

            # pass 2: make sure all multi captures are Arrays
            my $iter :=
              nqp::iterator(nqp::getattr($!regexsub,Regex,'$!capnames'));
            nqp::while(
              $iter,
              nqp::if(
                nqp::iseq_i(nqp::iterval(nqp::shift($iter)),2),
                nqp::bindkey($nameds,nqp::iterkey_s($iter),nqp::if(
                  nqp::isnull(my $current := nqp::atkey($nameds,nqp::iterkey_s($iter))),
                  nqp::create(Array),
                  nqp::p6bindattrinvres(
                    nqp::create(Array),List,'$!reified',nqp::if(
                      nqp::islist($current),
                      $current,
                      nqp::list($current)
                    )
                  )
                ))
              )
            );
        }

        # Historically, the Match iterator first produced all of the
        # positional captures in order, and *then* produced the named
        # captures in random order.  Some internals and some spectests
        # depend on this behaviour, so rather than just producing the
        # captures from the hash, we first need to extract the positional
        # ones in order to be able to produce them first.  Hopefully
        # we can get rid of this rigamarole at some point in the future.

        # pass 3: extract the positional captures into an array
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

        # we haz captures
        if $!cstack {

            # pass 1: put all positional captures in the list
            my int $i = -1;

            nqp::while(
              nqp::islt_i(($i = nqp::add_i($i,1)),nqp::elems($!cstack)),
              nqp::if(
                (my $known-as := nqp::getattr_s(
                  (my $match := nqp::atpos($!cstack,$i)),Match,'$!name')
                ),
                nqp::stmts(
                  (my $names := nqp::split("=",$known-as)),
                  nqp::while(
                    nqp::elems($names),
                    nqp::stmts(
                      (my str $pos = nqp::shift($names)),
                      nqp::if(
                        nqp::islt_i(nqp::ord($pos),58)       # probably numeric
                          && nqp::isne_i(nqp::ord($pos),36), # not a $
                        nqp::if(                             # numeric!
                          nqp::isnull(my $current := nqp::atpos($buffer,$pos)),
                          nqp::bindpos($buffer,$pos,$match),
                          nqp::if(
                            nqp::islist($current),
                            nqp::push($current,$match),
                            nqp::bindpos($buffer,$pos,nqp::list($current,$match))
                          )
                        )
                      )
                    )
                  )
                )
              )
            );
        }

        # had some regex action
        if nqp::isconcrete($!regexsub) {

            # pass 2: make sure all multi captures are Arrays
            my $iter :=
              nqp::iterator(nqp::getattr($!regexsub,Regex,'$!capnames'));
            nqp::while(
              $iter,
              nqp::if(
                nqp::isgt_i(nqp::iterval(nqp::shift($iter)),1),
                nqp::if(
                  (my str $pos = nqp::iterkey_s($iter)) # not empty
                    && nqp::islt_i(nqp::ord($pos),58)   # probably numeric
                    && nqp::isne_i(nqp::ord($pos),36),  # not a $
                  nqp::bindpos($buffer,$pos,nqp::if(
                    nqp::isnull(my $current := nqp::atpos($buffer,$pos)),
                    nqp::create(Array),
                    nqp::p6bindattrinvres(
                      nqp::create(Array),List,'$!reified',nqp::if(
                        nqp::islist($current),
                        $current,
                        nqp::list($current)
                      )
                    )
                  ))
                )
              )
            );
        }

        $buffer
    }

    # produce a list with positional captures
    multi method list(Match:D: --> List:D) { self.FLATTENABLE_LIST.List }
    multi method elems(Match:D: --> Int:D) { nqp::elems(self.FLATTENABLE_LIST) }
    multi method head(Match:D:) { self.AT-POS(0) }
    multi method head(Match:D: $head) {
        $head == 1
          ?? self.AT-POS(0)
          !! self.list.head($head)
    }
    multi method tail(Match:D:) {
        nqp::elems(my \buffer := self.FLATTENABLE_LIST)
          ?? nqp::atpos(buffer,nqp::sub_i(nqp::elems(buffer),1))
          !! Nil
    }
    multi method tail(Match:D: $tail) {
        self.list.tail($tail)
    }

    # produce a hash with named captures
    method hash(--> Map:D) {
        my $hash := nqp::hash;

        # we haz captures
        if $!cstack {

            # pass 1: put all positional captures in the list
            my int $i = -1;

            nqp::while(
              nqp::islt_i(($i = nqp::add_i($i,1)),nqp::elems($!cstack)),
              nqp::if(
                (my $known-as := nqp::getattr_s(
                  (my $match := nqp::atpos($!cstack,$i)),Match,'$!name')
                ),
                nqp::stmts(
                  (my $names := nqp::split("=",$known-as)),
                  nqp::while(
                    nqp::elems($names),
                    nqp::if(
                      nqp::isge_i(
                        nqp::ord(my str $key = nqp::shift($names)),
                        58                 # not numeric nor $
                      ),
                      nqp::if(
                        nqp::isnull(my $current := nqp::atkey($hash,$key)),
                        nqp::bindkey($hash,$key,$match),
                        nqp::if(
                          nqp::islist($current),
                          nqp::push($current,$match),
                          nqp::bindkey($hash,$key,nqp::list($current,$match))
                        )
                      )
                    )
                  )
                )
              )
            );
        }

        # had some regex action
        if nqp::isconcrete($!regexsub) {

            # pass 2: make sure all multi captures are Arrays
            my $iter :=
              nqp::iterator(nqp::getattr($!regexsub,Regex,'$!capnames'));
            nqp::while(
              $iter,
              nqp::if(
                nqp::iseq_i(nqp::iterval(nqp::shift($iter)),2),
                nqp::if(
                  (my str $key = nqp::iterkey_s($iter)) # not empty
                    && nqp::isge_i(nqp::ord($key),58),  # not numeric nor $
                  nqp::bindkey($hash,$key,nqp::if(
                    nqp::isnull(my $current := nqp::atkey($hash,$key)),
                    nqp::create(Array),
                    nqp::p6bindattrinvres(
                      nqp::create(Array),List,'$!reified',nqp::if(
                        nqp::islist($current),
                        $current,
                        nqp::list($current)
                      )
                    )
                  ))
                )
              )
            );
        }

        nqp::p6bindattrinvres(nqp::create(Map),Map,'$!storage',$hash)
    }
    method FLATTENABLE_HASH() is raw is implementation-detail {
        nqp::getattr(self.hash,Map,'$!storage')
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
            ) && nqp::iseq_i(
                   nqp::getattr_i(nqp::decont(a),Match,'$!from'),
                   nqp::getattr_i(nqp::decont(b),Match,'$!from')
                 )
              && nqp::iseq_s(a.target,b.target)
              && a.made eqv b.made
              && a.cstack eqv b.cstack
           )
    )
}

sub make(Mu \made) {
    nqp::bindattr(nqp::decont(nqp::getlexcaller('$/')),Match,'$!made',made)
}

# vim: expandtab shiftwidth=4
