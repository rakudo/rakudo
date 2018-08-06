my class Range  { ... }
my class Match  { ... }
my class X::Cannot::Capture      { ... }
my class X::Str::InvalidCharName { ... }
my class X::Str::Numeric  { ... }
my class X::Str::Match::x { ... }
my class X::Str::Subst::Adverb { ... }
my class X::Str::Trans::IllegalKey { ... }
my class X::Str::Trans::InvalidArg { ... }
my class X::Numeric::Confused { ... }
my class X::Syntax::Number::RadixOutOfRange { ... }

my constant $?TABSTOP = 8;

my class Str does Stringy { # declared in BOOTSTRAP
    # class Str is Cool
    #     has str $!value is box_target;

    my $empty := nqp::list;   # for nqp::splice

    multi method WHY('Life, the Universe and Everything': --> 42) { }

    multi method WHICH(Str:D:) {
        nqp::box_s(
          nqp::concat(
            nqp::if(
              nqp::eqaddr(self.WHAT,Str),
              'Str|',
              nqp::concat(nqp::unbox_s(self.^name), '|')
            ),
            $!value
          ),
          ValueObjAt
        )
    }
    submethod BUILD(Str() :$value = '' --> Nil) {
        nqp::bindattr_s(self, Str, '$!value', nqp::unbox_s($value))
    }

    multi method Bool(Str:D:) {
        nqp::p6bool(nqp::chars($!value));
    }
    method Capture() { die X::Cannot::Capture.new: :what(self) }

    multi method Str(Str:D:)     { self }
    multi method Stringy(Str:D:) { self }
    multi method DUMP(Str:D:) { self.perl }

    method Int(Str:D:) {
        nqp::if(
          nqp::isge_i(
            nqp::findnotcclass(
              nqp::const::CCLASS_NUMERIC,$!value,0,nqp::chars($!value)),
            nqp::chars($!value)
          )
#?if moar
            # Compare Str.chars == Str.codes to filter out any combining characters
            && nqp::iseq_i(
                nqp::chars($!value),
                nqp::codes($!value)
            )
#?endif
#?if jvm
            # RT #128542: https://rt.perl.org/Public/Bug/Display.html?id=128542
            # Needs Str.codes impl that doesn't just return chars
#?endif
          ,
          nqp::atpos(nqp::radix_I(10,$!value,0,0,Int),0),  # all numeric chars
          nqp::if(
            nqp::istype((my $numeric := self.Numeric),Failure),
            $numeric,
            $numeric.Int
          )
        )
    }
    method Num(Str:D:) {
        nqp::if(
            nqp::istype((my $numeric := self.Numeric),Failure),
            $numeric,
            $numeric.Num || nqp::if(
                # handle sign of zero. While self.Numeric will give correctly
                # signed zero for nums in strings, it won't for other types,
                # and since this method is `Num` we want to return proper zero.
                # Find first non-whitespace char and check whether it is one
                # of the minuses.
                nqp::chars(self)
                && (
                    nqp::iseq_i(
                        (my $ch := nqp::ord(
                            nqp::substr(
                                self,
                                nqp::findnotcclass(
                                    nqp::const::CCLASS_WHITESPACE, self, 0,
                                    nqp::sub_i(nqp::chars(self), 1)
                                ),
                                1,
                            )
                        )),
                        45, # '-' minus
                    ) || nqp::iseq_i($ch, 8722) # '−' minus
                ),
                -0e0,
                 0e0
            )
        )
    }

    multi method ACCEPTS(Str:D: Str:D \other) {
        nqp::p6bool(nqp::iseq_s(nqp::unbox_s(other),$!value));
    }
    multi method ACCEPTS(Str:D: Any:D \other) {
        nqp::p6bool(nqp::iseq_s(nqp::unbox_s(other.Str),$!value));
    }

    method chomp(Str:D:) {
        nqp::if(
          (nqp::isge_i((my int $chars = nqp::sub_i(nqp::chars($!value),1)),0)
            && nqp::iscclass(nqp::const::CCLASS_NEWLINE,$!value,$chars)),
          nqp::p6box_s(nqp::substr($!value,0,$chars)),
          self
        )
    }

    multi method chop(Str:D:) {
        nqp::if(
          nqp::isgt_i(nqp::chars($!value),0),
          nqp::p6box_s(
            nqp::substr($!value,0,nqp::sub_i(nqp::chars($!value),1))),
          ''
        )
    }
    multi method chop(Str:D: Int() $chopping) {
        my Int $chars = nqp::chars($!value) - $chopping;
        $chars > 0 ?? nqp::p6box_s(nqp::substr($!value,0,$chars)) !! '';
    }

    # TODO Use coercer in 1 candidate when RT131014
    proto method starts-with(|) {*}
    multi method starts-with(Str:D: Cool:D $needle) {self.starts-with: $needle.Str}
    multi method starts-with(Str:D: Str:D $needle) {
        nqp::p6bool(nqp::eqat(self, $needle, 0))
    }

    # TODO Use coercer in 1 candidate when RT131014
    proto method ends-with(|) {*}
    multi method ends-with(Str:D: Cool:D $suffix) {self.ends-with: $suffix.Str}
    multi method ends-with(Str:D: Str:D $suffix) {
        nqp::p6bool(nqp::eqat(
          $!value,
          nqp::getattr($suffix,Str,'$!value'),
          nqp::chars($!value) - nqp::chars(nqp::getattr($suffix,Str,'$!value'))
        ))
    }

    # TODO Use coercer in 1 candidate when RT131014
    proto method substr-eq(|) {*}
    multi method substr-eq(Str:D: Cool:D $needle) {self.substr-eq: $needle.Str}
    multi method substr-eq(Str:D: Str:D $needle) {
        nqp::p6bool(nqp::eqat($!value,nqp::getattr($needle,Str,'$!value'),0))
    }
    multi method substr-eq(Str:D: Cool:D $needle, Int:D $pos) {self.substr-eq: $needle.Str, $pos.Int}
    multi method substr-eq(Str:D: Str:D $needle, Int:D $pos) {
        nqp::p6bool(
          nqp::if(
            (nqp::isge_i($pos,0) && nqp::islt_i($pos,nqp::chars($!value))),
            nqp::eqat($!value,nqp::getattr($needle,Str,'$!value'),$pos)
          )
        )
    }

    # TODO Use coercer in 1 candidate when RT131014
    proto method contains(|) {*}
    multi method contains(Str:D: Cool:D $needle) {self.contains: $needle.Str}
    multi method contains(Str:D: Str:D $needle) {
        nqp::p6bool(nqp::isne_i(
          nqp::index($!value,nqp::getattr($needle,Str,'$!value'),0),-1
        ))
    }
    multi method contains(Str:D: Cool:D $needle, Int(Cool:D) $pos) {self.contains: $needle.Str, $pos}
    multi method contains(Str:D: Str:D $needle, Int:D $pos) {
        nqp::p6bool(
          nqp::if(
            (nqp::isge_i($pos,0) && nqp::islt_i($pos,nqp::chars($!value))),
            nqp::isne_i(
              nqp::index($!value,nqp::getattr($needle,Str,'$!value'),$pos),-1)
          )
        )
    }

    # TODO Use coercer in 1 candidate when RT131014
    proto method indices(|) {*}
    multi method indices(Str:D: Cool:D $needle, *%pars) {self.indices: $needle.Str, |%pars}
    multi method indices(Str:D: Str:D $needle, :$overlap) {
        nqp::stmts(
          (my $need    := nqp::getattr($needle,Str,'$!value')),
          (my int $add  = nqp::if($overlap,1,nqp::chars($need) || 1)),
          (my $indices := nqp::create(IterationBuffer)),
          (my int $pos),
          (my int $i),
          nqp::while(
            nqp::isge_i(($i = nqp::index($!value,$need,$pos)),0),
            nqp::stmts(
              nqp::push($indices,nqp::p6box_i($i)),
              ($pos = nqp::add_i($i,$add))
            )
          ),
          nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',$indices)
        )
    }
    multi method indices(Str:D: Cool:D $needle, Cool:D $start, *%pars) {self.indices: $needle.Str, $start.Int, |%pars}
    multi method indices(Str:D: Str:D $needle, Int:D $start, :$overlap) {
        nqp::stmts(
          (my int $pos = $start),
          nqp::if(
            nqp::isgt_i($pos,nqp::chars($!value)),
            nqp::create(List),   # position after string, always empty List
            nqp::stmts(
              (my $need   := nqp::getattr($needle,Str,'$!value')),
              (my int $add = nqp::if($overlap,1,nqp::chars($need) || 1)),
              (my $indices := nqp::create(IterationBuffer)),
              (my int $i),
              nqp::while(
                nqp::isge_i(($i = nqp::index($!value,$need,$pos)),0),
                nqp::stmts(
                  nqp::push($indices,nqp::p6box_i($i)),
                  ($pos = nqp::add_i($i,$add))
                )
              ),
              nqp::p6bindattrinvres(nqp::create(List),List,'$!reified',$indices)
            )
          )
        )
    }

    # TODO Use coercer in 1 candidate when RT131014
    proto method index(|) {*}
    multi method index(Str:D: Cool:D $needle) {self.index: $needle.Str}
    multi method index(Str:D: Str:D $needle) {
        nqp::if(
          nqp::islt_i((my int $i =
            nqp::index($!value,nqp::getattr($needle,Str,'$!value'))),
            0
          ),
          Nil,
          nqp::p6box_i($i)
        )
    }
    multi method index(Str:D: Cool:D $needle, Cool:D $pos) {self.index: $needle.Str, $pos.Int}
    multi method index(Str:D: Str:D $needle, Int:D $pos) {
        nqp::if(
          nqp::isbig_I(nqp::decont($pos)),
          Failure.new(X::OutOfRange.new(
            :what("Position in index"),
            :got($pos),
            :range("0..{self.chars}")
          )),
          nqp::if(
            nqp::islt_i($pos,0),
            Failure.new(X::OutOfRange.new(
              :what("Position in index"),
              :got($pos),
              :range("0..{self.chars}")
            )),
            nqp::if(
              nqp::islt_i((my int $i = nqp::index(
                $!value,nqp::getattr($needle,Str,'$!value'),$pos
              )),0),
              Nil,
              nqp::p6box_i($i)
            )
          )
        )
    }

    # TODO Use coercer in 1 candidate when RT131014
    proto method rindex(|) {*}
    multi method rindex(Str:D: Cool:D $needle) {self.rindex: $needle.Str}
    multi method rindex(Str:D: Str:D $needle) {
        nqp::if(
          nqp::islt_i((my int $i =
            nqp::rindex($!value,nqp::getattr($needle,Str,'$!value'))),
            0
          ),
          Nil,
          nqp::p6box_i($i)
        )
    }
    multi method rindex(Str:D: Cool:D $needle, Cool:D $pos) {self.rindex: $needle.Str, $pos.Int}
    multi method rindex(Str:D: Str:D $needle, Int:D $pos) {
        nqp::if(
          nqp::isbig_I(nqp::decont($pos)),
          Failure.new(X::OutOfRange.new(
            :what("Position in rindex"),
            :got($pos),
            :range("0..{self.chars}")
          )),
          nqp::if(
            nqp::islt_i($pos,0),
            Failure.new(X::OutOfRange.new(
              :what("Position in rindex"),
              :got($pos),
              :range("0..{self.chars}")
            )),
            nqp::if(
              nqp::islt_i((my int $i = nqp::rindex(
                $!value,nqp::getattr($needle,Str,'$!value'),$pos
              )),0),
              Nil,
              nqp::p6box_i($i)
            )
          )
        )
    }

    method pred(Str:D:) {
        (my int $chars = Rakudo::Internals.POSSIBLE-MAGIC-CHARS(self))
          ?? Rakudo::Internals.PRED(self,$chars - 1)
          !! self
    }

    method succ(Str:D:) {
        (my int $chars = Rakudo::Internals.POSSIBLE-MAGIC-CHARS(self))
          ?? Rakudo::Internals.SUCC(self,$chars - 1)
          !! self
    }

    multi method Numeric(Str:D:) {
        # Handle special empty string
        self.trim eq ""
          ?? 0
          !! val(self, :val-or-fail)
    }

    multi method gist(Str:D:) { self }
    multi method perl(Str:D:) {
        '"' ~ Rakudo::Internals.PERLIFY-STR(self) ~ '"'
    }

    proto method comb(|) {*}
    multi method comb(Str:D:) {
        Seq.new(class :: does Iterator {
            has str $!str;
            has int $!chars;
            has int $!pos;
            method !SET-SELF(\string) {
                nqp::stmts(
                  ($!str   = nqp::unbox_s(string)),
                  ($!chars = nqp::chars($!str)),
                  ($!pos = -1),
                  self
                )
            }
            method new(\string) { nqp::create(self)!SET-SELF(string) }
            method pull-one() {
                nqp::if(
                  nqp::islt_i(($!pos = nqp::add_i($!pos,1)),$!chars),
                  nqp::p6box_s(nqp::substr($!str,$!pos,1)),
                  IterationEnd
                )
            }
            method push-all($target --> IterationEnd) {
                nqp::stmts(
                  (my str $str = $!str),      # locals are faster
                  (my int $pos = $!pos),
                  (my int $chars = $!chars),
                  nqp::while(
                    nqp::islt_i(($pos = nqp::add_i($pos,1)),$chars),
                    $target.push(nqp::substr($str,$pos,1))
                  ),
                  ($!pos = $pos)
                )
            }
            method count-only() { nqp::p6box_i($!chars) }
            method bool-only(--> True) { }
        }.new(self));
    }
    multi method comb(Str:D: Int:D $size is copy, $limit = *) {
        my int $inf = nqp::istype($limit,Whatever) || $limit == Inf;
        return self.comb if $size <= 1 && $inf;
        $size = 1 if $size < 1;

        Seq.new(class :: does Iterator {
            has str $!str;
            has int $!chars;
            has int $!size;
            has int $!pos;
            has int $!max;
            has int $!todo;
            method !SET-SELF(\string,\size,\limit,\inf) {
                nqp::stmts(
                  ($!str   = nqp::unbox_s(string)),
                  ($!chars = nqp::chars($!str)),
                  ($!size  = 1 max size),
                  ($!pos   = -size),
                  ($!max   = 1 + floor( ( $!chars - 1 ) / $!size )),
                  ($!todo  = (inf ?? $!max !! (0 max limit)) + 1),
                  self
                )
            }
            method new(\string,\size,\limit,\inf) {
                nqp::if(
                  string,
                  nqp::create(self)!SET-SELF(string,size,limit,inf),
                  Rakudo::Iterator.Empty
                )
            }
            method pull-one() {
                ($!todo = $!todo - 1) && ($!pos = $!pos + $!size) < $!chars
                  ?? nqp::p6box_s(nqp::substr($!str, $!pos, $!size))
                  !! IterationEnd
            }
            method push-all($target --> IterationEnd) {
                my int $todo  = $!todo;
                my int $pos   = $!pos;
                my int $size  = $!size;
                my int $chars = $!chars;
                $target.push(nqp::p6box_s(nqp::substr($!str, $pos, $size)))
                  while ($todo = $todo - 1 ) && ($pos = $pos + $size) < $chars;
                $!pos = $!chars;
            }
            method count-only() { $!max }
            method bool-only(--> True) { }
        }.new(self,$size,$limit,$inf))
    }
    multi method comb(Str:D: Str $pat) {
        return self.comb(1) unless $pat;
        Seq.new(class :: does Iterator {
            has str $!str;
            has str $!pat;
            has int $!pos;
            method !SET-SELF(\string, \pat) {
                $!str = nqp::unbox_s(string);
                $!pat = nqp::unbox_s(pat);
                self
            }
            method new(\string, \pat) { nqp::create(self)!SET-SELF(string,pat) }
            method pull-one() {
                my int $found = nqp::index($!str, $!pat, $!pos);
                if $found < 0 {
                    IterationEnd
                }
                else {
                    $!pos = $found + 1;
                    nqp::p6box_s($!pat)
                }
            }
        }.new(self, $pat));
    }
    multi method comb(Str:D: Str $pat, $limit) {
        return self.comb($pat)
          if nqp::istype($limit,Whatever) || $limit == Inf;
        return self.comb(1, $limit) unless $pat;

        Seq.new(class :: does Iterator {
            has str $!str;
            has str $!pat;
            has int $!pos;
            has int $!todo;
            method !SET-SELF(\string, \pat, \limit) {
                $!str  = nqp::unbox_s(string);
                $!pat  = nqp::unbox_s(pat);
                $!todo = nqp::unbox_i(limit.Int);
                self
            }
            method new(\string, \pat, \limit) {
                nqp::create(self)!SET-SELF(string, pat, limit)
            }
            method pull-one() {
                my int $found = nqp::index($!str, $!pat, $!pos);
                if $found < 0 || $!todo == 0 {
                    IterationEnd
                }
                else {
                    $!pos  = $found + 1;
                    $!todo = $!todo - 1;
                    nqp::p6box_s($!pat)
                }
            }
        }.new(self, $pat, $limit));
    }
    multi method comb(Str:D: Regex:D $pattern, :$match) {
        Seq.new(nqp::if(
          $match,
          self.match($pattern, :g),
          self.match($pattern, :g, :as(Str))
        ).iterator)
    }
    multi method comb(Str:D: Regex:D $pattern, $limit, :$match) {
        nqp::if(
          nqp::istype($limit,Whatever) || $limit == Inf,
          self.comb($pattern, :$match),
          Seq.new(nqp::if(
            $match,
            self.match($pattern, :x(1..$limit)),
            self.match($pattern, :x(1..$limit), :as(Str))
          ).iterator)
        )
    }

    # cache cursor initialization lookup
    my $cursor-init := Match.^lookup("!cursor_init");

    my \CURSOR-GLOBAL     := Match.^lookup("CURSOR_MORE"   );  # :g
    my \CURSOR-OVERLAP    := Match.^lookup("CURSOR_OVERLAP");  # :ov
    my \CURSOR-EXHAUSTIVE := Match.^lookup("CURSOR_NEXT"   );  # :ex

    my \POST-MATCH  := Match.^lookup("MATCH" );  # Match object
    my \POST-STR    := Match.^lookup("STR"   );  # Str object

    # iterate with post-processing
    class POST-ITERATOR does Iterator {
        has Mu $!cursor; # cannot put these 3 lines in role
        has Mu $!move;
        has Mu $!post;
        method !SET-SELF(\cursor,\move,\post) {
            $!cursor := cursor;
            $!move   := move;
            $!post   := post;
            self
        }
        method new(\c,\t,\p) { nqp::create(self)!SET-SELF(c,t,p) }
        method pull-one() is raw {
            nqp::if(
              nqp::isge_i(nqp::getattr_i($!cursor,Match,'$!pos'),0),
              nqp::stmts(
                (my $pulled := $!cursor),
                ($!cursor := $!move($!cursor)),
                $!post($pulled)
              ),
              IterationEnd
            )
        }
        method skip-one() is raw {
            nqp::if(
              nqp::isge_i(nqp::getattr_i($!cursor,Match,'$!pos'),0),
              ($!cursor := $!move($!cursor)),
            )
        }
        method push-all($target --> IterationEnd) {
            nqp::while(
              nqp::isge_i(nqp::getattr_i($!cursor,Match,'$!pos'),0),
              nqp::stmts(
                $target.push($!post($!cursor)),
                ($!cursor := $!move($!cursor))
              )
            )
        }
    }

    # iterate returning Matches
    class CURSOR-ITERATOR does Iterator {
        has Mu $!cursor;
        has Mu $!move;
        method !SET-SELF(\cursor,\move) {
            $!cursor := cursor;
            $!move   := move;
            self
        }
        method new(\c,\t) { nqp::create(self)!SET-SELF(c,t) }
        method pull-one() is raw {
            nqp::if(
              nqp::isge_i(nqp::getattr_i($!cursor,Match,'$!pos'),0),
              nqp::stmts(
                (my $pulled := $!cursor),
                ($!cursor := $!move($!cursor)),
                $pulled
              ),
              IterationEnd
            )
        }
        method skip-one() is raw {
            nqp::if(
              nqp::isge_i(nqp::getattr_i($!cursor,Match,'$!pos'),0),
              ($!cursor := $!move($!cursor)),
            )
        }
        method push-all($target --> IterationEnd) {
            nqp::while(
              nqp::isge_i(nqp::getattr_i($!cursor,Match,'$!pos'),0),
              nqp::stmts(
                $target.push($!cursor),
                ($!cursor := $!move($!cursor))
              )
            )
        }
    }

    # Look for short/long named parameter and remove it from the hash
    sub fetch-short-long(\opts, str $short, str $long, \store --> Nil) {
        nqp::if(
          nqp::existskey(opts,$short),
          nqp::stmts(
            (store = nqp::atkey(opts,$short)),
            nqp::deletekey(opts,$short)
          ),
          nqp::if(
            nqp::existskey(opts,$long),
            nqp::stmts(
              (store = nqp::atkey(opts,$long)),
              nqp::deletekey(opts,$long)
            )
          )
        )
    }

    # Look for named parameters, do not remove from hash
    sub fetch-all-of(\opts, @names, \store --> Nil) {
        nqp::stmts(
          (my int $elems = @names.elems),   # reifies
          (my $list := nqp::getattr(@names,List,'$!reified')),
          (my int $i = -1),
          nqp::while(
            nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
            nqp::if(
              nqp::existskey(opts,nqp::unbox_s(nqp::atpos($list,$i))),
              (store = nqp::atkey(opts,nqp::unbox_s(nqp::atpos($list,$i)))),
            )
          )
        )
    }

    sub die-before-first($got) {
        die "Attempt to retrieve before :1st match -- :nth({
            $got // $got.^name
        })"
    }

    # All of these !match methods take a nqp::getlexcaller value for the $/
    # to be set as the first parameter.  The second parameter is usually
    # the Match object to be used (or something from which a Match can
    # be made).

    # Generic fallback for matching with a pattern
    method !match-pattern(\slash, $pattern, str $name, $value, \opts) {
        nqp::stmts(
          (my $opts := nqp::getattr(opts,Map,'$!storage')),
          nqp::bindkey($opts,$name,$value),
          fetch-short-long($opts, "p", "pos", my $p),
          fetch-short-long($opts, "c", "continue", my $c),
          nqp::unless(nqp::defined($c), $c = 0),
          nqp::if(
            nqp::elems($opts),
            nqp::if(
              nqp::defined($p),
              self!match-cursor(slash,
                $pattern($cursor-init(Match,self,:$p)), '', 0, $opts),
              self!match-cursor(slash,
                $pattern($cursor-init(Match,self,:$c)), '', 0, $opts)
            ),
            nqp::if(
              nqp::defined($p),
              self!match-one(slash,
                $pattern($cursor-init(Match,self,:$p))),
              self!match-one(slash,
                $pattern($cursor-init(Match,self,:$c)))
            )
          )
        )
    }

    # Generic fallback for matching with a cursor.  This is typically
    # called if more than one named parameter was specified.  Arguments
    # 3/4 are the initial named parameter matched: instead of flattening
    # the named parameter into another slurpy hash, we pass the name and
    # the value as extra parameters, and add it back in the hash with
    # named parameters.
    method !match-cursor(\slash, \cursor, str $name, $value, \opts) {
        nqp::stmts(
          (my $opts := nqp::getattr(opts,Map,'$!storage')),
          nqp::if(
            nqp::chars($name),
            nqp::bindkey($opts,$name,$value)
          ),
          fetch-short-long($opts, "ex", "exhaustive", my $ex),
          fetch-short-long($opts, "ov", "overlap",    my $ov),
          (my \move := nqp::if($ex, CURSOR-EXHAUSTIVE,
            nqp::if($ov, CURSOR-OVERLAP, CURSOR-GLOBAL))),

          fetch-short-long($opts, "as", "as", my $as),
          (my \post := nqp::if(nqp::istype($as,Str), POST-STR, POST-MATCH)),

          fetch-short-long($opts, "g", "global", my $g),
          nqp::if(
            nqp::elems($opts),
            nqp::stmts(
              fetch-short-long($opts, "x", "x", my $x),
              fetch-all-of($opts, <st nd rd th nth>, my $nth),
              nqp::if(
                nqp::defined($nth),
                nqp::if(
                  nqp::defined($x),                             # :nth && :x
                  self!match-x(slash,
                    self!match-nth(slash, cursor,
                      move, post, $nth, nqp::hash).iterator, $x),
                  self!match-nth(slash, cursor,
                    move, post, $nth, nqp::hash)  # nth
                ),
                nqp::if(
                  nqp::defined($x),
                  self!match-x(slash,                           # :x
                    POST-ITERATOR.new(cursor, move, post), $x),
                  nqp::if(                                      # only :ex|ov|g
                    $ex || $ov || $g,
                    self!match-list(slash, cursor, move, post),
                    self!match-one(slash, cursor)
                  )
                )
              )
            ),
            nqp::if(                                            # only :ex|ov|g
              $ex || $ov || $g,
              self!match-list(slash, cursor, move, post),
              self!match-one(slash, cursor)
            )
          )
        )
    }

    # Match object at given position
    method !match-one(\slash, \cursor) {
        nqp::decont(slash = nqp::if(
          nqp::isge_i(nqp::getattr_i(cursor,Match,'$!pos'),0),
          cursor.MATCH,
          Nil
        ))
    }

    # Some object at given position
    method !match-as-one(\slash, \cursor, \as) {
        nqp::decont(slash = nqp::if(
          nqp::isge_i(nqp::getattr_i(cursor,Match,'$!pos'),0),
          nqp::if(nqp::istype(as,Str), POST-STR, POST-MATCH)(cursor),
          Nil
        ))
    }

    # Create list from the appropriate Sequence given the move
    method !match-list(\slash, \cursor, \move, \post) {
        nqp::decont(slash = nqp::if(
          nqp::isge_i(nqp::getattr_i(cursor,Match,'$!pos'),0),
          Seq.new(POST-ITERATOR.new(cursor, move, post)).list,
          List.new,
        ))
    }

    # Handle matching of the nth match specification.
    method !match-nth(\slash, \cursor, \move, \post, $nth, %opts) {
        nqp::if(
          nqp::elems(nqp::getattr(%opts,Map,'$!storage')),
          self!match-cursor(slash, cursor, 'nth', $nth, %opts),
          nqp::if(
            nqp::defined($nth),
            nqp::if(
              nqp::istype($nth,Whatever),
              self!match-last(slash, cursor, move),
              nqp::if(
                nqp::istype($nth,Numeric),
                nqp::if(
                  $nth == Inf,
                  self!match-last(slash, cursor, move),
                  nqp::if(
                    $nth < 1,
                    die-before-first($nth),
                    self!match-nth-int(slash, cursor, move, post, $nth.Int)
                  )
                ),
                nqp::if(
                  nqp::istype($nth,WhateverCode),
                  nqp::if(
                    nqp::iseq_i((my int $tail = abs($nth(-1))),1),
                    self!match-last(slash, cursor, move),
                    self!match-nth-tail(slash, cursor, move, $tail)
                  ),
                  nqp::if(
                    nqp::istype($nth,Callable),
                    self!match-nth-int(slash,
                      cursor, move, post, $nth()),
                    self!match-nth-iterator(slash,
                      POST-ITERATOR.new(cursor, move, post),
                      $nth.iterator)
                  )
                )
              )
            ),
            self!match-one(slash, cursor)
          )
        )
    }

    # Give back the nth match found
    method !match-nth-int(\slash, \cursor, \move, \post, int $nth) {
        nqp::decont(slash = nqp::if(
          nqp::isge_i(nqp::getattr_i(cursor,Match,'$!pos'),0),
          nqp::if(
            nqp::eqaddr(
              (my $pulled := POST-ITERATOR.new(cursor, move, post)
                .skip-at-least-pull-one(nqp::sub_i($nth,1))),
              IterationEnd
            ),
            Nil,              # not enough matches
            $pulled           # found it!
          ),
          Nil                 # no matches whatsoever
        ))
    }

    # Give back the N-tail match found
    method !match-nth-tail(\slash, \cursor, \move, int $tail) {
        nqp::decont(slash = nqp::if(
          nqp::eqaddr((my $pulled :=
            Rakudo::Iterator.LastNValues(
              CURSOR-ITERATOR.new(cursor, move),
              $tail, 'match', 1).pull-one),
            IterationEnd
          ),
          Nil,
          $pulled.MATCH
        ))
    }

    # Give last value of given iterator, or Nil if none
    method !match-last(\slash, \cursor, \move) {
        nqp::decont(slash = nqp::if(
          nqp::eqaddr((my $pulled :=
            Rakudo::Iterator.LastValue(
              CURSOR-ITERATOR.new(cursor, move),
              'match')),
            IterationEnd
          ),
          Nil,
          $pulled.MATCH
        ))
    }

    # These !match methods take an iterator instead of a cursor.
    # Give list with matches found given a range with :nth
    method !match-nth-range(\slash, \iterator, $min, $max) {
        nqp::decont(slash = nqp::stmts(
          (my int $skip = $min),
          nqp::if(
            nqp::islt_i($skip,1),
            die-before-first($min),
            nqp::stmts(
              nqp::while(
                nqp::isgt_i($skip,1) && iterator.skip-one,
                ($skip = nqp::sub_i($skip,1))
              ),
              nqp::if(
                nqp::iseq_i($skip,1),
                nqp::if(                       # did not exhaust while skipping
                  $max == Inf,                 # * is Inf in N..*
                  nqp::stmts(                  # open ended
                    (my $matches := nqp::create(IterationBuffer)),
                    nqp::until(
                      nqp::eqaddr(
                        (my $pulled := iterator.pull-one),
                        IterationEnd
                      ),
                      nqp::push($matches,$pulled)
                    ),
                    nqp::p6bindattrinvres(
                      nqp::create(List),List,'$!reified',$matches)
                  ),
                  nqp::stmts(                  # upto the max index
                    (my int $todo = $max - $min + 1),
                    ($matches :=
                      nqp::setelems(nqp::create(IterationBuffer),$todo)),
                    (my int $i = -1),
                    nqp::until(
                      nqp::iseq_i(($i = nqp::add_i($i,1)),$todo)
                        || nqp::eqaddr(
                             ($pulled := iterator.pull-one),IterationEnd),
                      nqp::bindpos($matches,$i,$pulled)
                    ),
                    nqp::if(
                      nqp::iseq_i($i,$todo),
                      nqp::p6bindattrinvres(  # found all values
                        nqp::create(List),List,'$!reified',$matches),
                      Empty                   # no match, since not all values
                    )
                  )
                ),
                Empty                         # exhausted while skipping
              )
            )
          )
        ))
    }

    # Give list with matches found given an iterator with :nth
    method !match-nth-iterator(\slash, \source, \indexes) {
        nqp::decont(slash = nqp::stmts(
          Seq.new(Rakudo::Iterator.MonotonicIndexes(
            source, indexes, 1,
            -> $got,$next {
              nqp::if(
                $next == 1,
                die-before-first($got),
                (die "Attempt to fetch match #$got after #{$next - 1}")
              )
            }
          )).list
        ))
    }

    # Give list with matches found given an iterator with :x
    method !match-x(\slash, \iterator, $x) {
        nqp::if(
          nqp::istype($x,Whatever),
          Seq.new(iterator).list,
          nqp::if(
            nqp::istype($x,Numeric),
            nqp::if(
              $x == Inf,
              Seq.new(iterator).list,
              nqp::if(
                nqp::istype($x,Int),
                self!match-x-range(slash, iterator, $x, $x),
                nqp::stmts(
                  (my int $xint = $x.Int),
                  self!match-x-range(slash, iterator, $xint, $xint)
                )
              )
            ),
            nqp::if(
              nqp::istype($x,Range),
              self!match-x-range(slash, iterator, $x.min, $x.max),
              nqp::stmts(
                (slash = Nil),
                Failure.new(X::Str::Match::x.new(:got($x)))
              )
            )
          )
        )
    }

    # Give list with matches found given a range with :x
    method !match-x-range(\slash, \iterator, $min, $max) {
        nqp::decont(slash = nqp::stmts(
          (my int $todo = nqp::if($max == Inf, 0x7fffffff, $max)),
          (my $matches := nqp::create(IterationBuffer)),
          nqp::until(
            nqp::islt_i(($todo = nqp::sub_i($todo,1)), 0) ||
              nqp::eqaddr((my $pulled := iterator.pull-one),IterationEnd),
            nqp::push($matches,$pulled)
          ),
          nqp::if(
            nqp::elems($matches) >= $min,
            nqp::p6bindattrinvres(
              nqp::create(List),List,'$!reified',$matches),
            ()
          )
        ))
    }

    proto method match(|) { $/ := nqp::getlexcaller('$/'); {*} }
    multi method match(Cool:D $pattern, |c) {
        $/ := nqp::getlexcaller('$/');
        self.match(/ "$pattern": /,|c)
    }

    # All of these .match candidates take a single required named parameter
    # so that handling specification of a single named parameter can be much
    # quicker.  Unfortunately, we cannot cheaply do MMD on an empty slurpy
    # hash, which would make things much more simple.
    multi method match(Regex:D $pattern, :continue(:$c)!, *%_) {
        nqp::if(
          nqp::elems(nqp::getattr(%_,Map,'$!storage')),
          self!match-pattern(nqp::getlexcaller('$/'), $pattern, 'c', $c, %_),
          self!match-one(nqp::getlexcaller('$/'),
            $pattern($cursor-init(Match,self,:$c)))
        )
    }
    multi method match(Regex:D $pattern, :pos(:$p)!, *%_) {
        nqp::if(
          nqp::elems(nqp::getattr(%_,Map,'$!storage')),
          self!match-pattern(nqp::getlexcaller('$/'), $pattern, 'p', $p, %_),
          nqp::if(
            nqp::defined($p),
            self!match-one(nqp::getlexcaller('$/'),
              $pattern($cursor-init(Match,self,:$p))),
            self!match-one(nqp::getlexcaller('$/'),
              $pattern($cursor-init(Match,self,:0c)))
          )
        )
    }
    multi method match(Regex:D $pattern, :global(:$g)!, *%_) {
        nqp::if(
          nqp::elems(nqp::getattr(%_,Map,'$!storage')),
          self!match-cursor(nqp::getlexcaller('$/'),
            $pattern($cursor-init(Match,self,:0c)), 'g', $g, %_),
          nqp::if(
            $g,
            self!match-list(nqp::getlexcaller('$/'),
              $pattern($cursor-init(Match,self,:0c)),
              CURSOR-GLOBAL, POST-MATCH),
            self!match-one(nqp::getlexcaller('$/'),
              $pattern($cursor-init(Match,self,:0c)))
          )
        )
    }
    multi method match(Regex:D $pattern, :overlap(:$ov)!, *%_) {
        nqp::if(
          nqp::elems(nqp::getattr(%_,Map,'$!storage')),
          self!match-cursor(nqp::getlexcaller('$/'),
            $pattern($cursor-init(Match,self,:0c)), 'ov', $ov, %_),
          nqp::if(
            $ov,
            self!match-list(nqp::getlexcaller('$/'),
              $pattern($cursor-init(Match,self,:0c)),
              CURSOR-OVERLAP, POST-MATCH),
            self!match-one(nqp::getlexcaller('$/'),
              $pattern($cursor-init(Match,self,:0c)))
          )
        )
    }
    multi method match(Regex:D $pattern, :exhaustive(:$ex)!, *%_) {
        nqp::if(
          nqp::elems(nqp::getattr(%_,Map,'$!storage')),
          self!match-cursor(nqp::getlexcaller('$/'),
            $pattern($cursor-init(Match,self,:0c)), 'ex', $ex, %_),
          nqp::if(
            $ex,
            self!match-list(nqp::getlexcaller('$/'),
              $pattern($cursor-init(Match,self,:0c)),
              CURSOR-EXHAUSTIVE, POST-MATCH),
            self!match-one(nqp::getlexcaller('$/'),
              $pattern($cursor-init(Match,self,:0c)))
          )
        )
    }
    multi method match(Regex:D $pattern, :$x!, *%_) {
        nqp::if(
          nqp::elems(nqp::getattr(%_,Map,'$!storage')),
          self!match-cursor(nqp::getlexcaller('$/'),
            $pattern($cursor-init(Match,self,:0c)), 'x', $x, %_),
          nqp::if(
            nqp::defined($x),
            self!match-x(nqp::getlexcaller('$/'),
              POST-ITERATOR.new($pattern($cursor-init(Match,self,:0c)),
                CURSOR-GLOBAL, POST-MATCH
              ), $x),
            self!match-one(nqp::getlexcaller('$/'),
              $pattern($cursor-init(Match,self,:0c)), $x)
          )
        )
    }
    multi method match(Regex:D $pattern, :$st!, *%_) {
        self!match-nth(nqp::getlexcaller('$/'),
          $pattern($cursor-init(Match,self,:0c)),
          CURSOR-GLOBAL, POST-MATCH, $st, %_)
    }
    multi method match(Regex:D $pattern, :$nd!, *%_) {
        self!match-nth(nqp::getlexcaller('$/'),
          $pattern($cursor-init(Match,self,:0c)),
          CURSOR-GLOBAL, POST-MATCH, $nd, %_)
    }
    multi method match(Regex:D $pattern, :$rd!, *%_) {
        self!match-nth(nqp::getlexcaller('$/'),
          $pattern($cursor-init(Match,self,:0c)),
          CURSOR-GLOBAL, POST-MATCH, $rd, %_)
    }
    multi method match(Regex:D $pattern, :$th!, *%_) {
        self!match-nth(nqp::getlexcaller('$/'),
          $pattern($cursor-init(Match,self,:0c)),
          CURSOR-GLOBAL, POST-MATCH, $th, %_)
    }
    multi method match(Regex:D $pattern, :$nth!, *%_) {
        self!match-nth(nqp::getlexcaller('$/'),
          $pattern($cursor-init(Match,self,:0c)),
          CURSOR-GLOBAL, POST-MATCH, $nth, %_)
    }
    multi method match(Regex:D $pattern, :$as!, *%_) {
        nqp::if(
          nqp::elems(nqp::getattr(%_,Map,'$!storage')),
          self!match-cursor(nqp::getlexcaller('$/'),
            $pattern($cursor-init(Match,self,:0c)), 'as', $as, %_),
          self!match-as-one(nqp::getlexcaller('$/'),
            $pattern($cursor-init(Match,self,:0c)), $as)
        )
    }
    multi method match(Regex:D $pattern, *%_) {
        nqp::if(
          nqp::elems(nqp::getattr(%_,Map,'$!storage')),
          self!match-cursor(nqp::getlexcaller('$/'),
            $pattern($cursor-init(Match,self,:0c)), '', 0, %_),
          self!match-one(nqp::getlexcaller('$/'),
            $pattern($cursor-init(Match,self,:0c)))
        )
    }

    proto method subst-mutate(|) {
        $/ := nqp::getlexcaller('$/');
        {*}
    }
    multi method subst-mutate(
      Str:D $self is rw: Any:D $matcher, $replacement,
      :ii(:$samecase), :ss(:$samespace), :mm(:$samemark), *%options
    ) {
        my $global = %options<g> || %options<global>;
        my \caller_dollar_slash := nqp::getlexcaller('$/');
        my $SET_DOLLAR_SLASH     = nqp::istype($matcher, Regex);
        my $word_by_word = so $samespace || %options<s> || %options<sigspace>;

        my \matches := %options
          ?? self.match($matcher, |%options)
          !! self.match($matcher);  # 30% faster
        nqp::if(
          nqp::istype(matches, Failure) || nqp::isfalse(matches),
          nqp::stmts(
            $SET_DOLLAR_SLASH && (try caller_dollar_slash = $/),
            matches),
          nqp::stmts(
            ($self = $self!APPLY-MATCHES: matches, $replacement,
              caller_dollar_slash, $SET_DOLLAR_SLASH, $word_by_word,
              $samespace, $samecase, $samemark),
            $SET_DOLLAR_SLASH && (try caller_dollar_slash = matches),
            matches))
    }

    proto method subst(|) {
        $/ := nqp::getlexcaller('$/');
        {*}
    }
    multi method subst(Str:D: Str:D $original, Str:D $final, *%options) {
        nqp::if(
          (my $opts := nqp::getattr(%options,Map,'$!storage'))
            && nqp::isgt_i(nqp::elems($opts),1),
            self!SUBST(nqp::getlexcaller('$/'),$original,$final,|%options),
          nqp::if(
            nqp::elems($opts),
            nqp::if(                                      # one named
              nqp::atkey($opts,'g') || nqp::atkey($opts,'global'),
              Rakudo::Internals.TRANSPOSE(self, $original, $final),
              nqp::if(                                    # no trueish g/global
                nqp::existskey($opts,'g') || nqp::existskey($opts,'global'),
                Rakudo::Internals.TRANSPOSE-ONE(self, $original, $final),
                self!SUBST(nqp::getlexcaller('$/'),$original,$final,|%options)
              )
            ),
            Rakudo::Internals.TRANSPOSE-ONE(self, $original, $final) # no nameds
          )
        )
    }
    multi method subst(Str:D: $matcher, $replacement, *%options) {
        self!SUBST(nqp::getlexcaller('$/'), $matcher, $replacement, |%options)
    }
    method !SUBST(Str:D: \caller_dollar_slash, $matcher, $replacement,
      :global(:$g), :ii(:$samecase), :ss(:$samespace), :mm(:$samemark),
      *%options
    ) {
        X::Str::Subst::Adverb.new(:name($_), :got(%options{$_})).throw
          if %options{$_} for <ov ex>;

        my $SET_DOLLAR_SLASH = nqp::istype($matcher, Regex);
        my $word_by_word = so $samespace || %options<s> || %options<sigspace>;

        my \matches := %options
          ?? self.match($matcher, :$g, |%options)
          !! self.match($matcher, :$g);  # 30% faster
        nqp::if(
          nqp::istype(matches, Failure),
          nqp::stmts(
            $SET_DOLLAR_SLASH && (try caller_dollar_slash = Nil),
            matches),
          nqp::if(
            matches,
            nqp::stmts(
              (my \res := self!APPLY-MATCHES: matches, $replacement,
                   caller_dollar_slash, $SET_DOLLAR_SLASH, $word_by_word,
                   $samespace, $samecase, $samemark),
              $SET_DOLLAR_SLASH && (try caller_dollar_slash = matches),
              res),
            nqp::stmts(
              $SET_DOLLAR_SLASH && (try caller_dollar_slash = matches),
              self)))
    }

    # NOTE: this method is also called by s/// op in src/Perl6/Actions.nqp
    method !APPLY-MATCHES(\matches,$replacement,\cds,\SDS,\word_by_word,\space,\case,\mark) {
        my \callable       := nqp::istype($replacement,Callable);

        my int $prev;
        my str $str    = nqp::unbox_s(self);
        my Mu $result := nqp::list_s();

        # need to do something special
        if SDS || space || case || mark || callable {
            my \noargs        := callable ?? $replacement.count == 0 !! False;
            my \fancy         := space || case || mark || word_by_word;
            my \case-and-mark := case && mark;

            for flat matches -> $m {
                try cds = $m if SDS;
                nqp::push_s(
                  $result,nqp::substr($str,$prev,nqp::unbox_i($m.from) - $prev)
                );

                if fancy {
                    my $mstr := $m.Str;
                    my $it := ~(callable
                      ?? (noargs ?? $replacement() !! $replacement($m))
                      !! $replacement
                    );
                    if word_by_word {  # all spacers delegated to word-by-word
                        my &filter :=
                        case-and-mark
                        ?? -> $w,$p { $w.samemark($p).samecase($p) }
                        !! case
                            ?? -> $w,$p { $w.samecase($p) }
                            !! -> $w,$p { $w.samemark($p) }
                        nqp::push_s($result,nqp::unbox_s(
                          $it.word-by-word($mstr,&filter,:samespace(?space))
                        ) );
                    }
                    elsif case-and-mark {
                        nqp::push_s($result,nqp::unbox_s(
                          $it.samecase($mstr).samemark($mstr)
                        ) );
                    }
                    elsif case {
                        nqp::push_s($result,nqp::unbox_s($it.samecase(~$m)));
                    }
                    else { # mark
                        nqp::push_s($result,nqp::unbox_s($it.samemark(~$m)));
                    }
                }
                else {
                    nqp::push_s($result,nqp::unbox_s( ~(callable
                      ?? (noargs ?? $replacement() !! $replacement($m))
                      !! $replacement
                    ) ) );
                }
                $prev = nqp::unbox_i($m.to);
            }
            nqp::push_s($result,nqp::substr($str,$prev));
            nqp::p6box_s(nqp::join('',$result));
        }

        # simple string replacement
        else {
            for flat matches -> $m {
                nqp::push_s(
                  $result,nqp::substr($str,$prev,nqp::unbox_i($m.from) - $prev)
                );
                $prev = nqp::unbox_i($m.to);
            }
            nqp::push_s($result,nqp::substr($str,$prev));
            nqp::p6box_s(nqp::join(nqp::unbox_s(~$replacement),$result));
        }
    }

#?if moar
    multi method ords(Str:D:) { self.NFC.list }
#?endif
#?if !moar
    multi method ords(Str:D:) {
        Seq.new(class :: does Iterator {
            has str $!str;
            has int $!chars;
            has int $!pos;
            method !SET-SELF(\string) {
                $!str   = nqp::unbox_s(string);
                $!chars = nqp::chars($!str);
                $!pos   = -1;
                self
            }
            method new(\string) { nqp::create(self)!SET-SELF(string) }
            method pull-one() {
                nqp::if(
                  nqp::islt_i(($!pos = nqp::add_i($!pos,1)),$!chars),
                  nqp::p6box_i(nqp::ordat($!str,$!pos)),
                  IterationEnd
                )
            }
        }.new(self));
    }
#?endif

    proto method lines(|) {*}
    multi method lines(Str:D: :$count!) {
        # we should probably deprecate this feature
        $count ?? self.lines.elems !! self.lines;
    }
    multi method lines(Str:D: $limit) {
        nqp::istype($limit,Whatever) || $limit == Inf
          ?? self.lines
          !! self.lines.head($limit)
    }
    multi method lines(Str:D:) {
        Seq.new(class :: does Iterator {
            has str $!str;
            has int $!chars;
            has int $!pos;
            method !SET-SELF(\string) {
                $!str   = nqp::unbox_s(string);
                $!chars = nqp::chars($!str);
                $!pos   = 0;
                self
            }
            method new(\string) { nqp::create(self)!SET-SELF(string) }
            method pull-one() {
                my int $left;
                return IterationEnd if ($left = $!chars - $!pos) <= 0;

                my int $nextpos = nqp::findcclass(
                  nqp::const::CCLASS_NEWLINE, $!str, $!pos, $left);
                my str $found = nqp::substr($!str, $!pos, $nextpos - $!pos);
                $!pos = $nextpos + 1;
                $found;
            }
            method push-all($target --> IterationEnd) {
                my int $left;
                my int $nextpos;

                while ($left = $!chars - $!pos) > 0 {
                    $nextpos = nqp::findcclass(
                      nqp::const::CCLASS_NEWLINE, $!str, $!pos, $left);

                    $target.push(nqp::substr($!str, $!pos, $nextpos - $!pos));
                    $!pos = $nextpos + 1;
                }
            }
        }.new(self));
    }

    method !ensure-split-sanity(\v,\k,\kv,\p) {
        # cannot combine these
        my int $any = ?v + ?k + ?kv + ?p;
        X::Adverb.new(
          what   => 'split',
          source => 'Str',
          nogo   => (:v(v),:k(k),:kv(kv),:p(p)).grep(*.value).map(*.key),
        ).throw if nqp::isgt_i($any,1);
        $any
    }

    method !ensure-limit-sanity(\limit --> Nil) {
        X::TypeCheck.new(
          operation => 'split ($limit argument)',
          expected  => 'any Real type (non-NaN) or Whatever',
          got       => limit.perl,
        ).throw if limit === NaN;

        limit = Inf if nqp::istype(limit,Whatever);
    }

    method parse-base(Str:D: Int:D $radix) {
        fail X::Syntax::Number::RadixOutOfRange.new(:$radix)
            unless 2 <= $radix <= 36; # (0..9,"a".."z").elems == 36

        # do not modify $!value directly as that affects other same strings
        my ($value, $sign, $sign-offset) = $!value, 1, 0;
        given $value.substr(0,1) {
            when '-'|'−' { $sign = -1; $sign-offset = 1 }
            when '+'     {             $sign-offset = 1 }
        }

        if $value.contains('.') { # fractional
            my ($whole, $fract) = $value.split: '.', 2;
            my $w-parsed := nqp::radix_I($radix, $whole, $sign-offset, 0, Int);
            my $f-parsed := nqp::radix_I($radix, $fract, 0,            0, Int);

            # Whole part did not parse in its entirety
            fail X::Str::Numeric.new(
                :source($value),
                :pos($w-parsed[2] max $sign-offset),
                :reason("malformed base-$radix number"),
            ) unless $w-parsed[2] == nqp::chars($whole)
                or nqp::chars($whole) == $sign-offset; # or have no whole part

            # Fractional part did not parse in its entirety
            fail X::Str::Numeric.new(
                :source($value),
                :pos(
                      ($w-parsed[2] max $sign-offset)
                    + 1 # decimal dot
                    + ($f-parsed[2] max 0)
                ),
                :reason("malformed base-$radix number"),
            ) unless $f-parsed[2] == nqp::chars($fract);

            $sign * ($w-parsed[0] + $f-parsed[0]/$f-parsed[1]);
        }
        else { # Int
            my $parsed := nqp::radix_I($radix, $value, $sign-offset, 0, Int);

            # Did not parse the number in its entirety
            fail X::Str::Numeric.new(
                :source($value),
                :pos($parsed[2] max $sign-offset),
                :reason("malformed base-$radix number"),
            ) unless $parsed[2] == nqp::chars($value);

            $sign * $parsed[0];
        }
    }

    multi method split(Str:D: Regex:D $pat, $limit is copy = Inf;;
      :$v is copy, :$k, :$kv, :$p, :$skip-empty) {

        my int $any = self!ensure-split-sanity($v,$k,$kv,$p);

        self!ensure-limit-sanity($limit);
        return Seq.new(Rakudo::Iterator.Empty) if $limit <= 0;

        my \matches = $limit == Inf
          ?? self.match($pat, :g)
          !! self.match($pat, :x(1..$limit-1));

        my str $str   = nqp::unbox_s(self);
        my int $elems = +matches;  # make sure all reified
        return Seq.new(Rakudo::Iterator.OneValue(self)) unless $elems;

        my $matches  := nqp::getattr(matches,List,'$!reified');
        my $result   := nqp::create(IterationBuffer);
        my int $i = -1;
        my int $pos;
        my int $found;

        if $any || $skip-empty {
            my int $notskip = !$skip-empty;
            my int $next;
            while nqp::islt_i(++$i,$elems) {
                my $match := nqp::decont(nqp::atpos($matches,$i));
                $found  = nqp::getattr_i($match,Match,'$!from');
                $next   = $match.to;
                if $notskip {
                    nqp::push($result,
                      nqp::substr($str,$pos,nqp::sub_i($found,$pos)));
                }
                elsif nqp::sub_i($found,$pos) -> $chars {
                    nqp::push($result,
                      nqp::substr($str,$pos,$chars));
                }
                nqp::if(
                  $any,
                  nqp::if(
                    $v,
                    nqp::push($result,$match),                  # v
                    nqp::if(
                      $k,
                      nqp::push($result,0),                     # k
                      nqp::if(
                        $kv,
                        nqp::stmts(
                          nqp::push($result,0),                 # kv
                          nqp::push($result,$match)             # kv
                        ),
                        nqp::push($result, Pair.new(0,$match))  # $p
                      )
                    )
                  )
                );
                $pos = $next;
            }
            nqp::push($result,nqp::substr($str,$pos))
              if $notskip || nqp::islt_i($pos,nqp::chars($str));
        }
        else {
            my $match;
            nqp::setelems($result,$elems + 1);
            while nqp::islt_i(++$i,$elems) {
                $match := nqp::decont(nqp::atpos($matches,$i));
                $found  = nqp::getattr_i($match,Match,'$!from');
                nqp::bindpos($result,$i,
                  nqp::substr($str,$pos,nqp::sub_i($found,$pos)));
                $pos = $match.to;
            }
            nqp::bindpos($result,$i,nqp::substr($str,$pos));
        }

        Seq.new(Rakudo::Iterator.ReifiedList($result))
    }

    multi method split(Str:D: Str(Cool) $match;;
      :$v is copy, :$k, :$kv, :$p, :$skip-empty) {
        my int $any = self!ensure-split-sanity($v,$k,$kv,$p);

        # nothing to work with
        my str $needle = nqp::unbox_s($match);
        my int $chars  = nqp::chars($needle);
        return Seq.new($chars && !$skip-empty
          ?? Rakudo::Iterator.OneValue(self)
          !! Rakudo::Iterator.Empty
        ) unless self.chars;

        # split really, really fast in NQP, also supports ""
        my $matches := nqp::split($needle,nqp::unbox_s(self));

        # interleave the necessary strings if needed
        if $chars {
            if $any {
                my $match-list :=
                     $v  ?? nqp::list($needle)
                  !! $k  ?? nqp::list(0)
                  !! $kv ?? nqp::list(0,$needle)
                  !!        nqp::list(Pair.new(0,$needle)); # $p

                if $match-list {
                    my int $i = nqp::elems($matches);
                    if $skip-empty {
                        nqp::splice($matches,$match-list,$i,

                          nqp::not_i(nqp::isne_i(
                            nqp::chars(nqp::atpos($matches,$i)),0)))
                              while $i = nqp::sub_i($i,1);
                        nqp::splice($matches,$empty,0,1)
                          unless nqp::chars(nqp::atpos($matches,0));
                    }
                    else {
                        nqp::splice($matches,$match-list,$i,0)
                          while $i = nqp::sub_i($i,1);
                    }
                }
            }
            elsif $skip-empty {
                my int $i = nqp::elems($matches);
                my $match-list := nqp::list;
                while nqp::isge_i($i = nqp::sub_i($i,1),0) {
                  nqp::splice($matches,$match-list,$i,1)
                    if nqp::iseq_i(nqp::chars(nqp::atpos($matches,$i)),0);
                }
            }
        }

        # single chars need empty before/after, unless inhibited
        elsif !$skip-empty {
            nqp::unshift($matches,"");
            nqp::push($matches,"");
        }

        Seq.new(Rakudo::Iterator.ReifiedList($matches))
    }

    multi method split(Str:D: Str(Cool) $match, $limit is copy = Inf;;
      :$v is copy, :$k, :$kv, :$p, :$skip-empty) {
        my int $any = self!ensure-split-sanity($v,$k,$kv,$p);

        self!ensure-limit-sanity($limit);
        return Seq.new(Rakudo::Iterator.Empty) if $limit <= 0;

        # nothing to work with
        my int $chars = $match.chars;
        if !self.chars {
            return $chars ?? self.list !! ();
        }

        # nothing to do
        elsif $limit == 1 {
            return self.list;
        }

        # want them all
        elsif $limit == Inf {
            return self.split($match,:$v,:$k,:$kv,:$p,:$skip-empty);
        }

        # we have something to split on
        elsif $chars {

            # let the multi-needle handler handle all nameds
            return self.split(($match,),$limit,:$v,:$k,:$kv,:$p,:$skip-empty)
              if $any || $skip-empty;

            # make the sequence
            Seq.new(class :: does Iterator {
                has str $!string;
                has int $!chars;
                has str $!match;
                has int $!match-chars;
                has int $!todo;
                has int $!pos;
                method !SET-SELF(\string, \match, \todo) {
                    $!string      = nqp::unbox_s(string);
                    $!chars       = nqp::chars($!string);
                    $!match       = nqp::unbox_s(match);
                    $!match-chars = nqp::chars($!match);
                    $!todo        = todo - 1;
                    self
                }
                method new(\string,\match,\todo) {
                    nqp::create(self)!SET-SELF(string,match,todo)
                }
                method !last-part() is raw {
                    my str $string = nqp::substr($!string,$!pos);
                    $!pos  = $!chars + 1;
                    $!todo = 0;
                    nqp::p6box_s($string)
                }
                method !next-part(int $found) is raw {
                    my str $string =
                      nqp::substr($!string,$!pos, $found - $!pos);
                    $!pos = $found + $!match-chars;
                    nqp::p6box_s($string);
                }
                method pull-one() is raw {
                    if $!todo {
                        $!todo = $!todo - 1;
                        my int $found = nqp::index($!string,$!match,$!pos);
                        nqp::islt_i($found,0)
                          ?? nqp::isle_i($!pos,$!chars)
                            ?? self!last-part
                            !! IterationEnd
                          !! self!next-part($found);
                    }
                    else {
                        nqp::isle_i($!pos,$!chars)
                          ?? self!last-part
                          !! IterationEnd
                    }
                }
                method push-all($target --> IterationEnd) {
                    while $!todo {
                        $!todo = $!todo - 1;
                        my int $found = nqp::index($!string,$!match,$!pos);
                        nqp::islt_i($found,0)
                          ?? ($!todo = 0)
                          !! $target.push(self!next-part($found));
                    }
                    $target.push(self!last-part) if nqp::isle_i($!pos,$!chars);
                }
                method sink-all(--> IterationEnd) { }
            }.new(self,$match,$limit));
        }

        # just separate chars
        else {
            Seq.new(class :: does Iterator {
                has str $!string;
                has int $!todo;
                has int $!chars;
                has int $!pos;
                has int $!first;
                has int $!last;
                method !SET-SELF(\string, \todo, \skip-empty) {
                    $!string = nqp::unbox_s(string);
                    $!chars  = nqp::chars($!string);
                    $!todo   = todo;
                    $!first  = !skip-empty;

                    if $!todo > $!chars + 2 {  # will return all chars
                        $!todo = $!chars + 1;
                        $!last = !skip-empty;
                    }
                    else {
                        $!todo = $!todo - 1;
                        $!last = !skip-empty && ($!todo == $!chars + 1);
                    }
                    self
                }
                method new(\string,\todo,\skip-empty) {
                    nqp::create(self)!SET-SELF(string,todo,skip-empty)
                }
                method pull-one() is raw {
                    if $!first {             # do empty string first
                        $!first = 0;
                        $!todo  = $!todo - 1;
                        ""
                    }
                    elsif $!todo {           # next char
                        $!todo = $!todo - 1;
                        nqp::p6box_s(nqp::substr($!string,$!pos++,1))
                    }
                    elsif $!last {           # do final empty string
                        $!last = 0;
                        ""
                    }
                    elsif nqp::islt_i($!pos,$!chars) {  # do rest of string
                        my str $rest = nqp::substr($!string,$!pos);
                        $!pos = $!chars;
                        nqp::p6box_s($rest)
                    }
                    else {
                        IterationEnd
                    }
                }
                method push-all($target --> IterationEnd) {
                    $target.push("") if $!first;
                    $!todo = $!todo - 1;
                    while $!todo {
                        $target.push(
                          nqp::p6box_s(nqp::substr($!string,$!pos++,1)));
                        $!todo = $!todo - 1;
                    }
                    $target.push( nqp::p6box_s(nqp::substr($!string,$!pos)))
                      if nqp::islt_i($!pos,$!chars);
                    $target.push("") if $!last;
                }
                method count-only() { nqp::p6box_i($!todo + $!first + $!last) }
                method bool-only() { nqp::p6bool($!todo + $!first + $!last) }
                method sink-all(--> IterationEnd) { }
            }.new(self,$limit,$skip-empty));
        }
    }

    multi method split(Str:D: @needles, $parts is copy = Inf;;
       :$v is copy, :$k, :$kv, :$p, :$skip-empty) {
        my int $any = self!ensure-split-sanity($v,$k,$kv,$p);

        # must all be Cool, otherwise we'll just use a regex
        return self.split(rx/ @needles /,:$v,:$k,:$kv,:$p,:$skip-empty) # / hl
          unless Rakudo::Internals.ALL_TYPE(@needles,Cool);

        self!ensure-limit-sanity($parts);
        return Seq.new(Rakudo::Iterator.Empty) if $parts <= 0;

        my int $limit = $parts.Int
          unless nqp::istype($parts,Whatever) || $parts == Inf;

        my str $str       = nqp::unbox_s(self);
        my $positions    := nqp::list;
        my $needles      := nqp::list_s;
        my $needle-chars := nqp::list_i;
        my $needles-seen := nqp::hash;
        my int $tried;
        my int $fired;

        # search using all needles
        my int $index = 0;
        for @needles -> $needle {
            my str $need  = nqp::unbox_s($needle.DEFINITE ?? $needle.Str !! "");
            my int $chars = nqp::chars($need);
            nqp::push_s($needles,$need);
            nqp::push_i($needle-chars,$chars);

            # search for this needle if there is one, and not done before
            nqp::if(
              nqp::isgt_i($chars,0)
                && nqp::not_i(nqp::existskey($needles-seen,$need)),
              nqp::stmts(
                nqp::bindkey($needles-seen,$need,1),
                (my int $pos),
                (my int $i),
                (my int $seen = nqp::elems($positions)),
                nqp::if(
                  nqp::isgt_i($limit,0),  # 0 = no limit
                  nqp::stmts(
                    (my int $todo = $limit),
                    nqp::while(
                      nqp::isge_i(($todo = nqp::sub_i($todo,1)),0)
                        && nqp::isge_i($i = nqp::index($str,$need,$pos),0),
                      nqp::stmts(
                        nqp::push($positions,nqp::list_i($i,$index)),
                        ($pos = nqp::add_i($i,1)),
                      )
                    )
                  ),
                  nqp::while(
                    nqp::isge_i($i = nqp::index($str,$need,$pos),0),
                    nqp::stmts(
                      nqp::push($positions,nqp::list_i($i,$index)),
                      ($pos = nqp::add_i($i,1))
                    )
                  )
                ),
                ($tried = nqp::add_i($tried,1)),
                ($fired =
                  nqp::add_i($fired,nqp::isge_i(nqp::elems($positions),$seen)))
              )
            );
            $index = nqp::add_i($index, 1);
        }

        # no needle tried, assume we want chars
        return self.split("",$limit) if nqp::not_i($tried);

        # sort by position if more than one needle fired
        $positions := nqp::getattr(
          Rakudo::Sorting.MERGESORT-REIFIED-LIST-WITH(
            nqp::p6bindattrinvres(
              nqp::create(List),List,'$!reified',$positions
            ),
            -> \a, \b {
                nqp::cmp_i(
                  nqp::atpos_i(a,0),
                  nqp::atpos_i(b,0)
                ) || nqp::cmp_i(
                  nqp::atpos_i($needle-chars,nqp::atpos_i(b,1)),
                  nqp::atpos_i($needle-chars,nqp::atpos_i(a,1))
                )
            }
          ),
          List,
          '$!reified'
        ) if nqp::isgt_i($fired,1);

        # remove elements we don't want
        if nqp::isgt_i($limit,0) {
            nqp::stmts(
              (my int $limited = 1),   # split one less than entries returned
              (my int $elems = nqp::elems($positions)),
              (my int $pos),
              (my int $i = -1),
              nqp::while(
                nqp::islt_i(($i = nqp::add_i($i,1)),$elems)
                  && nqp::islt_i($limited,$limit),
                nqp::if(
                  nqp::isge_i(   # not hidden by other needle
                    nqp::atpos_i(nqp::atpos($positions,$i),0),
                    $pos
                  ),
                  nqp::stmts(
                    ($limited = nqp::add_i($limited,1)),
                    ($pos = nqp::add_i(
                      nqp::atpos_i(nqp::atpos($positions,$i),0),
                      nqp::atpos_i($needle-chars,
                        nqp::atpos_i(nqp::atpos($positions,$i),1))
                    ))
                  )
                )
              ),
              nqp::if(
                nqp::islt_i($i,$elems),
                nqp::splice($positions,$empty,
                  $i,nqp::sub_i(nqp::elems($positions),$i))
              )
            )
        }

        # create the final result
        my int $skip = ?$skip-empty;
        my int $pos = 0;
        my $result := nqp::create(IterationBuffer);
        if $any {
            nqp::stmts(
              (my int $i = -1),
              (my int $elems = nqp::elems($positions)),
              nqp::while(
                nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                nqp::if(
                  nqp::isge_i( # not hidden by other needle
                    (my int $from = nqp::atpos_i(
                      (my $pair := nqp::atpos($positions,$i)),0)
                    ),
                    $pos
                  ),
                  nqp::stmts(
                    (my int $needle-index = nqp::atpos_i($pair,1)),
                    nqp::unless(
                      $skip && nqp::iseq_i($from,$pos),
                      nqp::push($result,
                        nqp::substr($str,$pos,nqp::sub_i($from,$pos)))
                    ),
                    nqp::if($k || $kv,
                      nqp::push($result,nqp::clone($needle-index))
                    ),
                    nqp::if($v || $kv,
                      nqp::push($result,nqp::atpos_s($needles,$needle-index))
                    ),
                    nqp::if($p,
                      nqp::push($result,Pair.new(
                        $needle-index,nqp::atpos_s($needles,$needle-index)))
                    ),
                    ($pos = nqp::add_i(
                      $from,
                      nqp::atpos_i($needle-chars,$needle-index)
                    ))
                  )
                )
              )
            )
        }
        else {
            nqp::stmts(
              (my int $i = -1),
              (my int $elems = nqp::elems($positions)),
              nqp::while(
                nqp::islt_i(($i = nqp::add_i($i,1)),$elems),
                nqp::if(
                  nqp::isge_i( # not hidden by other needle
                    (my int $from = nqp::atpos_i(
                      (my $pair := nqp::atpos($positions,$i)),0)
                    ),
                    $pos
                  ),
                  nqp::stmts(
                    nqp::unless(
                      $skip && nqp::iseq_i($from,$pos),
                      nqp::push($result,
                        nqp::substr($str,$pos,nqp::sub_i($from,$pos))),
                    ),
                    ($pos = nqp::add_i($from,
                      nqp::atpos_i($needle-chars,nqp::atpos_i($pair,1))
                    ))
                  )
                )
              )
            )
        }
        nqp::push($result,nqp::substr($str,$pos))
          unless $skip && nqp::iseq_i($pos,nqp::chars($str));

        Seq.new(Rakudo::Iterator.ReifiedList($result))
    }

    # Note that in these same* methods, as used by s/LHS/RHS/, the
    # pattern is actually the original string matched by LHS, while the
    # invocant "original" is really the replacement RHS part.  Confusing...
    method samecase(Str:D: Str:D $pattern) {
        nqp::if(
          nqp::chars(nqp::unbox_s($pattern)),        # something to work with
          nqp::stmts(
            (my $result := nqp::list_s),
            (my $cases  := nqp::getattr($pattern,Str,'$!value')),
            (my int $base-chars  = nqp::chars($!value)),
            (my int $cases-chars = nqp::if(
              nqp::isgt_i(nqp::chars($cases),$base-chars),
              $base-chars,
              nqp::chars($cases)
            )),
            (my int $i = 0),
            (my int $j = 0),
            (my int $prev-case = nqp::if(            # set up initial case
              nqp::iscclass(nqp::const::CCLASS_LOWERCASE,$cases,0),
              -1,
              nqp::iscclass(nqp::const::CCLASS_UPPERCASE,$cases,0)
            )),

            nqp::while(                              # other chars in pattern
              nqp::islt_i(($i = nqp::add_i($i,1)),$cases-chars),
              nqp::stmts(
                (my int $case = nqp::if(             # -1 =lc, 1 = uc, 0 = else
                  nqp::iscclass(nqp::const::CCLASS_LOWERCASE,$cases,$i),
                  -1,
                  nqp::iscclass(nqp::const::CCLASS_UPPERCASE,$cases,$i)
                )),
                nqp::if(
                  nqp::isne_i($case,$prev-case),
                  nqp::stmts(                        # seen a change
                    nqp::push_s($result,nqp::if(
                      nqp::iseq_i($prev-case,-1),    # coming from lc
                      nqp::lc(nqp::substr($!value,$j,nqp::sub_i($i,$j))),
                      nqp::if(
                        nqp::iseq_i($prev-case,1),   # coming from uc
                        nqp::uc(nqp::substr($!value,$j,nqp::sub_i($i,$j))),
                        nqp::substr($!value,$j,nqp::sub_i($i,$j))
                      )
                    )),
                    ($prev-case = $case),
                    ($j         = $i)
                  )
                )
              )
            ),

            nqp::if(                                 # something left
              nqp::islt_i($j,$base-chars),
              nqp::push_s($result,nqp::if(
                nqp::iseq_i($prev-case,-1),          # must become lc
                nqp::lc(nqp::substr($!value,$j,nqp::sub_i($base-chars,$j))),
                nqp::if(
                  nqp::iseq_i($prev-case,1),         # must become uc
                  nqp::uc(nqp::substr($!value,$j,nqp::sub_i($base-chars,$j))),
                  nqp::substr($!value,$j,nqp::sub_i($base-chars,$j))
                )
              ))
            ),

            nqp::join("",$result)                    # wrap it up
          ),

          self                                       # nothing to be done
        )
    }

#?if moar
    method samemark(Str:D: Str:D $pattern) {
        nqp::if(
          nqp::chars(nqp::unbox_s($pattern)),        # something to work with
          nqp::stmts(
            (my $base   := nqp::split("",$!value)),
            (my $marks  := nqp::split("",nqp::unbox_s($pattern))),
            (my int $base-elems  = nqp::elems($base)),
            (my int $marks-elems = nqp::elems($marks) min $base-elems),
            (my $result := nqp::setelems(nqp::list_s,$base-elems)),

            (my int $i = -1),
            nqp::while(                               # for all marks
              nqp::islt_i(($i = nqp::add_i($i,1)),$marks-elems),
              nqp::bindpos_s($result,$i,              # store the result of:
                nqp::stmts(
                  (my $marks-nfd := nqp::strtocodes(  # char + accents of mark
                    nqp::atpos($marks,$i),
                    nqp::const::NORMALIZE_NFD,
                    nqp::create(NFD)
                  )),
                  nqp::shift_i($marks-nfd),           # lose the char
                  (my $marks-base := nqp::strtocodes( # char + accents of base
                    nqp::atpos($base,$i),
                    nqp::const::NORMALIZE_NFD,
                    nqp::create(NFD)
                  )),
                  nqp::strfromcodes(                  # join base+rest of marks
                    nqp::splice(
                      $marks-base,
                      $marks-nfd,
                      1,
                      nqp::sub_i(nqp::elems($marks-base),1)
                    )
                  )
                )
              )
            ),

            ($i = nqp::sub_i($i,1)),
            nqp::while(                               # remaining base chars
              nqp::islt_i(($i = nqp::add_i($i,1)),$base-elems),
              nqp::bindpos_s($result,$i,              # store the result of:
                nqp::stmts(
                  ($marks-base := nqp::strtocodes(    # char+all accents of base
                    nqp::atpos($base,$i),
                    nqp::const::NORMALIZE_NFD,
                    nqp::create(NFD)
                  )),
                  nqp::strfromcodes(                  # join base+rest of marks
                    nqp::splice(
                      $marks-base,
                      $marks-nfd, # NOTE: state of last iteration previous loop
                      1,
                      nqp::sub_i(nqp::elems($marks-base),1)
                    )
                  )
                )
              )
            ),

            nqp::join("",$result)                     # wrap it up
          ),

          self                                        # nothing to be done
        )
    }
#?endif
#?if jvm
    method samemark(Str:D: Str:D $pattern) { X::NYI.new(:feature<samemark>).throw }
#?endif

    method samespace(Str:D: Str:D $pattern) { self.word-by-word($pattern, :samespace) }

    method word-by-word(Str:D: Str:D $pattern, &filter?, Bool :$samespace) {
        my str $str = nqp::unbox_s(self);
        my str $pat = nqp::unbox_s($pattern);
        my Mu $ret := nqp::list_s;

        my int $chars = nqp::chars($str);
        my int $pos = 0;
        my int $nextpos;
        my int $patchars = nqp::chars($pat);
        my int $patpos = 0;
        my int $patnextpos;
        my int $left;
        my $patword;

        # Still something to look for?
        while ($left = $chars - $pos) > 0 {

            $nextpos = nqp::findcclass(
              nqp::const::CCLASS_WHITESPACE, $str, $pos, $left);

            $patnextpos = nqp::findcclass(nqp::const::CCLASS_WHITESPACE, $pat, $patpos, $patchars - $patpos);

            if &filter {
                # We latch on last pattern word if pattern runs out of words first.
                $patword := nqp::p6box_s(nqp::substr($pat, $patpos, $patnextpos - $patpos)) if $patpos < $patchars;
                nqp::push_s($ret, nqp::unbox_s(filter(nqp::substr($str, $pos, $nextpos - $pos), $patword)));
            }
            else {
                nqp::push_s($ret, nqp::substr($str, $pos, $nextpos - $pos));
            }

            # Did we have the last word?
            last if $nextpos >= $chars;

            $pos = nqp::findnotcclass( nqp::const::CCLASS_WHITESPACE,
              $str, $nextpos, $chars - $nextpos);
            if $patnextpos >= $patchars {  # No more pat space, just copy original space.
                nqp::push_s($ret,
                  nqp::substr($str, $nextpos, $pos - $nextpos));
                $patpos = $patnextpos;
            }
            else {  # Traverse pat space, use if wanted
                $patpos = nqp::findnotcclass( nqp::const::CCLASS_WHITESPACE,
                  $pat, $patnextpos, $patchars - $patnextpos);

                if $samespace {  # Carry over pattern space?
                    nqp::push_s($ret,
                      nqp::substr($pat, $patnextpos, $patpos - $patnextpos));
                }
                else {   # Nope, just use original space.
                    nqp::push_s($ret,
                      nqp::substr($str, $nextpos, $pos - $nextpos));
                }
            }
        }

        nqp::join("",$ret)
    }

    method trim-leading(Str:D:) {
        my str $str = nqp::unbox_s(self);
        my int $pos = nqp::findnotcclass(
                          nqp::const::CCLASS_WHITESPACE,
                          $str, 0, nqp::chars($str));
        $pos ?? nqp::p6box_s(nqp::substr($str, $pos)) !! self;
    }

    method trim-trailing(Str:D:) {
        my str $str = nqp::unbox_s(self);
        my int $pos = nqp::chars($str) - 1;
        $pos = $pos - 1
            while nqp::isge_i($pos, 0)
               && nqp::iscclass(nqp::const::CCLASS_WHITESPACE, $str, $pos);
        nqp::islt_i($pos, 0) ?? '' !! nqp::p6box_s(nqp::substr($str, 0, $pos + 1));
    }

    method trim(Str:D:) {
        my str $str  = nqp::unbox_s(self);
        my int $pos  = nqp::chars($str) - 1;
        my int $left = nqp::findnotcclass(
                           nqp::const::CCLASS_WHITESPACE, $str, 0, $pos + 1);
        $pos = $pos - 1
            while nqp::isge_i($pos, $left)
               && nqp::iscclass(nqp::const::CCLASS_WHITESPACE, $str, $pos);
        nqp::islt_i($pos, $left) ?? '' !! nqp::p6box_s(nqp::substr($str, $left, $pos + 1 - $left));
    }

    proto method words(|) {*}
    multi method words(Str:D: :$autoderef!) { # in Actions.postprocess_words
        my @list := self.words.List;
        return @list == 1 ?? @list[0] !! @list;
    }
    multi method words(Str:D: $limit) {
        nqp::istype($limit,Whatever) || $limit == Inf
          ?? self.words
          !! self.words.head($limit)
    }
    multi method words(Str:D:) {
        Seq.new(class :: does Iterator {
            has str $!str;
            has int $!chars;
            has int $!pos;

            method !SET-SELF(\string) {
                $!str   = nqp::unbox_s(string);
                $!chars = nqp::chars($!str);
                $!pos   = nqp::findnotcclass(
                  nqp::const::CCLASS_WHITESPACE, $!str, 0, $!chars);
                self
            }
            method new(\string) { nqp::create(self)!SET-SELF(string) }
            method pull-one() {
                my int $left;
                my int $nextpos;

                if ($left = $!chars - $!pos) > 0 {
                    $nextpos = nqp::findcclass(
                      nqp::const::CCLASS_WHITESPACE, $!str, $!pos, $left);

                    my str $found =
                      nqp::substr($!str, $!pos, $nextpos - $!pos);
                    $!pos = nqp::findnotcclass( nqp::const::CCLASS_WHITESPACE,
                      $!str, $nextpos, $!chars - $nextpos);

                    return nqp::p6box_s($found);
                }
                IterationEnd
            }
            method push-all($target --> IterationEnd) {
                my int $left;
                my int $nextpos;

                while ($left = $!chars - $!pos) > 0 {
                    $nextpos = nqp::findcclass(
                      nqp::const::CCLASS_WHITESPACE, $!str, $!pos, $left);

                    $target.push(nqp::p6box_s(
                      nqp::substr($!str, $!pos, $nextpos - $!pos)
                    ));
                    $!pos = nqp::findnotcclass( nqp::const::CCLASS_WHITESPACE,
                      $!str, $nextpos, $!chars - $nextpos);
                }
            }
        }.new(self));
    }

    proto method encode(|) {*}
    multi method encode(Str:D $encoding = 'utf8', :$replacement, Bool() :$translate-nl = False, :$strict) {
        Encoding::Registry.find($encoding)
            .encoder(:$replacement, :$translate-nl, :$strict)
            .encode-chars(self)
    }

#?if moar
    method NFC() {
        nqp::strtocodes(nqp::unbox_s(self), nqp::const::NORMALIZE_NFC, nqp::create(NFC))
    }
    method NFD() {
        nqp::strtocodes(nqp::unbox_s(self), nqp::const::NORMALIZE_NFD, nqp::create(NFD))
    }
    method NFKC() {
        nqp::strtocodes(nqp::unbox_s(self), nqp::const::NORMALIZE_NFKC, nqp::create(NFKC))
    }
    method NFKD() {
        nqp::strtocodes(nqp::unbox_s(self), nqp::const::NORMALIZE_NFKD, nqp::create(NFKD))
    }
#?endif
#?if jvm
    method NFC()  { X::NYI.new(:feature<NFC>).throw }
    method NFD()  { X::NYI.new(:feature<NFD>).throw }
    method NFKC() { X::NYI.new(:feature<NFKC>).throw }
    method NFKD() { X::NYI.new(:feature<NFKD>).throw }
#?endif

    method wordcase(Str:D: :&filter = &tclc, Mu :$where = True) {
        self.subst(:g, / [<:L> \w* ] +% <['\-]> /, -> $m {  # ' highlighting
            my Str $s = $m.Str;
            $s ~~ $where ?? filter($s) !! $s;
        });
    }

    proto method trans(|) { $/ := nqp::getlexcaller('$/'); {*} }
    multi method trans(Str:D: Pair:D \what, *%n) {
        my $from = what.key;
        my $to   = what.value;
        $/ := nqp::getlexcaller('$/');

        return self.trans((what,), |%n)
          if !nqp::istype($from,Str)   # from not a string
          || !$from.defined            # or a type object
          || !nqp::istype($to,Str)     # or to not a string
          || !$to.defined              # or a type object
          || %n;                       # or any named params passed

        # from 1 char
        return Rakudo::Internals.TRANSPOSE(self, $from, substr($to,0,1))
          if $from.chars == 1;

        my str $sfrom  = Rakudo::Internals.EXPAND-LITERAL-RANGE($from,0);
        my str $str    = nqp::unbox_s(self);
        my str $chars  = nqp::chars($str);
        my Mu $result := nqp::list_s();
        my str $check;
        my int $i = -1;

        # something to convert to
        if $to.chars -> $tochars {
            nqp::setelems($result,$chars);

            # all convert to one char
            if $tochars == 1 {
                my str $sto = nqp::unbox_s($to);

                while nqp::islt_i(++$i,$chars) {
                    $check = nqp::substr($str,$i,1);
                    nqp::bindpos_s(
                      $result, $i, nqp::iseq_i(nqp::index($sfrom,$check),-1)
                        ?? $check
                        !! $sto
                    );
                }
            }

            # multiple chars to convert to
            else {
                my str $sto = Rakudo::Internals.EXPAND-LITERAL-RANGE($to,0);
                my int $sfl = nqp::chars($sfrom);
                my int $found;

                # repeat until mapping complete
                $sto = $sto ~ $sto while nqp::islt_i(nqp::chars($sto),$sfl);

                while nqp::islt_i(++$i,$chars) {
                    $check = nqp::substr($str,$i,1);
                    $found = nqp::index($sfrom,$check);
                    nqp::bindpos_s($result, $i, nqp::iseq_i($found,-1)
                      ?? $check
                      !! nqp::substr($sto,$found,1)
                    );
                }
            }
        }

        # just remove
        else {
            while nqp::islt_i(++$i,$chars) {
                $check = nqp::substr($str,$i,1);
                nqp::push_s($result, $check)
                  if nqp::iseq_i(nqp::index($sfrom,$check),-1);
            }
        }

        nqp::p6box_s(nqp::join('',$result));
    }

    my class LSM {
        has str $!source;
        has     $!substitutions;
        has int $!squash;
        has int $!complement;
        has str $!prev_result;

        has int $!index;
        has int $!next_match;
        has int $!substitution_length;

        has $!first_substitution; # need this one for :c with arrays
        has $!next_substitution;
        has $!match_obj;
        has $!last_match_obj;

        has str $!unsubstituted_text;
        has str $!substituted_text;

        method !SET-SELF(\source,\substitutions,\squash,\complement) {
            $!source         = nqp::unbox_s(source);
            $!substitutions := nqp::getattr(substitutions,List,'$!reified');
            $!squash         = ?squash;
            $!complement     = ?complement;
            $!prev_result    = '';
            self
        }
        method new(\source,\substitutions,\squash,\complement) {
            nqp::create(self)!SET-SELF(source,substitutions,squash,complement)
        }

        method !compare_substitution(
          $substitution, int $pos, int $length --> Nil
        ) {
            if nqp::isgt_i($!next_match,$pos)
              || nqp::iseq_i($!next_match,$pos)
                   && nqp::islt_i($!substitution_length,$length) {

                $!next_match          = $pos;
                $!substitution_length = $length;
                $!next_substitution   = $substitution;
                $!match_obj           = $!last_match_obj;
            }
        }

        method !increment_index($s --> Nil) {
            $/ := nqp::getlexcaller('$/');
            if nqp::istype($s,Regex) {
                $!index = $!next_match + (
                    substr($!source,$!index) ~~ $s ?? $/.chars !! 0
                );
                $!last_match_obj = $/;
            }
            else {
                $!index = $!next_match
                  + nqp::chars(nqp::istype($s,Str) ?? $s !! $s.Str);
            }
        }

        # note: changes outer $/
        method get_next_substitution_result {
            my $value = $!complement
              ?? $!first_substitution.value
              !! $!next_substitution.value;

            my $outer_slash := nqp::getlexcaller('$/');
            $/ := nqp::getlexcaller('$/');
            $outer_slash = $!match_obj;

            my str $result = nqp::istype($value,Callable)
              ?? $value().Str
              !! nqp::istype($value,Str)
                ?? $value
                !! $value.Str;
            my str $orig_result = $result;

            $result = ''
              if $!squash
              && nqp::chars($!prev_result)
              && nqp::iseq_s($!prev_result,$result)
              && nqp::iseq_s($!unsubstituted_text,'');

            $!prev_result = $orig_result;
            $result
        }

        method next_substitution() {
            $/ := nqp::getlexcaller('$/');
            $!next_match = nqp::chars($!source);
            $!first_substitution = nqp::atpos($!substitutions,0)
              unless nqp::defined($!first_substitution);

            # triage substitutions left to do
            my $todo := nqp::list;
            my $iter := nqp::iterator($!substitutions);
            while $iter {
                my $this := nqp::shift($iter);
                my $key  := $this.key;
                if nqp::istype($key,Regex) {
                    if $!source.match($key, :continue($!index)) -> \m {
                        $!last_match_obj = $/;
                        self!compare_substitution($this, m.from, m.to - m.from);
                        nqp::push($todo,$this);
                    }
                }
                elsif nqp::istype($key,Cool) {
                    my str $skey = nqp::istype($key,Str) ?? $key !! $key.Str;
                    my int $pos  = nqp::index($!source,$skey,$!index);
                    if nqp::isge_i($pos,0) {
                        self!compare_substitution($this,$pos,nqp::chars($skey));
                        nqp::push($todo,$this);
                    }
                }
                else {
                    X::Str::Trans::IllegalKey.new(key => $this).throw;
                }
            }
            $!substitutions := $todo;

            $!unsubstituted_text =
              nqp::substr($!source,$!index,$!next_match - $!index);
            if $!next_substitution.defined {
                if $!complement {
                    my $oldidx = $!index;
                    if nqp::chars($!unsubstituted_text) -> \todo {
                        my $result = self.get_next_substitution_result;
                        self!increment_index($!next_substitution.key);
                        $!substituted_text = nqp::substr(
                          $!source,
                          $oldidx + todo,
                          $!index - $oldidx - todo,
                        );
                        $!unsubstituted_text = $!squash
                          ?? $result
                          !! $result x todo;
                    }
                    else {
                        return if $!next_match == nqp::chars($!source);
                        my $result = self.get_next_substitution_result;
                        self!increment_index($!next_substitution.key);
                        $!substituted_text = '';
                        $!unsubstituted_text =
                          nqp::substr($!source,$oldidx,$!index - $oldidx);
                    }
                }
                else {
                    return if $!next_match == nqp::chars($!source);
                    $!substituted_text = self.get_next_substitution_result;
                    self!increment_index($!next_substitution.key);
                }
            }

            nqp::islt_i($!next_match,nqp::chars($!source))
              && nqp::elems($!substitutions)
        }

        method result() {
            $/ := nqp::getlexcaller('$/');
            my Mu $result := nqp::list_s;

            while self.next_substitution {
                nqp::push_s($result,$!unsubstituted_text);
                nqp::push_s($result,$!substituted_text);
            }
            nqp::push_s($result,$!unsubstituted_text);
            nqp::p6box_s(nqp::join('', $result))
        }
    }
    multi method trans(Str:D:
      *@changes, :c(:$complement), :s(:$squash), :d(:$delete)) {

        # nothing to do
        return self unless self.chars;

        $/ := nqp::getlexcaller('$/');

        my sub myflat(*@s) {
            @s.map: { nqp::istype($_, Iterable) ?? .list.Slip !! $_ }
        }
        my sub expand($s) {
            nqp::istype($s,Iterable) || nqp::istype($s,Positional)
              ?? (my @ = myflat($s.list).Slip)
              !! Rakudo::Internals.EXPAND-LITERAL-RANGE($s,1)
        }

        my int $just-strings = !$complement && !$squash;
        my int $just-chars   = $just-strings;
        my $needles := nqp::list;
        my $pins    := nqp::list;

        my $substitutions := nqp::list;
        for @changes -> $p {
            X::Str::Trans::InvalidArg.new(got => $p).throw
              unless nqp::istype($p,Pair);

            my $key   := $p.key;
            my $value := $p.value;
            if nqp::istype($key,Regex) {
                $just-strings = 0;
                nqp::push($substitutions,$p);
            }
            elsif nqp::istype($value,Callable) {
                $just-strings = 0;
                nqp::push($substitutions,Pair.new($_,$value)) for expand $key;
            }
            else {
                my $from := nqp::getattr(expand($key),  List,'$!reified');
                my $to   := nqp::getattr(expand($value),List,'$!reified');
                my $from-elems = nqp::elems($from);
                my $to-elems   = nqp::elems($to);
                my $padding = $delete
                  ?? ''
                  !! $to-elems
                    ?? nqp::atpos($to,$to-elems - 1)
                    !! '';

                my int $i = -1;
                while nqp::islt_i($i = $i + 1,$from-elems) {
                    my $key   := nqp::atpos($from,$i);
                    my $value := nqp::islt_i($i,$to-elems)
                      ?? nqp::atpos($to,$i)
                      !! $padding;
                    nqp::push($substitutions,Pair.new($key,$value));
                    if $just-strings {
                        if nqp::istype($key,Str) && nqp::istype($value,Str) {
                            $key := nqp::unbox_s($key);
                            $just-chars = 0 if nqp::isgt_i(nqp::chars($key),1);
                            nqp::push($needles,$key);
                            nqp::push($pins,nqp::unbox_s($value));
                        }
                        else {
                            $just-strings = 0;
                        }
                    }
                }
            }
        }

        # can do special cases for just strings
        if $just-strings {

            # only need to go through string once
            if $just-chars {
                my $lookup   := nqp::hash;
                my int $elems = nqp::elems($needles);
                my int $i     = -1;
                nqp::bindkey($lookup,
                  nqp::atpos($needles,$i),nqp::atpos($pins,$i))
                  while nqp::islt_i($i = $i + 1,$elems);

                my $result := nqp::split("",nqp::unbox_s(self));
                $i = -1;
                $elems = nqp::elems($result);
                nqp::bindpos($result,$i,
                  nqp::atkey($lookup,nqp::atpos($result,$i)))
                    if nqp::existskey($lookup,nqp::atpos($result,$i))
                  while nqp::islt_i($i = $i + 1,$elems);
                nqp::join("",$result)
            }

            # use multi-needle split with in-place mapping
            else {
                nqp::stmts(
                  (my $iterator := self.split($needles,:k).iterator),
                  (my $strings := nqp::list_s($iterator.pull-one)),
                  nqp::until(
                    nqp::eqaddr((my $i := $iterator.pull-one),IterationEnd),
                    nqp::stmts(
                      nqp::push_s($strings,nqp::atpos($pins,$i)),
                      nqp::push_s($strings,$iterator.pull-one)
                    )
                  ),
                  nqp::join("",$strings)
                )
            }
        }

        # alas, need to use more complex route
        else {
            LSM.new(self,$substitutions,$squash,$complement).result;
        }
    }

    method parse-names(Str:D:) {
        # XXX TODO: issue deprecation warning in 6.d; remove in 6.e
        self.uniparse
    }
    method uniparse(Str:D:) {
        my     \names := nqp::split(',', self);
        my int $elems  = nqp::elems(names);
        my int $i      = -1;
        my str $res    = '';
        nqp::while(
            nqp::islt_i( ($i = nqp::add_i($i,1)), $elems ),
            ($res = nqp::concat($res,
                nqp::unless(
                    nqp::strfromname(nqp::atpos(names, $i).trim),
                    X::Str::InvalidCharName.new(
                        :name(nqp::atpos(names, $i).trim)
                    ).fail
            ))),
        );
        $res
    }

    proto method indent($) {*}
    # Zero indent does nothing
    multi method indent(Int() $steps where { $_ == 0 }) {
        self;
    }

    # Positive indent does indent
    multi method indent(Int() $steps where { $_ > 0 }) {
    # We want to keep trailing \n so we have to .comb explicitly instead of .lines
        self.comb(/:r ^^ \N* \n?/).map({
            given $_.Str {
                when /^ \n? $ / {
                    $_;
                }
                # Use the existing space character if they're all the same
                # (but tabs are done slightly differently)
                when /^(\t+) ([ \S .* | $ ])/ {
                    $0 ~ "\t" x ($steps div $?TABSTOP) ~
                         ' '  x ($steps mod $?TABSTOP) ~ $1
                }
                when /^(\h) $0* [ \S | $ ]/ {
                    $0 x $steps ~ $_
                }

                # Otherwise we just insert spaces after the existing leading space
                default {
                    $_ ~~ /^(\h*) (.*)$/;
                    $0 ~ (' ' x $steps) ~ $1
                }
            }
        }).join;
    }

    # Negative indent (de-indent)
    multi method indent(Int() $steps where { $_ < 0 }) {
        de-indent(self, $steps);
    }

    # Whatever indent (de-indent)
    multi method indent(Whatever $steps) {
        de-indent(self, $steps);
    }

    sub de-indent($obj, $steps) {
        # Loop through all lines to get as much info out of them as possible
        my @lines = $obj.comb(/:r ^^ \N* \n?/).map({
            # Split the line into indent and content
            my ($indent, $rest) = @($_ ~~ /^(\h*) (.*)$/);

            # Split the indent into characters and annotate them
            # with their visual size
            my $indent-size = 0;
            my @indent-chars = $indent.comb.map(-> $char {
                my $width = $char eq "\t"
                    ?? $?TABSTOP - ($indent-size mod $?TABSTOP)
                    !! 1;
                $indent-size += $width;
                $char => $width;
            }).eager;

            { :$indent-size, :@indent-chars, :rest(~$rest) };
        });

        # Figure out the amount * should de-indent by, we also use this for warnings
        my $common-prefix = min @lines.grep({ .<indent-size> ||  .<rest> ~~ /\S/}).map({ $_<indent-size> });
        return $obj if $common-prefix === Inf;

        # Set the actual de-indent amount here
        my Int $de-indent = nqp::istype($steps,Whatever)
          ?? $common-prefix
          !! -$steps;

        warn "Asked to remove $de-indent spaces, but the shortest indent is $common-prefix spaces"
            if $de-indent > $common-prefix;

        # Work forwards from the left end of the indent whitespace, removing
        # array elements up to # (or over, in the case of tab-explosion)
        # the specified de-indent amount.
        @lines.map(-> $l {
            my $pos = 0;
            while $l<indent-chars> and $pos < $de-indent {
                if $l<indent-chars>.shift.key eq "\t" {
                    $pos -= $pos % $?TABSTOP;
                    $pos += $?TABSTOP;
                } else {
                    ++$pos
                }
            }
            if $l<indent-chars> and $pos % $?TABSTOP {
                my $check = $?TABSTOP - $pos % $?TABSTOP;
                $check = $l<indent-chars>[lazy 0..^$check].first(*.key eq "\t",:k);
                with $check {
                    $l<indent-chars>.shift for 0..$check;
                    $pos -= $pos % $?TABSTOP;
                    $pos += $?TABSTOP;
                }
            }
            $l<indent-chars>».key.join ~ ' ' x ($pos - $de-indent) ~ $l<rest>;
        }).join;
    }

    multi method substr(Str:D:) {
        die "Must at least specify a 'from' value with 'substr'"
    }
    multi method substr(Str:D: Int:D \start) {
        nqp::if(
          nqp::islt_i((my int $from = nqp::unbox_i(start)),0)
            || nqp::isgt_i($from,nqp::chars($!value)),
          Rakudo::Internals.SUBSTR-START-OOR($from,nqp::chars($!value)),
          nqp::substr($!value,$from)
        )
    }
    multi method substr(Str:D: Int:D \start, Int:D \want) {
        nqp::if(
          nqp::islt_i((my int $from = nqp::unbox_i(start)),0)
            || nqp::isgt_i($from,nqp::chars($!value)),
          Rakudo::Internals.SUBSTR-START-OOR($from,nqp::chars($!value)),
          nqp::if(
            nqp::islt_i((my int $chars = nqp::unbox_i(want)),0),
            Rakudo::Internals.SUBSTR-CHARS-OOR($chars),
            nqp::substr($!value,$from,$chars)
          )
        )
    }
    multi method substr(Str:D: Int:D \start, Callable:D \want) {
        nqp::if(
          nqp::islt_i((my int $from = nqp::unbox_i(start)),0)
            || nqp::isgt_i($from,nqp::chars($!value)),
          Rakudo::Internals.SUBSTR-START-OOR($from,nqp::chars($!value)),
          nqp::if(
            nqp::islt_i((my int $chars = (want)(nqp::chars($!value) - $from).Int),0),
            Rakudo::Internals.SUBSTR-CHARS-OOR($chars),
            nqp::substr($!value,$from,$chars)
          )
        )
    }
    multi method substr(Str:D: Callable:D \start) {
        nqp::if(
          nqp::islt_i((my int $from = (start)(nqp::chars($!value)).Int),0)
            || nqp::isgt_i($from,nqp::chars($!value)),
          Rakudo::Internals.SUBSTR-START-OOR($from,nqp::chars($!value)),
          nqp::substr($!value,$from)
        )
    }
    multi method substr(Str:D: Callable:D \start, Int:D \want) {
        nqp::if(
          nqp::islt_i((my int $from = (start)(nqp::chars($!value)).Int),0)
            || nqp::isgt_i($from,nqp::chars($!value)),
          Rakudo::Internals.SUBSTR-START-OOR($from,nqp::chars($!value)),
          nqp::if(
            nqp::islt_i((my int $chars = nqp::unbox_i(want)),0),
            Rakudo::Internals.SUBSTR-CHARS-OOR($chars),
            nqp::substr($!value,$from,$chars)
          )
        )
    }
    multi method substr(Str:D: Callable:D \start, Callable:D \want) {
        nqp::if(
          nqp::islt_i((my int $from = (start)(nqp::chars($!value)).Int),0)
            || nqp::isgt_i($from,nqp::chars($!value)),
          Rakudo::Internals.SUBSTR-START-OOR($from,nqp::chars($!value)),
          nqp::if(
            nqp::islt_i((my int $chars = (want)(nqp::chars($!value) - $from).Int),0),
            Rakudo::Internals.SUBSTR-CHARS-OOR($chars),
            nqp::substr($!value,$from,$chars)
          )
        )
    }
    multi method substr(Str:D: Range:D \start) {
        nqp::if(
          nqp::islt_i((my int $from = (start.min + start.excludes-min).Int),0)
            || nqp::isgt_i($from,nqp::chars($!value)),
          Rakudo::Internals.SUBSTR-START-OOR($from,nqp::chars($!value)),
          nqp::if(
            start.max == Inf,
            nqp::substr($!value,$from),
            nqp::substr($!value,$from,(start.max - start.excludes-max - $from + 1).Int)
          )
        )
    }
    multi method substr(Str:D: Regex:D, $) {
        die "You cannot use a Regex on 'substr', did you mean 'subst'?"  # GH 1314
    }
    multi method substr(Str:D: \start) {
        self.substr(start.Int)
    }
    multi method substr(Str:D: \from, \want) {
        nqp::istype(want,Whatever)
        || (! nqp::istype(want, Callable) && want == Inf)
          ?? self.substr(from)
          !! self.substr(nqp::istype(from, Callable) ?? from !! from.Int,
                         nqp::istype(want, Callable) ?? want !! want.Int)
    }

    multi method substr-rw(Str:D \SELF: \start, $want = Inf) is rw {
        my int $max  = nqp::chars($!value);
        my int $from = nqp::istype(start,Callable)
          ?? (start)($max)
          !! nqp::istype(start,Range)
            ?? start.min + start.excludes-min
            !! start.Int;
        return Rakudo::Internals.SUBSTR-START-OOR($from,$max)
          if nqp::islt_i($from,0) || nqp::isgt_i($from,$max);

        my int $chars = nqp::istype(start,Range)
          ?? start.max == Inf
            ?? nqp::sub_i($max,$from)
            !! start.max - start.excludes-max - $from + 1
          !! nqp::istype($want,Whatever) || $want == Inf
            ?? nqp::sub_i($max,$from)
            !! nqp::istype($want,Callable)
              ?? $want(nqp::sub_i($max,$from))
              !! $want.Int;

        nqp::islt_i($chars,0)
          ?? Rakudo::Internals.SUBSTR-CHARS-OOR($chars)
          !! Proxy.new(
               FETCH => sub ($) {        # need to access updated HLL Str
                   nqp::substr(nqp::unbox_s(SELF),$from,$chars)
               },
               STORE => sub ($, Str() $new) {
                   SELF = nqp::p6box_s(  # need to make it a new HLL Str
                     nqp::concat(
                       nqp::substr($!value,0,$from),
                       nqp::concat(
                         nqp::unbox_s($new),
                         nqp::substr($!value,nqp::add_i($from,$chars))
                       )
                     )
                   )
               }
             )
    }

    proto method codes(|) {*}
    multi method codes(Str:D: --> Int:D) {
#?if moar
        nqp::codes(self)
#?endif
#?if jvm
        nqp::p6box_i(nqp::chars(nqp::unbox_s(self)))
#?endif
    }
    multi method codes(Str:U: --> Int:D) {
        self.Str;  # generate undefined warning
        0
    }

    proto method chars(|) {*}
    multi method chars(Str:D: --> Int:D) {
        nqp::p6box_i(nqp::chars($!value))
    }
    multi method chars(Str:U: --> Int:D) {
        self.Str;  # generate undefined warning
        0
    }

    proto method uc(|) {*}
    multi method uc(Str:D:) {
        nqp::p6box_s(nqp::uc($!value));
    }
    multi method uc(Str:U:) {
        self.Str;
    }

    proto method lc(|) {*}
    multi method lc(Str:D:) {
        nqp::p6box_s(nqp::lc($!value));
    }
    multi method lc(Str:U:) {
        self.Str;
    }

    proto method tc(|) {*}
    multi method tc(Str:D:) {
        nqp::p6box_s(nqp::tc(nqp::substr($!value,0,1)) ~ nqp::substr($!value,1));
    }
    multi method tc(Str:U:) {
        self.Str
    }

    proto method fc(|) {*}
    multi method fc(Str:D:) {
        nqp::p6box_s(nqp::fc($!value));
    }
    multi method fc(Str:U:) {
        self.Str;
    }

    proto method tclc(|) {*}
    multi method tclc(Str:D:) {
        nqp::p6box_s(nqp::tclc($!value))
    }
    multi method tclc(Str:U:) {
        self.Str
    }

    proto method flip(|) {*}
    multi method flip(Str:D:) {
        nqp::p6box_s(nqp::flip($!value))
    }
    multi method flip(Str:U:) {
        self.Str
    }

    proto method ord(|) {*}
    multi method ord(Str:D: --> Int:D) {
        nqp::chars($!value)
          ?? nqp::p6box_i(nqp::ord($!value))
          !! Nil;
    }
    multi method ord(Str:U: --> Nil) { }
}


multi sub prefix:<~>(Str:D \a)         { a.Str }
multi sub prefix:<~>(str   $a --> str) { $a    }

multi sub infix:<~>(Str:D \a, Str:D \b --> Str:D) {
    nqp::p6box_s(nqp::concat(nqp::unbox_s(a), nqp::unbox_s(b)))
}
multi sub infix:<~>(str $a, str $b --> str) { nqp::concat($a, $b) }
multi sub infix:<~>(Any:D \a, Str:D \b) {
    nqp::p6box_s(nqp::concat(nqp::unbox_s(a.Stringy), nqp::unbox_s(b)))
}
multi sub infix:<~>(Str:D \a, Any:D \b) {
    nqp::p6box_s(nqp::concat(nqp::unbox_s(a), nqp::unbox_s(b.Stringy)))
}
multi sub infix:<~>(*@args) { @args.join }

multi sub infix:<x>(Str:D $s, Bool:D $repetition --> Str:D) {
    nqp::if($repetition, $s, '')
}
multi sub infix:<x>(Str:D $s, Int:D $repetition --> Str:D) {
    nqp::if(nqp::islt_i($repetition, 1), '', nqp::x($s, $repetition))
}
multi sub infix:<x>(str $s, int $repetition --> str) {
    nqp::if(nqp::islt_i($repetition, 1), '', nqp::x($s, $repetition))
}

multi sub infix:<cmp>(Str:D \a, Str:D \b --> Order:D) {
    ORDER(nqp::cmp_s(nqp::unbox_s(a), nqp::unbox_s(b)))
}
multi sub infix:<cmp>(str $a, str $b --> Order:D) {
    ORDER(nqp::cmp_s($a, $b))
}

multi sub infix:<===>(Str:D \a, Str:D \b --> Bool:D) {
    nqp::p6bool(
      nqp::eqaddr(a.WHAT,b.WHAT)
      && nqp::iseq_s(nqp::unbox_s(a), nqp::unbox_s(b))
    )
}
multi sub infix:<===>(str $a, str $b --> Bool:D) {
    nqp::p6bool(nqp::iseq_s($a, $b))
}

multi sub infix:<leg>(Str:D \a, Str:D \b --> Order:D) {
    ORDER(nqp::cmp_s(nqp::unbox_s(a), nqp::unbox_s(b)))
}
multi sub infix:<leg>(str $a, str $b --> Order:D) {
    ORDER(nqp::cmp_s($a, $b))
}

multi sub infix:<eq>(Str:D \a, Str:D \b --> Bool:D) {
    nqp::p6bool(nqp::iseq_s(nqp::unbox_s(a), nqp::unbox_s(b)))
}
multi sub infix:<eq>(str $a, str $b --> Bool:D) {
    nqp::p6bool(nqp::iseq_s($a, $b))
}

multi sub infix:<ne>(Str:D \a, Str:D \b --> Bool:D) {
    nqp::p6bool(nqp::isne_s(nqp::unbox_s(a), nqp::unbox_s(b)))
}
multi sub infix:<ne>(str $a, str $b --> Bool:D) {
    nqp::p6bool(nqp::isne_s($a, $b))
}

multi sub infix:<lt>(Str:D \a, Str:D \b --> Bool:D) {
    nqp::p6bool(nqp::islt_s(nqp::unbox_s(a), nqp::unbox_s(b)))
}
multi sub infix:<lt>(str $a, str $b --> Bool:D) {
    nqp::p6bool(nqp::islt_s($a, $b))
}

multi sub infix:<le>(Str:D \a, Str:D \b --> Bool:D) {
    nqp::p6bool(nqp::isle_s(nqp::unbox_s(a), nqp::unbox_s(b)))
}
multi sub infix:<le>(str $a, str $b --> Bool:D) {
    nqp::p6bool(nqp::isle_s($a, $b))
}

multi sub infix:<gt>(Str:D \a, Str:D \b --> Bool:D) {
    nqp::p6bool(nqp::isgt_s(nqp::unbox_s(a), nqp::unbox_s(b)))
}
multi sub infix:<gt>(str $a, str $b --> Bool:D) {
    nqp::p6bool(nqp::isgt_s($a, $b))
}

multi sub infix:<ge>(Str:D \a, Str:D \b --> Bool:D) {
    nqp::p6bool(nqp::isge_s(nqp::unbox_s(a), nqp::unbox_s(b)))
}
multi sub infix:<le>(str $a, str $b --> Bool:D) {
    nqp::p6bool(nqp::isle_s($a, $b))
}

multi sub infix:<~|>(Str:D \a, Str:D \b --> Str:D) {
    nqp::p6box_s(nqp::bitor_s(nqp::unbox_s(a), nqp::unbox_s(b)))
}
multi sub infix:<~|>(str $a, str $b --> str) { nqp::bitor_s($a, $b) }

multi sub infix:<~&>(Str:D \a, Str:D \b --> Str:D) {
    nqp::p6box_s(nqp::bitand_s(nqp::unbox_s(a), nqp::unbox_s(b)))
}
multi sub infix:<~&>(str $a, str $b --> str) { nqp::bitand_s($a, $b) }

multi sub infix:<~^>(Str:D \a, Str:D \b --> Str:D) {
    nqp::p6box_s(nqp::bitxor_s(nqp::unbox_s(a), nqp::unbox_s(b)))
}
multi sub infix:<~^>(str $a, str $b --> str) { nqp::bitxor_s($a, $b) }

multi sub prefix:<~^>(Str \a) {
    Failure.new("prefix:<~^> NYI")   # XXX
}

# XXX: String-wise shifts NYI
multi sub infix:«~>»(Str:D \a, Int:D \b --> Str:D) {
    X::NYI.new(feature => "infix:«~>»").throw;
}
multi sub infix:«~>»(str $a, int $b) {
    X::NYI.new(feature => "infix:«~>»").throw;
}
multi sub infix:«~<»(Str:D \a, Int:D \b --> Str:D) {
    X::NYI.new(feature => "infix:«~<»").throw;
}
multi sub infix:«~<»(str $a, int $b) {
    X::NYI.new(feature => "infix:«~<»").throw;
}

proto sub trim($, *%) {*}
multi sub trim(Cool:D $s --> Str:D) { $s.trim }

proto sub trim-leading($, *%) {*}
multi sub trim-leading (Cool:D $s --> Str:D) { $s.trim-leading }

proto sub trim-trailing($, *%) {*}
multi sub trim-trailing(Cool:D $s --> Str:D) { $s.trim-trailing }

# the opposite of Real.base, used for :16($hex_str)
proto sub UNBASE ($, $, *%) {*}
multi sub UNBASE(Int:D $base, Any:D $num) {
    X::Numeric::Confused.new(:$num, :$base).throw;
}
multi sub UNBASE(Int:D $base, Str:D $str) {
    my Str $ch = substr($str, 0, 1);
    if $ch eq '0' {
        $ch = substr($str, 1, 1);
        if    $base <= 11 && $ch eq any(<x d o b>)
           or $base <= 24 && $ch eq any <o x>
           or $base <= 33 && $ch eq 'x' {
            $str.Numeric;
        } else {
            ":{$base}<$str>".Numeric;
        }
    } elsif $ch eq ':' && substr($str, 1, 1) ~~ ('1'..'9') {
        $str.Numeric;
    } else {
        ":{$base}<$str>".Numeric;
    }
}

# for :16[1, 2, 3]
sub UNBASE_BRACKET($base, @a) {
    my $v = 0;
    my $denom = 1;
    my Bool $seen-dot = False;
    for @a {
        if $seen-dot {
            die "Only one decimal dot allowed" if $_ eq '.';
            $denom *= $base;
            $v += $_ / $denom
        }
        elsif $_ eq '.' {
            $seen-dot = True;
        }
        else {
            $v = $v * $base + $_;
        }
    }
    $v;
}
proto sub infix:<unicmp>($, $, *%) is pure {*}
proto sub infix:<coll>($, $, *%) {*}
#?if moar
multi sub infix:<unicmp>(Str:D \a, Str:D \b --> Order:D) {
    ORDER(
        nqp::unicmp_s(
            nqp::unbox_s(a), nqp::unbox_s(b), 85,0,0))
}
multi sub infix:<unicmp>(Pair:D \a, Pair:D \b) {
    (a.key unicmp b.key) || (a.value unicmp b.value)
}
multi sub infix:<coll>(Str:D \a, Str:D \b --> Order:D) {
    ORDER(
        nqp::unicmp_s(
            nqp::unbox_s(a), nqp::unbox_s(b), $*COLLATION.collation-level,0,0))
}
multi sub infix:<coll>(Cool:D \a, Cool:D \b --> Order:D) {
    ORDER(
        nqp::unicmp_s(
            nqp::unbox_s(a.Str), nqp::unbox_s(b.Str), $*COLLATION.collation-level,0,0))
}
multi sub infix:<coll>(Pair:D \a, Pair:D \b) {
    (a.key coll b.key) || (a.value coll b.value)
}
#?endif
#?if jvm
multi sub infix:<unicmp>(Str:D \a, Str:D \b) { die "unicmp NYI on JVM" }
multi sub infix:<coll>(Str:D \a, Str:D \b)   { die "coll NYI on JVM" }
#?endif

proto sub chrs(|) {*}
multi sub chrs(*@c --> Str:D) { @c.chrs }

proto sub parse-base($, $, *%) {*}
multi sub parse-base(Str:D $str, Int:D $radix) { $str.parse-base($radix) }

proto sub substr($, $?, $?, *%) {*}
multi sub substr(\what)                { what.substr             }
multi sub substr(\what, \from)         { what.substr(from)       }
multi sub substr(\what, \from, \chars) { what.substr(from,chars) }

proto sub substr-rw($, $?, $?, *%) is rw {*}
multi sub substr-rw(\what) is rw                { what.substr-rw             }
multi sub substr-rw(\what, \from) is rw         { what.substr-rw(from)       }
multi sub substr-rw(\what, \from, \chars) is rw { what.substr-rw(from,chars) }

multi sub infix:<eqv>(Str:D \a, Str:D \b) {
    nqp::p6bool(
      nqp::unless(
        nqp::eqaddr(nqp::decont(a),nqp::decont(b)),
        nqp::eqaddr(a.WHAT,b.WHAT) && nqp::iseq_s(a,b)
      )
    )
}

proto sub samemark($, $, *%) {*}
multi sub samemark($s, $pat) { $s.samemark($pat) }

sub parse-names(Str:D \names) {
    # XXX TODO: issue deprecation warning in 6.d; remove in 6.e
    names.uniparse
}

proto sub uniparse($, *%) {*}
multi sub uniparse(Str:D \names) { names.uniparse }

# vim: ft=perl6 expandtab sw=4
