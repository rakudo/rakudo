my class Range  { ... }
my class Match  { ... }
my class IntStr     { ... }
my class ComplexStr { ... }
my class NumStr     { ... }
my class RatStr     { ... }
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

    multi method WHICH(Str:D: --> ValueObjAt:D) {
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

    multi method Bool(Str:D: --> Bool:D) {
        nqp::hllbool(nqp::chars($!value));
    }
    method Capture() { X::Cannot::Capture.new( :what(self) ).throw }

    multi method Str(Str:D: --> Str:D)     { self }
    multi method Stringy(Str:D: --> Str:D) { self }
    multi method DUMP(Str:D: --> Str:D) { self.raku }

    method Int(Str:D: --> Int:D) {
        nqp::isge_i(
          nqp::findnotcclass(
            nqp::const::CCLASS_NUMERIC,self,0,nqp::chars(self)),
          nqp::chars(self)
        )
#?if !jvm
          # check for any combining characters
          && nqp::iseq_i(nqp::chars(self),nqp::codes(self))
#?endif
#?if jvm
            # RT #128542: https://rt.perl.org/Public/Bug/Display.html?id=128542
            # Needs Str.codes impl that doesn't just return chars
#?endif
          ?? nqp::atpos(nqp::radix_I(10,self,0,0,Int),0)  # all numeric chars
          !! nqp::istype((my $n := self.Numeric),Int) || nqp::istype($n,Failure)
            ?? $n
            !! $n.Int
    }
    method Num(Str:D: --> Num:D) {
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

    multi method ACCEPTS(Str:D: Str:D \other --> Bool:D) {
        nqp::hllbool(nqp::iseq_s(nqp::unbox_s(other),$!value));
    }
    multi method ACCEPTS(Str:D: Any:D \other --> Bool:D) {
        nqp::hllbool(nqp::iseq_s(nqp::unbox_s(other.Str),$!value));
    }

    method chomp(Str:D: --> Str:D) {
        nqp::substr(
          self,
          0,
          nqp::chars(self) - nqp::iscclass(                       #?js: NFG
            nqp::const::CCLASS_NEWLINE,self,nqp::chars(self) - 1  #?js: NFG
          )
        )
    }

    multi method chop(Str:D: --> Str:D) {
        nqp::substr(
          self,
          0,
          nqp::chars(self) && nqp::chars(self) - 1
        )
    }
    multi method chop(Str:D: Int:D $chopping --> Str:D) {
        nqp::substr(
          self,
          0,
          nqp::not_i(nqp::isbig_I(nqp::decont($chopping)))
            && nqp::isgt_i(nqp::chars(self),$chopping)
            && nqp::sub_i(nqp::chars(self),$chopping)
        )
    }
    multi method chop(Str:D: $chopping --> Str:D) {
        self.chop($chopping.Int)
    }

    multi method starts-with(Str:D: Cool:D $needle --> Bool:D) {
        nqp::hllbool(nqp::eqat(self, $needle.Str, 0))
    }
    multi method starts-with(Str:D: Str:D $needle --> Bool:D) {
        nqp::hllbool(nqp::eqat(self, $needle, 0))
    }

    multi method ends-with(Str:D: Cool:D $needle --> Bool:D) {
        self.ends-with: $needle.Str
    }
    multi method ends-with(Str:D: Str:D $needle --> Bool:D) {
        nqp::hllbool(
          nqp::eqat(
            self,$needle,nqp::sub_i(nqp::chars(self),nqp::chars($needle))
          )
        )
    }

    multi method substr-eq(Str:D: Cool:D $needle --> Bool:D) {
        nqp::hllbool(nqp::eqat(self,$needle.Str,0))
    }
    multi method substr-eq(Str:D: Str:D $needle --> Bool:D) {
        nqp::hllbool(nqp::eqat(self,$needle,0))
    }
    multi method substr-eq(Str:D: Cool:D $needle, Int:D $pos --> Bool:D) {
        nqp::hllbool(
          nqp::isge_i($pos,0) && nqp::eqat(self,$needle.Str,$pos)
        )
    }
    multi method substr-eq(Str:D: Str:D $needle, Int:D $pos --> Bool:D) {
        nqp::hllbool(
          nqp::isge_i($pos,0) && nqp::eqat(self,$needle,$pos)
        )
    }

    multi method contains(Str:D: Cool:D $needle --> Bool:D) {
        nqp::hllbool(nqp::isne_i(nqp::index($!value,$needle.Str,0),-1))
    }
    multi method contains(Str:D: Str:D $needle --> Bool:D) {
        nqp::hllbool(nqp::isne_i(nqp::index($!value,$needle,0),-1))
    }
    multi method contains(Str:D: Cool:D $needle, Int:D $pos --> Bool:D) {
        nqp::hllbool(
          nqp::isge_i($pos,0)
            && nqp::islt_i($pos,nqp::chars(self))
            && nqp::isne_i(nqp::index(self,$needle.Str,$pos),-1)
        )
    }
    multi method contains(Str:D: Str:D $needle, Int:D $pos --> Bool:D) {
        nqp::hllbool(
          nqp::isge_i($pos,0)
            && nqp::islt_i($pos,nqp::chars(self))
            && nqp::isne_i(nqp::index(self,$needle,$pos),-1)
        )
    }
    multi method contains(Str:D: Cool:D $needle, Cool:D $pos --> Bool:D) {
        self.contains($needle.Str, $pos.Int)
    }
    multi method contains(Str:D: Str:D $needle, Cool:D $pos --> Bool:D) {
        self.contains($needle, $pos.Int)
    }

    multi method indices(Str:D: Cool:D $needle, :$overlap) {
        self.indices: $needle.Str, :$overlap
    }
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
    multi method indices(Str:D: Cool:D $needle, Cool:D $start, :$overlap) {
        self.indices: $needle.Str, $start.Int, :$overlap
    }
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

    multi method index(Str:D: Cool:D $needle --> Int:D) {
        nqp::islt_i((my $i := nqp::index(self,$needle.Str)),0) ?? Nil !! $i
    }
    multi method index(Str:D: Str:D $needle --> Int:D) {
        nqp::islt_i((my $i := nqp::index(self,$needle)),0) ?? Nil !! $i
    }
    multi method index(Str:D: Cool:D $needle, Cool:D $pos --> Int:D) {
        self.index: $needle.Str, $pos.Int
    }
    multi method index(Str:D: Cool:D $needle, Int:D $pos --> Int:D) {
        nqp::isbig_I(nqp::decont($pos)) || nqp::islt_i($pos,0)
          ?? self!INDEX-OOR($pos)
          !! nqp::islt_i((my $i := nqp::index(self,$needle.Str,$pos)),0)
            ?? Nil
            !! $i
    }
    multi method index(Str:D: Str:D $needle, Int:D $pos --> Int:D) {
        nqp::isbig_I(nqp::decont($pos)) || nqp::islt_i($pos,0)
          ?? self!INDEX-OOR($pos)
          !! nqp::islt_i((my $i := nqp::index(self,$needle,$pos)),0)
            ?? Nil
            !! $i
    }
    method !INDEX-OOR($got) {
        Failure.new(X::OutOfRange.new(
          :what("Position in index"),
          :$got,
          :range("0..{ nqp::chars(self) }")
        ))
    }

    multi method rindex(Str:D: Cool:D $needle --> Int:D) {
        nqp::islt_i((my $i := nqp::rindex($!value,$needle.Str)),0) ?? Nil !! $i
    }
    multi method rindex(Str:D: Str:D $needle --> Int:D) {
        nqp::islt_i((my $i := nqp::rindex($!value,$needle)),0) ?? Nil !! $i
    }
    multi method rindex(Str:D: Cool:D $needle, Cool:D $pos --> Int:D) {
        self.rindex: $needle.Str, $pos.Int
    }
    multi method rindex(Str:D: Cool:D $needle, Int:D $pos --> Int:D) {
        nqp::isbig_I(nqp::decont($pos)) || nqp::islt_i($pos,0)
          ?? self!RINDEX-OOR($pos)
          !! nqp::islt_i((my $i := nqp::rindex(self,$needle.Str,$pos)),0)
            ?? Nil
            !! $i
    }
    multi method rindex(Str:D: Str:D $needle, Int:D $pos --> Int:D) {
        nqp::isbig_I(nqp::decont($pos)) || nqp::islt_i($pos,0)
          ?? self!RINDEX-OOR($pos)
          !! nqp::islt_i((my $i := nqp::rindex(self,$needle,$pos)),0)
            ?? Nil
            !! $i
    }
    method !RINDEX-OOR($got) {
        Failure.new(X::OutOfRange.new(
          :what("Position in rindex"),
          :$got,
          :range("0..{ nqp::chars(self) }")
        ))
    }

    method pred(Str:D: --> Str:D) {
        (my int $chars = Rakudo::Internals.POSSIBLE-MAGIC-CHARS(self))
          ?? Rakudo::Internals.PRED(self,$chars - 1)
          !! self
    }

    method succ(Str:D: --> Str:D) {
        (my int $chars = Rakudo::Internals.POSSIBLE-MAGIC-CHARS(self))
          ?? Rakudo::Internals.SUCC(self,$chars - 1)
          !! self
    }

    multi method gist(Str:D: --> Str:D) { self }
    multi method raku(Str:D: --> Str:D) {
        '"' ~ Rakudo::Internals.PERLIFY-STR(self) ~ '"'
    }

    my class CombAll does PredictiveIterator {
        has str $!str;
        has int $!chars;
        has int $!pos;
        method !SET-SELF(\string) {
            nqp::stmts(
              ($!str   = nqp::unbox_s(string)),
              ($!chars = nqp::chars($!str)), #?js: NFG
              ($!pos = -1),
              self
            )
        }
        method new(\string) { nqp::create(self)!SET-SELF(string) }
        method pull-one() {
            nqp::if(
              nqp::islt_i(($!pos = nqp::add_i($!pos,1)),$!chars),
              nqp::p6box_s(nqp::substr($!str,$!pos,1)), #?js: NFG
              IterationEnd
            )
        }
        method skip-one() {
            nqp::islt_i(($!pos = nqp::add_i($!pos,1)),$!chars)
        }
        method push-all(\target --> IterationEnd) {
            nqp::stmts(
              (my str $str = $!str),      # locals are faster
              (my int $pos = $!pos),
              (my int $chars = $!chars),
              nqp::while(
                nqp::islt_i(($pos = nqp::add_i($pos,1)),$chars),
                target.push(nqp::substr($str,$pos,1)) #?js: NFG
              ),
              ($!pos = $pos)
            )
        }
        method count-only(--> Int:D) {
            nqp::p6box_i($!chars - $!pos - nqp::islt_i($!pos,$!chars))
        }
    }
    multi method comb(Str:D: --> Seq:D) { Seq.new(CombAll.new(self)) }

    my class CombN does PredictiveIterator {
        has str $!str;
        has int $!chars;
        has int $!size;
        has int $!pos;
        has int $!todo;
        method !SET-SELF(\string,\size,\limit) {
            $!str   = nqp::unbox_s(string);
            $!chars = nqp::chars($!str); #?js: NFG
            $!size  = size < 1 ?? 1 !! size;
            $!pos   = -$!size;
            $!todo  = 1 + (($!chars - 1) div $!size);
            $!todo  = limit
              unless nqp::istype(limit,Whatever) || limit > $!todo;
            $!todo  = $!todo + 1;
            self
        }
        method new(\string,\size,\limit) {
            nqp::if(
              string,
              nqp::create(self)!SET-SELF(string,size,limit),
              Rakudo::Iterator.Empty
            )
        }
        method pull-one() {
            nqp::if(
              ($!todo = $!todo - 1),
              nqp::p6box_s(
                 nqp::substr($!str,($!pos = $!pos + $!size), $!size) #?js: NFG
              ),
              IterationEnd
            )
        }
        method push-all(\target --> IterationEnd) {
            my int $todo  = $!todo;
            my int $pos   = $!pos;
            my int $size  = $!size;
            my int $chars = $!chars;
            nqp::while(
              ($todo = $todo - 1),
              target.push(
                nqp::p6box_s(
                  nqp::substr($!str,($pos = $pos + $size), $size) #?js: NFG
                )
              )
            );
            $!todo = 0;
        }
        method count-only(--> Int:D) {
            nqp::p6box_i($!todo - nqp::isgt_i($!todo,0))
        }
    }

    multi method comb(Str:D: Int:D $size, $limit = * --> Seq:D) {
        $size <= 1 && (nqp::istype($limit,Whatever) || $limit == Inf)
          ?? self.comb
          !! Seq.new(CombN.new(self,$size,$limit))
    }

    my class CombPat does Iterator {
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
    }
    multi method comb(Str:D: Str:D $pat --> Seq:D) {
        $pat
          ?? Seq.new(CombPat.new(self,$pat))
          !! self.comb
    }

    my class CombPatLimit does Iterator {
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
    }
    multi method comb(Str:D: Str:D $pat, $limit --> Seq:D) {
        nqp::istype($limit,Whatever) || $limit == Inf
          ?? self.comb($pat)
          !! $pat
            ?? Seq.new(CombPatLimit.new(self,$pat,$limit))
            !! self.comb(1,$limit)
    }

    multi method comb(Str:D: Regex:D $pattern, :$match --> Seq:D) {
        Seq.new(nqp::if(
          $match,
          self.match($pattern, :g),
          self.match($pattern, :g, :as(Str))
        ).iterator)
    }
    multi method comb(Str:D: Regex:D $pattern, $limit, :$match --> Seq:D) {
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
        method push-all(\target --> IterationEnd) {
            nqp::while(
              nqp::isge_i(nqp::getattr_i($!cursor,Match,'$!pos'),0),
              nqp::stmts(
                target.push($!post($!cursor)),
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
        method push-all(\target --> IterationEnd) {
            nqp::while(
              nqp::isge_i(nqp::getattr_i($!cursor,Match,'$!pos'),0),
              nqp::stmts(
                target.push($!cursor),
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
                    $matches.List
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
                      $matches.List,          # found all values
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
            $matches.List,
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

#?if !jvm
    multi method ords(Str:D:) { self.NFC.list }
#?endif
#?if jvm
    multi method ords(Str:D: --> Seq:D) {
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

    multi method lines(Str:D: :$count! --> Int:D) {
        # we should probably deprecate this feature
        $count ?? self.lines.elems !! self.lines;
    }
    multi method lines(Str:D: $limit --> Seq:D) {
        nqp::istype($limit,Whatever) || $limit == Inf
          ?? self.lines
          !! self.lines.head($limit)
    }

    my class Lines does PredictiveIterator {
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
            nqp::if(
              (my int $left = $!chars - $!pos) > 0,
              nqp::stmts(
                (my int $nextpos = nqp::findcclass(
                  nqp::const::CCLASS_NEWLINE, $!str, $!pos, $left)),
                (my $found := nqp::p6box_s(
                  nqp::substr($!str, $!pos, $nextpos - $!pos)
                )),
#?if moar
                ($!pos = $nextpos + 1),
#?endif
#?if !moar
                ($!pos = $nextpos +
                  (nqp::iseq_s(nqp::substr($!str, $nextpos, 2), "\r\n") ?? 2 !! 1)),
#?endif
                $found
              ),
              IterationEnd
            )
        }
        method push-all(\target --> IterationEnd) {
            my int $left;
            my int $nextpos;

            while ($left = $!chars - $!pos) > 0 {
                $nextpos = nqp::findcclass(
                  nqp::const::CCLASS_NEWLINE, $!str, $!pos, $left);

                target.push(nqp::substr($!str, $!pos, $nextpos - $!pos));
#?if moar
                $!pos = $nextpos + 1;
#?endif
#?if !moar
                $!pos = $nextpos +
                  (nqp::iseq_s(nqp::substr($!str, $nextpos, 2), "\r\n") ?? 2 !! 1);
#?endif
            }
        }
        method count-only(--> Int:D) {
            my int $left;
            my int $seen;
            my int $pos   = $!pos;
            my int $chars = $!chars;

            while ($left = $chars - $pos) > 0 {
                $pos = nqp::findcclass(
                  nqp::const::CCLASS_NEWLINE, $!str, $pos, $left) + 1;
                $seen = $seen + 1;
            }
            nqp::p6box_i($seen)
        }
        method bool-only(--> Bool:D) {
            nqp::hllbool(nqp::islt_i($!pos,$!chars))
        }
    }
    multi method lines(Str:D: --> Seq:D) { Seq.new(Lines.new(self)) }

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
          got       => limit.raku,
        ).throw if limit === NaN;

        limit = Inf if nqp::istype(limit,Whatever);
    }

    proto method parse-base(|) {*}
    multi method parse-base(Str:D: Int:D $radix --> Numeric:D) {
        2 <= $radix <= 36                    # (0..9,"a".."z").elems == 36
          ?? nqp::chars(self)
            ?? nqp::atpos(                   # something to parse
                 (my $r := nqp::radix_I($radix,self,0,0x02,Int)),
                 2
               ) == nqp::chars(self)
              ?? nqp::atpos($r,0)
              !! self!slow-parse-base($radix,nqp::atpos($r,0),nqp::atpos($r,2))
            !! self!parse-fail($radix, 0)    # nothing to parse
          !! Failure.new(X::Syntax::Number::RadixOutOfRange.new(:$radix))
    }

    # Shortcut for generating parsing Failure
    method !parse-fail($radix, $pos --> Failure) {
        Failure.new(X::Str::Numeric.new(
          :source(self),
          :$pos,
          :reason("malformed base-$radix number"),
        ))
    }

    # Slow path for non-simple integer values
    method !slow-parse-base(int $radix, \whole, int $failed-at --> Numeric:D) {
        $failed-at == -1                                      # nothing parsed
          ?? nqp::eqat(self,'.',0)                             # .x ??
            ?? self!parse-rat($radix, 0, 1)
            !! nqp::eqat(self,'.',1)                            # -.x −.x +.x ??
              ?? nqp::eqat(self,'-',0) || nqp::eqat(self,'−',0)  # -. −.
                ?? nqp::istype((my $f := self!parse-rat($radix, 0, 2)),Failure)
                  ?? $f                                           # fail
                  !! -$f                                          # negate val
                !! nqp::eqat(self,'+',0)                         # +.
                  ?? self!parse-rat($radix, 0, 2)
                  !! self!parse-fail($radix, 0)
              !! self!parse-fail($radix, 0)
            !! nqp::eqat(self,'.',$failed-at)                   # 123. ??
              ?? self!parse-rat($radix, whole, $failed-at + 1)
              !! self!parse-fail($radix, $failed-at)
    }

    # Helper path for parsing rats
    method !parse-rat(int $radix, Int:D $whole, int $offset --> Numeric:D) {
        my $fract := nqp::radix_I($radix,self,$offset,0,Int);
        nqp::atpos($fract,2) == nqp::chars(self)   # fraction parsed entirely?
          ?? DIVIDE_NUMBERS(
               nqp::islt_I($whole,0)
                 ?? nqp::sub_I(
                      nqp::mul_I($whole,nqp::atpos($fract,1),Int),
                      nqp::atpos($fract,0),
                      Int
                    )
                 !! nqp::add_I(
                      nqp::mul_I($whole,nqp::atpos($fract,1),Int),
                      nqp::atpos($fract,0),
                      Int
                    ),
               nqp::atpos($fract,1),
               Rat,
               Rat
             )
          !! self!parse-fail($radix, nqp::atpos($fract,2) max $offset)
    }
    method !eggify($egg --> Int:D) { self.trans($egg => "01").parse-base(2) }
    multi method parse-base(Str:D: "camel" --> Int:D) { self!eggify: "🐪🐫" }
    multi method parse-base(Str:D: "beer"  --> Int:D) { self!eggify: "🍺🍻" }

    multi method split(Str:D: Regex:D $pat, $limit is copy = Inf;;
      :$v is copy, :$k, :$kv, :$p, :$skip-empty --> Seq:D) {

        my int $any = self!ensure-split-sanity($v,$k,$kv,$p);

        self!ensure-limit-sanity($limit);
        return Seq.new(Rakudo::Iterator.Empty) if $limit <= 0;

        my \matches = $limit == Inf
          ?? self!match-list($/, $pat($cursor-init(Match,self,:0c)),
                CURSOR-GLOBAL, POST-MATCH)
          !! self.match($pat, :x(1..$limit-1));

        my str $str   = nqp::unbox_s(self);
        my int $elems = matches.elems;  # make sure all reified
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
      :$v is copy, :$k, :$kv, :$p, :$skip-empty --> Seq:D) {
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

    my class SplitStrLimit does Iterator {
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
        method push-all(\target --> IterationEnd) {
            while $!todo {
                $!todo = $!todo - 1;
                my int $found = nqp::index($!string,$!match,$!pos);
                nqp::islt_i($found,0)
                  ?? ($!todo = 0)
                  !! target.push(self!next-part($found));
            }
            target.push(self!last-part) if nqp::isle_i($!pos,$!chars);
        }
        method sink-all(--> IterationEnd) { }
    }
    my class SplitEmptyLimit does PredictiveIterator {
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
        method push-all(\target --> IterationEnd) {
            target.push("") if $!first;
            $!todo = $!todo - 1;
            while $!todo {
                target.push(
                  nqp::p6box_s(nqp::substr($!string,$!pos++,1)));
                $!todo = $!todo - 1;
            }
            target.push( nqp::p6box_s(nqp::substr($!string,$!pos)))
              if nqp::islt_i($!pos,$!chars);
            target.push("") if $!last;
        }
        method count-only() { nqp::p6box_i($!todo + $!first + $!last) }
        method sink-all(--> IterationEnd) { }
    }
    multi method split(Str:D: Str(Cool) $match, $limit is copy = Inf;;
      :$v is copy, :$k, :$kv, :$p, :$skip-empty) {
        my int $any = self!ensure-split-sanity($v,$k,$kv,$p);

        self!ensure-limit-sanity($limit);
        return Seq.new(Rakudo::Iterator.Empty) if $limit <= 0;

        # nothing to work with
        my int $chars = $match.chars;
        if !self.chars {
            $chars ?? self.list !! ();
        }

        # nothing to do
        elsif $limit == 1 {
            self.list;
        }

        # want them all
        elsif $limit == Inf {
            self.split($match,:$v,:$k,:$kv,:$p,:$skip-empty);
        }

        # we have something to split on
        elsif $chars {
            $any || $skip-empty
              # let the multi-needle handler handle all nameds
              ?? self.split(($match,),$limit,:$v,:$k,:$kv,:$p,:$skip-empty)
              # make the sequence
              !! Seq.new(SplitStrLimit.new(self,$match,$limit))
        }

        # just separate chars
        else {
            Seq.new(SplitEmptyLimit.new(self,$limit,$skip-empty))
        }
    }

    multi method split(Str:D: @needles, $parts is copy = Inf;;
       :$v is copy, :$k, :$kv, :$p, :$skip-empty --> Seq:D) {
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
    method samecase(Str:D: Str:D $pattern --> Str:D) {
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

#?if !jvm
    method samemark(Str:D: Str:D $pattern --> Str:D) {
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

    method trim-leading(Str:D: --> Str:D) {
        nqp::substr(
          self,
          nqp::findnotcclass(
            nqp::const::CCLASS_WHITESPACE,self,0,nqp::chars(self)
          )
        )
    }

    method trim-trailing(Str:D: --> Str:D) {
        nqp::if(
          nqp::iscclass(
            nqp::const::CCLASS_WHITESPACE,
            self,
            (my int $pos = nqp::chars(self) - 1)
          ),
          nqp::stmts(   # at least one trailing whitespace
            nqp::while(
              nqp::isge_i(--$pos,0)
                && nqp::iscclass(nqp::const::CCLASS_WHITESPACE,self,$pos),
              nqp::null
            ),
            nqp::substr(self,0,$pos + 1)
          ),
          self          # no whitespace, so done
        )
    }

    method trim(Str:D: --> Str:D) {
        my int $left = nqp::findnotcclass(
          nqp::const::CCLASS_WHITESPACE,
          self,
          0,
          (my int $pos = nqp::chars(self))
        );
        nqp::while(
          nqp::isgt_i(--$pos,$left)
            && nqp::iscclass(nqp::const::CCLASS_WHITESPACE,self,$pos),
          nqp::null
        );
        nqp::substr(self,$left,$pos + 1 - $left)
    }

    multi method words(Str:D: $limit --> Seq:D) {
        nqp::istype($limit,Whatever) || $limit == Inf
          ?? self.words
          !! self.words.head($limit)
    }
    my class Words does PredictiveIterator {
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
            nqp::if(
              (my int $left = $!chars - $!pos) > 0,
              nqp::stmts(
                (my int $nextpos = nqp::findcclass(
                  nqp::const::CCLASS_WHITESPACE, $!str, $!pos, $left)),
                (my $found := nqp::p6box_s(
                  nqp::substr($!str, $!pos, $nextpos - $!pos)
                )),
                ($!pos = nqp::findnotcclass( nqp::const::CCLASS_WHITESPACE,
                  $!str, $nextpos, $!chars - $nextpos)),
                $found
              ),
              IterationEnd
            )
        }
        method push-all(\target --> IterationEnd) {
            my int $left;
            my int $nextpos;

            while ($left = $!chars - $!pos) > 0 {
                $nextpos = nqp::findcclass(
                  nqp::const::CCLASS_WHITESPACE, $!str, $!pos, $left);

                target.push(nqp::p6box_s(
                  nqp::substr($!str, $!pos, $nextpos - $!pos)
                ));
                $!pos = nqp::findnotcclass( nqp::const::CCLASS_WHITESPACE,
                  $!str, $nextpos, $!chars - $nextpos);
            }
        }
        method count-only(--> Int:D) {
            my int $left;
            my int $nextpos;
            my int $seen;
            my int $pos   = $!pos;
            my int $chars = $!chars;

            while ($left = $chars - $pos) > 0 {
                $nextpos = nqp::findcclass(
                  nqp::const::CCLASS_WHITESPACE, $!str, $pos, $left);
                $pos = nqp::findnotcclass( nqp::const::CCLASS_WHITESPACE,
                  $!str, $nextpos, $chars - $nextpos);
                $seen = $seen + 1;
            }
            $seen
        }
        method bool-only(--> Bool:D) {
            nqp::hllbool(nqp::islt_i($!pos,$!chars))
        }
    }
    multi method words(Str:D: --> Seq:D) { Seq.new(Words.new(self)) }

    # Internal method, used in Actions.postprocess_words/postprocess_quotewords
    method WORDS_AUTODEREF(Str:D:) {
        Words.new(self).push-all(my $words := nqp::create(IterationBuffer));
        nqp::elems($words) == 1
          ?? nqp::shift($words)
          !! $words.List
    }

    proto method encode(|) {*}
    multi method encode(Str:D $encoding = 'utf8',
      :$replacement, Bool() :$translate-nl = False, :$strict --> Blob:D) {
        Encoding::Registry.find($encoding)
            .encoder(:$replacement, :$translate-nl, :$strict)
            .encode-chars(self)
    }

#?if !jvm
    method NFC(--> NFC:D) {
        nqp::strtocodes(nqp::unbox_s(self), nqp::const::NORMALIZE_NFC, nqp::create(NFC))
    }
    method NFD(--> NFD:D) {
        nqp::strtocodes(nqp::unbox_s(self), nqp::const::NORMALIZE_NFD, nqp::create(NFD))
    }
    method NFKC(--> NFKC:D) {
        nqp::strtocodes(nqp::unbox_s(self), nqp::const::NORMALIZE_NFKC, nqp::create(NFKC))
    }
    method NFKD(--> NFKD:D) {
        nqp::strtocodes(nqp::unbox_s(self), nqp::const::NORMALIZE_NFKD, nqp::create(NFKD))
    }
#?endif
#?if jvm
    method NFC()  { X::NYI.new(:feature<NFC>).throw }
    method NFD()  { X::NYI.new(:feature<NFD>).throw }
    method NFKC() { X::NYI.new(:feature<NFKC>).throw }
    method NFKD() { X::NYI.new(:feature<NFKD>).throw }
#?endif

    method unival(Str:D:) { self ?? self.ord.unival !! Nil }
    method univals(Str:D:) { self.ords.map: *.unival }

    method wordcase(Str:D: :&filter = &tclc, Mu :$where = True --> Str:D) {
        self.subst(:g, / [<:L> \w* ] +% <['\-]> /, -> $m {  # ' highlighting
            my Str $s = $m.Str;
            $s ~~ $where ?? filter($s) !! $s;
        });
    }

    proto method trans(|) { $/ := nqp::getlexcaller('$/'); {*} }
    multi method trans(Str:D: Pair:D \what, *%n --> Str:D) {
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
      *@changes, :c(:$complement), :s(:$squash), :d(:$delete) --> Str:D) {

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

    method parse-names(Str:D: --> Str:D) {
        # XXX TODO: issue deprecation warning in 6.d; remove in 6.e
        self.uniparse
    }
    method uniparse(Str:D: --> Str:D) {
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
    multi method indent(Str:D: Int() $steps where { $_ == 0 }) {
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

    method !SUBSTR-START-OOR($from) {
        Failure.new(X::OutOfRange.new(
          :what('Start argument to substr'),
          :got($from.gist),
          :range("0.." ~ nqp::chars(self)),
          :comment( nqp::istype($from, Callable) || -$from > nqp::chars(self)
            ?? ''
            !! "use *-{abs $from} if you want to index relative to the end"),
        ))
    }
    method !SUBSTR-CHARS-OOR($chars) {
        Failure.new(X::OutOfRange.new(
          :what('Number of characters argument to substr'),
          :got($chars.gist),
          :range<0..^Inf>,
          :comment("use *-{abs $chars} if you want to index relative to the end"),
        ))
    }

    multi method substr(Str:D: Int:D $from --> Str:D) {
        nqp::islt_i($from,0) || nqp::isgt_i($from,nqp::chars(self))  #?js: NFG
          ?? self!SUBSTR-START-OOR($from)
          !! nqp::substr(self,$from)                                 #?js: NFG
    }
    multi method substr(Str:D: Int:D $from, Int:D $want --> Str:D) {
        nqp::islt_i($from,0) || nqp::isgt_i($from,nqp::chars(self))  #?js: NFG
          ?? self!SUBSTR-START-OOR($from)
          !! nqp::islt_i($want,0)
            ?? self!SUBSTR-CHARS-OOR($want)
            !! nqp::substr(self,$from,$want)                         #?js: NFG
    }
    multi method substr(Str:D: Int:D $from, &want --> Str:D) {
        self.substr(
          $from,
          want(nqp::sub_i(nqp::chars(self),$from)).Int               #?js: NFG
        )
    }
    multi method substr(Str:D: Int:D $from, Whatever --> Str:D) {
        self.substr($from)
    }
    multi method substr(Str:D: Int:D $from, Num:D $want --> Str:D) {
        nqp::isnanorinf($want)
          ?? $want == Inf
            ?? self.substr($from)
            !! self!SUBSTR-CHARS-OOR($want)
          !! self.substr($from, $want.Int)
    }
    multi method substr(Str:D: &want --> Str:D) {
        self.substr(want(nqp::chars(self)).Int)                      #?js: NFG
    }
    multi method substr(Str:D: &from, Int:D $want --> Str:D) {
        self.substr(from(nqp::chars(self)).Int, $want)               #?js: NFG
    }
    multi method substr(Str:D: &from, &want --> Str:D) {
        my int $from = from(nqp::chars(self)).Int;
        self.substr($from, want(nqp::sub_i(nqp::chars(self),$from)).Int)
    }
    multi method substr(Str:D: Range:D \start --> Str:D) {
        nqp::if(
          nqp::islt_i((my int $from = (start.min + start.excludes-min).Int),0)
            || nqp::isgt_i($from,nqp::chars($!value)), #?js: NFG
          self!SUBSTR-START-OOR($from),
          nqp::if(
            start.max == Inf,
            nqp::substr($!value,$from), #?js: NFG
            nqp::substr($!value,$from,(start.max - start.excludes-max - $from + 1).Int) #?js: NFG
          )
        )
    }
    multi method substr(Str:D: Regex:D, $) {
        die "You cannot use a Regex on 'substr', did you mean 'subst'?"  # GH 1314
    }
    multi method substr(Str:D: \start --> Str:D) {
        self.substr(start.Int)
    }
    multi method substr(Str:D: \from, \want --> Str:D) {
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
        return self!SUBSTR-START-OOR($from)
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
          ?? self!SUBSTR-CHARS-OOR($chars)
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
        nqp::codes(self)
    }
    multi method codes(Str:U: --> Int:D) {
        self.Str;  # generate undefined warning
        0
    }

    proto method chars(|) {*}
    multi method chars(Str:D: --> Int:D) {
        nqp::p6box_i(nqp::chars($!value)) #?js: NFG
    }
    multi method chars(Str:U: --> Int:D) {
        self.Str;  # generate undefined warning
        0
    }

    proto method uc(|) {*}
    multi method uc(Str:D: --> Str:D) {
        nqp::p6box_s(nqp::uc($!value));
    }
    multi method uc(Str:U: --> Str:D) {
        self.Str;
    }

    proto method lc(|) {*}
    multi method lc(Str:D: --> Str:D) {
        nqp::p6box_s(nqp::lc($!value));
    }
    multi method lc(Str:U: --> Str:D) {
        self.Str;
    }

    proto method tc(|) {*}
    multi method tc(Str:D: --> Str:D) {
        nqp::p6box_s(nqp::tc(nqp::substr($!value,0,1)) ~ nqp::substr($!value,1)); #?js: NFG
    }
    multi method tc(Str:U: --> Str:D) {
        self.Str
    }

    proto method fc(|) {*}
    multi method fc(Str:D: --> Str:D) {
        nqp::p6box_s(nqp::fc($!value));
    }
    multi method fc(Str:U: --> Str:D) {
        self.Str;
    }

    proto method tclc(|) {*}
    multi method tclc(Str:D: --> Str:D) {
        nqp::p6box_s(nqp::tclc($!value))
    }
    multi method tclc(Str:U: --> Str:D) {
        self.Str
    }

    proto method flip(|) {*}
    multi method flip(Str:D: --> Str:D) {
        nqp::p6box_s(nqp::flip($!value))
    }
    multi method flip(Str:U: --> Str:D) {
        self.Str
    }

    proto method ord(|) {*}
    multi method ord(Str:D: --> Int:D) {
        nqp::chars($!value)
          ?? nqp::p6box_i(nqp::ord($!value))
          !! Nil;
    }
    multi method ord(Str:U: --> Nil) { }

    # Helper class to indicate parsing has failed.  It takes the position
    # at which the parse failed, and a reason for failure.
    # An IterationBuffer is used so that we don't need to worry about
    # the list of values being wrapped in a List when passed around.
    # This also means that we can use nqp::ops directly on the object.
    class Failed is repr('VMArray') {

        # Turn the Failed information into an actual Failure for the
        # given string.
        method Failure($source) is hidden-from-backtrace {
            Failure.new(
              X::Str::Numeric.new(
                source => $source,
                pos    => nqp::atpos(self,0),
                reason => nqp::atpos(self,1)
              )
            )
        }
    }

    # Helper class to indicate parsing has been successful, and the value
    # that has been found so far, but did not reach the end of the string.
    # An IterationBuffer is used so that we don't need to worry about
    # the list of values being wrapped in a List when passed around.
    # This also means that we can use nqp::ops directly on the object.
    class NotDone is repr('VMArray') {

        # Turn the NotDone info to a Failed state
        method Failed() {
            my \failed := nqp::create(Failed);
            nqp::push(failed,nqp::atpos(self,0));
            nqp::push(failed,"Trailing characters after number");
            failed
        }

        # Check if the given string has the given needle at the position.
        method eqat($string, str $needle) {
            nqp::eqat($string,$needle,nqp::atpos(self,0))
        }

        # Move the location up one character.  Return the value if it is now
        # at the end of the string, else return updated NotDone.
        method next(Str:D $string) {
            nqp::unshift(self,nqp::shift(self) + 1);
            nqp::atpos(self,0) == nqp::chars($string)
              ?? nqp::atpos(self,1)                    # done
              !! self                                  # still trailing chars
        }

        method raku() {
            "Not done at position {nqp::atpos(self,0)} with value {nqp::atpos(self,1)}"
        }
    }

    # Helper method to indicate a value parse totally failed at the given
    # position.  Takes the position at which the failure occurred, and
    # the reason for failure.  Does not create a Failure object directly,
    # because that is only needed in some cases.  So use a cheaper Failed
    # object instead.
    method !failed(
      Int:D $pos,
      Str:D $reason = "Trailing characters after number"
    --> Failed:D) {
        my \failed := nqp::create(Failed);
        nqp::push(failed,$pos);
        nqp::push(failed,$reason);
        failed
    }

    # Helper method to indicate a value parse failed at the given position,
    # but a value was found so far, and the end of the parse might indicate
    # further features, such as e/E for Nums, +/- for complex, * for power
    # multiplication.
    method !notdone(Int:D $pos, \value --> NotDone:D) {
#nqp::say("  notdone");
        my \notdone := nqp::create(NotDone);
        nqp::push(notdone,$pos);
        nqp::push(notdone,value);
        notdone
    }

    # Helper sub to transparently negate actual values, or the value inside
    # a NotDone, while leaving Faileds intact.
    proto sub negate(|) {*}
    multi sub negate(Failed:D \failed) { failed }
    multi sub negate(NotDone:D \notdone) {
        nqp::push(notdone,-nqp::pop(notdone));
        notdone
    }
    multi sub negate(Any:D \value) { -value }

    # A hack for making sure a Num string gets numified correctly with the
    # given value and exponent.  Because doing this "manually" introduces
    # differences at the 10th decimal position.  So instead, we just concat
    # the values so far into a string again, and let nqp::numify handle it.
    # This is probably sub-optimal, but would require a re-design of the
    # whole thing or use of dynamic variables to remember the original
    # offset of parsing.  Which would also suck on non-Nums.
    sub numify(\value, \exponent) {
        nqp::numify(
          nqp::concat(
            value.Str,
            nqp::concat('e',exponent.Str)
          )
        )
    }

    # Helper method to deduce whether a value can be returned as is,
    # or needs to be wrapped in a NotDone because the end of the string
    # has not been reached yet.
    method !deduce-notdone(int $pos, \value) {
#nqp::say("  deduce-notdone");
        $pos == nqp::chars(self)
          ?? value
          !! self!notdone($pos, value)
    }

    # Helper method that examines the NotDone (if a NotDone was given),
    # and changes the NotDone to the value if it was at the end, or a
    # new NotDone positioned at the next char.
    method !ends-with(Str:D $end, \result) {
#nqp::say("  ends-with");
        nqp::istype(result,NotDone)
          ?? result.eqat(self, $end)
            ?? result.next(self)                                      # value>
            !! self!failed(nqp::chars(self),                          # value
               "'{nqp::atpos(result,1)}' did not end with '$end'")
          !! self!failed(nqp::chars(self),                            # value
               "'{result}' did not end with '$end'")
    }

    # Helper method for deducing the radix of a radix marker.  Returns
    # the radix, or 0 if no valid radix marker found
    method !deduce-radix(int $offset, int $pos) {
#nqp::say("  deduce-radix");
        my int $radix = nqp::index(
          "  b     o d     x",          # quick radix lookup
          nqp::substr(self,$pos,1)      # radix identifier
        );
        $radix > 0 && (
             ($pos == $offset + 1)                                    # 0x???
          || ($pos == $offset + 2 && (
                  nqp::eqat(self,'+',$offset)                         # +0x???
               || nqp::eqat(self,'-',$offset)                         # -0x???
               || nqp::eqat(self,'−',$offset)
             ))
        ) && $radix
    }

    # Basic entry point for parsing the val() out of a string.
    method !val() {
#nqp::say("  val");
        nqp::chars(self)
          ?? nqp::atpos(                                              # ???
               (my \parsed := nqp::radix_I(10,self,0,0x02,Int)),2
          ) == nqp::chars(self)
            ?? nqp::atpos(parsed,0)                                   # 1234
            !! nqp::istype((my \value := self!val-parse-value(10, 0)),NotDone)
              ?? self!val-more(value)                                 # value???
              !! value                                                # value
          !! 0                                                        # ''
    }

    # Entry point if there where characters left after the string was parsed.
    # Takes a NotDone, and looks for slashy and complex handling.
    method !val-more(\notdone) {
#nqp::say("  val-more");
        my int $pos = nqp::atpos(notdone,0);
        my \sofar  := nqp::atpos(notdone,1);

        nqp::eqat(self,'/',$pos)
          ?? self!val-slashy-rat(sofar, $pos + 1)                     # 123/???
          !! nqp::eqat(self,'+',$pos)
               || nqp::eqat(self,'-',$pos)
               || nqp::eqat(self,'−',$pos)
            ?? self!val-complex(sofar, $pos)                          # 123+???
            !!      (nqp::eqat(self,  'i',$pos) && $pos == nqp::chars(self) - 1)
                 || (nqp::eqat(self,'\\i',$pos) && $pos == nqp::chars(self) - 2)
              ?? Complex.new(0, sofar)                                # 123i
              !! notdone.Failed                                       # 123huh
    }

    # Entry point for handling slashy Rats.  Takes the numerator value and
    # offset from which to start parsing.
    method !val-slashy-rat(\numerator, int $offset) {
#nqp::say("  val-slashy-rat");
        my \value := self!val-parse-value(10, $offset);
        nqp::istype(value,Failed)
          ?? value
          !! nqp::istype(value,NotDone)
            ?? value.Failed
            !! numerator / value
    }

    # Entry point for handling a power specification (*123**456).  Takes
    # the value so far, and offset from which to parse to process the Ints
    # of the power spec.
    method !val-power(\value, int $offset) {
#nqp::say("  val-power");
        my \parsed := nqp::radix_I(10,self,$offset,0x02,Int);
        my \base   := nqp::atpos(parsed,0);
        my int $pos = nqp::atpos(parsed,2);
        $pos == nqp::chars(self)
          ?? self!failed($pos,                                        # 1*4
               "Did not find '**' in power specfication")
          !! $pos == -1
            ?? self!failed($offset,                                   # 1*
               "Found trailing '*' in apparent power specification")
            !! nqp::eqat(self,'**',$pos)
              ?? base < 0
                ?? negate(self!val-power-exponent(                    # 1*-4**??
                     value, -base, $pos + 2))
                !! self!val-power-exponent(value, base, $pos + 2)     # 1*4**???
              !! self!failed($pos)
    }

    # Helper method for handling the end result of a * b ** c
    method !val-power-exponent(\value, \base, int $offset) {
#nqp::say("  val-power-exponent");
        my \parsed   := nqp::radix_I(10,self,$offset,0x02,Int);
        my \exponent := nqp::atpos(parsed,0);
        my int $pos   = nqp::atpos(parsed,2);
        $pos == nqp::chars(self)
          ?? value * base ** exponent                                 # 1*4**2
          !! $pos == -1
            ?? self!failed($offset,                                   # 1*4**
               "Found trailing '**' in apparent power specification")
            !! self!notdone($pos, value * base ** exponent)           # 1*4**2?
    }

    # Entry point for handling complex values with a real component.  Takes
    # the real value and offset to start parsing.
    method !val-complex(\real, int $offset) {
#nqp::say("  val-complex");
        $offset == nqp::chars(self) - 2 && nqp::eqat(self,'i',$offset + 1)
          ?? Complex.new(real, nqp::eqat(self,'+',$offset) || -1)     # +i
          !! nqp::istype(
               (my \value := self!val-parse-value(10, $offset)),
               NotDone
             )
            ?? (nqp::eqat(self,'i',(my int $pos = nqp::atpos(value,0)))
                 && $pos == nqp::chars(self) - 1)
                 || (nqp::eqat(self,'\\i',$pos) && $pos == nqp::chars(self) - 2)
              ?? Complex.new(real, nqp::atpos(value,1))               # 12+4i
              !! self!failed($pos,                                    # 12+4huh
                   "Did not find 'i' after what looks like a complex number")
            !! self!failed($pos,                                      # 12+4
                 "Missing 'i' after what looks like a complex number")
    }

    # Entry point for parsing a value without restrictions.  Takes the
    # offset at which parsing should start.  Either returns an Int, a dotty
    # Rat, a Num, or a Failed or NotDone.
    method !val-parse-value(int $radix, int $offset) {
#nqp::say("  val-parse-value");
        my \parsed := nqp::radix_I($radix,self,$offset,0x02,Int);
        my \value  := nqp::atpos(parsed,0);
        my int $pos = nqp::atpos(parsed,2);
        $pos == -1
          ?? self!val-no-int($offset)                                 # ????
          !! $pos == nqp::chars(self)
            ?? value                                                  # 123
            !! value
              ?? self!val-starts-with-int($radix, value, $pos)        # 12???
              !! (my int $newradix = self!deduce-radix($offset, $pos))
                ?? self!val-radix-marked(                             # 0x???
                     $newradix,
                     $pos + 1,
                     nqp::eqat(self,'-',$offset) || nqp::eqat(self,'−',$offset)
                   )
                !! nqp::eqat(self,'-',$offset) || nqp::eqat(self,'−',$offset)
                  ?? negate(                                          # -0???
                       self!val-starts-with-int($radix, value, $pos)
                     )
                  !! self!val-starts-with-int($radix, value, $pos)    # 0???
    }

    # Helper method for processing strings with a given radix.  Also takes
    # flags value to indicate whether the final value should be negated.
    method !val-radix-marked(int $radix, int $offset, int $flags) {
#nqp::say("  val-radix-marked");
        my \parsed := nqp::radix_I(
          $radix,self,$offset + nqp::eqat(self,'_',$offset),$flags,Int);
        my \value  := nqp::atpos(parsed,0);
        my int $pos = nqp::atpos(parsed,2);
        $pos == nqp::chars(self)
          ?? value
          !! $pos == -1
            ?? self!failed($offset,                                   # 0xhuh
                 "No digits found after radix marker")
            !! nqp::eqat(self,'.',$pos)
              ?? self!val-radix-fract(value, $radix, $pos + 1)        # 0x1.
              !! nqp::eqat(self,'*',$pos)
                ?? self!val-power(value, $pos + 1)                    # 0x2*??
                !! self!notdone($pos, value)                          # 0x1???
    }

    # Helper method for processing strings with a given radix and a
    # fractional part.
    method !val-radix-fract(\whole, int $radix, int $offset) {
#nqp::say("  val-radix-fract");
        my \parsed := nqp::radix_I($radix,self,$offset,0,Int);
        my int $pos = nqp::atpos(parsed,2);
        $pos == -1
          ?? self!failed($offset,                                     # 123.huh
               "No fraction found after the decimal point")
          !! self!val-radix-fract-dot(                                # 123.45??
               whole, nqp::atpos(parsed,1), nqp::atpos(parsed,0), $pos)
    }

    # Helper method for creating the result of a string with a given radix
    # and a fractional part.
    method !val-radix-fract-dot(\whole, \multiplier, \fract, int $pos) {
#nqp::say("  val-radix-fract-dot");
        my \value := DIVIDE_NUMBERS(
            whole < 0
              ?? nqp::sub_I(nqp::mul_I(whole,multiplier,Int),fract,Int)
              !! nqp::add_I(nqp::mul_I(whole,multiplier,Int),fract,Int),
            multiplier,Rat,Rat
        );
        $pos == nqp::chars(self)
          ?? value                                                    # 0x2.4
          !! nqp::eqat(self,'*',$pos)
            ?? self!val-power(value, $pos + 1)                        # 0x2.4*??
            !! self!notdone($pos, value)                              # 0x2.4???
    }

    # Entry point for parsing a value that didn't start with anything
    # numeric.  Returns with either a dotty Rat, a Num, or a Failed or a
    # NotDone.
    method !val-no-int(int $offset) {
#nqp::say("  val-no-int");
        nqp::eqat(self,'.',$offset)
          ?? self!val-rat-fract(0, 10, $offset + 1)                   # .???
          !! nqp::eqat(self,'-',$offset) || nqp::eqat(self,'−',$offset)
            ?? negate(self!val-no-int-plus-minus($offset + 1))        # -???
            !! nqp::eqat(self,'+',$offset)
              ?? self!val-no-int-plus-minus($offset + 1)              # +???
              !! nqp::eqat(self,':',$offset)
                ?? self!val-radix($offset + 1)                        # :????
                !! self!val-named($offset)                            # ???
    }

    # Helper method for handling some named constant prefixed with a
    # plus or a minus.
    method !val-no-int-plus-minus(int $offset) {
#nqp::say("  val-no-int-plus-minus");
        nqp::eqat(self,'.',$offset)                                   # ±???
          ?? self!val-rat-fract(0, 10, $offset + 1)                   # ±.???
          !! nqp::eqat(self,':',$offset)
            ?? self!val-radix($offset + 1)                            # ±:????
            !! self!val-named($offset)                                # ±???
    }

    # Entry point for parsing a value that started with an int value,
    # followed by a non-numerical char.  Returns with either a dotty
    # Rat, a Num, or a Failed or a NotDone.
    method !val-starts-with-int(int $radix, \value, int $pos) {
#nqp::say("  val-starts-with-int");
        nqp::eqat(self,'.',$pos)
          ?? self!val-rat-fract(value, $radix, $pos + 1)              # 12.???
          !! nqp::eqat(self,'e',$pos) || nqp::eqat(self,'E',$pos)
            ?? self!val-num(value, $pos + 1)                          # 12e???
            !! nqp::eqat(self,'*',$pos)
              ?? self!val-power(value, $pos + 1)                      # 12*???
              !! self!notdone($pos, value)                            # 12???
    }

    # Entry point for parsing named values, such as Inf, NaN.  Returns with
    # either a Num, a Failed or a NotDone.
    method !val-named(int $offset) {
#nqp::say("  val-named");
        nqp::eqat(self,'Inf',$offset)
          ?? $offset == nqp::chars(self) - 5 
               && nqp::eqat(self,'Inf-i',$offset)
            ?? self!failed($offset + 4)                               # Inf-i
            !! self!val-named-trailingi($offset + 3, Inf)             # Inf??
          !! nqp::eqat(self,'∞',$offset)
            ?? self!deduce-notdone($offset + 1, Inf)                  # ∞???
            !! nqp::eqat(self,'NaN',$offset)
              ?? self!val-named-trailingi($offset + 3, NaN)           # NaN??
              !! self!failed($offset,
                   "base-10 number must begin with valid digits or '.'")
    }

    # Helper method for checking trailing "i" for incorrect complex numbers
    # that have Inf/NaN in their imaginary part.
    method!val-named-trailingi($offset, \value) {
#nqp::say("  val-named-trailingi");
        nqp::eqat(self,'i',$offset)
          ?? self!failed($offset)
          !! self!deduce-notdone($offset, value)
    }

    # Entry point for parsing an Int or a dotty Rat, with or without an
    # Int part.  So accepts strings in the form "DDD", "DDD.DDD" and ".DDD".
    # Returns a value, a Failed or a NotDone.
    method !val-int-rat(int $radix, int $offset) {
#nqp::say("  val-int-rat");
        nqp::eqat(self,'.',$offset)
          ?? self!val-rat-fract(0, $radix, $offset + 1)               # .???
          !! self!val-int-rat-whole($radix, $offset);                 # ???
    }

    # Helper method for parsing an Int or a dotty Rat with an Int part.
    # Accepts strings in the form "DDD" and "DDD.DDD".  Returns an Int,
    # a Rat, a Failed or a NotDone.
    method !val-int-rat-whole(int $radix, int $offset) {
#nqp::say("  val-int-rat-whole");
        my \parsed := nqp::radix_I($radix,self,$offset,0,Int);
        my \value  := nqp::atpos(parsed,0);
        my int $pos = nqp::atpos(parsed,2);
        $pos == -1
          ?? self!failed($offset,                                    # ???
               "Did not find any numeric value where one was expected")
          !! nqp::eqat(self,'.',$pos)
            ?? self!val-rat-fract(value, $radix, $pos + 1)           # 123.???
            !! self!deduce-notdone($pos, value)                      # 123???
    }

    # Helper method for parsing the fractional part of a dotty Rat.  Accepts
    # strings of the form "DDD".  Returns a Rat, Failed or a NotDone.
    method !val-rat-fract(\whole, int $radix, int $offset) {
#nqp::say("  val-rat-fract");
        my \parsed := nqp::radix_I($radix,self,$offset,0,Int);
        my int $pos = nqp::atpos(parsed,2);
        $pos == -1
          ?? self!failed($offset,                                     # 123.huh
               "No fraction found after the decimal point")
          !! self!val-rat-fract-dot(                                  # 123.45??
               whole, nqp::atpos(parsed,1), nqp::atpos(parsed,0), $pos)
    }

    # Helper method for creating a Rat.  Returns a Rat or a NotDone.
    method !val-rat-fract-dot(\whole, \multiplier, \fract, int $pos) {
#nqp::say("  val-rat-fract-dot");
        my \value := DIVIDE_NUMBERS(
          (whole < 0
            ?? nqp::sub_I(nqp::mul_I(whole,multiplier,Int),fract,Int)
            !! nqp::add_I(nqp::mul_I(whole,multiplier,Int),fract,Int)
          ), multiplier, Rat, Rat);

        $pos == nqp::chars(self)
          ?? value                                                    # 12.4
          !! nqp::eqat(self,'e',$pos) || nqp::eqat(self,'E',$pos)
            ?? self!val-num(value, $pos + 1)                          # 12.4e??
            !! nqp::eqat(self,'*',$pos)
              ?? self!val-power(value, $pos + 1)                      # 12.4*??
              !! self!notdone($pos, value)                            # 12.4??
    }

    # Entry point for creating a Num.  Returns a Num or a NotDone.
    # Takes a value (could be either an Int or a Rat) and the offset at
    # which to start parsing for the exponent.
    method !val-num(\sofar, int $offset) {
#nqp::say("  val-num");
        my \parsed   := nqp::radix_I(10,self,$offset,0x02,Int);
        my \exponent := nqp::atpos(parsed,0);
        my int $pos   = nqp::atpos(parsed,2);
        $pos == nqp::chars(self)
          ?? numify(sofar, exponent)                                  # 12e5
          !! $pos == -1
            ?? self!failed($offset,                                   # 12ehuh
                 "No exponent found after '{nqp::substr(self,$offset - 1,1)}'")
            !! nqp::eqat(self,'*',$pos)
              ?? self!val-power(numify(sofar, exponent), $pos + 1)    # 12e5*???
              !! self!deduce-notdone($pos, numify(sofar, exponent))   # 12e5???
    }

    # Entry point for when an adverbial : is encountered.  Takes the offset
    # at which a decimal number should be found.  Either returns a numeric
    # value, or a Blob, or a NotDone or a Failed.
    method !val-radix(int $offset) {
#nqp::say("  val-radix");
        my \parsed := nqp::radix_I(10,self,$offset,0,Int);
        my int $pos = nqp::atpos(parsed,2);
        $pos == -1
          ?? self!failed($offset,                                     # :
               "No decimal value found after colon")
          !! $pos == nqp::chars(self)
            ?? self!failed($pos,                                      # :10
                 "No value found after general radix specification")
            !! self!val-radix-which(nqp::atpos(parsed,0), $pos)       # :10???
    }

    # Entry point for general radix processing: :10<>, :10[] and :10{}.
    # Returns either an Int, Rat or Blob, or a Failed or NotDone.
    method !val-radix-which(int $radix, int $pos) {
#nqp::say("  val-radix-which");
        nqp::eqat(self,'<',$pos)
          ?? self!val-radix-general($radix, $pos + 1, '>')            # :10<???
          !! nqp::eqat(self,'«',$pos)
            ?? self!val-radix-general($radix, $pos + 1, '»')          # :10«???
            !! nqp::eqat(self,'[',$pos)
              ?? self!val-radix-list($radix, $pos + 1)                # :10[???
              !! nqp::eqat(self,'{',$pos)
                ?? self!val-radix-blob($radix, $pos + 1)              # :10{???
                !! self!failed($pos,                                  # :10huh
                   "Incorrect delimiter found for radix specification")
    }

    # Entry point for :10<> processing.  Checks for valid radix value.
    # Returns either an Int or Rat, or a Failed or NotDone.
    method !val-radix-general(int $radix, int $offset, Str:D $end) {
#nqp::say("  val-radix-general");
        $radix < 2 || $radix > 36
          ?? self!failed($offset,                                     # :99<???
               "Radix $radix out of range, should be 2 <= radix <= 36")
          !! self!ends-with($end,                                     # :10<???
               self!val-parse-value($radix, $offset))
    }

    # Entry point for :10[] processing.
    method !val-radix-list(int $base, int $offset) {
#nqp::say("  val-radix-list");
        (my int $end = nqp::index(self,']',$offset)) == -1
          ?? self!failed(nqp::chars(self),
               "Could not find final ']' of radix-list")
          !! UNBASE_BRACKET(
               $base,
               nqp::substr(self,$offset,$end - $offset).split(',').map: {
                   $_
                     ?? $_ eq '.'
                       ?? $_
                       !! nqp::istype((my \value := $_!val),Failed)
                         ?? (return value)
                         !! value
                     !! (return self!failed(nqp::chars(self)))
               }
             )
    }

    # Entry point for :10{} processing.
    method !val-radix-blob(int $radix, int $offset) {
#nqp::say("  val-radix-blob");
        my int $pos = nqp::findnotcclass(
          nqp::const::CCLASS_WHITESPACE,
          self,$offset,nqp::chars(self) - $offset
        );
        $pos > nqp::chars(self)
          ?? self!failed($offset,
               "Nothing to create a blob off")
          !! self!blobify($radix, $pos)
    }

    # Helper method for creating a blob from a :10{ } blob literal.
    method !blobify(int $radix, int $start) {
#nqp::say("  blobify");
        my int $offset = $start;
        my \blob := Blob.new;  # nqp::create(Blob) doesn't work ??

        while $offset < nqp::chars(self)
          && nqp::not_i(nqp::eqat(self,'}',$offset)) {
            my \parsed := nqp::radix_I($radix,self,$offset,0,Int);
            my int $pos = nqp::atpos(parsed,2);
            if $pos == -1 {
                return self!failed($offset,
                  "Did not find valid value for Blob");
            }
            else {
                nqp::push_i(blob,nqp::atpos(parsed,0));
                $offset = nqp::findnotcclass(
                  nqp::const::CCLASS_WHITESPACE,
                  self,$pos,nqp::chars(self) - $pos
                );
            }
        }

        $offset < nqp::chars(self)
          ?? self!deduce-notdone($offset + 1, blob)
          !! self!failed(nqp::chars(self),
               "Did not find '}' closer")
    }

    # val() logic on the invocant.  Runs the main !val() logic on a trimmed
    # version of the invocant, and allomorphizes the result if it can, or
    # returns the invocant if parsing failed.
    proto method val(|) {*}
    multi method val(Str:D:) {
        my $r := self.trim!val;
        nqp::istype($r,Failed)
          ?? self
          !! nqp::istype($r,Int)
            ?? IntStr.new($r, self)
            !! nqp::istype($r,Rat)
              ?? RatStr.new($r, self)
              !! nqp::istype($r,Num)
                ?? NumStr.new($r, self)
                !! nqp::istype($r,Complex)
                  ?? ComplexStr.new($r, self)
                  !! nqp::istype($r,Blob)
                    ?? $r
                    !! die "No allomorph known for { $r.^name }"
    }

    # Basic Numeric conversion, using the val logic
    multi method Numeric(Str:D: --> Numeric:D) {
        nqp::istype((my \value := self.trim!val),Failed)
          ?? value.Failure(self)
          !! value
    }
}

multi sub prefix:<~>(Str:D \a --> Str:D) { a.Str }
multi sub prefix:<~>(str   $a --> str)   { $a    }

multi sub infix:<~>(str $a, str $b   --> str) {
    nqp::concat($a, $b)
}
multi sub infix:<~>(Str:D \a, str $b --> str) {
    nqp::concat(nqp::unbox_s(a), $b)
}
multi sub infix:<~>(str $a, Str:D \b --> str) {
    nqp::concat($a, nqp::unbox_s(b))
}

multi sub infix:<~>(Str:D \a, Str:D \b --> Str:D) {
    nqp::p6box_s(nqp::concat(nqp::unbox_s(a), nqp::unbox_s(b)))
}

multi sub infix:<~>(Cool:D \a, Str:D \b --> Str:D) {
    nqp::p6box_s(nqp::concat(nqp::unbox_s(a.Str), nqp::unbox_s(b)))
}
multi sub infix:<~>(Str:D \a, Cool:D \b --> Str:D) {
    nqp::p6box_s(nqp::concat(nqp::unbox_s(a), nqp::unbox_s(b.Str)))
}
multi sub infix:<~>(Cool:D \a, Cool:D \b --> Str:D) {
    nqp::p6box_s(nqp::concat(nqp::unbox_s(a.Str), nqp::unbox_s(b.Str)))
}

multi sub infix:<~>(Any:D \a, Str:D \b --> Str:D) {
    nqp::p6box_s(nqp::concat(nqp::unbox_s(a.Stringy), nqp::unbox_s(b)))
}
multi sub infix:<~>(Str:D \a, Any:D \b --> Str:D) {
    nqp::p6box_s(nqp::concat(nqp::unbox_s(a), nqp::unbox_s(b.Stringy)))
}
# Any/Any candidate in src/core.c/Stringy.pm6

multi sub infix:<~>(str @args --> str) { nqp::join('',@args) }
multi sub infix:<~>(@args)  { @args.join }
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
    nqp::hllbool(
      nqp::eqaddr(a.WHAT,b.WHAT)
      && nqp::iseq_s(nqp::unbox_s(a), nqp::unbox_s(b)) #?js: NFG
    )
}
multi sub infix:<===>(str $a, str $b --> Bool:D) {
    nqp::hllbool(nqp::iseq_s($a, $b)) #?js: NFG
}

multi sub infix:<leg>(Str:D \a, Str:D \b --> Order:D) {
    ORDER(nqp::cmp_s(nqp::unbox_s(a), nqp::unbox_s(b)))
}
multi sub infix:<leg>(str $a, str $b --> Order:D) {
    ORDER(nqp::cmp_s($a, $b))
}

multi sub infix:<eq>(Str:D \a, Str:D \b --> Bool:D) {
    nqp::hllbool(nqp::iseq_s(nqp::unbox_s(a), nqp::unbox_s(b))) #?js: NFG
}
multi sub infix:<eq>(str $a, str $b --> Bool:D) {
    nqp::hllbool(nqp::iseq_s($a, $b)) #?js: NFG
}

multi sub infix:<ne>(Str:D \a, Str:D \b --> Bool:D) {
    nqp::hllbool(nqp::isne_s(nqp::unbox_s(a), nqp::unbox_s(b)))
}
multi sub infix:<ne>(str $a, str $b --> Bool:D) {
    nqp::hllbool(nqp::isne_s($a, $b))
}

multi sub infix:<lt>(Str:D \a, Str:D \b --> Bool:D) {
    nqp::hllbool(nqp::islt_s(nqp::unbox_s(a), nqp::unbox_s(b)))
}
multi sub infix:<lt>(str $a, str $b --> Bool:D) {
    nqp::hllbool(nqp::islt_s($a, $b))
}

multi sub infix:<le>(Str:D \a, Str:D \b --> Bool:D) {
    nqp::hllbool(nqp::isle_s(nqp::unbox_s(a), nqp::unbox_s(b)))
}
multi sub infix:<le>(str $a, str $b --> Bool:D) {
    nqp::hllbool(nqp::isle_s($a, $b))
}

multi sub infix:<gt>(Str:D \a, Str:D \b --> Bool:D) {
    nqp::hllbool(nqp::isgt_s(nqp::unbox_s(a), nqp::unbox_s(b)))
}
multi sub infix:<gt>(str $a, str $b --> Bool:D) {
    nqp::hllbool(nqp::isgt_s($a, $b))
}

multi sub infix:<ge>(Str:D \a, Str:D \b --> Bool:D) {
    nqp::hllbool(nqp::isge_s(nqp::unbox_s(a), nqp::unbox_s(b)))
}
multi sub infix:<le>(str $a, str $b --> Bool:D) {
    nqp::hllbool(nqp::isle_s($a, $b))
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
multi sub infix:«~>»(Str:D \a, Int:D \b) {
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
multi sub UNBASE(Int:D $base, Str:D $str --> Numeric:D) {
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
#?if !jvm
multi sub infix:<unicmp>(Str:D \a, Str:D \b --> Order:D) {
    ORDER(
        nqp::unicmp_s(
            nqp::unbox_s(a), nqp::unbox_s(b), 85,0,0))
}
multi sub infix:<unicmp>(Pair:D \a, Pair:D \b --> Order:D) {
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
multi sub infix:<coll>(Pair:D \a, Pair:D \b --> Order:D) {
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
multi sub substr(\what --> Str:D)                { what.substr             }
multi sub substr(\what, \from --> Str:D)         { what.substr(from)       }
multi sub substr(\what, \from, \chars --> Str:D) { what.substr(from,chars) }

proto sub substr-rw($, $?, $?, *%) is rw {*}
multi sub substr-rw(\what) is rw                { what.substr-rw             }
multi sub substr-rw(\what, \from) is rw         { what.substr-rw(from)       }
multi sub substr-rw(\what, \from, \chars) is rw { what.substr-rw(from,chars) }

multi sub infix:<eqv>(Str:D \a, Str:D \b --> Bool:D) {
    nqp::hllbool(
      nqp::unless(
        nqp::eqaddr(nqp::decont(a),nqp::decont(b)),
        nqp::eqaddr(a.WHAT,b.WHAT) && nqp::iseq_s(a,b)
      )
    )
}

proto sub samemark($, $, *%) {*}
multi sub samemark($s, $pat --> Str:D) { $s.samemark($pat) }

sub parse-names(Str:D \names) {
    # XXX TODO: issue deprecation warning in 6.d; remove in 6.e
    names.uniparse
}

proto sub uniparse($, *%) {*}
multi sub uniparse(Str:D \names --> Str:D) { names.uniparse }

# vim: ft=perl6 expandtab sw=4
