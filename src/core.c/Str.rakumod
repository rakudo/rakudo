my class Date    { ... }
my class Range   { ... }
my class Match   { ... }
my class Version { ... }
my class X::Cannot::Capture      { ... }
my class X::Str::InvalidCharName { ... }
my class X::Str::Numeric  { ... }
my class X::Str::Match::x { ... }
my class X::Str::Subst::Adverb { ... }
my class X::Str::Trans::IllegalKey { ... }
my class X::Str::Trans::InvalidArg { ... }
my class X::NoZeroArgMeaning { ... }
my class X::Numeric::Confused { ... }
my class X::Syntax::Number::RadixOutOfRange { ... }

my constant $?TABSTOP = 8;

my class Str does Stringy { # declared in BOOTSTRAP
    # class Str is Cool
    #     has str $!value is box_target;

    my $empty := nqp::list;   # for nqp::splice

    # cache cursor initialization lookup
    my $cursor-init := Match.^lookup("!cursor_init");

    my \CURSOR-GLOBAL     := Match.^lookup("CURSOR_MORE"   );  # :g
    my \CURSOR-OVERLAP    := Match.^lookup("CURSOR_OVERLAP");  # :ov
    my \CURSOR-EXHAUSTIVE := Match.^lookup("CURSOR_NEXT"   );  # :ex

    my &POST-MATCH  := Match.^lookup("MATCH" );  # Match object
    my &POST-STR    := Match.^lookup("STR"   );  # Str object

    my &POPULATE := Match.^lookup("MATCH" );  # populate Match object

    multi method IO(Str:D:) { IO::Path.new(self) }

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

    multi method Str(Str:D:) { self }
    multi method Stringy(Str:D:) { self }
    multi method DUMP(Str:D: --> Str:D) { self.raku }

    proto method COERCE(|) {*}
    multi method COERCE(Mu \s) {
        self.new(:value(nqp::p6box_s(s)))
    }

    method Int(Str:D: --> Int:D) {
        nqp::istype((my $n := self.Numeric),Int) || nqp::istype($n,Failure)
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
    method Version(Str:D: --> Version:D) { Version.new(self) }

    multi method ACCEPTS(Str:D: Str:D $other --> Bool:D) {
        nqp::hllbool(nqp::iseq_s($other,$!value));
    }
    multi method ACCEPTS(Str:D: Any:D \other --> Bool:D) {
        nqp::hllbool(nqp::iseq_s(other.Str,$!value));
    }

    multi method chomp(Str:D: --> Str:D) {
        nqp::box_s(
          nqp::substr(
            self,
            0,
            nqp::chars(self) - nqp::iscclass(                       #?js: NFG
              nqp::const::CCLASS_NEWLINE,self,nqp::chars(self) - 1  #?js: NFG
            )
          ),
          self
        )
    }
    multi method chomp(Str:D: Str:D $needle--> Str:D) {
        my int $offset = nqp::sub_i(nqp::chars(self),nqp::chars($needle));
        nqp::eqat(self,$needle,$offset)
          ?? nqp::substr(self,0,$offset)
          !! self
    }

    multi method chop(Str:D: --> Str:D) {
        nqp::box_s(
          nqp::substr(
            self,
            0,
            nqp::chars(self) && nqp::chars(self) - 1
          ),
          self
        )
    }
    multi method chop(Str:D: Int:D $chopping --> Str:D) {
        nqp::box_s(
          nqp::substr(
            self,
            0,
            nqp::not_i(nqp::isbig_I(nqp::decont($chopping)))
              && nqp::isgt_i(nqp::chars(self),$chopping)
              && nqp::sub_i(nqp::chars(self),$chopping)
          ),
          self
        )
    }
    multi method chop(Str:D: $chopping --> Str:D) {
        self.chop($chopping.Int)
    }

    multi method starts-with(Str:D:
      Str:D $needle, :i(:$ignorecase)!, :m(:$ignoremark)
    --> Bool:D) {
        nqp::hllbool($ignorecase
          ?? $ignoremark
#?if moar
            ?? nqp::eqaticim(self,$needle,0)
            !! nqp::eqatic(self,$needle,0)
#?endif
#?if !moar
            ?? self!die-named('ignorecase and :ignoremark')
            !! nqp::eqat(nqp::fc(self),nqp::fc($needle),0)
#?endif
          !! $ignoremark
#?if moar
            ?? nqp::eqatim(self,$needle,0)
#?endif
#?if !moar
            ?? self!die-named('ignoremark')
#?endif
            !! nqp::eqat(self,$needle,0)
        )
    }

    multi method starts-with(Str:D:
      Str:D $needle, :m(:$ignoremark)!
    --> Bool:D) {
        nqp::hllbool($ignoremark
#?if moar
          ?? nqp::eqatim(self,$needle,0)
#?endif
#?if !moar
          ?? self!die-named('ignoremark')
#?endif
          !! nqp::eqat(self,$needle,0)
        )
    }

    multi method starts-with(Str:D: Str:D $needle --> Bool:D) {
        nqp::hllbool(nqp::eqat(self, $needle, 0))
    }

    multi method ends-with(Str:D:
      Str:D $needle, :i(:$ignorecase)!, :m(:$ignoremark)
    --> Bool:D) {
        nqp::hllbool($ignorecase
          ?? $ignoremark
#?if moar
            ?? nqp::eqaticim(self,$needle,
                 nqp::sub_i(nqp::chars(self),nqp::chars($needle)))
            !! nqp::eqatic(self,$needle,
               nqp::sub_i(nqp::chars(self),nqp::chars($needle)))
#?endif
#?if !moar
            ?? self!die-named('ignorecase and :ignoremark')
            !! nqp::eqat(nqp::fc(self),nqp::fc($needle),
                 nqp::sub_i(nqp::chars(self),nqp::chars($needle)))
#?endif
          !! $ignoremark
#?if moar
            ?? nqp::eqatim(self,$needle,
                 nqp::sub_i(nqp::chars(self),nqp::chars($needle)))
#?endif
#?if !moar
            ?? self!die-named('ignoremark')
#?endif
            !! nqp::eqat(self,$needle,
                 nqp::sub_i(nqp::chars(self),nqp::chars($needle)))
        )
    }

    multi method ends-with(Str:D:
      Str:D $needle, :m(:$ignoremark)!
    --> Bool:D) {
        nqp::hllbool($ignoremark
#?if moar
          ?? nqp::eqatim(self,$needle,
               nqp::sub_i(nqp::chars(self),nqp::chars($needle)))
#?endif
#?if !moar
          ?? self!die-named('ignoremark')
#?endif
          !! nqp::eqat(self,$needle,
               nqp::sub_i(nqp::chars(self),nqp::chars($needle)))
        )
    }

    multi method ends-with(Str:D:
      Str:D $needle
    --> Bool:D) {
        nqp::hllbool(
          nqp::eqat(
            self,$needle,nqp::sub_i(nqp::chars(self),nqp::chars($needle))
          )
        )
    }

    multi method substr-eq(Str:D:
      Str:D $needle, Int:D $pos, :i(:$ignorecase)!, :m(:$ignoremark)
    --> Bool:D) {
        nqp::isbig_I(nqp::decont($pos)) || nqp::islt_i($pos,0)
          ?? self!fail-oor($pos)
          !! nqp::hllbool($ignorecase
               ?? $ignoremark
#?if moar
                 ?? nqp::eqaticim(self,$needle,$pos)
                 !! nqp::eqatic(self,$needle,$pos)
#?endif
#?if !moar
                 ?? self!die-named('ignorecase and :ignoremark')
                 !! nqp::eqat(nqp::fc(self),nqp::fc($needle),$pos)
#?endif
               !! $ignoremark
#?if moar
                 ?? nqp::eqatim(self,$needle,$pos)
#?endif
#?if !moar
                 ?? self!die-named('ignoremark')
#?endif
                 !! nqp::eqat(self,$needle,$pos)
             )
    }

    multi method substr-eq(Str:D:
      Str:D $needle, Int:D $pos, :m(:$ignoremark)!
    --> Bool:D) {
        nqp::isbig_I(nqp::decont($pos)) || nqp::islt_i($pos,0)
          ?? self!fail-oor($pos)
          !! nqp::hllbool($ignoremark
#?if moar
               ?? nqp::eqatim(self,$needle,$pos)
#?endif
#?if !moar
               ?? self!die-named('ignoremark')
#?endif
               !! nqp::eqat(self,$needle,$pos)
             )
    }

    multi method substr-eq(Str:D: Str:D $needle --> Bool:D) {
        self.starts-with($needle, |%_)
    }
    multi method substr-eq(Str:D: Str:D $needle, Int:D $pos --> Bool:D) {
        nqp::isbig_I(nqp::decont($pos)) || nqp::islt_i($pos,0)
          ?? self!fail-oor($pos)
          !! nqp::hllbool(nqp::eqat(self,$needle,$pos))
    }

    multi method substr-eq(Str:D: Str:D $needle, Callable:D $pos --> Bool:D) {
        self.substr-eq($needle, $pos(self.chars), |%_)
    }

    multi method contains(Str:D:
      Str:D $needle, :i(:$ignorecase)!, :m(:$ignoremark)
    --> Bool:D) {
        nqp::hllbool(
          nqp::isne_i($ignorecase
            ?? $ignoremark
#?if moar
              ?? nqp::indexicim(self,$needle,0)
              !! nqp::indexic(self,$needle,0)
#?endif
#?if !moar
              ?? self!die-named('ignorecase and :ignoremark')
              !! nqp::index(nqp::fc(self),nqp::fc($needle),0)
#?endif
            !! $ignoremark
#?if moar
              ?? nqp::indexim(self,$needle,0)
#?endif
#?if !moar
              ?? self!die-named('ignoremark')
#?endif
              !! nqp::index(self,$needle,0),
            -1
          )
        )
    }

    multi method contains(Str:D:
      Str:D $needle, :m(:$ignoremark)!
    --> Bool:D) {
        nqp::hllbool(
          nqp::isne_i($ignoremark
#?if moar
            ?? nqp::indexim(self,$needle,0)
#?endif
#?if !moar
            ?? self!die-named('ignoremark')
#?endif
            !! nqp::index(self,$needle,0),
            -1
          )
        )
    }

    multi method contains(Str:D: Str:D $needle --> Bool:D) {
        nqp::hllbool(nqp::isne_i(nqp::index($!value,$needle,0),-1))
    }
    multi method contains(Str:D: Regex:D $needle --> Bool:D) {
        nqp::hllbool(
          nqp::isge_i(
            nqp::getattr_i($needle($cursor-init(Match,self,:0c)),Match,'$!pos'),
            0
          )
        )
    }

    multi method contains(Str:D:
      Str:D $needle, Int:D $pos, :i(:$ignorecase)!, :m(:$ignoremark)
    --> Bool:D) {
        nqp::isbig_I(nqp::decont($pos)) || nqp::islt_i($pos,0)
          ?? self!fail-oor($pos)
          !! nqp::hllbool(
               nqp::isne_i($ignorecase
                 ?? $ignoremark
#?if moar
                   ?? nqp::indexicim(self,$needle,$pos)
                   !! nqp::indexic(self,$needle,$pos)
#?endif
#?if !moar
                   ?? self!die-named('ignorecase and :ignoremark')
                   !! nqp::index(nqp::fc(self),nqp::fc($needle),$pos)
#?endif
                 !! $ignoremark
#?if moar
                   ?? nqp::indexim(self,$needle,$pos)
#?endif
#?if !moar
                   ?? self!die-named('ignoremark')
#?endif
                   !! nqp::index(self,$needle,$pos),
                 -1
               )
             )
    }

    multi method contains(Str:D:
      Str:D $needle, Int:D $pos, :m(:$ignoremark)!
    --> Bool:D) {
        nqp::isbig_I(nqp::decont($pos)) || nqp::islt_i($pos,0)
          ?? self!fail-oor($pos)
          !! nqp::hllbool(
               nqp::isne_i(
                 $ignoremark
#?if moar
                   ?? nqp::indexim(self,$needle,$pos)
#?endif
#?if !moar
                   ?? self!die-named('ignoremark')
#?endif
                   !! nqp::index(self,$needle,$pos),
                 -1,
               )
             )
    }

    multi method contains(Str:D: Str:D $needle, Int:D $pos --> Bool:D) {
        nqp::isbig_I(nqp::decont($pos)) || nqp::islt_i($pos,0)
          ?? self!fail-oor($pos)
          !! nqp::hllbool(nqp::isne_i(nqp::index(self,$needle,$pos),-1))
    }
    multi method contains(Str:D: Regex:D $needle, Int:D $pos --> Bool:D) {
        nqp::isbig_I(nqp::decont($pos)) || nqp::islt_i($pos,0)
          ?? self!fail-oor($pos)
          !! nqp::hllbool(
               nqp::islt_i($pos,nqp::chars(self)) && nqp::isge_i(
                 nqp::getattr_i(
                   $needle($cursor-init(Match,self,:c($pos))),Match,'$!pos'
                 ),
                 0
               )
        )
    }

    # create indices using index
    method !indices(str $needle, $overlap, int $start) {
        my $indices := nqp::create(IterationBuffer);
        my int $add  = $overlap ?? 1 !! nqp::chars($needle) || 1;
        my int $pos  = $start;
        my int $index;
        nqp::while(
          nqp::isne_i(($index = nqp::index(self,$needle,$pos)),-1),
          nqp::stmts(
            nqp::push($indices,nqp::p6box_i($index)),
            ($pos = nqp::add_i($index,$add))
          )
        );
        $indices.List
    }

    # create indices using index with ignorecase
    method !indicesic(str $needle, $overlap, int $start) {
        my $indices := nqp::create(IterationBuffer);
        my int $add  = $overlap ?? 1 !! nqp::chars($needle) || 1;
        my int $pos  = $start;
        my int $index;
#?if moar
        nqp::while(
          nqp::isne_i(($index = nqp::indexic(self,$needle,$pos)),-1),
#?endif
#?if !moar
        my str $fcself   = nqp::fc(self);
        my str $fcneedle = nqp::fc($needle);
        nqp::while(
          nqp::isne_i(($index = nqp::index($fcself,$fcneedle,$pos)),-1),
#?endif
          nqp::stmts(
            nqp::push($indices,nqp::p6box_i($index)),
            ($pos = nqp::add_i($index,$add))
          )
        );
        $indices.List
    }

    # create indices using index with ignoremark
    method !indicesim(str $needle, $overlap, int $start) {
#?if moar
        my $indices := nqp::create(IterationBuffer);
        my int $add  = $overlap ?? 1 !! nqp::chars($needle) || 1;
        my int $pos  = $start;
        my int $index;
        nqp::while(
          nqp::isne_i(($index = nqp::indexim(self,$needle,$pos)),-1),
          nqp::stmts(
            nqp::push($indices,nqp::p6box_i($index)),
            ($pos = nqp::add_i($index,$add))
          )
        );
        $indices.List
#?endif
#?if !moar
        self!die-named('ignoremark',2)
#?endif
    }

    # create indices using index with ignorecase and ignoremark
    method !indicesicim(str $needle, $overlap, int $start) {
#?if moar
        my $indices := nqp::create(IterationBuffer);
        my int $add  = $overlap ?? 1 !! nqp::chars($needle) || 1;
        my int $pos  = $start;
        my int $index;
        nqp::while(
          nqp::isne_i(($index = nqp::indexicim(self,$needle,$pos)),-1),
          nqp::stmts(
            nqp::push($indices,nqp::p6box_i($index)),
            ($pos = nqp::add_i($index,$add))
          )
        );
        $indices.List
#?endif
#?if !moar
        self!die-named('ignorecase and :ignoremark',2)
#?endif
    }

    multi method indices(Str:D:
      Str:D $needle, :i(:$ignorecase), :m(:$ignoremark), :$overlap
    ) {
        $ignorecase
          ?? $ignoremark
            ?? self!indicesicim($needle, $overlap, 0)
            !! self!indicesic($needle, $overlap, 0)
          !! $ignoremark
            ?? self!indicesim($needle, $overlap, 0)
            !! self!indices($needle, $overlap, 0)
    }

    multi method indices(Str:D:
      Str:D $needle, Int:D $pos, :i(:$ignorecase), :m(:$ignoremark), :$overlap
    ) {
        nqp::isbig_I(nqp::decont($pos)) || nqp::islt_i($pos,0)
          ?? self!fail-oor($pos)
          !! $ignorecase
            ?? $ignoremark
              ?? self!indicesicim($needle, $overlap, $pos)
              !! self!indicesic($needle, $overlap, $pos)
            !! $ignoremark
              ?? self!indicesim($needle, $overlap, $pos)
              !! self!indices($needle, $overlap, $pos)
    }

    multi method indices(Str:D: Str:D $needle, :$overlap) {
        self!indices($needle, $overlap, 0)
    }
    multi method indices(Str:D: Str:D $needle, Int:D $pos, :$overlap) {
        nqp::isbig_I(nqp::decont($pos)) || nqp::islt_i($pos,0)
          ?? self!fail-oor($pos)
          !! self!indices($needle, $overlap, $pos)
    }

#?if !moar
    # helper method for quitting if not supported
    method !die-named(str $named, $levels = 1) {
        NYI("Named parameter ':$named' on '{
            callframe($levels + 1).code.name
        }'").throw
    }
#?endif

    multi method index(Str:D:
      Str:D $needle, :i(:$ignorecase)!, :m(:$ignoremark)
    --> Int:D) {
        nqp::isne_i(
          (my $index := $ignorecase
            ?? $ignoremark
#?if moar
              ?? nqp::indexicim(self,$needle,0)
              !! nqp::indexic(self,$needle,0)
#?endif
#?if !moar
              ?? self!die-named('ignorecase and :ignoremark')
              !! nqp::index(nqp::fc(self),nqp::fc($needle),0)
#?endif
            !! $ignoremark
#?if moar
              ?? nqp::indexim(self,$needle,0)
#?endif
#?if !moar
              ?? self!die-named('ignoremark')
#?endif
              !! nqp::index(self,$needle,0)
          ),-1
        ) ?? $index !! Nil
    }

    multi method index(Str:D:
      Str:D $needle, Int:D $pos, :i(:$ignorecase)!, :m(:$ignoremark)
    --> Int:D) {
        nqp::isbig_I(nqp::decont($pos)) || nqp::islt_i($pos,0)
          ?? self!fail-oor($pos)
          !! nqp::isne_i(
               (my $index := $ignorecase
                 ?? $ignoremark
#?if moar
                   ?? nqp::indexicim(self,$needle,$pos)
                   !! nqp::indexic(self,$needle,$pos)
#?endif
#?if !moar
                   ?? self!die-named('ignorecase and :ignoremark')
                   !! nqp::index(nqp::fc(self),nqp::fc($needle),$pos)
#?endif
                 !! $ignoremark
#?if moar
                   ?? nqp::indexim(self,$needle,$pos)
#?endif
#?if !moar
                   ?? self!die-named('ignoremark')
#?endif
                   !! nqp::index(self,$needle,$pos)
               ),-1
             ) ?? $index !! Nil
    }

    multi method index(Str:D:
      @needles, :i(:$ignorecase)!, :m(:$ignoremark)
    --> Int:D) {
        my int $i;
        my int $index = -1;
        my int $chars = nqp::chars(self);
        if $ignorecase {
            if $ignoremark {
#?if moar
                $chars = $index = $i
                  if ($i =
                       nqp::indexicim(nqp::substr(self,0,$chars),.Str,0)
                     ) > -1
                  for @needles;
#?endif
#?if !moar
                self!die-named('ignorecase and :ignoremark')
#?endif
            }
            else {
#?if moar
                $chars = $index = $i
                  if ($i =
                        nqp::indexic(nqp::substr(self,0,$chars),.Str,0)
                     ) > -1
                  for @needles;
#?endif
#?if !moar
                my str $str = nqp::fc(self);
                $chars = $index = $i
                  if ($i =
                       nqp::index(nqp::substr(self,0,$chars),nqp::fc(.Str))
                     ) > -1
                  for @needles;
#?endif
            }
        }
        elsif $ignoremark {
#?if moar
            $chars = $index = $i
              if ($i = nqp::indexim(nqp::substr(self,0,$chars),.Str,0)) > -1
              for @needles;
#?endif
#?if !moar
            self!die-named('ignoremark')
#?endif
        }
        else {
            $chars = $index = $i
              if ($i = nqp::index(nqp::substr(self,0,$chars),.Str,0)) > -1
              for @needles;
        }

        $index == -1 ?? Nil !! $index
    }

    multi method index(Str:D:
      Str:D $needle, :m(:$ignoremark)!
    --> Int:D) {
        nqp::isne_i(
          (my $index := $ignoremark
#?if moar
            ?? nqp::indexim(self,$needle,0)
#?endif
#?if !moar
            ?? self!die-named('ignoremark')
#?endif
            !! nqp::index(self,$needle,0)
          ),-1
        ) ?? $index !! Nil
    }
    multi method index(Str:D:
      Str:D $needle, Int:D $pos, :m(:$ignoremark)!
    --> Int:D) {
        nqp::isbig_I(nqp::decont($pos)) || nqp::islt_i($pos,0)
          ?? self!fail-oor($pos)
          !! nqp::isne_i(
               (my $index := $ignoremark
#?if moar
                 ?? nqp::indexim(self,$needle,$pos)
#?endif
#?if !moar
                 ?? self!die-named('ignoremark')
#?endif
                 !! nqp::index(self,$needle,$pos)
               ),-1
             ) ?? $index !! Nil
    }
    multi method index(Str:D:
      @needles, :m(:$ignoremark)!
    --> Int:D) {
        my int $i;
        my int $index = -1;
        my int $chars = nqp::chars(self);
        if $ignoremark {
#?if moar
            $chars = $index = $i
              if ($i = nqp::indexim(nqp::substr(self,0,$chars),.Str,0)) > -1
              for @needles;
#?endif
#?if !moar
            self!die-named('ignorecase and :ignoremark')
#?endif
        }
        else {
            $chars = $index = $i
              if ($i = nqp::index(nqp::substr(self,0,$chars),.Str)) > -1
              for @needles;
        }

        $index == -1 ?? Nil !! $index
    }

    multi method index(Str:D: Str:D $needle --> Int:D) {
        nqp::isne_i((my $index := nqp::index(self,$needle)),-1)
          ?? $index !! Nil
    }
    multi method index(Str:D: Str:D $needle, Int:D $pos --> Int:D) {
        nqp::isbig_I(nqp::decont($pos)) || nqp::islt_i($pos,0)
          ?? self!fail-oor($pos)
          !! nqp::isne_i((my $index := nqp::index(self,$needle,$pos)),-1)
            ?? $index !! Nil
    }
    multi method index(Str:D: @needles --> Int:D) {
        my int $i;
        my int $index = -1;
        my int $chars = nqp::chars(self);
        $chars = $index = $i
          if ($i = nqp::index(nqp::substr(self,0,$chars), .Str)) > -1
          for @needles;
         $index == -1 ?? Nil !! $index
    }

    # helper method for failing with out of range exception
    method !fail-oor($got) {
        X::OutOfRange.new(
          :what("Position in calling '{ callframe(2).code.name }'"),
          :$got,
          :range("0..{ nqp::chars(self) }")
        ).Failure
    }

    multi method rindex(Str:D: Str:D $needle --> Int:D) {
        nqp::isne_i((my $index := nqp::rindex($!value,$needle)),-1)
          ?? $index !! Nil
    }
    multi method rindex(Str:D: Str:D $needle, Int:D $pos --> Int:D) {
        nqp::isbig_I(nqp::decont($pos)) || nqp::islt_i($pos,0)
          ?? self!fail-oor($pos)
          !! nqp::isne_i((my $index := nqp::rindex(self,$needle,$pos)),-1)
            ?? $index !! Nil
    }
    multi method rindex(Str:D: @needles --> Int:D) {
        my int $i;
        my int $index = -1;
        $index = $i
          if ($i = nqp::rindex(self,.Str)) > $index
          for @needles;
        $index == -1 ?? Nil !! $index
    }

    method pred(Str:D: --> Str:D) {
        (my int $chars = Rakudo::Internals.POSSIBLE-MAGIC-CHARS(self))
          ?? nqp::istype(
               (my $pred := Rakudo::Internals.PRED(self,$chars - 1)),
               Failure
             ) ?? $pred
               !! nqp::box_s($pred,self)
          !! self
    }

    method succ(Str:D: --> Str:D) {
        (my int $chars = Rakudo::Internals.POSSIBLE-MAGIC-CHARS(self))
          ?? nqp::istype(
               (my $succ := Rakudo::Internals.SUCC(self,$chars - 1)),
               Failure
             ) ?? $succ
               !! nqp::box_s($succ,self)
          !! self
    }

    method !combiners() {
        X::Str::Numeric.new(
          source => self,
          pos    => 1,
          reason => "combining codepoints found at"
        ).Failure
    }
    multi method Numeric(Str:D: Bool :$fail-or-nil --> Numeric:D) {
#?if !jvm
        # check for any combining characters
        nqp::isne_i(nqp::chars(self),nqp::codes(self))
          ?? self!combiners
          !!
#?endif
#?if jvm
            # https://github.com/Raku/old-issue-tracker/issues/5418
            # Needs Str.codes impl that doesn't just return chars
#?endif
        nqp::iseq_i(                              # all numeric?
          nqp::findnotcclass(
            nqp::const::CCLASS_NUMERIC,self,0,nqp::chars(self)),
          nqp::chars(self)
        ) ?? nqp::isle_i(nqp::chars($!value),18)  # upto 18 digits can be native
            ?? nqp::atpos(nqp::radix(10,self,0,0),0) # quick conversion, "" also
            !! nqp::atpos(nqp::radix_I(10,self,0,0,Int),0)
          !! nqp::atpos(                          # try parsing as an Int
               (my $n := nqp::radix_I(10,self,0,0b10,Int)),
               2
             ) == nqp::chars(self)
            ?? nqp::atpos($n,0)                   # fast path Int ok
            !! nqp::findnotcclass(                # any non-whitespace?
                 nqp::const::CCLASS_WHITESPACE,
                 self,0,nqp::chars(self)
               ) == nqp::chars(self)
              ?? 0                                # just spaces
              !! val(self, :val-or-fail, :$fail-or-nil)          # take the slow route
    }

    multi method gist(Str:D:) { self }
    multi method raku(Str:D: --> Str:D) {
        nqp::chars(self)
          ?? nqp::findnotcclass(
               nqp::const::CCLASS_WORD,self,0,nqp::chars(self)
             ) == nqp::chars(self)
            ?? nqp::concat('"',nqp::concat(self,'"'))  # fast path alpha
            !! self!rakufy                             # slow path non-alpha
          !! '""'                                      # empty string
    }

    # Special case escape values for rakufication
    my constant $escapes = do {
        my $list := nqp::list;
        nqp::bindpos($list,nqp::ord("\0"),  '\0');
        nqp::bindpos($list,nqp::ord('$'),   '\$');
        nqp::bindpos($list,nqp::ord('@'),   '\@');
        nqp::bindpos($list,nqp::ord('%'),   '\%');
        nqp::bindpos($list,nqp::ord('&'),   '\&');
        nqp::bindpos($list,nqp::ord('{'),   '\{');
        nqp::bindpos($list,nqp::ord("\b"),  '\b');
        nqp::bindpos($list,nqp::ord("\x0A"),'\n');
        nqp::bindpos($list,nqp::ord("\r"),  '\r');
        nqp::bindpos($list,nqp::ord("\t"),  '\t');
        nqp::bindpos($list,nqp::ord('"'),   '\"');
        nqp::bindpos($list,nqp::ord('\\'),  '\\\\');
        $list
    }

    # Helper method to create hex representation of char
    method !hexify(str $char) is pure {
        nqp::concat(
          '\x[',
          nqp::concat(
#?if !jvm
            $char.NFC.map( *.base(16) ).join(','),
#?endif
#?if jvm
            nqp::p6box_i(nqp::ord($char)).base(16),
#?endif
            ']'
          )
        )
    }

    # Under NFG-supporting implementations, must be sure that any leading
    # combiners are escaped, otherwise they will be combined onto the "
    # under concatenation closure, which ruins round-tripping. Also handle
    # the \r\n grapheme correctly.
    method !rakufy() {
        my $rakufied := nqp::list_s('"');              # array add chars to
        my int $chars = nqp::chars(self);

        my int $i = -1;
        my str $char;
        my int $ord;
        nqp::while(
          nqp::islt_i(++$i,$chars),                    # check all chars
          nqp::stmts(
            ($char = nqp::substr(self,$i,1)),
            ($ord  = nqp::ord($char)),
            nqp::push_s(
              $rakufied,
#?if !jvm
              nqp::if(
                nqp::isge_i($ord,768)                  # different from "0" ??
                  && nqp::isgt_i(
                       nqp::atpos(
                         nqp::radix_I(10,              # failure -> value 0
                           nqp::getuniprop_str(
                             $ord,
                             nqp::unipropcode('Canonical_Combining_Class')
                           ),
                           0,0,Int
                         ),
                         0
                       ),
                       0
                     ),
                self!hexify($char),                    # escape since > 0
#?endif
                nqp::if(
                  nqp::iseq_s($char,"\r\n"), # <-- this is a synthetic codepoint
                  '\r\n',                              # it's the common LF
                  nqp::ifnull(                         # not a common LF
                    nqp::atpos($escapes,$ord),
                    nqp::if(
                      nqp::iscclass(nqp::const::CCLASS_PRINTING,$char,0),
                      $char,                           # it's a printable
                      self!hexify($char)               # need to escape
                    )
                  )
                )
              )
#?if !jvm
            )
#?endif
          )
        );
        nqp::push_s($rakufied,'"');
        nqp::join('',$rakufied)
    }

    my class CombAll does PredictiveIterator {
        has str $!str;
        has Mu  $!what;
        has int $!pos;
        method !SET-SELF($string) {
            $!str   = $string;
            $!what := $string.WHAT;
            $!pos   = -1;
            self
        }
        method new($string) { nqp::create(self)!SET-SELF($string) }
        method pull-one() {
            nqp::islt_i(++$!pos,nqp::chars($!str))
              ?? nqp::box_s(nqp::substr($!str,$!pos,1),$!what) #?js: NFG
              !! IterationEnd
        }
        method skip-one() {
            nqp::islt_i(++$!pos,nqp::chars($!str))
        }
        method push-all(\target --> IterationEnd) {
            my str $str   = $!str;      # locals are faster
            my int $pos   = $!pos;
            my int $chars = nqp::chars($str);
            my Mu $what  := $!what;
            nqp::while(
              nqp::islt_i(++$pos,$chars),
              target.push(nqp::box_s(nqp::substr($str,$pos,1),$what)) #?js: NFG
            );
            $!pos = $pos;
        }
        method count-only(--> Int:D) {
            nqp::box_i(
              nqp::chars($!str) - $!pos - nqp::islt_i($!pos,nqp::chars($!str)),
              Int
            )
        }
        method sink-all(--> IterationEnd) { $!pos = nqp::chars($!str) }
    }
    multi method comb(Str:D: --> Seq:D) { Seq.new(CombAll.new(self)) }

    multi method comb(Str:D: Int:D $size, $limit = * --> Seq:D) {
        $size <= 1 && (nqp::istype($limit,Whatever) || $limit == Inf)
          ?? self.comb
          !! Seq.new:
               Rakudo::Iterator.NGrams: self, $size, $limit, $size, True
    }

    my class CombPat does Iterator {
        has str $!str;
        has Mu  $!what;
        has str $!pat;
        has int $!patsz;
        has int $!pos;
        method !SET-SELF($string, $pat) {
            $!str   = $string;
            $!what := $string.WHAT;
            $!pat   = $pat;
            $!patsz = nqp::chars($!pat);
            self
        }
        method new($string, $pat) { nqp::create(self)!SET-SELF($string, $pat) }
        method pull-one() {
            nqp::if(
              nqp::islt_i(
                (my int $found = nqp::index($!str,$!pat,$!pos)),
                0
              ),
              IterationEnd,
              nqp::stmts(
                $!pos = nqp::add_i($found,$!patsz),
                nqp::box_s($!pat,$!what)
              )
            )
        }
    }
    multi method comb(Str:D: Str:D $pat --> Seq:D) {
        $pat
          ?? Seq.new(CombPat.new(self,$pat))
          !! self.comb
    }

    my class CombPatLimit does Iterator {
        has str $!str;
        has Mu  $!what;
        has str $!pat;
        has int $!pos;
        has int $!todo;
        method !SET-SELF($string, $pat, $limit) {
            $!str   = $string;
            $!what := $string.WHAT;
            $!pat   = $pat;
            $!todo  = $limit.Int;
            self
        }
        method new($string, $pat, $limit) {
            nqp::create(self)!SET-SELF($string, $pat, $limit)
        }
        method pull-one() {
            nqp::if(
              nqp::islt_i(
                (my int $found = nqp::index($!str, $!pat, $!pos)),
                0
              ) || nqp::iseq_i($!todo,0),
              IterationEnd,
              nqp::stmts(
                ($!pos  = nqp::add_i($found,1)),
                --$!todo,
                nqp::box_s($!pat,$!what)
              )
            )
        }
    }
    multi method comb(Str:D: Str:D $pat, $limit --> Seq:D) {
        nqp::istype($limit,Whatever) || $limit == Inf
          ?? self.comb($pat)
          !! $pat
            ?? Seq.new(CombPatLimit.new(self, $pat, $limit))
            !! self.comb(1, $limit)
    }

    # iterate with post-processing
    my class POST-ITERATOR does Iterator {
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
            $!cursor := $!move($!cursor)
              if nqp::isge_i(nqp::getattr_i($!cursor,Match,'$!pos'),0)
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
    my class CURSOR-ITERATOR does Iterator {
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
            $!cursor := $!move($!cursor)
              if nqp::isge_i(nqp::getattr_i($!cursor,Match,'$!pos'),0)
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

    multi method comb(Str:D: Regex:D $regex, $limit = *, :$match! --> Seq:D) {
        Seq.new: $match
          ?? Rakudo::Iterator.MatchMatch: $regex, self, $limit
          !! Rakudo::Iterator.MatchStr:   $regex, self, $limit
    }
    multi method comb(Str:D: Regex:D $regex --> Seq:D) {
        Seq.new: Rakudo::Iterator.MatchStr: $regex, self, *
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
        my int $elems = @names.elems;   # reifies
        my $list     := nqp::getattr(@names,List,'$!reified');
        my int $i     = -1;
        nqp::while(
          nqp::islt_i(++$i,$elems),
          nqp::if(
            nqp::existskey(opts,nqp::unbox_s(nqp::atpos($list,$i))),
            (store = nqp::atkey(opts,nqp::unbox_s(nqp::atpos($list,$i)))),
          )
        );
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
    method !match-pattern(Mu \slash, $pattern, str $name, $value, \opts) {
        my $opts := nqp::getattr(opts,Map,'$!storage');
        nqp::bindkey($opts,$name,$value);
        fetch-short-long($opts, "p", "pos", my $p);
        fetch-short-long($opts, "c", "continue", my $c);
        nqp::unless(
          nqp::defined($c),
          $c = 0
        );
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
    }

    # Generic fallback for matching with a cursor.  This is typically
    # called if more than one named parameter was specified.  Arguments
    # 3/4 are the initial named parameter matched: instead of flattening
    # the named parameter into another slurpy hash, we pass the name and
    # the value as extra parameters, and add it back in the hash with
    # named parameters.
    method !match-cursor(Mu \slash, \cursor, str $name, $value, \opts) {
        my $opts := nqp::getattr(opts,Map,'$!storage');
        nqp::bindkey($opts,$name,$value) if nqp::chars($name);
        fetch-short-long($opts, "ex", "exhaustive", my $ex);
        fetch-short-long($opts, "ov", "overlap",    my $ov);
        my \move := $ex
          ?? CURSOR-EXHAUSTIVE
          !! $ov
            ?? CURSOR-OVERLAP
            !! CURSOR-GLOBAL;

        fetch-short-long($opts, "as", "as", my $as);
        my \post := nqp::istype($as,Str) ?? &POST-STR !! &POST-MATCH;

        fetch-short-long($opts, "g", "global", my $g);
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
    }

    # Match object at given position
    method !match-one(Mu \slash, \cursor) {
        slash
          = my $match := nqp::isge_i(nqp::getattr_i(cursor,Match,'$!pos'),0)
          ?? cursor.MATCH
          !! Nil;
        $match
    }

    # Some object at given position
    method !match-as-one(Mu \slash, \cursor, \as) {
        slash = my $match := nqp::if(
          nqp::isge_i(nqp::getattr_i(cursor,Match,'$!pos'),0),
          nqp::if(nqp::istype(as,Str), &POST-STR, &POST-MATCH)(cursor),
          Nil
        );
        $match
    }

    # Create list from the appropriate Sequence given the move
    method !match-list(Mu \slash, \cursor, \move, \post) {
        slash
          = my $match := nqp::isge_i(nqp::getattr_i(cursor,Match,'$!pos'),0)
            ?? Seq.new(POST-ITERATOR.new(cursor, move, post)).list
            !! List.new;
        $match
    }

    # Handle matching of the nth match specification.
    method !match-nth(Mu \slash, \cursor, \move, \post, $nth, %opts) {
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
    method !match-nth-int(Mu \slash, \cursor, \move, \post, int $nth) {
        slash
          = my $match := nqp::isge_i(nqp::getattr_i(cursor,Match,'$!pos'),0)
          ?? nqp::eqaddr(
              (my $pulled := POST-ITERATOR.new(cursor, move, post)
                .skip-at-least-pull-one(nqp::sub_i($nth,1))),
              IterationEnd
            )
            ?? Nil              # not enough matches
            !! $pulled          # found it!
          !! Nil;               # no matches whatsoever
        $match
    }

    # Give back the N-tail match found
    method !match-nth-tail(Mu \slash, \cursor, \move, int $tail) {
        slash = my $match := nqp::eqaddr(
          (my $pulled := Rakudo::Iterator.LastNValues(
            CURSOR-ITERATOR.new(cursor, move), $tail, 'match', 1
          ).pull-one),
          IterationEnd
        ) ?? Nil !! $pulled.MATCH;
        $match
    }

    # Give last value of given iterator, or Nil if none
    method !match-last(Mu \slash, \cursor, \move) {
        slash = my $match := nqp::eqaddr(
          (my $pulled := Rakudo::Iterator.LastValue(
            CURSOR-ITERATOR.new(cursor, move), 'match')
          ),
          IterationEnd
        ) ?? Nil !! $pulled.MATCH;
        $match
    }

    # These !match methods take an iterator instead of a cursor.
    # Give list with matches found given a range with :nth
    method !match-nth-range(Mu \slash, \iterator, $min, $max) {
        slash = my $match := nqp::stmts(
          (my int $skip = $min),
          nqp::if(
            nqp::islt_i($skip,1),
            die-before-first($min),
            nqp::stmts(
              nqp::while(
                nqp::isgt_i($skip,1) && iterator.skip-one,
                --$skip
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
                      nqp::iseq_i(++$i,$todo)
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
        );
        $match
    }

    # Give list with matches found given an iterator with :nth
    method !match-nth-iterator(Mu \slash, \source, \indexes) {
        slash = my $match := nqp::stmts(
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
        );
        $match
    }

    # Give list with matches found given an iterator with :x
    method !match-x(Mu \slash, $iterator, $x) {
        nqp::if(
          nqp::istype($x,Whatever),
          Seq.new($iterator).list,
          nqp::if(
            nqp::istype($x,Numeric),
            nqp::if(
              $x == Inf,
              Seq.new($iterator).list,
              nqp::if(
                nqp::istype($x,Int),
                self!match-x-range(slash, $iterator, $x, $x),
                nqp::stmts(
                  (my int $xint = $x.Int),
                  self!match-x-range(slash, $iterator, $xint, $xint)
                )
              )
            ),
            nqp::if(
              nqp::istype($x,Range),
              self!match-x-range(slash, $iterator, $x.min, $x.max),
              nqp::stmts(
                (slash = Nil),
                X::Str::Match::x.new(:got($x)).Failure
              )
            )
          )
        )
    }

    # Give list with matches found given a range with :x
    method !match-x-range(Mu \slash, $iterator, $min, $max) {
        slash = my $match := nqp::stmts(
          (my int $todo = nqp::if($max == Inf, 0x7fffffff, $max)),
          (my $matches := nqp::create(IterationBuffer)),
          nqp::until(
            nqp::islt_i(--$todo, 0) ||
              nqp::eqaddr((my $pulled := $iterator.pull-one),IterationEnd),
            nqp::push($matches,$pulled)
          ),
          nqp::if(
            nqp::elems($matches) >= $min,
            $matches.List,
            ()
          )
        );
        $match
    }

#?if moar
    proto method match(Any, |) {*}
#?endif
#?if !moar
    proto method match(Any, |) { $/ := nqp::getlexcaller('$/'); {*} }
#?endif
    multi method match(Cool:D $pattern, |c) {
        $/ := nqp::getlexcaller('$/');
        self.match(/ "$pattern": /,|c)
    }

    # All of these .match candidates take a single required named parameter
    # so that handling specification of a single named parameter can be much
    # quicker.  Unfortunately, we cannot cheaply do MMD on an empty slurpy
    # hash, which would make things much more simple.
    multi method match(Regex:D $pattern, :continue(:$c)!, *%_) {
        nqp::elems(nqp::getattr(%_,Map,'$!storage'))
          ?? self!match-pattern(
               nqp::getlexcaller('$/'), $pattern, 'c', $c, %_)
          !! self!match-one(
               nqp::getlexcaller('$/'),$pattern($cursor-init(Match,self,:$c)))
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
              CURSOR-GLOBAL, &POST-MATCH),
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
              CURSOR-OVERLAP, &POST-MATCH),
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
              CURSOR-EXHAUSTIVE, &POST-MATCH),
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
                CURSOR-GLOBAL, &POST-MATCH
              ), $x),
            self!match-one(nqp::getlexcaller('$/'),
              $pattern($cursor-init(Match,self,:0c)), $x)
          )
        )
    }
    multi method match(Regex:D $pattern, :$st!, *%_) {
        self!match-nth(nqp::getlexcaller('$/'),
          $pattern($cursor-init(Match,self,:0c)),
          CURSOR-GLOBAL, &POST-MATCH, $st, %_)
    }
    multi method match(Regex:D $pattern, :$nd!, *%_) {
        self!match-nth(nqp::getlexcaller('$/'),
          $pattern($cursor-init(Match,self,:0c)),
          CURSOR-GLOBAL, &POST-MATCH, $nd, %_)
    }
    multi method match(Regex:D $pattern, :$rd!, *%_) {
        self!match-nth(nqp::getlexcaller('$/'),
          $pattern($cursor-init(Match,self,:0c)),
          CURSOR-GLOBAL, &POST-MATCH, $rd, %_)
    }
    multi method match(Regex:D $pattern, :$th!, *%_) {
        self!match-nth(nqp::getlexcaller('$/'),
          $pattern($cursor-init(Match,self,:0c)),
          CURSOR-GLOBAL, &POST-MATCH, $th, %_)
    }
    multi method match(Regex:D $pattern, :$nth!, *%_) {
        self!match-nth(nqp::getlexcaller('$/'),
          $pattern($cursor-init(Match,self,:0c)),
          CURSOR-GLOBAL, &POST-MATCH, $nth, %_)
    }
    multi method match(Regex:D $pattern, :$as!, *%_) {
        nqp::elems(nqp::getattr(%_,Map,'$!storage'))
          ?? self!match-cursor(nqp::getlexcaller('$/'),
               $pattern($cursor-init(Match,self,:0c)), 'as', $as, %_)
          !! self!match-as-one(nqp::getlexcaller('$/'),
               $pattern($cursor-init(Match,self,:0c)), $as)
    }
    multi method match(Regex:D $pattern, *%_) {
        nqp::elems(nqp::getattr(%_,Map,'$!storage'))
          ?? self!match-cursor(nqp::getlexcaller('$/'),
               $pattern($cursor-init(Match,self,:0c)), '', 0, %_)
          !! self!match-one(nqp::getlexcaller('$/'),
               $pattern($cursor-init(Match,self,:0c)))
    }

#?if moar
    proto method subst-mutate(|) {*}
#?endif
#?if !moar
    proto method subst-mutate(|) { $/ := nqp::getlexcaller('$/'); {*} }
#?endif
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
          nqp::istype(matches,Failure) || nqp::isfalse(matches),
          $SET_DOLLAR_SLASH && (try caller_dollar_slash = $/),
          nqp::stmts(
            ($self = nqp::box_s(
              $self!APPLY-MATCHES(
                matches, $replacement,
                caller_dollar_slash, $SET_DOLLAR_SLASH,
                $word_by_word, $samespace, $samecase, $samemark
              ),
              self
            )),
            $SET_DOLLAR_SLASH && (try caller_dollar_slash = matches),
          )
        );

        matches
    }

    multi method subst(Str:D: Str:D $original, Str:D $final = "", *%options) {
        my $result := nqp::if(
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
        );

        nqp::istype($result,Failure)
          ?? $result
          !! nqp::box_s($result,self)
    }
    multi method subst(Str:D: $matcher, $replacement = "", *%options) {
        nqp::istype(
          (my $result := self!SUBST(
            nqp::getlexcaller('$/'),
            $matcher,
            $replacement,
            |%options
          )),
          Failure
        ) ?? $result
          !! nqp::box_s($result,self)
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

            # fast path for something like `s:g[ \w+ ] = "foo"`
            if !fancy && !callable {
                for (nqp::istype(matches, Capture) ?? flat matches !! matches.list) -> $m {
                    cds = $m if nqp::isrwcont(cds);
                    nqp::push_s(
                      $result,nqp::substr($str,$prev,nqp::unbox_i($m.from) - $prev)
                    );
                    $prev = nqp::unbox_i($m.to);
                }
                nqp::push_s($result,nqp::substr($str,$prev));
                nqp::p6box_s(nqp::join(nqp::unbox_s(~$replacement),$result));
            }
            else {
                for (nqp::istype(matches, Capture) ?? flat matches !! matches.list) -> $m {
                    cds = $m if nqp::isrwcont(cds) && SDS;
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
                              $it!word-by-word($mstr,&filter,:samespace(?space))
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
        }

        # simple string replacement
        else {
            for (nqp::istype(matches, Capture) ?? flat matches !! matches.list) -> $m {
                nqp::push_s(
                  $result,nqp::substr($str,$prev,nqp::unbox_i($m.from) - $prev)
                );
                $prev = nqp::unbox_i($m.to);
            }
            nqp::push_s($result,nqp::substr($str,$prev));
            nqp::p6box_s(nqp::join(nqp::unbox_s(~$replacement),$result));
        }
    }

    multi method lines(Str:D: :$count! --> Int:D) {
        # we should probably deprecate this feature
        $count ?? self.lines.elems !! self.lines;
    }
    multi method lines(Str:D: $limit --> Seq:D) {
        self.lines.head($limit)
    }
    multi method lines(Str:D: $limit, Bool :$chomp! --> Seq:D) {
        self.lines(:$chomp).head($limit)
    }

    my class Lines does PredictiveIterator {
        has str $!str;
        has Mu  $!what;
        has int $!chars;
        has int $!pos;
        method !SET-SELF($string) {
            $!str   = $string;
            $!what := $string.WHAT;
            $!chars = nqp::chars($!str);
            $!pos   = 0;
            self
        }
        method new($string) { nqp::create(self)!SET-SELF($string) }
        method pull-one() {
            nqp::if(
              (my int $left = $!chars - $!pos) > 0,
              nqp::stmts(
                (my int $findpos = nqp::findcclass(
                  nqp::const::CCLASS_NEWLINE, $!str, $!pos, $left)),
                (my $found := nqp::box_s(
                  nqp::substr($!str, $!pos, $findpos - $!pos),
                  $!what
                )),
                ($!pos = $findpos +
#?if moar
                  1
#?endif
#?if !moar
                  (nqp::iseq_s(nqp::substr($!str, $findpos, 2), "\r\n") ?? 2 !! 1)
#?endif
                ),
                $found
              ),
              IterationEnd
            )
        }
        method push-all(\target --> IterationEnd) {
            my int $left;
            my int $pos   = $!pos;
            my int $chars = $!chars;
            my str $str   = $!str;
            my Mu  $what := $!what;

            while ($left = $chars - $pos) > 0 {
                my int $findpos = nqp::findcclass(
                  nqp::const::CCLASS_NEWLINE, $str, $pos, $left);
                target.push(nqp::box_s(
                  nqp::substr($str, $pos, $findpos - $pos),
                  $what
                ));
                $pos = $findpos +
#?if moar
                  1
#?endif
#?if !moar
                  (nqp::iseq_s(nqp::substr($str, $findpos, 2), "\r\n") ?? 2 !! 1)
#?endif
                  ;
            }
        }
        method count-only(--> Int:D) {
            my int $left;
            my int $seen;
            my int $pos   = $!pos;
            my int $chars = $!chars;
            my str $str   = $!str;

            while ($left = $chars - $pos) > 0 {
                $pos = nqp::findcclass(
                  nqp::const::CCLASS_NEWLINE, $str, $pos, $left) + 1;
                $seen = $seen + 1;
            }
            nqp::p6box_i($seen)
        }
        method bool-only(--> Bool:D) {
            nqp::hllbool(nqp::islt_i($!pos,$!chars))
        }
        method sink-all(--> IterationEnd) { }
    }

    my class LinesKeepNL is Lines {
        has str $!str;
        has Mu  $!what;
        has int $!chars;
        has int $!pos;
        method !SET-SELF($string) {
            $!str   = $string;
            $!what := $string.WHAT;
            $!chars = nqp::chars($!str);
            $!pos   = 0;
            self
        }
        method new($string) { nqp::create(self)!SET-SELF($string) }
        method pull-one() {
            nqp::if(
              (my int $left = $!chars - $!pos) > 0,
              nqp::stmts(
                (my int $findpos = nqp::findcclass(
                  nqp::const::CCLASS_NEWLINE, $!str, $!pos, $left)),
                (my int $nextpos = $findpos +
#?if moar
                  1
#?endif
#?if !moar
                  (nqp::iseq_s(nqp::substr($!str, $findpos, 2), "\r\n") ?? 2 !! 1)
#?endif
                ),
                (my $found := nqp::box_s(
                  nqp::substr($!str, $!pos, $nextpos - $!pos),
                  $!what
                )),
                ($!pos = $nextpos),
                $found
              ),
              IterationEnd
            )
        }
        method push-all(\target --> IterationEnd) {
            my int $left;

            while ($left = $!chars - $!pos) > 0 {
                my int $findpos = nqp::findcclass(
                  nqp::const::CCLASS_NEWLINE, $!str, $!pos, $left);
                my int $nextpos = $findpos +
#?if moar
                  1
#?endif
#?if !moar
                  (nqp::iseq_s(nqp::substr($!str, $findpos, 2), "\r\n") ?? 2 !! 1)
#?endif
                  ;

                target.push(nqp::box_s(
                  nqp::substr($!str, $!pos, $nextpos - $!pos),
                  $!what
                ));
                $!pos = $nextpos;
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
    multi method lines(Str:D: Bool :$chomp! --> Seq:D) {
        Seq.new(($chomp ?? Lines !! LinesKeepNL).new(self))
    }

    method !ensure-split-sanity($v, $k, $kv, $p) {
        # cannot combine these
        my int $any = $v.Bool + $k.Bool + $kv.Bool + $p.Bool;
        X::Adverb.new(
          what   => 'split',
          source => 'Str',
          nogo   => (:$v, :$k, :$kv, :$p).grep(*.value).map(*.key),
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
          !! X::Syntax::Number::RadixOutOfRange.new(:$radix).Failure
    }

    # Shortcut for generating parsing Failure
    method !parse-fail($radix, $pos --> Failure) {
        X::Str::Numeric.new(
          :source(self),
          :$pos,
          :reason("malformed base-$radix number"),
        ).Failure
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
        my $base  := nqp::pow_I(nqp::box_i($radix,Int),nqp::atpos($fract,1),Num,Int);
        nqp::atpos($fract,2) == nqp::chars(self)   # fraction parsed entirely?
          ?? DIVIDE_NUMBERS(
               nqp::islt_I($whole,0)
                 ?? nqp::sub_I(
                      nqp::mul_I($whole,$base,Int),
                      nqp::atpos($fract,0),
                      Int
                    )
                 !! nqp::add_I(
                      nqp::mul_I($whole,$base,Int),
                      nqp::atpos($fract,0),
                      Int
                    ),
               $base,
               Rat,
               Rat
             )
          !! self!parse-fail($radix, nqp::atpos($fract,2) max $offset)
    }
    method !eggify($egg --> Int:D) { self.trans($egg => "01").parse-base(2) }
    multi method parse-base(Str:D: "camel" --> Int:D) { self!eggify: "🐪🐫" }
    multi method parse-base(Str:D: "beer"  --> Int:D) { self!eggify: "🍺🍻" }

#-------------------------------------------------------------------------------
# .split and associated logic

    multi method split(Str:D: Regex:D $regex, $limit = Whatever;;
      :$v , :$k, :$kv, :$p, :$skip-empty --> Seq:D) {

        Seq.new: self!ensure-split-sanity($v,$k,$kv,$p)
          ?? Rakudo::Iterator.MatchSplitMap(   # additional mapping needed
              $regex,
              self,
              $k                               # mapper
                ?? { 0 }                        # just dummy keys
                !! $v
                  ?? &POPULATE                  # full Match objects
                  !! $kv
                    ?? { (0, POPULATE($_)) }    # alternating key/Match
                    !! { 0 => POPULATE($_) },   # key => Match
              $limit,
              $skip-empty)
          !! $skip-empty                       # no additional mapping
            ?? Rakudo::Iterator.Truthy(        # skip empties
                 Rakudo::Iterator.MatchSplit($regex, self, $limit))
            !! Rakudo::Iterator.MatchSplit(    # produce all strings
                 $regex, self, $limit)
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

        # handle subclassed strings
        unless nqp::eqaddr(self.WHAT,Str) {
            my $subclassed := nqp::list;
            my $what       := self.WHAT;
            nqp::while(
              nqp::elems($matches),
              nqp::push(
                $subclassed,
                nqp::box_s(nqp::shift($matches),$what)
              )
            );
            $matches := $subclassed;
        }

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
                              while --$i;
                        nqp::splice($matches,$empty,0,1)
                          unless nqp::chars(nqp::atpos($matches,0));
                    }
                    else {
                        nqp::splice($matches,$match-list,$i,0)
                          while --$i;
                    }
                }
            }
            elsif $skip-empty {
                my int $i = nqp::elems($matches);
                my $match-list := nqp::list;
                while nqp::isge_i(--$i,0) {
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
        has Mu  $!what;
        has int $!chars;
        has str $!match;
        has int $!match-chars;
        has int $!todo;
        has int $!pos;
        method !SET-SELF($string, $match, $todo) {
            $!string      = $string;
            $!what       := $string.WHAT;
            $!chars       = nqp::chars($!string);
            $!match       = $match;
            $!match-chars = nqp::chars($!match);
            $!todo        = $todo - 1;
            self
        }
        method new($string, $match, $todo) {
            nqp::create(self)!SET-SELF($string, $match, $todo)
        }
        method !last-part() is raw {
            my str $string = nqp::substr($!string,$!pos);
            $!pos  = $!chars + 1;
            $!todo = 0;
            nqp::box_s($string,$!what)
        }
        method !next-part(int $found) is raw {
            my str $string =
              nqp::substr($!string,$!pos, $found - $!pos);
            $!pos = $found + $!match-chars;
            nqp::box_s($string,$!what);
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
        has Mu  $!what;
        has int $!todo;
        has int $!chars;
        has int $!pos;
        has int $!first;
        has int $!last;
        method !SET-SELF($string, $todo, $skip-empty) {
            $!string = $string;
            $!what  := $string.WHAT;
            $!chars  = nqp::chars($!string);
            $!todo   = $todo;
            $!first  = !$skip-empty;

            if $!todo > $!chars + 2 {  # will return all chars
                $!todo = $!chars + 1;
                $!last = !$skip-empty;
            }
            else {
                $!todo = $!todo - 1;
                $!last = !$skip-empty && ($!todo == $!chars + 1);
            }
            self
        }
        method new($string, $todo, $skip-empty) {
            nqp::create(self)!SET-SELF($string, $todo, $skip-empty)
        }
        method pull-one() is raw {
            if $!first {             # do empty string first
                $!first = 0;
                $!todo  = $!todo - 1;
                ""
            }
            elsif $!todo {           # next char
                $!todo = $!todo - 1;
                nqp::box_s(nqp::substr($!string,$!pos++,1),$!what)
            }
            elsif $!last {           # do final empty string
                $!last = 0;
                ""
            }
            elsif nqp::islt_i($!pos,$!chars) {  # do rest of string
                my str $rest = nqp::substr($!string,$!pos);
                $!pos = $!chars;
                nqp::box_s($rest,$!what)
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
                  nqp::box_s(nqp::substr($!string,$!pos++,1),$!what));
                $!todo = $!todo - 1;
            }
            target.push(nqp::box_s(nqp::substr($!string,$!pos),$!what))
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
              !! Seq.new: SplitStrLimit.new(self, $match, $limit)
        }

        # just separate chars
        else {
            Seq.new: SplitEmptyLimit.new(self, $limit, $skip-empty)
        }
    }

    multi method split(
      Str:D: @needles,
             $parts is copy = Inf;;
            :$v is copy,
            :$k,
            :$kv,
            :$p,
            :$skip-empty
    --> Seq:D) {
        my int $any = self!ensure-split-sanity($v,$k,$kv,$p);

        self!ensure-limit-sanity($parts);
        return Seq.new(Rakudo::Iterator.Empty) if $parts <= 0;

        my int $limit = $parts.Int
          unless nqp::istype($parts,Whatever) || $parts == Inf;
        my str $str = nqp::unbox_s(self);

        # A list with integer arrays containing for each needle found:
        # - the start position
        # - the number of chars
        # - the needle index
        my $pins := nqp::create(IterationBuffer);

        # A hash with the needles seen so far, to prevent potentially
        # expensive doublelookups
        my $needles-seen := nqp::hash;

        my int $tried;  # number of needles tried
        my int $fired;  # number of different needles with a match

        # search using all needles
        my int $index;
        for @needles -> $needle {
            my int $seen = nqp::elems($pins);

            if nqp::istype($needle,Regex) {
                my str $gist = $needle.gist;
                unless nqp::existskey($needles-seen,$gist) {
                    nqp::bindkey($needles-seen,$gist,1);

                    my $cursor;
                    my int $pos;
                    my int $c;

                    if $limit {
                        my int $todo = $limit;
                        nqp::while(
                          nqp::isge_i(--$todo,0)
                            && nqp::isgt_i(
                                 ($pos = (
                                   $cursor := $needle(
                                     $cursor-init(Match,$str,:$c)
                                   )
                                 ).pos),
                                 -1
                               ),
                          nqp::stmts(
                            nqp::push(
                              $pins,
                              nqp::list_i(
                                $cursor.from,
                                $pos - $cursor.from,
                                $index
                              )
                            ),
                            ($c = $pos)
                          )
                        );
                    }
                    else {
                        nqp::while(
                          nqp::isgt_i(
                            ($pos = (
                              $cursor := $needle($cursor-init(Match,$str,:$c))
                            ).pos),
                            -1
                          ),
                          nqp::stmts(
                            nqp::push(
                              $pins,
                              nqp::list_i(
                                $cursor.from,
                                $pos - $cursor.from,
                                $index
                              )
                            ),
                            ($c = $pos)
                          )
                        );
                    }
                }
            }
            else {
                my str $need = $needle.DEFINITE
                  ?? nqp::unbox_s($needle.Str)
                  !! "";
                my int $chars = nqp::chars($need);

                # search for this needle if there is one, and not done before
                if nqp::isgt_i($chars,0)
                  && nqp::not_i(nqp::existskey($needles-seen,$need)) {
                    nqp::bindkey($needles-seen,$need,1);
                    my int $pos;
                    my int $i;

                    if $limit {
                        my int $todo = $limit;
                        nqp::while(
                          nqp::isge_i(--$todo,0)
                            && nqp::isge_i($i = nqp::index($str,$need,$pos),0),
                          nqp::stmts(
                            nqp::push($pins,nqp::list_i($i,$chars,$index)),
                            ($pos = nqp::add_i($i,1)),
                          )
                        );
                    }
                    else {
                        nqp::while(
                          nqp::isge_i($i = nqp::index($str,$need,$pos),0),
                          nqp::stmts(
                            nqp::push($pins,nqp::list_i($i,$chars,$index)),
                            ($pos = nqp::add_i($i,1)),
                          )
                        );
                    }
                }
            }

            ++$tried;
            $fired = nqp::add_i(
              $fired,
              nqp::isge_i(nqp::elems($pins),$seen)
            );

            ++$index;  # next please!
        }

        # No needle tried, assume we want chars
        return self.split("",$limit) if nqp::not_i($tried);

        # Sort by position if more than one needle fired
        $pins := nqp::getattr(
          Rakudo::Sorting.MERGESORT-REIFIED-LIST-WITH-int(
            $pins.List,
            -> \a, \b {
                nqp::cmp_i(           # by start position
                  nqp::atpos_i(a,0),
                  nqp::atpos_i(b,0)
                ) || nqp::cmp_i(      # at same position, longest first
                  nqp::atpos_i(b,1),
                  nqp::atpos_i(a,1)
                )
            }
          ),
          List,
          '$!reified'
        ) if nqp::isgt_i($fired,1);

        # remove elements we do not want
        if nqp::isgt_i($limit,0) {
            my int $limited = 1;   # split one less than entries returned
            my int $elems = nqp::elems($pins);
            my int $pos;
            my int $i = -1;
            nqp::while(
              nqp::islt_i(++$i,$elems)
                && nqp::islt_i($limited,$limit),
              nqp::if(
                nqp::isge_i(   # not hidden by pos of needle $i
                  nqp::atpos_i(nqp::atpos($pins,$i),0),
                  $pos
                ),
                nqp::stmts(
                  ++$limited,
                  ($pos = nqp::add_i(  # move pos past needle $i
                    nqp::atpos_i(nqp::atpos($pins,$i),0),
                    nqp::atpos_i(nqp::atpos($pins,$i),1)
                  ))
                )
              )
            );

            # Clean out the rest
            nqp::splice(
              $pins,$empty,$i,nqp::sub_i(nqp::elems($pins),$i)
            ) if nqp::islt_i($i,$elems);
        }

        # create the final result
        my int $skip = ?$skip-empty;
        my int $pos;
        my $result := nqp::create(IterationBuffer);
        if $any {
            my int $i = -1;
            my int $elems = nqp::elems($pins);
            nqp::while(
              nqp::islt_i(++$i,$elems),
              nqp::if(
                nqp::isge_i( # not hidden by other needle
                  (my int $from = nqp::atpos_i(
                    (my $pin := nqp::atpos($pins,$i)),0)
                  ),
                  $pos
                ),
                nqp::stmts(
                  (my int $needle-index = nqp::atpos_i($pin,2)),
                  (my str $found = nqp::substr(
                    $str,nqp::atpos_i($pin,0),nqp::atpos_i($pin,1)
                  )),
                  nqp::unless(
                    $skip && nqp::iseq_i($from,$pos),
                    nqp::push(
                      $result,
                      nqp::box_s(
                        nqp::substr($str,$pos,nqp::sub_i($from,$pos)),
                        self
                      )
                    )
                  ),
                  nqp::if(
                    $k || $kv,
                    nqp::push($result,nqp::clone(nqp::atpos_i($pin,2)))
                  ),
                  nqp::if(
                    $v || $kv,
                    nqp::push($result,nqp::box_s($found,self))
                  ),
                  nqp::if(
                    $p,
                    nqp::push(
                      $result,
                      Pair.new(nqp::atpos_i($pin,2),nqp::box_s($found,self))
                    )
                  ),
                  ($pos = nqp::add_i($from,nqp::chars($found)))
                )
              )
            );
        }
        else {
            my int $i = -1;
            my int $elems = nqp::elems($pins);
            nqp::while(
              nqp::islt_i(++$i,$elems),
              nqp::if(
                nqp::isge_i( # not hidden by other needle
                  (my int $from = nqp::atpos_i(
                    (my $pin := nqp::atpos($pins,$i)),0)
                  ),
                  $pos
                ),
                nqp::stmts(
                  nqp::unless(
                    $skip && nqp::iseq_i($from,$pos),
                    nqp::push(
                      $result,
                      nqp::box_s(
                        nqp::substr($str,$pos,nqp::sub_i($from,$pos)),
                        self
                      )
                    ),
                  ),
                  ($pos = nqp::add_i($from,nqp::atpos_i($pin,1)))
                )
              )
            );
        }
        nqp::push(
          $result,
          nqp::box_s(nqp::substr($str,$pos),self)
        ) unless $skip && nqp::iseq_i($pos,nqp::chars($str));

        Seq.new(Rakudo::Iterator.ReifiedList($result))
    }

#-------------------------------------------------------------------------------

    # Note that in these same* methods, as used by s/LHS/RHS/, the
    # pattern is actually the original string matched by LHS, while the
    # invocant "original" is really the replacement RHS part.  Confusing...
    multi method samecase(Str:D: Str:D $pattern --> Str:D) {
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
              nqp::islt_i(++$i,$cases-chars),
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

            nqp::box_s(nqp::join("",$result),self)   # wrap it up
          ),

          self                                       # nothing to be done
        )
    }

#?if !jvm
    multi method samemark(Str:D: Str:D $pattern --> Str:D) {
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
              nqp::islt_i(++$i,$marks-elems),
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

            --$i,
            nqp::while(                               # remaining base chars
              nqp::islt_i(++$i,$base-elems),
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

            nqp::box_s(nqp::join("",$result),self)    # wrap it up
          ),

          self                                        # nothing to be done
        )
    }
#?endif
#?if jvm
    multi method samemark(Str:D: Str:D $pattern) { NYI('samemark').throw }
#?endif

    multi method samespace(Str:D: Str:D $pattern) { self!word-by-word($pattern, :samespace) }

    method !word-by-word(Str:D $pattern, &filter?, Bool :$samespace) {
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

        nqp::box_s(nqp::join("",$ret),self)
    }

    multi method trim(Str:D: --> Str:D) {
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
        nqp::box_s(nqp::substr(self,$left,$pos + 1 - $left),self)
    }

    multi method trim-leading(Str:D: --> Str:D) {
        nqp::box_s(nqp::substr(
          self,
          nqp::findnotcclass(
            nqp::const::CCLASS_WHITESPACE,self,0,nqp::chars(self)
          )
        ),self)
    }

    multi method trim-trailing(Str:D: --> Str:D) {
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
            nqp::box_s(nqp::substr(self,0,$pos + 1),self)
          ),
          self          # no whitespace, so done
        )
    }

    multi method words(Str:D: $limit --> Seq:D) {
        self.words.head($limit)
    }
    my class Words does PredictiveIterator {
        has str $!str;
        has Mu  $!what;
        has int $!chars;
        has int $!pos;

        method !SET-SELF($string) {
            $!str   = $string;
            $!what := $string.WHAT;
            $!chars = nqp::chars($!str);
            $!pos   = nqp::findnotcclass(
              nqp::const::CCLASS_WHITESPACE, $!str, 0, $!chars);
            self
        }
        method new($string) { nqp::create(self)!SET-SELF($string) }
        method pull-one() {
            nqp::if(
              (my int $left = $!chars - $!pos) > 0,
              nqp::stmts(
                (my int $nextpos = nqp::findcclass(
                  nqp::const::CCLASS_WHITESPACE, $!str, $!pos, $left)),
                (my $found := nqp::box_s(
                  nqp::substr($!str, $!pos, $nextpos - $!pos),
                  $!what
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
            my int $pos   = $!pos;
            my int $chars = $!chars;
            my str $str   = $!str;
            my Mu  $what := $!what;

            while ($left = $chars - $pos) > 0 {
                $nextpos = nqp::findcclass(
                  nqp::const::CCLASS_WHITESPACE, $str, $pos, $left);

                target.push(nqp::box_s(
                  nqp::substr($str, $pos, $nextpos - $pos),
                  $what
                ));
                $pos = nqp::findnotcclass(nqp::const::CCLASS_WHITESPACE,
                  $str, $nextpos, $chars - $nextpos);
            }
        }
        method count-only(--> Int:D) {
            my int $left;
            my int $nextpos;
            my int $seen;
            my int $pos   = $!pos;
            my int $chars = $!chars;
            my str $str   = $!str;

            while ($left = $chars - $pos) > 0 {
                $nextpos = nqp::findcclass(
                  nqp::const::CCLASS_WHITESPACE, $str, $pos, $left);
                $pos = nqp::findnotcclass(nqp::const::CCLASS_WHITESPACE,
                  $str, $nextpos, $chars - $nextpos);
                $seen = $seen + 1;
            }
            $seen
        }
        method bool-only(--> Bool:D) {
            nqp::hllbool(nqp::islt_i($!pos,$!chars))
        }
        method sink-all(--> IterationEnd) { }
    }
    multi method words(Str:D: --> Seq:D) { Seq.new(Words.new(self)) }

    # Internal method, used in RakuAST::QuotedString for word lists. Called
    # either at compile time for literal lists or runtime otherwise.
    my Mu $nbsp := nqp::hash(
        "\x00A0", True,
        "\x2007", True,
        "\x202F", True,
        "\xFEFF", True,
    );
    method WORDS_AUTODEREF(Str:D:) is implementation-detail {
        my $result := nqp::list();
        my int $pos = 0;
        my int $eos = nqp::chars(self);
        my int $ws;
        while ($pos = nqp::findnotcclass(nqp::const::CCLASS_WHITESPACE, self, $pos, $eos)) < $eos {
            # Search for another white space character as long as we hit non-breakable spaces.
            $ws = $pos;
            $ws++ while nqp::existskey($nbsp,
                nqp::substr(self, $ws = nqp::findcclass(nqp::const::CCLASS_WHITESPACE,
                    self, $ws, $eos), 1));
            nqp::push($result, nqp::substr(self, $pos, $ws - $pos));
            $pos = $ws;
        }
        nqp::hllize($result)
    }

    multi method encode(Str:D $encoding = 'utf8',
      :$replacement, Bool() :$translate-nl = False, :$strict --> Blob:D) {
        Encoding::Registry.find($encoding)
            .encoder(:$replacement, :$translate-nl, :$strict)
            .encode-chars(self)
    }

    my &SMART-WORDS = / [<:L> \w* ] +% <['\-]> /;

    multi method wordcase(Str:D: :&filter, Mu :$where = True --> Str:D) {
        my int $c;
        my int $pos;
        my int $from;
        my str $word;

        my $parts := nqp::list_s;

        nqp::until(
          nqp::islt_i(
            ($pos = nqp::getattr_i(
              (my $m := SMART-WORDS($cursor-init(Match,self,:$c))),
              Match,
              '$!pos'
            )),
            0
          ),
          nqp::stmts(
            nqp::if(
              nqp::isgt_i(($from = nqp::getattr_i($m,Match,'$!from')),$c),
              nqp::push_s($parts,nqp::substr($!value,$c,nqp::sub_i($from,$c)))
            ),
            ($word = nqp::substr($!value,$from,nqp::sub_i($pos,$from))),
            nqp::push_s(
              $parts,
              nqp::if(
                $where.ACCEPTS($word),
                nqp::if(&filter,filter($word),nqp::tclc($word)),
                $word
              )
            ),
            ($c = $pos)
          )
        );

        nqp::push_s(
          $parts,
          nqp::substr($!value,$c,nqp::sub_i(nqp::chars($!value),$c))
        ) if nqp::islt_i($c,nqp::chars($!value));

        nqp::box_s(nqp::join('',$parts),self)
    }

#-------------------------------------------------------------------------------
# .trans handling and associated subroutines

    # Expand a literal range of characters into an IterationBuffer
    # according to the (partially un)documented .trans semantics
    my sub expand-literal-range(str $range) {

        my Mu $result := nqp::create(IterationBuffer);
        my int $chars  = nqp::chars($range);
        my int $start  = 1;
        my int $found  = nqp::index($range,'..',$start);

        # Found and not at the end without trailing range spec
        nqp::while(
          $found != -1 && $found != $chars - 2,
          nqp::stmts(
            nqp::if(
              (my int $unsplit = $found - $start),
              nqp::splice(
                $result,
                nqp::split("",nqp::substr($range,$start - 1,$unsplit)),
                nqp::elems($result),
                0
              )
            ),

            # Add the range excluding last (may be begin point next range)
            (my int $from = nqp::ordat($range,$found - 1) - 1),
            (my int $to   = nqp::ordat($range,$found + 2)),
            nqp::while(
              ++$from < $to,
              nqp::push($result,nqp::chr($from))
            ),

            # Look for next range
            ($found = nqp::index($range,'..',$start = $found + 3))
          )
        );

        # Add final bits
        nqp::splice(
          $result,
          nqp::split("",nqp::substr($range,$start - 1)),
          nqp::elems($result),
          0
        ) if $start <= $chars;

        $result
    }

    # Shortcuts for throwing tantrums
    sub invalid-trans-arg($what) {
        X::Str::Trans::InvalidArg.new(got => $what.WHAT).throw
    }
    sub illegal-trans-key($what) {
        X::Str::Trans::IllegalKey.new(got => $what.WHAT).throw
    }

    # Simplified expansion logic
    my proto sub expand(|) {*}
    my multi sub expand(Iterable:D $iterable, $special is raw) {
        my $buffer := nqp::create(IterationBuffer);

        # Recursive splatter, also ensures we have strings
        sub splat($iterator --> Nil) {
            nqp::until(
              nqp::eqaddr((my $pulled := $iterator.pull-one),IterationEnd),
              nqp::if(
                nqp::istype($pulled,Iterable),
                splat($pulled.iterator),
                nqp::if(
                  nqp::istype($pulled,Callable),
                  nqp::stmts(
                    nqp::push($buffer,$pulled),
                    ($special = True)
                  ),
                  nqp::if(
                    nqp::istype($pulled,Cool),
                    nqp::push($buffer,$pulled.Str),
                    illegal-trans-key($pulled)
                  )
                )
              )
            );
        }

        splat($iterable.iterator);
        $buffer
    }
    my multi sub expand(Callable:D $what, $special is raw ) {
        my $buffer := nqp::create(IterationBuffer);
        nqp::push($buffer,$what);
        $special = True;
        $buffer
    }
    my multi sub expand(Cool:D $what, $) {
        expand-literal-range($what.Str)
    }
    my multi sub expand($what, $) {
        illegal-trans-key($what);
    }

    # Process all specifications, return whether slow path needed
    my sub needles-pins(@specs, $needles is raw, $pins is raw, $delete) {
        my $needs-split;
        for @specs {
            my $from  := expand(.key, my $regex);
            my $value := .value;
            my $to    := $regex && nqp::not_i(nqp::istype($value,Iterable))
              ?? nqp::list(nqp::istype($value,Callable)
                   ?? $value
                   !! nqp::istype($value,Cool)
                     ?? $value.Str
                     !! invalid-trans-arg($value)
                 )
              !! expand(.value, my $callable);

            # Make sure there's a "to" for every "from"
            if nqp::islt_i(nqp::elems($to),nqp::elems($from)) {
                my $padding := $delete || nqp::not_i(nqp::elems($to))
                  ?? ""
                  !! nqp::atpos($to,nqp::elems($to) - 1);
                my int $todo = nqp::elems($from) - nqp::elems($to);
                nqp::while(
                  $todo--,
                  nqp::push($to,$padding)
                );
            }

            # Add to jobs to do
            nqp::splice($needles,$from,nqp::elems($needles),0);
            nqp::splice($pins,$to,nqp::elems($pins),0);
            $needs-split = True if $regex || $callable;
        }

        # Clear we need slow path
        if $needs-split {
            True
        }

        # May be possible to use fast path, make sure
        else {
            my int $i = nqp::elems($needles);
            nqp::while(
              --$i >= 0
                && nqp::chars(nqp::atpos($needles,$i)) <= 1,
              nqp::null
            );
            $i >= 0
        }
    }

    # Slow path with multiple specs without any nameds
    my sub trans-no-nameds(Str:D $string, @specs, $delete?) {

        # Make sure we just got Pairs
        invalid-trans-arg(@specs.are) unless @specs.are(Pair);

        # Make sure we have a writable $/
        $/ := nqp::getlexcaller('$/');
        $/ := my $ unless nqp::iscont($/);

        # Set up needles and pins
        my $needles := nqp::create(IterationBuffer);
        my $pins    := nqp::create(IterationBuffer);

        # Must use the slower split :kv path
        if needles-pins(@specs, $needles, $pins, $delete) {

            my $parts := nqp::list_s;
            # :kv produces: before, index, match, after, index, match, after ...
            my $iterator := $string.split($needles.List, :kv).iterator;
            nqp::push_s($parts,$iterator.pull-one);                     # before
            nqp::until(
              nqp::eqaddr((my $i := $iterator.pull-one),IterationEnd),  # index
              nqp::stmts(
                ($/ = $iterator.pull-one),                              # match
                nqp::push_s(
                  $parts,
                  nqp::if(
                    nqp::istype((my $pin := nqp::atpos($pins,$i)),Callable),
                    $pin($/).Str,
                    $pin
                  )
                ),
                nqp::push_s($parts,$iterator.pull-one)                  # after
              )
            );
            nqp::box_s(nqp::join("",$parts),$string)
        }

        # Fast single char to char(s) trans
        else {

            # Create a lookup table
            my $lookup   := nqp::hash;
            my int $elems = nqp::elems($needles);
            my int $i     = -1;
            nqp::while(
              nqp::islt_i(++$i,$elems),
              nqp::bindkey($lookup,
                nqp::atpos($needles,$i),nqp::atpos($pins,$i)
              )
            );

            # Run over each char and perform substitution if needed
            my $result := nqp::split("",nqp::unbox_s($string));
            $i = -1;
            $elems = nqp::elems($result);
            nqp::while(
              nqp::islt_i(++$i,$elems),
              nqp::unless(
                nqp::isnull(
                  my $mapped := nqp::atkey($lookup,nqp::atpos($result,$i))
                ),
                nqp::bindpos($result,$i,nqp::decont($mapped))
              )
            );
            nqp::box_s(nqp::join("",$result),$string)
        }
    }

    my sub trans-with-nameds(Str:D $string, @specs, %_) {

        # Make sure we have access to the caller's lexical $/
        $/ := nqp::getlexcaller('$/');

        # Get the named arguments manually
        my int $complement = %_<c> // %_<complement> // 0;
        my int $squash     = %_<s> // %_<squash>     // 0;
        my     $delete    := %_<d> // %_<delete>;

        # Use simplified path if possible
        return trans-no-nameds($string, @specs, $delete)
          unless $complement || $squash;

        # Set up needles and pins
        my $needles     := nqp::create(IterationBuffer);
        my $pins        := nqp::create(IterationBuffer);
        my $needs-split := needles-pins(@specs, $needles, $pins, $delete);

        # Make sure we have a writable $/
        $/ := my $ unless nqp::iscont($/);

        # :kv produces: before, index, match, after, index, match, after ...
        my $iterator := $string.split(
          nqp::elems($needles) == 1
            ?? nqp::atpos($needles,0)
            !! $needles.List,
          :kv
        ).iterator;

        my $parts := nqp::list_s;
        if $complement {
            my str $before = $iterator.pull-one;                       # before
            my int $last-i;
            my str $replacement = nqp::null_s;
            nqp::until(
              nqp::eqaddr((my $i := $iterator.pull-one),IterationEnd), # index
              nqp::stmts(
                ($/ = $iterator.pull-one),                             # match
                ($replacement = nqp::if(
                  nqp::istype((my $pin := nqp::atpos($pins,$last-i)),Callable),
                  $pin($/).Str,
                  $pin
                )),
                nqp::if(
                  nqp::chars($before),
                  nqp::push_s(
                    $parts,
                    nqp::x($replacement,$squash || nqp::chars($before))
                  )
                ),
                nqp::push_s($parts,$/.Str),
                ($before = $iterator.pull-one),                        # after
                ($last-i = $i)
              )
            );

            nqp::if(
              nqp::chars($before),
              nqp::stmts(
                nqp::if(  # make sure there is a replacement for final part
                  nqp::isnull_s($replacement),
                  ($replacement = nqp::if(
                    nqp::istype(($pin := nqp::atpos($pins,0)),Callable),
                    $pin($/).Str,
                    $pin
                  ))
                ),
                nqp::push_s(
                  $parts,
                  nqp::x($replacement,$squash || nqp::chars($before))
                )
              )
            );
        }

        # Implied squash
        else {
            my str $lastpin = "";
            nqp::push_s($parts,$iterator.pull-one);                    # before

            nqp::until(
              nqp::eqaddr((my $i := $iterator.pull-one),IterationEnd), # index
              nqp::stmts(
                ($/ = $iterator.pull-one),                             # match
                nqp::if(
                  nqp::istype((my $pin := nqp::atpos($pins,$i)),Callable),
                  $pin($/).Str,
                  $pin
                ),
                nqp::if(
                  nqp::isne_s($pin,$lastpin),
                  nqp::stmts(
                    nqp::push_s($parts,$pin),
                    ($lastpin = $pin)
                  )
                ),
                nqp::if(
                  nqp::chars(nqp::push_s($parts,$iterator.pull-one)),  # after
                  ($lastpin = "")
                )
              )
            );
        }

        nqp::box_s(nqp::join("",$parts),$string)
    }

    multi method trans(Str:D: --> Str:D) {
        # Remove valid named args
        %_<c complement d delete s squash>:delete;

        # A noop only if there are no named args left
        warn "Unexpected named variable(s) '%_.pairs.raku.substr(1,*-5)' specified with .trans, did you mean to specify Pair(s)?".naive-word-wrapper
          if %_.Bool;

        self
    }

    multi method trans(Str:D: Pair:D $what --> Str:D) {

        # Type objects or empty string means no action to take
        return self
          unless (my $from := $what.key).defined
              && (my $to   := $what.value).defined
              && self.chars;

        # Callables need access to the lexically visible $/ of the caller
        $/ := nqp::getlexcaller('$/');

        # Slow path for any nameds
        if nqp::elems(nqp::getattr(%_,Map,'$!storage')) {
            self.trans(($what,), |%_)
        }

        # Common case: regex
        elsif nqp::istype($from,Regex) {

            # Fast path regex to string: can be simplified to split/join
            if nqp::istype($to,Str) {
                self.split($from).join($to)
            }
            elsif nqp::istype($to,Callable) {
                my $parts := nqp::list_s;
                # :kv produces: before, index, match, after, index, match, after ...
                my $iterator := self.split($from, :kv).iterator;
                nqp::push_s($parts,$iterator.pull-one);     # before
                nqp::until(
                  nqp::eqaddr(                              # index
                    (my $i := $iterator.pull-one),
                    IterationEnd
                  ),
                  nqp::stmts(
                    ($/ = $iterator.pull-one),              # match
                    nqp::push_s($parts,$to($/).Str),
                    nqp::push_s($parts,$iterator.pull-one)  # after
                  )
                );
                nqp::box_s(nqp::join("",$parts),self)
            }

            # Something weird at the destination (Iterable)
            else {
                nqp::istype($to,Cool)
                  ?? trans-no-nameds(self, ($what,))
                  !! invalid-trans-arg($what);
            }
        }

        # Nothing weird at the destination
        elsif nqp::istype($to,Str) {

            # Nothing weird at the source
            if nqp::istype($from,Str) {
                my int $from-chars = $from.chars;

                # Fast path single char to single other char
                if $from-chars == 1 {
                    nqp::box_s(
                      Rakudo::Internals.TRANSPOSE(self,$from,$to.substr(0,1)),
                      self
                    )
                }

                # Slower path with multiple source chars
                else {

                    my str $sfrom  = $from-chars < 4
                      ?? $from  # cannot have a range to expand
                      !! nqp::join("",expand-literal-range($from));
                    my str $str    = self;
                    my int $chars  = nqp::chars($str);
                    my Mu $result := nqp::list_s;
                    my str $check;
                    my int $i = -1;

                    # Something to convert to
                    if $to.chars -> $tochars {
                        nqp::setelems($result,$chars);

                        # All convert to one char
                        if $tochars == 1 {
                            nqp::while(
                              nqp::islt_i(++$i,$chars),
                              nqp::stmts(
                                ($check = nqp::substr($str,$i,1)),
                                nqp::bindpos_s(
                                  $result,
                                  $i,
                                  nqp::if(
                                    nqp::iseq_i(nqp::index($sfrom,$check),-1),
                                    $check,
                                    $to
                                  )
                                )
                              )
                            );
                        }

                        # Multiple chars to convert to
                        else {
                            my $bto := expand-literal-range($to);
                            my int $found;

                            # Repeat until mapping complete
                            nqp::while(
                              nqp::islt_i(nqp::elems($bto),nqp::chars($sfrom)),
                              nqp::splice($bto, $bto, nqp::elems($bto), 0)
                            );

                            nqp::while(
                              nqp::islt_i(++$i,$chars),
                              nqp::stmts(
                                ($check = nqp::substr($str,$i,1)),
                                ($found = nqp::index($sfrom,$check)),
                                nqp::bindpos_s(
                                  $result,
                                  $i,
                                  nqp::if(
                                    nqp::iseq_i($found,-1),
                                    $check,
                                    nqp::atpos($bto,$found)
                                  )
                                )
                              )
                            );
                        }
                    }

                    # Just remove
                    else {
                        nqp::while(
                          nqp::islt_i(++$i,$chars),
                          ($check = nqp::substr($str,$i,1)),
                          nqp::if(
                            nqp::iseq_i(nqp::index($sfrom,$check),-1),
                            nqp::push_s($result, $check)
                          )
                        );
                    }

                    nqp::box_s(nqp::join('',$result),self)
                }
            }

            # Something weird at the source (probably an Iterable)
            else {
                trans-no-nameds(self, ($what,))
            }
        }

        # Something weird at the destination (Iterable / Callable)
        else {
            nqp::istype($from,Cool)
              ?? nqp::istype($to,Cool) || nqp::istype($to,Callable)
                ?? trans-no-nameds(self, ($what,))
                !! invalid-trans-arg($to)
              !! illegal-trans-key($from)
        }
    }

    # The slurpy path is quite a bit more expensive, so handle it
    # separately so that the Positional candidate can be fast
    multi method trans(Str:D: *@specs --> Str:D) {

        # Make sure we just got Pairs
        invalid-trans-arg(@specs.are) unless @specs.are(Pair);

        # Callables need access to the lexically visible $/ of the caller
        $/ := nqp::getlexcaller('$/');

        self.chars
          ?? %_
            ?? trans-with-nameds(self, @specs, %_)
            !! trans-no-nameds(self, @specs)
          !! self
    }
    multi method trans(Str:D: @specs --> Str:D) {

        # Make sure we just got Pairs
        invalid-trans-arg(@specs.are) unless @specs.are(Pair);

        # Callables need access to the lexically visible $/ of the caller
        $/ := nqp::getlexcaller('$/');

        self.chars
          ?? %_
            ?? trans-with-nameds(self, @specs, %_)
            !! trans-no-nameds(self, @specs)
          !! self
    }

#-------------------------------------------------------------------------------
# .indent and associated subroutines

    # Zero indent does nothing
    multi method indent(Str:D: Int() $steps where { $_ == 0 }) {
        self;
    }

    # Positive indent does indent
    multi method indent(Int() $steps where { $_ > 0 }) {
        nqp::box_s(self.lines(:!chomp).map({
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
        }).join,self)
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
        my @lines = $obj.lines(:!chomp).map({
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
        nqp::box_s(@lines.map(-> $l {
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
        }).join,nqp::decont($obj))
    }

    method !SUBSTR-START-OOR($from) {
        X::OutOfRange.new(
          :what('Start argument to substr'),
          :got($from.gist),
          :range("0.." ~ nqp::chars(self)),
          :comment( nqp::istype($from, Callable) || -$from > nqp::chars(self)
            ?? ''
            !! "use *-{abs $from} if you want to index relative to the end"),
        ).Failure
    }
    method !SUBSTR-CHARS-OOR($chars) {
        X::OutOfRange.new(
          :what('Number of characters argument to substr'),
          :got($chars.gist),
          :range<0..^Inf>,
          :comment("use *-{abs $chars} if you want to index relative to the end"),
        ).Failure
    }

    multi method substr(Str:D: --> Str:D) { self }
    multi method substr(Str:D: Int:D $from --> Str:D) {
        nqp::islt_i($from,0) || nqp::isgt_i($from,nqp::chars(self))  #?js: NFG
          ?? self!SUBSTR-START-OOR($from)
          !! nqp::box_s(nqp::substr(self,$from),self)                #?js: NFG
    }
    multi method substr(Str:D: Int:D $from, Int:D $want --> Str:D) {
        nqp::islt_i($from,0) || nqp::isgt_i($from,nqp::chars(self))  #?js: NFG
          ?? self!SUBSTR-START-OOR($from)
          !! nqp::islt_i($want,0)
            ?? self!SUBSTR-CHARS-OOR($want)
            !! nqp::box_s(nqp::substr(self,$from,$want),self)        #?js: NFG
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
        nqp::islt_i((my int $from = (start.min + start.excludes-min).Int),0)
          || nqp::isgt_i($from,nqp::chars($!value))                    #?js: NFG
          ?? self!SUBSTR-START-OOR($from)
          !! nqp::box_s(
               (start.max == Inf
                 ?? nqp::substr($!value,$from)                         #?js: NFG
                 !! nqp::substr(
                      $!value,
                      $from,
                      (start.max - start.excludes-max - $from + 1).Int #?js: NFG
                    )
               ),
               self
             )
    }
    multi method substr(Str:D: Regex:D, $) {                           # GH 1314
        die "You cannot use a Regex on 'substr', did you mean 'subst'?"
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

    multi method substr-rw(Str:D $container is rw:) is rw {
        self!substr-proxify($container, 0, self.chars)
    }
    multi method substr-rw(Str:D $container is rw: Range:D $range) is rw {
        my $start := $range.min.Int + $range.excludes-min;
        self!substr-proxify(
          $container,
          $start,
          $range.max == Inf
            ?? self.chars - $start
            !! $range.max.Int - $range.excludes-max - $start + 1
        )
    }
    multi method substr-rw(Str:D $container is rw:
      Callable:D $code, $want?
    ) is rw {
        my int $chars = self.chars;
        my int $start = $code($chars);
        self!substr-proxify($container, $start, $want // $chars - $start)
    }
    multi method substr-rw(Str:D $container is rw: Int(Any) $start) is rw {
        self!substr-proxify($container, $start, self.chars - $start)
    }
    multi method substr-rw(Str:D $container is rw:
      Int(Any) $start, Whatever
    ) is rw {
        self!substr-proxify($container, $start, self.chars - $start)
    }
    multi method substr-rw(Str:D $container is rw:
      Int(Any) $start, Any:D $want
    ) is rw {
        self!substr-proxify($container, $start, $want)
    }

    method !substr-proxify(
      Str:D $container is rw, int $from, Any:D $want is copy
    ) is rw {
        my int $max   = self.chars;
        my int $chars = nqp::istype($want,Callable)
          ?? $want($max) - $from
          !! $want == Inf
            ?? $max - $from
            !! $want.Int;

        $from < 0 || $from > $max
          ?? self!SUBSTR-START-OOR($from)
          !! $chars < 0
            ?? self!SUBSTR-CHARS-OOR($chars)
            !! Proxy.new(
                 FETCH => sub ($) {            # need to access updated HLL Str
                     nqp::substr(nqp::unbox_s($container),$from,$chars)
                 },
                 STORE => sub ($, Str() $new) {
                     $container = nqp::box_s(  # need to make it a new HLL Str
                       nqp::concat(
                         nqp::substr($!value,0,$from),
                         nqp::concat(
                           nqp::unbox_s($new),
                           nqp::substr($!value,nqp::add_i($from,$chars))
                         )
                       ),
                       self.WHAT
                     )
                 }
               )
    }

    multi method codes(Str:D: --> Int:D) {
        nqp::codes(self)
    }
    multi method codes(Str:U: --> Int:D) {
        self.Str;  # generate undefined warning
        0
    }

    multi method chars(Str:D: --> Int:D) {
        nqp::p6box_i(nqp::chars($!value)) #?js: NFG
    }
    multi method chars(Str:U: --> Int:D) {
        self.Str;  # generate undefined warning
        0
    }

    multi method uc(Str:D: --> Str:D) { nqp::box_s(nqp::uc($!value),self) }
    multi method uc(Str:U: --> Str:D) { self.Str }

    multi method lc(Str:D: --> Str:D) { nqp::box_s(nqp::lc($!value),self) }
    multi method lc(Str:U: --> Str:D) { self.Str }

    multi method tc(Str:D: --> Str:D) {
        nqp::box_s(
          nqp::concat(                                 #?js: NFG
            nqp::tc(nqp::substr(self,0,1)),
            nqp::substr(self,1)
          ),
          self
        )
    }
    multi method tc(Str:U: --> Str:D) { self.Str }

    multi method fc(Str:D: --> Str:D) { nqp::box_s(nqp::fc($!value),self) }
    multi method fc(Str:U: --> Str:D) { self.Str }

    multi method tclc(Str:D: --> Str:D) { nqp::box_s(nqp::tclc($!value),self) }
    multi method tclc(Str:U: --> Str:D) { self.Str }

    multi method flip(Str:D: --> Str:D) { nqp::box_s(nqp::flip($!value),self) }
    multi method flip(Str:U: --> Str:D) { self.Str }

    method Date(Str:D:)     { Date.new(self)     }
    method DateTime(Str:D:) { DateTime.new(self) }

    # A naive word wrapper intended to be used for aligning error messages.
    # Naive in the sense that it assumes all graphemes are the same width,
    # and words that are too long for a line, will simply live on a line of
    # their own, even if that is longer than the given maximum width.
    method naive-word-wrapper(
      int :$max    = 72,
      str :$indent = "",
    --> Str:D) is implementation-detail {
        my $lines := nqp::list_s;
        my $line  := nqp::list_s;
        my int $width = nqp::chars($indent);

        for self.words -> str $word {
            my int $visible-width = $word.subst(/\e '[' \d+ m/, :global).chars;
            if $width + $visible-width >= $max {
                if nqp::elems($line) {
                    nqp::push_s(
                      $lines,
                      nqp::concat($indent,nqp::join(" ",$line))
                    );
                    $line := nqp::list_s($word);
                    $width = nqp::chars($indent) + $visible-width;
                }
                else {
                    nqp::push_s($lines,nqp::concat($indent,$word));
                    $width = nqp::chars($indent);
                }
            }
            else {
                nqp::push_s($line,$word);
                $width = $width + 1 + $visible-width;
            }
        }

        nqp::pop_s($line) if nqp::elems($line) && nqp::atpos_s($line,-1) eq "";
        nqp::push_s($lines,nqp::concat($indent,nqp::join(" ",$line)))
          if nqp::elems($line);

        nqp::join("\n",$lines)
    }

    method is-whitespace(Str:D: --> Bool:D) is implementation-detail {
        nqp::hllbool(
          nqp::iseq_i(
            nqp::findnotcclass(
              nqp::const::CCLASS_WHITESPACE,self,0,nqp::chars(self)
            ),
            nqp::chars(self)
          )
        )
    }

    method leading-whitespace(Str:D: --> Str:D) is implementation-detail {
        nqp::substr(self,0,nqp::findnotcclass(
          nqp::const::CCLASS_WHITESPACE,self,0,nqp::chars(self)
        ))
    }

    method trailing-whitespace(Str:D: --> Str:D) is implementation-detail {
        nqp::substr(self,nqp::chars(self) - nqp::findnotcclass(
          nqp::const::CCLASS_WHITESPACE,nqp::flip(self),0,nqp::chars(self)
        ))
    }

    method no-zero-arg(Str:D:
    ) is implementation-detail is hidden-from-backtrace {
        X::NoZeroArgMeaning.new(:name(self)).Failure
    }
}

multi sub prefix:<~>(Str:D $a --> Str:D) { $a.Str }
multi sub prefix:<~>(str   $a --> str)   { $a     }

multi sub infix:<~>(str $a, str $b   --> str) {
    nqp::concat($a, $b)
}
multi sub infix:<~>(Str:D $a, str $b --> str) {
    nqp::concat(nqp::unbox_s($a),$b)
}
multi sub infix:<~>(str $a, Str:D $b --> str) {
    nqp::concat($a,nqp::unbox_s($b))
}

multi sub infix:<~>(Str:D $a, Str:D $b --> Str:D) {
    nqp::box_s(
      nqp::concat(nqp::unbox_s($a),nqp::unbox_s($b)),
      Str
    )
}

multi sub infix:<~>(Cool:D $a, Str:D $b --> Str:D) {
    nqp::box_s(
      nqp::concat(nqp::unbox_s($a.Str),nqp::unbox_s($b)),
      Str
    )
}
multi sub infix:<~>(Str:D $a, Cool:D $b --> Str:D) {
    nqp::box_s(
      nqp::concat(nqp::unbox_s($a),nqp::unbox_s($b.Str)),
      Str
    )
}
multi sub infix:<~>(Cool:D $a, Cool:D $b --> Str:D) {
    nqp::box_s(
      nqp::concat(nqp::unbox_s($a.Str),nqp::unbox_s($b.Str)),
      Str
    )
}

multi sub infix:<~>(Any:D \a, Str:D $b --> Str:D) {
    nqp::box_s(
      nqp::concat(nqp::unbox_s(a.Stringy), nqp::unbox_s($b)),
      Str
    )
}
multi sub infix:<~>(Str:D $a, Any:D \b --> Str:D) {
    nqp::box_s(
      nqp::concat(nqp::unbox_s($a), nqp::unbox_s(b.Stringy)),
      Str
    )
}
# Any/Any candidate in src/core.c/Stringy.rakumod

multi sub infix:<~>(str @args --> str) { nqp::join('',@args) }
multi sub infix:<~>(@args)  { @args.join }
multi sub infix:<~>(*@args) { @args.join }

multi sub infix:<x>(Str:D $s, Bool:D $repetition --> Str:D) {
    $repetition ?? $s !! ''
}
multi sub infix:<x>(Str:D $s, Int:D $repetition --> Str:D) {
    nqp::islt_i($repetition,1) ?? '' !! nqp::x($s, $repetition)
}
multi sub infix:<x>(str $s, int $repetition --> str) {
    nqp::islt_i($repetition,1) ?? '' !! nqp::x($s, $repetition)
}

multi sub infix:<cmp>(Str:D $a, Str:D $b) {
    ORDER(nqp::cmp_s(nqp::unbox_s($a), nqp::unbox_s($b)))
}
multi sub infix:<cmp>(str $a, str $b) {
    ORDER(nqp::cmp_s($a, $b))
}

multi sub infix:<===>(Str:D $a, Str:D $b --> Bool:D) {
    nqp::hllbool(
      nqp::eqaddr($a.WHAT,$b.WHAT)
      && nqp::iseq_s(nqp::unbox_s($a),nqp::unbox_s($b)) #?js: NFG
    )
}
multi sub infix:<===>(str $a, str $b --> Bool:D) {
    nqp::hllbool(nqp::iseq_s($a, $b)) #?js: NFG
}

multi sub infix:<leg>(Str:D $a, Str:D $b) {
    ORDER(nqp::cmp_s(nqp::unbox_s($a),nqp::unbox_s($b)))
}
multi sub infix:<leg>(str $a, str $b) {
    ORDER(nqp::cmp_s($a, $b))
}

multi sub infix:<eq>(Str:D $a, Str:D $b --> Bool:D) {
    nqp::hllbool(nqp::iseq_s(nqp::unbox_s($a),nqp::unbox_s($b))) #?js: NFG
}
multi sub infix:<eq>(str $a, str $b --> Bool:D) {
    nqp::hllbool(nqp::iseq_s($a, $b)) #?js: NFG
}

multi sub infix:<ne>(Str:D $a, Str:D $b --> Bool:D) {
    nqp::hllbool(nqp::isne_s(nqp::unbox_s($a),nqp::unbox_s($b)))
}
multi sub infix:<ne>(str $a, str $b --> Bool:D) {
    nqp::hllbool(nqp::isne_s($a, $b))
}

multi sub infix:<lt>(Str:D $a, Str:D $b --> Bool:D) {
    nqp::hllbool(nqp::islt_s(nqp::unbox_s($a),nqp::unbox_s($b)))
}
multi sub infix:<lt>(str $a, str $b --> Bool:D) {
    nqp::hllbool(nqp::islt_s($a, $b))
}

multi sub infix:<le>(Str:D $a, Str:D $b --> Bool:D) {
    nqp::hllbool(nqp::isle_s(nqp::unbox_s($a),nqp::unbox_s($b)))
}
multi sub infix:<le>(str $a, str $b --> Bool:D) {
    nqp::hllbool(nqp::isle_s($a, $b))
}

multi sub infix:<gt>(Str:D $a, Str:D $b --> Bool:D) {
    nqp::hllbool(nqp::isgt_s(nqp::unbox_s($a),nqp::unbox_s($b)))
}
multi sub infix:<gt>(str $a, str $b --> Bool:D) {
    nqp::hllbool(nqp::isgt_s($a, $b))
}

multi sub infix:<ge>(Str:D $a, Str:D $b --> Bool:D) {
    nqp::hllbool(nqp::isge_s(nqp::unbox_s($a),nqp::unbox_s($b)))
}
multi sub infix:<ge>(str $a, str $b --> Bool:D) {
    nqp::hllbool(nqp::isge_s($a, $b))
}

multi sub infix:<~|>(Str:D $a, Str:D $b --> Str:D) {
    nqp::p6box_s(nqp::bitor_s(nqp::unbox_s($a),nqp::unbox_s($b)))
}
multi sub infix:<~|>(str $a, str $b --> str) { nqp::bitor_s($a, $b) }

multi sub infix:<~&>(Str:D $a, Str:D $b --> Str:D) {
    nqp::p6box_s(nqp::bitand_s(nqp::unbox_s($a),nqp::unbox_s($b)))
}
multi sub infix:<~&>(str $a, str $b --> str) { nqp::bitand_s($a, $b) }

multi sub infix:<~^>(Str:D $a, Str:D $b --> Str:D) {
    nqp::p6box_s(nqp::bitxor_s(nqp::unbox_s($a),nqp::unbox_s($b)))
}
multi sub infix:<~^>(str $a, str $b --> str) { nqp::bitxor_s($a, $b) }

multi sub prefix:<~^>(Str $) {
    NYI "prefix:<~^>"   # XXX
}

# XXX: String-wise shifts NYI
multi sub infix:«~>»(Str:D $, Int:D $) {
    NYI("infix:«~>»").throw;
}
multi sub infix:«~>»(str $, int $) {
    NYI("infix:«~>»").throw;
}
multi sub infix:«~<»(Str:D $, Int:D $ --> Str:D) {
    NYI("infix:«~<»").throw;
}
multi sub infix:«~<»(str $, int $) {
    NYI("infix:«~<»").throw;
}

proto sub trim($, *%) {*}
multi sub trim(Cool:D $s --> Str:D) { $s.trim }

proto sub trim-leading($, *%) {*}
multi sub trim-leading (Cool:D $s --> Str:D) { $s.trim-leading }

proto sub trim-trailing($, *%) {*}
multi sub trim-trailing(Cool:D $s --> Str:D) { $s.trim-trailing }

# the opposite of Real.base, used for :16($hex_str)
proto sub UNBASE ($, $, *%) is implementation-detail {*}
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
sub UNBASE_BRACKET($base, @a) is implementation-detail {
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

proto sub parse-base($, $, *%) {*}
multi sub parse-base(Str:D $str, Int:D $radix) { $str.parse-base($radix) }

proto sub substr($, $?, $?, *%) {*}
multi sub substr(str $s) { $s }
multi sub substr(str $s, int $f) { nqp::substr($s,$f) }
multi sub substr(str $s, int $f, int $c) { nqp::substr($s,$f,$c) }

multi sub substr(\what --> Str:D)                { what.substr             }
multi sub substr(\what, \from --> Str:D)         { what.substr(from)       }
multi sub substr(\what, \from, \chars --> Str:D) { what.substr(from,chars) }

proto sub substr-rw($, $?, $?, *%) is rw {*}
multi sub substr-rw(\what) is rw                { what.substr-rw             }
multi sub substr-rw(\what, \from) is rw         { what.substr-rw(from)       }
multi sub substr-rw(\what, \from, \chars) is rw { what.substr-rw(from,chars) }

multi sub infix:<eqv>(Str:D $a, Str:D $b --> Bool:D) {
    nqp::hllbool(
      nqp::unless(
        nqp::eqaddr($a,$b),
        nqp::eqaddr($a.WHAT,$b.WHAT) && nqp::iseq_s($a,$b)
      )
    )
}

proto sub samemark($, $, *%) {*}
multi sub samemark($s, $pat --> Str:D) { $s.samemark($pat) }

# vim: expandtab shiftwidth=4
