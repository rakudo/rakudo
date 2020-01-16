BEGIN {
    # Workaround for regression in https://github.com/rakudo/rakudo/issues/1566
    # The actual bug is that Callable role gets mixed in into routines
    # before it's composed, and when it is composed, the routines end up
    # not "doing" `Callable` role, even though they do. There are many more
    # routines suffering this issue, but these three regressed since last
    # release and we don't have the time to fix the primary bug before the
    # release, so in this fudge goes.
    &min.^compose;
    &max.^compose;
    &minmax.^compose;
}

my class Cool { # declared in BOOTSTRAP
    # class Cool is Any

    ## numeric methods

    method abs()  { self.Numeric.abs }
    method conj()  { self.Numeric.conj }
    method sqrt()  { self.Numeric.sqrt }
    method sign()  { self.Real.sign }
    method rand() { self.Num.rand }
    method sin()  { self.Numeric.sin }
    method asin() { self.Numeric.asin }
    method cos()  { self.Numeric.cos }
    method acos() { self.Numeric.acos }
    method tan()  { self.Numeric.tan }
    method atan() { self.Numeric.atan }
    method atan2($y = 1e0) { self.Numeric.atan2($y.Numeric) }
    method sec()  { self.Numeric.sec }
    method asec() { self.Numeric.asec }
    method cosec()  { self.Numeric.cosec }
    method acosec() { self.Numeric.acosec }
    method cotan()  { self.Numeric.cotan }
    method acotan() { self.Numeric.acotan }
    method sinh()  { self.Numeric.sinh }
    method asinh() { self.Numeric.asinh }
    method cosh()  { self.Numeric.cosh }
    method acosh() { self.Numeric.acosh }
    method tanh()  { self.Numeric.tanh }
    method atanh() { self.Numeric.atanh }
    method sech()  { self.Numeric.sech }
    method asech() { self.Numeric.asech }
    method cosech()  { self.Numeric.cosech }
    method acosech() { self.Numeric.acosech }
    method cotanh()  { self.Numeric.cotanh }
    method acotanh() { self.Numeric.acotanh }
    method cis()     { self.Numeric.cis }
    method is-prime(--> Bool:D) { self.Real.is-prime }

    proto method log(|) {*}
    multi method log(Cool:D: )      { self.Numeric.log          }
    multi method log(Cool:D: $base) { self.Numeric.log($base.Numeric) }

    proto method exp(|) {*}
    multi method exp(Cool:D: )      { self.Numeric.exp          }
    multi method exp(Cool:D: $base) { self.Numeric.exp($base.Numeric) }

    proto method round(|) {*}
    multi method round()      { self.Numeric.round()      }
    multi method round($base) { self.Numeric.round($base) }

    method roots(Cool $n)   { self.Numeric.roots($n)    }
    method log2()           { self.Numeric.log2         }
    method log10()          { self.Numeric.log10        }
    method unpolar($n)      { self.Numeric.unpolar($n.Numeric) }

    method floor()          { self.Numeric.floor        }
    method ceiling()        { self.Numeric.ceiling      }
    method truncate()       { self.Numeric.truncate     }

    ## string methods

    method chars(--> Int:D) {
        self.Str.chars
    }
    method codes() {
        self.Str.codes
    }

    method fmt($format = '%s') {
        Rakudo::Internals.initialize-sprintf-handler;
        nqp::p6box_s(
            nqp::sprintf(nqp::unbox_s($format.Stringy), nqp::list(self))
        )
    }

    method uc() {
        self.Str.uc
    }

    method lc() {
        self.Str.lc
    }

    method tc() {
        self.Str.tc
    }

    method fc() {
        self.Str.fc
    }

    method tclc() {
        self.Str.tclc
    }

    method wordcase()   { self.Str.wordcase }

    method uniname()        { uniname(self) }
    method uninames()       { uninames(self) }
    method unival(Cool:D:)  { self.Int.unival }
    method univals(Cool:D:) { self.Str.univals }
    method uniprop(|c)      { uniprop(self, |c) }
    method uniprop-int(|c)  { uniprop-int(self, |c) }
    method uniprop-bool(|c) { uniprop-bool(self, |c) }
    method uniprop-str(|c)  { uniprop-str(self, |c) }
    method uniprops(|c)     { uniprops(self, |c) }
    method unimatch(|c)     { unimatch(self, |c) }

    method chomp(Cool:D:) { self.Str.chomp }

    proto method chop(|)                {*}
    multi method chop(Cool:D:)          { self.Str.chop }
    multi method chop(Cool:D: Int() $n) { self.Str.chop($n) }

    method ord(--> Int:D) {
        self.Str.ord
    }
    method chr() {
        self.Int.chr;
    }

    proto method chrs(|) {*}
    multi method chrs(Cool:D:) { self.list.chrs }

    proto method ords(|) {*}
    multi method ords(Cool:D:) { self.Str.ords }

    method flip() {
        self.Str.flip
    }
    method trans(|c) { self.Str.trans(|c) }

    proto method starts-with(|) {*}
    multi method starts-with(Cool:D: Cool:D $needle --> Bool:D) {
        nqp::hllbool(nqp::eqat(self.Str, $needle.Str, 0))
    }   
    multi method starts-with(Cool:D: Str:D $needle --> Bool:D) {
        nqp::hllbool(nqp::eqat(self.Str, $needle, 0)) 
    }   

    proto method ends-with(|) {*}
    multi method ends-with(Cool:D: Cool:D $suffix --> Bool:D) {
        self.Str.ends-with: $suffix.Str
    }
    multi method ends-with(Cool:D: Str:D $suffix --> Bool:D) {
        self.Str.ends-with: $suffix
    }

    proto method substr(|) {*}
    multi method substr(\from)         { self.Str.substr(from)       }
    multi method substr(\from, \chars) { self.Str.substr(from,chars) }

    proto method substr-rw(|) {*}
    multi method substr-rw(\SELF:) is rw {
        (SELF = self.Str).substr-rw
    }
    multi method substr-rw(\SELF: \from) is rw {
        (SELF = self.Str).substr-rw(from)
    }
    multi method substr-rw(\SELF: \from, \chars) is rw {
        (SELF = self.Str).substr-rw(from,chars)
    }

    proto method substr-eq(|) {*}
    multi method substr-eq(Cool:D: Cool:D $needle --> Bool:D) {
        nqp::hllbool(nqp::eqat(self.Str,$needle.Str,0))
    }   
    multi method substr-eq(Cool:D: Str:D $needle --> Bool:D) {
        nqp::hllbool(nqp::eqat(self.Str,$needle,0))
    }   
    multi method substr-eq(Cool:D: Cool:D $needle, Int:D $pos --> Bool:D) {
        self.Str.substr-eq($needle.Str, $pos)
    }   
    multi method substr-eq(Cool:D: Str:D $needle, Int:D $pos --> Bool:D) {
        self.Str.substr-eq($needle, $pos)
    }   

    proto method contains(|) {*}
    multi method contains(Cool:D: Cool:D $needle --> Bool:D) {
        nqp::hllbool(nqp::isne_i(nqp::index(self.Str,$needle.Str,0),-1))
    }   
    multi method contains(Cool:D: Str:D $needle --> Bool:D) {
        nqp::hllbool(nqp::isne_i(nqp::index(self.Str,$needle,0),-1))
    }   
    multi method contains(Cool:D: Cool:D $needle, Int:D $pos --> Bool:D) {
        self.Str.contains($needle.Str, $pos)
    }   
    multi method contains(Cool:D: Str:D $needle, Int:D $pos --> Bool:D) {
        self.Str.contains($needle, $pos)
    }   
    multi method contains(Cool:D: Cool:D $needle, Cool:D $pos --> Bool:D) {
        self.Str.contains($needle.Str, $pos.Int)
    }
    multi method contains(Cool:D: Str:D $needle, Cool:D $pos --> Bool:D) {
        self.Str.contains($needle, $pos.Int)
    }

    proto method indices(|) {*}
    multi method indices(Cool:D: Cool:D $needle, :$overlap) {
        self.Str.indices: $needle.Str, :$overlap
    }   
    multi method indices(Cool:D: Str:D $needle, :$overlap) {
        self.Str.indices: $needle, :$overlap
    }   
    multi method indices(Cool:D: Cool:D $needle, Cool:D $start, :$overlap) {
        self.Str.indices: $needle.Str, $start.Int, :$overlap
    }   
    multi method indices(Cool:D: Str:D $needle, Int:D $start, :$overlap) {
        self.Str.indices: $needle, $start, :$overlap
    }

    proto method index(|) {*}
    multi method index(Cool:D: Cool:D $needle --> Int:D) {
        nqp::if(
          nqp::islt_i((my int $i = nqp::index(self.Str,$needle.Str)),0),
          Nil,
          nqp::p6box_i($i)
        )
    }
    multi method index(Cool:D: Str:D $needle --> Int:D) {
        nqp::if(
          nqp::islt_i((my int $i = nqp::index(self.Str,$needle)),0),
          Nil,
          nqp::p6box_i($i)
        )
    }
    multi method index(Cool:D: Cool:D $needle, Cool:D $pos --> Int:D) {
        self.Str.index: $needle.Str, $pos.Int
    }   
    multi method index(Cool:D: Cool:D $needle, Int:D $pos --> Int:D) {
        self.Str.index: $needle.Str, $pos
    }   
    multi method index(Cool:D: Str:D $needle, Int:D $pos --> Int:D) {
        self.Str.index: $needle, $pos
    }   

    proto method rindex(|) {*}
    multi method rindex(Cool:D: Cool:D $needle --> Int:D) {
        nqp::if(
          nqp::islt_i((my int $i = nqp::rindex(self.Str,$needle.Str)),0),
          Nil,
          nqp::p6box_i($i)
        )
    }
    multi method rindex(Cool:D: Str:D $needle --> Int:D) {
        nqp::if(
          nqp::islt_i((my int $i = nqp::rindex(self.Str,$needle)),0),
          Nil,
          nqp::p6box_i($i)
        )
    }
    multi method rindex(Cool:D: Cool:D $needle, Cool:D $pos --> Int:D) {
        self.rindex: $needle.Str, $pos.Int
    }
    multi method rindex(Str:D: Cool:D $needle, Int:D $pos --> Int:D) {
        self.Str.rindex: $needle.Str, $pos
    }
    multi method rindex(Cool:D: Str:D $needle, Int:D $pos --> Int:D) {
        self.Str.rindex: $needle, $pos
    }

    method split(Cool: |c) {
        self.Stringy.split(|c);
    }

    method match(Cool:D: |c) {
        $/ := nqp::getlexcaller('$/');
        self.Stringy.match(|c)
    }

    proto method comb(|) {*}
    multi method comb(Cool:D: --> Seq:D) {
        self.Str.comb
    }
    multi method comb(Cool:D: Cool:D $size, $limit = * --> Seq:D) {
        self.Str.comb($size.Int, $limit)
    }
    multi method comb(Cool:D: Int:D $size, $limit = * --> Seq:D) {
        self.Str.comb($size, $limit)
    }
    multi method comb(Cool:D: Cool:D $pat --> Seq:D) {
        self.Str.comb($pat.Str)
    }
    multi method comb(Cool:D: Str:D $pat --> Seq:D) {
        self.Str.comb($pat)
    }
    multi method comb(Cool:D: Cool:D $pat, $limit --> Seq:D) {
        self.Str.comb($pat.Str, $limit)
    }
    multi method comb(Cool:D: Str:D $pat, $limit --> Seq:D) {
        self.Str.comb($pat, $limit)
    }
    multi method comb(Cool:D: Regex:D $pattern, :$match --> Seq:D) {
        self.Str.comb($pattern, :$match)
    }
    multi method comb(Cool:D: Regex:D $pattern, $limit, :$match --> Seq:D) {
        self.Str.comb($pattern, $limit, :$match)
    }

    proto method lines(|) {*}
    multi method lines(Cool:D:)           { self.Str.lines          }
    multi method lines(Cool:D: :$count! ) { self.Str.lines(:$count) }
    multi method lines(Cool:D: $limit )   { self.Str.lines($limit)  }

    proto method words(|) {*}
    multi method words(Cool:D:)         { self.Str.words         }
    multi method words(Cool:D: $limit ) { self.Str.words($limit) }

    method subst(|c) {
        $/ := nqp::getlexcaller('$/');
        self.Stringy.subst(|c);
    }

    # `$value-to-subst-mutate` will show up in errors when called on non-rw
    # container, so use more descriptive name instead of just `$self`
    method subst-mutate(Cool:D $value-to-subst-mutate is rw: |c) {
        $/ := nqp::getlexcaller('$/');
        my $str   = $value-to-subst-mutate.Str;
        my $match := $str.subst-mutate(|c);
        $value-to-subst-mutate = $str if $match;  # only change if successful
        $match
    }

    proto method IO(|) {*}
    multi method IO(Cool:D:) { IO::Path.new(self) }
    multi method IO(Cool:U:) { IO::Path }

    method sprintf(*@args) { sprintf(self, @args) };
    method printf (*@args) {  printf(self, @args) };
    method samecase(Cool:D: Cool $pattern) { self.Stringy.samecase($pattern) }

    method path() { self.Stringy.IO }
    method trim         () { self.Stringy.trim          };
    method trim-leading () { self.Stringy.trim-leading  };
    method trim-trailing() { self.Stringy.trim-trailing };

    method EVAL(*%opts) {
        EVAL(self, context => CALLER::, |%opts);
    }

    multi method Real() {
        nqp::if(
            nqp::istype((my $numeric := self.Numeric), Failure),
            $numeric,
            $numeric.Real
        )
    }

    proto method Int(|) {*}
    multi method Int()  {
        nqp::if(
            nqp::istype((my $numeric := self.Numeric), Failure),
            $numeric,
            $numeric.Int
        )
    }

    proto method UInt(|) {*}
    multi method UInt()  {
        nqp::istype((my $got := self.Int),Failure)
          ?? $got
          !! $got < 0
            ?? Failure.new(X::OutOfRange.new(
                 :what('Coercion to UInt'),
                 :$got,
                 :range<0..^Inf>
               ))
            !! $got
    }

    method Num()  {
        nqp::if(
            nqp::istype((my $numeric := self.Numeric), Failure),
            $numeric,
            $numeric.Num
        )
    }

    method Rat()  {
        nqp::if(
            nqp::istype((my $numeric := self.Numeric), Failure),
            $numeric,
            $numeric.Rat
        )
    }

    method FatRat()  {
        nqp::if(
            nqp::istype((my $numeric := self.Numeric), Failure),
            $numeric,
            $numeric.FatRat
        )
    }

    method Complex()  {
        nqp::if(
            nqp::istype((my $numeric := self.Numeric), Failure),
            $numeric,
            $numeric.Complex
        )
    }
}
Metamodel::ClassHOW.exclude_parent(Cool);

proto sub chop($, $?, *%) {*}
multi sub chop(Cool:D $s --> Str:D) { $s.chop }
multi sub chop(Cool:D $s, Int() $n --> Str:D) { $s.chop($n) }

proto sub chomp($, *%) {*}
multi sub chomp(Cool $s --> Str:D) { $s.chomp }

proto sub flip($, *%) {*}
multi sub flip(Cool $s --> Str:D) { $s.flip }

proto sub index($, $, $?, *%) {*}
multi sub index(Cool $s, Cool $needle)            { $s.index($needle)      }
multi sub index(Cool $s, Cool $needle, Cool $pos) { $s.index($needle,$pos) }

proto sub rindex($, $, $?, *%) {*}
multi sub rindex(Cool $s, Cool $needle, Cool $pos) { $s.rindex($needle, $pos) }
multi sub rindex(Cool $s, Cool $needle)            { $s.rindex($needle) }

proto sub lc($, *%) {*}
multi sub lc(Cool $s) { $s.lc }

proto sub ord($, *%) {*}
multi sub ord(Cool $s) { $s.ord }

proto sub uc($, *%) {*}
multi sub uc(Cool $s) { $s.uc }

proto sub tc($, *%) {*}
multi sub tc(Cool $s) { $s.tc }

proto sub fc($, *%) {*}
multi sub fc(Cool $s) { $s.fc }

proto sub tclc($, *%) {*}
multi sub tclc(Cool $s) { $s.tclc }

proto sub indices($, |) {*}
multi sub indices(Cool $s, |c) { $s.indices(|c) }

proto sub ords($, *%) {*}
multi sub ords(Cool:D $s) { $s.ords }

proto sub comb($, $, $?, *%) {*}
multi sub comb(Regex $matcher, Cool $input, $limit = *, :$match) {
    $input.comb($matcher, $limit, :$match)
}
multi sub comb(Str $matcher, Cool $input, $limit = *) {
    $input.comb($matcher, $limit)
}
multi sub comb(Int:D $size, Cool $input, $limit = *) {
    $input.comb($size, $limit)
}

proto sub wordcase($, *%) is pure {*}
multi sub wordcase(Str:D $x) {$x.wordcase }
multi sub wordcase(Cool $x)  {$x.Str.wordcase }

proto sub sprintf($, |) {*}
multi sub sprintf(Cool:D $format, *@args) {
    CATCH {
        when X::Cannot::Lazy {
            X::Cannot::Lazy.new(:action('(s)printf')).throw
        }
        default {
            Rakudo::Internals.HANDLE-NQP-SPRINTF-ERRORS($_).throw
        }
    }
    Rakudo::Internals.initialize-sprintf-handler;
    nqp::p6box_s(
      nqp::sprintf(nqp::unbox_s($format.Stringy),
        nqp::if(
          @args.elems,
          nqp::clone(nqp::getattr(@args,List,'$!reified')),
          nqp::create(IterationBuffer)
        )
      )
    )
}

proto sub printf($, |) {*}
multi sub printf(Cool:D $format, *@args) { print sprintf $format, @args }

proto sub samecase($, $, *%) {*}
multi sub samecase(Cool:D $string, Cool:D $pattern) { $string.samecase($pattern) }

proto sub split($, $, |) {*}
multi sub split($pat, Cool:D $target, |c) { c ?? $target.split($pat, |c) !! $target.split($pat) }

proto sub chars($, *%) is pure {*}
multi sub chars(Cool $x)  { $x.Str.chars }

multi sub chars(Str:D $x) { nqp::p6box_i(nqp::chars($x)) } #?js: NFG
multi sub chars(str $x --> int) { nqp::chars($x) } #?js: NFG

# These probably belong in a separate unicodey file

proto sub uniname($, *%) {*}
multi sub uniname(Str:D $str)  { $str ?? uniname($str.ord) !! Nil }
multi sub uniname(Int:D $code) { nqp::getuniname($code) }

proto sub uninames($, *%) {*}
multi sub uninames(Str:D $str) { $str.NFC.map: { uniname($_) } }

#?if jvm
multi sub unival(|)       { die 'unival NYI on jvm backend' }
multi sub univals(|)      { die 'univals NYI on jvm backend' }
multi sub uniprop(|)      { die 'uniprop NYI on jvm backend' }
multi sub uniprop-int(|)  { die 'uniprop-int NYI on jvm backend' }
multi sub uniprop-bool(|) { die 'uniprop-bool NYI on jvm backend' }
multi sub uniprop-str(|)  { die 'uniprop-str NYI on jvm backend' }
multi sub uniprops(|)     { die 'uniprops NYI on jvm backend' }
multi sub unimatch(|)     { die 'unimatch NYI on jvm backend' }
#?endif

#?if js
multi sub uniprop(|)      { die 'uniprop NYI on js backend' }
multi sub uniprop-int(|)  { die 'uniprop-int NYI on js backend' }
multi sub uniprop-bool(|) { die 'uniprop-bool NYI on js backend' }
multi sub uniprop-str(Int:D $code, Stringy:D $propname) {
    nqp::getuniprop_str($code,nqp::unipropcode($propname));
}
multi sub uniprops(|)     { die 'uniprops NYI on jvm backend' }
multi sub unimatch(|)     { die 'unimatch NYI on js backend' }
#?endif

#?if !jvm
proto sub unival($, *%) {*}
multi sub unival(Str:D $str) { $str ?? $str.ord.unival !! Nil }
multi sub unival(Int:D $code) { $code.unival }

proto sub univals($, *%) {*}
multi sub univals(Str:D $str) { $str.univals }
#?endif

#?if moar
proto sub uniprop($, |) {*}
multi sub uniprop(Str:D $str, |c) { $str ?? uniprop($str.ord, |c) !! Nil }
multi sub uniprop(Int:D $code) {
    nqp::getuniprop_str($code,nqp::unipropcode('General_Category'));
}
multi sub uniprop(Int:D $code, Stringy:D $propname) {
    ## The code below was generated by tools/build/makeUNIPROP.p6
    my constant $prefs = nqp::hash(
      'AHex','B','ASCII_Hex_Digit','B','Age','S','Alpha','B','Alphabetic','B',
      'Bidi_C','B','Bidi_Class','S','Bidi_Control','B','Bidi_M','B',
      'Bidi_Mirrored','B','Bidi_Mirroring_Glyph','bmg',
      'Bidi_Paired_Bracket_Type','S','Block','S','CE','B','CI','B','CWCF','B',
      'CWCM','B','CWKCF','B','CWL','B','CWT','B','CWU','B',
      'Canonical_Combining_Class','S','Case_Folding','S','Case_Ignorable','B',
      'Cased','B','Changes_When_Casefolded','B','Changes_When_Casemapped','B',
      'Changes_When_Lowercased','B','Changes_When_NFKC_Casefolded','B',
      'Changes_When_Titlecased','B','Changes_When_Uppercased','B','Comp_Ex','B',
      'Composition_Exclusion','B','DI','B','Dash','B',
      'Decomposition_Mapping','S','Decomposition_Type','S',
      'Default_Ignorable_Code_Point','B','Dep','B','Deprecated','B','Dia','B',
      'Diacritic','B','East_Asian_Width','S','Emoji','B','Emoji_Modifier','B',
      'Emoji_Modifier_Base','B','Emoji_Presentation','B','Expands_On_NFC','B',
      'Expands_On_NFD','B','Expands_On_NFKC','B','Expands_On_NFKD','B',
      'Ext','B','Extender','B','FC_NFKC','S','FC_NFKC_Closure','S',
      'Full_Composition_Exclusion','B','GCB','S','General_Category','S',
      'Gr_Base','B','Gr_Ext','B','Gr_Link','B','Grapheme_Base','B',
      'Grapheme_Cluster_Break','S','Grapheme_Extend','B','Grapheme_Link','B',
      'Hangul_Syllable_Type','S','Hex','B','Hex_Digit','B','Hyphen','B',
      'IDC','B','IDS','B','IDSB','B','IDST','B','IDS_Binary_Operator','B',
      'IDS_Trinary_Operator','B','ID_Continue','B','ID_Start','B',
      'ISO_Comment','S','Ideo','B','Ideographic','B','InPC','S','InSC','S',
      'Indic_Positional_Category','S','Indic_Syllabic_Category','S',
      'Join_C','B','Join_Control','B','Joining_Group','S','Joining_Type','S',
      'LOE','B','Line_Break','S','Logical_Order_Exception','B','Lower','B',
      'Lowercase','B','Lowercase_Mapping','lc','Math','B','NChar','B',
      'NFC_QC','S','NFC_Quick_Check','S','NFD_QC','S','NFD_Quick_Check','S',
      'NFKC_CF','S','NFKC_Casefold','S','NFKC_QC','S','NFKC_Quick_Check','S',
      'NFKD_QC','S','NFKD_Quick_Check','S','Name','na',
      'Noncharacter_Code_Point','B','Numeric_Type','S','Numeric_Value','nv',
      'OAlpha','B','ODI','B','OGr_Ext','B','OIDC','B','OIDS','B','OLower','B',
      'OMath','B','OUpper','B','Other_Alphabetic','B',
      'Other_Default_Ignorable_Code_Point','B','Other_Grapheme_Extend','B',
      'Other_ID_Continue','B','Other_ID_Start','B','Other_Lowercase','B',
      'Other_Math','B','Other_Uppercase','B','PCM','B','Pat_Syn','B',
      'Pat_WS','B','Pattern_Syntax','B','Pattern_White_Space','B',
      'Prepended_Concatenation_Mark','B','QMark','B','Quotation_Mark','B',
      'RI','B','Radical','B','Regional_Indicator','B','SB','S','SD','B',
      'STerm','B','Script','S','Sentence_Break','S','Sentence_Terminal','B',
      'Simple_Case_Folding','S','Simple_Lowercase_Mapping','S',
      'Simple_Titlecase_Mapping','S','Simple_Uppercase_Mapping','S',
      'Soft_Dotted','B','Term','B','Terminal_Punctuation','B',
      'Titlecase_Mapping','tc','UIdeo','B','Unified_Ideograph','B','Upper','B',
      'Uppercase','B','Uppercase_Mapping','uc','VS','B',
      'Variation_Selector','B','Vertical_Orientation','S','WB','S','WSpace','B',
      'White_Space','B','Word_Break','S','XIDC','B','XIDS','B',
      'XID_Continue','B','XID_Start','B','XO_NFC','B','XO_NFD','B',
      'XO_NFKC','B','XO_NFKD','B','age','S','bc','S','blk','S','bmg','bmg',
      'bpt','S','ccc','S','cf','S','cjkCompatibilityVariant','S','dm','S',
      'dt','S','ea','S','gc','S','hst','S','isc','S','jg','S','jt','S',
      'kCompatibilityVariant','S','lb','S','lc','lc','na','na','nt','S',
      'nv','nv','sc','S','scf','S','sfc','S','slc','S','space','B','stc','S',
      'suc','S','tc','tc','uc','uc','vo','S',
    );
    ## End generated code
    my int $prop = nqp::unipropcode($propname);
    my str $pref = nqp::ifnull(nqp::atkey($prefs, $propname),'');
    nqp::if(
      nqp::iseq_s($pref, 'S'),
      nqp::getuniprop_str($code,$prop),
      nqp::if(
        nqp::iseq_s($pref, 'I'),
        nqp::getuniprop_int($code,$prop),
        nqp::if(
          nqp::iseq_s($pref, 'B'),
          nqp::hllbool(nqp::getuniprop_bool($code,$prop)),
          nqp::if(
            nqp::iseq_s($pref, 'lc'),
            nqp::lc(nqp::chr(nqp::unbox_i($code))),
            nqp::if(
              nqp::iseq_s($pref, 'tc'),
              nqp::tc(nqp::chr(nqp::unbox_i($code))),
              nqp::if(
                nqp::iseq_s($pref, 'uc'),
                nqp::uc(nqp::chr(nqp::unbox_i($code))),
                nqp::if(
                  nqp::iseq_s($pref, 'na'),
                  nqp::getuniname($code),
                  nqp::if(
                    nqp::iseq_s($pref, 'nv'),
                    $code.unival,
                    nqp::if(
                      nqp::iseq_s($pref, 'bmg'),
                      nqp::stmts(
                        (my int $bmg-ord = nqp::getuniprop_int($code, $prop)),
                        $bmg-ord ?? nqp::chr($bmg-ord) !! ''),
                      nqp::stmts(
                        (my $result := nqp::getuniprop_str($code,$prop)),
                        nqp::if(
                          nqp::istrue($result),
                          nqp::stmts(
                            nqp::bindkey($prefs, $propname, 'S'),
                            $result),
                          nqp::stmts(
                            nqp::bindkey($prefs, $propname, 'I'),
                            nqp::getuniprop_int($code,$prop)))))))))))))
}
# Unicode functions
proto sub uniprop-int($, $, *%) {*}
multi sub uniprop-int(Str:D $str, Stringy:D $propname) {
    $str ?? uniprop-int($str.ord, $propname) !! Nil }
multi sub uniprop-int(Int:D $code, Stringy:D $propname) {
    nqp::getuniprop_int($code,nqp::unipropcode($propname));
}

proto sub uniprop-bool($, $, *%) {*}
multi sub uniprop-bool(Str:D $str, Stringy:D $propname) {
    $str ?? uniprop-bool($str.ord, $propname) !! Nil
}
multi sub uniprop-bool(Int:D $code, Stringy:D $propname) {
    nqp::hllbool(nqp::getuniprop_bool($code,nqp::unipropcode($propname)));
}

proto sub uniprop-str($, $, *%) {*}
multi sub uniprop-str(Str:D $str, Stringy:D $propname) {
    $str ?? uniprop-str($str.ord, $propname) !! Nil
}
multi sub uniprop-str(Int:D $code, Stringy:D $propname) {
    nqp::getuniprop_str($code,nqp::unipropcode($propname));
}
proto sub uniprops($, $?, *%) {*}
multi sub uniprops(Str:D $str, Stringy:D $propname = "General_Category") {
    $str.ords.map: { uniprop($_, $propname) }
}

proto sub unimatch($, |) {*}
multi sub unimatch(Str:D $str, |c) { $str ?? unimatch($str.ord, |c) !! Nil }
# This multi below can be removed when MoarVM bug #448 is fixed
multi sub unimatch(Int:D $code, Stringy:D $pvalname, Stringy:D $propname) {
    uniprop($code, $propname) eq $pvalname;
}
multi sub unimatch(Int:D $code, Stringy:D $pvalname, Stringy:D $propname = $pvalname) {
    my $prop := nqp::unipropcode($propname);
    nqp::hllbool(nqp::matchuniprop($code,$prop,nqp::unipvalcode($prop,$pvalname)));
}
#?endif

# vim: ft=perl6 expandtab sw=4
