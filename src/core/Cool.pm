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

    proto method log(|) {*}
    multi method log(Cool:D: )      { self.Numeric.log          }
    multi method log(Cool:D: $base) { self.Numeric.log($base.Numeric) }

    proto method exp(|) {*}
    multi method exp(Cool:D: )      { self.Numeric.exp          }
    multi method exp(Cool:D: $base) { self.Numeric.exp($base.Numeric) }

    proto method round(|) { * }
    multi method round()      { self.Numeric.round()      }
    multi method round($base) { self.Numeric.round($base) }

    method roots(Cool $n)   { self.Numeric.roots($n)    }
    method log10()          { self.Numeric.log10        }
    method unpolar($n)      { self.Numeric.unpolar($n.Numeric) }

    method floor()          { self.Numeric.floor        }
    method ceiling()        { self.Numeric.ceiling      }
    method truncate()       { self.Numeric.truncate     }

    ## string methods

    method chars() returns Int:D {
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

    method substr($from, $length?)           { substr(   self,$from,$length) }
    method substr-rw(\SELF: $from, $length?) { substr-rw(SELF,$from,$length) }

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
    method unival()         { unival(self) }
    method univals()        { univals(self) }
    method uniprop(|c)      { uniprop(self, |c) }
    method uniprop-int(|c)  { uniprop-int(self, |c) }
    method uniprop-bool(|c) { uniprop-bool(self, |c) }
    method uniprop-str(|c)  { uniprop-str(self, |c) }
    method uniprops(|c)     { uniprops(self, |c) }
    method unimatch(|c)     { unimatch(self, |c) }

    method chomp(Cool:D:) { self.Str.chomp }

    proto method chop(|)                { * }
    multi method chop(Cool:D:)          { self.Str.chop }
    multi method chop(Cool:D: Int() $n) { self.Str.chop($n) }

    method ord(--> Int) {
        self.Str.ord
    }
    method chr() {
        self.Int.chr;
    }
    method chrs(Cool:D:) { chrs(self.list) }
    method ords(Cool:D:) { self.Str.ords }


    method flip() {
        self.Str.flip
    }
    method trans(|c) { self.Str.trans(|c) }

    proto method starts-with(|) {*}
    multi method starts-with(Cool:D: Str(Cool) $needle) {
        self.Str.starts-with($needle)
    }

    proto method ends-with(|) {*}
    multi method ends-with(Cool:D: Str(Cool) $suffix) {
        self.Str.ends-with($suffix)
    }

    proto method substr-eq(|) {*}
    multi method substr-eq(Cool:D: Str(Cool) $needle, Cool $pos = 0) {
        self.Str.substr-eq($needle,$pos)
    }

    proto method contains(|) {*}
    multi method contains(Cool:D: Str(Cool) $needle, Cool $pos = 0) {
        self.Str.contains($needle,$pos.Int)
    }

    proto method indices(|) {*}
    multi method indices(Cool:D: Str(Cool) $needle, :$overlap) {
        self.Str.indices($needle,:$overlap)
    }
    multi method indices(Cool:D: Str(Cool) $needle,Int(Cool) $start,:$overlap) {
        self.Str.indices($needle,$start,:$overlap)
    }

    proto method index(|) {*}
    multi method index(Cool:D: Str(Cool) $needle) {
        self.Str.index($needle)
    }
    multi method index(Cool:D: Str(Cool) $needle, Int(Cool) $pos) {
        self.Str.index($needle,$pos)
    }

    proto method rindex(|) {*}
    multi method rindex(Cool:D: Str(Cool) $needle) {
        self.Str.rindex($needle)
    }
    multi method rindex(Cool:D: Str(Cool) $needle, Int(Cool) $pos) {
        self.Str.rindex($needle,$pos)
    }

    method split(Cool: |c) {
        self.Stringy.split(|c);
    }

    multi method match(Cool:D: $target, *%adverbs) {
        $/ := nqp::getlexcaller('$/');
        self.Stringy.match($target, |%adverbs)
    }

    proto method comb(|) { * }
    multi method comb() { self.Str.comb() }
    multi method comb(Regex $matcher, $limit = Inf) { self.Str.comb($matcher, $limit) }
    multi method comb(Str $matcher, $limit = Inf) { self.Str.comb($matcher, $limit) }

    proto method lines(|) {*}
    multi method lines(Cool:D: |c) { self.Str.lines(|c) }

    proto method words(|) {*}
    multi method words(Cool:D: |c) { self.Str.words(|c) }

    method subst(|c) {
        $/ := nqp::getlexdyn('$/');
        self.Stringy.subst(|c);
    }

    proto method subst-mutate(|) {
        $/ := nqp::getlexdyn('$/');
        {*}
    }
    multi method subst-mutate(Cool:D $self is rw: |c) {
        $/ := nqp::getlexdyn('$/');
        my $str   = $self.Str;
        my $match = $str.subst-mutate(|c);
        $self     = $str;
        $match
    }

    proto method IO(|) { * }
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

    proto method Int(|) { * }
    multi method Int()  {
        nqp::if(
            nqp::istype((my $numeric := self.Numeric), Failure),
            $numeric,
            $numeric.Int
        )
    }

    proto method UInt(|) { * }
    multi method UInt()  {
        my $got := self.Int;
        $got < 0
          ?? Failure.new(X::OutOfRange.new(
               :what('Coercion to UInt'),
               :$got,
               :range<0..^Inf>))
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
}
Metamodel::ClassHOW.exclude_parent(Cool);

proto sub chop(|) { * }
multi sub chop(Cool:D $s)           returns Str { $s.chop }
multi sub chop(Cool:D $s, Int() $n) returns Str { $s.chop($n) }

sub chomp(Cool:D $s) returns Str { $s.chomp }

sub flip(Cool $s) returns Str      { $s.flip }
sub index(Cool $s,$needle,$pos=0)  { $s.index($needle,$pos) }
sub lc(Cool $s)                    { $s.lc }
sub ord(Cool $s)                   { $s.ord }
sub uc(Cool $s)                    { $s.uc }
sub tc(Cool $s)                    { $s.tc }
sub fc(Cool $s)                    { $s.fc }
sub tclc(Cool $s)                  { $s.tclc }

sub indices(Cool $s,$needle,$pos=0,:$overlap) {
    $s.indices($needle,$pos,:$overlap);
}

proto sub rindex($, $, $?) is pure { * };
multi sub rindex(Cool $s, Cool $needle, Cool $pos) { $s.rindex($needle, $pos) };
multi sub rindex(Cool $s, Cool $needle)            { $s.rindex($needle) };

proto sub ords($) is pure     { * }
multi sub ords(Cool $s)       { ords($s.Stringy) }

proto sub comb($, $, $?)            { * }
multi sub comb(Regex $matcher, Cool $input, $limit = *) { $input.comb($matcher, $limit) }
multi sub comb(Str $matcher, Cool $input, $limit = *) { $input.comb($matcher, $limit) }
multi sub comb(Int:D $matcher, Cool $input, $limit = *) { $input.comb($matcher, $limit) }

proto sub wordcase($) is pure { * }
multi sub wordcase(Str:D $x) {$x.wordcase }
multi sub wordcase(Cool $x)  {$x.Str.wordcase }

sub sprintf(Cool $format, *@args) {
    CATCH {
        when X::Cannot::Lazy {
            X::Cannot::Lazy.new(:action('(s)printf')).throw
        }
        default {
            Rakudo::Internals.HANDLE-NQP-SPRINTF-ERRORS($_).throw
        }
    }
    Rakudo::Internals.initialize-sprintf-handler;
    @args.elems;
    nqp::p6box_s(
        nqp::sprintf(nqp::unbox_s($format.Stringy),
            nqp::clone(nqp::getattr(@args||[], List, '$!reified'))
        )
    )
}

sub printf(Cool $format, *@args)          { print sprintf $format, @args }
sub samecase(Cool $string, Cool $pattern) { $string.samecase($pattern) }
sub split($pat, Cool $target, |c)         { $target.split($pat, |c) }

proto sub chars($) is pure {*}
multi sub chars(Cool $x)  { $x.Str.chars }
multi sub chars(Str:D $x) { nqp::p6box_i(nqp::chars($x)) }
multi sub chars(str $x) returns int { nqp::chars($x) }

# These probably belong in a separate unicodey file

proto sub uniname(|) {*}
multi sub uniname(Str:D $str)  { $str ?? uniname($str.ord) !! Nil }
multi sub uniname(Int:D $code) { nqp::getuniname($code) }

proto sub uninames(|) {*}
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

#?if moar
proto sub uniprop(|) {*}
multi sub uniprop(Str:D $str, |c) { $str ?? uniprop($str.ord, |c) !! Nil }
multi sub uniprop(Int:D $code) {
    nqp::getuniprop_str($code,nqp::unipropcode('General_Category'));
}
multi sub uniprop(Int:D $code, Stringy:D $propname) {
    # prop-mappings can be removed when MoarVM bug #448 is fixed...
    ## The code below was generated by tools/build/makeUNIPROP.pl6
    state %prop-mappings = nqp::hash(
      'OGr_Ext','Other_Grapheme_Extend','cjkIRG_MSource','kIRG_MSource','Dash','Dash',
      'CI','Case_Ignorable','uc','Uppercase_Mapping','Radical','Radical',
      'Dia','Diacritic','CWCF','Changes_When_Casefolded','lc','Lowercase_Mapping',
      'IDS','ID_Start','cf','Case_Folding','cjkIRG_TSource','kIRG_TSource',
      'sc','Script','jt','Joining_Type','NFD_QC','NFD_Quick_Check',
      'XO_NFD','Expands_On_NFD','cjkOtherNumeric','kOtherNumeric',
      'scf','Simple_Case_Folding','sfc','Simple_Case_Folding','Lower','Lowercase',
      'Join_C','Join_Control','JSN','Jamo_Short_Name','bc','Bidi_Class',
      'SD','Soft_Dotted','dm','Decomposition_Mapping','cjkIRG_USource','kIRG_USource',
      'jg','Joining_Group','NFKC_CF','NFKC_Casefold','slc','Simple_Lowercase_Mapping',
      'STerm','Sentence_Terminal','UIdeo','Unified_Ideograph',
      'cjkAccountingNumeric','kAccountingNumeric','Upper','Uppercase','Math','Math',
      'IDST','IDS_Trinary_Operator','cjkIRG_VSource','kIRG_VSource',
      'NFKD_QC','NFKD_Quick_Check','Ext','Extender','NFKC_QC','NFKC_Quick_Check',
      'CE','Composition_Exclusion','Alpha','Alphabetic',
      'stc','Simple_Titlecase_Mapping','OAlpha','Other_Alphabetic',
      'XIDC','XID_Continue','age','Age','tc','Titlecase_Mapping',
      'cjkPrimaryNumeric','kPrimaryNumeric','OIDS','Other_ID_Start',
      'FC_NFKC','FC_NFKC_Closure','Cased','Cased','Hyphen','Hyphen',
      'XO_NFC','Expands_On_NFC','nv','Numeric_Value',
      'CWKCF','Changes_When_NFKC_Casefolded','OIDC','Other_ID_Continue',
      'XO_NFKD','Expands_On_NFKD','InPC','Indic_Positional_Category',
      'dt','Decomposition_Type','cjkIICore','kIICore','Bidi_M','Bidi_Mirrored',
      'XO_NFKC','Expands_On_NFKC','XIDS','XID_Start','isc','ISO_Comment',
      'Gr_Ext','Grapheme_Extend','NChar','Noncharacter_Code_Point',
      'scx','Script_Extensions','SB','Sentence_Break','Bidi_C','Bidi_Control',
      'CWT','Changes_When_Titlecased','Gr_Link','Grapheme_Link','OMath','Other_Math',
      'OUpper','Other_Uppercase','DI','Default_Ignorable_Code_Point',
      'CWCM','Changes_When_Casemapped','cjkIRG_GSource','kIRG_GSource',
      'LOE','Logical_Order_Exception','WB','Word_Break',
      'cjkIRG_JSource','kIRG_JSource','NFC_QC','NFC_Quick_Check',
      'WSpace','White_Space','space','White_Space',
      'PCM','Prepended_Concatenation_Mark','ODI','Other_Default_Ignorable_Code_Point',
      'bpb','Bidi_Paired_Bracket','blk','Block','OLower','Other_Lowercase',
      'CWU','Changes_When_Uppercased','InSC','Indic_Syllabic_Category',
      'VS','Variation_Selector','QMark','Quotation_Mark','Pat_Syn','Pattern_Syntax',
      'IDC','ID_Continue','IDSB','IDS_Binary_Operator','Ideo','Ideographic',
      'cjkCompatibilityVariant','kCompatibilityVariant',
      'suc','Simple_Uppercase_Mapping','hst','Hangul_Syllable_Type',
      'nt','Numeric_Type','bmg','Bidi_Mirroring_Glyph',
      'cjkIRG_HSource','kIRG_HSource','ea','East_Asian_Width','lb','Line_Break',
      'Term','Terminal_Punctuation','Pat_WS','Pattern_White_Space',
      'AHex','ASCII_Hex_Digit','cjkIRG_KSource','kIRG_KSource','Hex','Hex_Digit',
      'cjkIRG_KPSource','kIRG_KPSource','na1','Unicode_1_Name',
      'bpt','Bidi_Paired_Bracket_Type','gc','General_Category',
      'GCB','Grapheme_Cluster_Break','Gr_Base','Grapheme_Base',
      'CWL','Changes_When_Lowercased','na','Name','Name_Alias','Name_Alias',
      'Dep','Deprecated','Comp_Ex','Full_Composition_Exclusion',
      'cjkRSUnicode','kRSUnicode','Unicode_Radical_Stroke','kRSUnicode',
      'URS','kRSUnicode','ccc','Canonical_Combining_Class',
    );
    state %prefs = nqp::hash(
      'Other_Grapheme_Extend','B','Emoji_Modifier','B','Dash','B',
      'Case_Ignorable','B','Uppercase_Mapping','uc','Radical','B','Diacritic','B',
      'Changes_When_Casefolded','B','Lowercase_Mapping','lc','ID_Start','B',
      'Case_Folding','S','Script','S','Joining_Type','S','NFD_Quick_Check','S',
      'Expands_On_NFD','B','Simple_Case_Folding','S','Lowercase','B',
      'Join_Control','B','Bidi_Class','S','Soft_Dotted','B',
      'Decomposition_Mapping','S','Joining_Group','S','NFKC_Casefold','S',
      'Simple_Lowercase_Mapping','S','Sentence_Terminal','B','Unified_Ideograph','B',
      'Uppercase','B','Math','B','IDS_Trinary_Operator','B','NFKD_Quick_Check','S',
      'Extender','B','NFKC_Quick_Check','S','Composition_Exclusion','B',
      'Alphabetic','B','Simple_Titlecase_Mapping','S','Other_Alphabetic','B',
      'XID_Continue','B','Age','S','Titlecase_Mapping','tc','Other_ID_Start','B',
      'FC_NFKC_Closure','S','Cased','B','Hyphen','B','Expands_On_NFC','B',
      'Numeric_Value','nv','Changes_When_NFKC_Casefolded','B','Other_ID_Continue','B',
      'Expands_On_NFKD','B','Indic_Positional_Category','S','Decomposition_Type','S',
      'Bidi_Mirrored','B','Expands_On_NFKC','B','XID_Start','B','ISO_Comment','S',
      'Grapheme_Extend','B','Noncharacter_Code_Point','B','Sentence_Break','S',
      'Bidi_Control','B','Changes_When_Titlecased','B','Grapheme_Link','B',
      'Other_Math','B','Other_Uppercase','B','Default_Ignorable_Code_Point','B',
      'Changes_When_Casemapped','B','Logical_Order_Exception','B','Word_Break','S',
      'NFC_Quick_Check','S','White_Space','B','Prepended_Concatenation_Mark','B',
      'Other_Default_Ignorable_Code_Point','B','Block','S','Other_Lowercase','B',
      'Changes_When_Uppercased','B','Indic_Syllabic_Category','S',
      'Variation_Selector','B','Quotation_Mark','B','Pattern_Syntax','B',
      'ID_Continue','B','IDS_Binary_Operator','B','Ideographic','B',
      'kCompatibilityVariant','S','Simple_Uppercase_Mapping','S',
      'Hangul_Syllable_Type','S','Numeric_Type','S','Bidi_Mirroring_Glyph','bmg',
      'East_Asian_Width','S','Line_Break','S','Terminal_Punctuation','B',
      'Pattern_White_Space','B','ASCII_Hex_Digit','B','Hex_Digit','B',
      'Bidi_Paired_Bracket_Type','S','General_Category','S',
      'Grapheme_Cluster_Break','S','Grapheme_Base','B','Changes_When_Lowercased','B',
      'Name','na','Emoji','B','Emoji_Presentation','B','Deprecated','B',
      'Full_Composition_Exclusion','B','Canonical_Combining_Class','S',
    );
    ## End generated code
    $propname := nqp::atkey(%prop-mappings, $propname) if nqp::existskey(%prop-mappings,$propname);
    my $prop := nqp::unipropcode($propname);
    given nqp::atkey(%prefs, $propname) {
        when 'S'   { nqp::getuniprop_str($code,$prop) }
        when 'I'   { nqp::getuniprop_int($code,$prop) }
        when 'B'   { nqp::p6bool(nqp::getuniprop_bool($code,$prop)) }
        when 'lc'  { nqp::lc( nqp::chr( nqp::unbox_i($code) ) ) }
        when 'tc'  { nqp::tc( nqp::chr( nqp::unbox_i($code) ) ) }
        when 'uc'  { nqp::uc( nqp::chr( nqp::unbox_i($code) ) ) }
        when 'na'  { nqp::getuniname($code) }
        when 'nv'  { unival($code) }
        when 'bmg' {
            my int $bmg-ord = nqp::getuniprop_int($code, $prop);
            $bmg-ord ?? nqp::chr($bmg-ord) !! '';
        }
        default {
            my $result = nqp::getuniprop_str($code,$prop);
            if $result ne '' { nqp::bindkey(%prefs, $propname, 'S'); $result }
            else             { nqp::bindkey(%prefs, $propname, 'I'); nqp::getuniprop_int($code,$prop) }
        }
    }
}
# Unicode functions
proto sub uniprop-int(|) {*}
multi sub uniprop-int(Str:D $str, Stringy:D $propname) {
    $str ?? uniprop-int($str.ord, $propname) !! Nil }
multi sub uniprop-int(Int:D $code, Stringy:D $propname) {
    nqp::getuniprop_int($code,nqp::unipropcode($propname));
}

proto sub uniprop-bool(|) {*}
multi sub uniprop-bool(Str:D $str, Stringy:D $propname) {
    $str ?? uniprop-bool($str.ord, $propname) !! Nil
}
multi sub uniprop-bool(Int:D $code, Stringy:D $propname) {
    nqp::p6bool(nqp::getuniprop_bool($code,nqp::unipropcode($propname)));
}

proto sub uniprop-str(|) {*}
multi sub uniprop-str(Str:D $str, Stringy:D $propname) {
    $str ?? uniprop-str($str.ord, $propname) !! Nil
}
multi sub uniprop-str(Int:D $code, Stringy:D $propname) {
    nqp::getuniprop_str($code,nqp::unipropcode($propname));
}
proto sub uniprops(|) {*}
multi sub uniprops(Str:D $str, Stringy:D $propname = "General_Category") {
    $str.ords.map: { uniprop($_, $propname) }
}

proto sub unival(|) {*}
multi sub unival(Str:D $str) { $str ?? unival($str.ord) !! Nil }
multi sub unival(Int:D $code) {
    state $nuprop = nqp::unipropcode("Numeric_Value_Numerator");
    state $deprop = nqp::unipropcode("Numeric_Value_Denominator");
    my $nu = nqp::getuniprop_str($code, $nuprop);
    my $de = nqp::getuniprop_str($code, $deprop);
    !$de || $de eq '1' ?? $nu.Int !! $nu / $de;
}

proto sub univals(|) {*}
multi sub univals(Str:D $str) { $str.ords.map: { unival($_) } }

proto sub unimatch(|) {*}
multi sub unimatch(Str:D $str, |c) { $str ?? unimatch($str.ord, |c) !! Nil }
# This multi below can be removed when MoarVM bug #448 is fixed
multi sub unimatch(Int:D $code, Stringy:D $pvalname, Stringy:D $propname) {
    uniprop($code, $propname) eq $pvalname;
}
multi sub unimatch(Int:D $code, Stringy:D $pvalname, Stringy:D $propname = $pvalname) {
    my $prop := nqp::unipropcode($propname);
    nqp::p6bool(nqp::matchuniprop($code,$prop,nqp::unipvalcode($prop,$pvalname)));
}
#?endif

# vim: ft=perl6 expandtab sw=4
